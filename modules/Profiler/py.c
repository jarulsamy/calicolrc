#include <string.h>
#include <mono/metadata/profiler.h>
#include <mono/metadata/assembly.h>

MonoProfiler *pyprof_profiler;

struct _MonoProfiler {
    MonoClass *pma;
    GHashTable *hash;
    int depth;
    int read, write;
};

void
pyprof_load (MonoAssembly *assembly, gpointer user_data)
{
    MonoProfiler *prof;
    MonoImage *image;

    prof = (MonoProfiler *)user_data;

    if (prof->pma) {
        return;
    }

    image = mono_assembly_get_image (assembly);

    if (!strcmp (mono_image_get_name (image), "IronPython")) {
        prof->pma = mono_class_from_name (
            image, "IronPython.Runtime", "PythonModuleAttribute");
        g_print ("PythonModuleAttribute loaded\n");
        g_hash_table_insert (prof->hash, assembly, (gpointer)-1);
    }
}

static MonoAssembly *
pyprof_method_get_assembly (MonoMethod *method)
{
    MonoClass *klass;
    MonoImage *image;

    klass = mono_method_get_class (method);
    image = mono_class_get_image (klass);
    return mono_image_get_assembly (image);
}

static gboolean
pyprof_filter_assembly (MonoProfiler *prof, MonoAssembly *assembly)
{
    int flag;
    MonoDomain *domain;
    MonoObject *object;
    MonoCustomAttrInfo *cinfo;

    flag = (int)g_hash_table_lookup (prof->hash, assembly);

    if (!flag) {
        /* cinfo = mono_custom_attrs_from_assembly (assembly); */
        domain = mono_domain_get ();
        object = (MonoObject *)mono_assembly_get_object (domain, assembly);
        cinfo = mono_reflection_get_custom_attrs_info (object);
        flag = -1;
        if (cinfo) {
            if (mono_custom_attrs_has_attr (cinfo, prof->pma)) {
                flag = 1;
            }
            mono_custom_attrs_free (cinfo);
        }
        g_hash_table_insert (prof->hash, assembly, (gpointer)flag);
    }

    return flag > 0;
}

static gboolean
pyprof_filter_method (MonoProfiler *prof, MonoMethod *method)
{
    const char *name;

    name = mono_method_get_name (method);

    if (!strcmp (name, ".cctor") ||
        !strcmp (name, ".ctor") ||
        !strcmp (name, "TryGetExtraValue") ||
        !strcmp (name, "TrySetExtraValue")) {
        return FALSE;
    }

    return TRUE;
}

static gchar *
pyprof_format_method (MonoMethod *method)
{
    MonoClass *klass;
    MonoImage *image;
    const char *image_name, *method_name;

    klass = mono_method_get_class (method);
    image = mono_class_get_image (klass);
    image_name = mono_image_get_name (image);
    method_name = mono_method_get_name (method);

    if (!strcmp (method_name, "Initialize")) {
        method_name = "<module>";
    }

    return g_strdup_printf ("%s:%s", image_name, method_name);
}

void
pyprof_enter (MonoProfiler *prof, MonoMethod *method)
{
    MonoAssembly *assembly;
    char *name, *indent;

    if (!prof->pma) {
        return;
    }

    assembly = pyprof_method_get_assembly (method);

    if (!pyprof_filter_assembly (prof, assembly)) {
        return;
    }

    if (!pyprof_filter_method (prof, method)) {
        return;
    }

    name = pyprof_format_method (method);
    indent = g_strnfill (prof->depth, ' ');
    g_print ("%sENTER: %s\n", indent, name);
    g_free (name);
    g_free (indent);

    prof->depth++;
}

void
pyprof_leave (MonoProfiler *prof, MonoMethod *method)
{
    MonoAssembly *assembly;
    char *name, *indent;

    if (!prof->pma) {
        return;
    }

    assembly = pyprof_method_get_assembly (method);

    if (!pyprof_filter_assembly (prof, assembly)) {
        return;
    }

    if (!pyprof_filter_method (prof, method)) {
        return;
    }

    prof->depth--;

    name = pyprof_format_method (method);
    indent = g_strnfill (prof->depth, ' ');
    g_print ("%sLEAVE: %s\n", indent, name);
    g_free (name);
    g_free (indent);
}

int
pyprof_pipe ()
{
    return pyprof_profiler->read;
}

void
pyprof_close ()
{
    close (pyprof_profiler->write);
}

void
pyprof_print (const gchar *string)
{
    write (pyprof_profiler->write, string, strlen (string));
}

void
mono_profiler_startup (const char *desc)
{
    MonoProfiler *prof;
    int pipes[2];
    MonoProfileFlags flags = 0;

    prof = g_new0 (MonoProfiler, 1);
    prof->hash = g_hash_table_new (NULL, NULL);

    pipe (pipes);
    prof->read = pipes[0];
    prof->write = pipes[1];

    pyprof_profiler = prof;

    g_set_print_handler (pyprof_print);
    g_print ("pyprof startup\n");

    mono_install_assembly_load_hook (pyprof_load, prof);

    mono_profiler_install (prof, NULL);

    flags |= MONO_PROFILE_ENTER_LEAVE;
    mono_profiler_install_enter_leave (pyprof_enter, pyprof_leave);

    flags |= MONO_PROFILE_EXCEPTIONS;
    mono_profiler_install_exception (NULL, pyprof_leave, NULL);

    mono_profiler_set_events (flags);
}
