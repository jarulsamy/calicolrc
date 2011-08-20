module("state3", package.seeall);

t0 = 0;
timeout = 1.0;
accept = false;

function entry()
  print(_NAME.." entry");
end

function update()
  t0 = t0 + 0.1;
  cprint(_NAME.." update")

  if (t0 == .5) then
    return "ball";
  end
  if (t0 > timeout) then
    accept = true;
    return "timeout";
  end
end

function exit()
    print("   Exit from 3");
end