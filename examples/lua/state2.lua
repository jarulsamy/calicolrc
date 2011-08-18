module("state2", package.seeall);

t0 = 0;
timeout = 1.0;

function entry()
  print(_NAME.." entry");
end

function update()
  t0 = t0 + 0.1;
  cprint(_NAME.." update")

  if (t0 == .5) then
    return "lost";
  end

  if (t0 > timeout) then
    return "timeout";
  end
end

function exit()
end