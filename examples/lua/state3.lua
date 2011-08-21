module("state3", package.seeall);

t0 = 0;
timeout = 1.0;

function entry()
  print(_NAME.." entry");
end

function update()
  t0 = t0 + 0.1;
  print(_NAME.." update")

  if (t0 > timeout) then
    return "accept";
  end
end

function exit()
    print("   Exit from 3");
end