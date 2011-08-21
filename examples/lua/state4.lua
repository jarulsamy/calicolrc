module("state4", package.seeall);

function entry()
  print(_NAME.." entry");
end

function update()
  print(_NAME.." update");
end

function exit()
  print(_NAME.." exit");
end