local _dump = require("pl.pretty").dump

local chartParams = { width = 0, height = 300 }

local chartImage = nil

if not love then love = {} end

function execRcommand(rcommand)
   local commandTemplate = 'emacsclient -s allegretto -e "(comint-send-string (get-process \\"R\\") \\"%s\\n\\")"'
   -- convert R command to shell command
   -- double up backslashes and put a doubled up backslash and an escaped quote for each quote
   local shellcmd = string.format(commandTemplate,
								  rcommand:gsub("\\", "\\\\"):gsub('"', '\\\\\\"'))
   os.execute(shellcmd)
end

function refreshImage(command)
   execRcommand('png(filename="chartImage-tmp.png", width=1680, height=300, units="px", pointsize=12)')
   execRcommand('chartSeries(myohlc)')
   execRcommand('dev.off()')
   love.timer.sleep(0.25)
   chartImage = love.graphics.newImage("chartImage-tmp.png")
end

function love.load()
   print("Allegretto starting")
   love.window.setMode(0, 0, {fullscreen=false, centered=false, resizable=true})
   chartParams.width = love.window.getWidth()
   chartImage = love.graphics.newImage("chartImage-tmp.png")
end

function love.update(dt)
   if love.keyboard.isDown(" ") then
   end
end

function love.keypressed(k)
   if k == 'escape' then
      love.event.quit()
   end

   if k == "enter" then
   end

   -- handle chart commands if we're in the chart area
   if love.mouse.getY() <= chartParams.height then
	  -- xRegion is:
	  -- 0 : left-ish side
	  -- 1 : center
	  -- 2 : right
	  local xRegion = math.floor(3 * love.mouse.getX() / chartParams.width)
	  if k == "up" then
		 refreshImage()
	  end
   end
end

function love.draw()
   love.graphics.draw(chartImage)
   --love.graphics.print("Hello World", 400, 300)
end
