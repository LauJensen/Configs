-- {{{ License
--
-- Awesome configuration, using awesome 3.4.6 on Arch GNU/Linux
--   * Adrian C. <anrxc@sysphere.org>

-- Screenshot: http://sysphere.org/gallery/snapshots

-- This work is licensed under the Creative Commons Attribution-Share
-- Alike License: http://creativecommons.org/licenses/by-sa/3.0/
-- }}}


-- {{{ Libraries
require("awful")
require("awful.rules")
require("awful.autofocus")
-- User libraries
require("vicious")
require("scratch")
require("naughty")
-- }}}


naughty.position = "top_left";

-- {{{ Variable definitions
local altkey = "Mod1"
local modkey = "Mod4"

local home   = os.getenv("HOME")
local exec   = awful.util.spawn
local sexec  = awful.util.spawn_with_shell

-- Beautiful theme
beautiful.init(home.."/.config/awesome/zenburn.lua")

-- Window management layouts
layouts = {
  awful.layout.suit.tile,        -- 1
  awful.layout.suit.tile.bottom, -- 2
  awful.layout.suit.fair,        -- 3
  awful.layout.suit.max,         -- 4
  awful.layout.suit.magnifier,   -- 5
  awful.layout.suit.floating     -- 6
}
-- }}}


-- {{{ Tags
tags = {
  names  = { "emacs", "web", "mail", "im", 6, 7, "rss", "media" },
  layout = { layouts[2], layouts[1], layouts[1], layouts[4], layouts[1],
             layouts[6], layouts[6], layouts[5], layouts[6]
}}

for s = 1, screen.count() do
    tags[s] = awful.tag(tags.names, s, tags.layout)
    awful.tag.setproperty(tags[s][5], "mwfact", 0.13)
    awful.tag.setproperty(tags[s][6], "hide",   true)
    awful.tag.setproperty(tags[s][7], "hide",   true)
end
-- }}}


-- {{{ Wibox
--
-- {{{ Widgets configuration
--
-- {{{ Reusable separator
separator = widget({ type = "imagebox" })
separator.image = image(beautiful.widget_sep)
-- }}}

-- {{{ CPU1 usage and temperature
cpuicon = widget({ type = "imagebox" })
cpuicon.image = image(beautiful.widget_cpu)
-- Initialize widgets
cpugraph  = awful.widget.graph()
-- Graph properties
cpugraph:set_width(40):set_height(14)
cpugraph:set_background_color(beautiful.fg_off_widget)
cpugraph:set_gradient_angle(0):set_gradient_colors({
   beautiful.fg_end_widget, beautiful.fg_center_widget, beautiful.fg_widget
}) -- Register widgets
vicious.cache(vicious.widgets.cpu)
vicious.register(cpugraph,  vicious.widgets.cpu,      "$2")
-- }}}


-- {{{ CPU2 usage and temperature
cpuicon2 = widget({ type = "imagebox" })
cpuicon2.image = image(beautiful.widget_cpu)
-- Initialize widgets
cpugraph2  = awful.widget.graph()
tzswidget2 = widget({ type = "textbox" })
-- Graph properties
cpugraph2:set_width(40):set_height(14)
cpugraph2:set_background_color(beautiful.fg_off_widget)
cpugraph2:set_gradient_angle(0):set_gradient_colors({
   beautiful.fg_end_widget, beautiful.fg_center_widget, beautiful.fg_widget
}) -- Register widgets
vicious.register(cpugraph2,  vicious.widgets.cpu,      "$3")
vicious.register(tzswidget2, vicious.widgets.thermal, " $1C", 19, "thermal_zone0")
-- }}}

-- {{{ Battery state
baticon = widget({ type = "imagebox" })
baticon.image = image(beautiful.widget_bat)
-- Initialize widget
batwidget = widget({ type = "textbox" })
-- Register widget
vicious.register(batwidget, vicious.widgets.bat, "$2%", 61, "BAT1")
-- }}}

-- {{{ Memory usage
memicon = widget({ type = "imagebox" })
memicon.image = image(beautiful.widget_mem)
-- Initialize widget
membar = awful.widget.progressbar()
-- Pogressbar properties
membar:set_vertical(true):set_ticks(true)
membar:set_height(12):set_width(8):set_ticks_size(2)
membar:set_background_color(beautiful.fg_off_widget)
membar:set_gradient_colors({ beautiful.fg_widget,
   beautiful.fg_center_widget, beautiful.fg_end_widget
}) -- Register widget
vicious.register(membar, vicious.widgets.mem, "$1", 13)
-- }}}

-- {{{ File system usage
fsicon = widget({ type = "imagebox" })
fsicon.image = image(beautiful.widget_fs)
-- Initialize widgets
fs = {
  r = awful.widget.progressbar(), h = awful.widget.progressbar(),
  s = awful.widget.progressbar(), b = awful.widget.progressbar()
}
-- Progressbar properties
for _, w in pairs(fs) do
  w:set_vertical(true):set_ticks(true)
  w:set_height(14):set_width(5):set_ticks_size(2)
  w:set_border_color(beautiful.border_widget)
  w:set_background_color(beautiful.fg_off_widget)
  w:set_gradient_colors({ beautiful.fg_widget,
     beautiful.fg_center_widget, beautiful.fg_end_widget
  }) -- Register buttons
  w.widget:buttons(awful.util.table.join(
    awful.button({ }, 1, function () exec("rox", false) end)
  ))
end -- Enable caching
vicious.cache(vicious.widgets.fs)
-- Register widgets
vicious.register(fs.r, vicious.widgets.fs, "${/ used_p}",            599)
vicious.register(fs.h, vicious.widgets.fs, "${/home used_p}",        599)
-- }}}

-- {{{ Network usage
dnicon = widget({ type = "imagebox" })
upicon = widget({ type = "imagebox" })
dnicon.image = image(beautiful.widget_net)
upicon.image = image(beautiful.widget_netup)
-- Initialize widget
netwidget = widget({ type = "textbox" })
-- Register widget
vicious.register(netwidget, vicious.widgets.net, '<span color="'
  .. beautiful.fg_netdn_widget ..'">${wlan0 down_kb}</span> <span color="'
  .. beautiful.fg_netup_widget ..'">${wlan0 up_kb}</span>', 3)
-- }}}

-- {{{ Org-mode agenda
orgicon = widget({ type = "imagebox" })
orgicon.image = image(beautiful.widget_org)
-- Initialize widget
orgwidget = widget({ type = "textbox" })
-- Configure widget
local orgmode = {
  files = { "/home/lau/Organize/Tasks.org" },
  color = {
    past   = '<span color="'..beautiful.fg_urgent..'">',
    today  = '<span color="'..beautiful.fg_normal..'">',
    soon   = '<span color="'..beautiful.fg_widget..'">',
    future = '<span color="'..beautiful.fg_netup_widget..'">'
}} -- Register widget
vicious.register(orgwidget, vicious.widgets.org,
  orgmode.color.past..'$1</span>-'..orgmode.color.today .. '$2</span>-' ..
  orgmode.color.soon..'$3</span>-'..orgmode.color.future.. '$4</span>', 601,
  orgmode.files
) -- Register buttons
orgwidget:buttons(awful.util.table.join(
  awful.button({ }, 1, function () exec("emacsclient -c -e '(org-agenda-list)'") end),
  awful.button({ }, 3, function () exec("emacsclient -c -e '(djcb-remember-frame)'") end)
))
-- }}}

-- {{{ Volume level
volicon = widget({ type = "imagebox" })
volicon.image = image(beautiful.widget_vol)
-- Initialize widgets
volbar    = awful.widget.progressbar()
volwidget = widget({ type = "textbox" })
-- Progressbar properties
volbar:set_vertical(true):set_ticks(true)
volbar:set_height(12):set_width(8):set_ticks_size(2)
volbar:set_background_color(beautiful.fg_off_widget)
volbar:set_gradient_colors({ beautiful.fg_widget,
   beautiful.fg_center_widget, beautiful.fg_end_widget
}) -- Enable caching
vicious.cache(vicious.widgets.volume)
-- Register widgets
vicious.register(volbar,    vicious.widgets.volume,  "$1",  2, "PCM")
vicious.register(volwidget, vicious.widgets.volume, " $1%", 2, "PCM")
-- Register buttons
volbar.widget:buttons(awful.util.table.join(
   awful.button({ }, 1, function () exec("kmix") end),
   awful.button({ }, 4, function () exec("amixer -q set PCM 4dB+", false) end),
   awful.button({ }, 5, function () exec("amixer -q set PCM 4dB-", false) end)
)) -- Register assigned buttons
volwidget:buttons(volbar.widget:buttons())
-- }}}

-- {{{ Date and time
dateicon = widget({ type = "imagebox" })
dateicon.image = image(beautiful.widget_date)
-- Initialize widget
datewidget = widget({ type = "textbox" })
-- Register widget
vicious.register(datewidget, vicious.widgets.date, "%R %d/%m/%Y", 61)
-- Register buttons
datewidget:buttons(awful.util.table.join(
  awful.button({ }, 1, function () exec("pylendar.py") end)
))
-- }}}

-- {{{ System tray
systray = widget({ type = "systray" })
-- }}}
-- }}}

-- {{{ Wibox initialisation
wibox     = {}
promptbox = {}
layoutbox = {}
taglist   = {}
taglist.buttons = awful.util.table.join(
    awful.button({ },        1, awful.tag.viewonly),
    awful.button({ modkey }, 1, awful.client.movetotag),
    awful.button({ },        3, awful.tag.viewtoggle),
    awful.button({ modkey }, 3, awful.client.toggletag),
    awful.button({ },        4, awful.tag.viewnext),
    awful.button({ },        5, awful.tag.viewprev
))

for s = 1, screen.count() do
    -- Create a promptbox
    promptbox[s] = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright })
    -- Create a layoutbox
    layoutbox[s] = awful.widget.layoutbox(s)
    layoutbox[s]:buttons(awful.util.table.join(
        awful.button({ }, 1, function () awful.layout.inc(layouts,  1) end),
        awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
        awful.button({ }, 4, function () awful.layout.inc(layouts,  1) end),
        awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)
    ))

    -- Create the taglist
    taglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.all, taglist.buttons)
    -- Create the wibox
    wibox[s] = awful.wibox({      screen = s,
        fg = beautiful.fg_normal, height = 12,
        bg = beautiful.bg_normal, position = "top",
        border_color = beautiful.border_focus,
        border_width = beautiful.border_width
    })
    -- Add widgets to the wibox
    wibox[s].widgets = {
        {   taglist[s], layoutbox[s], separator, promptbox[s],
            ["layout"] = awful.widget.layout.horizontal.leftright
        },
        --- s == screen.count() and systray or nil,
        s == 1 and systray or nil,
        separator, datewidget, dateicon,
        separator, volwidget,  volbar.widget, volicon,
        separator, orgwidget,  orgicon,
        separator, upicon,     netwidget, dnicon,
        separator, fs.b.widget, fs.s.widget, fs.h.widget, fs.r.widget, fsicon,
        separator, membar.widget, memicon,
        separator, batwidget, baticon,
        separator, tzswidget2, cpugraph.widget, cpuicon, cpugraph2.widget, cpuicon,
        separator, ["layout"] = awful.widget.layout.horizontal.rightleft
    }
end
-- }}}
-- }}}


-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))

-- Client bindings
clientbuttons = awful.util.table.join(
    awful.button({ },        1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize)
)
-- }}}


-- {{{ Key bindings
--
-- {{{ Global keys
globalkeys = awful.util.table.join(
    -- {{{ Applications
    awful.key({ modkey }, "e", function () exec("emacs") end),
    awful.key({ modkey }, "r", function () exec("thunar", false) end),
    awful.key({ modkey }, "w", function () exec("conkeror") end),
---    awful.key({ modkey }, "q", function () exec("emacsclient -c -e '(make-remember-frame)'") end),
    awful.key({ modkey }, "q", function () exec("emacsclient --eval '(make-remember-frame)'") end),
    awful.key({ modkey, "Shift"   }, "Escape", function () exec("sudo shutdown -hP now") end),
    -- }}}

    -- {{{ Multimedia keys
    awful.key({}, "#121", function () awful.util.spawn("amixer -q sset Master toggle") end),
    awful.key({}, "#122", function () awful.util.spawn("amixer -q sset PCM 2dB-") end),
    awful.key({}, "#123", function () awful.util.spawn("amixer -q sset PCM 2dB+") end),

    awful.key({}, "#157", function () if boosk then osk()
        else boosk, osk = pcall(require, "osk") end
    end),
    -- }}}

    -- {{{ Prompt menus
    awful.key({ altkey }, "F2", function ()
        awful.prompt.run({ prompt = "Run: " }, promptbox[mouse.screen].widget,
            function (...) promptbox[mouse.screen].text = exec(unpack(arg), false) end,
            awful.completion.shell, awful.util.getdir("cache") .. "/history")
    end),
    awful.key({ altkey }, "F3", function ()
        awful.prompt.run({ prompt = "Dictionary: " }, promptbox[mouse.screen].widget,
            function (words)
                sexec("crodict "..words.." | ".."xmessage -timeout 10 -file -")
            end)
    end),
    awful.key({ altkey }, "F5", function ()
        awful.prompt.run({ prompt = "Lua: " }, promptbox[mouse.screen].widget,
        awful.util.eval, nil, awful.util.getdir("cache") .. "/history_eval")
    end),
    -- }}}

    -- {{{ Awesome controls
    awful.key({ modkey }, "b", function ()
        wibox[mouse.screen].visible = not wibox[mouse.screen].visible
    end),
    awful.key({ modkey, "Shift" }, "q", awesome.quit),
    awful.key({ modkey, "Shift" }, "r", awesome.restart),
    -- }}}

    -- {{{ Tag browsing
    awful.key({ modkey }, "n",   awful.tag.viewnext),
    awful.key({ modkey }, "p",   awful.tag.viewprev),
    -- }}}

    -- {{{ Layout manipulation
    awful.key({ modkey }, "l",          function () awful.tag.incmwfact( 0.05) end),
    awful.key({ modkey }, "h",          function () awful.tag.incmwfact(-0.05) end),
    awful.key({ modkey, "Shift" }, "l", function () awful.client.incwfact(-0.05) end),
    awful.key({ modkey, "Shift" }, "h", function () awful.client.incwfact( 0.05) end),
    awful.key({ modkey, "Shift" }, "space", function () awful.layout.inc(layouts, -1) end),
    awful.key({ modkey },          "space", function () awful.layout.inc(layouts,  1) end),
    -- }}}

    -- {{{ Focus controls
    awful.key({ modkey, "Shift" }, "p", function () awful.screen.focus_relative(1) end),
    -- awful.key({ modkey }, "s", function () scratch.pad.toggle() end),
    awful.key({ modkey }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey }, "j", function ()
        awful.client.focus.byidx(1)
        if client.focus then client.focus:raise() end
    end),
    awful.key({ modkey }, "k", function ()
        awful.client.focus.byidx(-1)
        if client.focus then client.focus:raise() end
    end),
    awful.key({ modkey }, "Tab", function ()
        awful.client.focus.history.previous()
        if client.focus then client.focus:raise() end
    end),
    awful.key({ altkey }, "Escape", function ()
        awful.menu.menu_keys.down = { "Down", "Alt_L" }
        local cmenu = awful.menu.clients({width=230}, { keygrabber=true, coords={x=525, y=330} })
    end),
    awful.key({ modkey, "Shift" }, "j", function () awful.client.swap.byidx(1)  end),
    awful.key({ modkey, "Shift" }, "k", function () awful.client.swap.byidx(-1) end)
    -- }}}
)
-- }}}

-- {{{ Client manipulation
clientkeys = awful.util.table.join(
    awful.key({ modkey }, "c", function (c) c:kill() end),
    awful.key({ modkey }, "d", function (c) scratch.pad.set(c, 0.60, 0.60, true) end),
    awful.key({ modkey }, "f", function (c) awful.titlebar.remove(c)
        c.fullscreen           = not c.fullscreen
    end),
    awful.key({ modkey }, "m", function (c)
        c.maximized_horizontal = not c.maximized_horizontal
        c.maximized_vertical   = not c.maximized_vertical
    end),
    awful.key({ modkey }, "o",     awful.client.movetoscreen),
    awful.key({ modkey }, "Next",  function () awful.client.moveresize( 20,  20, -40, -40) end),
    awful.key({ modkey }, "Prior", function () awful.client.moveresize(-20, -20,  40,  40) end),
    awful.key({ modkey }, "Down",  function () awful.client.moveresize(  0,  20,   0,   0) end),
    awful.key({ modkey }, "Up",    function () awful.client.moveresize(  0, -20,   0,   0) end),
    awful.key({ modkey }, "Left",  function () awful.client.moveresize(-20,   0,   0,   0) end),
    awful.key({ modkey }, "Right", function () awful.client.moveresize( 20,   0,   0,   0) end),
    awful.key({ modkey, "Control"},"r", function (c) c:redraw() end),
    awful.key({ modkey, "Shift" }, "0", function (c) c.sticky = not c.sticky end),
    awful.key({ modkey, "Shift" }, "m", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey, "Shift" }, "c", function (c) exec("kill -CONT " .. c.pid) end),
    awful.key({ modkey, "Shift" }, "s", function (c) exec("kill -STOP " .. c.pid) end),
    awful.key({ modkey, "Shift" }, "t", function (c)
        if   c.titlebar then awful.titlebar.remove(c)
        else awful.titlebar.add(c, { modkey = modkey }) end
    end),
    awful.key({ modkey, "Shift" }, "f", function (c) if awful.client.floating.get(c)
        then awful.client.floating.delete(c);    awful.titlebar.remove(c)
        else awful.client.floating.set(c, true); awful.titlebar.add(c) end
    end)
)
-- }}}

-- {{{ Keyboard digits
local keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end
-- }}}

-- {{{ Tag controls
for i = 1, keynumber do
    globalkeys = awful.util.table.join( globalkeys,
        awful.key({ modkey }, "#" .. i + 9, function ()
            local screen = mouse.screen
            if tags[screen][i] then awful.tag.viewonly(tags[screen][i]) end
        end),
        awful.key({ modkey, "Control" }, "#" .. i + 9, function ()
            local screen = mouse.screen
            if tags[screen][i] then awful.tag.viewtoggle(tags[screen][i]) end
        end),
        awful.key({ modkey, "Shift" }, "#" .. i + 9, function ()
            if client.focus and tags[client.focus.screen][i] then
                awful.client.movetotag(tags[client.focus.screen][i])
            end
        end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9, function ()
            if client.focus and tags[client.focus.screen][i] then
                awful.client.toggletag(tags[client.focus.screen][i])
            end
        end))
end
-- }}}

-- Set keys
root.keys(globalkeys)
-- }}}


-- {{{ Rules
awful.rules.rules = {
    { rule = { }, properties = {
      focus = true,      size_hints_honor = false,
      keys = clientkeys, buttons = clientbuttons,
      border_width = beautiful.border_width,
      border_color = beautiful.border_normal }
    },
    { rule = { class = "Firefox",  instance = "Navigator" },
      properties = { tag = tags[screen.count()][3] } },
--    { rule = { class = "Emacs",    instance = "emacs" },
--      properties = { tag = tags[screen.count()][2] } },
    { rule = { class = "Emacs",    instance = "emacs" },
                                        properties = { tag = tags[1][1] } },
    { rule = { class = "Emacs",    instance = "_Remember_" },
      properties = { floating = true }, callback = awful.titlebar.add  },
    { rule = { class = "Conkeror" },    properties = { tag = tags[1][2]} },
    { rule = { class = "Lanikai" },     properties = { tag = tags[1][3]} },
---    { rule = { class = "Pidgin" },      properties = { tag = tags[2][4]} },
    { rule = { class = "Tilda" },       properties = { floating = true } },
    { rule = { class = "Vlc" },         properties = { floating = true } },
    { rule = { class = "gpicview" },    properties = { floating = true } },
    { rule = { class = "thunar" },      properties = { floating = true } },
    { rule = { class = "Pinentry.*" },  properties = { floating = true } },
}
-- }}}


-- {{{ Signals
--
-- {{{ Manage signal handler
client.add_signal("manage", function (c, startup)
    -- Add titlebar to floaters, but remove those from rule callback
    if awful.client.floating.get(c)
    or awful.layout.get(c.screen) == awful.layout.suit.floating then
        if   c.titlebar then awful.titlebar.remove(c)
        else awful.titlebar.add(c, {modkey = modkey}) end
    end

    -- Enable sloppy focus
    c:add_signal("mouse::enter", function (c)
        if  awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
        and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    -- Client placement
    if not startup then
        awful.client.setslave(c)

        if  not c.size_hints.program_position
        and not c.size_hints.user_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end
end)
-- }}}

-- {{{ Focus signal handlers
client.add_signal("focus",   function (c) c.border_color = beautiful.border_focus  end)
client.add_signal("unfocus", function (c) c.border_color = beautiful.border_normal end)
-- }}}

-- {{{ Arrange signal handler
for s = 1, screen.count() do screen[s]:add_signal("arrange", function ()
    local clients = awful.client.visible(s)
    local layout = awful.layout.getname(awful.layout.get(s))

    for _, c in pairs(clients) do -- Floaters are always on top
        if   awful.client.floating.get(c) or layout == "floating"
        then if not c.fullscreen then c.above       =  true  end
        else                          c.above       =  false end
    end
  end)
end
-- }}}
-- }}}

awful.util.spawn_with_shell("tilda")
awful.util.spawn_with_shell("conkeror")
awful.util.spawn_with_shell("thunderbird")
awful.util.spawn_with_shell("emacs")
