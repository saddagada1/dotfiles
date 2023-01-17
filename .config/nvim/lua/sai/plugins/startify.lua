local default_header = {
	type = "text",
	val = {
		[[                                  __                ]],
		[[     ___     ___    ___   __  __ /\_\    ___ ___    ]],
		[[    / _ `\  / __`\ / __`\/\ \/\ \\/\ \  / __` __`\  ]],
		[[   /\ \/\ \/\  __//\ \_\ \ \ \_/ |\ \ \/\ \/\ \/\ \ ]],
		[[   \ \_\ \_\ \____\ \____/\ \___/  \ \_\ \_\ \_\ \_\]],
		[[    \/_/\/_/\/____/\/___/  \/__/    \/_/\/_/\/_/\/_/]],
	},
	opts = {
		hl = "Type",
		shrink_margin = false,
		-- wrap = "overflow";
	},
}

vim.g.startify_custom_header = default_header
