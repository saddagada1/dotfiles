local status, tokyonight = pcall(require, "tokyonight")
if not status then
	print("colorscheme not found")
	return
end

tokyonight.setup({
	style = "night",
	transparent = true,
})

vim.cmd([[colorscheme tokyonight]])
