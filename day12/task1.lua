function main()
	filepath = "../inputs/day12"
	file = io.open(filepath, "r")

	spaces = {}
	blocks = {}

	currentBlock = ""
	firstLine = true

	for line in file:lines() do
		if line ~= "" then
			if firstLine then
				firstLine = false
			elseif line:sub(2, 2) == ":" then
				block = {}
				for i = 1, #currentBlock do
					char = currentBlock:sub(i, i)
					table.insert(block, char == "#")
				end
				table.insert(blocks, block)
				currentBlock = ""
			elseif line:find("x") then
				space = {}
				xPos = line:find("x")
				colonPos = line:find(":")
				space.width = tonumber(line:sub(1, xPos - 1))
				space.height = tonumber(line:sub(xPos + 1, colonPos - 1))
				space.blockReps = {}
				sum = 0
				numberStr = ""

				for i = 5, #line do
					c = line:sub(i, i)
					if c:match("%d") then
						numberStr = numberStr .. c
					elseif c == " " then
						if numberStr ~= "" then
							num = tonumber(numberStr)
							table.insert(space.blockReps, num)
						end
						numberStr = ""
					end
				end

				if sum <= space.width * space.height then
					table.insert(spaces, space)
				end
			else
				currentBlock = currentBlock .. line
			end
		end
	end

	if currentBlock ~= "" then
		block = {}
		for i = 1, #currentBlock do
			char = currentBlock:sub(i, i)
			table.insert(block, char == "#")
		end
		table.insert(blocks, block)
	end

	file:close()

	count = 0
	for _, space in ipairs(spaces) do
		totalBlockArea = 0
		for i, rep in ipairs(space.blockReps) do
			blockArea = 9
			totalBlockArea = totalBlockArea + blockArea * rep
		end
		if totalBlockArea <= space.width * space.height then
			count = count + 1
		end
	end

	print(count)
end

main()
