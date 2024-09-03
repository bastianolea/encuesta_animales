-- Function to log messages
local function log(message)
    io.stderr:write(message .. "\n")
end

-- Run R command and return result
local function run_r_command(cmd)
    local handle = io.popen(cmd)
    local result = handle:read("*a")
    handle:close()
    return result:gsub("%s+$", "")
end

-- Get the path to the main Lua filter in the surveydown package
local function get_main_filter_path()
    local cmd = 'Rscript -e "cat(system.file(\'quarto/filters\', \'main.lua\', package = \'surveydown\'))"'
    local result = run_r_command(cmd)

    if result == "" then
        error("Unable to find main.lua in the surveydown package. Please check your installation.")
    end

    return result
end

-- Get the main filter path
local main_filter_path = get_main_filter_path()

-- Load the main filter
local main_filter, load_error = loadfile(main_filter_path)

if load_error then
    error("Failed to load main filter: " .. load_error)
else
    main_filter = main_filter()  -- Execute the loaded chunk
end

-- Function to run the main filter
function Pandoc(doc)
    if main_filter and type(main_filter.Pandoc) == "function" then
        return main_filter.Pandoc(doc)
    else
        log("Warning: Main filter's Pandoc function not found or not a function")
        return doc
    end
end
