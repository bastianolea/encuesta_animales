-- Function to log error messages
local function log_error(message)
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
    local cmd = 'Rscript -e "cat(file.path(find.package(\'surveydown\'), \'quarto\', \'filters\', \'sd_main.lua\'))"'
    return run_r_command(cmd)
end

-- Function to load file
local function load_file(path)
    local f, err = loadfile(path)
    if f then
        return f
    else
        return nil, err
    end
end

-- Get the main filter path
local main_filter_path = get_main_filter_path()

-- Load the main filter
local main_filter, load_error = load_file(main_filter_path)

if load_error then
    log_error("Error loading main filter: " .. load_error)
    error(load_error)
else
    main_filter = main_filter()  -- Execute the loaded chunk
end

-- Function to run the main filter
function Pandoc(doc)
    if main_filter and type(main_filter.Pandoc) == "function" then
        return main_filter.Pandoc(doc)
    else
        log_error("Warning: Main filter's Pandoc function not found or not a function")
        return doc
    end
end
