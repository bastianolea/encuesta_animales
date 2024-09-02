-- Function to log messages
local function log(message)
    io.stderr:write(message .. "\n")
end

-- Detect the operating system
local function get_os()
    local os_type
    if package.config:sub(1,1) == '\\' then
        os_type = "windows"
    elseif os.execute('uname -s >/dev/null 2>&1') == 0 then
        local f = io.popen("uname -s")
        local uname = f:read("*a")
        f:close()
        if uname:match("^Darwin") then
            os_type = "macos"
        else
            os_type = "unix"
        end
    else
        os_type = "unknown"
    end

    -- Check for forward slash usage on Windows
    if os_type == "windows" and package.config:sub(1,1) == '/' then
        log("Windows detected, but using forward slashes")
        os_type = "windows_forward_slash"
    end

    return os_type
end

-- Get the path to the main Lua filter in the surveydown package
local function get_main_filter_path()
    local os_type = get_os()
    local cmd

    if os_type == "windows" or os_type == "windows_forward_slash" then
        cmd = 'Rscript -e "cat(system.file(\'quarto/filters\', \'main.lua\', package = \'surveydown\'))"'
    else -- macOS and Unix
        cmd = "Rscript -e \"cat(system.file('quarto/filters', 'main.lua', package = 'surveydown'))\""
    end

    local result
    if os_type == "windows" or os_type == "windows_forward_slash" then
        local file = io.popen(cmd, "r")
        result = file:read("*a")
        file:close()
    else
        result = pandoc.pipe("sh", {"-c", cmd}, "")
    end

    return result:gsub("%s+$", "")
end

-- Get the main filter path
local main_filter_path = get_main_filter_path()
log("Main filter path: " .. main_filter_path)

-- Load the main filter
local main_filter, load_error = loadfile(main_filter_path)

if load_error then
    log("Error loading main filter: " .. load_error)
    main_filter = nil
else
    log("Main filter loaded successfully")
    main_filter = main_filter()  -- Execute the loaded chunk
end

-- Function to run the main filter
function Pandoc(doc)
    if main_filter and type(main_filter.Pandoc) == "function" then
        log("Calling main filter's Pandoc function")
        return main_filter.Pandoc(doc)
    else
        log("Main filter's Pandoc function not found or not a function")
        return doc
    end
end

-- Log that this filter has finished loading
log("Cross-platform nested filter loaded successfully")
