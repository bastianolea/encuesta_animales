-- Function to log messages (you can modify this to write to a file if needed)
local function log(message)
    io.stderr:write(message .. "\n")
end

-- Get the path to the main Lua filter in the surveydown package
local cmd = "Rscript -e \"cat(system.file('quarto/filters', 'surveydown.lua', package = 'surveydown'))\""
local main_filter_path = pandoc.pipe("sh", {"-c", cmd}, ""):gsub("%s+$", "")

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
log("Nested filter loaded successfully")
