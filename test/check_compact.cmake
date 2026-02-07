# check_compact.cmake — Verify that a file contains no indentation.
#
# Usage: cmake -DFILE=path/to/file -DEXPECT_NO_INDENT=true -P check_compact.cmake
#
# Fails if any line in FILE starts with whitespace (space or tab), which
# indicates pretty-printed output rather than compact.

if(NOT EXISTS "${FILE}")
  message(FATAL_ERROR "File not found: ${FILE}")
endif()

file(READ "${FILE}" content)

# Check for lines starting with spaces (indentation)
string(REGEX MATCH "\n[ \t]+" indent_match "${content}")
if(indent_match)
  message(FATAL_ERROR
    "Compact output should have no indentation, but found indented lines in: ${FILE}")
endif()

message(STATUS "Compact output verified: ${FILE}")
