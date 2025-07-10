# Analysis of Missing Songs from Spotify API Search

## Overview
This analysis examines the differences between songs successfully found via Spotify API (`final_charted_from_spotify_1980_2000_sampled.csv`) and songs that remain missing after multiple search attempts (`missing_after_after_fourth_fetch_sampled.csv`).

## Dataset Summary
- **Found songs**: 5,000 entries
- **Missing songs**: 5,000 entries

## Key Reasons for Missing Songs

### 1. Inconsistent Chart Notation Prefixes
Many entries contain chart-specific prefixes that don't match actual song titles:
- **Missing file**: 28 entries with "New" prefix vs **Found file**: 7 entries
- **Missing file**: 40 entries with "Re" prefix vs **Found file**: 29 entries

**Examples:**
- `New(WE DON'T NEED THIS) FASCIST GROOVE THANG`
- `REROLLERCOASTER`
- `New(KEEP ON) SHINING/HOPE (NEVER GIVE UP)`

### 2. Complex Song Titles with Multiple Tracks
The missing file contains significantly more entries with forward slashes (353 vs 5,000 in found file), indicating:
- Medleys or double A-sides
- Multiple songs combined into single chart entries
- Format incompatible with Spotify's individual track structure

**Examples:**
- `GOING OUT OF MY HEAD/MICHAEL JACKSON`
- `YOU DON'T HAVE TO SAY YOU LOVE ME/CRY...`
- `TWILIGHT ZONE/WRATHCHILD`

### 3. Chart-Specific Formatting Artifacts
Many titles contain chart notation that doesn't represent actual release titles:
- Parenthetical year additions for re-releases: `{1987}`
- Chart position indicators and re-entry markers
- Prefix notation for new entries or re-entries

**Examples:**
- `New(YOUR LOVE KEEPS LIFTING ME) HIGHER AND HIGHER {1987}`
- `New(WE DON'T NEED THIS) FASCIST GROOVE THANG {1993}`

### 4. Featuring Artist Variations
Higher concentration of featuring artists in missing file creates matching challenges:
- **Missing file**: 287 entries with "FT", "FEAT", or "feat"
- **Found file**: 74 entries with featuring artists
- Different formatting conventions may not align with Spotify's standardized format

**Examples:**
- `FREEFALL FT JAN JOHNSTON`
- `LOVELAND FT RACHEL MCFARLANE`
- `NOMAD FEATURING MC MIKEE FREEDOM`

### 5. Special Characters and Non-Standard Formatting
- Non-standard apostrophes, brackets, and punctuation
- Chart-specific notations and symbols
- Unusual character combinations that don't match streaming metadata

### 6. Regional and Obscure Releases
Some entries appear to be:
- Very specific regional chart entries
- B-sides or rare releases
- Tracks not widely available on streaming platforms

## Conclusion
The successful matches in the "found" file tend to have cleaner, more standardized titles that align with typical music streaming metadata formats. The missing songs predominantly contain chart notation artifacts, complex formatting, and non-standard title structures that make exact API matching difficult.

## Recommendations
1. **Data Cleaning**: Remove chart notation prefixes ("New", "Re") before API searches
2. **Title Splitting**: Handle slash-separated titles as separate search queries
3. **Standardization**: Normalize featuring artist formats to match Spotify conventions
4. **Fuzzy Matching**: Implement approximate string matching for titles with special characters
5. **Alternative Search**: Use artist-only searches for highly formatted titles