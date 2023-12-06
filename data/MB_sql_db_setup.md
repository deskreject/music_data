# Recreating the PostgreSQL Skeleton for Musicbrainz table import

While there is no definitive guide on how to recreate the Musicbrainz database in PostgreSQL without going through the webserver setup, there is a workaround as outlined in steps below. 

First, it is necessary to create a new database in pgAdmin

Second, within this database, launch the query tool and visit (https://github.com/metabrainz/musicbrainz-server/tree/master/admin/sql) which includes an unordered collection of SQL scripts provided by the musicbrainz team. 

Third, run "CREATE SCHEMA musicbrainz" in the query tool, to create an empty schema

The SQL scripts of relevance for simply loading in tables into your database are as follows, in order that they should be executed in PGadmin's query tool (remove the \set ON_ERROR_STOP 1 line since it is specific to the psql command-line tool. Also, you can remove the BEGIN; and COMMIT):

    1) **Extensions.sql**: Run this first if there are any PostgreSQL extensions required by the other scripts.
    2) **CreateTypes.sql**: This defines custom data types used in the table definitions.
    3) **createfunctions.sql**: Creates some custom functions necessary for following scripts to run successfully -- **Important**: Running this will encounter an error, unless you comment out the portion relating to "mb_simple_tsvector"	
    4) **CreateCollations.sql**: Custom collations.
    5) **CreateTables.sql**: Creates all the table structures.
    6) **CreatePrimaryKeys.sql**: Adds primary key constraints, which are essential for table relationships.
    7) **CreateConstraints.sql**: Adds check constraints to tables for data integrity.
    8) **CreateFKConstraints.sql**: Adds foreign key constraints to establish relationships between tables.
    9) **CreateIndexes.sql**: Adds indexes to tables, which is important for query performance on larger datasets.

Once you have ran all the scripts successfully, you can proceed to the next step. 

# Importing individual tables

Download the MB datadump that provides you with the tab separted csv files (not json).

Make sure that all the MB datadump files are located in a file that PGadmin can access.
An example is provided below for importing the file "release_alt" to the table "release" in the musicbrainz databse in PostgreSQL. 

For individual table population, you can use the following scrip:
COPY musicbrainz.release FROM 'D:\musicbrainzTest\release_alt' WITH (FORMAT csv, DELIMITER E'\t', QUOTE E'^', NULL '\N');

explanation:
- COPY statement to tell PostgreSQL to import the data into the specified table
- DELIMITER statement - indicates the csv is tab separated
- QUOTE E'^' - important to define a character that will be understood as starting a comment. by default, " is interpreted to start a comment. However, in the MB data, there are some names that include " to mean inches or is used in a song name. Therefore, it is important to define
some other symbol as starting a comment in order not to encounter errors when importing a csv. Here, ^ was chosen after removing all instances of the symbol from the csvs. **Important** - you are tampering with original MB datadump files, hence it is important to carefully choose a symbol that won't lead
to issues down the line if deleted (e.g. not a good idea to use a letter and delete all instances of it). Depending on the use case for the data, a different symbol may be more appropriate to delete from the csv. ^ was chosen because it seemed to be the least used symbol in the csvs investigated. **After altering the csv, it is adviseable to save it as a new csv, rather than overwriting the original datadump file**
- NULL '\N' - tells PostgreSQL to interpret any instance of '\N' in the data files as indicating "NA" or "NULL". 


# Steps that need to be done to wholesale import all Musicbrainz tables into the Database

It would be necessary to create at least 2 scripts:
- A (python?) script that goes over every CSV file related to the datadump and replace all instances of " with some other symbol like ^
- A script that reads the names of all data files of the datadump and imports them sequentially into the appropriate tables of the PostgreSQL database

