* Create folders for CIDA project;

* Keywords: CIDA project, folder organization, new project

***********************************************
CENTER FOR INNOVATIVE DESIGN AND ANALYSIS
PROGRAMMER: LALA GRAU
DATE: 2018-07
PURPOSE: CREATE PROJECT FOLDERS
***********************************************;


%let root=PLEASE PASTE THE PATHWAY (WITHOUT QUOTATION MARKS);


*SET SAS SESSION OPTIONS;
OPTIONS DLCREATEDIR;

*CREATE FOLDERS;
libname create "&root/Code";
libname create "&root/Background";
libname create "&root/DataRaw";
libname create "&root/DataProcessed";
libname create "&root/DataProcessed/Results";
libname create "&root/Dissemination";
libname create "&root/Reports";


*RESET SAS SESSION OPTIONS;
LIBNAME Create CLEAR;

OPTIONS NODLCREATEDIR;
