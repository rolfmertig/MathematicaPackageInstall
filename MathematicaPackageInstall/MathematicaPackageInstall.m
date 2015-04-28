(* ::Package:: *)

(* ::Author:: Rolf Mertig , GluonVision GmbH,  http://www.gluonvision.com *)

(* ::License:: LGPL *)

(* ::Title:: *)

(* Wolfram Mathematica Package and Palette Installer 2.0 *)

(* PackageInstall provides a simple JLink-based web-installer of github-based Mathematica 
   application packages or .m files 
   (or any URL-based zip packages) containing Mathematica Application Packages *)

(* ::Context:: MathematicaPackageInstall` *)

(* 
load this file by 
Get @ "http://goo.gl/Ncbbi6"

or 
Import @ "https://raw.githubusercontent.com/rolfmertig/MathematicaPackageInstall/master/MathematicaPackageInstall/MathematicaPackageInstall.m"*)

(* This could be commented *)
If[$Notebooks && ($VersionNumber < 10), SetOptions[$FrontEndSession, MessageOptions -> {"InsufficientVersionWarning" -> False}]];

MathematicalPackageInstall`Version = 2.0;

Quiet[BeginPackage["MathematicaPackageInstall`", {"CopyRemote`", "Unzip`"}]];

Unprotect @@ {MathematicaPackageInstall, InstallPalette, MathematicaPackageUninstall, UninstallPalette};
ClearAll @@ {MathematicaPackageInstall, InstallPalette, MathematicaPackageUninstall, UninstallPalette};

MathematicaPackageInstall::usage = 
"MathematicaPackageInstall[\"MyPackage\"] or \n
MathematicaPackageInstall[\"https://github.com/rolfmertig/MyPackage/archive/master.zip\"] \n
downloads and unzips master.zip from $WebRepository/MyPackage/archive/master.zip,\n
to the directory specified by the setting specified by the option Directory,\n
installing to $UserBaseDirectory/Applications by default.\n
$WebRepository is set to  \"https://github.com/rolfmertig/\" by default, but can be changed.\n
If MyPackage already exists it is overwritten, no backups are done.";

InstallPalette::usage = "InstallPalette[\"http://www.mertig.com/mathdepot/ButtonTools.nb\"] installs ButtonTools.nb into 
the default directory specified by the option Directory.";

UninstallPalette::usage = "UninstallPalette[\"ButtonTools.nb\"] uninstalls ButtonTools.nb from the directory specified by the option
Directory. Please close the palette first before calling UninstallPalette.";

MathematicaPackageUninstall::usage = 
"MathematicaPackageUninstall[\"MyPackage\"] uninstalls (deletes) MyPackage from 
the Applications folder in $UserBaseDirectory.";

$InstallerDirectory::usage = "$InstallerDirectory is the directory to install packages to. 
	It is set by default to FileNameJoin[{$UserBaseDirectory,\"Applications\"}].";

$AutomaticInstall::usage = "If $AutomaticInstall is set to True, 
the three packages MathematicaPackageInstall`, CopyRemote` and Unzip` are installed locally already when loading this
package the first time. Otherwise they are loaded from the internet.";

$WebRepository::usage = "$WebRepository is the base url (default \"http://github.com/rolfmertig/\" ) where the packages are.";

MathematicaPackageInstall::notazipfile = "The URL `1` is not referring to a zip file.";
MathematicaPackageInstall::badurl = "The URL `1` does not exist or is not reachable.";
MathematicaPackageInstall::baddir = "The directory `1` does not exist.";

URLNeeds::usage = "URLNeeds[\"MyPackage`\"] loads MyPackage.m from $WebRepository/MyPackage/master/MyPackage/MyPackage.m";

(* this is customizable of course, 
   so you may do 
MathematicaPackageInstall`$WebRepository = "http://raw.githubusercontent.com/yourname/";
   before loading this package
   
The default for now is googlecode.com/hg/  
*)

(*
If[ !ValueQ[$WebRepository], $WebRepository = "https://raw.githubusercontent.com/rolfmertig/"; ];
*)
If[ !ValueQ[$WebRepository], $WebRepository = "https://github.com/rolfmertig/"; ];

(* install package by default, but provide the possibility of not to do so by
   setting MathematicaPackageInstall = $AutomaticInstall = False
   before loading this package
*)

If[ !ValueQ[$AutomaticInstall], $AutomaticInstall = False];

If[ !ValueQ[$InstallerDirectory],
  $InstallerDirectory :=
      FileNameJoin[{$UserBaseDirectory, "Applications"}]
];

(* TODO: implement some kind of uptdate mechanism of this and the dependent packages
   If[!ValueQ[$AutoUpdate], $AutoUpdate =  True];
*)

Begin["`Private`"];

(* auxiliary functions *)

toraw = Function[url, If[ StringMatchQ[url, "https://github.com/*"],
                          StringReplace[url, "github.com" -> "raw.githubusercontent.com"],
                          url
                      ]
];

(* first call Needs, then go to toraw@$WebRepository *)
(* Notice that URLNeeds is simpler than Needs, so Kernel/init.m should be trivial, since it is ignored in URLNeeds ... *)
URLNeeds = Function[package,
  (*Quiet@*)If[ FindFile[package] =!= $Failed,
    Needs[package],
    Import[
    	(*this gives, e.g.:
    	https://raw.githubusercontent.com/rolfmertig/Unzip/master/Unzip/Unzip/Unzip.m
    	 *)
        toraw[$WebRepository] <> 
        StringReplace[package, "`" -> "/master/"] <> 
 		StringReplace[package, "`" -> ""] <> "/" <> 
		StringReplace[package, a__ ~~ "`" :> (a ~~ ".m")]
	]
  ]
];


nbQ = StringMatchQ[#, "*.nb", IgnoreCase -> True]&;
zipQ = StringMatchQ[#, "*.zip", IgnoreCase -> True]&;
urlQ = CopyRemote`URLQ;

(*  a NotebookClose function which does nothing if $Notebooks is False *)
closenb = Function[locnb, If[ $Notebooks && FileExistsQ[locnb],
  Select[Notebooks[],
    ToFileName[ "FileName" /. NotebookInformation[#]] === locnb &] /. {n_NotebookObject} :> NotebookClose[n]
];
locnb];

fixversion[nb_String?FileExistsQ] :=
    Module[ {nbp},
      nbp = NotebookPut[Import[nb, "Package"] /. (FrontEndVersion -> _) :> Sequence[]];
      DeleteFile[nb];
      NotebookSave[nbp, nb];
      nb
    ];

closeandfix = Function[nb, If[ $Notebooks,
  fixversion @ closenb @ nb,
  nb
]];

(* InstallPalette *)
Options[InstallPalette] =
    Options[UninstallPalette] =
        {Directory :> FileNameJoin[{$UserBaseDirectory, "SystemFiles", "FrontEnd", "Palettes"}], Print -> True};

InstallPalette[url_String?urlQ, nb_String?nbQ, ops___?OptionQ] :=
    InstallPalette[ url <> nb, ops] /; StringTake[url, -1] === "/";

InstallPalette[url_String?urlQ, nb_String?nbQ, ops___?OptionQ] :=
    InstallPalette[ url <> "/" <> nb, ops] /; StringTake[url, -1] =!= "/";

InstallPalette[url_String?(nbQ[#] && !urlQ[#]&), ___] :=
    Message[MathematicaPackageInstall::badurl, url];

InstallPalette[url_String?(nbQ[#] && urlQ[#]&), OptionsPattern[]] := Module[{},
(* open the palette by default if we have a notebook interface *)
  closeandfix @ CopyRemote`CopyRemote[ url, OptionValue[Directory], Print -> OptionValue[Print]];
  If[$Notebooks,
  (* R.M & Szabolcs mentioned: http://mathematica.stackexchange.com/questions/8563/refreshing-the-palettes-menu/8564#8564 *)
    MathLink`CallFrontEnd[FrontEnd`ResetMenusPacket[{Automatic, Automatic}]];
    (* and http://mathematica.stackexchange.com/a/8604/12 *)
    FrontEndTokenExecute["OpenFromPalettesMenu", StringReplace[FileNameTake[url], "%20" -> " "]];
    Notebooks[]//First (* this gives the palette just opened*)
  ]
];

UninstallPalette[pal_String, OptionsPattern[]] :=
    Module[ {dir, palfile},
      dir = OptionValue[Directory];
      palfile = FileNameJoin[{dir, pal}];
      If[ FileExistsQ[palfile],
        DeleteFile[palfile]
      ];
        MathLink`CallFrontEnd[FrontEnd`ResetMenusPacket[{Automatic, Automatic}]];
    ];

(* MathematicaPackageInstall *)

packQ[s_String /; StringLength[s] > 1] := packQ[s] = 
    StringCount[s,"`"] === 1 && StringTake[s, -1] === "`";
packQ[_] :=
    False;

Options[MathematicaPackageInstall] = {Directory :> $InstallerDirectory, Print -> False};

(* maybe easier to allow also this: *)
MathematicaPackageInstall[p_String /; (!packQ[p]) && (!urlQ[p]), more___] := 
	MathematicaPackageInstall[p <> "`", more];

(* Enable MathematicaPackageInstall["CopyRemote`"] *)
MathematicaPackageInstall[p_String /; (packQ[p] && !StringMatchQ[p, "http*", IgnoreCase -> True]), ops___?OptionQ] :=
    MathematicaPackageInstall[$WebRepository, StringReplace[p, "`" -> "/" <> "archive/master.zip"], ops];

MathematicaPackageInstall[webrep_?urlQ, p_String?zipQ, ops___?OptionQ] :=
    MathematicaPackageInstall[ webrep <> p, ops] /; StringTake[webrep, -1] === "/";

MathematicaPackageInstall[webrep_?urlQ, p_String?zipQ, ops___?OptionQ] :=
    MathematicaPackageInstall[ webrep <> "/" <> p, ops] /; StringTake[webrep, -1] =!= "/";

MathematicaPackageInstall[zipurl_String?(!zipQ[#]&), ___?OptionQ] :=
    Message[MathematicaPackageInstall::notazipfile, zipurl];

MathematicaPackageInstall[url_String?(!urlQ[#]&), ___?OptionQ] /; (!FileExistsQ[url]) :=
    Message[MathematicaPackageInstall::badurl, url];

MathematicaPackageInstall[zipurl_String?(zipQ[#] && urlQ[#]&), OptionsPattern[]] := Catch @
    Module[ {diropt = OptionValue[Directory], before, zip, priopt = OptionValue[Print], package, packdir, unzippedmasterdir},
      before = Select[#, DirectoryQ]& @ FileNames["*", diropt, 1];
      zip = CopyRemote`CopyRemote[zipurl, Print -> priopt];
      If[zip === $Failed, Throw[$Failed]];
      Unzip`Unzip[zip, diropt, Print -> priopt];
      DeleteFile[zip];
      (* https://github.com/rolfmertig/MathematicaPackageInstall/archive/master.zip *)
      If[ StringMatchQ[zipurl, $WebRepository ~~ __ ~~ "/archive/master.zip"],
      	(* ovewrite *)
      	package = StringReplace[zipurl, $WebRepository ~~ pack__ ~~ "/archive/master.zip" :> pack];
      	packdir = FileNameJoin[{diropt, package}];
      	If[DirectoryQ[packdir], DeleteDirectory[packdir, DeleteContents -> True]];
      	If[FileExistsQ[packdir], Pause[.2]];
      	unzippedmasterdir = FileNameJoin[{diropt, package <> "-master"}];
(*  the directory structure should be such that:*)
      	If[!DirectoryQ[ FileNameJoin[{unzippedmasterdir, package}] ], 
      		Message[MathematicaPackageInstaller::baddir, FileNameJoin[{unzippedmasterdir, package}]],
           RenameDirectory[FileNameJoin[{unzippedmasterdir, package}], packdir];
           DeleteDirectory[unzippedmasterdir, DeleteContents -> True]
      	]
      ];
      (* return the installed package, if it got installed ...  *)
      If[DirectoryQ[packdir], packdir, $Failed]
    ];

MathematicaPackageInstall[localzipfile_String?FileExistsQ, OptionsPattern[]] :=
    Unzip`Unzip[localzipfile, OptionValue[Directory], Print -> OptionValue[Print]];

MathematicaPackageUninstall[context_String /; (!packQ[context]), opts___] :=
	MathematicaPackageUninstall[context<>"`", opts];

MathematicaPackageUninstall[context_String /; packQ[context], OptionsPattern[]] /;
    Block[ {$Path = FileNameJoin[{$UserBaseDirectory, "Applications"}]},
      FindFile[context] =!= $Failed
    ] :=
    DeleteDirectory[Function[d, If[ FileNameTake[d, -1] === "Kernel",
      ParentDirectory[d],
      d
    ]][DirectoryName[FindFile[context]]],
      DeleteContents -> True];
      
MathematicaPackageUninstall[context_String /; StringMatchQ[context, "*`"], OptionsPattern[]] /;
    Block[ {$Path = FileNameJoin[{$UserBaseDirectory, "Applications"}]},
      FindFile[context] === $Failed
    ] := Null; (* or return a message ? *)

With[ {list = {MathematicaPackageInstall, InstallPalette, MathematicaPackageUninstall, UninstallPalette}},
  SetAttributes[list, ReadProtected];
  Protect[list]
];


End[]

EndPackage[]


(* check for a setting in the Mathematica preferences *)
If[ ("AllowInternetUse" /. SystemInformation["Network"]) === False,
  Print["You have configured Mathematica not to access the internet.
    Before using MathematicaPackageInstall or InstallPalette please check the \"Allow Mathematica to use the Internet\" box in the
    Help \[FilledRightTriangle] Internet Connectivity dialog." ];
];

(* should be on the Path, therefore do this after EndPackage[] : *)
If[!MemberQ[$Packages, "CopyRemote`"], MathematicaPackageInstall`URLNeeds @ "CopyRemote`"];
If[!MemberQ[$Packages, "Unzip`"], MathematicaPackageInstall`URLNeeds @ "Unzip`"];

If[ MathematicaPackageInstall`$AutomaticInstall =!= False,
  If[ Quiet[FindFile["MathematicaPackageInstall`"]] === $Failed, MathematicaPackageInstall["MathematicaPackageInstall`"] ];
  If[ Quiet[FindFile["CopyRemote`"]] === $Failed, MathematicaPackageInstall["CopyRemote`"] ];
  If[ Quiet[FindFile["Unzip`"]] === $Failed, MathematicaPackageInstall["Unzip`"] ];
];

(* 

Import["https://raw.githubusercontent.com/rolfmertig/MathematicaPackageInstall/master/MathematicaPackageInstall/MathematicaPackageInstall.m"]

(* this function will copy the palette locally  to
   FileNameJoin[{$UserBaseDirectory, "SystemFiles", "FrontEnd", "Palettes", "ButtonTools.nb"}]
and open  it 
*)

MathematicaPackageInstall`InstallPalette["http://www.mertig.com/mathdepot/ButtonTools.nb"];

(* this will install the palette here:
   FileNameJoin[{$UserBaseDirectory, "SystemFiles", "FrontEnd", "Palettes", "ButtonTools.nb"}]];
*)
]
*)