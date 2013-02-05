(* ::Package:: *)

(* ::Author:: Rolf Mertig , GluonVision GmbH,  http://www.mertig.com *)

(* ::License:: LGPL *)

(* ::Title:: *)

(* Mathematica Package and Palette Installer 1.0 *)

(* Provides a simple JLink-based web-installer of zip packages containing Mathematica Application Packages *)

(* ::Context:: PackageInstaller` *)

(* This  can be useful to uncomment: 
If[$Notebooks && ($VersionNumber < 9), SetOptions[$FrontEnd, MessageOptions -> {"InsufficientVersionWarning" -> False}]]; 
*)

BeginPackage["PackageInstaller`"];

Unprotect @@ {InstallPackage, InstallPalette, UninstallPackage, UninstallPalette};
ClearAll @@ {InstallPackage, InstallPalette, UninstallPackage, UninstallPalette};

InstallPackage::usage = "InstallPackage[\"MyPackage`\"] downloads and unzips MyPackage.zip 
from $WebRepository (http://mypackage.googlecode.com/MyPackage/MyPackage.zip by default) 
to the directory specified by the option Directory (installing to $UserBaseDirectory/Applications by default)"

InstallPalette::usage = "InstallPalette[\"https://dl.dropbox.com/u/38623/SE%20Uploader.nb\"] installs SE Uploader.nb into 
the default directory specified by the option Directory.";

UninstallPalette::usage = "UninstallPalette[\"SE Uploader.nb\"] uninstalls ButtonTools.nb from the directory specified by the option
Directory.";

UninstallPackage::usage = "UninstallPackage[context] returns Hold[DeleteDirectory][packagedirofcontext]. Please inspect the 
result and apply ReleaseHold onto it if you really want to delete it.";
 
$InstallerDirectory::usage = "$InstallerDirectory is the directory to install packages to. 
	It is set by default to FileNameJoin[{$UserBaseDirectory,\"Applications\"}].";

$AutoInstall::usage = "If $AutoInstall is set to True, 
the three packages Installer`, CopyRemote` and Unzip` are installed locally already when loading this
package the first time. Otherwise they are loaded from the internet.";

$WebRepository::usage = "$WebRepository is the base url (like googlecode.com/hg/) where the packages are.";

Installer::notazipfile = "The URL `1` is not referring to a zip file.";
Installer::badurl = "The URL `1` does not exist or is not reachable.";

URLNeeds::usage="URLNeeds[package`] loads package.m from http://package/$WebRepository/package";

(* this is customizable of course, 
   so you may do 
PackageInstaller`$WebRepository = "googlecode.com/hg/"; 
or
PackageInstaller`$WebRepository = "googlecode.com/git/"; 
   before loading this package 
   
The default for now is googlecode.com/hg/  
*)

If[ !ValueQ[$WebRepository], $WebRepository = "googlecode.com/hg/"; ];

(* don't install package by default, but provide the possibility *)
(* It is possible to install The basic packages right away, but not by default for now 
If[!ValueQ[$AutoInstall], $AutoInstall = True];
*)
If[ !ValueQ[$AutoInstall],
    $AutoInstall = False
];

If[ !ValueQ[$InstallerDirectory],
    $InstallerDirectory :=
        FileNameJoin[{$UserBaseDirectory, "Applications"}]
];

(* TODO: implement some kind of uptdate mechanism of this and the dependent packages
   If[!ValueQ[$AutoUpdate], $AutoUpdate =  True];
*)

Begin["`Private`"];

(* auxiliary functions *)

(* first call Needs, then go to $WebRepository, where we assume that the projectname is
ToLowerCase[package] and we assume a normal Mathematica application project structure *)
URLNeeds = Function[package, 
                     Quiet[If[ FindFile[package] =!= $Failed,
                               Needs[package],
                               Import[ "http://" <>
                               	StringReplace[
                               	ToLowerCase[ StringReplace[package, "`" -> ""] ] <> "." <> 
                               	 $WebRepository <> "/" <> StringReplace[package, "`" -> ""] <> "/" <>
                               	  StringReplace[package, a__~~"`"  :> (a ~~ ".m")], "//" -> "/"]]
                           ]]
];


nbQ = StringMatchQ[#, "*.nb", IgnoreCase -> True]&;
zipQ = StringMatchQ[#, "*.zip", IgnoreCase -> True]&;
urlQ = CopyRemote`URLQ;

(*  a NoetbookClose function which does nothing if $Notebooks is False *)
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
    InstallPalette[ url  <> nb, ops] /; StringTake[url, -1] === "/";
    
InstallPalette[url_String?urlQ, nb_String?nbQ, ops___?OptionQ] :=
    InstallPalette[ url  <> "/" <> nb, ops] /; StringTake[url, -1] =!= "/";
    
InstallPalette[url_String?(nbQ[#] && !urlQ[#]&), ___] :=
    Message[Installer::badurl, url];
    
InstallPalette[url_String?(nbQ[#] &&  urlQ[#]&), OptionsPattern[]] := Module[{},
(* open the palette by default if we have a notebook interface *)
    closeandfix @ CopyRemote`CopyRemote[ url, OptionValue[Directory], Print -> OptionValue[Print]];
    If[$Notebooks,
       (* R.M & Szabolcs mentioned: http://mathematica.stackexchange.com/questions/8563/refreshing-the-palettes-menu/8564#8564 *)
       MathLink`CallFrontEnd[FrontEnd`ResetMenusPacket[{Automatic, Automatic}]];
       (* and http://mathematica.stackexchange.com/a/8604/12 *)
       FrontEndTokenExecute["OpenFromPalettesMenu", StringReplace[FileNameTake[url], "%20"->" "]]
    ]
    ];

UninstallPalette[pal_String, OptionsPattern[]] :=
    Module[ {dir, palfile},
        dir = OptionValue[Directory];
        palfile = FileNameJoin[{dir, pal}];
        If[ FileExistsQ[palfile],
            DeleteFile[palfile]
        ]
    ];

(* InstallPackage *)

packQ[s_String /; StringLength[s] > 1] :=
    StringTake[s,-1] === "`";
packQ[_] :=
    False;

Options[InstallPackage] = {Directory :> $InstallerDirectory, Print -> False};

(* Enable InstallPackage["CopyRemote`"] *)
InstallPackage[p_String /; (packQ[p] && !StringMatchQ[p, "http*", IgnoreCase -> True]), ops___?OptionQ] :=
    InstallPackage[$WebRepository, StringReplace[p,"`"->".zip"], ops];

InstallPackage[webrep_?urlQ, p_String?zipQ, ops___?OptionQ] :=
    InstallPackage[ webrep <> p, ops] /; StringTake[webrep, -1] === "/";
    
InstallPackage[webrep_?urlQ, p_String?zipQ, ops___?OptionQ] :=
    InstallPackage[ webrep <> "/" <> p, ops] /; StringTake[webrep, -1] =!= "/";

InstallPackage[zipurl_String?(!zipQ[#]&), ___?OptionQ] :=
    Message[Installer::notazipfile, zipurl];
    
InstallPackage[url_String?(!urlQ[#]&), ___?OptionQ] /; (!FileExistsQ[url]) :=
    Message[Installer::badurl, url];

InstallPackage[zipurl_String?(zipQ[#] && urlQ[#]&), OptionsPattern[]] := Catch @ 
    Module[ {diropt = OptionValue[Directory], zip, priopt = OptionValue[Print]},
    	zip = 
        CopyRemote`CopyRemote[zipurl, Print ->priopt];
        If[zip === $Failed, Throw[$Failed]];
        Unzip`Unzip[zip, diropt, Print -> priopt];
        DeleteFile[zip];
        diropt
    ];
    
 InstallPackage[localzipfile_String?FileExistsQ, OptionsPattern[]] :=
 	Unzip`Unzip[localzipfile, OptionValue[Directory], Print -> OptionValue[Print]];

UninstallPackage[context_String /; StringMatchQ[context,"*`"], OptionsPattern[]] /; 
   Block[ {$Path = FileNameJoin[{$UserBaseDirectory, "Applications"}]},
       FindFile[context] =!= $Failed
   ] :=
    Hold[DeleteDirectory][Function[d, If[ FileNameTake[d,-1]==="Kernel",
                                          ParentDirectory[d],
                                          d
                                      ]][DirectoryName[FindFile[context]]],
      DeleteContents->True];

With[ {list = {InstallPackage, InstallPalette, UninstallPackage, UninstallPalette}},
    SetAttributes[list, ReadProtected];
    Protect[list]
];

End[]

EndPackage[]


(* check for a setting in the Mathematica preferences *)
If[ ("AllowInternetUse" /. SystemInformation["Network"]) === False,
    Print["You have configured Mathematica not to access the internet. 
    Before using InstallPackage or InstallPalette please check the \"Allow Mathematica to use the Internet\" box in the
    Help \[FilledRightTriangle] Internet Connectivity dialog." ];
];

(* should be on the Path, therefore do this after EndPackage[] : *)
PackageInstaller`URLNeeds /@ {"CopyRemote`", "Unzip`"};

If[ PackageInstaller`$AutoInstall =!= False,
    If[ Quiet[FindFile["PackageInstaller`"]] === $Failed,
        InstallPackage["http://pacakgeinstaller.googlecode.com/hg/", "PackageInstaller.zip"]
    ];
    If[ Quiet[FindFile["CopyRemote`"]] === $Failed,
        InstallPackage["http://copyremote.googlecode.com/hg/", "CopyRemote.zip"]
    ];
    If[ Quiet[FindFile["Unzip`"]] === $Failed,
        InstallPackage["http://unzip.googlecode.com.com/hg/", "Unzip.zip"]
    ];
];

(* 
If[$VersionNumber < 8, 
   Print["SE Uploader requires Mathematica 8 or higher"],
(* get the Installer:*)

Import["http://packageinstaller.googlecode.com/hg/PackageInstaller/PackageInstaller.m"]; 

(* this is the location of the uploader as advertised here:
http://meta.mathematica.stackexchange.com/questions/5/can-i-easily-post-images-to-this-site-directly-from-mathematica-yes*)

(* this function will copy the palette locally  to 
   FileNameJoin[{$UserBaseDirectory, "SystemFiles", "FrontEnd", "Palettes", "SE Uploader.nb"}] 
and open  it 
*)

PackageInstaller`InstallPalette["https://dl.dropbox.com/u/38623/SE%20Uploader.nb"];

(* this will install the palette here:
   FileNameJoin[{$UserBaseDirectory, "SystemFiles", "FrontEnd", "Palettes", "SE Uploader.nb"}]]; 
*)
]

*)