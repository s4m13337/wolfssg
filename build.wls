#!/usr/bin/env wolframscript

scriptPath = DirectoryName[ExpandFileName[First[$ScriptCommandLine]]];

(* Templates *)
baseTemplate = FileNameJoin[{"templates", "base.html"}];
blogTemplate = FileNameJoin[{"templates", "blog.html"}];
baseTemplateString = FileTemplate[baseTemplate];
blogTemplateString = FileTemplate[blogTemplate];

(* Pages *)
CreateDirectory["tmp"];
inputPages = FileNames[All, "pages"];
tmpPages = FileNameJoin[{"tmp", FileBaseName[#] <> ".html"}] & /@ inputPages;
MapThread[
    RunProcess[{"pandoc", "-f", "markdown-smart", #1, "-o", #2}]&, 
    {inputPages, tmpPages}
];

(* Apply HTML content to base template *)
buildPages = FileNameJoin[{"build", FileBaseName[#] <> ".html"}] & /@ inputPages;
buildTitles = Capitalize[FileBaseName[#]] & /@ inputPages;
buildContent = Import[#, "String"] & /@ tmpPages;

generatePage[title_, content_] := Module[
    {pageOutput},
    pageOutput = TemplateApply[
        baseTemplateString, 
        <|
            "title" -> "s4m13337 - " <> title,
            "homeLink" -> "home.html",
            "aboutLink" -> "about.html",
            "blogLink" -> "blog.html",
            "contactLink" -> "contact.html",
            "content" -> content
        |>
    ];
    pageOutput
]

Print[buildContent]

MapThread[
    Export[
        #1,
        generatePage[#2, #3],
        "Text"
    ]&,
    {buildPages, buildTitles, buildContent}
]

(* Posts *)
CreateDirectory[FileNameJoin[{"tmp","posts"}]];
inputPosts = FileNames[All, "posts"];
tmpPosts = FileNameJoin[{"tmp", "posts", FileBaseName[#] <> ".html"}] & /@ inputPosts;
MapThread[RunProcess[{"pandoc", #1, "-o", #2}]&, {inputPosts, tmpPosts}];

(* Function to extract front matter*)
extractFrontMatter[file_] := With[
    {content = Import[file, "Text"]},
    ToExpression[
        "<|" <> StringTrim[
            StringReplace[
                First[StringCases[content, "---\n" ~~ x__ ~~ "---" :> x]],
                {":" -> "->", "\n" -> ","}
            ], ","
        ] <> "|>"
    ]
]

postsFrontMatter = extractFrontMatter[#]& /@ inputPosts ;
postsContent = Import[#, "Text"]& /@ tmpPosts;

(* Apply contents to templates *)
generatePost[frontMatter_, content_] := Module[
    {postOutput, blogOutput},
    postOutput = TemplateApply[
        blogTemplateString, 
        <|
            "postTitle" -> frontMatter["title"], 
            "postDate" -> frontMatter["date"],
            "postContent" -> content
        |>
    ];

    blogOutput = TemplateApply[
        baseTemplateString,
        <|
            "title" -> "s4m13337 " <> frontMatter["title"],
            "homeLink" -> "../home.html",
            "aboutLink" -> "../about.html",
            "blogLink" -> "../blog.html",
            "contactLink" -> "../contact.html",
            "content" -> postOutput
        |>
    ];
    blogOutput
]

(* Build posts and put them in build directory *)
buildPosts = FileNameJoin[{"build", "posts", FileBaseName[#] <> ".html"}] & /@ inputPosts;
htmlPosts = MapThread[generatePost[#1, #2]&, {postsFrontMatter, postsContent}]
MapThread[
    Export[
        #1,
        #2,
        "String"
    ]&,
    {buildPosts, htmlPosts}
]

DeleteDirectory["tmp", DeleteContents -> True];