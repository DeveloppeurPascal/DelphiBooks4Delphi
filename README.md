# Delphi client library for Delphi-Books.com API

[Cette page en français.](LISEZMOI.md)

A set of examples of how to use the open data API of the Delphi Books website in Delphi.

This code repository contains a project developed in Object Pascal language under Delphi. You don't know what Delphi is and where to download it ? You'll learn more [on this web site](https://delphi-resources.developpeur-pascal.fr/).

## Projects list

### Folder "REST-Debugger"

Contains files for [REST Debugger](https://www.embarcadero.com/free-tools/rest-debugger) tool available from [Embarcadero website](https://www.embarcadero.com/) (in "Free Tools") and from Tools menu in RAD Studio/Delphi/C++Builder IDE. 

You can use it to test the API (like in [postman](https://www.postman.com)) and generate components to paste directly in your VCL or FMX projects.

### Folder "samples/FMX-Books-List-Sample"

Contains a sample FMX projects to use on Windows, macOS, Linux, Android tablets and iPad/iPad mini.

It access to the list of all books and display the list. For each book the cover is downloaded and displayed when needed.

### Folder "samples/VCL-ListBooks"

VCL sample with list of books and authors in a TListBox

### Folder "samples/VCL-ListBooks-Search"

VCL sample with list of books and authors in a TFDMemTable to allow searching in the list.

## Talks and conferences

### Learn To Code Summer Camp 2021

* [Services web et accès réseau sous Delphi](https://apprendre-delphi.fr/ltcsc2021-02.php) (in French)

### Twitch

Follow my live game development coding sessions on [my Twitch channel](https://www.twitch.tv/patrickpremartin) or as replays on [Serial Streameur](https://serialstreameur.fr/delphi-books.php) mostly in French.

## Source code installation

To download this code repository, we recommend using "git", but you can also download a ZIP file directly from [its GitHub repository](https://github.com/DeveloppeurPascal/DelphiBooks4Delphi).

This project uses dependencies in the form of sub-modules. They will be absent from the ZIP file. You'll have to download them by hand.

* [DeveloppeurPascal/librairies](https://github.com/DeveloppeurPascal/librairies) must be installed in the ./lib-externes/librairies subfolder.

## Documentation and support

I use comments in [XMLDOC](https://docwiki.embarcadero.com/RADStudio/en/XML_Documentation_Comments) format in Delphi to document my projects. They are recognized by Help Insight, which offers real-time input help in the code editor.

I regularly use the [DocInsight](https://devjetsoftware.com/products/documentation-insight/) tool to enter them and check their formatting.

Documentation is exported in HTML by [DocInsight](https://devjetsoftware.com/products/documentation-insight/) or [PasDoc](https://pasdoc.github.io) to the /docs folder of the repository. You can also [access it online](https://developpeurpascal.github.io/DelphiBooks4Delphi) through the hosting offered by GitHub Pages.

Further information (tutorials, articles, videos, FAQ, talks and links) can be found on [the project website](https://delphibooks4delphi.developpeur-pascal.fr/) or [the project devlog](https://developpeur-pascal.fr/delphi-books.html).

If you need explanations or help in understanding or using parts of this project in yours, please [contact me](https://developpeur-pascal.fr/nous-contacter.php). I can either direct you to an online resource, or offer you assistance in the form of a paid or free service, depending on the case. You can also contact me at a conference or during an online presentation.

## Compatibility

As an [Embarcadero MVP](https://www.embarcadero.com/resources/partners/mvp-directory), I benefit from the latest versions of [Delphi](https://www.embarcadero.com/products/delphi) and [C++ Builder](https://www.embarcadero.com/products/cbuilder) in [RAD Studio](https://www.embarcadero.com/products/rad-studio) as soon as they are released. I therefore work with these versions.

Normally, my libraries and components should also run on at least the current version of [Delphi Community Edition](https://www.embarcadero.com/products/delphi/starter).

There's no guarantee of compatibility with earlier versions, even though I try to keep my code clean and avoid using too many of the new ways of writing in it (type inference, inline var and multiline strings).

If you detect any anomalies on earlier versions, please don't hesitate to [report them](https://github.com/DeveloppeurPascal/DelphiBooks4Delphi/issues) so that I can test and try to correct or provide a workaround.

## License to use this code repository and its contents

This source code is distributed under the [AGPL 3.0 or later](https://choosealicense.com/licenses/agpl-3.0/) license.

You are free to use the contents of this code repository anywhere provided :
* you mention it in your projects
* distribute the modifications made to the files provided in this AGPL-licensed project (leaving the original copyright notices (author, link to this repository, license) must be supplemented by your own)
* to distribute the source code of your creations under the AGPL license.

If this license doesn't suit your needs (especially for a commercial project) I also offer [classic licenses for developers and companies](https://delphibooks4delphi.developpeur-pascal.fr/).

Some elements included in this repository may depend on third-party usage rights (images, sounds, etc.). They are not reusable in your projects unless otherwise stated.

The source codes of this code repository as well as any compiled version are provided “as is” without warranty of any kind.

## How to ask a new feature, report a bug or a security issue ?

If you want an answer from the project owner the best way to ask for a new feature or report a bug is to go to [the GitHub repository](https://github.com/DeveloppeurPascal/DelphiBooks4Delphi) and [open a new issue](https://github.com/DeveloppeurPascal/DelphiBooks4Delphi/issues).

If you found a security issue please don't report it publicly before a patch is available. Explain the case by [sending a private message to the author](https://developpeur-pascal.fr/nous-contacter.php).

You also can fork the repository and contribute by submitting pull requests if you want to help. Please read the [CONTRIBUTING.md](CONTRIBUTING.md) file.

## Support the project and its author

If you think this project is useful and want to support it, please make a donation to [its author](https://github.com/DeveloppeurPascal). It will help to maintain the code and binaries.

You can use one of those services :

* [GitHub Sponsors](https://github.com/sponsors/DeveloppeurPascal)
* Ko-fi [in French](https://ko-fi.com/patrick_premartin_fr) or [in English](https://ko-fi.com/patrick_premartin_en)
* [Patreon](https://www.patreon.com/patrickpremartin)
* [Liberapay](https://liberapay.com/PatrickPremartin)
* [Paypal](https://www.paypal.com/paypalme/patrickpremartin)

You can buy [my softwares](https://lic.olfsoftware.fr/products.php?lng=en), [my video games](https://lic.gamolf.fr/products.php?lng=en) or [a developer license for my libraries](https://lic.developpeur-pascal.fr/products.php?lng=en) if you use them in your projects.

If you speak French [subscribe to Zone Abo](https://zone-abo.fr/nos-abonnements.php) to access my complete online archive (articles, videos, training videos, ebooks).
