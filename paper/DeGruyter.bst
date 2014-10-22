%%
%% This is file `DeGruyter.bst',
%%
 % ===============================================================
 % IMPORTANT NOTICE:
 %
 % This generated file can be redistributed and/or modified under the terms
 % of the LaTeX Project Public License Distributed from CTAN
 % archives in directory macros/latex/base/lppl.txt; either
 % version 1 of the License, or any later version.
 % ===============================================================
 % This bibliography style file is intended for texts in ENGLISH
 % This is an author-year citation style bibliography. As such, it is
 % non-standard LaTeX, and requires a special package file to function properly.
 % Such a package is    natbib.sty   by Patrick W. Daly
 % The form of the \bibitem entries is
 %   \bibitem[Jones et al.(1990)]{key}...
 %   \bibitem[Jones et al.(1990)Jones, Baker, and Smith]{key}...
 % The essential feature is that the label (the part in brackets) consists
 % of the author names, as they should appear in the citation, with the year
 % in parentheses following. There must be no space before the opening
 % parenthesis!
 % With natbib v5.3, a full list of authors may also follow the year.
 % In natbib.sty, it is possible to define the type of enclosures that is
 % really wanted (brackets or parentheses), but in either case, there must
 % be parentheses in the label.
 % The \cite command functions as follows:
 %   \citet{key} ==>>                Jones et al. (1990)
 %   \citet*{key} ==>>               Jones, Baker, and Smith (1990)
 %   \citep{key} ==>>                (Jones et al., 1990)
 %   \citep*{key} ==>>               (Jones, Baker, and Smith, 1990)
 %   \citep[chap. 2]{key} ==>>       (Jones et al., 1990, chap. 2)
 %   \citep[e.g.][]{key} ==>>        (e.g. Jones et al., 1990)
 %   \citep[e.g.][p. 32]{key} ==>>   (e.g. Jones et al., p. 32)
 %   \citeauthor{key} ==>>           Jones et al.
 %   \citeauthor*{key} ==>>          Jones, Baker, and Smith
 %   \citeyear{key} ==>>             1990
 %---------------------------------------------------------------------

ENTRY
  { address
    author
    booktitle
    chapter
    edition
    editor
    eid
    howpublished
    institution
    journal
    key
    month
    note
    number
    organization
    pages
    publisher
    school
    series
    title
    type
    url
    volume
    year
  }
  {}
  { label extra.label sort.label short.list }
INTEGERS { output.state before.all mid.sentence after.sentence after.block }
FUNCTION {init.state.consts}
{ #0 'before.all :=
  #1 'mid.sentence :=
  #2 'after.sentence :=
  #3 'after.block :=
}
STRINGS { s t}
FUNCTION {output.nonnull}
{ 's :=
  output.state mid.sentence =
    { ", " * write$ }
    { output.state after.block =
        { add.period$ write$
          newline$
          "\newblock " write$
        }
        { output.state before.all =
            'write$
            { add.period$ " " * write$ }
          if$
        }
      if$
      mid.sentence 'output.state :=
    }
  if$
  s
}
FUNCTION {output}
{ duplicate$ empty$
    'pop$
    'output.nonnull
  if$
}
FUNCTION {output.check}
{ 't :=
  duplicate$ empty$
    { pop$ "empty " t * " in " * cite$ * warning$ }
    'output.nonnull
  if$
}
FUNCTION {fin.entry}
{ add.period$
  write$
  newline$
}

FUNCTION {new.block}
{ output.state before.all =
    'skip$
    { after.block 'output.state := }
  if$
}
FUNCTION {new.sentence}
{ output.state after.block =
    'skip$
    { output.state before.all =
        'skip$
        { after.sentence 'output.state := }
      if$
    }
  if$
}
FUNCTION {add.blank}
{  " " * before.all 'output.state :=
}

FUNCTION {date.block}
{
  ":" *
  add.blank
}

FUNCTION {not}
{   { #0 }
    { #1 }
  if$
}
FUNCTION {and}
{   'skip$
    { pop$ #0 }
  if$
}
FUNCTION {or}
{   { pop$ #1 }
    'skip$
  if$
}
FUNCTION {non.stop}
{ duplicate$
   "}" * add.period$
   #-1 #1 substring$ "." =
}

FUNCTION {new.block.checkb}
{ empty$
  swap$ empty$
  and
    'skip$
    'new.block
  if$
}
FUNCTION {field.or.null}
{ duplicate$ empty$
    { pop$ "" }
    'skip$
  if$
}
FUNCTION {emphasize}
{ duplicate$ empty$
    { pop$ "" }
    { "\emph{" swap$ * "}" * }
  if$
}
FUNCTION {tie.or.space.prefix}
{ duplicate$ text.length$ #3 <
    { "~" }
    { " " }
  if$
  swap$
}

FUNCTION {capitalize}
{ "u" change.case$ "t" change.case$ }

FUNCTION {space.word}
{ " " swap$ * " " * }
 % Here are the language-specific definitions for explicit words.
 % Each function has a name bbl.xxx where xxx is the English word.
 % The language selected here is ENGLISH
FUNCTION {bbl.and}
{ "and"}

FUNCTION {bbl.etal}
{ "et~al." }

FUNCTION {bbl.editors}
{ "eds." }

FUNCTION {bbl.editor}
{ "ed." }

FUNCTION {bbl.edby}
{ "edited by" }

FUNCTION {bbl.edition}
{ "edition" }

FUNCTION {bbl.volume}
{ "volume" }

FUNCTION {bbl.of}
{ "of" }

FUNCTION {bbl.number}
{ "number" }

FUNCTION {bbl.nr}
{ "no." }

FUNCTION {bbl.in}
{ "in" }

FUNCTION {bbl.pages}
{ "" }

FUNCTION {bbl.page}
{ "" }

FUNCTION {bbl.chapter}
{ "chapter" }

FUNCTION {bbl.techrep}
{ "Technical Report" }

FUNCTION {bbl.mthesis}
{ "Master's thesis" }

FUNCTION {bbl.phdthesis}
{ "Ph.D. thesis" }

MACRO {jan} {"January"}

MACRO {feb} {"February"}

MACRO {mar} {"March"}

MACRO {apr} {"April"}

MACRO {may} {"May"}

MACRO {jun} {"June"}

MACRO {jul} {"July"}

MACRO {aug} {"August"}

MACRO {sep} {"September"}

MACRO {oct} {"October"}

MACRO {nov} {"November"}

MACRO {dec} {"December"}

 %-------------------------------------------------------------------
 % Begin module:
 % \ProvidesFile{econjour.mbs}[2006/09/25 1.0]
%%--------------------------------------------------------------------------------------
%%	Economics Journals from the JEL Abbreviation List
%%--------------------------------------------------------------------------------------
%%	Journals from the JEL's abbreviations for journal names
%%	at http://www.aeaweb.org/journal/abbrev.html
%%    This includes most but not all the journals indexed in EconLit
%%	Additional journals not listed by the JEL are included at the end
%%--------------------------------------------------------------------------------------
MACRO {AcademiaEconPap} {"Academia Economic Papers"}
MACRO {AccBusFinanHist} {"Accounting, Business and Financial History"}
MACRO {AccRev} {"Accounting Review"}
MACRO {ACESBull} {"ACES Bulletin"}
MACRO {ActaOecon} {"Acta Oeconomica"}
MACRO {AdvancConsInterest} {"Advancing the Consumer Interest"}
MACRO {AfricanDevelRevRevueAfricaineDevel} {"African Development Review/Revue Africaine de Developpement"}
MACRO {AfricanEconHist} {"African Economic History"}
MACRO {AfricanFinanceJ} {"African Finance Journal"}
MACRO {AfrikaSpec} {"Afrika Spectrum"}
MACRO {Agenda} {"Agenda"}
MACRO {Agribusiness} {"Agribusiness"}
MACRO {AgrResourceEconRev} {"Agricultural and Resource Economics Review"}
MACRO {AgrEconRes} {"Agricultural Economics Research"}
MACRO {AgrEcon} {"Agricultural Economics"}
MACRO {AgrFinanceRev} {"Agricultural Finance Review"}
MACRO {AgrHumanValues} {"Agriculture and Human Values"}
MACRO {AgrResourcesQuart} {"Agriculture and Resources Quarterly"}
MACRO {AgronomiaMesoamericana} {"Agronomia Mesoamericana"}
MACRO {ASArchJGermanStatisticalSociety} {"Allgemeines Statistisches Archiv/Journal of the German Statistical Society"}
MACRO {AER} {"American Economic Review"}
MACRO {AmerEconomist} {"American Economist"}
MACRO {AmerEnterprise} {"American Enterprise"}
MACRO {AmerHistoricalRev} {"American Historical Review"}
MACRO {AmerJAgrEcon} {"American Journal of Agricultural Economics"}
MACRO {AmerJEconSociology} {"American Journal of Economics and Sociology"}
MACRO {AmerLawEconRev} {"American Law and Economics Review"}
MACRO {AmerPolitSciRev} {"American Political Science Review"}
MACRO {AmerProspect} {"American Prospect"}
MACRO {AmerRealEstateUrbanEconAssocJ} {"American Real Estate and Urban Economics Association Journal"}
MACRO {AmerStatistician} {"American Statistician"}
MACRO {AnalPrevision} {"Analyse Pr\'evision"}
MACRO {AnnEconStatist} {"Annales d'Economie et de Statistique"}
MACRO {AnnINSEE} {"Annales de l'INSEE"}
MACRO {AnnSciEconAppl} {"Annales de Sciences Economiques Appliqu\'es"}
MACRO {AnnFacEconComUnivPalermo} {"Annali della facolt‡ di Economia e Commercio, Universit‡ Palermo"}
MACRO {AnnEconSocMeas} {"Annals of Economic and Social Measurement"}
MACRO {AnnEconFinance} {"Annals of Economics and Finance"}
MACRO {AnnPublicCoopEcon} {"Annals of Public and Cooperative Economics"}
MACRO {AnnRegSci} {"Annals of Regional Science"}
MACRO {AnnAmerAcadPolitSocSci} {"Annals of the American Academy of Political and Social Science"}
MACRO {AntitrustBull} {"Antitrust Bulletin"}
MACRO {AntitrustLawEcon} {"Antitrust Law and Economics Review"}
MACRO {ApplEconLetters} {"Applied Economics Letters"}
MACRO {ApplEcon} {"Applied Economics"}
MACRO {ApplFinanEcon} {"Applied Financial Economics"}
MACRO {ApplGeogrDevel} {"Applied Geography and Development"}
MACRO {ApplMathFinance} {"Applied Mathematical Finance"}
MACRO {AquacultureEconManage} {"Aquaculture Economics and Management"}
MACRO {ArchivesEconHist} {"Archives of Economic History"}
MACRO {ArquivoInst} {"Arquivo do Instituto Gulbenkian de Ci\^encia"}
MACRO {ArthaVikas} {"Artha-Vikas"}
MACRO {Arthaniti} {"Arthaniti"}
MACRO {ASEANEconBull} {"ASEAN Economic Bulletin"}
MACRO {AsiaPacificBusRev} {"Asia Pacific Business Review"}
MACRO {AsiaPacificJEconBus} {"Asia Pacific Journal of Economics and Business"}
MACRO {AsiaPacificDevelJ} {"Asia-Pacific Development Journal"}
MACRO {AsiaPacificEconRev} {"Asia-Pacific Economic Review"}
MACRO {AsiaPacificFinanMarkets} {"Asia-Pacific Financial Markets"}
MACRO {AsianDevelRev} {"Asian Development Review"}
MACRO {AsianEconJ} {"Asian Economic Journal"}
MACRO {AsianEconRev} {"Asian Economic Review"}
MACRO {AsianJAgrEcon} {"Asian Journal of Agricultural Economics"}
MACRO {AsianPacificEconLit} {"Asian-Pacific Economic Literature"}
MACRO {AtlanticEconJ} {"Atlantic Economic Journal"}
MACRO {Aussenwirtschaft} {"Aussenwirtschaft"}
MACRO {AustralasianJRegStud} {"Australasian Journal of Regional Studies"}
MACRO {AustralianBullLab} {"Australian Bulletin of Labour"}
MACRO {AustralianCommoditiesForecastsIssues} {"Australian Commodities Forecasts and Issues"}
MACRO {AustralianEconHistRev} {"Australian Economic History Review"}
MACRO {AustralianEconPap} {"Australian Economic Papers"}
MACRO {AustralianEconRev} {"Australian Economic Review"}
MACRO {AustralianJAgrResourceEcon} {"Australian Journal of Agricultural and Resource Economics"}
MACRO {AustralianJAgrEcon} {"Australian Journal of Agricultural Economics"}
MACRO {AustralianJLabEcon} {"Australian Journal of Labour Economics"}
MACRO {AustralianJManage} {"Australian Journal of Management"}
MACRO {AustralianTaxForum} {"Australian Tax Forum"}
MACRO {BancaNazLavoroQuartRev} {"Banca Nazionale del Lavoro Quarterly Review"}
MACRO {Bancaria} {"Bancaria"}
MACRO {BancoEspanaEconBull} {"Banco de Espa\~na Economic Bulletin"}
MACRO {BangladeshDevelStud} {"Bangladesh Development Studies"}
MACRO {BangladeshEconRev} {"Bangladesh Economic Review"}
MACRO {BankEnglQuartBull} {"Bank of England Quarterly Bulletin"}
MACRO {BankIsraelBankingRev} {"Bank of Israel Banking Review"}
MACRO {BankIsraelEconRev} {"Bank of Israel Economic Review"}
MACRO {BankJapanMonetEconStud} {"Bank of Japan Monetary and Economic Studies"}
MACRO {BankKoreaEconPap} {"Bank of Korea Economic Papers"}
MACRO {BankVallettaRev} {"Bank of Valletta Review"}
MACRO {Banker} {"Banker"}
MACRO {BehavioralResAcc} {"Behavioral Research in Accounting"}
MACRO {BellJEcon} {"Bell Journal of Economics"}
MACRO {BolMusSocArgent} {"Boletin del Musea Social Argentino"}
MACRO {BrazilianElectronicJEcon} {"Brazilian Electronic Journal of Economics"}
MACRO {BrazilianRevEconometrics} {"Brazilian Review of Econometrics"}
MACRO {BritJIndRelat} {"British Journal of Industrial Relations"}
MACRO {BritRevEconIssues} {"British Review of Economic Issues"}
MACRO {BrookingsPapEconAct} {"Brookings Papers on Economic Activity"}
MACRO {BrookingsWhartonPapFinanServices} {"Brookings-Wharton Papers on Financial Services"}
MACRO {BrookingsWhartonPapUrbanAff} {"Brookings-Wharton Papers on Urban Affairs"}
MACRO {BuffaloLawRev} {"Buffalo Law Review"}
MACRO {BullIntFiscalDoc} {"Bulletin for International Fiscal Documentation"}
MACRO {BullEconRes} {"Bulletin of Economic Research"}
MACRO {BullIndonesianEconStud} {"Bulletin of Indonesian Economic Studies"}
MACRO {BullOxfordUnivInstEconStatist} {"Bulletin of the Oxford University Institute of Economics and Statistics"}
MACRO {BullUnivOsakaPrefect} {"Bulletin of University of Osaka Prefecture"}
MACRO {BusEconHist} {"Business and Economic History"}
MACRO {BusEcon} {"Business Economics"}
MACRO {BusHistRev} {"Business History Review"}
MACRO {BusHist} {"Business History"}
MACRO {CahEconSociologieRurales} {"Cahiers d'Economie et Sociologie Rurales"}
MACRO {CahEconPolitique} {"Cahiers d'Economie Politique"}
MACRO {CahEconBruxelles} {"Cahiers Economiques de Bruxelles"}
MACRO {CahEconSoc} {"Cahiers Economiques et Sociaux"}
MACRO {CalifManageRev} {"California Management Review"}
MACRO {CambridgeEconPolRev} {"Cambridge Economic Policy Review"}
MACRO {CambridgeJEcon} {"Cambridge Journal of Economics"}
MACRO {CanBusEcon} {"Canadian Business Economics"}
MACRO {CanJAgrEcon} {"Canadian Journal of Agricultural Economics"}
MACRO {CanJDevelStud} {"Canadian Journal of Development Studies"}
MACRO {CanJEcon} {"Canadian Journal of Economics"}
MACRO {CanJRegSci} {"Canadian Journal of Regional Science"}
MACRO {CanPublicPol} {"Canadian Public Policy"}
MACRO {CanTaxJ} {"Canadian Tax Journal"}
MACRO {CarnegieRochesterConfSerPublicPol} {"Carnegie-Rochester Conference Series on Public Policy"}
MACRO {CatoJ} {"Cato Journal"}
MACRO {CEMLABolMens} {"CEMLA Boletin Mensual"}
MACRO {CentralEuropJOperationsResEcon} {"Central European Journal for Operations Research and Economics"}
MACRO {CentralEuropJOperationsRes} {"Central European Journal of Operations Research"}
MACRO {CEPALRev} {"CEPAL Review"}
MACRO {Challenge} {"Challenge"}
MACRO {ChinaEconRev} {"China Economic Review"}
MACRO {ChinaQuart} {"China Quarterly"}
MACRO {ChineseEconStud} {"Chinese Economic Studies"}
MACRO {ChineseEconomy} {"Chinese Economy"}
MACRO {CIRIECERevistaEconPublicaSocCooperativa} {"CIRIEC-Espa\~na, Revista de Econom\'\ia P\'ublica, Social y Cooperativa"}
MACRO {CityscapeJPolDevelRes} {"Cityscape: A Journal of Policy Development and Research"}
MACRO {ColeccionEstudCIEPLAN} {"Colecci\'on Estudios CIEPLAN"}
MACRO {ColumbiaJWorldBus} {"Columbia Journal of World Business"}
MACRO {ComercioExterior} {"Comercio Exterior"}
MACRO {ComertulMod} {"Comertul Modern"}
MACRO {CommunicationsStrategies} {"Communications and Strategies"}
MACRO {CommunistEconomiesEconTransformation} {"Communist Economies and Economic Transformation"}
MACRO {ComparEconStud} {"Comparative Economic Studies"}
MACRO {ComputationalEcon} {"Computational Economics"}
MACRO {ComputerSciEconManage} {"Computer Science in Economics and Management"}
MACRO {ConflictManagePeaceSci} {"Conflict Management and Peace Science"}
MACRO {Consommation} {"Consommation"}
MACRO {ConstitPolitEconomy} {"Constitutional Political Economy"}
MACRO {ContemporaryEconPol} {"Contemporary Economic Policy"}
MACRO {ContempPolIssues} {"Contemporary Policy Issues"}
MACRO {ContribtoPolitEconomy} {"Contributions to Political Economy"}
MACRO {CriticRev} {"Critical Review"}
MACRO {CuadernosEcon} {"Cuadernos de Economia"}
MACRO {CuadernosdelaCVF} {"Cuadernos de la C.V.F."}
MACRO {CuadernosEconICE} {"Cuadernos Economicos de I.C.E."}
MACRO {CyprusJEcon} {"Cyprus Journal of Economics"}
MACRO {CyprusRev} {"Cyprus Review"}
MACRO {CzechEconDigest} {"Czechoslovak Economic Digest"}
MACRO {CzechEconPap} {"Czechoslovak Economic Papers"}
MACRO {CzechJOperationsRes} {"Czechoslovak Journal for Operations Research"}
MACRO {DeEconomia} {"De Econom\'\ia"}
MACRO {DeEconomist} {"De Economist"}
MACRO {DefencePeaceEcon} {"Defence and Peace Economics"}
MACRO {DefenceEcon} {"Defence Economics"}
MACRO {DemogrEcon} {"Demograf\'\ia y Econom\'\ia"}
MACRO {Demography} {"Demography"}
MACRO {DerDonaurum} {"Der Donaurum"}
MACRO {DesarrolloEcon} {"Desarrollo Econ\'omico"}
MACRO {DevelopingEconomies} {"Developing Economies"}
MACRO {DevelChange} {"Development and Change"}
MACRO {DevelSouthernAfrica} {"Development Southern Africa"}
MACRO {Development} {"Development"}
MACRO {EastAfricanJRuralDevel} {"East African Journal of Rural Development"}
MACRO {EastWestJEconBus} {"East-West Journal of Economics and Business"}
MACRO {EastWestSerEconBusEnviron} {"East-West Series in Economics, Business, and the Environment"}
MACRO {EasternAfricaEconRev} {"Eastern Africa Economic Review"}
MACRO {EasternEconJ} {"Eastern Economic Journal"}
MACRO {EasternEuropEcon} {"Eastern European Economics"}
MACRO {EcologEcon} {"Ecological Economics"}
MACRO {EconomicaLaPlata} {"Econ\'omica (National University of La Plata)"}
MACRO {EconChilena} {"Econom\'\ia Chilena"}
MACRO {EconometricRev} {"Econometric Reviews"}
MACRO {ET} {"Econometric Theory"}
MACRO {Econometrica} {"Econometrica"}
MACRO {EconometricsJ} {"Econometrics Journal"}
MACRO {EconomiaPeru} {"Economia (Pontifical Catholic University of Peru)"}
MACRO {EconomiaPortugal} {"Economia (Portuguese Catholic University)"}
MACRO {EconAplicadaBrazilianJApplEcon} {"Economia Aplicada/Brazilian Journal of Applied Economics"}
MACRO {EconSceltePubJPublicFinancePublicChoice} {"Economia delle Scelte Pubbliche/Journal of Public Finance and Public Choice"}
MACRO {EconLavoro} {"Economia e Lavoro"}
MACRO {EconInd} {"Economia Industrial"}
MACRO {EconIntIntEcon} {"Economia Internazionale/International Economics"}
MACRO {EconInt} {"Economia Internazionale"}
MACRO {EconMexicanaNuevaEpoca} {"Economia Mexicana, Nueva Epoca"}
MACRO {EconPolitica} {"Economia Politica"}
MACRO {EconAff} {"Economic Affairs"}
MACRO {EconAnalPolNS} {"Economic Analysis and Policy, New Series"}
MACRO {EconAnalPol} {"Economic Analysis and Policy"}
MACRO {EconAnalWorkersManage} {"Economic Analysis and Workers' Management"}
MACRO {EconAnal} {"Economic Analysis"}
MACRO {EconBusBull} {"Economic and Business Bulletin"}
MACRO {EconBusRev} {"Economic and Business Review"}
MACRO {EconFinanModelling} {"Economic and Financial Modelling"}
MACRO {EconFinanRev} {"Economic and Financial Review"}
MACRO {EconIndDemocracy} {"Economic and Industrial Democracy"}
MACRO {EconLabRelatRev} {"Economic and Labour Relations Review"}
MACRO {EconSocRev} {"Economic and Social Review"}
MACRO {EconBullGhana2ndSer} {"Economic Bulletin of Ghana, Second Series"}
MACRO {EconComputatEconCybernStudRes} {"Economic Computation and Economic Cybernetics Studies and Research"}
MACRO {EconDesign} {"Economic Design"}
MACRO {EconDevelCultChange} {"Economic Development and Cultural Change"}
MACRO {EconDevelQuart} {"Economic Development Quarterly"}
MACRO {EconForum} {"Economic Forum"}
MACRO {EconGeogr} {"Economic Geography"}
MACRO {EconHistRev2ndSer} {"Economic History Review, Second Series"}
MACRO {EconHistRev} {"Economic History Review"}
MACRO {EconInq} {"Economic Inquiry"}
MACRO {EconIssues} {"Economic Issues"}
MACRO {EconJ} {"Economic Journal"}
MACRO {EconModelling} {"Economic Modelling"}
MACRO {EconNotes} {"Economic Notes"}
MACRO {EconPerspect} {"Economic Perspectives"}
MACRO {EconPolRev} {"Economic Policy Review"}
MACRO {EconPolAEuropForum} {"Economic Policy: A European Forum"}
MACRO {EconRec} {"Economic Record"}
MACRO {EconRevKeizaiKenkyu} {"Economic Review (Keizai Kenkyu)"}
MACRO {EconStudQuart} {"Economic Studies Quarterly"}
MACRO {EconSystemsRes} {"Economic Systems Research"}
MACRO {EconSystems} {"Economic Systems"}
MACRO {EconTheory} {"Economic Theory"}
MACRO {EconomicaNS} {"Economica, N.S."}
MACRO {Economica} {"Economica"}
MACRO {EconBusEduc} {"Economics and Business Education"}
MACRO {EconPhilos} {"Economics and Philosophy"}
MACRO {EconPolitics} {"Economics and Politics"}
MACRO {EconLetters} {"Economics Letters"}
MACRO {EconEducRev} {"Economics of Education Review"}
MACRO {EconGovernance} {"Economics of Governance"}
MACRO {EconInnovationNewTech} {"Economics of Innovation and New Technology"}
MACRO {EconPlanning} {"Economics of Planning"}
MACRO {EconTransition} {"Economics of Transition"}
MACRO {EconBiannCollecRecentGerStud} {"Economics: A Biannual Collection of Recent German Studies"}
MACRO {EconJEconAssoc} {"Economics: The Journal of the Economics Association"}
MACRO {Economics} {"Economics"}
MACRO {EconAppl} {"Economie Appliqu\'ee"}
MACRO {EconPrevision} {"Economie et Pr\'evision"}
% MACRO {EconInt} {"Economie Internationale"}
MACRO {EconRurale} {"Economie Rurale"}
MACRO {EconSoc} {"Economies et Soci\'et\'es"}
MACRO {EconSocTijdschr} {"Economisch en Sociaal Tijdschrift"}
MACRO {EconomyHist} {"Economy and History"}
MACRO {EconomySociety} {"Economy and Society"}
MACRO {EducEcon} {"Education Economics"}
MACRO {EIBPap} {"EIB Papers"}
MACRO {Ekonomia} {"Ekonomia"}
MACRO {EkonMatObzor} {"Ekonomicko-Matematicky Obzor"}
MACRO {EkonomikaRadvanskoyiUkhayeny} {"Ekonomika Radvanskoyi Ukhayeny"}
MACRO {EkonSamfundetsTidskr} {"Ekonomiska Samfundets Tidskrift"}
MACRO {EkonIstrazivanjaEconRes} {"Ekonomska Istrazivanja/Economic Research"}
MACRO {EkonMisaoPraksa} {"Ekonomska Misao i Praksa"}
MACRO {TrimEcon} {"El Trimestre Economico"}
MACRO {Empirica} {"Empirica"}
MACRO {EmpiricalEcon} {"Empirical Economics"}
MACRO {EnergyEcon} {"Energy Economics"}
MACRO {EnergyJ} {"Energy Journal"}
MACRO {EnergyStudRev} {"Energy Studies Review"}
MACRO {EngEcon} {"Engineering Economist"}
MACRO {EnterpriseSociety} {"Enterprise and Society"}
MACRO {EntrepreneurshipRegDevel} {"Entrepreneurship and Regional Development"}
MACRO {EnvironDevelEcon} {"Environment and Development Economics"}
MACRO {EnvironPlanningA} {"Environment and Planning A"}
MACRO {EnvironPlanningCGovtPol} {"Environment and Planning C: Government and Policy"}
MACRO {EnvironResourceEcon} {"Environmental and Resource Economics"}
MACRO {EnvironEconPolStud} {"Environmental Economics and Policy Studies"}
MACRO {EnvironValues} {"Environmental Values"}
MACRO {EstuddeEcon} {"Estudios de Economia"}
MACRO {EstudEcon} {"Estudios Econ\'omicos"}
MACRO {EstudEmpresariales} {"Estudios Empresariales"}
MACRO {EstudInt} {"Estudios Internacionales"}
MACRO {EstudosEcon} {"Estudos Economicos"}
MACRO {EtudConjoncture} {"Etudes et Conjoncture"}
MACRO {EurasianGeogrEcon} {"Eurasian Geography and Economics"}
MACRO {EuroChoices} {"EuroChoices"}
MACRO {EuropEconRev} {"European Economic Review"}
MACRO {EuropFinanceRev} {"European Finance Review"}
MACRO {EuropFinanManage} {"European Financial Management"}
MACRO {EuropJDevelRes} {"European Journal of Development Research"}
MACRO {EuropJFinance} {"European Journal of Finance"}
MACRO {EuropJHousingPol} {"European Journal of Housing Policy"}
MACRO {EuropJIndRelat} {"European Journal of Industrial Relations"}
MACRO {EuropJIntRelat} {"European Journal of International Relations"}
MACRO {EuropJLawEcon} {"European Journal of Law and Economics"}
MACRO {EuropJPE} {"European Journal of Political Economy"}
MACRO {EuropJHistEconThought} {"European Journal of the History of Economic Thought"}
MACRO {EuropRevAgrEcon} {"European Review of Agricultural Economics"}
MACRO {EuropRevEconHist} {"European Review of Economic History"}
MACRO {ExperEcon} {"Experimental Economics"}
MACRO {ExplorationsEconHist} {"Explorations in Economic History"}
MACRO {FamilyEconNutritionRev} {"Family Economics and Nutrition Review"}
MACRO {FamilyEconRev} {"Family Economics Review"}
MACRO {FDICBankingRev} {"FDIC Banking Review"}
MACRO {FRBBusRevPhila} {"Federal Reserve Bank Business Review of Philadelphia"}
MACRO {FRBAtlantaEconRev} {"Federal Reserve Bank of Atlanta Economic Review"}
MACRO {FRBAtlantaRev} {"Federal Reserve Bank of Atlanta Monthly Review"}
MACRO {FRBBostonNewEngEconRev} {"Federal Reserve Bank of Boston New England Economic Review"}
MACRO {FRBChicagoEconPerspect} {"Federal Reserve Bank of Chicago Economic Perspectives"}
MACRO {FRBClevelandEconRev} {"Federal Reserve Bank of Cleveland Economic Review"}
MACRO {FRBDallasEconFinanRev} {"Federal Reserve Bank of Dallas Economic and Financial Review"}
MACRO {FRBDallasEconRev} {"Federal Reserve Bank of Dallas Economic Review"}
MACRO {FRBKansasCityEconRev} {"Federal Reserve Bank of Kansas City Economic Review"}
MACRO {FRBKansasCityRev} {"Federal Reserve Bank of Kansas City Monthly Review"}
MACRO {FRBMinneapolisQuartRev} {"Federal Reserve Bank of Minneapolis Quarterly Review"}
MACRO {FRBNewYorkEconPolRev} {"Federal Reserve Bank of New York Economic Policy Review"}
MACRO {FRBNewYorkRev} {"Federal Reserve Bank of New York Monthly Review"}
MACRO {FRBNewYorkQuartRev} {"Federal Reserve Bank of New York Quarterly Review"}
MACRO {FRBPhiladelphiaBusRev} {"Federal Reserve Bank of Philadelphia Business Review"}
MACRO {FRBRichmondEconQuart} {"Federal Reserve Bank of Richmond Economic Quarterly"}
MACRO {FRBRichmondEconRev} {"Federal Reserve Bank of Richmond Economic Review"}
MACRO {FRBSanFranciscoEconRev} {"Federal Reserve Bank of San Francisco Economic Review"}
MACRO {FRBSanFranciscoRev} {"Federal Reserve Bank of San Francisco Monthly Review"}
MACRO {FRBStLouisRev} {"Federal Reserve Bank of St. Louis Review"}
MACRO {FedResBull} {"Federal Reserve Bulletin"}
MACRO {FeministEcon} {"Feminist Economics"}
MACRO {FinanceUver} {"Finance a \'uver"}
MACRO {FinanceDevel} {"Finance and Development"}
MACRO {FinanceStochastics} {"Finance and Stochastics"}
MACRO {FinanceTradeRev} {"Finance and Trade Review"}
MACRO {FinanceIndia} {"Finance India"}
MACRO {Finance} {"Finance"}
MACRO {FinanEngJapaneseMarkets} {"Financial Engineering and the Japanese Markets"}
MACRO {FinanHistRev} {"Financial History Review"}
MACRO {FinanIndustryStud} {"Financial Industry Studies"}
MACRO {FinanManage} {"Financial Management"}
MACRO {FinanMarketsInstInstruments} {"Financial Markets, Institutions and Instruments"}
MACRO {FinanPracticeEduc} {"Financial Practice and Education"}
MACRO {FinanRev} {"Financial Review"}
MACRO {FinanzArch} {"FinanzArchiv"}
MACRO {FinnishEconPap} {"Finnish Economic Papers"}
MACRO {FiscStud} {"Fiscal Studies"}
MACRO {FoodPol} {"Food Policy"}
MACRO {FoodResInstStud} {"Food Research Institute Studies"}
MACRO {ForeignAff} {"Foreign Affairs"}
MACRO {Foresight} {"Foresight"}
MACRO {ForumDevelStud} {"Forum for Development Studies"}
MACRO {ForumSocEcon} {"Forum for Social Economics"}
MACRO {GamesEconBehav} {"Games and Economic Behavior"}
MACRO {GenevaPapRiskInsTheory} {"Geneva Papers on Risk and Insurance Theory"}
MACRO {GenevaPapRiskInsIssuesPractice} {"Geneva Papers on Risk and Insurance: Issues and Practice"}
MACRO {GerEconRev} {"German Economic Review"}
MACRO {GiornEcon} {"Giornale degli Economisti e Annali di Economia"}
MACRO {GlobalBusEconRev} {"Global Business and Economics Review"}
MACRO {GlobalEconRev} {"Global Economic Review"}
MACRO {GlobalEconomyQuart} {"Global Economy Quarterly"}
MACRO {GlobalEnvironPolitics} {"Global Environmental Politics"}
MACRO {GlobalFinanceJ} {"Global Finance Journal"}
MACRO {GospodarstvoIstre} {"Gospodarstvo Istre"}
MACRO {GreekEconRev} {"Greek Economic Review"}
MACRO {GrowthChange} {"Growth and Change"}
MACRO {HarvardBusRev} {"Harvard Business Review"}
MACRO {HealthCareManageSci} {"Health Care Management Science"}
MACRO {HealthEcon} {"Health Economics"}
MACRO {HealthMarketingQuart} {"Health Marketing Quarterly"}
MACRO {HealthServicesRes} {"Health Services Research"}
MACRO {HistAbstr} {"Historical Abstracts"}
MACRO {HistEconIdeas} {"History of Economic Ideas"}
MACRO {HistEconRev} {"History of Economics Review"}
MACRO {HistPolitEconomy} {"History of Political Economy"}
MACRO {HitotsubashiJComManage} {"Hitotsubashi Journal of Commerce and Management"}
MACRO {HitotsubashiJEcon} {"Hitotsubashi Journal of Economics"}
MACRO {HongKongEconPap} {"Hong Kong Economic Papers"}
MACRO {HousingFinanceRev} {"Housing Finance Review"}
MACRO {HousingPolDebate} {"Housing Policy Debate"}
MACRO {HousingStud} {"Housing Studies"}
MACRO {HumanResourceDevelQuart} {"Human Resource Development Quarterly"}
MACRO {Humanomics} {"Humanomics"}
MACRO {IfoStudien} {"Ifo Studien"}
MACRO {IIUMJEconManage} {"IIUM Journal of Economics and Management"}
MACRO {IlPensieroEconItaliano} {"Il Pensiero Economico Italiano"}
MACRO {IlPolitico} {"Il Politico"}
MACRO {IlRisparmio} {"Il Risparmio"}
MACRO {IllAgrEcon} {"Illinois Agricultural Economics"}
MACRO {IMFStaffPap} {"IMF Staff Papers"}
MACRO {ImpactSciSociety} {"Impact of Science on Society"}
MACRO {IndependentRev} {"Independent Review"}
MACRO {IndianEconSocHistRev} {"Indian Economic and Social History Review"}
MACRO {IndianEconJ} {"Indian Economic Journal"}
MACRO {IndianEconRev} {"Indian Economic Review"}
MACRO {IndianJApplEcon} {"Indian Journal of Applied Economics"}
MACRO {IndianJEcon} {"Indian Journal of Economics"}
MACRO {IndianJGenderStud} {"Indian Journal of Gender Studies"}
MACRO {IndianJLabEcon} {"Indian Journal of Labour Economics"}
MACRO {IndianJQuantEcon} {"Indian Journal of Quantitative Economics"}
MACRO {IndianaBusRev} {"Indiana Business Review"}
MACRO {IndCorpChange} {"Industrial and Corporate Change"}
MACRO {IndLabRelatRev} {"Industrial and Labor Relations Review"}
MACRO {IndOrganRev} {"Industrial Organization Review"}
MACRO {IndRelat} {"Industrial Relations"}
MACRO {IndustryDevel} {"Industry and Development"}
MACRO {IndustryInnovation} {"Industry and Innovation"}
MACRO {Info} {"Info"}
MACRO {InfoComercialEspanolaRevistaEcon} {"Informacion Comercial Espa\~nola Revista de Economia"}
MACRO {InfoEconPol} {"Information Economics and Policy"}
MACRO {Innovations} {"Innovations"}
MACRO {Inquiry} {"Inquiry"}
MACRO {InsMathematicsEcon} {"Insurance: Mathematics and Economics"}
MACRO {IntegrationTrade} {"Integration and Trade"}
MACRO {IntermountainEconRev} {"Intermountain Economic Review"}
MACRO {IntAdvancesEconRes} {"International Advances in Economic Research"}
MACRO {IntContribtoLabStud} {"International Contributions to Labour Studies"}
MACRO {IntEconInsights} {"International Economic Insights"}
MACRO {IntEconJ} {"International Economic Journal"}
MACRO {IER} {"International Economic Review"}
MACRO {IntEconomy} {"International Economy"}
MACRO {IntFinance} {"International Finance"}
MACRO {IntGameTheoryRev} {"International Game Theory Review"}
MACRO {IntJApplEconEconometrics} {"International Journal of Applied Economics and Econometrics"}
MACRO {IntJBus} {"International Journal of Business"}
MACRO {IntJFinanceEcon} {"International Journal of Finance and Economics"}
MACRO {IntJForecasting} {"International Journal of Forecasting"}
MACRO {IntJGameTheory} {"International Journal of Game Theory"}
MACRO {IntJIndOrgan} {"International Journal of Industrial Organization"}
MACRO {IntJManpower} {"International Journal of Manpower"}
MACRO {IntJMaritimeEcon} {"International Journal of Maritime Economics"}
MACRO {IntJProductEcon} {"International Journal of Production Economics"}
MACRO {IntJSocEcon} {"International Journal of Social Economics"}
MACRO {IntJEconBus} {"International Journal of the Economics of Business"}
MACRO {IntJTheoreticalApplFinance} {"International Journal of Theoretical and Applied Finance"}
MACRO {IntJTransportEcon} {"International Journal of Transport Economics"}
MACRO {IntJUrbanRegRes} {"International Journal of Urban and Regional Research"}
MACRO {IntLabRev} {"International Labour Review"}
MACRO {IntMonetFundStaffPap} {"International Monetary Fund Staff Papers"}
MACRO {IntOrgan} {"International Organization"}
MACRO {IntPolRev} {"International Policy Review"}
MACRO {IntRegSciRev} {"International Regional Science Review"}
MACRO {IntRevApplEcon} {"International Review of Applied Economics"}
MACRO {IntRevEconFinance} {"International Review of Economics and Finance"}
MACRO {IntRevFinanAnal} {"International Review of Financial Analysis"}
MACRO {IntRevLawEcon} {"International Review of Law and Economics"}
MACRO {IntSocSciJ} {"International Social Science Journal"}
MACRO {IntTaxPublicFinance} {"International Tax and Public Finance"}
MACRO {IntTradeJ} {"International Trade Journal"}
MACRO {InvestigacionEcon} {"Investigaci\'on Econ\'omica"}
MACRO {InvestigacionesEcon} {"Investigaciones Economicas"}
MACRO {InvestPol} {"Investment Policy"}
MACRO {IrishBankingRev} {"Irish Banking Review"}
MACRO {IrishJAgrEconRuralSociology} {"Irish Journal of Agricultural Economics and Rural Sociology"}
MACRO {ISERev} {"ISE Review"}
MACRO {IslamicEconStud} {"Islamic Economic Studies"}
MACRO {JahrNationalokonStatist} {"Jahrb\^^bucher f\^^bur National\^^bokonomie und Statistik"}
MACRO {JahrWirtschOsteuropas} {"Jahrbuch der Wirtschaft Osteuropas"}
MACRO {JahrRegionalwissenRevRegRes} {"Jahrbuch f\^^bur Regionalwissenschaft/Review of Regional Research"}
MACRO {JapanWorldEconomy} {"Japan and the World Economy"}
MACRO {JapanResQuart} {"Japan Research Quarterly"}
MACRO {JapaneseEconRev} {"Japanese Economic Review"}
MACRO {JapaneseEconStud} {"Japanese Economic Studies"}
MACRO {JapaneseEconomy} {"Japanese Economy"}
MACRO {JStudEconEconometrics} {"Journal for Studies in Economics and Econometrics"}
MACRO {JAccEcon} {"Journal of Accounting and Economics"}
MACRO {JAccRes} {"Journal of Accounting Research"}
MACRO {JAccAudFinance} {"Journal of Accounting, Auditing and Finance"}
MACRO {JAfricanBus} {"Journal of African Business"}
MACRO {JAfricanEconomies} {"Journal of African Economies"}
MACRO {JAfricanFinanceEconDevel} {"Journal of African Finance and Economic Development"}
MACRO {JAgrApplEcon} {"Journal of Agricultural and Applied Economics"}
MACRO {JAgrResourceEcon} {"Journal of Agricultural and Resource Economics"}
MACRO {JAgrEconRes} {"Journal of Agricultural Economics Research"}
MACRO {JAgrEcon} {"Journal of Agricultural Economics"}
MACRO {JApplBusRes} {"Journal of Applied Business Research"}
MACRO {JApplEconometrics} {"Journal of Applied Econometrics"}
MACRO {JApplEcon} {"Journal of Applied Economics"}
MACRO {JApplStatist} {"Journal of Applied Statistics"}
MACRO {JAsiaPacificBus} {"Journal of Asia-Pacific Business"}
MACRO {JAsianEcon} {"Journal of Asian Economics"}
MACRO {JAsianStud} {"Journal of Asian Studies"}
MACRO {JAustralianPolitEconomy} {"Journal of Australian Political Economy"}
MACRO {JBankRes} {"Journal of Bank Research"}
MACRO {JBankingFinance} {"Journal of Banking and Finance"}
MACRO {JBehavioralEcon} {"Journal of Behavioral Economics"}
MACRO {JBioecon} {"Journal of Bioeconomics"}
MACRO {JBES} {"Journal of Business and Economic Statistics"}
MACRO {JBusRes} {"Journal of Business Research"}
MACRO {JBus} {"Journal of Business"}
MACRO {JCommonMarketStud} {"Journal of Common Market Studies"}
MACRO {JComparEcon} {"Journal of Comparative Economics"}
MACRO {JConflictResolution} {"Journal of Conflict Resolution"}
MACRO {JConsAff} {"Journal of Consumer Affairs"}
MACRO {JConsPol} {"Journal of Consumer Policy"}
MACRO {JConsRes} {"Journal of Consumer Research"}
MACRO {JCorpFinanContractingGovernanceOrgan} {"Journal of Corporate Finance: Contracting, Governance and Organization"}
MACRO {JCultEcon} {"Journal of Cultural Economics"}
MACRO {JDerivatives} {"Journal of Derivatives"}
MACRO {JDevelopingAreas} {"Journal of Developing Areas"}
MACRO {JDevelEconPol} {"Journal of Development and Economic Policies"}
MACRO {JDevelEcon} {"Journal of Development Economics"}
MACRO {JDevelStud} {"Journal of Development Studies"}
MACRO {JEastWestStud} {"Journal of East and West Studies"}
MACRO {JEastWestBus} {"Journal of East-West Business"}
MACRO {JEconometrics} {"Journal of Econometrics"}
MACRO {JEconSocMeas} {"Journal of Economic and Social Measurement"}
MACRO {JEconSocPol} {"Journal of Economic and Social Policy"}
MACRO {JEconSocRes} {"Journal of Economic and Social Research"}
MACRO {JEconBehavOrgan} {"Journal of Economic Behavior and Organization"}
MACRO {JEconCooperationIslamicCountries} {"Journal of Economic Cooperation among Islamic Countries"}
MACRO {JEconDevel} {"Journal of Economic Development"}
MACRO {JEDC} {"Journal of Economic Dynamics and Control"}
MACRO {JEconEduc} {"Journal of Economic Education"}
MACRO {JEconGrowth} {"Journal of Economic Growth"}
MACRO {JEH} {"Journal of Economic History"}
MACRO {JEconIntegration} {"Journal of Economic Integration"}
MACRO {JEconIssues} {"Journal of Economic Issues"}
MACRO {JEL} {"Journal of Economic Literature"}
MACRO {JEconMethodology} {"Journal of Economic Methodology"}
MACRO {JEP} {"Journal of Economic Perspectives"}
MACRO {JEconPsych} {"Journal of Economic Psychology"}
MACRO {JEconRes} {"Journal of Economic Research"}
MACRO {JEconStud} {"Journal of Economic Studies"}
MACRO {JEconSurveys} {"Journal of Economic Surveys"}
MACRO {JEconTheoryEconometrics} {"Journal of Economic Theory and Econometrics"}
MACRO {JET} {"Journal of Economic Theory"}
MACRO {JEconMVEA} {"Journal of Economics (MVEA)"}
MACRO {JEconZNationalokon} {"Journal of Economics (Zeitschrift f\^^bur National\^^bokonomie)"}
MACRO {JEconBus} {"Journal of Economics and Business"}
MACRO {JEconFinance} {"Journal of Economics and Finance"}
MACRO {JEconManageStrategy} {"Journal of Economics and Management Strategy"}
MACRO {JEducFinance} {"Journal of Education Finance"}
MACRO {JEmergingMarkets} {"Journal of Emerging Markets"}
MACRO {JEmpiricalFinance} {"Journal of Empirical Finance"}
MACRO {JEnergyDevel} {"Journal of Energy and Development"}
MACRO {JEnergyFinanceDevel} {"Journal of Energy Finance and Development"}
MACRO {JEnergyLit} {"Journal of Energy Literature"}
MACRO {JEnvironDevel} {"Journal of Environment and Development"}
MACRO {JEnvironEconManage} {"Journal of Environmental Economics and Management"}
MACRO {JEnvironPlanningManage} {"Journal of Environmental Planning and Management"}
MACRO {JEuropEconHist} {"Journal of European Economic History"}
MACRO {JEvolutionaryEcon} {"Journal of Evolutionary Economics"}
MACRO {JFamilyEconIssues} {"Journal of Family and Economic Issues"}
MACRO {JFarEasternBus} {"Journal of Far Eastern Business"}
MACRO {JFarmEcon} {"Journal of Farm Economics"}
MACRO {JFinance} {"Journal of Finance"}
MACRO {JFinanQuantAnal} {"Journal of Financial and Quantitative Analysis"}
MACRO {JFinanEcon} {"Journal of Financial Economics"}
MACRO {JFinanIntermediation} {"Journal of Financial Intermediation"}
MACRO {JFinanManageAnal} {"Journal of Financial Management and Analysis"}
MACRO {JFinanRes} {"Journal of Financial Research"}
MACRO {JFinanServicesRes} {"Journal of Financial Services Research"}
MACRO {JForensicEcon} {"Journal of Forensic Economics"}
MACRO {JFuturesMarkets} {"Journal of Futures Markets"}
MACRO {JGeographSystems} {"Journal of Geographical Systems"}
MACRO {JHealthEcon} {"Journal of Health Economics"}
MACRO {JHealthPoliticsPolLaw} {"Journal of Health Politics, Policy and Law"}
MACRO {JHigherEducPolManage} {"Journal of Higher Education Policy and Management"}
MACRO {JHousingEcon} {"Journal of Housing Economics"}
MACRO {JHousingRes} {"Journal of Housing Research"}
MACRO {JHumanRes} {"Journal of Human Resources"}
MACRO {JIncomeDistribution} {"Journal of Income Distribution"}
MACRO {JIndEcon} {"Journal of Industrial Economics"}
MACRO {JIndustryStud} {"Journal of Industry Studies"}
MACRO {JInstTheoreticalEcon} {"Journal of Institutional and Theoretical Economics"}
MACRO {JInteramerStudWorldAff} {"Journal of Interamerican Studies and World Affairs"}
MACRO {JInterdisciplinaryEcon} {"Journal of Interdisciplinary Economics"}
MACRO {JIntComparEcon} {"Journal of International and Comparative Economics"}
MACRO {JIntBusStud} {"Journal of International Business Studies"}
MACRO {JIntDevel} {"Journal of International Development"}
MACRO {JIntEconLaw} {"Journal of International Economic Law"}
MACRO {JIntEcon} {"Journal of International Economics"}
MACRO {JIntFinanMarketsInstMoney} {"Journal of International Financial Markets, Institutions and Money"}
MACRO {JIntMoneyFinance} {"Journal of International Money and Finance"}
MACRO {JIntTradeEconDevel} {"Journal of International Trade and Economic Development"}
MACRO {JKingAbdulazizUnivIslamicEcon} {"Journal of King Abdulaziz University: Islamic Economics"}
MACRO {JLabEcon} {"Journal of Labor Economics"}
MACRO {JLabRes} {"Journal of Labor Research"}
MACRO {JLawEcon} {"Journal of Law and Economics"}
MACRO {JLawEconOrgan} {"Journal of Law, Economics, and Organization"}
MACRO {JLegalEcon} {"Journal of Legal Economics"}
MACRO {JLegalStud} {"Journal of Legal Studies"}
MACRO {JMacroecon} {"Journal of Macroeconomics"}
MACRO {JManageAccRes} {"Journal of Management Accounting Research"}
MACRO {JManageGovernance} {"Journal of Management and Governance"}
MACRO {JMarketingRes} {"Journal of Marketing Research"}
MACRO {JMarketing} {"Journal of Marketing"}
MACRO {JMarketsMorality} {"Journal of Markets and Morality"}
MACRO {JMathEcon} {"Journal of Mathematical Economics"}
MACRO {JME} {"Journal of Monetary Economics"}
MACRO {JMCB} {"Journal of Money, Credit, and Banking"}
MACRO {JMultinatFinanManage} {"Journal of Multinational Financial Management"}
MACRO {JPeaceRes} {"Journal of Peace Research"}
MACRO {JPeaceSci} {"Journal of Peace Science"}
MACRO {JPolAnalManage} {"Journal of Policy Analysis and Management"}
MACRO {JPolModeling} {"Journal of Policy Modeling"}
MACRO {JPolReform} {"Journal of Policy Reform"}
MACRO {JPE} {"Journal of Political Economy"}
MACRO {JPopulationEcon} {"Journal of Population Economics"}
MACRO {JPortfolManage} {"Journal of Portfolio Management"}
MACRO {JPostKeynesianEcon} {"Journal of Post Keynesian Economics"}
MACRO {JPrivateEnterprise} {"Journal of Private Enterprise"}
MACRO {JProductivAnal} {"Journal of Productivity Analysis"}
MACRO {JPublicIntAff} {"Journal of Public and International Affairs"}
MACRO {JPublicEconTheory} {"Journal of Public Economic Theory"}
MACRO {JPublicEcon} {"Journal of Public Economics"}
MACRO {JPublicFinancePublicChoiceEconScelteP} {"Journal of Public Finance and Public Choice/Economia Delle Scelte Pubbliche"}
MACRO {JQuantEcon} {"Journal of Quantitative Economics"}
MACRO {JRealEstateFinanceEcon} {"Journal of Real Estate Finance and Economics"}
MACRO {JRealEstateLit} {"Journal of Real Estate Literature"}
MACRO {JRealEstatePortfolManage} {"Journal of Real Estate Portfolio Management"}
MACRO {JRealEstatePracticeEduc} {"Journal of Real Estate Practice and Education"}
MACRO {JRealEstateRes} {"Journal of Real Estate Research"}
MACRO {JRegAnalPol} {"Journal of Regional Analysis and Policy"}
MACRO {JRegSci} {"Journal of Regional Science"}
MACRO {JRegulEcon} {"Journal of Regulatory Economics"}
MACRO {JResIslamicEcon} {"Journal of Research in Islamic Economics"}
MACRO {JResPharmaceutEcon} {"Journal of Research in Pharmaceutical Economics"}
MACRO {JRiskIns} {"Journal of Risk and Insurance"}
MACRO {JRiskUncertainty} {"Journal of Risk and Uncertainty"}
MACRO {JSocEconDevel} {"Journal of Social and Economic Development"}
MACRO {JSocioEcon} {"Journal of Socio-Economics"}
MACRO {JSportsEcon} {"Journal of Sports Economics"}
MACRO {JTaxation} {"Journal of Taxation"}
MACRO {JTechTransfer} {"Journal of Technology Transfer"}
MACRO {JAmerRealEstateUrbanEconAssoc} {"Journal of the American Real Estate and Urban Economics Association"}
MACRO {JASA} {"Journal of the American Statistical Association"}
MACRO {JAsiaPacificEconomy} {"Journal of the Asia Pacific Economy"}
MACRO {JEconofBus} {"Journal of the Economics of Business"}
MACRO {JHistEconThought} {"Journal of the History of Economic Thought"}
MACRO {JJapaneseIntEconomies} {"Journal of the Japanese and International Economies"}
MACRO {JRoyStatisticalSociety} {"Journal of the Royal Statistical Society, Series A"}
MACRO {JSocSci} {"Journal of the Social Sciences"}
MACRO {JStatisticalSocInquirySocietyIreland} {"Journal of the Statistical and Social Inquiry Society of Ireland"}
MACRO {JTransnatManageDevel} {"Journal of Transnational Management Development"}
MACRO {JTranspEconPol} {"Journal of Transport Economics and Policy"}
MACRO {JUrbanEcon} {"Journal of Urban Economics"}
MACRO {JWorldBus} {"Journal of World Business"}
MACRO {JWorldTradeLaw} {"Journal of World Trade Law"}
MACRO {JWorldTrade} {"Journal of World Trade"}
MACRO {KansantAikak} {"Kansantaloudellinen Aikakauskirja"}
MACRO {KeioEconStud} {"Keio Economic Studies"}
MACRO {KobeEconBusRev} {"Kobe Economic and Business Review"}
MACRO {KobeUnivEconRev} {"Kobe University Economic Review"}
MACRO {KokuminKeizaiZasshi} {"Kokumin-Keizai Zasshi"}
MACRO {Konjunkturpolitik} {"Konjunkturpolitik"}
MACRO {KreditKapital} {"Kredit und Kapital"}
MACRO {Kyklos} {"Kyklos"}
MACRO {KyotoUnivEconRev} {"Kyoto University Economic Review"}
MACRO {LActualEconRevAnalEcon} {"L'Actualit\'e Economique/Revue D'Analyse Economique"}
MACRO {LActualEcon} {"L'Actualit\'e Economique"}
MACRO {LImpresa} {"L'Impresa"}
MACRO {LIndustriaNS} {"L'Industria, Nuova Serie"}
MACRO {LIndustria} {"L'Industria"}
MACRO {LabHist} {"Labor History"}
MACRO {LabEcon} {"Labour Economics"}
MACRO {Labour} {"Labour"}
MACRO {LahoreJEcon} {"Lahore Journal of Economics"}
MACRO {LandEcon} {"Land Economics"}
MACRO {LatinAmerPoliticsSociety} {"Latin American Politics and Society"}
MACRO {LawContempProb} {"Law and Contemporary Problems"}
MACRO {LecturasEcon} {"Lecturas de Economia"}
MACRO {LibyanEconBusRev} {"Libyan Economic and Business Review"}
MACRO {LiiketaloudellinenAikak} {"Liiketaloudellinen Aikakauskirja"}
MACRO {LloydsBankRev} {"Lloyds Bank Review"}
MACRO {LocalEconomy} {"Local Economy"}
MACRO {LogistTranspRev} {"Logistics and Transportation Review"}
MACRO {MD} {"Macroeconomic Dynamics"}
MACRO {MalayanEconRev} {"Malayan Economic Review"}
MACRO {ManageAcc} {"Management Accounting"}
MACRO {ManageTrainRev} {"Management Training Review"}
MACRO {Management} {"Management"}
MACRO {ManagerialDecEcon} {"Managerial and Decision Economics"}
MACRO {ManchesterSchEconSocStud} {"Manchester School of Economic and Social Studies"}
MACRO {ManchesterSch} {"Manchester School"}
MACRO {Margin} {"Margin"}
MACRO {MarineResourceEcon} {"Marine Resource Economics"}
MACRO {MaritimePolManage} {"Maritime Policy and Management"}
MACRO {MarketingSci} {"Marketing Science"}
MACRO {MarquetteBusRev} {"Marquette Business Review"}
MACRO {Matekon} {"Matekon"}
MACRO {MathFinance} {"Mathematical Finance"}
MACRO {MathMethodsOperationsRes} {"Mathematical Methods of Operations Research"}
MACRO {MathSocSci} {"Mathematical Social Sciences"}
MACRO {Methodus} {"Methodus"}
MACRO {Metrika} {"Metrika"}
MACRO {Metroecon} {"Metroeconomica"}
MACRO {MichAcademician} {"Michigan Academician"}
MACRO {MichLawRev} {"Michigan Law Review"}
MACRO {MiddleEastBusEconRev} {"Middle East Business and Economic Review"}
MACRO {MiddleEastEconPap} {"Middle East Economic Papers"}
MACRO {MiddleEastJ} {"Middle East Journal"}
MACRO {METUStudDevel} {"Middle East Technical University Studies in Development"}
MACRO {MissValJBusEcon} {"Mississippi Valley Journal of Business and Economics"}
MACRO {MitteilungenArbeitsmarktBerufsforschung} {"Mitteilungen aus der Arbeitsmarkt- und Berufsforschung"}
MACRO {MOCTMOSTEconPolTransitionalEconomies} {"MOCT-MOST: Economic Policy in Transitional Economies"}
MACRO {ModAsianStud} {"Modern Asian Studies"}
MACRO {MomentoEcon} {"Momento Econ\'omico"}
MACRO {MondoAperto} {"Mondo Aperto"}
MACRO {MonedaCredito} {"Moneda y Cr\'edito"}
MACRO {MonetaCredito} {"Moneta e Credito"}
MACRO {MonetEconStud} {"Monetary and Economic Studies"}
MACRO {MonLabRev} {"Monthly Labor Review"}
MACRO {MultinatFinanceJ} {"Multinational Finance Journal"}
MACRO {NatInstEconRev} {"National Institute Economic Review"}
MACRO {NatTaxJ} {"National Tax Journal"}
MACRO {NatWestminsterBankQuartRev} {"National Westminster Bank Quarterly Review"}
MACRO {NationalkonTidsskr} {"National konomisk Tidsskrift"}
MACRO {NaturalResModeling} {"Natural Resource Modeling"}
MACRO {NaturalResJ} {"Natural Resources Journal"}
MACRO {NebrJEconBus} {"Nebraska Journal of Economics and Business"}
MACRO {Netnomics} {"Netnomics"}
MACRO {NewEconomy} {"New Economy"}
MACRO {NewEnglandEconRev} {"New England Economic Review"}
MACRO {NewMexicoBus} {"New Mexico Business"}
MACRO {NewPolitEconomy} {"New Political Economy"}
MACRO {NewTrendsCzechEcon} {"New Trends Czech. Econ."}
MACRO {NewZealandEconPap} {"New Zealand Economic Papers"}
MACRO {NewZealandGeographer} {"New Zealand Geographer"}
MACRO {NigerianJEconSocStud} {"Nigerian Journal of Economic and Social Studies"}
MACRO {NomuraResInstQuartEconRev} {"Nomura Research Institute Quarterly Economic Review"}
MACRO {NonlinearDynamPsychandLifeSci} {"Nonlinear Dynamics, Psychology, and Life Sciences"}
MACRO {NonprofitManageLeadership} {"Nonprofit Management and Leadership"}
MACRO {NordicJPolitEconomy} {"Nordic Journal of Political Economy"}
MACRO {NAmerActuarialJ} {"North American Actuarial Journal"}
MACRO {NAmerJEconFinance} {"North American Journal of Economics and Finance"}
MACRO {OECDEconStud} {"OECD Economic Studies"}
MACRO {OhioStateUBullBusRes} {"Ohio State University Bulletin of Business Research"}
MACRO {OkonPolit} {"Okonomi og Politik"}
MACRO {Okonomi} {"Okonomi"}
MACRO {OPECRev} {"OPEC Review"}
MACRO {OpenEconomiesRev} {"Open Economies Review"}
MACRO {ORSpectrum} {"OR Spectrum"}
MACRO {ORSpektrum} {"OR Spektrum"}
MACRO {OrbisEcon} {"Orbis Economicus"}
MACRO {OregonBusRev} {"Oregon Business Review"}
MACRO {OrganEnviron} {"Organization and Environment"}
MACRO {OsakaEconPap} {"Osaka Economic Papers"}
MACRO {OxfordBullEconStatist} {"Oxford Bulletin of Economics and Statistics"}
MACRO {OxfordDevelStud} {"Oxford Development Studies"}
MACRO {OxfordEconPap} {"Oxford Economic Papers"}
MACRO {OxfordRevEconPol} {"Oxford Review of Economic Policy"}
MACRO {PacificAsianJEnergy} {"Pacific and Asian Journal of Energy"}
MACRO {PacificEconBull} {"Pacific Economic Bulletin"}
MACRO {PacificEconRev} {"Pacific Economic Review"}
MACRO {PacificBasinFinanceJ} {"Pacific-Basin Finance Journal"}
MACRO {PakistanDevelRev} {"Pakistan Development Review"}
MACRO {PakistanEconSocRev} {"Pakistan Economic and Social Review"}
MACRO {PakistanJApplEcon} {"Pakistan Journal of Applied Economics"}
MACRO {PapEconEspanola} {"Papeles de Econom\'\ia Espa\~nola"}
MACRO {PapRegSci} {"Papers in Regional Science"}
MACRO {PapNonMarketDecMaking} {"Papers on Non-Market Decision Making"}
MACRO {PeaceResSocietyIntPap} {"Peace Research Society International Papers"}
MACRO {PeaceSciSociety} {"Peace Science Society (International) Papers"}
MACRO {PensaIberamerRevistaEconPolitica} {"Pensamiento Iberoamericano: Revista de Econom\'\ia Pol\'\itica"}
MACRO {PerspectLabIncome} {"Perspectives on Labour and Income"}
MACRO {PesquisaPlanejamentoEcon} {"Pesquisa e Planejamento Econ\^omico"}
MACRO {PharmacoEcon} {"PharmacoEconomics"}
MACRO {PhilippineEconJ} {"Philippine Economic Journal"}
MACRO {PhilippineJBusEcon} {"Philippine Journal of Business and Economics"}
MACRO {PhilippineJBusFinance} {"Philippine Journal of Business and Finance"}
MACRO {PhilippineRevEconBus} {"Philippine Review of Economics and Business"}
MACRO {PhilippineRevEcon} {"Philippine Review of Economics"}
MACRO {PhysicaA} {"Physica A: Statistical Mechanics and its Applications"}
MACRO {PolAnal} {"Policy Analysis"}
MACRO {PolRev} {"Policy Review"}
MACRO {PolSci} {"Policy Sciences"}
MACRO {PolStud} {"Policy Studies"}
MACRO {Policy} {"Policy"}
MACRO {PoliticaEcon} {"Politica Economica"}
MACRO {PolitSciQuart} {"Political Science Quarterly"}
MACRO {PolitickEkon} {"Politick Ekonomie"}
MACRO {PopulationDevelRev} {"Population and Development Review"}
MACRO {PopulationBull} {"Population Bulletin"}
MACRO {PopulationResPolRev} {"Population Research and Policy Review"}
MACRO {PopulationRev} {"Population Review"}
MACRO {PopulationStud} {"Population Studies"}
MACRO {Population} {"Population"}
MACRO {PostCommunistEconomies} {"Post-Communist Economies"}
MACRO {PostSovietAff} {"Post-Soviet Affairs"}
MACRO {PostSovietGeogrEcon} {"Post-Soviet Geography and Economics"}
MACRO {PragueEconPap} {"Prague Economic Papers"}
MACRO {ProbDesarrollo} {"Problemas del Desarrollo"}
MACRO {ProbEconTransition} {"Problems of Economic Transition"}
MACRO {ProbEcon} {"Problems of Economics"}
MACRO {PublicAdminRev} {"Public Administration Review"}
MACRO {PublicBudgFinance} {"Public Budgeting and Finance"}
MACRO {PublicChoice} {"Public Choice"}
MACRO {PublicFinanceQuart} {"Public Finance Quarterly"}
MACRO {PublicFinanceRev} {"Public Finance Review"}
MACRO {PublicFinance} {"Public Finance"}
MACRO {PublicManagementSources} {"Public Management Sources"}
MACRO {PublicPol} {"Public Policy"}
MACRO {PunjabUnivEcon} {"Punjab University Economist"}
MACRO {QuaderniEconFinanza} {"Quaderni di Economia e Finanza"}
MACRO {QuaderniStoricidelleMarche} {"Quaderni Storici delle Marche"}
MACRO {QuantFinance} {"Quantitative Finance"}
MACRO {QuartJAustrianEcon} {"Quarterly Journal of Austrian Economics"}
MACRO {QuartJBusEcon} {"Quarterly Journal of Business and Economics"}
MACRO {QJE} {"Quarterly Journal of Economics"}
MACRO {QuartJIndianEconStudSocSci} {"Quarterly Journal of Indian Economic Studies in Social Science"}
MACRO {QuartRevAgrEcon} {"Quarterly Review of Agricultural Economics"}
MACRO {QuartRevEconBus} {"Quarterly Review of Economics and Business"}
MACRO {QuartRevEconFinance} {"Quarterly Review of Economics and Finance"}
MACRO {QuartRevRuralEconomy} {"Quarterly Review of the Rural Economy"}
MACRO {RegionDevel} {"R\'egion et D\'eveloppement"}
MACRO {RAND} {"RAND Journal of Economics"}
MACRO {RassegnaEcon} {"Rassegna Economica"}
MACRO {RealEstateEcon} {"Real Estate Economics"}
MACRO {RechEconLouvainLouvainEconRev} {"Recherches Economiques de Louvain/Louvain Economic Review"}
MACRO {RechEconLouvain} {"Recherches Economiques de Louvain"}
MACRO {RegSciUrbanEcon} {"Regional Science and Urban Economics"}
MACRO {RegSciPersp} {"Regional Science Perspectives"}
MACRO {RegStud} {"Regional Studies"}
MACRO {Regulation} {"Regulation"}
MACRO {ResEcon} {"Research in Economics"}
MACRO {ResLawEcon} {"Research in Law and Economics"}
MACRO {ResPol} {"Research Policy"}
MACRO {ResourceEnergyEcon} {"Resource and Energy Economics"}
MACRO {ResourcesEnergy} {"Resources and Energy"}
MACRO {ResourcesPol} {"Resources Policy"}
MACRO {RevAccStud} {"Review of Accounting Studies"}
MACRO {RevAfricanPolitEconomy} {"Review of African Political Economy"}
MACRO {RevAgrEcon} {"Review of Agricultural Economics"}
MACRO {RevAustrianEcon} {"Review of Austrian Economics"}
MACRO {RevBlackPolitEconomy} {"Review of Black Political Economy"}
MACRO {RevBusEconRes} {"Review of Business and Economic Research"}
MACRO {RevDerivativesRes} {"Review of Derivatives Research"}
MACRO {RevDevelEcon} {"Review of Development Economics"}
MACRO {RevEconCondItaly} {"Review of Economic Conditions in Italy"}
MACRO {RevEconDesign} {"Review of Economic Design"}
MACRO {RED} {"Review of Economic Dynamics"}
MACRO {REStud} {"Review of Economic Studies"}
MACRO {REStat} {"Review of Economics and Statistics"}
MACRO {RFE} {"Review of Financial Economics"}
MACRO {RFStud} {"Review of Financial Studies"}
MACRO {RevIncomeWealth} {"Review of Income and Wealth"}
MACRO {RevIndOrgan} {"Review of Industrial Organization"}
MACRO {RevIntEcon} {"Review of International Economics"}
MACRO {RevIntPolitEconomy} {"Review of International Political Economy"}
MACRO {RevIntStud} {"Review of International Studies"}
MACRO {RevMarketingAgrEcon} {"Review of Marketing and Agricultural Economics"}
MACRO {RevPolitEconomy} {"Review of Political Economy"}
MACRO {RevPublicDataUse} {"Review of Public Data Use"}
MACRO {RevQuantFinanceAcc} {"Review of Quantitative Finance and Accounting"}
MACRO {RevRadicalPolitEcon} {"Review of Radical Political Economics"}
MACRO {RevRegStud} {"Review of Regional Studies"}
MACRO {RevSocEconomy} {"Review of Social Economy"}
MACRO {RevUrbanRegDevelStud} {"Review of Urban and Regional Development Studies"}
MACRO {RevistaBrasileiraEcon} {"Revista Brasileira de Economia"}
MACRO {RevistaAdminEmpresas} {"Revista de Administra\c cao d Empr\^esas"}
MACRO {RevistaAnalEcon} {"Revista de Analisis Economico"}
MACRO {RevistaEconInst} {"Revista de Econom\'\ia Institucional"}
MACRO {RevistaEconyEstadist} {"Revista de Econom\'\ia y Estad\'\istica, N.S."}
MACRO {RevistaEcon} {"Revista de Econom\'\ia"}
MACRO {RevistaEconometria} {"Revista de Econometria"}
MACRO {RevistaEconAplicada} {"Revista de Economia Aplicada"}
MACRO {RevistaEconPoliticaBrazilianJPolitEconomy} {"Revista de Economia Pol\'\itica/Brazilian Journal of Political Economy"}
MACRO {RevistaEconPolitica} {"Revista de Economia Pol\'\itica"}
MACRO {RevistaEstadist} {"Revista de Estad\'\istica"}
MACRO {RevistaEstudPolit} {"Revista de Estudios Pol\'\iticos"}
MACRO {RevistaHistEcon} {"Revista de Historia Econ\'omica"}
MACRO {RevistaHistInd} {"Revista de Historia Industrial"}
MACRO {RevistaFacCiencEconAdminMontevideo} {"Revista de la Facultad de Ciencas Econ\'omicas y de Administraci\'on de Montevideo"}
MACRO {RevistaEspanolaEcon} {"Revista Espa\~nola de Econom\'\ia"}
MACRO {RevueCanSciAdminCanJAdminSci} {"Revue Canadienne des Sciences de l'Administration/Canadian Journal of Administrative Sciences"}
MACRO {RevueEconDevel} {"Revue d'Economie du D\'eveloppement"}
MACRO {RevueEconFinanc} {"Revue d'Economie Financi\`ere"}
MACRO {RevueEconInd} {"Revue d'Economie Industrielle"}
MACRO {RevueEconPolitique} {"Revue d'Economie Politique"}
MACRO {RevueEconRegUrbaine} {"Revue d'Economie Regionale et Urbaine"}
MACRO {RevueEtudComparEstOuest} {"Revue d'Etudes Comparatives Est-Ouest"}
MACRO {RevueLOFCE} {"Revue de L'OFCE"}
MACRO {RevueSciFinanc} {"Revue de Science Financi\`ere"}
MACRO {RevueEconMadagascar} {"Revue Economique de Madagascar"}
MACRO {RevueEcon} {"Revue Economique"}
MACRO {RevueFinanceControleStrategie} {"Revue Finance Contr\^ole Strat\'egie"}
MACRO {RevueFrancaiseEcon} {"Revue Fran\c caise d'Economie"}
MACRO {RevueFrancaiseGestion} {"Revue Fran\c caise de Gestion"}
MACRO {RevueRoumaineSciSocSerieSciEcon} {"Revue Roumaine des Sciences Sociales; S\'erie Sciences Economiques"}
MACRO {RevueTiersMonde} {"Revue Tiers Monde"}
MACRO {RicercheEcon} {"Ricerche Economiche"}
MACRO {RISECIntRevEconBus} {"RISEC: International Review of Economics and Business"}
MACRO {RiskDecPol} {"Risk Decision and Policy"}
MACRO {RivistaPoliticaEcon} {"Rivista di Politica Economica"}
MACRO {RivistaStatistAppl} {"Rivista di Statistica Applicata"}
MACRO {RivistaStoriaEconNS} {"Rivista di Storia Economica, N.S."}
MACRO {RivistaIntSciEconCom} {"Rivista Internazionale di Scienze Economiche e Commerciali"}
MACRO {RivistaIntSciSoc} {"Rivista Internazionale di Scienze Sociali"}
MACRO {RivistaItalianaEcon} {"Rivista Italiana degli Economisti"}
MACRO {RussianEEuropFinanceTrade} {"Russian and East European Finance and Trade"}
MACRO {RussianEconTrends} {"Russian Economic Trends"}
MACRO {ScandEconHistRev} {"Scandinavian Economic History Review"}
MACRO {ScandJEcon} {"Scandinavian Journal of Economics"}
MACRO {SchmollersJahr} {"Schmollers Jahrbuch f\^^bur Wirtschafts- und Sozialwissenschaften"}
MACRO {SJZWSJApplSocSciStud} {"Schmollers Jahrbuch: Zeitschrift f\^^bur Wirtschafts- und Sozialwissenschaften/Journal of Applied Social Science Studies"}
MACRO {SchweizNationalbankQuart} {"Schweizerische Nationalbank Quartalsheft"}
MACRO {SZVStatistSwissJEconStatist} {"Schweizerische Zeitschrift f\^^bur Volkswirtschaft und Statistik/Swiss Journal of Economics and Statistics"}
MACRO {SZVStatist} {"Schweizerische Zeitschrift f\^^bur Volkswirtschaft und Statistik"}
MACRO {SciSociety} {"Science and Society"}
MACRO {ScotJPolitEconomy} {"Scottish Journal of Political Economy"}
MACRO {SeoulJEcon} {"Seoul Journal of Economics"}
MACRO {SingaporeEconRev} {"Singapore Economic Review"}
MACRO {SkandBankenQuartRev} {"Skandinaviska Banken Quarterly Review"}
MACRO {SloanManageRev} {"Sloan Management Review"}
MACRO {SmallBusEcon} {"Small Business Economics"}
MACRO {SocEconStud} {"Social and Economic Studies"}
MACRO {SocChoiceWelfare} {"Social Choice and Welfare"}
MACRO {SocRes} {"Social Research"}
MACRO {SocSciJapanJ} {"Social Science Japan Journal"}
MACRO {SocSciQuart} {"Social Science Quarterly"}
MACRO {SocSecBull} {"Social Security Bulletin"}
MACRO {SouthAfricanJEconManageSciNS} {"South African Journal of Economic and Management Sciences, N.S."}
MACRO {SouthAfricanJEcon} {"South African Journal of Economics"}
MACRO {SouthernEconJ} {"Southern Economic Journal"}
MACRO {SouthernJAgrEcon} {"Southern Journal of Agricultural Economics"}
MACRO {SouthernQuart} {"Southern Quarterly"}
MACRO {SovietEEuropForeignTrade} {"Soviet and Eastern European Foreign Trade"}
MACRO {SovietEconomy} {"Soviet Economy"}
MACRO {SovietStud} {"Soviet Studies"}
MACRO {SpanishEconRev} {"Spanish Economic Review"}
MACRO {Spoudai} {"Spoudai"}
MACRO {Statistica} {"Statistica"}
MACRO {StatisticalJ} {"Statistical Journal"}
MACRO {StatisticalPap} {"Statistical Papers"}
MACRO {StatsokonTidsskr} {"Statsokonomisk Tidsskrift"}
MACRO {StructChangeEconDynam} {"Structural Change and Economic Dynamics"}
MACRO {StudEcon} {"Studi Economici"}
MACRO {StudEconFinance} {"Studies in Economics and Finance"}
MACRO {StudFamilyPlanning} {"Studies in Family Planning"}
MACRO {SNDE} {"Studies in Nonlinear Dynamics and Econometrics"}
MACRO {StudCercetEcon} {"Studii si Cercetari Economice"}
MACRO {SATEBSAJEconManageSciNS} {"Suid-Afrikaanse Tydskrif vir Ekonomiese en Bestuurswetenskappe/South African Journal of Economic and Management Sciences, N.S."}
MACRO {SATEBSAJEconManageSci} {"Suid-Afrikaanse Tydskrif vir Ekonomiese en Bestuurswetenskappe/South African Journal of Economic and Management Sciences"}
MACRO {SupremeCourtEconRev} {"Supreme Court Economic Review"}
MACRO {SurvCurrBus} {"Survey of Current Business"}
MACRO {SwedishEconPolRev} {"Swedish Economic Policy Review"}
MACRO {SwedishJEcon} {"Swedish Journal of Economics"}
MACRO {SwissPolitSciRev} {"Swiss Political Science Review"}
MACRO {TahqEq} {"Tahqiqat-e eqtesadi (Quarterly Journal of Economic Research)"}
MACRO {TaiwanEconRev} {"Taiwan Economic Review"}
MACRO {TeachingBusEcon} {"Teaching Business and Economics"}
MACRO {TechAnalStrategicManage} {"Technology Analysis and Strategic Management"}
MACRO {TechCult} {"Technology and Culture"}
MACRO {TelecommunicationsPol} {"Telecommunications Policy"}
MACRO {TheoryDec} {"Theory and Decision"}
MACRO {TijdschrEconManage} {"Tijdschrift voor Economie en Management"}
MACRO {TijdschrEcon} {"Tijdschrift voor Economie"}
MACRO {TourismHospitalityManage} {"Tourism and Hospitality Management"}
MACRO {TourismEcon} {"Tourism Economics"}
MACRO {Transaction} {"Trans-action"}
MACRO {TransnatCorporations} {"Transnational Corporations"}
MACRO {TranspJ} {"Transportation Journal"}
MACRO {TranspResPartAPolPractice} {"Transportation Research: Part A: Policy and Practice"}
MACRO {TranspResPartBMethodological} {"Transportation Research: Part B: Methodological"}
MACRO {TranspResPartDTransportEnviron} {"Transportation Research: Part D: Transport and Environment"}
MACRO {TranspResPartELogistTranspRev} {"Transportation Research: Part E: Logistics and Transporation Review"}
MACRO {Transportation} {"Transportation"}
MACRO {TravailEmploi} {"Travail et Emploi"}
MACRO {UkrainianEconRev} {"Ukrainian Economic Review"}
MACRO {UNCTADRev} {"UNCTAD Review"}
MACRO {UNMonChron} {"United Nations Monthly Chronicle"}
MACRO {UnivMissouriBusGovtRev} {"University of Missouri Business and Government Review"}
MACRO {UnivWashBusRev} {"University of Washington Business Review"}
MACRO {UrbanStud} {"Urban Studies"}
MACRO {VentureCapital} {"Venture Capital"}
MACRO {VierteljahrsWirtschaftsforschung} {"Vierteljahrsheftze zur Wirtschaftsforschung"}
MACRO {WaterResourcesRes} {"Water Resources Research"}
MACRO {WeltwirtschArchRevWorldEcon} {"Weltwirtschaftliches Archiv/Review of World Economics"}
MACRO {WeltwirtschArch} {"Weltwirtschaftliches Archiv"}
MACRO {WesternEconJ} {"Western Economic Journal"}
MACRO {WesternJAgrEcon} {"Western Journal of Agricultural Economics"}
MACRO {WirtschRecht} {"Wirtschaft und Recht"}
MACRO {WirtschaftspolitischeBltter} {"Wirtschaftspolitische Bltter"}
MACRO {WissZMartinLutherUnivHalleWittenberg} {"Wissenshaftliche Zeitschrift der Martin Luther Universitat Halle-Wittenberg"}
MACRO {WorkingUSA} {"WorkingUSA"}
MACRO {WorldBankEconRev} {"World Bank Economic Review"}
MACRO {WorldBankResObserver} {"World Bank Research Observer"}
MACRO {WorldDevel} {"World Development"}
MACRO {WorldEconomy} {"World Economy"}
MACRO {YaleEconEssays} {"Yale Economic Essays"}
MACRO {YaleJRegul} {"Yale Journal on Regulation"}
MACRO {YaleLawJ} {"Yale Law Journal"}
MACRO {YapiKrediEconRev} {"Yapi Kredi Economic Review"}
MACRO {YorkshireBullEconSocRes} {"Yorkshire Bull. Econ. Soc. Res."}
MACRO {ZagrebIntRevEconBus} {"Zagreb International Review of Economics and Business"}
MACRO {ZbornikRadovaEkonFakulRijeka} {"Zbornik Radova Ekonomskog Fakulteta Rijeka"}
MACRO {ZREFURPRSEJEconBus} {"Zbornik Radova Ekonomskog Fakulteta U. Rijeci/Proceedings of Rijeka School of Economics: Journal of Economics and Business"}
MACRO {ZAllgTextileMarktwirtsch} {"Zeitschrift f\^^bur Allgemaine und Textile Marktwirts schaft"}
MACRO {ZBetriebswirtschaft} {"Zeitschrift f\^^bur Betriebswirtschaft"}
MACRO {ZgesStaatswissenJITE} {"Zeitschrift f\^^bur die gesamte Staatswissenschaft (JITE)"}
MACRO {ZNationalokon} {"Zeitschrift f\^^bur National\^^bokonomie"}
MACRO {ZWirtschSozialwissen} {"Zeitschrift f\^^bur Wirtschafts- und Sozialwissenschaften"}
MACRO {ZWirtschaftspolitik} {"Zeitschrift f\^^bur Wirtschaftspolitik"}
%%---------------------------------------------------------------------------------------
%%	Non-JEL Journals
%%	Created individually
%%---------------------------------------------------------------------------------------
%%---------------------------------------------------------------------------------------
%%	De Gruyter Journals
%%---------------------------------------------------------------------------------------
MACRO {AdvancesEconAnalPol} {"Advances in Economic Analysis and Policy"}
MACRO {AdvancesMacro} {"Advances in Macroeconomics"}
MACRO {AdvancesTheoreticalEcon} {"Advances in Theoretical Economics"}
MACRO {ContribEconAnalPol} {"Contributions to Economic Analysis and Policy"}
MACRO {ContribMacro} {"Contributions to Macroeconomics"}
MACRO {ContribTheoreticalEcon} {"Contributions to Theoretical Economics"}
MACRO {FronEconAnalPol} {"Frontiers of Economic Analysis and Policy"}
MACRO {FronMacro} {"Frontiers of Macroeconomics"}
MACRO {FronTheoreticalEcon} {"Frontiers of Theoretical Economics"}
MACRO {TopicsEconAnalPol} {"Topics in Economic Analysis and Policy"}
MACRO {TopicsMacro} {"Topics in Macroeconomics"}
MACRO {TopicsTheoreticalEcon} {"Topics in Theoretical Economics"}
%%---------------------------------------------------------------------------------------
%%	Some AMS Math Journals
%%---------------------------------------------------------------------------------------
%%	Note: uses AMS style for keyword
%%---------------------------------------------------------------------------------------
MACRO {MATHA} {"Mathematische Annalen"}
MACRO {NIEAW1} {"Nieuw Archief voor Wiskunde"}
MACRO {NOTAM} {"Notices of the American Mathematical Society"}
%%---------------------------------------------------------------------------------------
%%	Some IEEE Journals
%%---------------------------------------------------------------------------------------
%%	Note: uses IEEE style for keyword
%%---------------------------------------------------------------------------------------
MACRO {IEEE_J_SPL} {"{IEEE} Signal Processing Letters"}
MACRO {IEEE_J_AUEA} {"{IEEE} Transactions on Audio and Electroacoustics"}
MACRO {IEEE_J_ASSP} {"{IEEE} Transactions on Acoustics, Speech, and Signal Processing"}
MACRO {IEEE_J_AC} {"{IEEE} Transactions on Automatic Control"}
MACRO {IEEE_J_SP} {"{IEEE} Transactions on Signal Processing"}
MACRO {IEEE_J_IT} {"{IEEE} Transactions on Information Theory"}
MACRO {IEEE_J_PROC} {"Proceedings of the {IEEE}"}
MACRO {IEEE_J_CS} {"{IEEE} Trans. Circuits Syst. I"}
%%---------------------------------------------------------------------------------------
%%	Other Individual Journals
%%---------------------------------------------------------------------------------------
MACRO {ApplStatist} {"Applied Statistics"}
MACRO {AnnFinance} {"Annals of Finance"}
MACRO {AnnMathStatist} {"Annals of Mathematical Statistics"}
MACRO {AnnProb} {"Annals of Probability"}
MACRO {AnnStatist} {"Annals of Statistics"}
MACRO {AmerBusLawJ} {"American Business Law Journal"}
MACRO {FedReg} {"Federal Register"}
MACRO {IntJBifurcationChaos} {"International Journal of Bifurcation and Chaos"}
MACRO {IntJSystSci} {"International Journal of Systems Science"}
MACRO {IntRevCompPublicPol} {"International Review of Comparative Public Policy"}
MACRO {JRoyStatisticalSocietySerA} {"Journal of the Royal Statistical Society, Series A"}
MACRO {JRoyStatisticalSocietySerB} {"Journal of the Royal Statistical Society, Series B"}
MACRO {JStatisticalSocietyLon} {"Journal of the Statistical Society of London"}
MACRO {JTimeSerAn} {"Journal of Time Series Analysis"}
MACRO {PhysicaD} {"Physica D: Nonlinear Phenomena"}
MACRO {PhysRevE} {"Physical Review E"}
MACRO {PhysRevLett} {"Physical Review Letters"}
MACRO {RevGeophys} {"Reviews of Geophysics"}
MACRO {ScandJStatist} {"Scandinavian Journal of Statistics"}
MACRO {StochasticsStochasticRep} {"Stochastics and Stochastic Reports"}
MACRO {WeltArch} {"Weltwirtschaftliches Archiv"}
%%---------------------------------------------------------------------------------------
 % End module: econjour.mbs
%% Copyright 1994-2005 Patrick W Daly
MACRO {acmcs} {"ACM Computing Surveys"}

MACRO {acta} {"Acta Informatica"}

MACRO {cacm} {"Communications of the ACM"}

MACRO {ibmjrd} {"IBM Journal of Research and Development"}

MACRO {ibmsj} {"IBM Systems Journal"}

MACRO {ieeese} {"IEEE Transactions on Software Engineering"}

MACRO {ieeetc} {"IEEE Transactions on Computers"}

MACRO {ieeetcad}
 {"IEEE Transactions on Computer-Aided Design of Integrated Circuits"}

MACRO {ipl} {"Information Processing Letters"}

MACRO {jacm} {"Journal of the ACM"}

MACRO {jcss} {"Journal of Computer and System Sciences"}

MACRO {scp} {"Science of Computer Programming"}

MACRO {sicomp} {"SIAM Journal on Computing"}

MACRO {tocs} {"ACM Transactions on Computer Systems"}

MACRO {tods} {"ACM Transactions on Database Systems"}

MACRO {tog} {"ACM Transactions on Graphics"}

MACRO {toms} {"ACM Transactions on Mathematical Software"}

MACRO {toois} {"ACM Transactions on Office Information Systems"}

MACRO {toplas} {"ACM Transactions on Programming Languages and Systems"}

MACRO {tcs} {"Theoretical Computer Science"}
FUNCTION {bibinfo.check}
{ swap$
  duplicate$ missing$
    {
      pop$ pop$
      ""
    }
    { duplicate$ empty$
        {
          swap$ pop$
        }
        { swap$
          pop$
        }
      if$
    }
  if$
}
FUNCTION {bibinfo.warn}
{ swap$
  duplicate$ missing$
    {
      swap$ "missing " swap$ * " in " * cite$ * warning$ pop$
      ""
    }
    { duplicate$ empty$
        {
          swap$ "empty " swap$ * " in " * cite$ * warning$
        }
        { swap$
          pop$
        }
      if$
    }
  if$
}
STRINGS  { bibinfo}
INTEGERS { nameptr namesleft numnames }

FUNCTION {format.names}
{ 'bibinfo :=
  duplicate$ empty$ 'skip$ {
  's :=
  "" 't :=
  #1 'nameptr :=
  s num.names$ 'numnames :=
  numnames 'namesleft :=
    { namesleft #0 > }
    { s nameptr
      duplicate$ #1 >
        { "{f.~}{vv~}{ll}{, jj}" }
        { "{vv~}{ll}{, f.}{, jj}" }
      if$
      format.name$
      bibinfo bibinfo.check
      't :=
      nameptr #1 >
        {
          namesleft #1 >
            { ", " * t * }
            {
              s nameptr "{ll}" format.name$ duplicate$ "others" =
                { 't := }
                { pop$ }
              if$
              numnames #2 >
                { "," * }
                'skip$
              if$
              t "others" =
                {
                  " " * bbl.etal *
                }
                {
                  bbl.and
                  space.word * t *
                }
              if$
            }
          if$
        }
        't
      if$
      nameptr #1 + 'nameptr :=
      namesleft #1 - 'namesleft :=
    }
  while$
  } if$
}
FUNCTION {format.names.ed}
{
  'bibinfo :=
  duplicate$ empty$ 'skip$ {
  's :=
  "" 't :=
  #1 'nameptr :=
  s num.names$ 'numnames :=
  numnames 'namesleft :=
    { namesleft #0 > }
    { s nameptr
      "{f.~}{vv~}{ll}{, jj}"
      format.name$
      bibinfo bibinfo.check
      't :=
      nameptr #1 >
        {
          namesleft #1 >
            { ", " * t * }
            {
              s nameptr "{ll}" format.name$ duplicate$ "others" =
                { 't := }
                { pop$ }
              if$
              numnames #2 >
                { "," * }
                'skip$
              if$
              t "others" =
                {

                  " " * bbl.etal *
                }
                {
                  bbl.and
                  space.word * t *
                }
              if$
            }
          if$
        }
        't
      if$
      nameptr #1 + 'nameptr :=
      namesleft #1 - 'namesleft :=
    }
  while$
  } if$
}
FUNCTION {format.key}
{ empty$
    { key field.or.null }
    { "" }
  if$
}

FUNCTION {format.authors}
{ author "author" format.names
}
FUNCTION {get.bbl.editor}
{ editor num.names$ #1 > 'bbl.editors 'bbl.editor if$ }

FUNCTION {format.editors}
{ editor "editor" format.names duplicate$ empty$ 'skip$
    {
      "," *
      " " *
      get.bbl.editor
      *
    }
  if$
}
FUNCTION {format.note}
{
  url empty$
    'skip$
    { "\urlprefix\url{" url * "}" * output }
  if$
 note empty$
    { "" }
    { note #1 #1 substring$
      duplicate$ "{" =
        'skip$
        { output.state mid.sentence =
          { "l" }
          { "u" }
        if$
        change.case$
        }
      if$
      note #2 global.max$ substring$ * "note" bibinfo.check
    }
  if$
}

FUNCTION {format.title}
{ title
  duplicate$ empty$ 'skip$
    { "t" change.case$ }
  if$
  "title" bibinfo.check
  duplicate$ empty$ 'skip$
    {
      "\enquote{" swap$ *
      non.stop
        { ",} " * }
        { "} " * }
      if$
    }
  if$
}
FUNCTION {end.quote.title}
{ title empty$
    'skip$
    { before.all 'output.state := }
  if$
}
FUNCTION {format.full.names}
{'s :=
 "" 't :=
  #1 'nameptr :=
  s num.names$ 'numnames :=
  numnames 'namesleft :=
    { namesleft #0 > }
    { s nameptr
      "{vv~}{ll}" format.name$
      't :=
      nameptr #1 >
        {
          namesleft #1 >
            { ", " * t * }
            {
              s nameptr "{ll}" format.name$ duplicate$ "others" =
                { 't := }
                { pop$ }
              if$
              t "others" =
                {
                  " " * bbl.etal *
                }
                {
                  numnames #2 >
                    { "," * }
                    'skip$
                  if$
                  bbl.and
                  space.word * t *
                }
              if$
            }
          if$
        }
        't
      if$
      nameptr #1 + 'nameptr :=
      namesleft #1 - 'namesleft :=
    }
  while$
}

FUNCTION {author.editor.key.full}
{ author empty$
    { editor empty$
        { key empty$
            { cite$ #1 #3 substring$ }
            'key
          if$
        }
        { editor format.full.names }
      if$
    }
    { author format.full.names }
  if$
}

FUNCTION {author.key.full}
{ author empty$
    { key empty$
         { cite$ #1 #3 substring$ }
          'key
      if$
    }
    { author format.full.names }
  if$
}

FUNCTION {editor.key.full}
{ editor empty$
    { key empty$
         { cite$ #1 #3 substring$ }
          'key
      if$
    }
    { editor format.full.names }
  if$
}

FUNCTION {make.full.names}
{ type$ "book" =
  type$ "inbook" =
  or
    'author.editor.key.full
    { type$ "proceedings" =
        'editor.key.full
        'author.key.full
      if$
    }
  if$
}

FUNCTION {output.bibitem}
{ newline$
  "\bibitem[{" write$
  label write$
  ")" make.full.names duplicate$ short.list =
     { pop$ }
     { * }
   if$
  "}]{" * write$
  cite$ write$
  "}" write$
  newline$
  ""
  before.all 'output.state :=
}

FUNCTION {n.dashify}
{
  't :=
  ""
    { t empty$ not }
    { t #1 #1 substring$ "-" =
        { t #1 #2 substring$ "--" = not
            { "--" *
              t #2 global.max$ substring$ 't :=
            }
            {   { t #1 #1 substring$ "-" = }
                { "-" *
                  t #2 global.max$ substring$ 't :=
                }
              while$
            }
          if$
        }
        { t #1 #1 substring$ *
          t #2 global.max$ substring$ 't :=
        }
      if$
    }
  while$
}

FUNCTION {word.in}
{ bbl.in
  " " * }

FUNCTION {format.date}
{ year "year" bibinfo.check duplicate$ empty$
    {
      "empty year in " cite$ * "; set to ????" * warning$
       pop$ "????"
    }
    'skip$
  if$
  extra.label *
  before.all 'output.state :=
  " (" swap$ * ")" *
}
FUNCTION {format.btitle}
{ title "title" bibinfo.check
  duplicate$ empty$ 'skip$
    {
      emphasize
    }
  if$
}
FUNCTION {either.or.check}
{ empty$
    'pop$
    { "can't use both " swap$ * " fields in " * cite$ * warning$ }
  if$
}
FUNCTION {format.bvolume}
{ volume empty$
    { "" }
    { bbl.volume volume tie.or.space.prefix
      "volume" bibinfo.check * *
      series "series" bibinfo.check
      duplicate$ empty$ 'pop$
        { emphasize ", " * swap$ * }
      if$
      "volume and number" number either.or.check
    }
  if$
}
FUNCTION {format.number.series}
{ volume empty$
    { number empty$
        { series field.or.null }
        { series empty$
            { number "number" bibinfo.check }
            { output.state mid.sentence =
                { bbl.number }
                { bbl.number capitalize }
              if$
              number tie.or.space.prefix "number" bibinfo.check * *
              bbl.in space.word *
              series "series" bibinfo.check *
            }
          if$
        }
      if$
    }
    { "" }
  if$
}

FUNCTION {format.edition}
{ edition duplicate$ empty$ 'skip$
    {
      output.state mid.sentence =
        { "l" }
        { "t" }
      if$ change.case$
      "edition" bibinfo.check
      " " * bbl.edition *
    }
  if$
}
INTEGERS { multiresult }
FUNCTION {multi.page.check}
{ 't :=
  #0 'multiresult :=
    { multiresult not
      t empty$ not
      and
    }
    { t #1 #1 substring$
      duplicate$ "-" =
      swap$ duplicate$ "," =
      swap$ "+" =
      or or
        { #1 'multiresult := }
        { t #2 global.max$ substring$ 't := }
      if$
    }
  while$
  multiresult
}
FUNCTION {format.pages}
{ pages duplicate$ empty$ 'skip$
    { duplicate$ multi.page.check
        {
          n.dashify
        }
        {
        }
      if$
      "pages" bibinfo.check
    }
  if$
}
FUNCTION {format.journal.pages}
{ pages duplicate$ empty$ 'pop$
    { swap$ duplicate$ empty$
        { pop$ pop$ format.pages }
        {
          ", " *
          swap$
          n.dashify
          "pages" bibinfo.check
          *
        }
      if$
    }
  if$
}
FUNCTION {format.journal.eid}
{ eid "eid" bibinfo.check
  duplicate$ empty$ 'pop$
    { swap$ duplicate$ empty$ 'skip$
      {
          ", " *
      }
      if$
      swap$ *
    }
  if$
}
FUNCTION {format.vol.num.pages}
{ volume field.or.null
  duplicate$ empty$ 'skip$
    {
      "volume" bibinfo.check
    }
  if$
}

FUNCTION {format.chapter.pages}
{ chapter empty$
    { "" }
    { type empty$
        { bbl.chapter }
        { type "l" change.case$
          "type" bibinfo.check
        }
      if$
      chapter tie.or.space.prefix
      "chapter" bibinfo.check
      * *
    }
  if$
}

FUNCTION {format.booktitle}
{
  booktitle "booktitle" bibinfo.check
  emphasize
}
FUNCTION {format.in.ed.booktitle}
{ format.booktitle duplicate$ empty$ 'skip$
    {
      format.bvolume duplicate$ empty$ 'pop$
        { ", " swap$ * * }
      if$
      editor "editor" format.names.ed duplicate$ empty$ 'pop$
        {
          "," *
          " " *
          get.bbl.editor
          ", " *
          * swap$
          * }
      if$
      word.in swap$ *
    }
  if$
}
FUNCTION {format.thesis.type}
{ type duplicate$ empty$
    'pop$
    { swap$ pop$
      "t" change.case$ "type" bibinfo.check
    }
  if$
}
FUNCTION {format.tr.number}
{ number "number" bibinfo.check
  type duplicate$ empty$
    { pop$ bbl.techrep }
    'skip$
  if$
  "type" bibinfo.check
  swap$ duplicate$ empty$
    { pop$ "t" change.case$ }
    { tie.or.space.prefix * * }
  if$
}
FUNCTION {format.article.crossref}
{
  word.in
  " \cite{" * crossref * "}" *
}
FUNCTION {format.book.crossref}
{ volume duplicate$ empty$
    { "empty volume in " cite$ * "'s crossref of " * crossref * warning$
      pop$ word.in
    }
    { bbl.volume
      swap$ tie.or.space.prefix "volume" bibinfo.check * * bbl.of space.word *
    }
  if$
  " \cite{" * crossref * "}" *
}
FUNCTION {format.incoll.inproc.crossref}
{
  word.in
  " \cite{" * crossref * "}" *
}
FUNCTION {format.org.or.pub}
{ 't :=
  ""
  address empty$ t empty$ and
    'skip$
    {
      address "address" bibinfo.check *
      t empty$
        'skip$
        { address empty$
            'skip$
            { ": " * }
          if$
          t *
        }
      if$
    }
  if$
}
FUNCTION {format.publisher.address}
{ publisher "publisher" bibinfo.warn format.org.or.pub
}

FUNCTION {format.organization.address}
{ organization "organization" bibinfo.check format.org.or.pub
}

FUNCTION {article}
{ output.bibitem
  format.authors "author" output.check
  author format.key output
  format.date "year" output.check
  date.block
  format.title "title" output.check
  end.quote.title
  crossref missing$
    {
      journal
      "journal" bibinfo.check
      emphasize
      "journal" output.check
      format.vol.num.pages output
    }
    { format.article.crossref output.nonnull
    }
  if$
  eid empty$
    { format.journal.pages }
    { format.journal.eid }
  if$
  format.note output
  fin.entry
}
FUNCTION {book}
{ output.bibitem
  author empty$
    { format.editors "author and editor" output.check
      editor format.key output
    }
    { format.authors output.nonnull
      crossref missing$
        { "author and editor" editor either.or.check }
        'skip$
      if$
    }
  if$
  format.date "year" output.check
  date.block
  format.btitle "title" output.check
  crossref missing$
    { format.bvolume output
      format.number.series output
      format.publisher.address output
    }
    {
      format.book.crossref output.nonnull
    }
  if$
  format.edition output
  format.note output
  fin.entry
}
FUNCTION {booklet}
{ output.bibitem
  format.authors output
  author format.key output
  format.date "year" output.check
  date.block
  format.title "title" output.check
  end.quote.title
  howpublished "howpublished" bibinfo.check output
  address "address" bibinfo.check output
  format.note output
  fin.entry
}

FUNCTION {inbook}
{ output.bibitem
  author empty$
    { format.editors "author and editor" output.check
      editor format.key output
    }
    { format.authors output.nonnull
      crossref missing$
        { "author and editor" editor either.or.check }
        'skip$
      if$
    }
  if$
  format.date "year" output.check
  date.block
  format.btitle "title" output.check
  crossref missing$
    {
      format.number.series output
      format.publisher.address output
      format.bvolume output
      format.chapter.pages "chapter and pages" output.check
    }
    {
      format.chapter.pages "chapter and pages" output.check
      format.book.crossref output.nonnull
    }
  if$
  format.edition output
  format.pages "pages" output.check
  format.note output
  fin.entry
}

FUNCTION {incollection}
{ output.bibitem
  format.authors "author" output.check
  author format.key output
  format.date "year" output.check
  date.block
  format.title "title" output.check
  end.quote.title
  crossref missing$
    { format.in.ed.booktitle "booktitle" output.check
      format.number.series output
      format.publisher.address output
      format.chapter.pages output
      format.edition output
    }
    { format.incoll.inproc.crossref output.nonnull
      format.chapter.pages output
    }
  if$
  format.pages "pages" output.check
  format.note output
  fin.entry
}
FUNCTION {inproceedings}
{ output.bibitem
  format.authors "author" output.check
  author format.key output
  format.date "year" output.check
  date.block
  format.title "title" output.check
  end.quote.title
  crossref missing$
    { format.in.ed.booktitle "booktitle" output.check
      format.number.series output
      publisher empty$
        { format.organization.address output }
        { organization "organization" bibinfo.check output
          format.publisher.address output
        }
      if$
      format.bvolume output
    }
    { format.incoll.inproc.crossref output.nonnull
    }
  if$
  format.pages "pages" output.check
  format.note output
  fin.entry
}
FUNCTION {conference} { inproceedings }
FUNCTION {manual}
{ output.bibitem
  format.authors output
  author format.key output
  format.date "year" output.check
  date.block
  format.btitle "title" output.check
  organization "organization" bibinfo.check output
  address "address" bibinfo.check output
  format.edition output
  format.note output
  fin.entry
}

FUNCTION {mastersthesis}
{ output.bibitem
  format.authors "author" output.check
  author format.key output
  format.date "year" output.check
  date.block
  format.btitle
  "title" output.check
  bbl.mthesis format.thesis.type output.nonnull
  school "school" bibinfo.warn output
  address "address" bibinfo.check output
  format.note output
  fin.entry
}

FUNCTION {misc}
{ output.bibitem
  format.authors output
  author format.key output
  format.date "year" output.check
  date.block
  format.title output
  end.quote.title
  howpublished "howpublished" bibinfo.check output
  format.note output
  fin.entry
}
FUNCTION {phdthesis}
{ output.bibitem
  format.authors "author" output.check
  author format.key output
  format.date "year" output.check
  date.block
  format.btitle
  "title" output.check
  bbl.phdthesis format.thesis.type output.nonnull
  school "school" bibinfo.warn output
  address "address" bibinfo.check output
  format.note output
  fin.entry
}

FUNCTION {proceedings}
{ output.bibitem
  format.editors output
  editor format.key output
  format.date "year" output.check
  date.block
  format.btitle "title" output.check
  format.bvolume output
  format.number.series output
  publisher empty$
    { format.organization.address output }
    { organization "organization" bibinfo.check output
      format.publisher.address output
    }
  if$
  format.note output
  fin.entry
}

FUNCTION {techreport}
{ output.bibitem
  format.authors "author" output.check
  author format.key output
  format.date "year" output.check
  date.block
  format.title
  "title" output.check
  end.quote.title
  format.tr.number output.nonnull
  institution "institution" bibinfo.warn output
  address "address" bibinfo.check output
  format.note output
  fin.entry
}

FUNCTION {unpublished}
{ output.bibitem
  format.authors "author" output.check
  author format.key output
  format.date "year" output.check
  date.block
  format.title "title" output.check
  end.quote.title
  format.note "note" output.check
  fin.entry
}

FUNCTION {default.type} { misc }
READ
FUNCTION {sortify}
{ purify$
  "l" change.case$
}
INTEGERS { len }
FUNCTION {chop.word}
{ 's :=
  'len :=
  s #1 len substring$ =
    { s len #1 + global.max$ substring$ }
    's
  if$
}
FUNCTION {format.lab.names}
{ 's :=
  "" 't :=
  s #1 "{vv~}{ll}" format.name$
  s num.names$ duplicate$
  #2 >
    { pop$
      " " * bbl.etal *
    }
    { #2 <
        'skip$
        { s #2 "{ff }{vv }{ll}{ jj}" format.name$ "others" =
            {
              " " * bbl.etal *
            }
            { bbl.and space.word * s #2 "{vv~}{ll}" format.name$
              * }
          if$
        }
      if$
    }
  if$
}

FUNCTION {author.key.label}
{ author empty$
    { key empty$
        { cite$ #1 #3 substring$ }
        'key
      if$
    }
    { author format.lab.names }
  if$
}

FUNCTION {author.editor.key.label}
{ author empty$
    { editor empty$
        { key empty$
            { cite$ #1 #3 substring$ }
            'key
          if$
        }
        { editor format.lab.names }
      if$
    }
    { author format.lab.names }
  if$
}

FUNCTION {editor.key.label}
{ editor empty$
    { key empty$
        { cite$ #1 #3 substring$ }
        'key
      if$
    }
    { editor format.lab.names }
  if$
}

FUNCTION {calc.short.authors}
{ type$ "book" =
  type$ "inbook" =
  or
    'author.editor.key.label
    { type$ "proceedings" =
        'editor.key.label
        'author.key.label
      if$
    }
  if$
  'short.list :=
}

FUNCTION {calc.label}
{ calc.short.authors
  short.list
  "("
  *
  year duplicate$ empty$
  short.list key field.or.null = or
     { pop$ "" }
     'skip$
  if$
  *
  'label :=
}

FUNCTION {sort.format.names}
{ 's :=
  #1 'nameptr :=
  ""
  s num.names$ 'numnames :=
  numnames 'namesleft :=
    { namesleft #0 > }
    { s nameptr
      "{vv{ } }{ll{ }}{  f{ }}{  jj{ }}"
      format.name$ 't :=
      nameptr #1 >
        {
          "   "  *
          namesleft #1 = t "others" = and
            { "zzzzz" * }
            { t sortify * }
          if$
        }
        { t sortify * }
      if$
      nameptr #1 + 'nameptr :=
      namesleft #1 - 'namesleft :=
    }
  while$
}

FUNCTION {sort.format.title}
{ 't :=
  "A " #2
    "An " #3
      "The " #4 t chop.word
    chop.word
  chop.word
  sortify
  #1 global.max$ substring$
}
FUNCTION {author.sort}
{ author empty$
    { key empty$
        { "to sort, need author or key in " cite$ * warning$
          ""
        }
        { key sortify }
      if$
    }
    { author sort.format.names }
  if$
}
FUNCTION {author.editor.sort}
{ author empty$
    { editor empty$
        { key empty$
            { "to sort, need author, editor, or key in " cite$ * warning$
              ""
            }
            { key sortify }
          if$
        }
        { editor sort.format.names }
      if$
    }
    { author sort.format.names }
  if$
}
FUNCTION {editor.sort}
{ editor empty$
    { key empty$
        { "to sort, need editor or key in " cite$ * warning$
          ""
        }
        { key sortify }
      if$
    }
    { editor sort.format.names }
  if$
}
FUNCTION {presort}
{ calc.label
  label sortify
  "    "
  *
  type$ "book" =
  type$ "inbook" =
  or
    'author.editor.sort
    { type$ "proceedings" =
        'editor.sort
        'author.sort
      if$
    }
  if$
  #1 entry.max$ substring$
  'sort.label :=
  sort.label
  *
  "    "
  *
  title field.or.null
  sort.format.title
  *
  #1 entry.max$ substring$
  'sort.key$ :=
}

ITERATE {presort}
SORT
STRINGS { last.label next.extra }
INTEGERS { last.extra.num number.label }
FUNCTION {initialize.extra.label.stuff}
{ #0 int.to.chr$ 'last.label :=
  "" 'next.extra :=
  #0 'last.extra.num :=
  #0 'number.label :=
}
FUNCTION {forward.pass}
{ last.label label =
    { last.extra.num #1 + 'last.extra.num :=
      last.extra.num int.to.chr$ 'extra.label :=
    }
    { "a" chr.to.int$ 'last.extra.num :=
      "" 'extra.label :=
      label 'last.label :=
    }
  if$
  number.label #1 + 'number.label :=
}
FUNCTION {reverse.pass}
{ next.extra "b" =
    { "a" 'extra.label := }
    'skip$
  if$
  extra.label 'next.extra :=
  extra.label
  duplicate$ empty$
    'skip$
    { "{\natexlab{" swap$ * "}}" * }
  if$
  'extra.label :=
  label extra.label * 'label :=
}
EXECUTE {initialize.extra.label.stuff}
ITERATE {forward.pass}
REVERSE {reverse.pass}
FUNCTION {bib.sort.order}
{ sort.label
  "    "
  *
  year field.or.null sortify
  *
  "    "
  *
  title field.or.null
  sort.format.title
  *
  #1 entry.max$ substring$
  'sort.key$ :=
}
ITERATE {bib.sort.order}
SORT
FUNCTION {begin.bib}
{ preamble$ empty$
    'skip$
    { preamble$ write$ newline$ }
  if$
  "\begin{thebibliography}{" number.label int.to.str$ * "}" *
  write$ newline$
  "\newcommand{\enquote}[1]{``#1''}"
  write$ newline$
  "\providecommand{\natexlab}[1]{#1}"
  write$ newline$
  "\providecommand{\url}[1]{\texttt{#1}}"
  write$ newline$
  "\providecommand{\urlprefix}{URL }"
  write$ newline$
}
EXECUTE {begin.bib}
EXECUTE {init.state.consts}
ITERATE {call.type$}
FUNCTION {end.bib}
{ newline$
  "\end{thebibliography}" write$ newline$
}
EXECUTE {end.bib}
%% End of customized bst file
%%
%% End of file `DeGruyter.bst'.
