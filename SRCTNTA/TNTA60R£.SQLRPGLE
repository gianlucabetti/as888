000100051102     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000110051102
000120051102      *------------------------------------------------------------------------*
000130051117      *
000140051117      *                 GESTIONE ANAGRAFICO CLIENTI        ?
000150051117      *
000160051102      *------------------------------------------------------------------------*
000170060406      * ? ATTENZIONE!!!!! ?
000180060406      *? ?  Il posizionamento sul file video dei campi errati è gestito con le
000190060406      *? ?  posizioni di riga e colonna del campo a video e non con gli
000200060406      *? ?  indicatori, ne servivano troppi.
000210060406      *? ?  Se viene spostato un campo nel video aggiornare anche il relativo
000220060406      *? ?  posizionamento del cursore, campi HxRIGA e HxCOLO
000230051117      *
000240051117      *------------------------------------------------------------------------*
000250051102
000260051221     fCnaco00f  uf a e           k disk
000270120510     fCnaco16l  if   e           k disk    rename(cnaco000:cnaco16)  usropn
000280120510     f                                     prefix (ww_)
000290051115     fTfaco00f  uf a e           k disk    rename(cnaco000:tfaco000) usropn
000300060105     fCnind00f  uf a e           k disk    usropn
000310060105     fCnind02l  if   e           k disk    rename(cnind000:cnind002) usropn
000320060301     f                                     prefix(ww_)
000330051115     fTfind00f  uf a e           k disk    rename(cnind000:tfind000) usropn
000340051117     fCnclp00f  uf a e           k disk    usropn
000350051115     fTfclp00f  uf a e           k disk    rename(cnclp000:tfclp000) usropn
000360051117     fFncls01l  uf a e           k disk    usropn
000370051124     fTfcls01l  uf a e           k disk    rename(fncls000:tfcls000) usropn
000380051108     fAzorg01l  if   e           k disk
000390130806     fAZCMM01L  if   e           k disk
000400051129     fCnabi00f  if   e           k disk
000410051220     fFnacr01l  if   e           k disk    usropn
000420051117     fFnspe01l  if   e           k disk    usropn
000430051219     fFnsp201l  if   e           k disk    usropn
000440051107     fTabel00f  if   e           k disk
000450091119     fTfntc01l  uf a e           k disk
000460051220     fTncpo01l  uf   e           k disk
000470080306     FTNCPO05L  IF   E           K DISK    rename(tncpo000:tncpo005)
000480080306     F                                     prefix(c_)
000490080306     f                                     usropn
000500080306     FTNCPO06L  IF   E           K DISK    rename(tncpo000:tncpo006)
000510080306     F                                     prefix(c_)
000520080306     f                                     usropn
000530140714     fTncpo11l  if   e           k disk
000540110516     FTIATC07L  IF   E           K DISK
000550120510     fTICPI01L  if   e           k disk
000560051102     fTnta60d   cf   e             workstn
000570051107
000580051107      *------------------------------------------------------------------------*
000590051107      *  RIEPILOGO INDICATORI
000600051107      *------------------------------------------------------------------------*
000610051107      * 01 - Immissione
000620051107      * 02 - Modifica
000630051107      * 04 - Annullato
000640051107      * 05 - Visualizza
000650100806      * 06 - Anagrafica provvisoria da trattativa
000660051115      * 07 - Convalida offerte
000670051107      * 08 - Utente non abilitato - esce dal pgm
000680051115      * 09 - Sono in sede
000690051115      * 10 - Luogo 001 presente
000700051115      * 11 - Luogo 555 presente
000710051115      * 12 - Luogo 666 presente
000720051115      * 13 - Nota   85 presente
000730051115      * 14 - Luogo 005 presente
000740051115      * 15 - Nota   88 presente
000750051115      * 16 - Luogo 006 presente
000760051115      * 17 - Luogo 008 presente
000770051115      * 18 - Nota   87 presente
000780120928      * 19 - Protegge stato del credito
000790051128      * 20 - Utente EDP
000800051129      * 21 - Nota   84 presente
000810051129      * 22 - Non visualizzo le coordinate bancarie per pag. c/a
000820051129      * 23 - Proteggo le coordinate bancarie per pag. c/a
000830140722      * 24 - Note 02 o 03 presenti
000840140722      * 26 - Note 05 o 06 o T5 o I5 presenti
000850051110      * 28 - Errore generico
000860051221      * 29 - Sono in seconda videata
000870051107      * 30 - Comodo
000880140722      * 31 - Note 07 o 08 o T7 o I7 presenti
000890060208      * 33 - Luogo 300 presente
000900060208      * 34 - Nota   83 presente
000910060208      * 35 - Luogo 002 presente
000920060208      * 36 - Nota   86 presente
000930060208      * 37 - Luogo 200 presente
000940061106      * 39 - Codice ISO impostato in automatico
000950090612      * 40 - Nota   89 presente
000960051117      * 41/64 - P.o. di ricerca
000970100407      * 65 - Inibisco il tasto F20 potenziale xchè se no chiamata ricorsiva
000980100507      * 67 - Abilito F5-Attività
000990100728      * 68 - Nota   82 presente
001000091112      * 70 - Trattativa su cliente
001010100728      * 71 - Non visualizzo le coordinate bancarie per pag. Danni
001020100728      * 72 - Proteggo le coordinate bancarie per pag. Danni
001030100728      * 73 - Non visualizzo le coordinate bancarie per pag. Note Accr.
001040100728      * 74 - Proteggo le coordinate bancarie per pag. Note Accr.
001050100803      * 75 - Non visualizzo BIC contrassegni
001060100803      * 76 - Non visualizzo BIC danni
001070100803      * 77 - Non visualizzo BIC note accredito
001080111007      * 78 - F4 abilitato
001090120925      * 79 - Protegge blocco cliente
001100060217      * 80 - Non devo uscire dal pgm se F3
001110120928      * 81 - Protegge blocco pagamenti
001120150415      * 82 - Protegge Banca ABI/CAB pagamento fatture
001130170320      * 83 - Protegge Frequenza Fattura x Cl.in contenzioso
001140170320      * 84 - Protegge Codice Pagamento x Cl.in contenzioso
001150170421      * 85 - Protegge Flag Gestione Giacenze
001160051124      * 90 - Riemetto la videata
001170051107      *------------------------------------------------------------------------*
001180051107
001190051107      *   V A R I A B I L I
001200150424     d BonificoCA      s               n   inz(*off)
001210150424     d BonificoDN      s               n   inz(*off)
001220150424     d BonificoNA      s               n   inz(*off)
001230150427     d CodPagCA        s              1a   inz
001240150427     d CodPagDN        s              1a   inz
001250150427     d CodPagNA        s              1a   inz
001260051219     d codut           s                   like(TblKut) inz(1)
001270051118     d comando         s              1
001280090417     d conta           s              5i 0
001290051116     d dataiso         s               d   datfmt(*iso)
001300060526     d dataeur         s               d   datfmt(*eur)
001310051116     d dataoggi        s              8  0
001320051129     d dataoggi1       s              8  0
001330051116     d datascad        s              8  0
001340100406     d dataCRM         s              8  0
001350161129     d EndW06          s               n   inz(*off)
001360150424     d EoFW06          s               n   inz(*off)
001370080116     d erriban         s               n
001380051129     d kabi            s                   like(IndAbi)
001390051129     d kcab            s                   like(IndCab)
001400051129     d kcod            s                   like(TblCod)
001410060109     d kindiva         s                   like(IndIva)
001420051129     d kkey            s                   like(TblKey)
001430051222     d kksc            s                   like(AcoKsc)
001440051108     d kkut            s              1  0
001450051216     d kntcapl         s                   like(NtcApl)
001460051216     d kntcnk1         s                   like(NtcNk1)
001470051216     d kntcnk2         s                   like(NtcNk2)
001480051216     d kntctnt         s                   like(NtcTnt)
001490051216     d kspefls         s                   like(SpeFls) inz('L')
001500051216     d kspecli         s                   like(SpeCli)
001510051216     d kspecod         s                   like(SpeCod)
001520051219     d ksp2tpe         s                   like(Sp2Tpe) inz('EM')
001530051118     d ordina          s              1
001540150427     d PagEsteroCA     s               n   inz(*off)
001550150427     d PagEsteroDN     s               n   inz(*off)
001560150427     d PagEsteroNA     s               n   inz(*off)
001570051108     d parcdf          s             16
001580051108     d pardit          s              3
001590051108     d pardut          s             30
001600051129     d parkcc          s                   like(AcoKcc)
001610051108     d parerr          s              2
001620051108     d paresci         s              1
001630051108     d parfil          s             90
001640051108     d pariva          s             16
001650110407     d ParmCbl         s              3
001660051108     d parsta          s              1  0
001670051108     d paxkcm          s             80
001680051108     d paxkdm          s             60
001690051108     d paxksm          s            140
001700051108     d paxnum          s              2  0
001710090616     d rqsformatname...
001720090616     d                 S             10A
001730100729     d rqsData         s            256a
001740090616     d rqsdatasize...
001750090616     d                 S             10I 0
001760090616     d rqsopcode...
001770090616     d                 S             10A
001780090616     d rpyformatname...
001790090616     d                 S             10A
001800100729     d rpyData         s            256a
001810090616     d rpydatasize...
001820090616     d                 S             10I 0
001830090616     d rpyesito...
001840090616     d                 S             10I 0
001850100729     d savABIdn        s                   like(CLPabi)
001860100729     d savABIna        s                   like(CLPabi)
001870051220     d savabl          s                   like(AcoAbl)
001880100729     d savCABdn        s                   like(CLPcab)
001890100729     d savCABna        s                   like(CLPcab)
001900150427     d savCLStic       s                   like(CLStic)
001910051117     d savcpo          s                   like(AcoLib)
001920051130     d savfft          s                   like(v4cfft)
001930100730     d savIBANdn       s                   like(v5ibandn)
001940100730     d savIBANna       s                   like(v5ibanna)
001941171006     d savin05         s               n   inz
001950060215     d savin22         s              1
001960060215     d savin23         s              1
001970100728     d savin71         s              1
001980100728     d savin72         s              1
001990100728     d savin73         s              1
002000100728     d savin74         s              1
002010051130     d savitc          s                   like(v3citc)
002020091119     d savntcdc        s                   like(v2ntcdc)
002030081119     d savntwscf       s                   like(§ogntw)
002040100127     d savscf          s                   like(v4cscf)
002050051130     d savtft          s                   like(v4ctft)
002060100802     d savtipdn        s                   like(v5tpdn)
002070100802     d savtipna        s                   like(v5tpna)
002080100805     d dfttipdn        s                   like(v5tpdn)
002090100805     d dfttipna        s                   like(v5tpna)
002100051130     d savuni          s                   like(v4cuni)
002110080422     d sav4utip        s                   like(§4utip)
002120110217     d sav_Livello     s                   like(§oglpo) inz
002130051118     d xivaprv         s              2
002140051118     d xstato          s              1  0
002150051107     d xx              s              2  0
002160051108     d yy              s              2  0
002170160905     d ww              s              3s 0 inz
002180051107     d wabi            s                   like(UteAut)
002190051129     d wagenzia        s             16
002200051129     d wbanca          s             20
002210100730     d wbic            s                   like(v5bicdn)
002220051202     d wbloc           s              1
002230051118     d wcae            s                   like(v2ccae)
002240051118     d wcit            s                   like(v2ccit)
002250110217     d wCodIncAtt      s              7    inz
002260100805     d wcodpn          s                   like(v5tpdn)
002270100805     d wcodpo          s                   like(v5tpdn)
002280100729     d wdesbanca       s                   like(CLPbab)
002290051122     d wdestab         s             25
002300051212     d wfax005         s                   like(SpeFax)
002310051220     d wforza          s              1    inz(*Off)
002320061030     d wforzacdf       s              1    inz(*Off)
002330060105     d wforzacon       s              1    inz(*Off)
002340060711     d wforzaksc       s              1    inz(*Off)
002350061106     d wforzaiva       s              1    inz(*Off)
002360150728     d wForzaMail      s               n   inz(*off)
002370150424     d wForzaPag       s               n   inz(*off)
002380170516     d wForzaPotenziale...
002390170516     d                 s               n   inz(*off)
002400051128     d wfscf           s              1    inz
002410051122     d wfz3            s                   like(E13Fz3)
002420051116     d wgiorni         s              3  0
002430100729     d wiban           s                   like(v5ciban)
002440150424     d WinInfo         s               n   inz(*off)
002450051118     d wksc            s                   like(AcoKsc) inz
002460051118     d wksc04          s              4  0
002470051219     d wmail005        s                   like(Sp2Est)
002480051220     d wokabl          s              1    inz(*Off)
002490051219     d wokcap          s              1    inz(*Off)
002500051220     d wokimm          s              1    inz(*Off)
002510051219     d wokpot          s              1    inz(*Off)
002520051219     d wopz            s                   like(AcoOpz)
002530100728     d wpag            s              2a
002540100805     d wpgm            s             10a
002550051122     d wpoage          s              3  0
002560060711     d wpoageuni       s              3
002570051122     d wpocli          s              3
002580051117     d wpos            s              2  0
002590051129     d wpos1           s              2  0
002600051129     d wpos2           s              2  0
002610051118     d wprv            s                   like(v2cprv)
002620051124     d wrag            s             40
002630051207     d wrich           s                   like(ta12ric)
002640140722     d wRGR            s                   like(TA12rgr)   inz
002650051118     d wsta            s                   like(v2csta)
002660080222     d w_scfsta        s                   like(indsta)
002670051117     d wtel            s                   like(v2ctel)
002680051219     d wtic            s                   like(AcoTic)
002690051118     d wtlf            s                   like(v2ctlf)
002700051207     d wtnt            s                   like(NtcTnt)
002710051118     d wvia            s                   like(v2cvia)
002720051213     d wvideo          s              2
002730051108     d wtibs69         s              1    inz('0')
002740060531     d wtibs73         s              1    inz('0')
002750100127     d wtnta40         s              1n   inz(*off)
002760081020     d wtrmk50         s              1    inz('0')
002770101014     d wuffsede        s             35
002780051216     d w001a           s              1
002790051108     d w002a           s              2
002800051115     d w003a           s              3
002810051108     d w0030           s              3  0
002820051107     d w004a           s              4
002830051117     d w0040           s              4  0
002840070704     d w0040ksc        s              4  0
002850051124     d w005a           s              5
002860051220     d w0060           s              6  0
002870051124     d w0070           s              7  0
002880051220     d w0100           s             10  0
002890051108     d zz              s              2  0
002900110407     d $Blocco         s              1n
002910051219     d $d01            s              1    inz('0')
002920051129     d §fil            s             90
002930051129     d $h              s              3  0
002940091104     d $interroga      s              1n
002950051129     d $j              s              3  0
002960051129     d $k              s              3  0
002970051129     d $loop           s              1  0
002980091124     d $modifica       s              1n
002990091106     d $pgmrich        s              1n
003000051129     d $rig            s              1  0
003010051108     d §tip            s              1
003020051122     d $x              s              3  0
003030051122     d $y              s              3  0
003040051122     d $z              s              3  0
003050071128     d $ufi001         s                   like(§4lufi)
003060071128     d $ufi002         s                   like(§4lufi)
003070071128     d $ufi005         s                   like(§4lufi)
003080071128     d $ufi006         s                   like(§4lufi)
003090071128     d $ufi008         s                   like(§4lufi)
003100071128     d $ufi200         s                   like(§4lufi)
003110071128     d $ufi300         s                   like(§4lufi)
003120071128     d $ufi555         s                   like(§4lufi)
003130071128     d $ufi666         s                   like(§4lufi)
003140080422     d $win            s               n
003150100729     d $windn          s               n
003160100729     d $winna          s               n
003170080306     d savcdf          s                   like(v2ccdf) inz
003180080306     d cod_forzcdf     s                   like(cpocdf) inz
003190080306     d cod_forziva     s                   like(cpopiv) inz
003200080306     d savrag          s                   like(cporag)
003210080306     d saviva          s                   like(v2civa) inz
003220080306     d codice          s                   like(v2civa) inz
003230100805     d $End            s               n
003240160905     d ClienteLog      s               n   inz
003241171213     d ClienteSofferenza...
003242171213     d                 s               n   inz
003250051107
003260051107      *   S C H I E R E
003270051212     d car             s              1    dim(10) ctdata perrcd(10)
003280170421     d CliImport       s              7  0 dim(999) inz
003290051122     d mod             s             15    dim(04) ctdata perrcd(1)
003300160914     d msg             s             79    dim(91) ctdata perrcd(1)
003310051124     d rigatf1         s              1    dim(78)
003320051122     d rigatf2         s              1    dim(62)
003330051122     d rigatf3         s              1    dim(62)
003340051122     d skpo            s              3    dim(30)
003350051122     d skpog           s              3    dim(250) inz(*Zeros)
003360170914     d skpog_RA        s              3    dim(250) inz(*Zeros)
003370051122     d skpogcpo        s              3    dim(250) inz(*Zeros)
003380060731     d tf04            s              1    dim(25) ctdata perrcd(25)
003390100524     d tf22            s              1    dim(25) ctdata perrcd(25)
003400100507     d tf05            s              1    dim(25) ctdata perrcd(25)
003410060203     d tf12            s              1    dim(25) ctdata perrcd(25)
003420051206     d tf14            s              1    dim(25) ctdata perrcd(25)
003430051124     d tf15            s              1    dim(25) ctdata perrcd(25)
003440060526     d tf19            s              1    dim(25) ctdata perrcd(25)
003450100406     d tf20            s              1    dim(25) ctdata perrcd(25)
003460101213     d tf17            s              1    dim(25) ctdata perrcd(25)
003470110223     d tf21            s              1    dim(25) ctdata perrcd(25)
003480051122     d £4              s              4    dim(20)
003490051124     d $tf             s              1    dim(25)
003500051107
003510051107      *   D S   I N T E R N E / E S T E R N E
003520051109
003530051109     d                 ds
003540051109     d aa1                     1      2  0
003550051109     d mm1                     3      4  0
003560051109     d gg1                     5      6  0
003570051109     d dataamg                 1      6  0
003580051109
003590051109     d                 ds
003600051109     d gg2                     1      2  0
003610051109     d mm2                     3      4  0
003620051109     d aa2                     5      6  0
003630051109     d datagma                 1      6  0
003640051129
003650051129     d parmbanca       ds
003660051129     d  pabi                   1      5  0
003670051129     d  pcab                   6     10  0
003680051129     d  pist                  11     50
003690051129     d  ppro                  51     52
003700051129     d  pflg                  53     53
003710051124
003720051124     d parmf11         ds
003730051124     d  paropz                 1      1
003740051124     d  parcli                 2      8
003750060216     d  partnta60              9      9
003760060216     d  parimta60             10     10
003770051124
003780051124     d parmf15         ds
003790051124     d  parcli1                       7  0
003800080116
003810080116     d parmiban        ds
003820080116     d  parmstato              1      2
003830080116     d  parmcheck              3      4
003840080206     d  parmcoori              5     34
003850051107
003860051107     d                 ds
003870051107     d v1cf01                  1      3
003880051107     d v1cf02                  4      6
003890051107     d v1cf03                  7      9
003900051107     d v1cf04                 10     12
003910051107     d v1cf05                 13     15
003920051107     d v1cf06                 16     18
003930051107     d v1cf07                 19     21
003940051107     d v1cf08                 22     24
003950051107     d v1cf09                 25     27
003960051107     d v1cf10                 28     30
003970051107     d v1cf11                 31     33
003980051107     d v1cf12                 34     36
003990051107     d v1cf13                 37     39
004000051107     d v1cf14                 40     42
004010051107     d v1cf15                 43     45
004020051107     d v1cf16                 46     48
004030051107     d v1cf17                 49     51
004040051107     d v1cf18                 52     54
004050051107     d v1cf19                 55     57
004060051107     d v1cf20                 58     60
004070051107     d v1cf21                 61     63
004080051107     d v1cf22                 64     66
004090051107     d v1cf23                 67     69
004100051108     d v1cf24                 70     72
004110051108     d skfil                   1     72    dim(24)
004120051108
004130051108     d                 ds
004140051108     d v1ivaeu                 1      2
004150051108     d v1ivait                 3     16
004160051108     d v1civa                  1     16
004170051109
004180051109     d                 ds
004190051109     d v2ivaeu                 1      2
004200051109     d v2ivait                 3     16
004210051109     d v2civa                  1     16
004220080115
004230080115     d v5ciban         ds
004240080115     d  v5ciban1               1      4
004250080115     d  v5ciban2               5      8
004260080115     d  v5ciban3               9     12
004270080115     d  v5ciban4              13     16
004280080115     d  v5ciban5              17     20
004290080115     d  v5ciban6              21     24
004300080115     d  v5ciban7              25     28
004310080208     d  v5ciban8              29     32
004320100728
004330100728     d v5ibandn        ds
004340100728     d  v5ibandn1              1      4
004350100728     d  v5ibandn2              5      8
004360100728     d  v5ibandn3              9     12
004370100728     d  v5ibandn4             13     16
004380100728     d  v5ibandn5             17     20
004390100728     d  v5ibandn6             21     24
004400100728     d  v5ibandn7             25     28
004410100728     d  v5ibandn8             29     32
004420100728
004430100728     d v5ibanna        ds
004440100728     d  v5ibanna1              1      4
004450100728     d  v5ibanna2              5      8
004460100728     d  v5ibanna3              9     12
004470100728     d  v5ibanna4             13     16
004480100728     d  v5ibanna5             17     20
004490100728     d  v5ibanna6             21     24
004500100728     d  v5ibanna7             25     28
004510100728     d  v5ibanna8             29     32
004520051116
004530051116     d wlbdat          ds
004540051116     d  g02dat                 1      8  0
004550051116     d  g02inv                 9     16  0
004560051116     d  g02err                17     17
004570051116     d  g02tgi                18     22  0
004580051107
004590051107     d                sds
004600051107     d  Vtcpgm                 1     10
004610051107
004620051107     d Azuteds       e ds                  Extname(Azute00f)
004630051107     d dDatiute      e ds
004640100729     d dPag          e ds
004650051117     d dged          e ds
004660051117     d dgei          e ds
004670051116     d dlat          e ds
004680170421     d dLPD          e ds
004690051116     d dmtc          e ds
004700051108     d ds_cnaco      e ds                  inz  extname(Cnaco00f) prefix(w_)
004710051108     d ds_cnind      e ds                  inz  extname(Cnind00f) prefix(w_)
004720051108     d ds_cnclp      e ds                  inz  extname(Cnclp00f) prefix(w_)
004730051108     d ds_fncls      e ds                  inz  extname(Fncls00f) prefix(w_)
004740060531     d cnaco_73      e ds                  inz  extname(Cnaco00f)
004750060531     d cnind_73      e ds                  inz  extname(Cnind00f)
004760060531     d cnclp_73      e ds                  inz  extname(Cnclp00f)
004770060531     d fncls_73      e ds                  inz  extname(Fncls00f)
004780121105
004790121105      // - CPORST file TNCPO
004800121105     d dCPO01        e ds                  inz
004810121105
004820110323     d dsbi          e ds
004830051108     d dsfa          e ds
004840051108     d dsff          e ds
004850051108     d dsic          e ds
004860051108     d ds1l          e ds
004870051108     d ds1t          e ds
004880051125     d ds13          e ds
004890051118     d ds15          e ds
004900051108     d ds2f          e ds
004910051207     d ds2g          e ds
004920051108     d ds2h          e ds
004930051108     d ds2u          e ds
004940051108     d ds4l          e ds
004950051108     d ds4u          e ds
004960051115     d ds4w          e ds
004970090803     d w_ds4w        e ds                  extname(ds4w) prefix(w_)
004980051125     d dtft          e ds
004990051108     d dute01        e ds
005000110217     d dY4O          e ds
005010051118     d Fnlv13ds      e ds
005020051118     d Fnlv14ds      e ds
005030070809     d FIOR37ds      e ds                  inz
005040070809     d  I37cgi       e                     inz(*all'9')
005050051220     d Fior50ds      e ds
005060051108     d kpjba         e ds
005070100406     d OG141         e ds
005080051125     d og143         e ds
005090051220     d og148         e ds
005100051107     d Tibs02ds      e ds
005110051122     d Tibs12ds      e ds
005120051107     d Tibs34ds      e ds
005130051108     d Tibs69ds      e ds
005140060531     d Tibs73ds      e ds
005150051118     d Tisi95ds      e ds
005160051207     d Tnta12ds      e ds
005170100127     d TNTA40DS      e ds
005180100604     d TNTA43DS      e ds
005190051124     d Tnta60ds      e ds
005200060731     d Tnta61ds      e ds
005210051213     d Tnta81ds      e ds
005220100406     d TRMK01DS      e ds
005230110223     d Trmk05ds      e ds
005240100406     d TRMK17DS      e ds
005250100406     d TRMK20ds      e ds
005260100507     d TRMK21DS      e ds
005270130808     d TRMK43ds      e ds                  inz
005280081020     d Trmk50ds      e ds
005290100407     d TRMK28DS      e ds
005300051108     d Trul31ds      e ds
005310051118     d Trul42ds      e ds
005320101213     d Trul57ds      e ds
005330100730     d TrulIbanI0    e ds                  qualified
005340100728     d TrulIbanI1    e ds                  qualified
005350100728     d TrulIbanO0    e ds                  qualified
005360100728     d TrulIbanO1    e ds                  qualified
005370090616     d trulsoli1     e ds                  qualified
005380090616     d trulsolo1     e ds                  qualified
005390060529     d TNTA73DS      e ds
005400061012     d Tisie5ds      e ds
005410051128     d Xdecds        e ds                  inz
005420051128     d   ocdesi      e                     inz(*Off)
005430151014     d xcfiva1ds     e ds
005440061106     d  ivaeu                  3      4
005450061106     d  ivait                  5     18
005460100805     d TNTBE00F      e ds                  extname(TNTBE00F)
005470160803
005480160803       // -?Storicizzazione variazioni
005490160803     d TRMK30DS      e ds
005500160803     d TNCPO_30      e ds                  extname(TNCPO00F) inz
005510160803     d TNCPO1_30     e ds                  extname(TNCPO10F) inz
005520160803     d TICPI_30      e ds                  extname(TICPI00F) inz
005530051107
005540051107      *   C O S T A N T I
005550060203      * titolo videata (lunghezza massima 35)
005560060203     d VtcTit          c                   const('** GESTIONE ANAGRAFICHE CLIEN-
005570060203     d                                     TI ** ')
005580091027     d VtcTitv         c                   const('****  Anagrafica Provvisoria -
005590091027     d                                      **** ')
005600051122     d VtcTitc         c                   const('**      CONVALIDA OFFERTE    -
005610060203     d                                        ** ')
005620051122     d cf2413          c                   const('F24=AlTasti(1/3)')
005630051122     d cf2423          c                   const('F24=AlTasti(2/3)')
005640051122     d cf2433          c                   const('F24=AlTasti(3/3)')
005650051122     d cf2412          c                   const('F24=AlTasti(1/2)')
005660051122     d cf2422          c                   const('F24=AlTasti(2/2)')
005670100127
005680100127     d Digits          c                   const('0123456789')
005690100406
005700100406      //---------------------------------------------------------------
005710100406      //?Definizione procedure esterne.
005720100406      //---------------------------------------------------------------
005730100604      // - Controllo se trattativa aperta
005740100604     d Tnta43R         pr                  extpgm('TNTA43R')
005750100604     d  tnta43ds                           likeds(tnta43ds)
005760110223
005770110223      // - Calcolo categoria potenziale
005780110223     d trmk05r         pr                  extpgm('TRMK05R')
005790110223     d  kpjba                              likeds(KPJBA)
005800110223     d  trmk05ds                           likeds(TRMK05ds)
005810100604
005820100406      // - Inserimento attività
005830100406     d trmk17r         pr                  extpgm('TRMK17R')
005840100406     d  kpjba                              likeds(KPJBA)
005850100406     d  trmk17ds                           likeds(TRMK17ds)
005860100507
005870100507      // - Interrogazione attività
005880100507     d trmk21r         pr                  extpgm('TRMK21R')
005890100507     d  kpjba                              likeds(KPJBA)
005900100407
005910100407      // - Modifica potenziale
005920100407     d trmk28r         pr                  extpgm('TRMK28R')
005930100407     d  trmk28ds                           likeds(TRMK28DS)
005940120510
005950120510      // - Pgm gestione INFO commerciali
005960120510     d trmk50r         pr                  extpgm('TRMK50R')
005970120510     d  kpjba                              likeds(KPJBA)
005980120510     d  trmk50ds                           likeds(TRMK50ds)
005990100728
006000100728      // - Coordinate bancarie
006010100728     d TrulIbanR       pr                  extpgm('TRULIBANR')
006020100729     d  rqsOpCode                    10a   const
006030100729     d  rpyEsito                     10i 0
006040100729     d  rqsFormatName                10a   const
006050100729     d  rqsData                     256a   options(*varsize)
006060100729     d  rqsDataSize                  10i 0 const
006070100729     d  rpyFormatName                10a   const
006080100729     d  rpyData                     256a   options(*varsize)
006090100729     d  rpyDataSize                  10i 0 const
006100100729
006110100729      // - Interrogazione tabella
006120100729     d Trul19R         pr                  extpgm('TRUL19R')
006130100729     d  kcod                          2a
006140100729     d  ordina                        1a
006150100729     d  kkey                          8a
006160100729     d  comando                       1a
006170101213
006180101213      // - Dati Fatturazione
006190101213     d trul57r         pr                  extpgm('TRUL57R')
006200101213     d  kpjba                              likeds(KPJBA)
006210101213     d  trul57ds                           likeds(TRUL57ds)
006220160803
006230160803       // -?Storicizzazione Variazioni Potenziale
006240160803     d TRMK30R         pr                  extpgm('TRMK30R')
006250160803     d  trmk30ds                           likeds(trmk30ds)
006260160803     d  tncpo_30                           likeds(tncpo_30)
006270160803     d  tncpo1_30                          likeds(tncpo1_30)
006280160803     d  ticpi_30                           likeds(ticpi_30)
006290100406
006300100406      //---------------------------------------------------------------
006310100406      //?prototipi
006320100406      //---------------------------------------------------------------
006330100729      /copy gaitrasrc/srcprotopr,tibs02r
006340150427      /copy gaitrasrc/srcprotopr,tibs73r
006350161129      /copy gaitrasrc/srcprotopr,tibs69r
006360100406      /copy gaitrasrc/srcprotopr,trmk01r
006370100406      /copy gaitrasrc/srcprotopr,trmk02r
006380100406      /copy gaitrasrc/srcprotopr,trmk20r
006390140722      /copy gaitrasrc/srcProtoPR,TNTA12R
006400051107
006410051107      *------------------------------------------------------------------------*
006420090417
006430090417     c/exec sql
006440090417     c+ set option dynusrprf = *owner, closqlcsr = *endmod
006450090417     c/end-exec
006460051107
006470051117      * Richiamato da Gestione visite/offerte o da Comferma offerte
006480051117      * salto la videata di richiesta del codice
006490091104      * anche nel caso di richiamo per sola interrogazione (nuovo campo DS)
006500091124      * oppure manutenzione di un codice specifico (cliente-CNACO)
006510091124     c                   If        (*In06 or *In07
006520091124     c                              or $interroga or $modifica)
006530091124     c                              and not *in28
006540051118     c                   Goto      vid02
006550051115     c                   EndIf
006560091124
006570091124     c   28              goto      fine
006580051124
006590051124     * §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ * ?
006600051124     ?*  DATI DI RICERCA    ?
006610051124     ?*  Prima videata      ?
006620051124
006630060110     c                   ExSr      Sr_Pulvid
006640051118
006650051118     c     emi01         Tag
006660051107
006670051107     c                   Write     Ta60t01
006680051107     c                   Exfmt     Ta60d01
006690051107     c                   Eval      *In28 = *Off
006700051107     c                   Eval      *In90 = *Off
006710051107     c                   Clear                   v1cmsg
006720051219     c                   Eval      $d01 = *On
006730051107
006740051124     ?* F3=Fine  ?
006750051107     c                   If        *InKc or
006760051107      * 08=utente non abilitato
006770051107     c                             *In08
006780051107     c                   Goto      Fine
006790051107     c                   EndIf
006800051107
006810051124     ?* Controllo  ?
006820051107     c                   ExSr      Sr_Ctrd01
006830051118     c   28              Goto      emi01
006840051124
006850051130     c     vid02         Tag
006860060221     c                   Eval      wokpot = *Off
006870051130
006880051130      * Carico i dati nelle varie videate
006890051216     c                   ExSr      Sr_Cardati
006900091112     c   70
006910091112     corn06              ExSr      Sr_Carluoghi
006920051216     c                   ExSr      Sr_Carnote
006930051130
006940051130     * §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ * ?
006950051130     ?*  DATI ANAGRAFICI/BLOCCO   ?
006960051130     ?*  Seconda videata      ?
006970051107
006980051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
006990051221     c                   Eval      *In29 = *On
007000051207     c                   ExSr      Sr_Tastifun
007010051207
007020051118     c     emi02         Tag
007030051129
007040051107     c                   Write     Ta60t01
007050051122     c                   Write     Ta60z01
007060051107     c                   Exfmt     Ta60d02
007070051107     c                   Eval      *In28 = *Off
007080051124     c                   Eval      *In90 = *Off
007090051108     c                   Clear                   v2cmsg
007100051213     c                   Eval      wvideo = '02'
007110051107
007120051124     ?* F3=Fine  ?
007130051107     c                   If        *InKc
007140060217     c                   ExSr      Sr_F03
007150060217     c   80              Goto      emi02
007160051107     c                   EndIf
007170051114
007180051124     ?* F12=Ritorno  ?
007190051118     c                   If        *Inkl
007200060110     c                   ExSr      Sr_Pulvid
007210091106      * se richiamato in interrogazione deve uscire dal pgm
007220091124     c                   if        $interroga or $modifica
007230091106     c                   ExSr      Sr_F03
007240091106     c                   else
007250100208      * se non è interrogazione e non è anagrafica provvisoria
007260100208      * con F12 devo sbloccare i file
007270100208     c                   if        not *in05 and not *in06
007280100208     c                   unlock    cnaco00f
007290100208     c                   unlock    cnind00f
007300100208     c                   unlock    cnclp00f
007310100208     c                   unlock    fncls01l
007320100208     c                   endif
007330051118     c                   Goto      emi01
007340091106     c                   endif
007350051118     c                   EndIf
007360090717
007370090717     ?* F2=Rubrica  ?
007380090717     c                   If        *Inkb
007390090717     c                   Eval      wrag = v2crag
007400090717     c  n05              Eval      wrich = 'M'
007410090717     c   05              Eval      wrich = 'V'
007420090717     c                   clear                   wtnt
007430090717     c                   ExSr      Sr_F02
007440090717     c                   Goto      emi02
007450090717     c                   EndIf
007460060731
007470060731     ?* F4=Abilitazioni  ?
007480060731     c                   if        *inkd
007490060731     c                   exsr      sr_f04
007500060731     c                   goto      emi02
007510060731     c                   endif
007520100507
007530100507       //?F5=Attività
007540100507     c                   IF        *inke
007550100507     c                   exsr      sr_F05
007560100507     c                   goto      emi02
007570100507     c                   ENDIF
007580051128
007590051128     ?* F6=Conferma  ?
007600051128     c                   If        *InKf
007610051128     c                   ExSr      Sr_Conferma
007611180119     c                   IF        ClienteSofferenza
007612180119     c                   goto      emi02
007613180119     c                   ENDIF
007620051213     c   28              Goto      emi02
007630051222     c                   If        Not *In06 and Not *In07
007640091125     c                             and not $modifica
007650060110     c                   ExSr      Sr_Pulvid
007660051222     c                   Goto      emi01
007670051222     c                   Else
007680051222     c                   Goto      fine
007690051222     c                   EndIf
007700051128     c                   EndIf
007710051122
007720051124     ?* F7=Cliente Unificante  ?
007730051122     c                   If        *Inkg
007740051122     c                   ExSr      Sr_F07
007750051122     c                   Goto      emi02
007760051122     c                   EndIf
007770051206
007780100407     ?* F20=Int.Cli.Potenziali  ?
007790100407     c                   If        *Inku
007800051206      * Imposto i primi 5 caratteri della ragione sociale
007810051206     c                   Eval      w005a = v2crag
007820100407     c                   ExSr      Sr_F20
007830051206     c                   Goto      emi02
007840051206     c                   EndIf
007850051122
007860051124     ?* F11=Luoghi  ?
007870051122     c                   If        *Inkk
007880051122     c                   ExSr      Sr_F11
007890051122     c                   Goto      emi02
007900051122     c                   EndIf
007910051206
007920051206     ?* F14=Info Commerciali  ?
007930051207     c                   If        *Inkn
007940060208     c                   If        v2ccpo = *Zeros
007950060208     c                   Eval      *In28 = *On
007960060208     c                   Eval      v2cmsg = msg(49)
007970060208     c                   Else
007980051207     c                   Eval      wrag = v2crag
007990051206     c                   ExSr      Sr_F14
008000060208     c                   EndIf
008010051206     c                   Goto      emi02
008020051206     c                   EndIf
008030051122
008040051124     ?* F15=Codici Collegati  ?
008050051122     c                   If        *Inkp
008060051124     c                   If        clpscf = *Zeros
008070051124     c                   Eval      *In28 = *On
008080051128     c                   Eval      v2cmsg = msg(33)
008090051125     c                   Eval      v2cmsg = %trim(v2cmsg) + ' ' +
008100051125     c                             'non è possibile interrogare'
008110051124     c                   Else
008120051124     c                   Eval      w0070 = clpscf
008130051122     c                   ExSr      Sr_F15
008140051124     c                   EndIf
008150051122     c                   Goto      emi02
008160051122     c                   EndIf
008170051122
008180051124     ?* F18=Note  ?
008190051122     c                   If        *Inks
008200101111     c                   If        v2ccpo = *Zeros
008210101111     c                   Eval      *In28 = *On
008220101111     c                   Eval      h2riga = 14
008230101111     c                   Eval      h2colo = 28
008240101111     c                   Eval      v2cmsg = msg(12)
008250101111     c                   Else
008260051124     c                   Eval      wrag = v2crag
008270051122     c                   ExSr      Sr_F18
008280101111     c                   EndIf
008290051122     c                   Goto      emi02
008300051122     c                   EndIf
008310090717
008320060526     ?* F19=Variazioni  ?
008330060526     c                   If        *Inkt
008340060526     c                   ExSr      Sr_F19
008350060526     c                   Goto      emi02
008360060526     c                   EndIf
008370100406
008380100524       //?F22=Richiesta Contatto
008390100524     c                   IF        *inkw
008400101111     c                   If        v2ccpo = *Zeros
008410101111     c                   Eval      *In28 = *On
008420101111     c                   Eval      h2riga = 14
008430101111     c                   Eval      h2colo = 28
008440101111     c                   Eval      v2cmsg = msg(12)
008450101111     c                   Else
008460100524     c                   exsr      sr_F22
008470101111     c                   EndIf
008480100406     c                   goto      emi02
008490100406     c                   ENDIF
008500101213
008510101213       //?F17=Dati Fatturazione
008520101213     c                   IF        *inkr
008530101213     c                   exsr      sr_F17
008540101213     c                   goto      emi02
008550101213     c                   ENDIF
008560110223
008570110223       //?F21=Blocco clienti
008580110223     c                   IF        *inkv
008590110223     c                   If        v2ccpo = *Zeros
008600110223     c                   Eval      *In28 = *On
008610110223     c                   Eval      h2riga = 14
008620110223     c                   Eval      h2colo = 28
008630110223     c                   Eval      v2cmsg = msg(12)
008640110223     c                   else
008650110223     c                   exsr      sr_F21
008660110310      /free
008670110310       //?Se ho pianificato l'attività devo richiamare la gestione
008680110310       //?delle info commerciali
008690110310         IF  OMK17f12 = *blanks and OMK17err = *blanks;
008700110310           exsr sr_f14;
008710110310         ENDIF;
008720110310      /end-free
008730110223     c                   Endif
008740110223     c                   goto      emi02
008750110223     c                   ENDIF
008760051122
008770051124     ?* F24=Altri tasti  ?
008780051124     c                   If        *Inky and $loop > 1
008790051124     c                   ExSr      Sr_F24
008800051124     c                   Goto      emi02
008810051124     c                   EndIf
008820051107
008830051124     ?* Controllo  ?
008840051117     c  n05              ExSr      Sr_Ctrd02
008850051124     c   28
008860051124     cor 90              Goto      emi02
008861171213     c                   IF        ClienteSofferenza
008862171213     c                   Goto      emi02
008863171213     c                   ENDIF
008870051130
008880051130     * §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ * ?
008890051130     ?*  DATI COMMERCIALI   ?
008900051130     ?*  Terza videata      ?
008910051130
008920051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
008930051221     c                   Eval      *In29 = *Off
008940051207     c                   ExSr      Sr_Tastifun
008950051219      * Ragione sociale x le videate dalla terza in poi
008960051219     c                   Eval      v3crag = v2crag
008970080313      * Se immissione imposto la categoria merceologica da potenziale
008980060110     c                   If        *In01 and savitc <> *Blanks and
008990060110     c                             v3citc = *Blanks
009000060110     c                   Eval      v3citc = savitc
009010060110     c                   EndIf
009020051207
009030051130     c     emi03         Tag
009040051130
009050051130     c                   Write     Ta60t01
009060051130     c                   Write     Ta60z01
009070051130     c                   Exfmt     Ta60d03
009080051130     c                   Eval      *In28 = *Off
009090051130     c                   Eval      *In90 = *Off
009100051130     c                   Clear                   v3cmsg
009110051213     c                   Eval      wvideo = '03'
009120051130
009130051130     ?* F3=Fine  ?
009140051130     c                   If        *InKc
009150060217     c                   ExSr      Sr_F03
009160060217     c   80              Goto      emi03
009170051130     c                   EndIf
009180051130
009190051130     ?* F12=Ritorno  ?
009200051207     c                   If        *Inkl
009210091111      * se richiamato in interrogazione deve uscire dal pgm
009220091124     c                   if        $interroga or $modifica
009230091111     c                   ExSr      Sr_F03
009240091111     c                   else
009250051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
009260051221     c                   Eval      *In29 = *On
009270051207     c                   ExSr      Sr_Tastifun
009280051207     c                   Goto      emi02
009290091111     c                   endif
009300051207     c                   EndIf
009310090717
009320090717     ?* F2=Rubrica  ?
009330090717     c                   If        *Inkb
009340090717     c                   Eval      wrag = v2crag
009350090717     c  n05              Eval      wrich = 'M'
009360090717     c   05              Eval      wrich = 'V'
009370090717     c                   clear                   wtnt
009380090717     c                   ExSr      Sr_F02
009390100607     c                   Goto      emi03
009400090717     c                   EndIf
009410060731
009420060731     ?* F4=Abilitazioni  ?
009430060731     c                   if        *inkd
009440060731     c                   exsr      sr_f04
009450060731     c                   goto      emi03
009460060731     c                   endif
009470100507
009480100507       //?F5=Attività
009490100507     c                   IF        *inke
009500100507     c                   exsr      sr_F05
009510100507     c                   goto      emi03
009520100507     c                   ENDIF
009530051130
009540051130     ?* F6=Conferma  ?
009550051130     c                   If        *InKf
009560051130     c                   ExSr      Sr_Conferma
009570051213     c   28              Goto      emi03
009580051222     c                   If        Not *In06 and Not *In07
009590091125     c                             and not $modifica
009600060110     c                   ExSr      Sr_Pulvid
009610051130     c                   Goto      emi01
009620051222     c                   Else
009630051222     c                   Goto      fine
009640051222     c                   EndIf
009650051130     c                   EndIf
009660051130
009670051130     ?* F7=Cliente Unificante  ?
009680051130     c                   If        *Inkg
009690051130     c                   ExSr      Sr_F07
009700051130     c                   Goto      emi03
009710051130     c                   EndIf
009720051130
009730051130     ?* F11=Luoghi  ?
009740051130     c                   If        *Inkk
009750051130     c                   ExSr      Sr_F11
009760051130     c                   Goto      emi03
009770051130     c                   EndIf
009780051206
009790051206     ?* F14=Info Commerciali  ?
009800051207     c                   If        *Inkn
009810060208     c                   If        v2ccpo = *Zeros
009820060208     c                   Eval      *In28 = *On
009830060208     c                   Eval      v3cmsg = msg(49)
009840060208     c                   Else
009850051207     c                   Eval      wrag = v3crag
009860051206     c                   ExSr      Sr_F14
009870060208     c                   EndIf
009880051206     c                   Goto      emi03
009890051206     c                   EndIf
009900051130
009910051130     ?* F15=Codici Collegati  ?
009920051130     c                   If        *Inkp
009930051130     c                   If        clpscf = *Zeros
009940051130     c                   Eval      *In28 = *On
009950051130     c                   Eval      v3cmsg = msg(33)
009960051130     c                   Eval      v3cmsg = %trim(v3cmsg) + ' ' +
009970051130     c                             'non è possibile interrogare'
009980051130     c                   Else
009990051130     c                   Eval      w0070 = clpscf
010000051130     c                   ExSr      Sr_F15
010010051130     c                   EndIf
010020051130     c                   Goto      emi03
010030051130     c                   EndIf
010040051130
010050051130     ?* F18=Note  ?
010060051130     c                   If        *Inks
010070051130     c                   Eval      wrag = v3crag
010080051130     c                   ExSr      Sr_F18
010090051130     c                   Goto      emi03
010100051130     c                   EndIf
010110060529
010120060529     ?* F19=Variazioni  ?
010130060529     c                   If        *Inkt
010140060529     c                   ExSr      Sr_F19
010150060529     c                   Goto      emi03
010160060529     c                   EndIf
010170100406
010180100524       //?F22=Richiesta Contatto
010190100524     c                   IF        *inkw
010200100524     c                   exsr      sr_F22
010210100406     c                   goto      emi03
010220100406     c                   ENDIF
010230101213
010240101213       //?F17=Dati Fatturazione
010250101213     c                   IF        *inkr
010260101213     c                   exsr      sr_F17
010270101213     c                   goto      emi03
010280101213     c                   ENDIF
010290110223
010300110223       //?F21=Blocco clienti
010310110223     c                   IF        *inkv
010320110223     c                   exsr      sr_F21
010330110310      /free
010340110310       //?Se ho pianificato l'attività devo richiamare la gestione
010350110310       //?delle info commerciali
010360110310         IF  OMK17f12 = *blanks and OMK17err = *blanks;
010370110310           exsr sr_f14;
010380110310         ENDIF;
010390110310      /end-free
010400110223     c                   goto      emi03
010410110223     c                   ENDIF
010420051130
010430051130     ?* F24=Altri tasti  ?
010440051130     c                   If        *Inky and $loop > 1
010450051130     c                   ExSr      Sr_F24
010460051130     c                   Goto      emi03
010470051130     c                   EndIf
010480051130
010490051130     ?* Controllo  ?
010500051130     c  n05              ExSr      Sr_Ctrd03
010510051219     c   28
010520051219     cor 90              Goto      emi03
010530051219      * se interrogazione controllo i luoghi e le note
010540051219     c                   If        *In05
010550140722     c                   eval      wRGR = 'RN'
010560140722     c                   exsr      sr_CtrNote
010570140722     c   28
010580140722     cor 90              goto      Emi03
010590140722     c                   eval      wRGR = 'RT'
010600140722     c                   exsr      sr_CtrNote
010610140722     c   28
010620140722     cor 90              goto      Emi03
010630140722     c                   eval      wRGR = 'RL'
010640140722     c                   exsr      sr_CtrNote
010650140722     c   28
010660140722     cor 90              goto      Emi03
010670051219     c                   EndIf
010680051108
010690051129
010700051124     * §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ * ?
010710100728     ?*  DATI FATTURAZIONE + PAGAMENTO FATTURA  ?
010720051130     ?*  Quarta videata      ?
010730051118
010740051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
010750051207     c                   ExSr      Sr_Tastifun
010760051219      * Se cliente non nuovo imposto la data prima spedizione fattura
010770051219      * uguale a oggi meno 1 anno
010780121119     c                   If        v3cnew = 'S' and *In01
010790051219     c                   Z-add     dataoggi1     dataamg
010800051219     c                   Z-add     aa1           aa2
010810051219     c                   Z-add     mm1           mm2
010820051219     c                   Z-add     gg1           gg2
010830051219     c                   Z-add     datagma       v4cdps
010840051219     c                   EndIf
010850051207
010860051130     c     emi04         Tag
010870100128
010880100128      * se sono da convalida offerta e non c'è il codice fatturazione
010890100129      * impostato imposto "?" nel campo del codice fatturazione
010900100129      * così all'enter si apre la window con i clienti trovati
010910100129     c                   IF        (v4cscf = *zeros or v4cscf = *blanks) and
010920100129     c                              *in07
010930100129     c                   eval      v4cscf = '000000?'
010940100128     c                   ENDIF
010950051129
010960051108     c                   Write     Ta60t01
010970051124     c                   Write     Ta60z01
010980051130     c                   Exfmt     Ta60d04
010990051108     c                   Eval      *In28 = *Off
011000051124     c                   Eval      *In90 = *Off
011010051130     c                   Clear                   v4cmsg
011020051213     c                   Eval      wvideo = '04'
011030051108
011040051124     ?* F3=Fine  ?
011050051108     c                   If        *InKc
011060060217     c                   ExSr      Sr_F03
011070060217     c   80              Goto      emi04
011080051108     c                   EndIf
011090051114
011100051124     ?* F12=Ritorno  ?
011110051207     c                   If        *InKl
011120091111      * se richiamato in interrogazione deve uscire dal pgm
011130091124     c                   if        $interroga or $modifica
011140091111     c                   ExSr      Sr_F03
011150091111     c                   else
011160051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
011170051221     c                   Eval      *In29 = *Off
011180051207     c                   ExSr      Sr_Tastifun
011190051207     c                   Goto      emi03
011200091111     c                   EndIf
011210051207     c                   EndIf
011220090717
011230090717     ?* F2=Rubrica  ?
011240090717     c                   If        *Inkb
011250090717     c                   Eval      wrag = v2crag
011260090717     c  n05              Eval      wrich = 'M'
011270090717     c   05              Eval      wrich = 'V'
011280090717     c                   clear                   wtnt
011290090717     c                   ExSr      Sr_F02
011300100607     c                   Goto      emi04
011310090717     c                   EndIf
011320060731
011330060731     ?* F4=Abilitazioni  ?
011340060731     c                   if        *inkd
011350060731     c                   exsr      sr_f04
011360060731     c                   goto      emi04
011370060731     c                   endif
011380100507
011390100507       //?F5=Attività
011400100507     c                   IF        *inke
011410100507     c                   exsr      sr_F05
011420100507     c                   goto      emi04
011430100507     c                   ENDIF
011440051128
011450051128     ?* F6=Conferma  ?
011460051128     c                   If        *InKf
011470051128     c                   ExSr      Sr_Conferma
011480051213     c   28              Goto      emi04
011490051222     c                   If        Not *In06 and Not *In07
011500091125     c                             and not $modifica
011510060110     c                   ExSr      Sr_Pulvid
011520051222     c                   Goto      emi01
011530051222     c                   Else
011540051222     c                   Goto      fine
011550051222     c                   EndIf
011560051128     c                   EndIf
011570051124
011580051124     ?* F7=Cliente Unificante  ?
011590051124     c                   If        *Inkg
011600051124     c                   ExSr      Sr_F07
011610051206     c                   Goto      emi04
011620051124     c                   EndIf
011630051124
011640051124     ?* F11=Luoghi  ?
011650051124     c                   If        *Inkk
011660051124     c                   ExSr      Sr_F11
011670051206     c                   Goto      emi04
011680051124     c                   EndIf
011690051206
011700051206     ?* F14=Info Commerciali  ?
011710051207     c                   If        *Inkn
011720060208     c                   If        v2ccpo = *Zeros
011730060208     c                   Eval      *In28 = *On
011740060208     c                   Eval      v4cmsg = msg(49)
011750060208     c                   Else
011760051207     c                   Eval      wrag = v3crag
011770051206     c                   ExSr      Sr_F14
011780060208     c                   EndIf
011790051206     c                   Goto      emi04
011800051206     c                   EndIf
011810051124
011820051124     ?* F15=Codici Collegati  ?
011830051124     c                   If        *Inkp
011840100127     c                   If        v4cscf = *Zeros or v4cscf = *blanks
011850100409     c                             or v4cscf = '000000?'
011860051124     c                   Eval      *In28 = *On
011870051130     c                   Eval      v4cmsg = msg(33)
011880051130     c                   Eval      v4cmsg = %trim(v4cmsg) + ' ' +
011890051125     c                             'non è possibile interrogare'
011900051124     c                   Else
011910100127     c                   Eval      w0070 = %dec(v4cscf:7:0)
011920051124     c                   ExSr      Sr_F15
011930051124     c                   EndIf
011940051206     c                   Goto      emi04
011950051124     c                   EndIf
011960051124
011970051124     ?* F18=Note  ?
011980051124     c                   If        *Inks
011990051130     c                   Eval      wrag = v3crag
012000051124     c                   ExSr      Sr_F18
012010051206     c                   Goto      emi04
012020051124     c                   EndIf
012030060529
012040060529     ?* F19=Variazioni  ?
012050060529     c                   If        *Inkt
012060060529     c                   ExSr      Sr_F19
012070060529     c                   Goto      emi04
012080060529     c                   EndIf
012090100406
012100100524       //?F22=Richiesta Contatto
012110100524     c                   IF        *inkw
012120100524     c                   exsr      sr_F22
012130100406     c                   goto      emi04
012140100406     c                   ENDIF
012150101213
012160101213       //?F17=Dati Fatturazione
012170101213     c                   IF        *inkr
012180101213     c                   exsr      sr_F17
012190101213     c                   goto      emi04
012200101213     c                   ENDIF
012210110223
012220110223       //?F21=Blocco clienti
012230110223     c                   IF        *inkv
012240110223     c                   exsr      sr_F21
012250110310      /free
012260110310       //?Se ho pianificato l'attività devo richiamare la gestione
012270110310       //?delle info commerciali
012280110310         IF  OMK17f12 = *blanks and OMK17err = *blanks;
012290110310           exsr sr_f14;
012300110310         ENDIF;
012310110310      /end-free
012320110223     c                   goto      emi04
012330110223     c                   ENDIF
012340051124
012350051124     ?* F24=Altri tasti  ?
012360051124     c                   If        *Inky and $loop > 1
012370051124     c                   ExSr      Sr_F24
012380051206     c                   Goto      emi04
012390051124     c                   EndIf
012400051108
012410051124     ?* Controllo  ?
012420051213     c  n05              ExSr      Sr_Ctrd04
012430051124     c   28
012440051130     cor 90              Goto      emi04
012450051219      * se interrogazione controllo i luoghi e le note
012460051219     c                   If        *In05
012470091112     c   70
012480091112     corn06              ExSr      Sr_Ctrl001
012490051219     c   28              Goto      emi04
012500051219     c                   ExSr      Sr_Ctrnt84
012510051219     c   28              Goto      emi04
012520051219     c                   EndIf
012530051107
012540051124     * §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ * ?
012550100728     ?*  PAGAMENTI VARI   ?
012560051130     ?*  Quinta videata      ?
012570051118
012580051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
012590051207     c                   ExSr      Sr_Tastifun
012600090612      * Blocco pagamenti
012610130322      * se Blocco pagamento NO e da stato del credito devo bloccare
012620130322     c                   If        wbloc = '0' and §4Wpbl = 'S'
012630130322      * se non sono in interrogazione
012640121002     c                             and not *in05 and
012650130322      * ed è stato variato lo stato del credito
012660121002     c                             V2Ccon <> %subst(CLPcon:2:2)
012670130322      * imposto il blocco dello stato del credito
012680130322     c                   Eval      wbloc = %subst(§4Wpjbkamm:1:1)
012690130322     c                   eval      V5Cblpedp = wbloc
012700060317     c                   EndIf
012710130322      * Imposto il blocco a video
012720130322     c                   If        wbloc = '0'
012730130322     c                   Eval      v5cblp = 'NO'
012740130322     c                   Else
012750100728     c                   Eval      v5cblp = 'SI'
012760060317     c                   EndIf
012770051207
012780051130     c     emi05         Tag
012790051129
012800051108     c                   Write     Ta60t01
012810051129     c                   Write     Ta60z01
012820051130     c                   Exfmt     Ta60d05
012830051108     c                   Eval      *In28 = *Off
012840051129     c                   Eval      *In90 = *Off
012850051130     c                   Clear                   v5cmsg
012860051213     c                   Eval      wvideo = '05'
012870051108
012880051129     ?* F3=Fine  ?
012890051108     c                   If        *InKc
012900060217     c                   ExSr      Sr_F03
012910060217     c   80              Goto      emi05
012920051108     c                   EndIf
012930051114
012940051129     ?* F12=Ritorno  ?
012950051207     c                   If        *InKl
012960091111      * se richiamato in interrogazione deve uscire dal pgm
012970091124     c                   if        $interroga or $modifica
012980091111     c                   ExSr      Sr_F03
012990091111     c                   else
013000051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
013010051207     c                   ExSr      Sr_Tastifun
013020051207     c                   Goto      emi04
013030091111     c                   EndIf
013040051207     c                   EndIf
013050090717
013060090717     ?* F2=Rubrica  ?
013070090717     c                   If        *Inkb
013080090717     c                   Eval      wrag = v2crag
013090090717     c  n05              Eval      wrich = 'M'
013100090717     c   05              Eval      wrich = 'V'
013110090717     c                   clear                   wtnt
013120090717     c                   ExSr      Sr_F02
013130100607     c                   Goto      emi05
013140090717     c                   EndIf
013150060731
013160060731     ?* F4=Abilitazioni  ?
013170060731     c                   if        *inkd
013180060731     c                   exsr      sr_f04
013190060731     c                   goto      emi05
013200060731     c                   endif
013210100507
013220100507       //?F5=Attività
013230100507     c                   IF        *inke
013240100507     c                   exsr      sr_F05
013250100507     c                   goto      emi05
013260100507     c                   ENDIF
013270051129
013280051129     ?* F6=Conferma  ?
013290051129     c                   If        *InKf
013300051129     c                   ExSr      Sr_Conferma
013310051213     c   28              Goto      emi05
013320051222     c                   If        Not *In06 and Not *In07
013330091125     c                             and not $modifica
013340060110     c                   ExSr      Sr_Pulvid
013350051222     c                   Goto      emi01
013360051222     c                   Else
013370051222     c                   Goto      fine
013380051222     c                   EndIf
013390051129     c                   EndIf
013400051129
013410051129     ?* F7=Cliente Unificante  ?
013420051129     c                   If        *Inkg
013430051129     c                   ExSr      Sr_F07
013440051130     c                   Goto      emi05
013450051129     c                   EndIf
013460051129
013470051129     ?* F11=Luoghi  ?
013480051129     c                   If        *Inkk
013490051129     c                   ExSr      Sr_F11
013500051130     c                   Goto      emi05
013510051129     c                   EndIf
013520051206
013530051206     ?* F14=Info Commerciali  ?
013540051207     c                   If        *Inkn
013550060208     c                   If        v2ccpo = *Zeros
013560060208     c                   Eval      *In28 = *On
013570060208     c                   Eval      v5cmsg = msg(49)
013580060208     c                   Else
013590051207     c                   Eval      wrag = v3crag
013600051206     c                   ExSr      Sr_F14
013610060208     c                   EndIf
013620051206     c                   Goto      emi05
013630051206     c                   EndIf
013640051129
013650051129     ?* F15=Codici Collegati  ?
013660051129     c                   If        *Inkp
013670100127     c                   If        v4cscf = *Zeros or v4cscf = *blanks
013680051129     c                   Eval      *In28 = *On
013690051130     c                   Eval      v5cmsg = msg(33)
013700051130     c                   Eval      v5cmsg = %trim(v5cmsg) + ' ' +
013710051129     c                             'non è possibile interrogare'
013720051129     c                   Else
013730100127     c                   Eval      w0070 = %dec(v4cscf:7:0)
013740051129     c                   ExSr      Sr_F15
013750051129     c                   EndIf
013760051130     c                   Goto      emi05
013770051129     c                   EndIf
013780051129
013790051129     ?* F18=Note  ?
013800051129     c                   If        *Inks
013810051130     c                   Eval      wrag = v3crag
013820051129     c                   ExSr      Sr_F18
013830051130     c                   Goto      emi05
013840051129     c                   EndIf
013850060529
013860060529     ?* F19=Variazioni  ?
013870060529     c                   If        *Inkt
013880060529     c                   ExSr      Sr_F19
013890060529     c                   Goto      emi05
013900060529     c                   EndIf
013910100406
013920100524       //?F22=Richiesta Contatto
013930100524     c                   IF        *inkw
013940100524     c                   exsr      sr_F22
013950100406     c                   goto      emi05
013960100406     c                   ENDIF
013970101213
013980101213       //?F17=Dati Fatturazione
013990101213     c                   IF        *inkr
014000101213     c                   exsr      sr_F17
014010101213     c                   goto      emi05
014020101213     c                   ENDIF
014030110223
014040110223       //?F21=Blocco clienti
014050110223     c                   IF        *inkv
014060110223     c                   exsr      sr_F21
014070110310      /free
014080110310       //?Se ho pianificato l'attività devo richiamare la gestione
014090110310       //?delle info commerciali
014100110310         IF  OMK17f12 = *blanks and OMK17err = *blanks;
014110110310           exsr sr_f14;
014120110310         ENDIF;
014130110310      /end-free
014140110223     c                   goto      emi04
014150110223     c                   ENDIF
014160051129
014170051129     ?* F24=Altri tasti  ?
014180051129     c                   If        *Inky and $loop > 1
014190051129     c                   ExSr      Sr_F24
014200051130     c                   Goto      emi05
014210051129     c                   EndIf
014220051129
014230051129     ?* Controllo  ?
014240051213     c  n05              ExSr      Sr_Ctrd05
014250051129     c   28
014260051130     cor 90              Goto      emi05
014270051219      * se interrogazione controllo i luoghi e le note
014280051219     c                   If        *In05
014290091112     c   70
014300091112     corn06              ExSr      Sr_Ctrl555
014310051219     c   28              Goto      emi05
014320091112     c   70
014330091112     corn06              ExSr      Sr_Ctrl666
014340051219     c   28              Goto      emi05
014350051219     c                   ExSr      Sr_Ctrnt85
014360051219     c   28              Goto      emi05
014370090616     c                   ExSr      Sr_Ctrnt89
014380090616     c   28              Goto      emi05
014390150424     c                   ExSr      Sr_Ctrnt82
014400100730     c   90
014410100730     cor 28              Goto      emi05
014420051219     c                   EndIf
014430051129
014440051129     * §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ * ?
014450051129     ?*  GESTIONE GIACENZE   ?
014460051130     ?*  Sesta videata      ?
014470051118
014480051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
014490051207     c                   ExSr      Sr_Tastifun
014500051207
014510120118     c     emi06         Tag
014520120118
014530120118      /free
014540120118       //?Se sono in convalida offerta e il campo tipo comunicazione al
014550120118       //?mittente non è impostato lo recupero dalla tabella '2G'
014560120118       //?valore di default
014570120118         IF  V6Ctcm = *blanks and *in07;
014580120118           V6Ctcm = §2Gtcm;
014590120118         ENDIF;
014600120118      /end-free
014610051206
014620051108     c                   Write     Ta60t01
014630051206     c                   Write     Ta60z01
014640051130     c                   Exfmt     Ta60d06
014650051108     c                   Eval      *In28 = *Off
014660051213     c                   Eval      *In90 = *Off
014670051130     c                   Clear                   v6cmsg
014680051213     c                   Eval      wvideo = '06'
014690051108
014700051206     ?* F3=Fine  ?
014710051108     c                   If        *InKc
014720060217     c                   ExSr      Sr_F03
014730060217     c   80              Goto      emi06
014740051108     c                   EndIf
014750051114
014760051206     ?* F12=Ritorno  ?
014770051207     c                   If        *InKl
014780091111      * se richiamato in interrogazione deve uscire dal pgm
014790091124     c                   if        $interroga or $modifica
014800091111     c                   ExSr      Sr_F03
014810091111     c                   else
014820051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
014830051207     c                   ExSr      Sr_Tastifun
014840051207     c                   Goto      emi05
014850091111     c                   EndIf
014860051207     c                   EndIf
014870090717
014880090717     ?* F2=Rubrica  ?
014890090717     c                   If        *Inkb
014900090717     c                   Eval      wrag = v2crag
014910090717     c  n05              Eval      wrich = 'M'
014920090717     c   05              Eval      wrich = 'V'
014930090717     c                   clear                   wtnt
014940090717     c                   ExSr      Sr_F02
014950100607     c                   Goto      emi06
014960090717     c                   EndIf
014970060731
014980060731     ?* F4=Abilitazioni  ?
014990060731     c                   if        *inkd
015000060731     c                   exsr      sr_f04
015010060731     c                   goto      emi06
015020060731     c                   endif
015030100507
015040100507       //?F5=Attività
015050100507     c                   IF        *inke
015060100507     c                   exsr      sr_F05
015070100507     c                   goto      emi06
015080100507     c                   ENDIF
015090051206
015100051206     ?* F6=Conferma  ?
015110051206     c                   If        *InKf
015120051206     c                   ExSr      Sr_Conferma
015130051213     c   28              Goto      emi06
015140051222     c                   If        Not *In06 and Not *In07
015150091125     c                             and not $modifica
015160060110     c                   ExSr      Sr_Pulvid
015170051222     c                   Goto      emi01
015180051222     c                   Else
015190051222     c                   Goto      fine
015200051222     c                   EndIf
015210051206     c                   EndIf
015220051206
015230051206     ?* F7=Cliente Unificante  ?
015240051206     c                   If        *Inkg
015250051206     c                   ExSr      Sr_F07
015260051206     c                   Goto      emi06
015270051206     c                   EndIf
015280051206
015290051206     ?* F11=Luoghi  ?
015300051206     c                   If        *Inkk
015310051206     c                   ExSr      Sr_F11
015320051206     c                   Goto      emi06
015330051206     c                   EndIf
015340051206
015350051206     ?* F14=Info Commerciali  ?
015360051207     c                   If        *Inkn
015370060208     c                   If        v2ccpo = *Zeros
015380060208     c                   Eval      *In28 = *On
015390060208     c                   Eval      v6cmsg = msg(49)
015400060208     c                   Else
015410051207     c                   Eval      wrag = v3crag
015420051206     c                   ExSr      Sr_F14
015430060208     c                   EndIf
015440051206     c                   Goto      emi06
015450051206     c                   EndIf
015460051206
015470051206     ?* F15=Codici Collegati  ?
015480051206     c                   If        *Inkp
015490100127     c                   If        v4cscf = *Zeros or v4cscf = *blanks
015500051206     c                   Eval      *In28 = *On
015510051206     c                   Eval      v6cmsg = msg(33)
015520051206     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
015530051206     c                             'non è possibile interrogare'
015540051206     c                   Else
015550100127     c                   Eval      w0070 = %dec(v4cscf:7:0)
015560051206     c                   ExSr      Sr_F15
015570051206     c                   EndIf
015580051206     c                   Goto      emi06
015590051206     c                   EndIf
015600051206
015610051206     ?* F18=Note  ?
015620051206     c                   If        *Inks
015630051206     c                   Eval      wrag = v3crag
015640051206     c                   ExSr      Sr_F18
015650051206     c                   Goto      emi06
015660051206     c                   EndIf
015670060529
015680060529     ?* F19=Variazioni  ?
015690060529     c                   If        *Inkt
015700060529     c                   ExSr      Sr_F19
015710060529     c                   Goto      emi06
015720060529     c                   EndIf
015730100406
015740100524       //?F22=Richiesta Contatto
015750100524     c                   IF        *inkw
015760100524     c                   exsr      sr_F22
015770100406     c                   goto      emi06
015780100406     c                   ENDIF
015790101213
015800101213       //?F17=Dati Fatturazione
015810101213     c                   IF        *inkr
015820101213     c                   exsr      sr_F17
015830101213     c                   goto      emi06
015840101213     c                   ENDIF
015850110223
015860110223       //?F21=Blocco clienti
015870110223     c                   IF        *inkv
015880110223     c                   exsr      sr_F21
015890110310      /free
015900110310       //?Se ho pianificato l'attività devo richiamare la gestione
015910110310       //?delle info commerciali
015920110310         IF  OMK17f12 = *blanks and OMK17err = *blanks;
015930110310           exsr sr_f14;
015940110310         ENDIF;
015950110310      /end-free
015960110223     c                   goto      emi06
015970110223     c                   ENDIF
015980051206
015990051206     ?* F24=Altri tasti  ?
016000051206     c                   If        *Inky and $loop > 1
016010051206     c                   ExSr      Sr_F24
016020051206     c                   Goto      emi06
016030051206     c                   EndIf
016040051206
016050051206     ?* Controllo  ?
016060051213     c  n05              ExSr      Sr_Ctrd06
016070051206     c   28
016080051206     cor 90              Goto      emi06
016090051219      * se interrogazione controllo i luoghi e le note
016100051219     c                   If        *In05
016110091112     c   70
016120091112     corn06              ExSr      Sr_Ctrl005
016130051219     c   28              Goto      emi06
016140051219     c                   ExSr      Sr_Ctrnt88
016150051219     c   28              Goto      emi06
016160091112     c   70
016170091112     corn06              ExSr      Sr_Ctrl300
016180060215     c   28              Goto      emi06
016190051219     c                   EndIf
016200051108
016210051130     * §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ * ?
016220051130     ?*  GESTIONE DANNI/TASSAZIONE PORTI ASSEGNATI/STOP AUTOTRASPORTATORI   ?
016230051130     ?*  Settima videata      ?
016240051213
016250051213      * Imposto il campo unico in fondo alla videata con i tasti funzione
016260051213     c                   ExSr      Sr_Tastifun
016270051118
016280051130     c     emi07         Tag
016290051108
016300051108     c                   Write     Ta60t01
016310051213     c                   Write     Ta60z01
016320051130     c                   Exfmt     Ta60d07
016330051108     c                   Eval      *In28 = *Off
016340051213     c                   Eval      *In90 = *Off
016350051130     c                   Clear                   v7cmsg
016360051213     c                   Eval      wvideo = '07'
016370051108
016380051213     ?* F3=Fine  ?
016390051108     c                   If        *InKc
016400060217     c                   ExSr      Sr_F03
016410060217     c   80              Goto      emi07
016420051108     c                   EndIf
016430051114
016440051213     ?* F12=Ritorno  ?
016450051213     c                   If        *InKl
016460091111      * se richiamato in interrogazione deve uscire dal pgm
016470091124     c                   if        $interroga or $modifica
016480091111     c                   ExSr      Sr_F03
016490091111     c                   else
016500051213      * Imposto il campo unico in fondo alla videata con i tasti funzione
016510051213     c                   ExSr      Sr_Tastifun
016520051213     c                   Goto      emi06
016530091111     c                   EndIf
016540051213     c                   EndIf
016550090717
016560090717     ?* F2=Rubrica  ?
016570090717     c                   If        *Inkb
016580090717     c                   Eval      wrag = v2crag
016590090717     c  n05              Eval      wrich = 'M'
016600090717     c   05              Eval      wrich = 'V'
016610090717     c                   clear                   wtnt
016620090717     c                   ExSr      Sr_F02
016630100607     c                   Goto      emi07
016640090717     c                   EndIf
016650060731
016660060731     ?* F4=Abilitazioni  ?
016670060731     c                   if        *inkd
016680060731     c                   exsr      sr_f04
016690060731     c                   goto      emi07
016700060731     c                   endif
016710100507
016720100507       //?F5=Attività
016730100507     c                   IF        *inke
016740100507     c                   exsr      sr_F05
016750100507     c                   goto      emi07
016760100507     c                   ENDIF
016770051213
016780051213     ?* F6=Conferma  ?
016790051213     c                   If        *InKf
016800051213     c                   ExSr      Sr_Conferma
016810051213     c   28              Goto      emi07
016820051222     c                   If        Not *In06 and Not *In07
016830091125     c                             and not $modifica
016840060110     c                   ExSr      Sr_Pulvid
016850051222     c                   Goto      emi01
016860051222     c                   Else
016870051222     c                   Goto      fine
016880051222     c                   EndIf
016890051213     c                   EndIf
016900051213
016910051213     ?* F7=Cliente Unificante  ?
016920051213     c                   If        *Inkg
016930051213     c                   ExSr      Sr_F07
016940051213     c                   Goto      emi07
016950051213     c                   EndIf
016960051213
016970051213     ?* F11=Luoghi  ?
016980051213     c                   If        *Inkk
016990051213     c                   ExSr      Sr_F11
017000051213     c                   Goto      emi07
017010051213     c                   EndIf
017020051213
017030051213     ?* F14=Info Commerciali  ?
017040051213     c                   If        *Inkn
017050060208     c                   If        v2ccpo = *Zeros
017060060208     c                   Eval      *In28 = *On
017070060208     c                   Eval      v7cmsg = msg(49)
017080060208     c                   Else
017090051213     c                   Eval      wrag = v3crag
017100051213     c                   ExSr      Sr_F14
017110060208     c                   EndIf
017120051213     c                   Goto      emi07
017130051213     c                   EndIf
017140051213
017150051213     ?* F15=Codici Collegati  ?
017160051213     c                   If        *Inkp
017170100127     c                   If        v4cscf = *Zeros or v4cscf = *blanks
017180051213     c                   Eval      *In28 = *On
017190051213     c                   Eval      v7cmsg = msg(33)
017200051213     c                   Eval      v7cmsg = %trim(v7cmsg) + ' ' +
017210051213     c                             'non è possibile interrogare'
017220051213     c                   Else
017230100127     c                   Eval      w0070 = %dec(v4cscf:7:0)
017240051213     c                   ExSr      Sr_F15
017250051213     c                   EndIf
017260051213     c                   Goto      emi07
017270051213     c                   EndIf
017280051213
017290051213     ?* F18=Note  ?
017300051213     c                   If        *Inks
017310051213     c                   Eval      wrag = v3crag
017320051213     c                   ExSr      Sr_F18
017330051213     c                   Goto      emi07
017340051213     c                   EndIf
017350060529
017360060529     ?* F19=Variazioni  ?
017370060529     c                   If        *Inkt
017380060529     c                   ExSr      Sr_F19
017390060529     c                   Goto      emi07
017400060529     c                   EndIf
017410100406
017420100524       //?F22=Richiesta Contatto
017430100524     c                   IF        *inkw
017440100524     c                   exsr      sr_F22
017450100406     c                   goto      emi07
017460100406     c                   ENDIF
017470101213
017480101213       //?F17=Dati Fatturazione
017490101213     c                   IF        *inkr
017500101213     c                   exsr      sr_F17
017510101213     c                   goto      emi07
017520101213     c                   ENDIF
017530110223
017540110223       //?F21=Blocco clienti
017550110223     c                   IF        *inkv
017560110223     c                   exsr      sr_F21
017570110310      /free
017580110310       //?Se ho pianificato l'attività devo richiamare la gestione
017590110310       //?delle info commerciali
017600110310         IF  OMK17f12 = *blanks and OMK17err = *blanks;
017610110310           exsr sr_f14;
017620110310         ENDIF;
017630110310      /end-free
017640110223     c                   goto      emi07
017650110223     c                   ENDIF
017660051213
017670051213     ?* F24=Altri tasti  ?
017680051213     c                   If        *Inky and $loop > 1
017690051213     c                   ExSr      Sr_F24
017700051213     c                   Goto      emi07
017710051213     c                   EndIf
017720051213
017730051213     ?* Controllo  ?
017740051213     c  n05              ExSr      Sr_Ctrd07
017750051219     c   28              Goto      emi07
017760051219      * se interrogazione controllo i luoghi e le note
017770051219     c                   If        *In05
017780091112     c   70
017790091112     corn06              ExSr      Sr_Ctrl006
017800051219     c   28              Goto      emi07
017810051219     c                   ExSr      Sr_Ctrnt87
017820051219     c   28              Goto      emi07
017830091112     c   70
017840091112     corn06              ExSr      Sr_Ctrl008
017850051219     c   28              Goto      emi07
017860091112     c   70
017870091112     corn06              ExSr      Sr_Ctrl002
017880060215     c   28              Goto      emi07
017890060215     c                   ExSr      Sr_Ctrnt83
017900060215     c   28              Goto      emi07
017910091112     c   70
017920091112     corn06              ExSr      Sr_Ctrl200
017930060215     c   28              Goto      emi07
017940060215     c                   ExSr      Sr_Ctrnt86
017950060215     c   28              Goto      emi07
017960051219     c                   EndIf
017970051213
017980051220     c  n05              Goto      emi07
017990051220
018000051220      * Se sono in interrogazione devo far vedere l'anagrafica clienti ritiro
018010051220      * visto che non è previsto il tasto F6 di conferma
018020051220      * Se però non è attiva la procedura ORM ritorno alla videata
018030160905      * se sto gestendo/interrogando cliente LOGISTICA no anagrafica clienti
018040160905      * ritiro
018050051220     c                   Movel     acoksc        w0030
018060051220     c                   Clear                   og148
018070051220     c     w0030         Chain     Azorg01l
018080051220     c                   If        %Found(Azorg01l)
018090051220     c                   Eval      og148 = orgde8
018100051220     c                   EndIf
018110120313     c                   If        §ogorm = *blanks
018120051220     c                   Goto      emi07
018130051220     c                   EndIf
018140160905     c                   IF        ClienteLog
018150160905     c                   goto      Fine
018160160905     c                   ENDIF
018170160905      /end-free
018180051220      * Richiamo l'interrogazione anagrafica clienti ritiro
018190070809     c                   reset                   FIOR37ds
018200070809     c                   eval      I37opz  = '5'
018210070809     c                   movel     ACOksc        I37fgs
018220070809     c                   movel     ACOksc        I37cro
018230070809     c                   movel(p)  FIOR37ds      KPJBU
018240070809     c                   call      'FIOR37R'
018250070809     c                   parm                    KPJBA
018260091111     c                   eval      fior37ds = kpjbu
018270051220
018280091124      * Se non richiamato neanche per interrogazione/modifica diretta
018290051220     c                   If        Not *In06 and Not *In07
018300091124     c                             and not $interroga
018310091124     c                             and not $modifica
018320060110     c                   ExSr      Sr_Pulvid
018330051220     c                   Goto      emi01
018340051220     c                   EndIf
018350051108
018360051107     c     Fine          Tag
018370060215
018380060215      * Se premuto F03 ritorno il dato al pgm chiamante
018390091113     c                   if        $pgmrich and
018400091113     c                             (*Inkc or w03cok = 'SI')
018410060215     c                   Eval      ta60f03 = '1'
018420060215     c                   EndIf
018430051221
018440051221      * Se richiamato da gestione visite passo i dati alla DS
018450051221     c                   If        *In06
018460051221     c                   Eval      ta60cpo = acolib
018470060607     c                   Eval      ta60cmm = clpage
018480051221     c                   EndIf
018490051108
018500051108     c                   If        wtibs69 = *On
018510051108     c                   Eval      i69tla = 'C'
018520051108     c                   Call      'TIBS69R'
018530051108     c                   Parm                    Tibs69ds
018540051108     c                   EndIf
018550060531     c                   If        wtibs73 = *On
018560060531     c                   Eval      ibs73tla = 'C'
018570060531     c                   Call      'TIBS73R'
018580060531     c                   Parm                    Tibs73ds
018590060531     c                   EndIf
018600081020     c                   If        wtrmk50 = *On
018610081020     c                   Eval      i50tla=    'C'
018620081020     c                   Call      'TRMK50R'
018630081020     c                   Parm                    kpjba
018640081020     c                   Parm                    TRMK50ds
018650081020     c                   EndIf
018660100127
018670100127     c                   IF        wtnta40
018680100127     c                   eval      ITA40tla = 'C'
018690100127     c                   call      'TNTA40R'
018700100127     c                   parm                    kpjba
018710100127     c                   parm                    TNTA40DS
018720100127     c                   ENDIF
018730160803
018740160803      /free
018750160803       //?Chiudo file storicizzazione variazioni potenziale
018760160803         clear TRMK30DS;
018770160803         clear tncpo_30;
018780160803         clear tncpo1_30;
018790160803         clear ticpi_30;
018800160803         IMK30tla = 'C';
018810160803         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
018820160803      /end-free
018830051107
018840051107     c                   Eval      *InLr = *On
018850051108
018860051108      *------------------------------------------------------------------------*
018870060110      * PULIZIA DELLE VIDEATE
018880051108      *------------------------------------------------------------------------*
018890060110     c     Sr_Pulvid     BegSr
018900060110
018910060110     c                   Clear                   h1riga
018920060110     c                   Clear                   h1colo
018930060110     c                   Clear                   h2riga
018940060110     c                   Clear                   h2colo
018950060110     c                   Clear                   h3riga
018960060110     c                   Clear                   h3colo
018970060110     c                   Clear                   h4riga
018980060110     c                   Clear                   h4colo
018990060110     c                   Clear                   h5riga
019000060110     c                   Clear                   h5colo
019010060110     c                   Clear                   h6riga
019020060110     c                   Clear                   h6colo
019030060110     c                   Clear                   h7riga
019040060110     c                   Clear                   h7colo
019041171005
019042171006       IF  savin05 = *off;
019043171006         *in05 = *off;
019044171006         *in04 = *off;
019045171006         clear VTCmod;
019046171006       ENDIF;
019050051108
019060060110      * Prima videata
019070051108     c                   Eval      v1ckcc = dutkci
019080051108     c                   Clear                   v1cksc
019090051108     c                   Clear                   v1crag
019100051108     c                   Clear                   skfil
019110051108     c                   Clear                   v1ccdf
019120051108     c                   Clear                   v1civa
019130051108
019140051108      * Imposto la sk dei p.o. di ricerca
019150051108      * se autorizzazzione diversa da distretto e se sono in filiale
019160051115     c                   If        wabi <> 'DI' and Not *In09
019170051108     c                   Clear                   yy
019180051108     c                   Do        24            xx
019190170914     c*//                If        skpog(xx) <> *Blanks and skpog(xx) > *Zeros
019200170914     c                   If        skPog_RA(xx) <> *Blanks and
019210170914     c                             skPog_RA(xx)  > *Zeros
019220051108     c                   Add       1             yy
019230170914     c*//                Move      skpog(xx)     skfil(yy)
019240170914     c                   Move      skPog_RA(xx)  skFil(yy)
019250051108     c                   EndIf
019260051108     c                   EndDo
019270051108     c                   EndIf
019280060110
019290060110      * Seconda videata
019300060110     c                   Clear                   v2cduv
019310060110     c                   Clear                   v2ddtr
019320060110     c                   Clear                   v2cdtr
019330060110     c                   Clear                   v2copz
019340060110     c                   Clear                   v2ctic
019350060110     c                   Clear                   v2clin
019360060110     c                   Clear                   v2crag
019370060110     c                   Clear                   v2cvia
019380060110     c                   Clear                   v2ccit
019390060110     c                   Clear                   v2ccae
019400060110     c                   Clear                   v2cprv
019410060110     c                   Clear                   v2csta
019420060110     c                   Clear                   v2ctel
019430060110     c                   Clear                   v2ctlf
019440060110     c                   Clear                   v2ccdf
019450060110     c                   Clear                   v2civa
019460060110     c                   Clear                   v2cabl
019470060110     c                   Clear                   v2cblc
019480060110     c                   Clear                   v2ccon
019490060110     c                   Clear                   v2tb02
019500060110     c                   Clear                   v2ccpo
019510090616     c                   Clear                   v2csol
019520090616     c                   Clear                   v2fsol
019530091119     c                   clear                   v2ntcdc
019540091119     c                   clear                   savntcdc
019541171213     c                   eval      ClienteSofferenza = *off
019550060110      * Terza videata
019560060110     c                   Clear                   v3cage
019570060110     c                   Clear                   v3cclv
019580060110     c                   Clear                   v3citc
019590121119     c                   clear                   v3cnew
019600060110      * Quarta videata
019610060110     c                   Clear                   v4cscf
019620060110     c                   Clear                   savscf
019630060110     c                   Clear                   v4cuni
019640060110     c                   Clear                   savuni
019650060110     c                   Clear                   v4ctft
019660060110     c                   Clear                   savtft
019670060110     c                   Clear                   v4cfft
019680060110     c                   Clear                   savfft
019690060110     c                   Clear                   v4ctdf
019700060110     c                   Clear                   v4csin
019710060110     c                   Clear                   v4cdps
019720060110     c                   Clear                   v4cdus
019730060110     c                   Clear                   v4cnpn
019740060110     c                   Clear                   v4cnpr
019750060110     c                   Clear                   v4cdpr
019760100727     c                   Clear                   v4ccdp
019770100727     c                   Clear                   v4cbna
019780100727     c                   Clear                   v4cabi
019790100727     c                   Clear                   v4ccab
019800121105     c                   clear                   V4Ctpf
019810060110      * Quinta videata
019820060110     c                   Clear                   wbloc
019830100728     c                   Clear                   v5cblp
019840060110     c                   Clear                   v5cfpc
019850080115     c                   clear                   v5ciban
019860060110     c                   Clear                   v5cbab
019870100729     c                   clear                   v5tpdn
019880100728     c                   clear                   v5ibandn
019890100728     c                   clear                   v5babdn
019900140203     c                   clear                   v5bicdn
019910100729     c                   clear                   v5tpna
019920100728     c                   clear                   v5ibanna
019930100728     c                   clear                   v5babna
019940140203     c                   clear                   v5bicna
019950150424     c                   eval      BonificoCA = *off
019960150424     c                   eval      BonificoDN = *off
019970150424     c                   eval      BonificoNA = *off
019980150427     c                   eval      PagEsteroCA = *off
019990150427     c                   eval      PagEsteroDN = *off
020000150427     c                   eval      PagEsteroNA = *off
020010150427     c                   clear                   CodPagCA
020020150427     c                   clear                   CodPagDN
020030150427     c                   clear                   CodPagNA
020040060110      * Sesta videata
020050060110     c                   Clear                   v6ctcm
020060060110     c                   Clear                   v6ctcf
020070060110     c                   Clear                   v6capg
020080060110     c                   Clear                   v6cdmg
020090060801     c                   Clear                   v6ccag
020100060110      * Settima videata
020110060110     c                   Clear                   v7csdn
020120060110     c                   Clear                   v7csin
020130060110     c                   Clear                   v7ctpa
020140060110     c                   Clear                   v7cstp
020150060317
020160060317      * Pulisco la ds dello stato del credito per non sporcarmi i dati
020170060317      * del blocco tra un cliente e l'altro
020180060317     c                   Clear                   ds4w
020190100805
020200100805     c                   clear                   wbic
020210100805     c                   clear                   wcodpn
020220100805     c                   clear                   wcodpo
020230100805     c                   clear                   wiban
020240100805     c                   clear                   wpgm
020250060110
020260051108     c                   EndSr
020270051107
020280051107      *------------------------------------------------------------------------*
020290051107      * CONTROLLO PRIMA VIDEATA - DATI DI RICERCA
020300051107      *------------------------------------------------------------------------*
020310051107     c     Sr_Ctrd01     BegSr
020320051117
020330051117      * Pulisco i campi del posizionamento cursore
020340051117     c                   Clear                   h1riga
020350051117     c                   Clear                   h1colo
020360051117      * Spengo indicatori di posizionamento cursore
020370051117     c                   Setoff                                       414243
020380051117     c                   Setoff                                       444546
020390051117     c                   Setoff                                       474849
020400051117     c                   Setoff                                       505152
020410051117     c                   Setoff                                       535455
020420051117     c                   Setoff                                       565758
020430051117     c                   Setoff                                       596061
020440051117     c                   Setoff                                       626364
020450060110      * Spengo indicatori di comodo
020460090914     c                   Setoff                                       0102
020470060110     c                   Setoff                                       04
020480060711
020490060711     c                   eval      wforzaksc = *off
020500051107
020510051107      * Almeno un dato deve essere inserito
020520051107     c                   If        v1ckcc = *Zeros and v1cksc = *Zeros and
020530051108     c                             v1crag = *Blanks and
020540051107     c                             v1ccdf = *Blanks and v1civa = *Blanks
020550051111     c                   Eval      *In28 = *On
020560060109     c                   Eval      h1riga = 06
020570051118     c                   Eval      h1colo = 04
020580051111     c                   Eval      v1cmsg = msg(02)
020590051107     c                   Leavesr
020600051107     c                   EndIf
020610051107
020620051107      * CAPOCONTO
020630051107     c                   If        v1ckcc = *zeros
020640051107     c                   Eval      *In28 = *On
020650060109     c                   Eval      h1riga = 06
020660051118     c                   Eval      h1colo = 04
020670051107     c                   Eval      v1cmsg = msg(03)
020680051107     c                   Leavesr
020690051107     c                   EndIf
020700051107
020710051219      * Se sono in filiale posso solo gestire i capoconti presenti in £4
020720051219      * per ora c'è solo il capoconto clienti (151)
020730051219if  1c                   If        Not *In09
020740051107     c                   Move      v1ckcc        w004a
020750051107     c     w004a         Lookup    £4                                     30
020760051107     c                   If        Not *In30
020770051107     c                   Eval      *In28 = *On
020780060109     c                   Eval      h1riga = 06
020790051118     c                   Eval      h1colo = 04
020800051107     c                   Eval      v1cmsg = msg(04)
020810051107     c                   Leavesr
020820051107     c                   EndIf
020830051219    1c                   EndIf
020840051219
020850051219      * CODICE CLIENTE NON IMMESSO
020860051219if  1c                   If        v1cksc = *Zeros
020870060207      * se non c'è una ricerca per ragione sociale errore
020880060227     c                   If        v1crag = *Blanks and
020890060227     c                             v1ccdf = *Blanks and v1civa = *Blanks
020900060207     c                   Eval      *In28 = *On
020910060207     c                   Eval      h1riga = 06
020920060207     c                   Eval      h1colo = 12
020930060207     c                   Eval      v1cmsg = msg(02)
020940060207     c                   Leavesr
020950060207     c                   EndIf
020960051219
020970051219      * Controllo la sk dei p.o. di ricerca
020980051219     c                   ExSr      Sr_Ctrfil
020990051219
021000051219s   2c                   Select
021010051219      * Ricerca Alfabetica
021020051219     c                   When      v1crag <> *Blanks
021030051219     c                   Movel     rsut          pardut
021040051219     c                   Movea     skfil         parfil
021050051219     c                   Movel     v1ckcc        parkcc
021060051219     c                   Clear                   pardit
021070051219     c                   Z-add     1             paxnum
021080051219      * 2 = richiesta di visualizzazione del cliente unificante
021090051219     c                   Z-add     2             kkut
021100051219     c                   Call      'XALFA3BR'
021110051219     c                   Parm                    pardut
021120051219     c                   Parm                    kkut
021130051219     c                   Parm                    v1crag
021140051219     c                   Parm                    parkcc
021150051219     c                   Parm                    parsta
021160051219     c                   Parm                    parfil
021170051219     c                   Parm                    pardit
021180051219     c                   Parm                    paxnum
021190051219     c                   Parm                    paxkcm
021200051219     c                   Parm                    paxksm
021210051219     c                   Parm                    paxkdm
021220051219     c                   If        parsta = -1
021230051219     c                   Goto      emi01
021240051219     c                   EndIf
021250051219     c                   Movel     paxksm        v1cksc
021260051219     c                   If        parkcc = *Zeros
021270051219     c                   Movel     paxkcm        v1ckcc
021280051219     c                   EndIf
021290051219      * Ricerca per Codice Fiscale
021300051219     c                   When      v1ccdf <> *Blanks
021310051219     c                   Movel     rsut          pardut
021320051219     c                   Movea     skfil         parfil
021330051219     c                   Movel     v1ckcc        parkcc
021340051219     c                   Clear                   pardit
021350051219     c                   Z-add     1             paxnum
021360051219      * 2 = richiesta di visualizzazione del cliente unificante
021370051219     c                   Z-add     2             kkut
021380051219     c                   Clear                   pariva
021390051219     c                   Movel     v1ccdf        parcdf
021400051219     c                   Call      'XALFA3BR'
021410051219     c                   Parm                    pardut
021420051219     c                   Parm                    kkut
021430051219     c                   Parm                    v1crag
021440051219     c                   Parm                    parkcc
021450051219     c                   Parm                    parsta
021460051219     c                   Parm                    parfil
021470051219     c                   Parm                    pardit
021480051219     c                   Parm                    paxnum
021490051219     c                   Parm                    paxkcm
021500051219     c                   Parm                    paxksm
021510051219     c                   Parm                    paxkdm
021520051219     c                   Parm                    paresci
021530051219     c                   Parm                    parerr
021540051219     c                   Parm                    pariva
021550051219     c                   Parm                    parcdf
021560051219     c                   If        parsta = -1
021570051219     c                   Goto      emi01
021580051219     c                   EndIf
021590051219     c                   Movel     paxksm        v1cksc
021600051219     c                   Clear                   v1ccdf
021610051219     c                   If        parkcc = *Zeros
021620051219     c                   Movel     paxkcm        v1ckcc
021630051219     c                   EndIf
021640051219      * Ricerca per Partita IVA
021650051219     c                   When      v1civa <> *Blanks
021660051219     c                   Movel     rsut          pardut
021670051219     c                   Movea     skfil         parfil
021680051219     c                   Movel     v1ckcc        parkcc
021690051219     c                   Clear                   pardit
021700051219     c                   Z-add     1             paxnum
021710051219      * 2 = richiesta di visualizzazione del cliente unificante
021720051219     c                   Z-add     2             kkut
021730051219      * Se non ho impostato il codice nazione vado in ricerca con la sola
021740060109      * partita Iva altrimenti P.IVa + Nazione
021750051219     c                   If        v1ivaeu = *Blanks
021760051219     c                   Movel     v1ivait       pariva
021770051219     c                   Else
021780051219     c                   Movel     v1civa        pariva
021790051219     c                   EndIf
021800051219     c                   Clear                   parcdf
021810051219     c                   Call      'XALFA3BR'
021820051219     c                   Parm                    pardut
021830051219     c                   Parm                    kkut
021840051219     c                   Parm                    v1crag
021850051219     c                   Parm                    parkcc
021860051219     c                   Parm                    parsta
021870051219     c                   Parm                    parfil
021880051219     c                   Parm                    pardit
021890051219     c                   Parm                    paxnum
021900051219     c                   Parm                    paxkcm
021910051219     c                   Parm                    paxksm
021920051219     c                   Parm                    paxkdm
021930051219     c                   Parm                    paresci
021940051219     c                   Parm                    parerr
021950051219     c                   Parm                    pariva
021960051219     c                   Parm                    parcdf
021970051219     c                   If        parsta = -1
021980051219     c                   Goto      emi01
021990051219     c                   EndIf
022000051219     c                   Movel     paxksm        v1cksc
022010051219     c                   Clear                   v1civa
022020051219     c                   If        parkcc = *Zeros
022030051219     c                   Movel     paxkcm        v1ckcc
022040051219     c                   EndIf
022050051219    2c                   EndSl
022060051219    1c                   EndIf
022070051107
022080051107      * CODICE CLIENTE IMMESSO
022090051108if  1c                   If        v1cksc <> *Zeros
022100060711      * per prima cosa cerco il commerciale unificante del
022110060711      * cliente immesso
022120060711      * aggancio cnlcp effettivo xchè sono in prima videata quindi sono
022130060711      * per forza nell'anagrafica effettiva e non nella provvisoria
022140060711     c                   Eval      kksc = v1cksc
022150060711     c     kcnaco        chain(n)  cnclp00f
022160060711    2c                   if        %found(cnclp00f)
022170130806     c     CLPage        chain     AZCMM000
022180130806    3c                   if        Not  %found(AZCMM01L)
022190130806     c                   clear                   CMMuni
022200060711    3c                   endif
022210130806     c                   movel     CMMuni        wpoageuni
022220060711    2c                   endif
022230160905      * Cerco network del cliente
022240160905     c                   Clear                   og143
022250160905     c                   Movel     v1cksc        w0030
022260160905     c     w0030         Chain     Azorg01l
022270160905     c                   If        %Found(Azorg01l)
022280160905     c                   Eval      og143 = orgde3
022290160905     c                   EndIf
022300160905     c                   eval      ClienteLog = (§OGntw = 'LOG')
022310051117      * Controllo se cliente gestito dall'utente
022320051219      * se non sono in interrogazione
022330051219if  2c                   If        Not *In05
022340161005     c                   IF        §UTEksclog <> 'S' and
022350161005     c                             ClienteLog
022360161005     c                   Eval      *In28 = *On
022370161005     c                   Eval      h1riga = 06
022380161005     c                   Eval      h1colo = 12
022390161005     c                   Eval      v1cmsg = msg(10)
022400161005     c                   Leavesr
022410161005     c                   ENDIF
022420051117     c                   Movel     v1cksc        w003a
022430051117     c     w003a         Lookup    skpog                                  30
022440060711     c                   If        Not *In30
022450060711    4c                   if        wpoageuni <> *zeros
022460060711     c     wpoageuni     lookup    skpog                                  30
022470060711    5c                   if        not *In30
022480060711     c                   eval      *In28 = *On
022490060711     c                   eval      h1riga = 06
022500060711     c                   eval      h1colo = 12
022510060711     c                   eval      v1cmsg = msg(10)
022520060711     c                   leavesr
022530060711    5c                   endif
022540060711     c                   eval      wforzaksc = *on
022550060711    4c                   endif
022560060711    4c                   if        wforzaksc = *off
022570051117     c                   Eval      *In28 = *On
022580060109     c                   Eval      h1riga = 06
022590051117     c                   Eval      h1colo = 12
022600051117     c                   Eval      v1cmsg = msg(10)
022610051117     c                   Leavesr
022620060711    4c                   endif
022630051117     c                   EndIf
022640051219   x2c                   Else
022650051219      * Se sono in interrogazione e non sono utente di sede non posso
022660051219      * interrogare i clienti logistica o XXX
022670170907      *?=> Interrogare i clienti Logistica è ora possibile?
022680170907      *   ?(come lo era già per i clienti di altre filiali NON?
022690170907      *   ?logistica e NON di competenza dell'utente)?
022700051219if  3c                   If        Not *In09
022710051219     c                   Move      v1ckcc        w004a
022720051219     c     w004a         Lookup    £4                                     30
022730051219if  4c                   If        *In30
022740170907     c*//*               If        §ogntw = 'LOG' or §ogntw = 'XXX'
022750170907     c                   If        §OGntw = 'XXX'
022760051219     c                   Eval      *In28 = *On
022770060109     c                   Eval      h1riga = 06
022780051219     c                   Eval      h1colo = 12
022790051219     c                   Eval      v1cmsg = msg(10)
022800051219     c                   Leavesr
022810051219     c                   EndIf
022820051219    4c                   EndIf
022830051219    3c                   EndIf
022840051219    2c                   EndIf
022850051219      * Aggancio l'anagrafica
022860051219      * solo cnaco xchè se siamo in prima videata è x forza anagrafica
022870051219      * effettiva, la provvisoria è solo se richiamato
022880051222     c                   Eval      kksc = v1cksc
022890060111     c  n05kCnaco        Chain(e)  Cnaco00f
022900060111     c   05kCnaco        Chain(n)  Cnaco00f
022910051219      * Allocato errore
022920060220if  2c                   If        %Error and Not *In05
022930051219     c                   Eval      *In28 = *on
022940060109     c                   Eval      h1riga = 06
022950051219     c                   Eval      h1colo = 12
022960051219     c                   Eval      v1cmsg = msg(68)
022970051219     c                   Leavesr
022980051219    2c                   EndIf
022990051108if  2c                   If        %Found(Cnaco00f)
023000051213      * Cliente esistente - Manutenzione/Visualizzazione
023010051115     c                   If        acoflg <> '*'
023020051115     c  n05              Eval      *In02 = *On
023030051115     c  n05              Eval      vtcmod = mod(02)
023040051115     c   05              Eval      vtcmod = mod(03)
023050051107      * Cliente annullato
023060051107     c                   Else
023070051107     c                   Eval      *In04 = *On
023080051213     c                   Eval      *In05 = *On
023090051108     c                   Eval      vtcmod = mod(04)
023100051107     c                   EndIf
023110051107      * Cliente nuovo - immissione
023120051108   x2c                   Else
023130051116      * --> Se gestione clienti inserisco
023140051115if  3c                   If        Not *In05
023150051107     c                   Eval      *In01 = *On
023160051108     c                   Eval      vtcmod = mod(01)
023170051116      * --> Se interrogazione clienti errore
023180051115   x3c                   Else
023190051115     c                   Eval      *In28 = *On
023200060109     c                   Eval      h1riga = 06
023210051117     c                   Eval      h1colo = 12
023220051115     c                   Eval      v1cmsg = msg(05)
023230051115     c                   Leavesr
023240051115    3c                   EndIf
023250051108    2c                   EndIf
023260051219    1c                   EndIf
023270051116
023280051213      * Se non sono in interrogazione
023290051213     c                   If        Not *In05
023300051116      * PASSWORD
023310051116     c                   If        v1cpwd = *Blanks
023320051116     c                   Eval      *In28 = *On
023330051117     c                   Eval      h1riga = 21
023340051117     c                   Eval      h1colo = 29
023350060109     c                   Eval      v1cmsg = msg(07)
023360051116     c                   Leavesr
023370051116     c                   EndIf
023380051116      * Controllo se esatta
023390051116     c                   If        v1cpwd <> §mtcpwd
023400051116     c                   Eval      *In28 = *On
023410051117     c                   Eval      h1riga = 21
023420051117     c                   Eval      h1colo = 29
023430060109     c                   Eval      v1cmsg = msg(06)
023440051116     c                   Leavesr
023450051116     c                   EndIf
023460051116      * Controllo la validità se non sono in sede
023470051213if  2c                   If        Not *In09
023480051116      * Data immissione/variazione password
023490051116     c                   Clear                   wlbdat
023500051116     c                   Z-Add     §mtcdta       g02dat
023510051116     c                   Call      'XSRDA8'
023520051116     c                   Parm                    wlbdat
023530051116     c                   Movel     g02inv        dataiso
023540051116      * Scadenza password
023550051116     c                   Clear                   Tibs02ds
023560051116     c                   Eval      t02mod = 'C'
023570051116     c                   Eval      t02sif = knsif
023580051116     c                   Eval      t02cod = 'MTC'
023590051125     c                   Eval      t02ke1 = 'PSW'
023600051116     c                   Call      'TIBS02R'
023610051116     c                   Parm                    kpjba
023620051116     c                   Parm                    Tibs02ds
023630051213if  3c                   If        t02err <> *Blanks
023640051116     c                   Eval      *In28 = *On
023650051117     c                   Eval      h1riga = 21
023660051117     c                   Eval      h1colo = 29
023670051116     c                   Eval      v1cmsg = msg(09)
023680051116     c                   Leavesr
023690051213   x3c                   Else
023700051116     c                   Movel     t02uni        wgiorni
023710051116     c                   Adddur    wgiorni:*d    dataiso
023720051116     c                   Move      dataiso       datascad
023730051213if  4c                   If        dataoggi > dataScad
023740051116     c                   Eval      *In28 = *On
023750051117     c                   Eval      h1riga = 21
023760051117     c                   Eval      h1colo = 29
023770051116     c                   Eval      v1cmsg = msg(08)
023780051116     c                   Leavesr
023790051213    4c                   EndIf
023800051213    3c                   EndIf
023810051213    2c                   EndIf
023820051213    1c                   EndIf
023830051107
023840051107     c                   EndSr
023850051108
023860051108      *------------------------------------------------------------------------*
023870051108      * CONTROLLO SK DEI P.O. DI RICERCA
023880051108      *------------------------------------------------------------------------*
023890051108     c     Sr_Ctrfil     BegSr
023900051108
023910051108do  1c                   Do        24            xx
023920051108if  2c                   If        skfil(xx) <> *Blanks
023930051108     c     '?'           Scan      skfil(xx)                              30
023940051108if  3c                   If        *In30
023950051108     c                   Eval      §tip = 'E'
023960051108     c                   call      'TNSD23R'
023970051108     c                   Parm                    §tip
023980051108     c                   Parm                    §fil
023990051108
024000051108      * Imposto i p.o. selezionati
024010121130if  4c                   If        §fil <> *Blanks and §fil > *Zeros
024020051108     c                   Movea     §fil          skpo
024030051108     c                   Eval      yy = 1
024040051108if  5c                   Dow       skpo(yy) >*Zeros and xx <= 24
024050051108     c     '?'           Scan      skfil(xx)                              30
024060051108if  6c                   If        *In30 or skfil(xx) <= *Zeros
024070051108     c                   Movel     skpo(yy)      skfil(xx)
024080051108     c                   Add       1             yy
024090051108    6c                   EndIf
024100051108     c                   Add       1             xx
024110051108    5c                   EndDo
024120051108   x4c                   Else
024130051108     c                   Clear                   skfil(xx)
024140051108    4c                   EndIf
024150051108     c                   Leavesr
024160051108    3c                   EndIf
024170051108
024180051108      * Controllo i p.o. di ricerca immessi
024190121130if  3c                   If        skfil(xx) <> *Blanks and skfil(xx) > *Zeros
024200051108     c                   Move      skfil(xx)     w0030
024210160905     c                   clear                   og143
024220051108     c     w0030         Chain     Azorg01l
024230160905if  4c                   If        Not %Found(Azorg01l)
024240160905     c******                       (orgfag <> 'A' and orgfag <> 'F')
024250051108     c     xx            Add       40            zz
024260051108     c                   Eval      *In(zz) = *On
024270160906     c                   eval      *in28 = *on
024280160914     c                   eval      V1Cmsg = msg(91)
024290051118     c                   Goto      emi01
024300051108    4c                   EndIf
024310160905     c                   eval      OG143 = ORGde3
024320170922     c                   IF        §OGntw = 'XXX'
024330160905     c     xx            Add       40            zz
024340160905     c                   Eval      *In(zz) = *On
024350160905     c                   eval      *in28 = *on
024360160914     c                   eval      V1Cmsg = msg(91)
024370160905     c                   Goto      emi01
024380160905     c                   ENDIF
024390051108    3c                   EndIf
024400051108    2c                   EndIf
024410051108    1c                   EndDo
024420051108
024430051108     c                   EndSr
024440051216
024450051216      *------------------------------------------------------------------------*
024460051216      * CARICAMENTO DATI DAI FILE ALLE VIDEATE
024470051216      *------------------------------------------------------------------------*
024480051216     c     Sr_Cardati    BegSr
024490051216
024500051220     c                   Clear                   savabl
024510051220     c                   Clear                   savcpo
024520051220     c                   Clear                   savfft
024530051220     c                   Clear                   savitc
024540051220     c                   Clear                   savscf
024550051220     c                   Clear                   savtft
024560051220     c                   Clear                   savuni
024570080422     c                   clear                   sav4utip
024580100802     c                   clear                   savtipdn
024590100802     c                   clear                   savtipna
024600051216     c                   Clear                   wfscf
024610060105     c                   Eval      wforzacon = *Off
024620060215     c                   Clear                   savin22
024630060215     c                   Clear                   savin23
024640100728     c                   Clear                   savin71
024650100728     c                   Clear                   savin72
024660100728     c                   Clear                   savin73
024670100728     c                   Clear                   savin74
024680091119     c                   clear                   savntcdc
024690061030     c                   eval      wforzacdf = *off
024700061106     c                   eval      wforzaiva = *off
024710061106     c                   eval      *in39 = *off
024720080422     c                   eval      $win = *off
024730100729     c                   eval      $windn = *off
024740100729     c                   eval      $winna = *off
024750100729     c                   clear                   wbanca
024760100729     c                   clear                   wagenzia
024770100729     c                   clear                   savabidn
024780100729     c                   clear                   savabina
024790100729     c                   clear                   savcabdn
024800100729     c                   clear                   savcabna
024810100730     c                   clear                   savibandn
024820100730     c                   clear                   savibanna
024830100803     c                   eval      *in75 = *off
024840100803     c                   eval      *in76 = *off
024850100803     c                   eval      *in77 = *off
024860130322     c                   eval      *in79 = *off
024870130322     c                   eval      *in81 = *off
024880150415     c                   eval      *in82 = *off
024890170317     c                   eval      *in83 = *off
024900170320     c                   eval      *in84 = *off
024910170421     c                   eval      *in85 = *off
024920150424     c                   eval      wForzaPag = *off
024930150728     c                   eval      wForzaMail = *off
024940170516       wForzaPotenziale = *off;
024950110217
024960110217      /free
024970110217       //?pulisco campi di comodo per controlli da fare sul commerciale
024980110217       //?quando NON è anagrafica provvisoria
024990110217        clear sav_livello;
025000110217        clear wCodIncAtt;
025010110217      /end-free
025020051216
025030051219      * Se non sono passata dalla prima videata
025040051219      * aggancio l'anagrafica
025050051219if  1c                   If        $d01 = *Off
025060060110      * Spengo indicatori di comodo
025070090914     c                   Setoff                                       0102
025080060110     c                   Setoff                                       04
025090051222     c                   eval      kksc = v1cksc
025100051219      * Se non è richiamato da gestione visite e non è interrogazione
025110051219      * aggancio Cnaco00f per allocare il rcd
025120051219if  2c                   If        Not *In06 and Not *In05
025130060111     c  n05kCnaco        Chain(e)  Cnaco00f
025140051216      * Allocato errore
025150051219if  3c                   If        %error
025160051219      * Torno in prima videata se non è conferma visita
025170051216     c                   If        Not *In07
025180051216     c                   Eval      *In28 = *on
025190051216     c                   Eval      v1cmsg = msg(68)
025200051216     c                   Goto      emi01
025210051216     c                   EndIf
025220051219      * Torno al chiamante se è conferma visita
025230051216     c                   If        *In07
025240051216     c                   Goto      fine
025250051216     c                   EndIf
025260051219    3c                   EndIf
025270051219    2c                   EndIf
025280051219
025290091111      * Se è Interrogazione
025300091111     c                   if        *in05
025310091111      * visita/trattativa
025320091111     c                   if        *in06
025330091111     c                   eval      kksc = v1cnrv
025340091111     c     kCnaco        Chain(n)  Tfaco00f
025350091111      * cliente
025360091111     c                   else
025370051222     c                   eval      kksc = v1cksc
025380091111     c     kCnaco        Chain(n)  Cnaco00f
025390091111     c                   endif
025400091111     c                   endif
025410051216
025420091111      * Richiamato da gestione visite non in interrogazione
025430051219      * aggancio Tfaco00f e alloco il record
025440091111if  2c                   If        *In06 and not *in05
025450051222     c                   eval      kksc = v1cnrv
025460051216     c     kCnaco        Chain(e)  Tfaco00f
025470051216      * Allocato errore
025480051216     c                   If        %error
025490051216     c                   Goto      fine
025500051216     c                   EndIf
025510051219    2c                   EndIf
025520051219
025530051216      * Record trovato
025540051219if  2c                   If        %Found
025550051216      * Cliente esistente - manutenzione/Visualizzazione
025560051219if  3c                   If        acoflg <> '*'
025570051216     c  n05              Eval      *In02 = *On
025580051216     c  n05              Eval      vtcmod = mod(02)
025590051216     c   05              Eval      vtcmod = mod(03)
025600051216      * Cliente annullato
025610051219   x3c                   Else
025620051216     c                   Eval      *In04 = *On
025630051216     c                   Eval      vtcmod = mod(04)
025640051219    3c                   EndIf
025650051219      * Cliente nuovo - immissione
025660051219      * non essendo passata dalla prima videata l'inserimento avviene
025670051219      * solo se sono richiamata da gestione visite/offerte
025680051219      * quindi non controllo se sono in interrogazione
025690051219   x2c                   Else
025700051216     c                   Eval      *In01 = *On
025710051216     c                   Eval      vtcmod = mod(01)
025720051219    2c                   EndIf
025730051219    1c                   EndIf
025740051216
025750051216      * Se non è immissione imposto i dati dei file a video
025760051216if  1c                   If        Not *In01
025770060111     c  n06
025780060111     cann05kCnaco        Chain     Cnind00f
025790060111     c  n06
025800060111     can 05kCnaco        Chain(n)  Cnind00f
025810091111     c   06
025820091111     cann05kCnaco        Chain     Tfind00f
025830091111     c   06
025840091111     can 05kCnaco        Chain(n)  Tfind00f
025850060111     c  n06
025860060111     cann05kCnaco        Chain     Cnclp00f
025870060111     c  n06
025880060111     can 05kCnaco        Chain(n)  Cnclp00f
025890091111     c   06
025900091111     cann05kCnaco        Chain     Tfclp00f
025910091111     c   06
025920091111     can 05kCnaco        Chain(n)  Tfclp00f
025930060111     c  n06
025940060111     cann05v1cksc        Chain     Fncls01l
025950060111     c  n06
025960060111     can 05v1cksc        Chain(n)  Fncls01l
025970091111     c   06
025980091111     cann05v1cnrv        Chain     Tfcls01l
025990091111     c   06
026000091111     can 05v1cnrv        Chain(n)  Tfcls01l
026010051216
026020051216      * Data ultima variazione
026030051216     c                   Z-add     acoduv        dataamg
026040051216     c                   Z-add     aa1           aa2
026050051216     c                   Z-add     mm1           mm2
026060051216     c                   Z-add     gg1           gg2
026070051216     c                   Z-add     datagma       v2cduv
026080051216      * Allineato rcd da sede o da p.o.
026090051216     c                   If        acoftr = 'R'
026100051216     c                   Eval      v2ddtr = 'Ricevuto da Sede'
026110080115     c                   endif
026120080115     c                   if        acoftr = 'T'
026130051216     c                   Eval      v2ddtr = 'Trasmesso a Sede'
026140051216     c                   EndIf
026150051216      * Data allineamento rcd
026160051216     c                   Z-add     acodtr        dataamg
026170051216     c                   Z-add     aa1           aa2
026180051216     c                   Z-add     mm1           mm2
026190051216     c                   Z-add     gg1           gg2
026200051216     c                   Z-add     datagma       v2cdtr
026210051219      * Dati non visualizzati a video
026220051219     c                   Eval      v2copz = acoopz
026230051219     c                   Eval      v2ctic = acotic
026240051219     c                   Eval      v2clin = indlin
026250051216      * Dati anagrafici
026260051216     c                   Eval      v2crag = acorag
026270051216     c                   Eval      v2cvia = indvia
026280051216     c                   Eval      v2ccit = indcit
026290051216     c                   Eval      v2ccae = indcae
026300051216     c                   Eval      v2cprv = indprv
026310051216     c                   Eval      v2csta = indsta
026320051216     c                   Eval      v2ctel = indtel
026330051216     c                   Eval      v2ctlf = indtlf
026340051216     c                   Eval      v2ccdf = indcdf
026350080306     c                   Eval      savcdf = v2ccdf
026360051216     c                   Movel     indiva        w002a
026370051216     c                   If        w002a >= '00'
026380051216     c                   Movel     indiva        v2ivait
026390051216     c                   Else
026400051216     c                   Movel     indiva        v2civa
026410051216     c                   EndIf
026420080306     c                   Eval      saviva = v2civa
026430051216      * Blocco cliente
026440051216     c                   Eval      v2cabl = acoabl
026450130322     c                   eval      V2Cabledp = ACOabl
026460130322      * se blocco del cliente è '*' (blocco contabile) oppure
026470130322      * '7' (blocco automatico)
026480130322      * proteggo il campo del blocco + causale blocco
026490130322      * sempre, filiale e sede
026500130322     c                   IF        ACOabl = '*' or ACOabl = '7'
026510130322     c                   eval      *in79 = *on
026520130322     c                   ENDIF
026530130322     c                   IF        ACOabl = '7'
026540130322     c                   eval      V2Cabl = '8'
026550130322     c                   ENDIF
026560051220     c                   Eval      savabl = acoabl
026570051216      * Causale blocco cliente
026580051216     c                   Eval      v2cblc = clpnar
026590051216      * Stato del credito
026600051216     c                   Move      clpcon        v2ccon
026610110407      * Se il pgm è stato richiamato per il blocco cliente
026620110407      * imposto il blocco e la causale
026630110407     c                   IF        $Blocco
026640110407     c                   eval      v2cabl = '8'
026650110407     c                   eval      v2cblc = Parmcbl
026660110407     c                   ENDIF
026670051216      * Cliente annullabile
026680051216     c                   Eval      v2tb02 = atb02
026690051216      * Codice Potenziale
026700051216     c                   Eval      v2ccpo = acolib
026710051216     c                   Eval      savcpo = acolib
026720090616      * Cliente da sollecitare
026730090616     c                   Eval      v2csol = clpsol
026740090616      * sollecito in inglese
026750090616     c                   Eval      v2fsol = %subst(clsflo:4:1)
026760051216      * Codice commerciale
026770051216     c                   Movel     clpage        v3cage
026780051216      * Codice importanza cliente
026790051216     c                   Eval      v3cclv = clpclv
026800080313      * categoria merceologica
026810051216     c                   Move      acoitc        v3citc
026820051216      * Codice sottoconto fatturazione
026830100127     c                   Eval      v4cscf = %editc(clpscf:'X')
026840100329      * se sono da convalida offerta e non c'è il codice fatturazione
026850100329      * e la partita IVA è $$ imposto come codice fatturazione il codice
026860100329      * cliente che sto generando
026870100329     c                   IF        (v4cscf = *zeros or v4cscf = *blanks) and
026880100329     c                              *in07 and v2ivaeu = '$$'
026890100329     c                   eval      v4cscf = %editc(v1cksc:'X')
026900100329     c                   ENDIF
026910100127     c                   Eval      savscf = v4cscf
026920051216      * Fattura unificata
026930051216     c                   Eval      v4cuni = %subst(clsflo:2:1)
026940051216     c                   Eval      savuni = v4cuni
026950051216      * Tipo fattura
026960051216     c                   Move      clptft        v4ctft
026970051216     c                   Move      clptft        savtft
026980051216      * Frequenza fattura
026990051216     c                   Move      clpfft        v4cfft
027000051216     c                   Move      clpfft        savfft
027010051216      * Tipo data fattura (Inizio mese/Fine mese)
027020051216     c                   Eval      v4ctdf = clpfun
027030051216      * Spese invio fattura (nel file il campo non ha decimali solo a video)
027040051216     c                   Move      indsin        v4csin
027050051216      * Stampa mittente originale in fattura
027060051216     c                   Eval      v4cstm = %subst(clsflo:6:1)
027070051216      * Data prima spedizione fattura (solo visualizzazione viene impostato
027080051216      *                                dalla fatturazione)
027090051216     c                   Z-add     clpdps        dataamg
027100051216     c                   Z-add     aa1           aa2
027110051216     c                   Z-add     mm1           mm2
027120051216     c                   Z-add     gg1           gg2
027130051216     c                   Z-add     datagma       v4cdps
027140051216      * Data ultima spedizione fattura (solo visualizzazione viene impostato
027150051216      *                                 dalla fatturazione)
027160051216     c                   Z-add     clpdus        dataamg
027170051216     c                   Z-add     aa1           aa2
027180051216     c                   Z-add     mm1           mm2
027190051216     c                   Z-add     gg1           gg2
027200051216     c                   Z-add     datagma       v4cdus
027210051216      * Numero protocollo interno (solo visualizzazione viene impostato da Proj)
027220051216     c                   Eval      v4cnpn = indnpn
027230051216      * Numero protocollo cliente (solo visualizzazione viene impostato da Proj)
027240051216     c                   Eval      v4cnpr = indnpr
027250051216      * Data protocollo cliente   (solo visualizzazione viene impostato da Proj)
027260051216     c                   Z-add     inddpr        dataamg
027270051216     c                   Z-add     aa1           aa2
027280051216     c                   Z-add     mm1           mm2
027290051216     c                   Z-add     gg1           gg2
027300051216     c                   Z-add     datagma       v4cdpr
027310051216      * Codice pagamento
027320100727     c                   Move      indcdp        v4ccdp
027330051216      * Abi
027340100727     c                   Eval      v4cabi = indabi
027350051216      * Cab
027360100727     c                   Eval      v4ccab = indcab
027370121105      * Tassazione Post Fatturazopme
027380121105     c                   eval      V4Ctpf = %subst(CLSflo:3:1)
027390090612      * Blocco pagamenti
027400051216     c                   Eval      wbloc = %subst(indopz:1:1)
027410130322     c                   eval      V5Cblpedp = %subst(indopz:1:1)
027420130322      * se blocco pagamenti NON è 'B' (blocco manuale)
027430130322      * proteggo il campo del blocco pagamenti
027440130322      * sempre, filiale e sede
027450130322     c                   IF        V5Cblpedp <> 'B' and
027460130322     c                             V5Cblpedp <> '0' and
027470130322     c                             V5Cblpedp <> *blanks
027480130322     c                   eval      *in81 = *on
027490130322     c                   ENDIF
027500130322     c                   If        wbloc = '0'
027510130322     c                   Eval      v5cblp = 'NO'
027520130322     c                   Else
027530130322     c                   Eval      v5cblp = 'SI'
027540130322     c                   EndIf
027550100728
027560051216      * Codice pagamento contrassegni
027570051216     c                   Eval      v5cfpc = clpfpc
027580080115      * Codice IBAN
027590080115     c                   eval      v5ciban = clpbab
027600100803      * il BIC per i contrassegni per ora non ce l'ho mai quindi non lo faccio
027610100803      * mai vedere
027620100803     c                   eval      *in75 = *on
027630100728
027640100728      /free
027650100728       //?Nuovi tipi pagamento
027660100729       //?Pagamento DANNI --> DN
027670100728         v5tpdn = %subst(clstic:1:1);
027680100728       //?Recupero le coordinate bancarie
027690100728         wpag = 'DN';
027700100728         rqsOpCode = 'SEARCH';
027710100728         exsr Coordinate;
027720100728         IF  rpyEsito = 0;
027730100728           IF not *in06;
027740100730             v5ibandn = TrulIbanO0.IBAN;
027750100730             savabidn = TrulIbanO0.ABI;
027760100730             savcabdn = TrulIbanO0.CAB;
027770100730             v5bicdn  = TrulIbanO0.BIC;
027780100728           ELSE;
027790100730             v5ibandn = TrulIbanO1.IBAN;
027800100730             savabidn = TrulIbanO1.ABI;
027810100730             savcabdn = TrulIbanO1.CAB;
027820100730             v5bicdn  = TrulIbanO1.BIC;
027830100728           ENDIF;
027840100728         ENDIF;
027850100803         IF  v5bicdn = *blanks;
027860100803           *in76 = *on;
027870100803         ENDIF;
027880100729       //?Pagamento NOTE ACCREDITO --> NA
027890100728         v5tpna = %subst(clstic:2:1);
027900100728       //?Recupero le coordinate bancarie
027910100728         wpag = 'NA';
027920100728         exsr Coordinate;
027930100728         IF  rpyEsito = 0;
027940100728           IF not *in06;
027950100730             v5ibanna = TrulIbanO0.IBAN;
027960100730             savabina = TrulIbanO0.ABI;
027970100730             savcabna = TrulIbanO0.CAB;
027980100730             v5bicna  = TrulIbanO0.BIC;
027990100728           ELSE;
028000100730             v5ibanna = TrulIbanO1.IBAN;
028010100730             savabina = TrulIbanO1.ABI;
028020100730             savcabna = TrulIbanO1.CAB;
028030100730             v5bicna  = TrulIbanO1.BIC;
028040100728           ENDIF;
028050100728         ENDIF;
028060100803         IF  v5bicna = *blanks;
028070100803           *in77 = *on;
028080100803         ENDIF;
028090100730         savibandn = v5ibandn;
028100100730         savibanna = v5ibanna;
028110100728      /end-free
028120100728
028130051216      * Tipo comunicazione al mittente
028140051216     c                   Eval      v6ctcm = %subst(clsflo:10:1)
028150051216      * Tipo comunicazione fine giacenza
028160051216     c                   Eval      v6ctcf = %subst(clsflo:1:1)
028170051216      * Apertura automatica giacenza
028180051216     c                   Eval      v6capg = %subst(clsflo:9:1)
028190051216      * Disposizione mittente in arrivo
028200051216     c                   If        acorx1 = *Zeros
028210051216     c                   Eval      v6cdmg = 'SI'
028220051216     c                   Else
028230051216     c                   Eval      v6cdmg = 'NO'
028240051216     c                   EndIf
028250060801      * Chiusura automatica giacenza
028260060801     c                   If        acory1 = *Zeros
028270060801     c                   Eval      v6ccag = 'SI'
028280060801     c                   Else
028290060801     c                   Eval      v6ccag = 'NO'
028300060801     c                   EndIf
028310051216      * Rimborso danni
028320051216      * Stampa copia denuncia al cliente
028330051216     c                   Eval      v7csdn = %subst(clsflo:5:1)
028340051216      * Comunicazioni in inglese
028350051216     c                   Eval      v7csin = %subst(clsflo:7:1)
028360051216      * Escludi da tassazione diretta porti assegnati
028370051216     c                   Eval      v7ctpa = %subst(clsflo:8:1)
028380051216      * Nr. stop autotrasportatori
028390051216     c                   Eval      v7cstp = clsstp
028400051216
028410051216      * Se immissione imposto dei campi di default
028420051216   x1c                   Else
028430051219      * Campi non visualizzati
028440051219     c                   Eval      v2copz = wopz
028450051219     c                   Eval      v2ctic = wtic
028460051216     c                   Eval      v2clin = '1'
028470051219      * Campi visualizzati
028480051216     c                   Clear                   v2ccpo
028490051216      * Se provengo da visite/offerte
028500051216if  2c                   If        *In06
028510051216      * Carico i dati del potenziale
028520051220     c     ta60cpo       Chain(n)  Tncpo01l
028530051216     c                   If        %Found(Tncpo01l)
028540051216     c                   ExSr      Sr_Carcpo
028550051216     c                   EndIf
028560051216    2c                   EndIf
028570090612      * Cliente da sollecitare
028580090616      * imposto il dft
028590090616     c                   ExSr      Sr_Ctrsol
028600051216      * Commerciale
028610051216     c                   Clear                   v3cage
028620051216      * Imposto il codice commerciale della visita
028630051216     c   06              Movel     ta60cmm       v3cage
028640080313      * categoria merceologica
028650051216     c                   Clear                   v3citc
028660080313      * Se impostata dal potenziale riporto la categoria merceologica
028670051216     c                   If        savitc <> *Blanks
028680051216     c                   Eval      v3citc = savitc
028690051216     c                   EndIf
028700051219      * Codice importanza cliente
028710100301      * lo imposto se non è immissione anagrafica provvisoria da trattativa
028720100806     c  n06              Eval      v3cclv = 'C'
028730051216      * Codice sottoconto fatturazione
028740100729      * imposto a 0 così l'utente è obbligato ad inserirlo
028750100303     c                   eval      v4cscf = '0000000'
028760051216      * Tipo fattura
028770051216     c                   Eval      v4ctft = '0'
028780051216     c                   Eval      savtft = '0'
028790051216      * Frequenza fattura
028800051216     c                   Eval      v4cfft = '4'
028810051216     c                   Eval      savfft = '4'
028820091109      * Tipo data fattura (Inizio mese/Fine mese)
028830091125      * se da trattativa
028840100806     c   06              eval      v4ctdf = 'F'
028850051216      * Spese invio fatturazione
028860051216     c                   Z-add     §gedrf        v4csin
028870060210      * se non è anagrafica provvisoria e cliente 8888 o 9999 non devo
028880060210      * imputare le spese di invio fattura
028890060210     c                   Move      v1cksc        w0040
028900060210     c                   If        Not *In06 and
028910060210     c                             (w0040 = 8888 or w0040 = 9999)
028920060210     c                   Clear                   v4csin
028930060210     c                   EndIf
028940051216      * Protocollo
028950051216     c                   Clear                   v4cnpn
028960051216     c                   Clear                   v4cnpr
028970051216     c                   Clear                   v4cdpr
028980051221      * Codice Pagamento
028990100727     c                   Move      *Zeros        v4ccdp
029000130322      * Blocco pagamento campo di work
029010130322     c                   eval      wbloc = '0'
029020080115      * Tipo Pagamento contrassegni
029030091125      * se NON è da trattativa
029040100806     c  n06              eval      v5cfpc = '2'
029050100805
029060100805      /free
029070100805       //?Tipo pagamento Danni
029080150424         v5tpdn = dfttipdn;
029090100805       //?Tipo pagamento Note Accredito
029100150424         v5tpna = dfttipna;
029110100805      /end-free
029120100728
029130051216      * Tipo comunicazione al mittente
029140120118      * lo imposto se non è immissione anagrafica provvisoria da trattativa
029150120118     c  n06              Eval      v6ctcm = §2gtcm
029160051216      * Tipo comunicazione fine giacenza
029170051216     c                   Eval      v6ctcf = §2gtfg
029180051216      * Apertura automatica giacenza
029190051216     c                   Clear                   v6capg
029200051216      * Disposizione mittente in arrivo
029210051216     c                   Eval      v6cdmg = 'SI'
029220060801      * Chiusura automatica giacenza
029230060801     c                   Eval      v6ccag = 'SI'
029240051216
029250051216    1c                   EndIf
029260110217
029270110217      /free
029280110217       //?Se non è anagrafica provvisoria
029290110217         IF  not *in06;
029300110217       //?Aggancio l'organigramma x controllo se filiale cliente
029310110217       //?è un primo o secondo livello
029320110217           clear og148;
029330110217           w0030 = %dec(%subst(%editc(v1cksc:'X'):1:3):3:0);
029340110217           chain  w0030  azorg01l;
029350110217           IF  %found(azorg01l);
029360110217             og148 = ORGde8;
029370110217           ENDIF;
029380110217           sav_Livello = §OGlpo;
029390110217       //?Recupero da tabella Y4O il codice cliente 'incassi da attribuire'
029400110217           clear TIBS02DS;
029410110217           clear dY4O;
029420110217           T02mod = 'C';
029430110217           T02cod = 'Y4O';
029440110217           T02ke1 = '201';
029450110217           T02ke2 = %editc(w0030:'X');
029460110217           TNTBE_RicercaControllo (kpjba : tibs02ds);
029470110217           IF  T02err = *blanks;
029480110217             dY4O = T02uni;
029490110217           ENDIF;
029500110217           wCodIncAtt = %subst(§4Osc1:2:7);
029510110217         ENDIF;
029520170421
029530170421       //?Se sono in manutenzione
029540170421       //?Se non sono utente EDP e il cliente che sto manutenzionando
029550170421       //?un cliente IMPORT DPD proteggo i flag relativi alla
029560170421       //?Gestione Giacenze
029570170421         IF  *in02 and not *in20 and %lookup(V1Cksc:CliImport) > 0;
029580170421           *in85 = *on;
029590170421         ENDIF;
029600170421
029610110217      /end-free
029620051216
029630051216      * Decodifico
029640051216     c                   ExSr      Sr_Decdati
029650060531     c
029660060531     c* Memorizzo l'immagine precedente per  verificare se dati variati
029670150427      /free
029680150427         exsr ImmaginePrima;
029690150427      /end-free
029700051216
029710051216     c                   EndSr
029720100728
029730100728      /free
029740100728       //--------------------------------------------------------------
029750100728       //?Operazioni relative alle coordinate bancarie.
029760100728       //--------------------------------------------------------------
029770100728       BEGSR Coordinate;
029780100802
029790100802         clear rpyEsito;
029800100728
029810100728       //?FNCBA00F
029820100728         IF  not *in06;
029830100730           clear TrulIbanI0;
029840100730           clear TrulIbanO0;
029850100728           TrulIbanI0.KSC = v1cksc;
029860100728           TrulIbanI0.PAG = wpag;
029870100729         //?Se non è ricerca passo i dati del video
029880100729           IF  rqsOpCode <> 'SEARCH';
029890100805             TrulIbanI0.IBAN  = wiban;
029900100805             TrulIbanI0.BIC   = wbic;
029910100805             TrulIbanI0.CODPO = wcodpo;
029920100805             TrulIbanI0.CODPN = wcodpn;
029930100805             TrulIbanI0.PGM   = wpgm;
029940100729           ENDIF;
029950100728           rqsFormatName = 'TRULIBANI0';
029960100728           rpyFormatName = 'TRULIBANO0';
029970100728           rqsDataSize   = %size(TrulIbanI0);
029980100728           rpyDataSize   = %size(TrulIbanO0);
029990100728           TrulIbanR (rqsOpCode:rpyEsito:
030000100729                      rqsFormatName:TrulIbanI0:rqsDataSize:
030010100729                      rpyFormatname:TrulIbanO0:rpyDataSize);
030020100728       //?TFCBA00F
030030100728         ELSE;
030040100730           clear TrulIbanI1;
030050100730           clear TrulIbanO1;
030060100729           TrulIbanI1.NRV = v1cnrv;
030070100728           TrulIbanI1.PAG = wpag;
030080100729         //?Se non è ricerca passo i dati del video
030090100729           IF  rqsOpCode <> 'SEARCH';
030100100730             TrulIbanI1.IBAN = wiban;
030110100730             TrulIbanI1.BIC  = wbic;
030120100729           ENDIF;
030130100728           rqsFormatName = 'TRULIBANI1';
030140100728           rpyFormatName = 'TRULIBANO1';
030150100728           rqsDataSize   = %size(TrulIbanI1);
030160100728           rpyDataSize   = %size(TrulIbanO1);
030170100728           TrulIbanR (rqsOpCode:rpyEsito:
030180100728                      rqsFormatName:TrulIbanI1:rqsDataSize:
030190100729                      rpyFormatname:TrulIbanO1:rpyDataSize);
030200100728         ENDIF;
030210100728
030220100728       ENDSR;
030230100728      /end-free
030240051216
030250051216      *------------------------------------------------------------------------*
030260051216      * DECODIFICO I DATI DELLE VIDEATE
030270051216      *------------------------------------------------------------------------*
030280051216     c     Sr_Decdati    BegSr
030290051216
030300060220     c                   Eval      *In19 = *Off
030310051216     c                   Eval      *In22 = *Off
030320051216     c                   Eval      *In23 = *Off
030330100728     c                   Eval      *In71 = *Off
030340100728     c                   Eval      *In72 = *Off
030350100728     c                   Eval      *In73 = *Off
030360100728     c                   Eval      *In74 = *Off
030370150415     c                   Eval      *In82 = *Off
030380051216
030390051216      * Decodifico lo stato del credito
030400051216     c                   Clear                   v2dcon
030410051216     c                   Clear                   ds4w
030420051216     c                   Eval      kcod = '4W'
030430051216     c                   Move(p)   v2ccon        w003a
030440051216     c                   Eval      kkey = w003a
030450051216     c     kTabel        Chain     Tabel00f
030460051216     c                   If        %Found(Tabel00f) and
030470051216     c                             tblflg = *Blanks
030480051216     c                   Eval      ds4w = tbluni
030490051216     c                   EndIf
030500051216     c                   Eval      v2dcon = §4wdes
030510051216
030520051216      * Se sono in filiale proteggo lo stato del credito se non è modificabile
030530120928      * da p.o.
030540130325      * da 25/03/2013 abbiamo deciso che anche in sede non è modificale, solo
030550130325      * da ProJ
030560130325     c                   If        §4wmbl = 'N'
030570060220     c                   Eval      *In19 = *On
030580051216     c                   EndIf
030590130322
030600120928      * Se sono in filiale proteggo il blocco pagamenti se non è modificabile
030610120928      * da p.o.
030620120928     c                   If        Not *In09 and §4wmbp = 'N'
030630120928     c                   Eval      *In81 = *On
030640120928     c                   EndIf
030650120925      * Se sono in filiale proteggo il blocco cliente se non è modificabile
030660120925      * da p.o.
030670120928     c                   If        Not *In09 and §4wmtb = 'N'
030680120925     c                   Eval      *In79 = *On
030690120925     c                   EndIf
030700170317      /free
030710170330       //?Se sono un utente NON abilitato alla variazione
030720170317       //?e il blocco clienti è protetto
030730170317       //?e il cliente è in contenzioso
030740170330       //?devo bloccare anche la frequenza fattura
030750170330         IF  §UTEclpfft <> 'S' and *in79 and
030760170330             §4Wtip <> *blanks;
030770170317           *in83 = *on;
030780170317         ENDIF;
030790170317      /end-free
030800051216
030810051216      * Decodifico la causale blocco cliente
030820110323     c                   clear                   dsbi
030830051216     c                   Clear                   v2dblc
030840051216     c                   Eval      kcod = 'BI'
030850051216     c                   Eval      kkey = v2cblc
030860051216     c     kTabel        Chain     Tabel00f
030870051216     c                   If        %Found(Tabel00f) and
030880051216     c                             tblflg = *Blanks
030890110323     c                   Eval      dsbi = tbluni
030900051216     c                   EndIf
030910110323     c                   Eval      v2dblc = §bides
030920051216
030930051216      * Decodifico il codice potenziale
030940051216     c                   Clear                   v2dcpo
030950110223     c                   clear                   v2hfls
030960051216     c                   If        v2ccpo <> *Zeros
030970051220     c     v2ccpo        Chain(n)  Tncpo01l
030980051216     c                   If        %Found(Tncpo01l)
030990051216     c                   Eval      v2dcpo = cporag
031000110223     c                   eval      v2hfls = CPOfls
031010121105     c                   eval      dCPO01 = CPOrst
031020051216     c                   EndIf
031030051216     c                   EndIf
031040051216
031050051216      * Decodifico il codice commerciale
031060051216     c                   Clear                   v3dage
031070130806     c                   If        %check(digits:V3Cage) = 0
031080130806     c                   move      V3Cage        CMMcod
031090130806     c     CMMcod        chain     AZCMM000
031100130806     c                   if        %Found(AZCMM01L)
031110130806     c                   movel     CMMdes        v3dage
031120130806     c                   endif
031130130806     c                   EndIf
031140051216
031150051216      * Decodifico il codice importanza cliente
031160051216     c                   Clear                   v3dclv
031170051216     c                   Clear                   dsic
031180051216     c                   Eval      kcod = 'IC'
031190051216     c                   Eval      kkey = v3cclv
031200051216     c     kTabel        Chain     Tabel00f
031210051216     c                   If        %Found(Tabel00f) and
031220051216     c                             tblflg = *Blanks
031230051216     c                   Eval      dsic = tbluni
031240051216     c                   EndIf
031250051216     c                   Eval      v3dclv = §sicde
031260051216
031270080313      * Decodifico categoria merceologica
031280051216     c                   Clear                   v3ditc
031290051216     c                   Clear                   ds1l
031300051216     c                   Eval      kcod = '1L'
031310051216     c                   Movel     v3citc        kkey
031320051216     c     kTabel        Chain     Tabel00f
031330051216     c                   If        %Found(Tabel00f) and
031340051216     c                             tblflg = *Blanks
031350051216     c                   Eval      ds1l = tbluni
031360051216     c                   EndIf
031370051216     c                   Eval      v3ditc = §1ldes
031380051216
031390051216      * Decodifico sottoconto fatturazione
031400051216     c                   Clear                   Tibs69ds
031410051216     c                   Move      v4cscf        I69kac
031420051216     c                   Call      'TIBS69R'
031430051216     c                   Parm                    Tibs69ds
031440051216     c                   Parm                    ds_cnaco
031450051216     c                   Parm                    ds_cnind
031460051216     c                   Parm                    ds_cnclp
031470051216     c                   Parm                    ds_fncls
031480051216     c                   Eval      wtibs69 = *On
031490051216     c                   Movel     w_acorag      v4dscf
031500051216
031510051216      * Decodifico il tipo fattura
031520051216     c                   Clear                   Tibs02ds
031530051216     c                   Eval      t02mod = 'C'
031540051216     c                   Eval      t02sif = knsif
031550051216     c                   Eval      t02cod = 'TFT'
031560051216     c                   Eval      t02ke1 = v4ctft
031570051216     c                   Call      'TIBS02R'
031580051216     c                   Parm                    kpjba
031590051216     c                   Parm                    Tibs02ds
031600051216     c                   If        t02err = *Blanks
031610051216     c                   Eval      dtft = t02uni
031620051216     c                   EndIf
031630051216     c                   Eval      v4dtft = §tftdes
031640051216
031650051216      * Decodifico la frequenza fattura
031660051216     c                   Clear                   v4dfft
031670051216     c                   Clear                   dsff
031680051216     c                   Eval      kcod = 'FF'
031690051216     c                   Eval      kkey = v4cfft
031700051216     c     kTabel        Chain     Tabel00f
031710051216     c                   If        %Found(Tabel00f) and
031720051216     c                             tblflg = *Blanks
031730051216     c                   Eval      dsff = tbluni
031740051216     c                   EndIf
031750051216     c                   Eval      v4dfft = §ffdes
031760051216
031770051216      * Decodifico il tipo data fattura
031780051216     c                   If        v4ctdf <> *Blanks
031790051216     c                   Clear                   v4dtdf
031800051216     c                   Eval      kcod = '13'
031810051216     c                   Eval      kkey = v4ctdf
031820051216     c     kTabel        Chain     Tabel00f
031830051216     c                   If        %Found(Tabel00f) and
031840051216     c                             tblflg = *Blanks
031850051216     c                   Eval      v4dtdf = tbluni
031860051216     c                   EndIf
031870051216     c                   EndIf
031880051216
031890051216      * Decodifico codice di pagamento
031900100727     c                   Clear                   v4dcdp
031910051216     c                   Clear                   dsfa
031920051216     c                   Eval      kcod = 'FA'
031930100727     c                   Movel     v4ccdp        w004a
031940051216     c                   Move      '1'           w004a
031950051216     c                   Eval      kkey = w004a
031960051216     c     kTabel        Chain     Tabel00f
031970051216     c                   If        %Found(Tabel00f) and
031980051216     c                             tblflg = *Blanks
031990051216     c                   Eval      dsfa = tbluni
032000051216     c                   EndIf
032010100727     c                   Eval      v4dcdp = §fades
032020150415
032030150415      /free
032040150415       //?Se utente di filiale e tipo pagamento utilizzabile solo da sede
032050150415       //?devo proteggere la Banca ABI/CAB del pagamento fatture
032060150415         IF  not *in09 and §FAuti = 'S';
032070150415           *in82 = *on;
032080150415         ENDIF;
032090170320       //?Se sono un utente di filiale
032100170320       //?e il blocco clienti è protetto
032110170320       //?e il cliente è in contenzioso
032120170330       //?devo bloccare il codice pagamento
032130170330         IF  not *in09 and *in79 and §4Wtip <> *blanks;
032140170320           *in84 = *on;
032150170320         ENDIF;
032160150415      /end-free
032170080206
032180080206      * decodifico le coordinate bancarie abi-cab
032190100729      * riscossione fattura
032200100729     c                   clear                   v4cbna
032210100727     c                   eval      kabi = v4cabi
032220100727     c                   eval      kcab = v4ccab
032230100729     c                   exsr      DesBanca
032240100729     c                   eval      v4cbna = wdesbanca
032250051216
032260051216      * Decodifico codice pagamanto contrassegni
032270051216     c                   Clear                   v5dfpc
032280051216     c                   Clear                   ds4u
032290051216     c                   Eval      kcod = '4U'
032300051216     c                   Eval      kkey = v5cfpc
032310051216     c     kTabel        Chain     Tabel00f
032320051216     c                   If        %Found(Tabel00f) and
032330051216     c                             tblflg = *Blanks
032340051216     c                   Eval      ds4u = tbluni
032350051216     c                   EndIf
032360051216     c                   Eval      v5dfpc = §4udes
032370100729      * tipo pagamento contrassegni, salvo il flag di tipo pagamento i/e
032380100729     c                   eval      sav4utip = §4utip
032390051216      * Le coordinate bancare le visualizzo o le proteggo in base al
032400051216      * tipo pagamento
032410051216     c                   Select
032420051216      * Coordinate italiane, visualizzo
032430051216      * Immissione coordinate non richiesta, non visualizzo
032440051216     c                   When      §4uabi = 'N'
032450051216     c                   Eval      *In22 = *On
032460060215     c                   Eval      savin22 = *In22
032470051216      * Se pagamento estero proteggo le coordinate bancarie perchè
032480051216      * gestite dalla sede.
032490080115     c                   when      §4utip = 'E'
032500051216     c                   Eval      *In23 = *On
032510060215     c                   Eval      savin23 = *In23
032520051216     c                   EndSl
032530080115      * se le visualizzo ed è un tipo pagamento italia decodifico la banca
032540080115     c                   if        not *in22 and not *in23
032550080206     c                             and v5ciban <> *blanks
032560080213     c                             and (%subst(v5ciban:1:2) = 'IT'
032570080213     c                              or %subst(v5ciban:1:2) = 'SM')
032580080116     c                   clear                   v5cbab
032590080213     c                   monitor
032600080115     c                   eval      kabi = %dec(%subst(v5ciban:6:5):5:0)
032610080115     c                   eval      kcab = %dec(%subst(v5ciban:11:5):5:0)
032620080213     c                   on-error
032630080213     c                   clear                   kabi
032640080213     c                   clear                   kcab
032650080213     c                   endmon
032660100729     c                   exsr      DesBanca
032670100729     c                   eval      v5cbab = wdesbanca
032680080115     c                   endif
032690090401      * se le visualizzo ed è un pagemento estero senza IBAN con
032700090401      * abi e cab no a 99999 decodifico la banca
032710090401     c                   if        not *in22 and *in23
032720090401     c                             and v5ciban = *blanks
032730090401     c                             and clpabi <> *all'9'
032740090401     c                             and clpcab <> *all'9'
032750090401     c                   monitor
032760090401     c                   eval      kabi = clpabi
032770090401     c                   eval      kcab = clpcab
032780090401     c                   on-error
032790090401     c                   clear                   kabi
032800090401     c                   clear                   kcab
032810090401     c                   endmon
032820100729     c                   exsr      DesBanca
032830100729     c                   eval      v5cbab = wdesbanca
032840090401     c                   endif
032850100728
032860100728      /free
032870100728       //?Pagamenti DANNI --> DN
032880100728       //?tipo pagamento
032890100728         clear v5dpdn;
032900100728         clear ds4u;
032910100728         kcod = '4U';
032920100730         kkey = v5tpdn;
032930100728         chain (codut:kcod:kkey) TABEL00F;
032940100728         IF  %found(TABEL00F) and TBLflg = *blanks;
032950100728           ds4u = TBLuni;
032960100728         ENDIF;
032970100730         v5dpdn = §4Udes;
032980100802       //?salvo il tipo pagamento
032990100802         savtipdn = v5tpdn;
033000100728       //?coordinate bancarie
033010100728         SELECT;
033020100728         //?se abi cab non obbligatori non visualizzo
033030100730           WHEN  §4Uabi = 'N';
033040100728             *in71 = *on;
033050100728             savin71 = *in71;
033060100805             *in76 = *off;
033070100728         //?se pagamento estero proteggo perchè solo la sede le gestisce
033080100730           WHEN  §4Utip = 'E';
033090100728             *in72 = *on;
033100100728             savin72 = *in72;
033110100728         ENDSL;
033120100729       //?se IBAN IT o SM (IBAN italia) visualizzo la descrizione banca
033130100729       //?recuperandola con ABI e CAB dell'IBAN
033140100729         IF  not *in71 and not *in72 and v5ibandn <> *blanks and
033150100729            (%subst(v5ibandn:1:2) = 'IT' or %subst(v5ibandn:1:2) = 'SM');
033160100729           clear v5babdn;
033170100729           monitor;
033180100729           kabi = %dec(%subst(v5ibandn:6:5):5:0);
033190100729           kcab = %dec(%subst(v5ibandn:11:5):5:0);
033200100729           on-error;
033210100729           clear kabi;
033220100729           clear kcab;
033230100729           endmon;
033240100729           exsr DesBanca;
033250100729           v5babdn = wDesBanca;
033260100729         ENDIF;
033270100729       //?se IBAN estero (campo iban vuoto e ABI e CAB impostati <> 99999)
033280100729       //?visualizzo la descrizione della banca recuperandola con ABI e CAB
033290100729       //?memorizzati sul file
033300100729         IF  not *in71 and not *in72 and v5ibandn = *blanks and
033310100729             savabidn <> *all'9' and savcabdn <> *all'9';
033320100729           clear v5babdn;
033330100729           monitor;
033340100729           kabi = savabidn;
033350100729           kcab = savcabdn;
033360100729           on-error;
033370100729           clear kabi;
033380100729           clear kcab;
033390100729           endmon;
033400100729           exsr DesBanca;
033410100729           v5babdn = wDesBanca;
033420100729         ENDIF;
033430100729
033440100729       //?Pagamenti NOTE ACCREDITO --> NA
033450100729       //?tipo pagamento
033460100729         clear v5dpna;
033470100729         clear ds4u;
033480100729         kcod = '4U';
033490100730         kkey = v5tpna;
033500100729         chain (codut:kcod:kkey) TABEL00F;
033510100729         IF  %found(TABEL00F) and TBLflg = *blanks;
033520100729           ds4u = TBLuni;
033530100729         ENDIF;
033540100730         v5dpna = §4Udes;
033550100802       //?salvo il tipo pagamento
033560100802         savtipna = v5tpna;
033570100729       //?coordinate bancarie
033580100729         SELECT;
033590100729         //?se abi cab non obbligatori non visualizzo
033600100730           WHEN  §4Uabi = 'N';
033610100729             *in73 = *on;
033620100729             savin73 = *in73;
033630100805             *in77 = *off;
033640100729         //?se pagamento estero proteggo perchè solo la sede le gestisce
033650100730           WHEN  §4Utip = 'E';
033660100729             *in74 = *on;
033670100729             savin74 = *in74;
033680100729         ENDSL;
033690100729       //?se IBAN IT o SM (IBAN italia) visualizzo la descrizione banca
033700100729       //?recuperandola con ABI e CAB dell'IBAN
033710100729         IF  not *in73 and not *in74 and v5ibanna <> *blanks and
033720100729            (%subst(v5ibanna:1:2) = 'IT' or %subst(v5ibanna:1:2) = 'SM');
033730100729           clear v5babna;
033740100729           monitor;
033750100729           kabi = %dec(%subst(v5ibanna:6:5):5:0);
033760100729           kcab = %dec(%subst(v5ibanna:11:5):5:0);
033770100729           on-error;
033780100729           clear kabi;
033790100729           clear kcab;
033800100729           endmon;
033810100729           exsr DesBanca;
033820100729           v5babna = wDesbanca;
033830100729         ENDIF;
033840100729       //?se IBAN estero (campo iban vuoto e ABI e CAB impostati <> 99999)
033850100729       //?visualizzo la descrizione della banca recuperandola con ABI e CAB
033860100729       //?memorizzati sul file
033870100729         IF  not *in73 and not *in74 and v5ibanna = *blanks and
033880100729             savabina <> *all'9' and savcabna <> *all'9';
033890100729           clear v5babna;
033900100729           monitor;
033910100729           kabi = savabina;
033920100729           kcab = savcabna;
033930100729           on-error;
033940100729           clear kabi;
033950100729           clear kcab;
033960100729           endmon;
033970100729           exsr DesBanca;
033980100729           v5babna = wDesbanca;
033990100729         ENDIF;
034000100729
034010100728      /end-free
034020051216
034030051216      * Decodifico tipo comunicazioni al mittente
034040051216     c                   Clear                   v6dtcm
034050051216     c                   Clear                   ds2f
034060051216     c                   Eval      kcod = '2F'
034070051216     c                   Eval      kkey = v6ctcm
034080051216     c     kTabel        Chain     Tabel00f
034090051216     c                   If        %Found(Tabel00f) and
034100051216     c                             tblflg = *Blanks
034110051216     c                   Eval      ds2f = tbluni
034120051216     c                   EndIf
034130051216     c                   Eval      v6dtcm = §2fdes
034140051216
034150051216      * Decodifico tipo comunicazioni fine giacenza
034160051216     c                   Clear                   v6dtcf
034170051216     c                   Clear                   ds2h
034180051216     c                   Eval      kcod = '2H'
034190051216     c                   Eval      kkey = v6ctcf
034200051216     c     kTabel        Chain     Tabel00f
034210051216     c                   If        %Found(Tabel00f) and
034220051216     c                             tblflg = *Blanks
034230051216     c                   Eval      ds2h = tbluni
034240051216     c                   EndIf
034250051216     c                   Eval      v6dtcf = §2hdes
034260051216
034270051216      * Decodifico apertura automatica giacenza
034280051216     c                   Clear                   v6dapg
034290051216     c                   Clear                   ds2u
034300051216     c                   Eval      kcod = '2U'
034310051216     c                   Eval      kkey = v6capg
034320051216     c     kTabel        Chain     Tabel00f
034330051216     c                   If        %Found(Tabel00f) and
034340051216     c                             tblflg = *Blanks
034350051216     c                   Eval      ds2u = tbluni
034360051216     c                   EndIf
034370051216     c                   Eval      v6dapg = §2udes
034380051216
034390051216     c                   EndSr
034400051216
034410051216      *------------------------------------------------------------------------*
034420051216      * CARICO I LUOGHI PREVISTI
034430051216      *------------------------------------------------------------------------*
034440051216     c     Sr_Carluoghi  BegSr
034450051216
034460051216     c                   Eval      *In10 = *Off
034470051216     c                   Eval      *In11 = *Off
034480051216     c                   Eval      *In12 = *Off
034490051216     c                   Eval      *In14 = *Off
034500051216     c                   Eval      *In16 = *Off
034510051216     c                   Eval      *In17 = *Off
034520060208     c                   Eval      *In33 = *Off
034530060208     c                   Eval      *In35 = *Off
034540060208     c                   Eval      *In37 = *Off
034550051216
034560051216      * Luogo invio fattura
034570051216     c                   Eval      kspecli = v1cksc
034580051216     c                   Eval      kspecod = '001'
034590051216     c     kFnspe        Chain     Fnspe01l
034600051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
034610051216     c                   Eval      *In10 = *On
034620051216     c                   EndIf
034630051216
034640051216      * Luogo intestatario pagamento contrassegno
034650051216     c                   Eval      kspecod = '555'
034660051216     c     kFnspe        Chain     Fnspe01l
034670051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
034680051216     c                   Eval      *In11 = *On
034690051216     c                   EndIf
034700051216
034710051216      * Luogo invio contrassegno mittente
034720051216     c                   Eval      kspecod = '666'
034730051216     c     kFnspe        Chain     Fnspe01l
034740051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
034750051216     c                   Eval      *In12 = *On
034760051216     c                   EndIf
034770051216
034780051216      * Luogo spedizione giacenza
034790051219     c                   Clear                   wfax005
034800051219     c                   Clear                   wmail005
034810051216     c                   Eval      kspecod = '005'
034820051216     c     kFnspe        Chain     Fnspe01l
034830051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
034840051216     c                   Eval      *In14 = *On
034850051216     c                   Eval      wfax005 = spefax
034860051219     c     kFnsp2        Chain     Fnsp201l
034870051219     c                   If        %Found(Fnsp201l) and sp2flg = *Blanks
034880051219     c                   Eval      wmail005 = sp2est
034890051219     c                   EndIf
034900051216     c                   EndIf
034910060208
034920060208      * Luogo invio merce resa
034930060208     c                   Eval      kspecod = '300'
034940060208     c     kFnspe        Chain     Fnspe01l
034950060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
034960060208     c                   Eval      *In33 = *On
034970060208     c                   EndIf
034980051216
034990051216      * Luogo preavviso/avviso danno
035000051216     c                   Eval      kspecod = '006'
035010051216     c     kFnspe        Chain     Fnspe01l
035020051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
035030051216     c                   Eval      *In16 = *On
035040051216     c                   EndIf
035050051216
035060051216      * Luogo progetto di liquidazione
035070051216     c                   Eval      kspecod = '008'
035080051216     c     kFnspe        Chain     Fnspe01l
035090051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
035100051216     c                   Eval      *In17 = *On
035110051216     c                   EndIf
035120060208
035130060208      * Luogo invio orm estero
035140060208     c                   Eval      kspecod = '002'
035150060208     c     kFnspe        Chain     Fnspe01l
035160060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
035170060208     c                   Eval      *In35 = *On
035180060208     c                   EndIf
035190060208
035200060208      * Luogo invio doc.a dogana
035210060208     c                   Eval      kspecod = '200'
035220060208     c     kFnspe        Chain     Fnspe01l
035230060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
035240060208     c                   Eval      *In37 = *On
035250060208     c                   EndIf
035260051216
035270051216      * Decodifico i luoghi caricati
035280051216     c                   ExSr      Sr_Decluoghi
035290051216
035300051216     c                   EndSr
035310051216
035320051216      *------------------------------------------------------------------------*
035330051216      * DECODIFICO I LUOGHI CARICATI
035340051216      *------------------------------------------------------------------------*
035350051216     c     Sr_Decluoghi  BegSr
035360051216
035370051216     c                   Clear                   v4dl001
035380051216     c                   Clear                   v4cl001
035390051216     c                   Clear                   v5dl555
035400051216     c                   Clear                   v5cl555
035410051216     c                   Clear                   v5dl666
035420051216     c                   Clear                   v5cl666
035430051216     c                   Clear                   v6dl005
035440051216     c                   Clear                   v6cl005
035450060208     c                   Clear                   v6dl300
035460060208     c                   Clear                   v6cl300
035470051216     c                   Clear                   v7dl006
035480051216     c                   Clear                   v7cl006
035490051216     c                   Clear                   v7dl008
035500051216     c                   Clear                   v7cl008
035510060208     c                   Clear                   v7dl002
035520060208     c                   Clear                   v7cl002
035530060208     c                   Clear                   v7dl200
035540060208     c                   Clear                   v7cl200
035550051216
035560051216      * Decodifico luogo 001
035570071128     c                   clear                   ds4l
035580051216     c                   Eval      kcod = '4L'
035590051216     c                   Eval      kkey = '001'
035600051216     c     kTabel        Chain     Tabel00f
035610051216     c                   If        %Found(Tabel00f) and
035620051216     c                             tblflg = *Blanks
035630051216     c                   Eval      ds4l = tbluni
035640051216     c                   EndIf
035650051216      * Imposto il campo di descrizione
035660060203     c                   Eval      v4dl001 = '001 - ' + §4ldes
035670071128     c                   eval      $ufi001 = §4lufi
035680051216
035690051216      * Decodifico luogo 555
035700071128     c                   clear                   ds4l
035710051216     c                   Eval      kkey = '555'
035720051216     c     kTabel        Chain     Tabel00f
035730051216     c                   If        %Found(Tabel00f) and
035740051216     c                             tblflg = *Blanks
035750051216     c                   Eval      ds4l = tbluni
035760051216     c                   EndIf
035770051216      * Imposto il campo di descrizione
035780060203     c                   Eval      v5dl555 = '555 - ' + §4ldes
035790071128     c                   eval      $ufi555 = §4lufi
035800051216
035810051216      * Decodifico luogo 666
035820071128     c                   clear                   ds4l
035830051216     c                   Eval      kkey = '666'
035840051216     c     kTabel        Chain     Tabel00f
035850051216     c                   If        %Found(Tabel00f) and
035860051216     c                             tblflg = *Blanks
035870051216     c                   Eval      ds4l = tbluni
035880051216     c                   EndIf
035890051216      * Imposto il campo di descrizione
035900060203     c                   Eval      v5dl666 = '666 - ' + §4ldes
035910071128     c                   eval      $ufi666 = §4lufi
035920051216
035930051216      * Decodifico luogo 005
035940071128     c                   clear                   ds4l
035950051216     c                   Eval      kkey = '005'
035960051216     c     kTabel        Chain     Tabel00f
035970051216     c                   If        %Found(Tabel00f) and
035980051216     c                             tblflg = *Blanks
035990051216     c                   Eval      ds4l = tbluni
036000051216     c                   EndIf
036010051216      * Imposto il campo di descrizione
036020060203     c                   Eval      v6dl005 = '005 - ' + §4ldes
036030071128     c                   eval      $ufi005 = §4lufi
036040060208
036050060208      * Decodifico luogo 300
036060071128     c                   clear                   ds4l
036070060208     c                   Eval      kkey = '300'
036080060208     c     kTabel        Chain     Tabel00f
036090060208     c                   If        %Found(Tabel00f) and
036100060208     c                             tblflg = *Blanks
036110060208     c                   Eval      ds4l = tbluni
036120060208     c                   EndIf
036130060208      * Imposto il campo di descrizione
036140060208     c                   Eval      v6dl300 = '300 - ' + §4ldes
036150071128     c                   eval      $ufi300 = §4lufi
036160051216
036170051216      * Decodifico luogo 006
036180071128     c                   clear                   ds4l
036190051216     c                   Eval      kkey = '006'
036200051216     c     kTabel        Chain     Tabel00f
036210051216     c                   If        %Found(Tabel00f) and
036220051216     c                             tblflg = *Blanks
036230051216     c                   Eval      ds4l = tbluni
036240051216     c                   EndIf
036250051216      * Imposto il campo di descrizione
036260060203     c                   Eval      v7dl006 = '006 - ' + §4ldes
036270071128     c                   eval      $ufi006 = §4lufi
036280051216
036290051216      * Decodifico luogo 008
036300071128     c                   clear                   ds4l
036310051216     c                   Eval      kkey = '008'
036320051216     c     kTabel        Chain     Tabel00f
036330051216     c                   If        %Found(Tabel00f) and
036340051216     c                             tblflg = *Blanks
036350051216     c                   Eval      ds4l = tbluni
036360051216     c                   EndIf
036370051216      * Imposto il campo di descrizione
036380060203     c                   Eval      v7dl008 = '008 - ' + §4ldes
036390071128     c                   eval      $ufi008 = §4lufi
036400060208
036410060208      * Decodifico luogo 002
036420071128     c                   clear                   ds4l
036430060208     c                   Eval      kkey = '002'
036440060208     c     kTabel        Chain     Tabel00f
036450060208     c                   If        %Found(Tabel00f) and
036460060208     c                             tblflg = *Blanks
036470060208     c                   Eval      ds4l = tbluni
036480060208     c                   EndIf
036490060208      * Imposto il campo di descrizione
036500060208     c                   Eval      v7dl002 = '002 - ' + §4ldes
036510071128     c                   eval      $ufi002 = §4lufi
036520060208
036530060208      * Decodifico luogo 200
036540071128     c                   clear                   ds4l
036550060208     c                   Eval      kkey = '200'
036560060208     c     kTabel        Chain     Tabel00f
036570060208     c                   If        %Found(Tabel00f) and
036580060208     c                             tblflg = *Blanks
036590060208     c                   Eval      ds4l = tbluni
036600060208     c                   EndIf
036610060208      * Imposto il campo di descrizione
036620060208     c                   Eval      v7dl200 = '200 - ' + §4ldes
036630071128     c                   eval      $ufi200 = §4lufi
036640051216
036650051216     c                   EndSr
036660051216
036670051216      *------------------------------------------------------------------------*
036680051216      * CARICO LE NOTE PREVISTE
036690051216      *------------------------------------------------------------------------*
036700051216     c     Sr_Carnote    BegSr
036710051216
036720051216     c                   Eval      *In24 = *Off
036730051216     c                   Eval      *In26 = *Off
036740051216     c                   Eval      *In31 = *Off
036750051216     c                   Eval      *In21 = *Off
036760051216     c                   Eval      *In13 = *Off
036770051216     c                   Eval      *In15 = *Off
036780051216     c                   Eval      *In18 = *Off
036790060208     c                   Eval      *In34 = *Off
036800060208     c                   Eval      *In36 = *Off
036810090612     c                   Eval      *In40 = *Off
036820100729     c                   Eval      *In68 = *Off
036830051216
036840091112     c   70
036850091112     corn06              Eval      kntcapl = 'C'
036860100806     c   06
036870091112     cann70              Eval      kntcapl = 'T'
036880051216     c                   Movel(p)  dutkci        kntcnk1
036890091112     c   70
036900091112     corn06              Move      v1cksc        kntcnk1
036910100806     c   06
036920100806     cann70              Move      v1cnrv        kntcnk1
036930051216     c                   Clear                   kntcnk2
036940091119
036950091119      * Nota DC
036960091119     c                   clear                   v2ntcdc
036970091119     c                   Movel     'DC'          kntctnt
036980091119     c     kTfntc        Chain(n)  Tfntc01l
036990091119     c                   If        %Found(Tfntc01l)
037000091119     c                   Eval      v2ntcdc = ntcrnt
037010091119     c                   Eval      savntcdc = ntcrnt
037020091119     c                   EndIf
037030091119
037040140722      * Note 02/03
037050140722     c                   exsr      sr_CarNoteRN
037060051216
037070140722      * Note 05/06/T5/I5
037080140722     c                   exsr      sr_CarNoteRT
037090051216
037100140722      * Note 07/08/T7/I7
037110140722     c                   exsr      sr_CarNoteRL
037120051216
037130051216      * Nota 84
037140051216     c                   Movel     '84'          kntctnt
037150091119     c     kTfntc        Chain(n)  Tfntc01l
037160051216     c                   If        %Found(Tfntc01l)
037170051216     c                   Eval      *In21 = *On
037180051216     c                   EndIf
037190051216
037200051216      * Nota 85
037210051216     c                   Movel     '85'          kntctnt
037220091119     c     kTfntc        Chain(n)  Tfntc01l
037230051216     c                   If        %Found(Tfntc01l)
037240051216     c                   Eval      *In13 = *On
037250051216     c                   EndIf
037260051216
037270051216      * Nota 88
037280051216     c                   Movel     '88'          kntctnt
037290091119     c     kTfntc        Chain(n)  Tfntc01l
037300051216     c                   If        %Found(Tfntc01l)
037310051216     c                   Eval      *In15 = *On
037320051216     c                   EndIf
037330051216
037340051216      * Nota 87
037350051216     c                   Movel     '87'          kntctnt
037360091119     c     kTfntc        Chain(n)  Tfntc01l
037370051216     c                   If        %Found(Tfntc01l)
037380051216     c                   Eval      *In18 = *On
037390051216     c                   EndIf
037400060208
037410060208      * Nota 83
037420060208     c                   Movel     '83'          kntctnt
037430091119     c     kTfntc        Chain(n)  Tfntc01l
037440060208     c                   If        %Found(Tfntc01l)
037450060208     c                   Eval      *In34 = *On
037460060208     c                   EndIf
037470060208
037480060208      * Nota 86
037490060208     c                   Movel     '86'          kntctnt
037500091119     c     kTfntc        Chain(n)  Tfntc01l
037510060208     c                   If        %Found(Tfntc01l)
037520060208     c                   Eval      *In36 = *On
037530060208     c                   EndIf
037540090612
037550090612      * Nota 89
037560090612     c                   Movel     '89'          kntctnt
037570091119     c     kTfntc        Chain(n)  Tfntc01l
037580090612     c                   If        %Found(Tfntc01l)
037590090612     c                   Eval      *In40 = *On
037600090612     c                   EndIf
037610100729
037620100729      * Nota 82
037630100729     c                   Movel     '82'          kntctnt
037640100729     c     kTfntc        Chain(n)  Tfntc01l
037650100729     c                   If        %Found(Tfntc01l)
037660100729     c                   Eval      *In68 = *On
037670100729     c                   EndIf
037680051216
037690051216      * Decodifico le note caricate
037700051216     c                   ExSr      Sr_Decnote
037710051216
037720051216     c                   EndSr
037730140722
037740140722      *------------------------------------------------------------------------*
037750140722      * CARICO LE NOTE 02/03 (NEWS NOMINATIVO)
037760140722      *------------------------------------------------------------------------*
037770140722     c     Sr_CarNoteRN  BegSr
037780140722      *
037790140722      * Nota 02
037800140722     c                   movel     '02'          kNtcTnt
037810140722     c     kTfntc        chain(n)  TFNTC
037820140722     c                   if        %found(TFNTC01L)
037830140722     c                   eval      *in24 = *on
037840140722     c                   endif
037850140722      *
037860140722      * Nota 03
037870140722     c                   Movel     '03'          kNtcTnt
037880140722     c     kTfntc        chain(n)  TFNTC
037890140722     c                   if        %found(TFNTC01L)
037900140722     c                   eval      *in24 = *on
037910140722     c                   endif
037920140722      *
037930140722     c                   EndSr
037940140722
037950140722      *------------------------------------------------------------------------*
037960140722      * CARICO LE NOTE 05/06/T5/I5 (RESPONSABILE TRASPORTI)
037970140722      *------------------------------------------------------------------------*
037980140722     c     Sr_CarNoteRT  BegSr
037990140722      *
038000140722      * Nota 05
038010140722     c                   movel     '05'          kNtcTnt
038020140722     c     kTfntc        chain(n)  TFNTC
038030140722     c                   if        %found(TFNTC01L)
038040140722     c                   eval      *in26 = *On
038050140722     c                   endif
038060140722      *
038070140722      * Nota 06
038080140722     c                   movel     '06'          kNtcTnt
038090140722     c     kTfntc        chain(n)  TFNTC
038100140722     c                   if        %found(TFNTC01L)
038110140722     c                   eval      *in26 = *On
038120140722     c                   endif
038130140722      *
038140140722      * Nota T5
038150140722     c                   movel     'T5'          kNtcTnt
038160140722     c     kTfntc        chain(n)  TFNTC
038170140722     c                   if        %found(TFNTC01L)
038180140722     c                   eval      *in26 = *On
038190140722     c                   endif
038200140722      *
038210140722      * Nota I5
038220140722     c                   movel     'I5'          kNtcTnt
038230140722     c     kTfntc        chain(n)  TFNTC
038240140722     c                   if        %found(TFNTC01L)
038250140722     c                   eval      *in26 = *On
038260140722     c                   endif
038270140722      *
038280140722     c                   EndSr
038290140722
038300140722      *------------------------------------------------------------------------*
038310140722      * CARICO LE NOTE 07/08/T7/I7 (RESPONSABILE LOGISTICA)
038320140722      *------------------------------------------------------------------------*
038330140722     c     Sr_CarNoteRL  BegSr
038340140722      *
038350140722      * Nota 07
038360140722     c                   movel     '07'          kNtcTnt
038370140722     c     kTfntc        chain(n)  TFNTC
038380140722     c                   if        %found(TFNTC01L)
038390140722     c                   eval      *in31 = *On
038400140722     c                   endif
038410140722      *
038420140722      * Nota 08
038430140722     c                   movel     '08'          kNtcTnt
038440140722     c     kTfntc        chain(n)  TFNTC
038450140722     c                   if        %found(TFNTC01L)
038460140722     c                   eval      *in31 = *On
038470140722     c                   endif
038480140722      *
038490140722      * Nota T7
038500140722     c                   movel     'T7'          kNtcTnt
038510140722     c     kTfntc        chain(n)  TFNTC
038520140722     c                   if        %found(TFNTC01L)
038530140722     c                   eval      *in31 = *On
038540140722     c                   endif
038550140722      *
038560140722      * Nota I5
038570140722     c                   movel     'I7'          kNtcTnt
038580140722     c     kTfntc        chain(n)  TFNTC
038590140722     c                   if        %found(TFNTC01L)
038600140722     c                   eval      *in31 = *On
038610140722     c                   endif
038620140722      *
038630140722     c                   EndSr
038640051216
038650051216      *------------------------------------------------------------------------*
038660051216      * DECODIFICO LE NOTE CARICATE
038670051216      *------------------------------------------------------------------------*
038680051216     c     Sr_Decnote    BegSr
038690051216
038700051216     c                   Clear                   v4dnt84
038710051216     c                   Clear                   v4cnt84
038720051216     c                   Clear                   v5dnt85
038730051216     c                   Clear                   v5cnt85
038740090612     c                   Clear                   v5dnt89
038750090612     c                   Clear                   v5cnt89
038760100729     c                   Clear                   v5cnt82
038770051216     c                   Clear                   v6dnt88
038780051216     c                   Clear                   v6cnt88
038790051216     c                   Clear                   v7dnt87
038800051216     c                   Clear                   v7cnt87
038810060208     c                   Clear                   v7dnt83
038820060208     c                   Clear                   v7cnt83
038830060208     c                   Clear                   v7dnt86
038840060208     c                   Clear                   v7cnt86
038850140722
038860140722     c                   Eval      kcod = '1T'
038870051216
038880051216      * Decodifico nota 84
038890051216     c                   Eval      kkey = '84'
038900051216     c     kTabel        Chain     Tabel00f
038910051216     c                   If        %Found(Tabel00f) and
038920051216     c                             tblflg = *Blanks
038930051216     c                   Eval      ds1t = tbluni
038940051216     c                   EndIf
038950051216      * Imposto il campo di descrizione
038960060203     c                   Eval      v4dnt84 = ' 84 - ' + §1tdes
038970051216
038980051216      * Decodifico nota 85
038990051216     c                   Eval      kkey = '85'
039000051216     c     kTabel        Chain     Tabel00f
039010051216     c                   If        %Found(Tabel00f) and
039020051216     c                             tblflg = *Blanks
039030051216     c                   Eval      ds1t = tbluni
039040051216     c                   EndIf
039050051216      * Imposto il campo di descrizione
039060060203     c                   Eval      v5dnt85 = ' 85 - ' + §1tdes
039070090612
039080090612      * Decodifico nota 89
039090090612     c                   Eval      kkey = '89'
039100090612     c     kTabel        Chain     Tabel00f
039110090612     c                   If        %Found(Tabel00f) and
039120090612     c                             tblflg = *Blanks
039130090612     c                   Eval      ds1t = tbluni
039140090612     c                   EndIf
039150090612      * Imposto il campo di descrizione
039160090612     c                   Eval      v5dnt89 = ' 89 - ' + §1tdes
039170100729
039180100729      * Decodifico nota 82
039190100729     c                   Eval      kkey = '82'
039200100729     c     kTabel        Chain     Tabel00f
039210100729     c                   If        %Found(Tabel00f) and
039220100729     c                             tblflg = *Blanks
039230100729     c                   Eval      ds1t = tbluni
039240100729     c                   EndIf
039250100729      * Imposto il campo di descrizione
039260100729     c                   Eval      v5dnt82 = ' 82 - ' + §1tdes
039270051216
039280051216      * Decodifico nota 88
039290051216     c                   Eval      kkey = '88'
039300051216     c     kTabel        Chain     Tabel00f
039310051216     c                   If        %Found(Tabel00f) and
039320051216     c                             tblflg = *Blanks
039330051216     c                   Eval      ds1t = tbluni
039340051216     c                   EndIf
039350051216      * Imposto il campo di descrizione
039360060203     c                   Eval      v6dnt88 = ' 88 - ' + §1tdes
039370051216
039380051216      * Decodifico nota 87
039390051216     c                   Eval      kkey = '87'
039400051216     c     kTabel        Chain     Tabel00f
039410051216     c                   If        %Found(Tabel00f) and
039420051216     c                             tblflg = *Blanks
039430051216     c                   Eval      ds1t = tbluni
039440051216     c                   EndIf
039450051216      * Imposto il campo di descrizione
039460060203     c                   Eval      v7dnt87 = ' 87 - ' + §1tdes
039470060208
039480060208      * Decodifico nota 83
039490060208     c                   Eval      kkey = '83'
039500060208     c     kTabel        Chain     Tabel00f
039510060208     c                   If        %Found(Tabel00f) and
039520060208     c                             tblflg = *Blanks
039530060208     c                   Eval      ds1t = tbluni
039540060208     c                   EndIf
039550060208      * Imposto il campo di descrizione
039560060208     c                   Eval      v7dnt83 = ' 83 - ' + §1tdes
039570060208
039580060208      * Decodifico nota 86
039590060208     c                   Eval      kkey = '86'
039600060208     c     kTabel        Chain     Tabel00f
039610060208     c                   If        %Found(Tabel00f) and
039620060208     c                             tblflg = *Blanks
039630060208     c                   Eval      ds1t = tbluni
039640060208     c                   EndIf
039650060208      * Imposto il campo di descrizione
039660060208     c                   Eval      v7dnt86 = ' 86 - ' + §1tdes
039670051216
039680051216     c                   EndSr
039690051216
039700051216      *------------------------------------------------------------------------*
039710051216      *  Imposto descrizione tasti funzione
039720051216      *------------------------------------------------------------------------*
039730051216     c     Sr_Tastifun   BegSr
039740051216
039750051216      * Conta i caratteri riempiti dalle RigaTf1
039760051216     c                   Clear                   $z
039770051216      * Conta i caratteri riempiti dalle RigaTf2
039780051216     c                   Clear                   $k
039790051216      * Conta i caratteri riempiti dalle RigaTf3
039800051216     c                   Clear                   $j
039810051216      * Conta la posizione del campone da cui partire per impostare
039820051216      * la descrizione del tasto funzione
039830051216     c                   Clear                   $y
039840051216
039850051216      * Stringhe che contengono le descrizioni dei tasti funzione
039860051216     c                   Reset                   rigatf1
039870051216     c                   Reset                   rigatf2
039880051216     c                   Reset                   rigatf3
039890060203
039900060731      * F4 - Abilitazioni
039910120117       //?Se no angrafica provvisoria e no immissione
039920111007     c                   eval      *in78 = *off
039930110907     c                   if        not *in01 and not *in06
039940111117     c                   eval      *in78 = *on
039950060731     c                   reset                   $tf
039960060731     c                   movea     tf04          $tf
039970060731     c                   exsr      rie_$tf
039980060731     c                   endif
039990100407
040000100524       //?F22-Richiesta Contatto
040010100806       //?Se anagrafica clienti abilito F22=Richiesta Contatto
040020100407       //?e se utente di filiale
040030100419       //?e se pgm da menù
040040100507       //?e se NON immissione
040050100806     c                   IF        not *in06 and
040060100407     c                             not *in07 and
040070100419     c                             not *in09 and
040080100507     c                             not *in01 and
040090100419     c                             not $pgmrich
040100100407     c                   reset                   $tf
040110100524     c                   Movea     tf22          $tf
040120100407     c                   ExSr      Rie_$tf
040130100407     c                   ENDIF
040140100507
040150100507       //?F5 - Attività
040160120124       //?se utente di sede solo in interrogazione
040170120124       //?se non sono in anagrafica provvisoria,
040180100507       //?se non è convalida offerta e se ci sono attività sul cliente
040190100507       //?e non sono in immissione
040200100507     c                   eval      *in67 = *off
040210100806     c                   IF        not *in06 and
040220100507     c                             not *in07 and
040230120124     c                             not *in01
040240100507      * cerco se ci sono attività sul cliente
040250110516     c     V1Cksc        setll     TIATC07L
040260110516     c                   IF        %equal(TIATC07L)
040270100507     c                   eval      *in67 = *on
040280120124      * se utente di sede e NON è interrogazione spengo F5
040290120124     c                   IF        *in09 and not *in05
040300120124     c                   eval      *in67 = *off
040310120124     c                   ENDIF
040320120124     c                   IF        *in67
040330100507     c                   reset                   $tf
040340100507     c                   movea     tf05          $tf
040350100507     c                   exsr      rie_$tf
040360120124     c                   ENDIF
040370100507     c                   ENDIF
040380100507     c                   ENDIF
040390100507
040400100507      * F15 - Codici collegati
040410100507     c                   If        Not *In01 and Not *In06
040420100507     c                   Reset                   $tf
040430100507     c                   Movea     tf15          $tf
040440100507     c                   ExSr      Rie_$tf
040450100507     c                   EndIf
040460100507
040470060731      * F14 - Info Commerciali
040480100806     c                   If        Not *In06
040490060731     c                   Reset                   $tf
040500060731     c                   Movea     tf14          $tf
040510060731     c                   ExSr      Rie_$tf
040520090914     c                   endif
040530060526      * F19 - Variazioni
040540060526     c                   If        Not *In01 and Not *In06
040550060526     c                   Reset                   $tf
040560060526     c                   Movea     tf19          $tf
040570060526     c                   ExSr      Rie_$tf
040580060526     c                   endif
040590100407      * F20 - Interrogazione clienti potenziali
040600100407     c                   If        *In29 = *On and not *in65 and
040610100806     c                             not *in06
040620100407     c                   Reset                   $tf
040630100407     c                   Movea     tf20          $tf
040640100407     c                   ExSr      Rie_$tf
040650100407     c                   EndIf
040660101213
040670101213       //?F17-Dati Fatturazione
040680110322     c                   IF        not *in06
040690101213     c                   reset                   $tf
040700101213     c                   Movea     tf17          $tf
040710101213     c                   ExSr      Rie_$tf
040720101213     c                   ENDIF
040730110223
040740110223       //?F21-Blocco cliente
040750110223       //?Abilito se anagrafica clienti
040760110223       //?se utente di filiale
040770110223       //?se pgm da menù
040780110223       //?se NON immissione
040790110223       //?se codice non bloccato
040800110223     c                   IF        not *in06 and
040810110223     c                             not *in07 and
040820110223     c                             not *in09 and
040830110223     c                             not *in01 and
040840110223     c                             v2cabl = *blanks and
040850110223     c                             not $pgmrich
040860110223     c                   reset                   $tf
040870110223     c                   Movea     tf21          $tf
040880110223     c                   ExSr      Rie_$tf
040890110223     c                   ENDIF
040900110223
040910060203      * F12 - Ritorno
040920060206     c                   If        (Not *In29 and *In06) or
040930060206     c                             (Not *In29 and *In07) or
040940060206     c                             (Not *In06 and Not *In07)
040950060203     c                   Reset                   $tf
040960060203     c                   Movea     tf12          $tf
040970060203     c                   ExSr      Rie_$tf
040980060206     c                   EndIf
040990051216
041000051216      * Pulisco la stringa
041010051216     c                   Eval      $h = 1
041020051216     c                   For       $h by 1 to 62
041030051216     c                   If        rigatf1($h) = '#'
041040051216     c                   Clear                   rigatf1($h)
041050051216     c                   EndIf
041060051216     c                   EndFor
041070051216     c                   Eval      $h = 1
041080051216     c                   For       $h by 1 to 62
041090051216     c                   If        rigatf2($h) = '#'
041100051216     c                   Clear                   rigatf2($h)
041110051216     c                   EndIf
041120051216     c                   EndFor
041130051216     c                   Eval      $h = 1
041140051216     c                   For       $h by 1 to 62
041150051216     c                   If        rigatf3($h) = '#'
041160051216     c                   Clear                   rigatf3($h)
041170051216     c                   EndIf
041180051216     c                   EndFor
041190051216
041200051216      * Imposto la descrizione del tasto funzione F24 e segnalo
041210051216      * quante righe ho riempito e quale devo emettere
041220051216     c                   Select
041230051216
041240051216     c                   When      $k <> *Zeros and $y <> *Zeros
041250051216     c                   Eval      $loop = 3
041260051216     c                   Eval      vzfd02 = cf2413
041270051216
041280051216     c                   When      $k <> *Zeros and $y = *Zeros
041290051216     c                   Eval      $loop = 2
041300051216     c                   Eval      vzfd02 = cf2412
041310051216
041320051216     c                   Other
041330051216
041340051216     c                   Eval      $loop = 1
041350051216     c                   Clear                   vzfd02
041360051216     c                   EndSl
041370051216
041380051216      * Prima riga di tasti funzione
041390051216     c                   Movea     rigatf1       vzfd01
041400051216     c                   Eval      $rig = 1
041410051216
041420051216     c                   EndSr
041430051216
041440051216      *------------------------------------------------------------------------*
041450051216      *  IMPOSTO LE STRINGHE CON I CAMPI FUNZIONE COMPATTATI
041460051216      *------------------------------------------------------------------------*
041470051216     c     Rie_$tf       BegSr
041480051216      *
041490051216     c                   Eval      $x = 1
041500051216     c     '#'           Lookup    $tf($x)                                30
041510051216     c                   Add       $x            $z
041520051216     c                   If        $Z <= 63
041530051216     c                   Eval      $j = $Z - $x + 1
041540051216     c                   Movea     $tf           rigatf1($j)
041550051216     c                   Else
041560051216     c                   Add       $x            $k
041570051216     c                   If        $k <= 63
041580051216     c                   Eval      $j = $K - $x + 1
041590051216     c                   Movea     $tf           rigatf2($j)
041600051216     c                   Else
041610051216     c                   Add       $x            $y
041620051216     c                   If        $y <= 63
041630051216     c                   Eval      $j = $y - $x + 1
041640051216     c                   Movea     $tf           rigatf3($j)
041650051216     c                   EndIf
041660051216     c                   EndIf
041670051216     c                   EndIf
041680051216
041690051216     c                   EndSr
041700051216
041710051108      *------------------------------------------------------------------------*
041720051130      * CONTROLLO SECONDA VIDEATA - DATI ANAGRAFICI/BLOCCO
041730051108      *------------------------------------------------------------------------*
041740051108     c     Sr_Ctrd02     BegSr
041750051117
041760051117      * Pulisco i campi del posizionamento cursore
041770051117     c                   Clear                   h2riga
041780051117     c                   Clear                   h2colo
041790051125
041800051118      * Imposto gli ulti 4 byte del codice cliente
041810051118     c                   Move      v1cksc        wksc04
041820051117
041830051117      * RAGIONE SOCIALE
041840051117      * --> Obbligatoria
041850051117     c                   If        v2crag = *Blanks
041860051117     c                   Eval      *In28 = *On
041870051130     c                   Eval      h2riga = 07
041880051117     c                   Eval      h2colo = 28
041890051117     c                   Eval      v2cmsg = msg(11)
041900051117     c                   Leavesr
041910051117     c                   EndIf
041920051117
041930051117      * CODICE POTENZIALE
041940051125     c                   ExSr      Sr_Ctrcpo
041950051125     c   28              Leavesr
041960051118
041970051118      * --> Caratteri non validi
041980051212     c                   Eval      xx = 1
041990051212     c                   For       xx by 1 to 10
042000051212     c                   If        car(xx) <> *Blanks
042010051212     c                   Eval      wpos = %scan(car(xx):v2crag)
042020051212     c                   If        wpos > 0
042030051212     c                   Eval      *In28 = *On
042040051212     c                   Eval      h2riga = 07
042050051212     c                   Eval      h2colo = 28
042060051212     c                   Eval      v2cmsg = msg(32)
042070051212     c                   Leavesr
042080051212     c                   EndIf
042090051212     c                   EndIf
042100051212     c                   EndFor
042110051118
042120051117      * INDIRIZZO
042130051118      * --> Controllo se cliente
042140051118     c                   If        v1ckcc = 151
042150051118     c                   Clear                   fnlv13ds
042160051118     c                   Eval      i14rsc = v2crag
042170051118     c                   Eval      i14ind = v2cvia
042180051118     c                   Eval      e14cap = v2ccae
042190051118     c                   Eval      e14loc = v2ccit
042200051118     c                   Eval      e14prv = v2cprv
042210051118     c                   Eval      e14nar = v2csta
042220051118     c                   ExSr      Sr_Ctrind
042230051118     c                   Eval      v2ccae = e14cap
042240051118     c                   Eval      v2ccit = e14loc
042250051118     c                   Eval      v2cprv = e14prv
042260051118     c                   Eval      v2csta = e14nar
042270051118     c   28              Leavesr
042280051118     c                   EndIf
042290051117
042300051117      * TELEFONO
042310051117      * --> Controllo (forzabile)
042320051124      *     Solo se variato il campo a video quindi al primo errore se non
042330051124      *     vario il campo non controllo più
042340051118     c                   If        v2ctel <> wtel and v2ctel <> *Blanks
042350051118     c                   Clear                   trul42ds
042360051118     c                   Eval      d42fax = v2ctel
042370051118     c                   Call      'TRUL42R'
042380051118     c                   Parm                    trul42ds
042390051118     c                   If        d42err = '1'
042400051118     c                   Eval      *In28 = *On
042410051202     c                   Eval      h2riga = 11
042420051118     c                   Eval      h2colo = 28
042430051118     c                   Eval      v2cmsg = d42msg
042440051118     c                   Leavesr
042450051118     c                   Eval      wtel = v2ctel
042460051117     c                   EndIf
042470051118     c                   EndIf
042480051118
042490051117      * FAX
042500051125      * --> Controllo (forzabile)
042510051125      *     Solo se variato il campo a video quindi al primo errore se non
042520051125      *     vario il campo non controllo più
042530051118     c                   If        v2ctlf <> wtlf and v2ctlf <> *Blanks
042540051118     c                   Clear                   trul42ds
042550051118     c                   Eval      d42fax = v2ctlf
042560051118     c                   Call      'TRUL42R'
042570051118     c                   Parm                    trul42ds
042580051118     c                   If        d42err = '1'
042590051118     c                   Eval      *In28 = *On
042600051202     c                   Eval      h2riga = 11
042610051118     c                   Eval      h2colo = 52
042620051118     c                   Eval      v2cmsg = d42msg
042630051118     c                   Leavesr
042640051118     c                   Eval      wtlf = v2ctlf
042650051118     c                   EndIf
042660051118     c                   EndIf
042670051118
042680051118      * CODICE FISCALE
042690051118     c                   ExSr      Sr_Ctrcdf
042700051118     c   28              Leavesr
042710051118
042720051118      * PARTITA IVA + STATO
042730051118     c                   ExSr      Sr_Ctriva
042740051118     c   28              Leavesr
042741171213      //se dal controllo torna che cliente già presente con stessa p.IVA
042742171213      //ma in sofferenza esco
042743171213       IF  ClienteSofferenza;
042744171213         leavesr;
042745171213       ENDIF;
042750051122
042760051122      * BLOCCO CLIENTE
042770051122     c                   ExSr      Sr_Ctrblc
042780051122     c   28              Leavesr
042790090616      * sollecito
042800090616     c                   ExSr      Sr_Ctrsol
042810090617     c   28              Leavesr
042820051108
042830051108     c                   EndSr
042840051130
042850051130      *------------------------------------------------------------------------*
042860051130      * CONTROLLO TERZA VIDEATA - DATI COMMERCIALI
042870051130      *------------------------------------------------------------------------*
042880051130     c     Sr_Ctrd03     BegSr
042890051130
042900051130      * Pulisco i campi del posizionamento cursore
042910051130     c                   Clear                   h3riga
042920051130     c                   Clear                   h3colo
042930051130
042940051130      * CODICE COMMERCIALE
042950051130     c                   ExSr      Sr_Ctrage
042960051130     c   28              Leavesr
042970051130
042980051130      * CODICE IMPORTANZA CLIENTE
042990051130     c                   ExSr      Sr_Ctrclv
043000051130     c   28              Leavesr
043010051130
043020051130      * CODICE ISTAT
043030051130     c                   ExSr      Sr_Ctritc
043040051130     c   28              Leavesr
043050051207
043060140722      * NOTE   02/03 - NEWS NOMINATIVO
043070140722     c                   eval      wRGR = 'RN'
043080140722     c                   exsr      sr_CtrNote
043090140722     c   28
043100140722     cor 90              leavesr
043110051207
043120140722      * NOTE   05/06/T5/I5 - RESPONSABILE TRASPORTI
043130140722     c                   eval      wRGR = 'RT'
043140140722     c                   exsr      sr_CtrNote
043150140722     c   28
043160140722     cor 90              leavesr
043170051207
043180140722      * NOTE   07/08/T7/I7 - RESPONSABILE LOGISTICA
043190140722     c                   eval      wRGR = 'RL'
043200140722     c                   exsr      sr_CtrNote
043210140722     c   28
043220140722     cor 90              leavesr
043230051130
043240051130     c                   EndSr
043250051108
043260051108      *------------------------------------------------------------------------*
043270051130      * CONTROLLO QUARTA VIDEATA - DATI FATTURAZIONE
043280051108      *------------------------------------------------------------------------*
043290051130     c     Sr_Ctrd04     BegSr
043300051125
043310051125      * Pulisco i campi del posizionamento cursore
043320051130     c                   Clear                   h4riga
043330051130     c                   Clear                   h4colo
043340051125
043350051125      * CODICE SOTTOCONTO FATTURAZIONE
043360051125     c                   ExSr      Sr_Ctrscf
043370100127     c   90
043380100127     cor 28              Leavesr
043390051128
043400051128      * FLAG FATTURA UNIFICATA
043410051128     c                   ExSr      Sr_Ctruni
043420051128     c   28              Leavesr
043430051125
043440051125      * TIPO FATTURA
043450051125     c                   ExSr      Sr_Ctrtft
043460051125     c   28              Leavesr
043470051125
043480051125      * FREQUENZA FATTURA
043490051125     c                   ExSr      Sr_Ctrfft
043500051125     c   28              Leavesr
043510051125
043520051125      * TIPO DATA FATTURA
043530051125     c                   ExSr      Sr_Ctrtdf
043540051125     c   28              Leavesr
043550051128
043560051128      * SPESE INVIO FATTURA
043570051128     c                   ExSr      Sr_Ctrsin
043580051128     c   28              Leavesr
043590100728
043600100728      * CODICE PAGAMENTO FATTURA
043610100728     c                   ExSr      Sr_Ctrcdp
043620100728     c   28              Leavesr
043630100728
043640100728      * BANCA APPOGGIO
043650100728     c                   ExSr      Sr_Ctrbna
043660100728     c   28              Leavesr
043670100728
043680100728      * ABI/CAB
043690100728     c                   ExSr      Sr_Ctrabicabf
043700100728     c   28              Leavesr
043710051128
043720051206      * LUOGO 001 - INVIO FATTURA
043730051212      * Se non sono in visite/offerte
043740051128     c                   If        Not *In06
043750091112      * o se trattativa da cliente
043760091112     c                             or *in70
043770051128     c                   ExSr      Sr_Ctrl001
043780051128     c   28              Leavesr
043790051128     c                   EndIf
043800051129
043810051206      * NOTA   84 - INVIO FATTURA
043820051129     c                   ExSr      Sr_Ctrnt84
043830051129     c   28              Leavesr
043840051108
043850051108     c                   EndSr
043860051216
043870051108      *------------------------------------------------------------------------*
043880051130      * CONTROLLO QUINTA VIDEATA - CONDIZIONI PAGAMENTO/PAG. CONTRASSEGNI
043890051108      *------------------------------------------------------------------------*
043900051130     c     Sr_Ctrd05     BegSr
043910051129
043920051129      * Pulisco i campi del posizionamento cursore
043930051130     c                   Clear                   h5riga
043940051130     c                   Clear                   h5colo
043950100728     c                   clear                   wbanca
043960100728     c                   clear                   wagenzia
043970100728
043980100728      * BLOCCO PAGAMENTI
043990130322      * Se impostato SI dall'utente
044000130322     c                   IF        not *in81 and V5Cblp = 'SI'
044010130325     c                   eval      wbloc = 'B'
044020130322     c                   ENDIF
044030130322      * Se impostato NO dall'utente
044040130322     c                   IF        not *in81 and V5Cblp = 'NO'
044050130325     c                   eval      wbloc = '0'
044060130322     c                   ENDIF
044070150424      /free
044080150424       //?Come prima cosa devo avvisare se anche solo 1 dei 3 pagamenti
044090150424       //?non è un pagamento tramite Bonifico
044100150424         exsr CtrPagamenti;
044110150424         IF  *in28;
044120150424           leavesr;
044130150424         ENDIF;
044140150424      /end-free
044150051108
044160051130      * CODICE PAGAMENTO CONTRASSEGNI
044170051130     c                   ExSr      Sr_Ctrfpc
044180060215     c                   If        *In28
044190060215     c                   Eval      *In22 = savin22
044200060215     c                   Eval      *In23 = savin23
044210060215     c                   Leavesr
044220060215     c                   EndIf
044230100729     c                   IF        *in90 and $Win
044240100729     c                   leavesr
044250100729     c                   ENDIF
044260150424
044270150424      /free
044280150427       //?Memorizzo se pagamento tramite Bonifico
044290150424         IF  §4Ucod = 'B';
044300150424           BonificoCA = *on;
044310150424         ELSE;
044320150424           BonificoCA = *off;
044330150424         ENDIF;
044340150427       //?Memorizzo se pagamento Estero
044350150427         IF  §4Utip = 'E';
044360150427           PagEsteroCA = *on;
044370150427         ELSE;
044380150427           PagEsteroCA = *off;
044390150427         ENDIF;
044400150427       //?Memorizzo il codice pagamento
044410150427         CodPagCA = V5Cfpc;
044420150424      /end-free
044430150424
044440080115      * CODICE IBAN
044450080115      * solo se richieste obbligatorie le coordinate
044460080115     c                   if        not *in22 and not *in23
044470100729     c                   eval      wiban = v5ciban
044480080115     c                   exsr      sr_ctriban
044490100729     c                   if        *in28
044500100729     c                   eval      h5riga = 10
044510100804     c                   eval      h5colo = 29
044520100729     c                   leavesr
044530100729     c                   endif
044540100729     c                   eval      v5cbab = wdesbanca
044550080115     c                   endif
044560100729
044570100729      /free
044580100729       //?PAGAMENTO DANNI
044590100729         exsr CtrPagDN;
044600100729         IF  *in28;
044610100729           *in71 = savin71;
044620100729           *in72 = savin72;
044630100729           leavesr;
044640100729         ENDIF;
044650150427       //?Memorizzo se pagamento tramite Bonifico
044660150424         IF  §4Ucod = 'B';
044670150424           BonificoDN = *on;
044680150424         ELSE;
044690150424           BonificoDN = *off;
044700150424         ENDIF;
044710150427       //?Memorizzo se pagamento Estero
044720150427         IF  §4Utip = 'E';
044730150427           PagEsteroDN = *on;
044740150427         ELSE;
044750150427           PagEsteroDN = *off;
044760150427         ENDIF;
044770150427       //?Memorizzo il codice pagamento
044780150427         CodPagDN = V5tpdn;
044790100729       //?se window per invio dati alla sede ritorno ad emettere la videata
044800100729       //?senza andare avanti con i controlli
044810100729         IF  $windn and *in90;
044820100729           leavesr;
044830100729         ENDIF;
044840100729       //?controllo le coordinate bancarie
044850100730         IF  not *in71 and not *in72 and d§PAGctr = 'SI';
044860100729           wiban = v5ibandn;
044870100729           exsr sr_ctriban;
044880100729           IF  *in28;
044890100729             h5riga = 13;
044900100804             h5colo = 29;
044910100729             leavesr;
044920100729           ENDIF;
044930100729           v5babdn = wDesBanca;
044940100729         ENDIF;
044950100730       //?se pagamento tramite bonifico deve essere inserito la nota 82-mail
044960100730       //?bonifici danni
044970100730         IF §4ucod = 'B' and not *in68 and v5cnt82 = *blanks;
044980100730           *in28 = *on;
044990100730           v5cmsg = msg(86);
045000100730           h5riga = 21;
045010100730           h5colo = 71;
045020100730           leavesr;
045030100730         ENDIF;
045040100729
045050100729       //?PAGAMENTO NOTE ACCREDITO
045060100729         exsr CtrPagNA;
045070100729         IF  *in28;
045080100729           *in73 = savin73;
045090100729           *in74 = savin74;
045100100729           leavesr;
045110100729         ENDIF;
045120150427       //?Memorizzo se pagamento tramite Bonifico
045130150424         IF  §4Ucod = 'B';
045140150424           BonificoNA = *on;
045150150424         ELSE;
045160150424           BonificoNA = *off;
045170150424         ENDIF;
045180150427       //?Memorizzo se pagamento Estero
045190150427         IF  §4Utip = 'E';
045200150427           PagEsteroNA = *on;
045210150427         ELSE;
045220150427           PagEsteroNA = *off;
045230150427         ENDIF;
045240150427       //?Memorizzo il codice pagamento
045250150427         CodPagNA = V5tpna;
045260150424      /end-free
045270100729       //?se window per invio dati alla sede ritorno ad emettere la videata
045280100729       //?senza andare avanti con i controlli
045290100729         IF  $winna and *in90;
045300100729           leavesr;
045310100729         ENDIF;
045320100729       //?controllo le coordinate bancarie
045330100804         IF  not *in73 and not *in74 and d§PAGctr = 'SI';
045340100729           wiban = v5ibanna;
045350100729           exsr sr_ctriban;
045360100729           IF  *in28;
045370100729             h5riga = 16;
045380100804             h5colo = 29;
045390100729             leavesr;
045400100729           ENDIF;
045410100729           v5babna = wDesBanca;
045420100729         ENDIF;
045430100729      /end-free
045440051206
045450051212      * Se non sono in visite/offerte
045460051212if  1c                   If        Not *In06
045470091112      * o se trattativa da cliente
045480091112     c                             or *in70
045490051206      * LUOGO 555 - INTESTAZIONE PAGAMENTO C/ASSEGNI
045500051206     c                   ExSr      Sr_Ctrl555
045510051206     c   28              Leavesr
045520051206
045530051206      * LUOGO 666 - LUOGO INVIO BONIFICI C/ASSEGNI
045540051206     c                   ExSr      Sr_Ctrl666
045550051206     c   28              Leavesr
045560051212    1c                   EndIf
045570051206
045580051206      * NOTA   85 - E-MAIL BONIFICI C/ASSEGNI
045590051206     c                   ExSr      Sr_Ctrnt85
045600051206     c   28              Leavesr
045610090616
045620090616      * NOTA   89 - E-MAIL RESPONSABILE AMMINISTRATIVO
045630090616     c                   ExSr      Sr_Ctrnt89
045640090616     c   28              Leavesr
045650100729
045660100729      * NOTA   82 - E-MAIL BONIFICO DANNI
045670150424     c                   ExSr      Sr_Ctrnt82
045680100730     c   90              goto      emi05
045690100729     c   28              Leavesr
045700051130
045710051108     c                   EndSr
045720051108
045730051108      *------------------------------------------------------------------------*
045740051130      * CONTROLLO SESTA VIDEATA - GESTIONE GIACENZE
045750051108      *------------------------------------------------------------------------*
045760051130     c     Sr_Ctrd06     BegSr
045770051206
045780051206      * Pulisco i campi del posizionamento cursore
045790051206     c                   Clear                   h6riga
045800051206     c                   Clear                   h6colo
045810060216
045820060216      * LUOGO 005 - SPEDIZIONE GIACENZA
045830060216     c                   If        Not *In06
045840091112     c                             or *in70
045850060216     c                   ExSr      Sr_Ctrl005
045860060216     c   28              Leavesr
045870060216     c                   EndIf
045880060216
045890060216      * NOTA   88 - E-MAIL GIACENZE
045900060216     c                   ExSr      Sr_Ctrnt88
045910060216     c   28              Leavesr
045920060216
045930060216      * LUOGO 300 - MERCE RESA
045940060216     c                   If        Not *In06
045950091112     c                             or *in70
045960060216     c                   ExSr      Sr_Ctrl300
045970060216     c   28              Leavesr
045980060216     c                   EndIf
045990051206
046000051206      * TIPO COMUNICAZIONE AL MITTENTE
046010051206     c                   ExSr      Sr_Ctrtcm
046020051206     c   28              Leavesr
046030051206
046040051206      * TIPO COMUNICAZIONE FINE GIACENZA
046050051206     c                   ExSr      Sr_Ctrtcf
046060051206     c   28              Leavesr
046070051206
046080051206      * APERTURA AUTOMATICA GIACENZA
046090051206     c                   ExSr      Sr_Ctrapg
046100051206     c   28              Leavesr
046110051206
046120051206      * DISPOSIZIONE MITTENTE IN ARRIVO
046130051206     c                   ExSr      Sr_Ctrdmg
046140051206     c   28              Leavesr
046150060801
046160060801      * CHIUSURA AUTOMATICA GIACENZA
046170060801     c                   ExSr      Sr_Ctrcag
046180060801     c   28              Leavesr
046190051108
046200051108     c                   EndSr
046210051108
046220051108      *------------------------------------------------------------------------*
046230051130      * CONTROLLO SETTIMA VIDEATA - GESTIONE DANNI/TASSAZIONE P.A./STOP
046240051108      *------------------------------------------------------------------------*
046250051130     c     Sr_Ctrd07     BegSr
046260051213
046270051213      * Pulisco i campi del posizionamento cursore
046280051213     c                   Clear                   h7riga
046290051213     c                   Clear                   h7colo
046300150728
046310150728      /free
046320150728       //?Come prima cosa devo avvisare se manca la mail per invio
046330150728       //?progetto di liquidazione
046340150728       //?NON lo faccio se anagrafica provvisoria
046350150728         IF  not *in06;
046360150728           exsr CtrMailDanni;
046370150728           IF  *in28;
046380150728             leavesr;
046390150728           ENDIF;
046400150728         ENDIF;
046410150728      /end-free
046420051213
046430051213      * LUOGO 006 - LUOGO PREAVVISO/AVVISO DANNO
046440051213     c                   If        Not *In06
046450091112     c                             or *in70
046460051213     c                   ExSr      Sr_Ctrl006
046470051213     c   28              Leavesr
046480051213     c                   EndIf
046490051213
046500051213      * NOTA   87 - E-MAIL DANNI
046510051213     c                   ExSr      Sr_Ctrnt87
046520051213     c   28              Leavesr
046530051213
046540051213      * LUOGO 008 - PROGETTO DI LIQUIDAZIONE
046550051213     c                   If        Not *In06
046560091112     c                             or *in70
046570051213     c                   ExSr      Sr_Ctrl008
046580051213     c   28              Leavesr
046590051213     c                   EndIf
046600051213
046610051213      * NR. STOP RITIRO
046620051213     c                   ExSr      Sr_Ctrstp
046630051213     c   28              Leavesr
046640060208
046650060208      * NOTA   83 - MAIL ORM ESTERO
046660060208     c                   ExSr      Sr_Ctrnt83
046670060208     c   28              Leavesr
046680060208
046690060208      * LUOGO 002 - INVIO ORM ESTERO
046700060208     c                   If        Not *In06
046710091112     c                             or *in70
046720060208     c                   ExSr      Sr_Ctrl002
046730060208     c   28              Leavesr
046740060208     c                   EndIf
046750060208
046760060208      * NOTA   86 - MANIFEST
046770060208     c                   ExSr      Sr_Ctrnt86
046780060208     c   28              Leavesr
046790060208
046800060208      * LUOGO 200 - DOC. DOGANA
046810060208     c                   If        Not *In06
046820091112     c                             or *in70
046830060208     c                   ExSr      Sr_Ctrl200
046840060208     c   28              Leavesr
046850060208     c                   EndIf
046860051108
046870051108     c                   EndSr
046880051216
046890051216      *------------------------------------------------------------------------*
046900051216      * TASTO FUNZIONE F02 - CONTATTI
046910051216      *------------------------------------------------------------------------*
046920051216     c     Sr_F02        BegSr
046930051216
046940051216     c                   Clear                   tnta12ds
046950051216     c                   Eval      ta12ric = wrich
046960091112     c   70
046970091112     corn06              Eval      ta12apl = 'C'
046980091112     c   70
046990091112     corn06              Eval      ta12ksc = v1cksc
047000100806     c   06
047010091112     cann70              Eval      ta12apl = 'T'
047020100806     c   06
047030100806     cann70              Eval      ta12nrv = v1cnrv
047040060217     c                   Eval      ta12rag = wrag
047050051216     c                   Eval      ta12tnt = wtnt
047060060216     c                   Eval      ta12gcli = '1'
047070060216     c   01              Eval      ta12icli = '1'
047080060307     c   01              Move      v3cage        ta12cmm
047090060227     c                   If        %Parms > 1 and
047100060227     c                             Not *In01 and Not *In05 and ta60icli = '1'
047110060221     c                   Eval      ta12icli = '1'
047120060307     c                   Move      v3cage        ta12cmm
047130060221     c                   EndIf
047140051216     c                   Call      'TNTA12R'
047150051216     c                   Parm                    kpjba
047160051216     c                   Parm                    tnta12ds
047170051216
047180051216     c                   ExSr      Sr_Carnote
047190051216
047200051216     c                   EndSr
047210060217
047220060217      *------------------------------------------------------------------------*
047230060217      * TASTO FUNZIONE F03 - FINE LAVORO
047240060217      *------------------------------------------------------------------------*
047250060217     c     Sr_F03        BegSr
047260060217
047270060217     c                   Eval      *In80 = *Off
047280060217
047290060217      * Se immissione emetto videata per chiedere conferma di uscita dal
047300060217      * pgm informando l'utente che i dati andranno persi
047310060217     c                   If        *In01
047320060217     c                   Eval      w03cok = 'NO'
047330060217     c                   Exfmt     ta60w03
047340060217      * Se ok per uscire dal pgm vado a fine
047350060217if  1c                   If        w03cok = 'SI'
047360060217     c                   Goto      fine
047370060217     c                   Else
047380060217     c                   Eval      *In80 = *On
047390060217     c                   Leavesr
047400060217    1c                   EndIf
047410060217     c                   EndIf
047420110302      * Imposto F3 fine se pgm richiamato
047430110302     c                   IF        $Pgmrich
047440110301     c                   eval      TA60f03 = '1'
047450110302     c                   ENDIF
047460110301
047470060217      * Se arrivo qua non è immissione e quindi vado a fine
047480060217     c                   Goto      fine
047490060217
047500060217     c                   EndSr
047510060731
047520060731      *------------------------------------------------------------------------*
047530060731      * TASTO FUNZIONE F04 - ABILITAZIONI
047540060731      *------------------------------------------------------------------------*
047550060731     c     sr_f04        begsr
047560060731
047570060731     c                   clear                   tnta61ds
047580060731     c                   eval      ta61tla = 'V'
047590060731     c                   eval      ta61ksc = v1cksc
047600060731     c                   call      'TNTA61R'
047610060731     c                   parm                    kpjba
047620060731     c                   parm                    tnta61ds
047630060731
047640060731     c                   endsr
047650100507
047660100507      /free
047670100507       //--------------------------------------------------------------
047680100507       //?Tasto Funzione F5 - Attività.
047690100507       //--------------------------------------------------------------
047700100507       BEGSR sr_F05;
047710100507
047720100507         clear trmk21ds;
047730100507         I21mod = 'I';
047740100507         I21ksc = V1Cksc;
047750120124         I21rsc = V2Crag;
047760100507         kpjbu = trmk21ds;
047770100507
047780100507         trmk21r (kpjba);
047790100507
047800100507       ENDSR;
047810100507      /end-free
047820060731
047830051216      *------------------------------------------------------------------------*
047840051216      * TASTO FUNZIONE F07 - CLIENTE UNIFICANTE
047850051216      *------------------------------------------------------------------------*
047860051216     c     Sr_F07        BegSr
047870051216
047880051216     c                   Clear                   tibs12ds
047890051216     c  n05              Eval      k12op0 = 'P02'
047900051216     c   05              Eval      k12op0 = 'P05'
047910051216     c                   Eval      k12op1 = 'O09'
047920051216     c                   Eval      k12f03 = '0'
047930051216     c                   Eval      k12f12 = '0'
047940051216     c                   Eval      k12err = '0'
047950051216     c                   Move      v1cksc        k12cop
047960051216     c                   Eval      kpjbu = tibs12ds
047970051216     c                   Call      'TIBS12R'
047980051216     c                   Parm                    kpjba
047990051216
048000051216     c                   EndSr
048010100407
048020100407      /free
048030100407       //--------------------------------------------------------------
048040100524       //?Tasto Funzione F22 - Richiesta Contatto.
048050100407       //--------------------------------------------------------------
048060100524       BEGSR sr_F22;
048070110223
048080110223       //?Non posso fare una nuova attività su cliente con potenziale in
048090110223       //?categoria PERSO, devo partire dal potenziale
048100110223         IF  V2Hfls = 'P';
048110110223         *in28 = *on;
048120110223         V2Cmsg = 'Nuova attività non possibile per Potenziale PERSO';
048130110223         leavesr;
048140110223         ENDIF;
048150100407
048160100407         clear trmk17ds;
048170100407         IMK17patt = 'S';
048180100407         IMK17att  = 'T';
048190100407         IMK17pcau = 'S';
048200100407         IMK17cau  = '20';
048210100407         IMK17cpo  = v2ccpo;
048220100407         IMK17ksc  = v1cksc;
048230100407         IF  v3cage > *zeros;
048240100407           IMK17cmm  = %dec(v3cage:7:0);
048250100407         ENDIF;
048260100407
048270100407         trmk17r (kpjba : TRMK17DS);
048280100507
048290100507       //?Ricarico i tasti funzione, nel caso venga inserita l'attività sul
048300100507       //?cliente così faccio vedere il tasto F5-Attività
048310100507         exsr Sr_Tastifun;
048320100407
048330100407       ENDSR;
048340101213
048350101213       //--------------------------------------------------------------
048360101213       //?Tasto Funzione F17 - Dati Fatturazione.
048370101213       //--------------------------------------------------------------
048380101213       BEGSR sr_F17;
048390101213
048400101213         clear trul57ds;
048410101213         D57ikscb = V1Cksc;
048420101213         IF  %check(digits:V4Cscf) > 0;
048430101213           D57ikscf = 0;
048440101213         ELSE;
048450101213           D57ikscf = %dec(V4Cscf:7:0);
048460101213         ENDIF;
048470101213         D57iunib = V4Cuni;
048480101213         D57itftb = V4Ctft;
048490101213         D57ifftb = V4Cfft;
048500101213         D57idftb = V4Ctdf;
048510101213         D57ipgm  = 'TNTA60R';
048520101213         D57isifb = V4Csin;
048530101213         D57icdpb = V4Ccdp;
048540101213         D57iabib = V4Cabi;
048550101213         D57icabb = V4Ccab;
048560101213
048570101213         trul57r (kpjba : TRUL57DS);
048580101213
048590101213       ENDSR;
048600110223
048610110223       //--------------------------------------------------------------
048620110223       //?Tasto Funzione F21 - Blocco Cliente.
048630110223       //--------------------------------------------------------------
048640110223       BEGSR sr_F21;
048650110223
048660110223         clear trmk17ds;
048670110223         IMK17patt = 'S';
048680110223         IMK17att  = 'T';
048690110223         IMK17pcau = 'S';
048700110223         IMK17cau  = '71';
048710110223         IMK17cpo  = v2ccpo;
048720110223         IMK17ksc  = v1cksc;
048730110223         IF  v3cage > *zeros;
048740110223           IMK17cmm  = %dec(v3cage:7:0);
048750110223         ENDIF;
048760110223
048770110223         trmk17r (kpjba : TRMK17DS);
048780110223
048790110223       //?Ricarico i tasti funzione
048800110223         exsr Sr_Tastifun;
048810110310
048820110223
048830110223       ENDSR;
048840110223
048850100407      /end-free
048860051216
048870051216      *------------------------------------------------------------------------*
048880051216      * TASTO FUNZIONE F11 - LUOGHI
048890051216      *------------------------------------------------------------------------*
048900051216     c     Sr_F11        BegSr
048910051216
048920051216     c                   Clear                   parmf11
048930051216     c  n05              Eval      paropz = '2'
048940051216     c   05              Eval      paropz = '5'
048950051216     c                   Move      v1cksc        parcli
048960060216     c                   Eval      partnta60 = '1'
048970060216     c   01              Eval      parimta60 = '1'
048980051216     c                   Eval      kpjbu = parmf11
048990051216     c                   Call      'TNTA80R'
049000051216     c                   Parm                    kpjba
049010051216
049020051216     c                   ExSr      Sr_Carluoghi
049030051216
049040051216     c                   EndSr
049050051216
049060051216      *------------------------------------------------------------------------*
049070051216      * TASTO FUNZIONE F14 - INFO COMMERCIALI
049080051216      *------------------------------------------------------------------------*
049090051216     c     Sr_F14        BegSr
049100051216
049110081020     c                   Clear                   trmk50ds
049120081020     c                   Eval      i50pgm = 'TNTA60R'
049130081020     c                   Movel     v2ccpo        i50cpo
049140081020     c                   Eval      i50rag = wrag
049150110310     c                   IF        *in02 and not *in06 and not *in07
049160110310     c                   Eval      i50mod = 'G'
049170110310     c                   ELSE
049180110310     c                   Eval      i50mod = 'I'
049190110310     c                   ENDIF
049200081020     c                   Call      'TRMK50R'
049210051216     c                   Parm                    kpjba
049220081020     c                   Parm                    trmk50ds
049230051216
049240081020     c                   Eval      wtrmk50 = *On
049250051216     c                   EndSr
049260051216
049270051216      *------------------------------------------------------------------------*
049280051216      * TASTO FUNZIONE F15 -
049290051216      *------------------------------------------------------------------------*
049300051216     c     Sr_F15        BegSr
049310051216
049320051216     c                   Clear                   parmf15
049330051216     c                   Move      w0070         parcli1
049340051216     c                   Eval      kpjbu = parmf15
049350051216     c                   Call      'TNTA83R'
049360051216     c                   Parm                    kpjba
049370051216
049380051216     c                   EndSr
049390051216
049400051216      *------------------------------------------------------------------------*
049410051216      * TASTO FUNZIONE F18 -
049420051216      *------------------------------------------------------------------------*
049430051216     c     Sr_F18        BegSr
049440100406
049450100406      /free
049460100806       //?Richiamo gestione note
049470100406          clear trmk20ds;
049480110511          IMK20tla  = 'L';
049490100406
049500100406         //?Se interrogazione richiamo in interrogazione
049510100406          IF  *in05;
049520100406            IMK20flm = 'I';
049530100406          ENDIF;
049540100406
049550101112          IMK20cpo = v2ccpo;
049560100806          IMK20ksc = v1cksc;
049570100406          IMK20rsc = v1crag;
049580100406
049590100406          trmk20r (kpjba : TRMK20DS);
049600100406
049610100406      /end-free
049620051216
049630051216     c                   EndSr
049640060526      *------------------------------------------------------------------------*
049650060526      * TASTO FUNZIONE F19 - Variazioni
049660060526      *------------------------------------------------------------------------*
049670060526     c     Sr_F19        BegSr
049680060526
049690060526     c                   Clear                   tnta73ds
049700060526     c                   Eval      i73kcc=acokcc
049710060526     c                   Eval      i73ksc=acoksc
049720060529     c                   Movel     v1crag        i73rag
049730060526     c                   movel     tnta73ds      kpjbu
049740060526     c                   Call      'TNTA73R'
049750060526     c                   Parm                    kpjba
049760060526     c                   EndSr
049770100407
049780100407      *------------------------------------------------------------------------*
049790100407      * TASTO FUNZIONE F20 - INTERROGAZIONE CLIENTI POTENZIALI
049800100407      *------------------------------------------------------------------------*
049810100407     c     Sr_F20        BegSr
049820100407
049830100407      /free
049840100806       //?Richiamo gestione potenziali
049850100407          clear trmk01ds;
049860100407
049870100407         //?Se non c'è il codice potenziale vado in ricerca
049880100407          IF  v2ccpo = 0;
049890100407            MK01k11  = '1';
049900100407            MK01rag  = w005a;
049910100407            kpjbu    = TRMK01DS;
049920100407            trmk01r (kpjba);
049930100407            TRMK01DS = kpjbu;
049940100407          ELSE;
049950100407         //?Se c'è il codice potenziale vado in gestione
049960100407            MK01k10 = 'N';
049970100407            MK01cpo = v2ccpo;
049980100407            trmk02r (kpjba : TRMK01DS);
049990100407          ENDIF;
050000100407
050010100407          IF MK01cpo > 0 and MK01cpo <> v2ccpo;
050020100407            v2ccpo = MK01cpo;
050030100407            v2dcpo = MK01rag;
050040100407            exsr Sr_Ctrcpo;
050050100407          ENDIF;
050060100407
050070100407      /end-free
050080100407
050090100407     c                   EndSr
050100051216
050110051216      *------------------------------------------------------------------------*
050120051216      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
050130051216      *------------------------------------------------------------------------*
050140051216     c     Sr_F24        begsr
050150051216
050160051216     c                   Select
050170051216
050180051216     c                   When      $loop = 2 and $rig =2  or
050190051216     c                             $loop = 3 and $rig =3
050200051216     c                   Movea     rigatf1       vzfd01
050210051216     c                   Z-add     1             $rig
050220051216
050230051216     c                   If        $loop = 2
050240051216     c                   Eval      vzfd02= cf2412
050250051216     c                   Else
050260051216     c                   Eval      vzfd02= cf2413
050270051216     c                   EndIf
050280051216
050290051216     c                   When      $loop = 2 and $rig =1 or
050300051216     c                             $loop = 3 and $rig =1
050310051216     c                   Movea     rigatf2       vzfd01
050320051216     c                   Z-add     2             $rig
050330051216
050340051216     c                   If        $loop = 2
050350051216     c                   Eval      vzfd02 = cf2422
050360051216     c                   Else
050370051216     c                   Eval      vzfd02 = cf2423
050380051216     c                   EndIf
050390051216
050400051216     c                   When      $loop = 3 and $rig =2
050410051216     c                   Movea     rigatf3       vzfd01
050420051216     c                   Z-add     3             $rig
050430051216     c                   Eval      vzfd02 = cf2433
050440051216     c                   EndSl
050450051216
050460051216     c                   EndSr
050470051216
050480051216      *------------------------------------------------------------------------*
050490051216      * CONFERMO I DATI
050500051216      *------------------------------------------------------------------------*
050510051216     c     Sr_Conferma   BegSr
050520051220
050530051220     c                   Eval      wokimm = *Off
050540051216
050550051216      * In base alla videata emessa faccio i controlli che servono per
050560051216      * poter confermare i dati del video
050570051216
050580051216     c                   Select
050590051216
050600051216      * F6 dato in seconda videata
050610051216     c                   When      wvideo = '02'
050620051216     c                   ExSr      Sr_Ctrd02
050630051216     c   28              Leavesr
050631180119     c                   IF        ClienteSofferenza
050632180119     c                   leavesr
050633180119     c                   ENDIF
050640051216     c                   ExSr      Sr_Ctrd03
050650051216     c  n28              ExSr      Sr_Ctrd04
050660051216     c  n28              ExSr      Sr_Ctrd05
050670051216     c  n28              ExSr      Sr_Ctrd06
050680051216     c  n28              ExSr      Sr_Ctrd07
050690051216     c   28              Eval      v2cmsg = msg(69)
050700150424     c   28              eval      wForzaPag = *off
050710150728     c   28              eval      wForzaMail = *off
050720051216     c   28              Leavesr
050730051216
050740051216      * F6 dato in terza videata
050750051216     c                   When      wvideo = '03'
050760051216     c                   ExSr      Sr_Ctrd03
050770051216     c   28              Leavesr
050780051216     c                   ExSr      Sr_Ctrd04
050790051216     c  n28              ExSr      Sr_Ctrd05
050800051216     c  n28              ExSr      Sr_Ctrd06
050810051216     c  n28              ExSr      Sr_Ctrd07
050820051216     c   28              Eval      v3cmsg = msg(69)
050830150424     c   28              eval      wForzaPag = *off
050840150728     c   28              eval      wForzaMail = *off
050850051216     c   28              Leavesr
050860051216
050870051216      * F6 dato in quarta videata
050880051216     c                   When      wvideo = '04'
050890051216     c                   ExSr      Sr_Ctrd04
050900051216     c   28              Leavesr
050910051216     c                   ExSr      Sr_Ctrd05
050920051216     c  n28              ExSr      Sr_Ctrd06
050930051216     c  n28              ExSr      Sr_Ctrd07
050940051216     c   28              Eval      v4cmsg = msg(69)
050950150424     c   28              eval      wForzaPag = *off
050960150728     c   28              eval      wForzaMail = *off
050970051216     c   28              Leavesr
050980051216
050990051216      * F6 dato in quinta videata
051000051216     c                   When      wvideo = '05'
051010051216     c                   ExSr      Sr_Ctrd05
051020051216     c   28              Leavesr
051030051216     c                   ExSr      Sr_Ctrd06
051040051216     c  n28              ExSr      Sr_Ctrd07
051050051216     c   28              Eval      v5cmsg = msg(69)
051060150728     c   28              eval      wForzaMail = *off
051070051216     c   28              Leavesr
051080051216
051090051216      * F6 dato in sesta videata
051100051216     c                   When      wvideo = '06'
051110051216     c                   ExSr      Sr_Ctrd06
051120051216     c   28              Leavesr
051130051216     c                   ExSr      Sr_Ctrd07
051140051216     c   28              Eval      v6cmsg = msg(69)
051150150728     c   28              eval      wForzaMail = *off
051160051216     c   28              Leavesr
051170051216
051180051216      * F6 dato in settima videata
051190051216     c                   When      wvideo = '07'
051200051216     c                   ExSr      Sr_Ctrd07
051210051216     c   28              Leavesr
051220051216
051230051216     c                   EndSl
051240051216
051250051216      * Se arrivo a questo punto vuol dire che tutte le videate sono a posto
051260051220
051270051220      * Recupero il flag di procedura ORM attiva
051280060731     c                   Movel     v1cksc        w0030
051290051220     c                   Clear                   og148
051300061012     c                   Clear                   og143
051310051220     c     w0030         Chain     Azorg01l
051320051220     c                   If        %Found(Azorg01l)
051330051220     c                   Eval      og148 = orgde8
051340061012     c                   Eval      og143 = orgde3
051350051220     c                   EndIf
051360160905
051370160905      * se cliente logistica no anagrafica provvisoria
051380160905     c                   IF        ClienteLog
051390160905     c                   goto      NoImmACR
051400160905     c                   ENDIF
051410051220
051420051220      * Se non sono in anagrafica provvisoria, la procedura ORM è attiva e
051430051220      * provengo da convalida offerte
051440120313if  1c                   If        Not *In06 and *In07 and §ogorm <> *blanks
051450051220      * Controllo se devo creare l'anagrafica clienti ritiro
051460051220     c                   Eval      wokimm = *On
051470060731     c                   Movel     v1cksc        w0100
051480051220     c     w0100         Setll     Fnacr01l
051490051220do  2c                   Do        *Hival
051500051220     c                   Read      Fnacr01l
051510051220      * Fine file esco
051520051220     c                   If        %Eof(Fnacr01l)
051530051220     c                   Leave
051540051220     c                   EndIf
051550051220     c                   Movel     acrcro        w0070
051560051220      * Codice diverso esco
051570060731     c                   If        w0070 > v1cksc
051580051220     c                   Leave
051590051220     c                   EndIf
051600051220      * Se trovo non devo inserire l'anagrafico clienti ritiro
051610060731     c                   If        w0070 = v1cksc
051620051220     c                   Eval      wokimm = *Off
051630051220     c                   Leave
051640051220     c                   EndIf
051650051220    2c                   EndDo
051660051220
051670051220      * Se il cliente è già sull'anagrafico clienti ritiro emetto la videata
051680051220      * con un msg info
051690051221     c                   Eval      *In29 = *Off
051700051220if  2c                   If        wokimm = *Off and wforza = *Off
051710051220     c                   Eval      *In28 = *On
051720051220     c                   Eval      wforza = *On
051730051220     c                   Select
051740051220     c                   When      wvideo = '02'
051750051220     c                   Eval      v2cmsg = msg(72)
051760051221     c                   Eval      *In29 = *On
051770051220     c                   Goto      emi02
051780051220     c                   When      wvideo = '03'
051790051220     c                   Eval      v3cmsg = msg(72)
051800051220     c                   Goto      emi03
051810051220     c                   When      wvideo = '04'
051820051220     c                   Eval      v4cmsg = msg(72)
051830051220     c                   Goto      emi04
051840051220     c                   When      wvideo = '05'
051850051220     c                   Eval      v5cmsg = msg(72)
051860051220     c                   Goto      emi05
051870051220     c                   When      wvideo = '06'
051880051220     c                   Eval      v6cmsg = msg(72)
051890051220     c                   Goto      emi06
051900051220     c                   When      wvideo = '07'
051910051220     c                   Eval      v7cmsg = msg(72)
051920051220     c                   Goto      emi07
051930051220     c                   EndSl
051940051220    2c                   EndIf
051950051220    1c                   EndIf
051960160905
051970160905     c     NoImmACR      tag
051980051220
051990150424      * aggiorno la nota "DC"
052000091119     c                   if        v2ntcdc <> savntcdc
052010091119     c                   ExSr      Sr_AggntcDC
052020091119     c                   endif
052030051220
052040051220      * Quindi posso passare i dati dal video ai vari file e aggiornare
052050051216
052060051216      * Immissione pulisco i rcd file e imposto la chiave
052070051219if  1c                   If        *In01
052080051216     c  n06              Clear                   Cnaco000
052090051216     c   06              Clear                   Tfaco000
052100051216     c  n06              Clear                   Cnind000
052110051216     c   06              Clear                   Tfind000
052120051216     c  n06              Clear                   Cnclp000
052130051216     c   06              Clear                   Tfclp000
052140051216     c  n06              Clear                   Fncls000
052150051216     c   06              Clear                   Tfcls000
052160051220      * Chiave ACO
052170051216     c                   Eval      acokut = codut
052180051216     c                   Eval      acokcc = dutkci
052190051222     c  n06              Eval      acoksc = v1cksc
052200051222     c   06              Eval      acoksc = v1cnrv
052210051220      * Chiave IND
052220051216     c                   Eval      indkut = acokut
052230051216     c                   Eval      indkcc = acokcc
052240051216     c                   Eval      indksc = acoksc
052250051220      * Chiave CLP
052260051216     c                   Eval      clpkut = acokut
052270051216     c                   Eval      clpkcc = acokcc
052280051216     c                   Eval      clpksc = acoksc
052290051220      * Chiave CLS
052300051216     c                   Eval      clsksc = acoksc
052310051219      * Campi non visualizzati solo di immissione (non vengono aggiornati)
052320051219     c                   Eval      acoopz = v2copz
052330051219     c                   Eval      acotic = v2ctic
052340051219     c                   Eval      indlin = v2clin
052350051219      * Codice filiale di trasmissione sempre uguale ai primi 3 byte del codice
052360051219      * cliente
052370060330     c                   Movel     acoksc        acoflt
052380051219      * Flag tipo trasmissione sempre a '3' xchè se sono in immissione vuol dire
052390051219      * che sto immettendo un cliente o un'anagrafica provvisoria
052400051219     c                   Eval      acoftt = '3'
052410051219      * Data prima spedizione fattura (solo immissione xchè la imposto io
052420051219      * nel caso di cliente non nuovo
052430051219     c                   Z-add     v4cdps        datagma
052440051219     c                   Eval      gg1 = gg2
052450051219     c                   Eval      mm1 = mm2
052460051219     c                   Eval      aa1 = aa2
052470051219     c                   Z-add     dataamg       clpdps
052480051219      * Cliente da sollecitare
052490090616     c                   Movel     v2csol        clpsol
052500051219    1c                   EndIf
052510051216
052520051219      * Campi che vengono aggiornati a video
052530051219      * ACO
052540051219     c                   Eval      atb02 = v2tb02
052550051219     c                   Eval      acorag = v2crag
052560051219     c                   If        v6cdmg = 'SI'
052570051219     c                   Clear                   acorx1
052580051219     c                   Else
052590051219     c                   Eval      acorx1 = 1
052600051219     c                   EndIf
052610060801     c                   If        v6ccag = 'SI'
052620060801     c                   Clear                   acory1
052630060801     c                   Else
052640060801     c                   Eval      acory1 = 1
052650060801     c                   EndIf
052660051219     c                   Z-add     *date         acoduv
052670130325     c   79              eval      ACOabl = V2Cabledp
052680130325     c  n79              eval      ACOabl = V2Cabl
052690051219     c                   Eval      acolib = v2ccpo
052700051219     c                   Move      v3citc        acoitc
052710051216     c                   Clear                   acodtr
052720051219     c                   Clear                   acoftr
052730051219      * IND
052740051216     c                   Eval      indvia = v2cvia
052750051219     c                   Eval      indcit = v2ccit
052760051216      * Se Cap numerico e non supera i 5 caratteri scrivo anche indcap
052770051216if  1c                   If        %subst(v2ccae:6:1) = *Blanks
052780051216     c                   Eval      wokcap = *On
052790051216     c                   Eval      xx = 1
052800051216     c                   For       xx by 1 to 5
052810051216     c                   If        %subst(v2ccae:(xx):1) < *zeros
052820051216     c                   Eval      wokcap = *Off
052830051216     c                   Leave
052840051216     c                   EndIf
052850051216     c                   EndFor
052860051216     c                   If        wokcap = *On
052870060208     c                   Movel     v2ccae        indcap
052880051216     c                   EndIf
052890051216    1c                   EndIf
052900051216     c                   Eval      indprv = v2cprv
052910051216     c                   Eval      indsta = v2csta
052920051216     c                   Eval      indtel = v2ctel
052930051219     c                   Eval      indcdf = v2ccdf
052940051219     c                   If        v2ivaeu = *Blanks
052950051219     c                   Movel     v2ivait       indiva
052960051219     c                   Else
052970051219     c                   Movel     v2civa        indiva
052980051219     c                   EndIf
052990051219     c                   Eval      indcdp = *all'0'
053000100727     c                   Move      v4ccdp        indcdp
053010051219     c                   Eval      %subst(indopz:1:1) = wbloc
053020100727     c                   Eval      indabi = v4cabi
053030100727     c                   Eval      indcab = v4ccab
053040051216     c                   Eval      indtlf = v2ctlf
053050051219     c                   Move      v4csin        indsin
053060051219     c                   Eval      indcae = v2ccae
053070051219      * CLP
053080100127     c                   Eval      clpscf = %dec(v4cscf:7:0)
053090051219     c                   Move      v4cfft        clpfft
053100051219     c                   Move      v4ctft        clptft
053110051219     c                   Clear                   clpfun
053120051219     c                   Eval      %subst(clpfun:1:1) = v4ctdf
053130051219     c                   Move      v3cage        clpage
053140051219     c                   Clear                   clpcon
053150051219     c                   Eval      %subst(clpcon:2:2) = v2ccon
053160051219     c                   Eval      clpnar = v2cblc
053170051219     c                   Eval      clpclv = v3cclv
053180080422     c                   eval      clpbab = v5ciban
053190080213      * se IBAN non impostato a video
053200080213      * pulisco i dati relativi a abi/cab e c/c
053210080213     c                   if        v5ciban = *blanks
053220090331      * ma solo se ABI NON è > 90000
053230090331     c                   if        clpabi < 90000
053240080213     c                   clear                   clpccb
053250080213     c                   clear                   clpabi
053260080213     c                   clear                   clpcab
053270090331     c                   endif
053280080213     c                   else
053290080213      * se IBAN italia memorizzo abi/cab e c/c
053300080213     c                   if        %subst(v5ciban:1:2) = 'IT' or
053310080213     c                             %subst(v5ciban:1:2) = 'SM'
053320080115     c                   eval      %subst(clpccb:1:12) =
053330080115     c                             %subst(v5ciban:16:12)
053340080115     c                   eval      %subst(clpccb:16:1) =
053350080115     c                             %subst(v5ciban:5:1)
053360080115     c                   eval      clpabi = %dec(%subst(v5ciban:6:5):5:0)
053370080115     c                   eval      clpcab = %dec(%subst(v5ciban:11:5):5:0)
053380080213     c                   endif
053390080213     c                   endif
053400080422     c                   Eval      clpfpc = v5cfpc
053410090617      * Cliente da sollecitare
053420090617     c                   Movel     v2csol        clpsol
053430051219      * CLS
053440051219     c                   Eval      clsstp = v7cstp
053450100729     c                   eval      %subst(CLStic:1:1) = v5tpdn
053460100729     c                   eval      %subst(CLStic:2:1) = v5tpna
053470051219     c                   Eval      %subst(clsflo:1:1) = v6ctcf
053480051219     c                   Eval      %subst(clsflo:2:1) = v4cuni
053490121105     c                   eval      %subst(CLSflo:3:1) = V4Ctpf
053500090616     c                   Eval      %subst(clsflo:4:1) = v2fsol
053510051219     c                   Eval      %subst(clsflo:5:1) = v7csdn
053520051219     c                   Eval      %subst(clsflo:6:1) = v4cstm
053530051219     c                   Eval      %subst(clsflo:7:1) = v7csin
053540051219     c                   Eval      %subst(clsflo:8:1) = v7ctpa
053550051219     c                   Eval      %subst(clsflo:9:1) = v6capg
053560051219     c                   Eval      %subst(clsflo:10:1) = v6ctcm
053570051219
053580051219      * Immissione
053590051219     c                   If        *In01
053600051219     c  n06              Write     Fncls000
053610051219     c   06              Write     Tfcls000
053620051219     c  n06              Write     Cnclp000
053630051219     c   06              Write     Tfclp000
053640051219     c  n06              Write     Cnind000
053650051219     c   06              Write     Tfind000
053660051219     c  n06              Write     Cnaco000
053670051219     c   06              Write     Tfaco000
053680060526     c* Scrivo file variazioni
053690060526     c  n06              exsr      ScriviACV
053700060526     c                   EndIf
053710060526     c
053720051219      * Manutenzione
053730051219     c                   If        *In02
053740051219     c  n06              Update    Fncls000
053750051219     c   06              Update    Tfcls000
053760051219     c  n06              Update    Cnclp000
053770051219     c   06              Update    Tfclp000
053780051219     c  n06              Update    Cnind000
053790051219     c   06              Update    Tfind000
053800051219     c  n06              Update    Cnaco000
053810051219     c   06              Update    Tfaco000
053820060526     c*
053830060526     c* Scrivo file variazioni
053840060526     c  n06              exsr      ScriviACV
053850051219     c                   EndIf
053860110927
053870110927      * Aggiorno il potenziale
053880160906      * solo se NON sono utente di sede
053890110927     c                   If        v2ccpo > *Zeros
053900160906     c                             and not *in09
053910110927     c                   ExSr      Sr_Aggcpo
053920110927     c                   EndIf
053930100729
053940100729      /free
053950100729       //?Memorizzo le coordinate bancarie
053960100730       //?Pagamento DANNI --> DN
053970100730         clear rqsOpCode;
053980100729         wpag = 'DN';
053990100729         wiban = v5ibandn;
054000100730         wbic  = v5bicdn;
054010100805         wcodpo = savtipdn;
054020100805         wcodpn = v5tpdn;
054030100805         wpgm  = 'TNTA60R';
054040100730         SELECT;
054050100730           WHEN  v5ibandn <> *blanks and savibandn = *blanks;
054060100805             rqsOpCode = 'INSERT';
054070100805           WHEN  v5ibandn = *blanks and savibandn <> *blanks;
054080100805             rqsOpCode = 'DELETE';
054090100805           OTHER;
054100100805             rqsOpCode = 'UPDATE';
054110100730         ENDSL;
054120100730         IF  rqsOpCode <> *blanks;
054130100730           exsr Coordinate;
054140100730         ENDIF;
054150100729       //?Non faccio controlli sul codice di ritorno
054160100729       //?ormai l'anagrafica è scritta
054170100729
054180100729       //?Pagamento NOTE ACCREDITO --> NA
054190100730         clear rqsOpCode;
054200100729         wpag = 'NA';
054210100729         wiban = v5ibanna;
054220100730         wbic  = v5bicna;
054230100805         wcodpo = savtipna;
054240100805         wcodpn = v5tpna;
054250100805         wpgm  = 'TNTA60R';
054260100730         SELECT;
054270100730           WHEN  v5ibanna <> *blanks and savibanna = *blanks;
054280100805             rqsOpCode = 'INSERT';
054290100805           WHEN  v5ibanna = *blanks and savibanna <> *blanks;
054300100805             rqsOpCode = 'DELETE';
054310100805           OTHER;
054320100805             rqsOpCode = 'UPDATE';
054330100730         ENDSL;
054340100730         IF  rqsOpCode <> *blanks;
054350100730           exsr Coordinate;
054360100729         ENDIF;
054370100729       //?Non faccio controlli sul codice di ritorno
054380100729       //?ormai l'anagrafica è scritta
054390110223
054400110223       //?Se NON è anagrafica provvisoria:
054410110223       //?il cliente è stato bloccato
054420110223       //?e il potenziale NON è in categoria PERSO
054430110310       //?oppure
054440110310       //?il cliente NON è più bloccato
054450110310       //?e il potenziale è in categoria PERSO
054460110223       //?oppure
054470110223       //?arrivo da convalida
054480110223       //?e il potenziale NON è in categoria CODIFICATO
054490110223         IF  not *in06 and
054500110223             ((ACOabl <> *blanks and
054510110223               savabl = *blanks and V2Hfls <> 'P') or
054520110310              (ACOabl = *blanks and
054530110310               savabl <> *blanks and V2Hfls = 'P') or
054540110223              (*in07 and V2Hfls <> 'C'));
054550110223       //?richiamo pgm di calcolo categoria potenziale
054560110223           clear trmk05ds;
054570110223           IMK05cpo = ACOlib;
054580110223           trmk05r (kpjba : TRMK05DS);
054590110223         //?se categoria variata aggiorno potenziale
054600110304           IF  OMK05err = *blanks and OMK05cat <> V2Hfls;
054610110223             chain(e) ACOlib TNCPO01L;
054620110223             IF  not %error and %found(TNCPO01L);
054630110223               CPOfls = OMK05cat;
054640110223               update TNCPO000;
054650110223             ENDIF;
054660110223           ENDIF;
054670110223         ENDIF;
054680170523
054690170523      * Se cliente logistica no anagrfica clienti ritiro
054700170523      * e no spalmatura coordinate IBAN
054710170523     c                   IF        ClienteLog
054720170523     c                   goto      ByPassaAcr
054730170523     c                   ENDIF
054740150424
054750150424       //?Devo spalmare le coordinate bancarie e i cod.pagamenti
054760150424         WinInfo = *off;
054770150424         EoFW06 = *off;
054780150424         SELECT;
054790150424       //?spalmo da pag. NA a CA e DN
054800150424         WHEN  BonificoNA and
054810150424               (not BonificoCA or not BonificoDN);
054820150424           exsr SpalmaNA;
054830150424         WHEN  BonificoDN and
054840150424               (not BonificoCA or not BonificoNA);
054850150424           exsr SpalmaDN;
054860150424         WHEN  BonificoCA and
054870150424               (not BonificoDN or not BonificoNA);
054880150424           exsr SpalmaCA;
054890150424         ENDSL;
054900150424         IF  WinInfo;
054910150424           DOW not EoFW06;
054920150424             exfmt TA60W06;
054930150424             IF  *inkf;
054940150424               EoFW06 = *on;
054950150424               leave;
054960150424             ENDIF;
054970150424           ENDDO;
054980150424         ENDIF;
054990150424
055000100729      /end-free
055010051220
055020051220      * Se non sono in anagrafica provvisoria
055030120313if  1c                   If        Not *In06 and §ogorm <> *blanks
055040051220
055050051220      * Controllo il blocco/sblocco dei clienti per aggiornare anche
055060051220      * l'anagrafica clienti ritiro (solo in caso di blocco)
055070121121      * lo faccio solo se modifica perchè se è immissione non c'è
055080121121      * ancora l'anagrafica clienti ritiro
055090121121     c                   IF        *in02
055100051220     c                   ExSr      Sr_Ctrabl
055110121121     c                   ENDIF
055120051220
055130051220      * Richiamo l'immissione anagrafica clienti ritiro
055140051220if  2c                   If        *In01 or wokimm = *On
055150121121
055160121121      * NON lo faccio se immissione di codici 8888 - 9999 o incassi
055170121121      * da attribuire --> 0001 1° livello
055180121121      *               --> 1000 2° livello
055190160429      * immissione fatta da profilo EDP
055200121121     c                   SELECT
055210121121     c                   WHEN      wksc04 = 8888 or wksc04 = 9999
055220121121     c                   goto      noacr
055230160429     c                   WHEN       *in20 and
055240121121     c                             ((sav_livello = '1' and wksc04 = 0001) or
055250121121     c                              (sav_livello = '2' and wksc04 = 1000) or
055260121121     c                              (sav_livello = *blanks and
055270121121     c                               wCodIncAtt  = *blanks and
055280121121     c                              (wksc04 = 1000  or  wksc04 = 0001)))
055290121121     c                   goto      noacr
055300121121     c                   ENDSL
055310070809     c                   reset                   FIOR37ds
055320070810     c                   eval      I37opz  = '3'
055330070809     c                   movel     ACOksc        I37fgs
055340070809     c                   movel     ACOksc        I37cro
055350070809     c                   movel     ACOksc        I37ksc
055360070809     c                   movel(p)  FIOR37ds      KPJBU
055370070810     c                   call      'FIOR37R'
055380070809     c                   parm                    KPJBA
055390051220    2c                   EndIf
055400121121     c     noacr         tag
055410051220      * Richiamo la modifica dell'anagrafica clienti ritiro
055420060217if  2c                   If        *In02 and Not *In07
055430070809     c                   reset                   FIOR37ds
055440070809     c                   eval      I37opz  = 'A'
055450070809     c                   movel     ACOksc        I37fgs
055460070809     c                   movel     ACOksc        I37cro
055470070809     c                   movel(p)  FIOR37ds      KPJBU
055480070809     c                   call      'FIOR37R'
055490070809     c                   parm                    KPJBA
055500051220    2c                   EndIf
055510051216
055520051220    1c                   EndIf
055530160905
055540160905     c     ByPassaAcr    tag
055550160905
055560061012     c*
055570061012     c* se cliente di network dpd richiamo pgm gestione legami depot/cod.PdC
055580061012     c* per permettere l'aggiornamento del codice cliente sul file dei depot
055590061012     c                   if        not *in06 and §ogntw='DPD'
055600061012     c* imposta la ds di procedura
055610061012     c                   CLEAR                   tisie5ds
055620061012     c                   MOVE      '02'          de5op0
055630061012     c                   MOVEL     *blanks       de5op1
055640061012     c                   MOVEL     '0'           de5f03
055650061012     c                   MOVEL     '0'           de5f12
055660061012     c                   MOVEL     '0'           de5err
055670061012     c                   MOVEL     *blanks       de5msg
055680061012     c                   movel     'N'           de5sel
055690061012     c* lancia il programma
055700061012     c                   CALL      'TISIE5R'
055710061012     c                   PARM                    kpjba
055720061012     c                   PARM                    tisie5ds
055730061012     c                   endif
055740120510
055750120510      /free
055760120510       //?Se il cliente è stato bloccato devo richiamare il pgm delle INFO
055770120510       //?per obbligo di azzeramento della % affido a BRT
055780120510       //?Il richiamo lo faccio solo se:
055790120510       //?--> è appena stato immesso il blocco
055800120510       //?--> il pgm non è stato richiamto dal CMR x impostare il blocco
055810120510       //?--> c'è il codice del potenziale
055820120510         IF  not $Blocco and
055830120510             ACOabl <> *blanks and ACOabl <> savabl and
055840120510             ACOlib > 0;
055850120510           IF  not %open(CNACO16L);
055860120510             open CNACO16L;
055870120510           ENDIF;
055880120510         //?tutto questo solo se sto bloccando l'ultimo cliente della catena
055890120510         //?se ne ho ancora dei NON bloccati non devo azzerare
055900120510           setll ACOlib CNACO16L;
055910120510           reade ACOlib CNACO16L;
055920120510           DOW not %eof(CNACO16L);
055930120510             IF  ww_ACOabl = *blanks;
055940120510               leavesr;
055950120510             ENDIF;
055960120510             reade ACOlib CNACO16L;
055970120510           ENDDO;
055980120510         //?Aggancio le INFO commerciali
055990120510         //?se la % affido a BRT è = 0 non devo azzerare
056000120510           chain (ACOlib:'10 ':'_BAR') TICPI01L;
056010120510 1         IF  not %found(ticpi01l) or CPIval = 0;
056020120510             leavesr;
056030120510           ENDIF;
056040120510
056050120510         //?pgm INFO
056060120510           clear TRMK50DS;
056070120510           I50pgm = 'TNTA60R';
056080120510           I50cpo = ACOlib;
056090120510           I50rag = ACOrag;
056100120510           I50mod = 'G';
056110120510           I50atb = '0';
056120120510           TRMK50R (kpjba : TRMK50DS);
056130120510
056140120510           wTRMK50 = *on;
056150120510
056160120510         ENDIF;
056170120510
056180120510      /end-free
056190051220
056200051216     c                   EndSr
056210051220
056220060526      *------------------------------------------------------------------------*
056230060526      * Scrittura file variazioni
056240060526      *------------------------------------------------------------------------*
056250060526     c     ScriviACV     BegSr
056260060531     c                   Clear                   Tibs73ds
056270060531     c                   eval      ibs73pru=knmus
056280060531     c                   eval      ibs73noj=knmeb
056290060601     c                   eval      ibs73pgm='TNTA60R'
056300060531     c                   eval      ibs73immag='D'
056310060526     c* Immissione
056320060526     c                   if        *in01
056330060531     c                   eval      ibs73cta='I'
056340060531     c                   Call      'TIBS73R'
056350060531     c                   Parm                    Tibs73ds
056360060531     c                   Parm                    cnaco_73
056370060531     c                   Parm                    cnind_73
056380060531     c                   Parm                    cnclp_73
056390060531     c                   Parm                    fncls_73
056400060526     c                   endif
056410060526     c* Manutenzione
056420060526     C                   IF        *IN02
056430060531     c                   eval      ibs73cta='M'
056440060531     c                   Call      'TIBS73R'
056450060531     c                   Parm                    Tibs73ds
056460060531     c                   Parm                    cnaco_73
056470060531     c                   Parm                    cnind_73
056480060531     c                   Parm                    cnclp_73
056490060531     c                   Parm                    fncls_73
056500060531     c                   endif
056510060526     c
056520060531     c                   Eval      wtibs73 = *On
056530060526     c                   EndSr
056540150424
056550150424      /free
056560150424       //--------------------------------------------------------------
056570150427       //?Spalmo coordinate bancarie e pagamenti CA.
056580150424       //--------------------------------------------------------------
056590150424       BEGSR SpalmaCA;
056600150427
056610150512       //?Se il pagamento CA è estero
056620150427       //?spalmo solo il codice senza le coordinate
056630150427         IF  PagEsteroCA;
056640150427           clear CLPbab;
056650150427         ENDIF;
056660150427
056670150427       //?Anagrafica clienti Reale
056680150427         IF  not *in06;
056690150427       //?Aggancio FNCLS
056700150427           chain V1Cksc FNCLS01L;
056710150427           IF  not %found(FNCLS01L);
056720150427             leavesr;
056730150427           ENDIF;
056740150427       //?Spalmo pagamento e coordinate per DN
056750151112           IF  CodPagDN <> '1';
056760150427           IF  not BonificoDN;
056770150427         //?coordinate bancarie
056780150427             clear rqsOpCode;
056790150427             wpag = 'DN';
056800150427             wiban = CLPbab;
056810150427             wcodpo = %subst(CLStic:1:1);
056820150427             wcodpn = CLPfpc;
056830150427             wpgm  = 'TNTA60R';
056840150427             rqsOpCode = 'INSERT';
056850150427             exsr Coordinate;
056860150427             %subst(CLStic:1:1) = CLPfpc;
056870151112             WinInfo = *on;
056880150427           ENDIF;
056890151112           ENDIF;
056900150427       //?Spalmo pagamento e coordinate per NA
056910151112           IF  CodPagNA <> '1';
056920150427           IF  not BonificoNA;
056930150427         //?coordinate bancarie
056940150427             clear rqsOpCode;
056950150427             wpag = 'NA';
056960150427             wiban = CLPbab;
056970150427             wcodpo = %subst(CLStic:2:1);
056980150427             wcodpn = CLPfpc;
056990150427             wpgm  = 'TNTA60R';
057000150427             rqsOpCode = 'INSERT';
057010150427             exsr Coordinate;
057020150427             %subst(CLStic:2:1) = CLPfpc;
057030151112             WinInfo = *on;
057040150427           ENDIF;
057050151112           ENDIF;
057060150427       //?Aggiorno anagrafica
057070150427           update FNCLS000;
057080150427         ENDIF;
057090150427
057100150427       //?Anagrafica clienti Provvisoria
057110150427         IF  *in06;
057120150427       //?Aggancio TFCLS
057130150427           chain V1Cnrv TFCLS01L;
057140150427           IF  not %found(TFCLS01L);
057150150427             leavesr;
057160150427           ENDIF;
057170150427       //?Spalmo pagamento e coordinate per DN
057180151112           IF  CodPagDN <> '1';
057190150427           IF  not BonificoDN;
057200150427             clear rqsOpCode;
057210150427             wpag = 'DN';
057220150427             wiban = CLPbab;
057230150427             wcodpo = %subst(CLStic:1:1);
057240150427             wcodpn = CLPfpc;
057250150427             wpgm  = 'TNTA60R';
057260150427             rqsOpCode = 'INSERT';
057270150427             exsr Coordinate;
057280150427             %subst(CLStic:1:1) = CLPfpc;
057290151112             WinInfo = *on;
057300150427           ENDIF;
057310151112           ENDIF;
057320150427       //?Spalmo pagamento e coordinate per NA
057330151112           IF  CodPagNA <> '1';
057340150427           IF  not BonificoNA;
057350150427             clear rqsOpCode;
057360150427             wpag = 'NA';
057370150427             wiban = CLPbab;
057380150427             wcodpo = %subst(CLStic:2:1);
057390150427             wcodpn = CLPfpc;
057400150427             wpgm  = 'TNTA60R';
057410150427             rqsOpCode = 'INSERT';
057420150427             exsr Coordinate;
057430150427             %subst(CLStic:2:1) = CLPfpc;
057440151112             WinInfo = *on;
057450150427           ENDIF;
057460151112           ENDIF;
057470150427       //?Aggiorno anagrafica
057480150427           update TFCLS000;
057490150427         ENDIF;
057500150424
057510150424       ENDSR;
057520150424
057530150424       //--------------------------------------------------------------
057540150427       //?Spalmo coordinate bancarie e pagamenti DN.
057550150424       //--------------------------------------------------------------
057560150424       BEGSR SpalmaDN;
057570150427
057580150427         SELECT;
057590150427
057600150427       //?Se il pagamento DN è estero ed è
057610150427       //?Anagrafica clienti Reale
057620150427       //?spalmo solo il codice senza le coordinate
057630150427         WHEN  PagEsteroDN and not *in06;
057640150427       //?Memorizzo immagine precedente
057650150427           exsr ImmaginePrima;
057660150427       //?Spalmo pagamento per CA
057670151112           IF  CodPagCA <> '1';
057680150427           IF  not BonificoCA;
057690150427         //?Aggancio CNCLP
057700150427             chain (codut:V1Ckcc:V1Cksc) CNCLP00F;
057710150427             IF  not %found(CNCLP00F);
057720150427               leavesr;
057730150427             ENDIF;
057740150427             CLPfpc = %subst(CLStic:1:1);
057750150427             update CNCLP000;
057760151112             WinInfo = *on;
057770150427           ENDIF;
057780151112           ENDIF;
057790150427       //?Spalmo pagamento per NA
057800151112           IF  CodPagNA <> '1';
057810150427           IF  not BonificoNA;
057820150427         //?Aggancio FNCLS
057830150427             chain V1Cksc FNCLS01L;
057840150427             IF  not %found(FNCLS01L);
057850150427               leavesr;
057860150427             ENDIF;
057870150427             %subst(CLStic:2:1) = %subst(CLStic:1:1);
057880150427             update FNCLS000;
057890151112             WinInfo = *on;
057900150427           ENDIF;
057910151112           ENDIF;
057920150427       //?Immagine Dopo
057930150427           exsr ImmagineDopo;
057940150427
057950150427       //?Se il pagamento DN è estero ed è
057960150427       //?Anagrafica clienti Provvisoria
057970150427       //?spalmo solo il codice senza le coordinate
057980150427         WHEN  PagEsteroDN and *in06;
057990150427       //?Spalmo pagamento per CA
058000151112           IF  CodPagCA <> '1';
058010150427           IF  not BonificoCA;
058020150427         //?Aggancio TFCLP
058030150427             chain (codut:V1Ckcc:V1Cnrv) TFCLP00F;
058040150427             IF  not %found(TFCLP00F);
058050150427               leavesr;
058060150427             ENDIF;
058070150427             CLPfpc = %subst(CLStic:1:1);
058080150427             update TFCLP000;
058090151112             WinInfo = *on;
058100150427           ENDIF;
058110151112           ENDIF;
058120150427       //?Spalmo pagamento per NA
058130151112           IF  CodPagNA <> '1';
058140150427           IF  not BonificoNA;
058150150427         //?Aggancio TFCLS
058160150427             chain V1Cnrv TFCLS01L;
058170150427             IF  not %found(TFCLS01L);
058180150427               leavesr;
058190150427             ENDIF;
058200150427             %subst(CLStic:2:1) = %subst(CLStic:1:1);
058210150427             update TFCLS000;
058220151112             WinInfo = *on;
058230150427           ENDIF;
058240151112           ENDIF;
058250150427
058260150427         OTHER;
058270150427       //?Se arrivo qua il pagamento non è estero quindi spalmo tutto
058280150424       //?Recupero le coordinate bancarie DN
058290150427           wpag = 'DN';
058300150427           rqsOpCode = 'SEARCH';
058310150427           exsr Coordinate;
058320150424       //?Spalmo coordinate e pagamento per CA
058330151112           IF  CodPagCA <> '1';
058340150427           IF  not BonificoCA;
058350150424       //?Memorizzo immagine precedente
058360150427             IF  not *in06;
058370150427               exsr ImmaginePrima;
058380150427         //?Aggancio CNCLP
058390150427               chain (codut:V1Ckcc:V1Cksc) CNCLP00F;
058400150427               IF  not %found(CNCLP00F);
058410150427                 leavesr;
058420150427               ENDIF;
058430150427             ELSE;
058440150427         //?Aggancio TFCLP
058450150427               chain (codut:V1Ckcc:V1Cnrv) TFCLP00F;
058460150427               IF  not %found(TFCLP00F);
058470150427                 leavesr;
058480150427               ENDIF;
058490150427             ENDIF;
058500150427             CLPfpc = %subst(CLStic:1:1);
058510150427             IF  not *in06;
058520150427               CLPbab = TrulIbanO0.IBAN;
058530150427               CLPabi = TrulIbanO0.ABI;
058540150427               CLPcab = TrulIbanO0.CAB;
058550150427               update CNCLP000;
058560150427         //?Memorizzo immagine sucessiva
058570150427               exsr ImmagineDopo;
058580150427             ELSE;
058590150427               CLPbab = TrulIbanO1.IBAN;
058600150427               CLPabi = TrulIbanO1.ABI;
058610150427               CLPcab = TrulIbanO1.CAB;
058620150427               update TFCLP000;
058630150427             ENDIF;
058640151112             WinInfo = *on;
058650150427           ENDIF;
058660151112           ENDIF;
058670150424
058680150424       //?Spalmo coordinate e pagamento per NA
058690151112           IF  CodPagNA <> '1';
058700150427           IF  not BonificoNA;
058710150427             clear rqsOpCode;
058720150427             wpag = 'NA';
058730150427         //?Aggancio FNCLS
058740150427             IF  not *in06;
058750150427               chain V1Cksc FNCLS01L;
058760150427               IF  not %found(FNCLS01L);
058770150427                 leavesr;
058780150427               ENDIF;
058790150427               wiban = TrulIbanO0.IBAN;
058800150427               wbic  = TrulIbanO0.BIC;
058810150427               wcodpo = %subst(CLStic:2:1);
058820150427               wcodpn = %subst(CLStic:1:1);
058830150427             ELSE;
058840150427         //?Aggancio TFCLS
058850150427               chain V1Cnrv TFCLS01L;
058860150427               IF  not %found(TFCLS01L);
058870150427                 leavesr;
058880150427               ENDIF;
058890150427               wiban = TrulIbanO1.IBAN;
058900150427               wbic  = TrulIbanO1.BIC;
058910150427               wcodpo = %subst(CLStic:2:1);
058920150427               wcodpn = %subst(CLStic:1:1);
058930150427             ENDIF;
058940150427             wpgm  = 'TNTA60R';
058950150427             rqsOpCode = 'INSERT';
058960150427             exsr Coordinate;
058970150427             %subst(CLStic:2:1) = %subst(CLStic:1:1);
058980150427         //?Aggiorno anagrafica
058990150427             IF  not *in06;
059000150427               update FNCLS000;
059010150427             ELSE;
059020150427               update TFCLS000;
059030150427             ENDIF;
059040151112             WinInfo = *on;
059050150427           ENDIF;
059060151112           ENDIF;
059070150427         ENDSL;
059080150424
059090150424       ENDSR;
059100150424
059110150424       //--------------------------------------------------------------
059120150427       //?Spalmo coordinate bancarie e pagamenti NA.
059130150424       //--------------------------------------------------------------
059140150424       BEGSR SpalmaNA;
059150150427
059160150427         SELECT;
059170150427
059180150427       //?Se il pagamento NA è estero ed è
059190150427       //?Anagrafica clienti Reale
059200150427       //?spalmo solo il codice senza le coordinate
059210150427         WHEN  PagEsteroNA and not *in06;
059220150427       //?Memorizzo immagine precedente
059230150427           exsr ImmaginePrima;
059240150427       //?Spalmo pagamento per CA
059250151112           IF  CodPagCA <> '1';
059260150427           IF  not BonificoCA;
059270150427         //?Aggancio CNCLP
059280150427             chain (codut:V1Ckcc:V1Cksc) CNCLP00F;
059290150427             IF  not %found(CNCLP00F);
059300150427               leavesr;
059310150427             ENDIF;
059320150427             CLPfpc = %subst(CLStic:2:1);
059330150427             update CNCLP000;
059340151112             WinInfo = *on;
059350150427           ENDIF;
059360151112           ENDIF;
059370150427       //?Spalmo pagamento per DN
059380151112           IF  CodPagDN <> '1';
059390150427           IF  not BonificoDN;
059400150427         //?Aggancio FNCLS
059410150427             chain V1Cksc FNCLS01L;
059420150427             IF  not %found(FNCLS01L);
059430150427               leavesr;
059440150427             ENDIF;
059450150427             %subst(CLStic:1:1) = %subst(CLStic:2:1);
059460150427             update FNCLS000;
059470151112             WinInfo = *on;
059480150427           ENDIF;
059490151112           ENDIF;
059500150427       //?Immagine Dopo
059510150427           exsr ImmagineDopo;
059520150427
059530150427       //?Se il pagamento NA è estero ed è
059540150427       //?Anagrafica clienti Provvisoria
059550150427       //?spalmo solo il codice senza le coordinate
059560150427         WHEN  PagEsteroNA and *in06;
059570150427       //?Spalmo pagamento per CA
059580151112           IF  CodPagCA <> '1';
059590150427           IF  not BonificoCA;
059600150427         //?Aggancio TFCLP
059610150427             chain (codut:V1Ckcc:V1Cnrv) TFCLP00F;
059620150427             IF  not %found(TFCLP00F);
059630150427               leavesr;
059640150427             ENDIF;
059650150427             CLPfpc = %subst(CLStic:2:1);
059660150427             update TFCLP000;
059670151112             WinInfo = *on;
059680150427           ENDIF;
059690151112           ENDIF;
059700150427       //?Spalmo pagamento per DN
059710151112           IF  CodPagDN <> '1';
059720150427           IF  not BonificoDN;
059730150427         //?Aggancio TFCLS
059740150427             chain V1Cnrv TFCLS01L;
059750150427             IF  not %found(TFCLS01L);
059760150427               leavesr;
059770150427             ENDIF;
059780150427             %subst(CLStic:1:1) = %subst(CLStic:2:1);
059790150427             update TFCLS000;
059800151112             WinInfo = *on;
059810150427           ENDIF;
059820151112           ENDIF;
059830150427
059840150427         OTHER;
059850150427       //?Se arrivo qua il pagamento non è estero quindi spalmo tutto
059860150424       //?Recupero le coordinate bancarie NA
059870150427           wpag = 'NA';
059880150427           rqsOpCode = 'SEARCH';
059890150427           exsr Coordinate;
059900150424       //?Spalmo coordinate e pagamento per CA
059910151112           IF  CodPagCA <> '1';
059920150427           IF  not BonificoCA;
059930150424       //?Memorizzo immagine precedente
059940150427             IF  not *in06;
059950150427               exsr ImmaginePrima;
059960150424       //?Aggancio CNCLP
059970150427               chain (codut:V1Ckcc:V1Cksc) CNCLP00F;
059980150427               IF  not %found(CNCLP00F);
059990150427                 leavesr;
060000150427               ENDIF;
060010150427             ELSE;
060020150427       //?Aggancio TFCLP
060030150427               chain (codut:V1Ckcc:V1Cnrv) TFCLP00F;
060040150427               IF  not %found(TFCLP00F);
060050150427                 leavesr;
060060150427               ENDIF;
060070150427             ENDIF;
060080150427             CLPfpc = %subst(CLStic:2:1);
060090150427             IF  not *in06;
060100150427               CLPbab = TrulIbanO0.IBAN;
060110150427               CLPabi = TrulIbanO0.ABI;
060120150427               CLPcab = TrulIbanO0.CAB;
060130150427               update CNCLP000;
060140150427         //?Memorizzo immagine sucessiva
060150150427               exsr ImmagineDopo;
060160150427             ELSE;
060170150427               CLPbab = TrulIbanO1.IBAN;
060180150427               CLPabi = TrulIbanO1.ABI;
060190150427               CLPcab = TrulIbanO1.CAB;
060200150427               update TFCLP000;
060210150427             ENDIF;
060220151112             WinInfo = *on;
060230150427           ENDIF;
060240151112           ENDIF;
060250150424
060260150424       //?Spalmo coordinate e pagamento per DN
060270151112           IF  CodPagDN <> '1';
060280150427           IF  not BonificoDN;
060290150427             clear rqsOpCode;
060300150427             wpag = 'DN';
060310150427         //?Aggancio FNCLS
060320150427             IF  not *in06;
060330150427               chain V1Cksc FNCLS01L;
060340150427               IF  not %found(FNCLS01L);
060350150427                 leavesr;
060360150427               ENDIF;
060370150427               wiban = TrulIbanO0.IBAN;
060380150427               wbic  = TrulIbanO0.BIC;
060390150427               wcodpo = %subst(CLStic:1:1);
060400150427               wcodpn = %subst(CLStic:2:1);
060410150427             ELSE;
060420150427         //?Aggancio TFCLS
060430150427               chain V1Cnrv TFCLS01L;
060440150427               IF  not %found(TFCLS01L);
060450150427                 leavesr;
060460150427               ENDIF;
060470150427               wiban = TrulIbanO1.IBAN;
060480150427               wbic  = TrulIbanO1.BIC;
060490150427               wcodpo = %subst(CLStic:1:1);
060500150427               wcodpn = %subst(CLStic:2:1);
060510150427             ENDIF;
060520150427             wpgm  = 'TNTA60R';
060530150427             rqsOpCode = 'INSERT';
060540150427             exsr Coordinate;
060550150427             %subst(CLStic:1:1) = %subst(CLStic:2:1);
060560150427         //?Aggiorno anagrafica
060570150427             IF  not *in06;
060580150427               update FNCLS000;
060590150427             ELSE;
060600150427               update TFCLS000;
060610150427             ENDIF;
060620151112             WinInfo = *on;
060630150427           ENDIF;
060640151112           ENDIF;
060650150424
060660150427         ENDSL;
060670150424
060680150424       ENDSR;
060690150424      /end-free
060700150424
060710051220      *------------------------------------------------------------------------*
060720051220      * AGGIORNAMENTO DEL CODICE POTENZIALE
060730051220      *------------------------------------------------------------------------*
060740051220     c     Sr_Aggcpo     BegSr
060750100407
060760100407      /free
060770100806       //?Se variato codice potenziale
060780110928       //?o se immissione di nuovo codice
060790110928        IF  (v2ccpo <> savcpo and savcpo > 0) or *in01;
060800100407         //?richiamo programma di comodo per sistemare potenziale
060810100407         //?sui vari archivi (trattativa, anagrafica provvisoria, attività)
060820100407         //?nel caso di trattativa aperta
060830110223         //?in ogni caso ricalcola e aggiorna sempre la categoria
060840110223         //?del vecchio e nuovo potenziale
060850100407          clear TRMK28DS;
060860100407          IMK28cpo   = v2ccpo;
060870100407          IMK28cpoex = savcpo;
060880100407          IMK28ksc   = v1cksc;
060890100407          trmk28r (TRMK28DS);
060900100407        ENDIF;
060910121105
060920121105       //?Se Immissione di nuovo cliente (no anagrafica provvisoria)
060930121105       //?devo aggiornare la data primo contatto sul Potenziale
060940121105        IF  *in01 and not *in06 and V2Ccpo > 0 and
060950121105           (§CPOdtpcon = *blanks or §CPOdtpcon = *zeros);
060960121105          chain(e) CPOcpo TNCPO01L;
060970121105          IF  not %error and %found(TNCPO01L);
060980121105            dCPO01 = CPOrst;
060990121105            §CPOdtpcon = %editc(dataoggi:'X');
061000121105            CPOrst = dCPO01;
061010121105            update TNCPO000;
061020121105          ENDIF;
061030121105        ENDIF;
061040121105
061050100407      /end-free
061060080307
061070080307      * se sono in anagrafica provvisoria aggiorno codice fiscale e p.iva
061080080307      * sul potenziale se sul potenziale mancano
061090080307     c                   if        *in06  and *in01
061100110516     c     v2ccpo        Chain(e)  Tncpo01l
061110110516     c                   if        %error
061120110516     c                   leavesr
061130110516     c                   endif
061140080307     c                   if        %found(tncpo01l)
061150160803      /free
061160160803       //?Salvo l'immagine precedente dell'anagrafica potenziale
061170160803         exsr ImmaginePrimaCpo;
061180160803      /end-free
061190080307     c                   if        (cpopiv=*blanks and v2civa<>*blanks)
061200080307     c                   eval      cpopiv=v2civa
061210080307     c                   z-add     *date         cpodtr
061220080307     c                   endif
061230080307     c                   if        (cpocdf=*blanks and v2ccdf<>*blanks)
061240080307     c                   eval      cpocdf=v2ccdf
061250080307     c                   z-add     *date         cpodtr
061260080307     c                   endif
061270080307     c                   update    tncpo000
061280160803      /free
061290160803       //?Scrivo la Variazione fatta sull'anagrafica potenziale
061300160803         exsr ScriviVarCpo;
061310160803      /end-free
061320080307     c                   endif
061330080307     c                   endif
061340051220
061350051220     c                   EndSr
061360051216
061370051216      *------------------------------------------------------------------------*
061380051125      * CONTROLLO DEL CODICE POTENZIALE
061390051125      *------------------------------------------------------------------------*
061400051125     c     Sr_Ctrcpo     BegSr
061410051125
061420111122      * Obbligatorio se cliente, se non è un 8888/9999
061430110223      * e se non è un 'incassi da attribuire'
061440111122      * se non è utente EDP e codice nuovo x 'incassi da attribuire'
061450051125     c                   If        v2ccpo = *Zeros and
061460110223     c                             v1ckcc = 151 and
061470051125     c                             wksc04 <> 8888 and wksc04 <> 9999
061480110223     c                             and %editc(v1cksc:'X') <> wCodIncAtt
061490170516      // Se cliente potenziale emetto un messaggio forzabile
061500170516       IF  (ClienteLog and not wForzaPotenziale) or
061510170516            not ClienteLog;
061520051125     c                   Eval      *In28 = *On
061530051202     c                   Eval      h2riga = 14
061540051125     c                   Eval      h2colo = 28
061550051125     c                   Eval      v2cmsg = msg(12)
061560170516       IF  ClienteLog;
061570170516         V2Cmsg = %trim(V2Cmsg) + '. Enter per forzare.';
061580170516         wForzaPotenziale = *on;
061590170516       ENDIF;
061600160429     c                   IF        *in20 and *in01
061610111122     c                             and sav_livello = *blanks
061620111122     c                             and wCodIncAtt = *blanks and
061630111122     c                             (wksc04 = 0001 or wksc04 = 1000)
061640111122     c                   clear                   v2cmsg
061650111122     c                   eval      *in28 = *off
061660111122     c                   ENDIF
061670051125     c                   Leavesr
061680170516       ENDIF;
061690051125     c                   EndIf
061700051125      * Carico i dati del potenziale se inserito
061710051125if  1c                   If        v2ccpo <> *Zeros and
061720051125      * e se variato il codice potenziale
061730051125     c                             v2ccpo <> savcpo
061740060221     c                             and wokpot = *off
061750051125      * Controllo se esite
061760060221     c     v2ccpo        Chain(n)  Tncpo01l
061770051125     c                   If        Not %Found(Tncpo01l)
061780051125     c                   Eval      *In28 = *On
061790051202     c                   Eval      h2riga = 14
061800051125     c                   Eval      h2colo = 28
061810051125     c                   Eval      v2cmsg = msg(13)
061820051125     c                   Leavesr
061830051125     c                   EndIf
061840051125      * Controllo se valido
061850051125     c                   If        cpoatb <> *Blanks
061860051125     c                   Eval      *In28 = *On
061870150427     c                   Eval      h2riga = 14
061880051125     c                   Eval      h2colo = 28
061890051125     c                   Eval      v2cmsg = msg(13)
061900051125     c                   Leavesr
061910051125     c                   EndIf
061920051125      * Controllo se l'utente può gestire il potenziale
061930051125     c                   Move      cpoflt        w003a
061940051125     c     w003a         Lookup    skpogcpo                               30
061950051125     c                   If        Not *In30
061960051125     c                   Eval      *In28 = *On
061970051202     c                   Eval      h2riga = 14
061980051125     c                   Eval      h2colo = 28
061990060110     c                   Eval      v2cmsg = msg(14)
062000051125     c                   Leavesr
062010051125     c                   EndIf
062020100604
062030100604      /free
062040100604       //?Se NON sono in ambiente di filiale non posso variare il potenziale
062050100604       IF  *in09 and v2ccpo <> savcpo and savcpo > 0;
062060100604         *in28 = *on;
062070100604         h2riga = 07;
062080100604         h2colo = 28;
062090100604         v2cmsg = 'Potenziale modificabile SOLO da utente di filiale';
062100100604         leavesr;
062110100604       ENDIF;
062120100806       //?controllo se con il vecchio potenziale c'è una
062130100604       //?trattativa aperta, in questo caso controllo che non ne esista già
062140100604       //?una con il nuovo potenziale
062150100806       IF  v2ccpo <> savcpo and savcpo > 0;
062160100604       //?con vecchio potenziale
062170100604         clear TNTA43ds;
062180100604         ITA43cpo = savcpo;
062190100604         ITA43ksc = v1cksc;
062200100604         ITA43cmm = %dec(v3cage:7:0);
062210100604         tnta43r (tnta43ds);
062220100604       //?trovo trattativa aperta
062230100604         IF  OTA43nrv > 0;
062240100604         //?rifaccio il controllo con nuovo potenziale
062250100604           clear TNTA43ds;
062260100604           ITA43cpo = v2ccpo;
062270100604           ITA43ksc = v1cksc;
062280100604           ITA43cmm = %dec(v3cage:7:0);
062290100604           tnta43r (tnta43ds);
062300100604         //?trovo trattativa aperta
062310100604           IF  OTA43nrv > 0;
062320100604             *in28 = *on;
062330100604             h2riga = 07;
062340100604             h2colo = 28;
062350100604             v2cmsg = 'Modifica non possibile, trattativa aperta sul +
062360100604                       potenziale immesso';
062370100604             leavesr;
062380100604           ENDIF;
062390100604         ENDIF ;
062400100604       ENDIF;
062410100604      /end-free
062420051212
062430051206      * Carico a video i dati del potenziale se sono in immissione
062440051206     c   01              ExSr      Sr_Carcpo
062450051125    1c                   EndIf
062460051125
062470051125     c                   EndSr
062480051116
062490051116      *------------------------------------------------------------------------*
062500051116      * CARICO I DATI DEL CODICE POTENZIALE
062510051116      *------------------------------------------------------------------------*
062520051116     c     Sr_Carcpo     BegSr
062530140714
062540140714     c     CPOcpo        Chain     Tncpo11l
062550051116
062560051116     c                   Eval      v2crag = cporag
062570051116     c                   Eval      v2cvia = cpovia
062580051116     c                   Eval      v2ccae = cpocap
062590051117     c                   Eval      v2csta = cponaz
062600051116     c                   Eval      v2ccit = cpocit
062610051116     c                   Eval      v2cprv = cpoprv
062620140714     c                   Eval      v2ctel = cpo1tel
062630140714     c                   Eval      v2ctlf = cpo1fax
062640051117     c                   Eval      v2ccpo = cpocpo
062650051117     c                   Eval      savcpo = cpocpo
062660051130     c                   Move      cposct        savitc
062670060208     c                   Eval      v2dcpo = cporag
062680080306     c                   Eval      v2ccdf = cpocdf
062690080306     c                   Eval      savcdf = v2ccdf
062700080306    5c                   if        %subst(cpopiv:1:2)>='00'
062710080306     c                   Movel     cpopiv        v2ivait
062720080306     c                   else
062730080306     c                   Movel     cpopiv        v2civa
062740080306    5c                   endif
062750080306     c                   Eval      saviva = v2civa
062760051116
062770051116     c                   EndSr
062780091119
062790091119      *------------------------------------------------------------------------*
062800091119      * AGGIORNAMENTO NOTA DC
062810091119      *------------------------------------------------------------------------*
062820091119     c     Sr_AggntcDC   BegSr
062830091119
062840091119     c   70
062850091119     corn06              Eval      kntcapl = 'C'
062860100806     c   06
062870091119     cann70              Eval      kntcapl = 'T'
062880091119     c                   Movel(p)  dutkci        kntcnk1
062890091119     c   70
062900091119     corn06              Move      v1cksc        kntcnk1
062910100806     c   06
062920100806     cann70              Move      v1cnrv        kntcnk1
062930091119     c                   Clear                   kntcnk2
062940091119     c                   Movel     'DC'          kntctnt
062950091119     c     kTfntc        Chain     Tfntc01l
062960091119     c                   If        %Found(Tfntc01l)
062970091119      * devo cancellarla perchè campo vuoto
062980091119     c                   if        v2ntcdc = *blanks
062990091119     c                   delete    tfntc
063000091119     c                   endif
063010091119      * aggiorno la nota
063020091119     c                   if        v2ntcdc <> *blanks
063030091119     c                   eval      ntcrnt = v2ntcdc
063040091119     c                   move      *date         ntcntr
063050091119     c                   update    tfntc
063060091119     c                   endif
063070091119     c                   endif
063080091119      * scrivo la nota
063090091119     c                   If        not %Found(Tfntc01l)
063100091119     c                   clear                   tfntc
063110091119     c                   eval      ntcapl = kntcapl
063120091119     c                   eval      ntcnk1 = kntcnk1
063130091119     c                   eval      ntctnt = kntctnt
063140091119     c                   eval      ntcrnt = v2ntcdc
063150091119     c                   eval      ntcsns = 'S'
063160091119     c                   move      *date         ntcntr
063170091119     c                   write     tfntc
063180091119     c                   endif
063190091119
063200091119     c                   EndSr
063210051212
063220051212      *--------------------------------------------------------------------*
063230051212      * WINDOW X CONFERMA DI VARIAZIONE POTENZIALE
063240051212      *--------------------------------------------------------------------*
063250051212     c     Sr_Winpot     begsr
063260051212
063270051212     c                   Reset                   wokpot
063280051212     c                   Eval      w01cok = 'NO'
063290051212     c                   Exfmt     ta60w01
063300051212      * Ok il nuovo potenziale è da tenere
063310051212if  1c                   If        w01cok = 'SI'
063320051212     c                   Eval      wokpot = *On
063330051212    1c                   EndIf
063340051212
063350051212     c                   EndSr
063360051118
063370051118      *------------------------------------------------------------------------*
063380051118      * CONTROLLO INDIRIZZO
063390051118      *------------------------------------------------------------------------*
063400051118     c     Sr_Ctrind     BegSr
063410051118
063420051118     c                   Clear                   fnlv13ds
063430051118     c                   Clear                   tisi95ds
063440051124     c                   Eval      *In90 = *Off
063450051118
063460051118      * Indirizzo completo
063470051118     c                   Z-add     *date         i14dri
063480051118     c                   Call      'FNLV14R'
063490051118     c                   Parm                    kpjba
063500051118     c                   Parm                    fnlv14ds
063510051118
063520051118s   1c                   Select
063530051118     c                   When      o14err = '1'
063540051124     c                   Eval      *In90 = *On
063550051130     c                   Eval      h2riga = 07
063560051118     c                   Eval      h2colo = 28
063570051124     c                   If        o14msg <> *Blanks
063580051124     c                   Eval      *In28 = *On
063590051118     c                   Eval      v2cmsg = o14msg
063600051124     c                   EndIf
063610051124     c                   Leavesr
063620051118     c                   When      o14err = '2'
063630051124     c                   Eval      *In90 = *On
063640051130     c                   Eval      h2riga = 08
063650051118     c                   Eval      h2colo = 28
063660051124     c                   If        o14msg <> *Blanks
063670051124     c                   Eval      *In28 = *On
063680051118     c                   Eval      v2cmsg = o14msg
063690051124     c                   EndIf
063700051124     c                   Leavesr
063710051118     c                   When      o14err = '5'
063720051124     c                   Eval      *In90 = *On
063730051130     c                   Eval      h2riga = 09
063740051118     c                   Eval      h2colo = 18
063750051124     c                   If        o14msg <> *Blanks
063760051124     c                   Eval      *In28 = *On
063770051118     c                   Eval      v2cmsg = o14msg
063780051124     c                   EndIf
063790051124     c                   Leavesr
063800051118     c                   When      o14err = '3'
063810051124     c                   Eval      *In90 = *On
063820051130     c                   Eval      h2riga = 09
063830051118     c                   Eval      h2colo = 28
063840051124     c                   If        o14msg <> *Blanks
063850051124     c                   Eval      *In28 = *On
063860051118     c                   Eval      v2cmsg = o14msg
063870051124     c                   EndIf
063880051124     c                   Leavesr
063890051118     c                   When      o14err = '4'
063900051124     c                   Eval      *In90 = *On
063910051130     c                   Eval      h2riga = 09
063920051118     c                   Eval      h2colo = 59
063930051124     c                   If        o14msg <> *Blanks
063940051124     c                   Eval      *In28 = *On
063950051118     c                   Eval      v2cmsg = o14msg
063960051124     c                   EndIf
063970051124     c                   Leavesr
063980051118     c                   When      o14err = '6'
063990051124     c                   Eval      *In90 = *On
064000051130     c                   Eval      h2riga = 09
064010051118     c                   Eval      h2colo = 62
064020051124     c                   If        o14msg <> *Blanks
064030051124     c                   Eval      *In28 = *On
064040051118     c                   Eval      v2cmsg = o14msg
064050051124     c                   EndIf
064060051124     c                   Leavesr
064070051118     c                   When      o14err <> *Blanks and o14msg = *Blanks
064080051118     c                   Goto      emi02
064090051118    1c                   EndSl
064100051118
064110051118      * Nazione con cappario controllo Cap
064120051118if  1c                   If        o14cpp = *Blanks
064130051118     c                   Eval      i95tcn = '7'
064140051118     c                   Eval      i95cap = e14cap
064150051118     c                   Eval      i95loc = e14loc
064160051118     c                   Eval      i95prv = e14prv
064170051118     c                   Eval      i95nar = e14nar
064180051118     c                   Z-add     *date         i95dat
064190051118     c                   Eval      i13af0 = 'S'
064200051118     c                   Eval      i13cnv = 'S'
064210051118     c                   Eval      i13af1 = 'S'
064220051118     c                   Eval      i13la3 = 'S'
064230051118      * Se dati variati controllo se congruenti
064240051118if  2c                   If        e14cap <> wcae or e14loc <> wcit or
064250051118     c                             e14prv <> wprv or e14nar <> wsta
064260051118     c                   Clear                   wfz3
064270051118     c                   Eval      wcae = e14cap
064280051118     c                   Eval      wcit = e14loc
064290051118     c                   Eval      wprv = e14prv
064300051118     c                   Eval      wsta = e14nar
064310051118    2c                   EndIf
064320051118     c                   Eval      E13fz3 = wfz3
064330051118     c                   Call      'FNLV13R'
064340051118     c                   Parm                    kpjba
064350051118     c                   Parm                    fnlv13ds
064360051118     c                   Parm                    tisi95ds
064370051118
064380051118     c                   If        o13ron = 'S'
064390051118     c                   Eval      e14nar = o95nar
064400051118     c                   EndIf
064410051118     c                   If        o13roc = 'S'
064420051118     c                   Eval      e14cap = o95cap
064430051118     c                   EndIf
064440051118     c                   If        o13rop = 'S'
064450051118     c                   Eval      e14prv = o95prv
064460051118     c                   EndIf
064470051118     c                   If        o13rol = 'S'
064480051118     c                   Eval      e14loc = o95loc
064490051118     c                   EndIf
064500051118
064510051124      * Reimposto le forzature
064520051118     c                   Eval      wfz3 = E13fz3
064530051118
064540051118s   2c                   Select
064550051118     c                   When      o13err = '5'
064560051118     c                   Eval      *In28 = *On
064570051130     c                   Eval      h2riga = 09
064580051118     c                   Eval      h2colo = 18
064590051118     c                   Eval      v2cmsg = o13msg
064600051118     c                   Leavesr
064610051118     c                   When      o13err = '3'
064620051118     c                   Eval      *In28 = *On
064630051130     c                   Eval      h2riga = 09
064640051118     c                   Eval      h2colo = 28
064650051118     c                   Eval      v2cmsg = o13msg
064660051118     c                   Leavesr
064670051118     c                   When      o13err = '4'
064680051118     c                   Eval      *In28 = *On
064690051130     c                   Eval      h2riga = 09
064700051118     c                   Eval      h2colo = 59
064710051118     c                   Eval      v2cmsg = o13msg
064720051118     c                   Leavesr
064730051118     c                   When      o13err = '6'
064740051118     c                   Eval      *In28 = *On
064750051130     c                   Eval      h2riga = 09
064760051118     c                   Eval      h2colo = 62
064770051118     c                   Eval      v2cmsg = o13msg
064780051118     c                   Leavesr
064790051118     c                   When      o13err <> *Blanks and o13msg = *Blanks
064800051118     c                   Goto      emi02
064810051118    2c                   EndSl
064820051118
064830051118      * Se nazione senza cappario
064840051118   x1c                   Else
064850051122      * Il cliente deve essere bloccato con 8 (si può usare solo x fatturare e
064860051122      *                                        non per bollettare)
064870051118if  2c                   If        v2cabl <> '8'
064880051118     c                   Eval      *In28 = *On
064890051130     c                   Eval      h2riga = 09
064900051118     c                   Eval      h2colo = 62
064910051118     c                   Eval      v2cmsg = msg(15)
064920051118     c                   Leavesr
064930051118    2c                   EndIf
064940051118    1c                   EndIf
064950051118
064960051118      * Cap obsoleto
064970051118     c                   If        o95cf1 = 'O'
064980051118     c                   Eval      *In28 = *On
064990051130     c                   Eval      h2riga = 09
065000051118     c                   Eval      h2colo = 18
065010051118     c                   Eval      v2cmsg = msg(16)
065020051118     c                   Leavesr
065030051118     c                   EndIf
065040051118
065050051118     c                   EndSr
065060051118
065070051118      *------------------------------------------------------------------------*
065080051118      * CONTROLLO CODICE FISCALE
065090051118      *------------------------------------------------------------------------*
065100051118     c     Sr_Ctrcdf     BegSr
065110151014     c                   clear                   w_scfsta
065120100729
065130061030      * devo richiamare il nuovo driver fatto per controllare il codice
065140061030      * fiscale e la partita iva
065150080421      * rihciamo anche per codice vuoto, per controllare i casi
065160080421      *  di obbligo non inserimento
065170151014     c                   clear                   xcfiva1ds
065180061030     c                   eval      xcfivapar = v2ccdf
065190061030     c                   eval      xcfivanar = v2csta
065200061030     c                   eval      xcfivaprv = v2cprv
065210151014     c                   eval      xcfivacap = v2ccae
065220151014     c                   eval      xcfivaloc = v2ccit
065230151014     c                   eval      xcfivapiva= v2civa
065240151014     c                   call      'XCFIVAR1'
065250151014     c                   parm                    xcfiva1ds
065260080421     c
065270080421    1c                   if        xcfivaflg < *zeros
065280051118     c                   Eval      *In28 = *On
065290051202     c                   Eval      h2riga = 12
065300051118     c                   Eval      h2colo = 28
065310080421     c                   Eval      v2cmsg = xcfivamsg
065320051118     c                   Leavesr
065330080421    1c                   EndIf
065340080421
065350080421      * il codice fiscale è obbligatorio se sto inserendo un codice nuovo
065360080421      * cliente italia a partire dalla data decorrenza per p.iva sarà
065370080421      * obbligatorio non forzabile anche in caso di codice non nuovo
065380080421
065390080421      * Escludo i casi previsti di NON inserimento
065400080421    1c  n06              if        xcfivaflg<>9 and
065410080421     c                             v2csta=*blanks and v2ccdf=*blanks
065420080421     c
065430080421     c* prima di dare errore verifico se cliente del sottoconto intestazione
065440080421     c* fattura ha la nazione ed in questo caso non do errore
065450080421     c                   exsr      sr_repscfsta
065460080421    2c                   if        w_scfsta=*blanks
065470080421     c                   eval      *In28 = *On
065480080421     c                   eval      h2riga = 12
065490080421     c                   eval      h2colo = 28
065500080421     c                   eval      v2cmsg = msg(51)
065510080421     c                   leavesr
065520080421    2c                   endif
065530080421    1c                   endif
065540080306     c* Se provengo dalle visite e il relativo potenziale non ha il cod.fisc
065550080306     c* errore forzabile se inserito un codice fiscale già presente su altro
065560080306     c* potenziale
065570080421    1c                   if        *in06 and v2ccpo<>*zeros and v2ccdf<>*blanks
065580080306     c     v2ccpo        Chain(n)  Tncpo01l
065590080421    2c                   if        %found(tncpo01l) and cpocdf=*blanks
065600080306     c                             and v2ccdf<>savcdf
065610080306     c                             and v2ccdf<>cod_forzcdf
065620080306     c                   clear                   savrag
065630080307     c                   eval      codice=v2ccdf
065640080306     c                   exsr      sr_CdfAltroP
065650080306     c                   exsr      sr_msgcdf
065660080306     c   28              leavesr
065670080421    2c                   endif
065680080421    1c                   endif
065690051118
065700051118     c                   EndSr
065710051118
065720051118      *------------------------------------------------------------------------*
065730051118      * CONTROLLO PARTITA IVA E STATO
065740051118      *------------------------------------------------------------------------*
065750051118     c     Sr_Ctriva     BegSr
065751171213
065752171213        ClienteSofferenza = *off;
065760051118
065770051118      * STATO
065780051118      * --> ricerca
065790051118     c                   Eval      wpos = %scan('?':v2csta)
065800051118     c                   If        wpos > 0
065810051118     c                   Eval      kcod = '15'
065820051118     c                   call      'TRUL19R'
065830051118     c                   parm                    kcod
065840051118     c                   parm                    ordina
065850051118     c                   parm                    kkey
065860051118     c                   parm                    comando
065870051118     c                   Eval      v2csta = kkey
065880051130     c                   Eval      h2riga = 09
065890051118     c                   Eval      h2colo = 62
065900051124     c                   Eval      *In90 = *On
065910051118     c                   EndIf
065920051118      * --> controllo
065930051118     c                   Clear                   ds15
065940051118     c                   Eval      kcod = '15'
065950051118     c                   Eval      kkey = v2csta
065960051118     c     kTabel        Chain     Tabel00f
065970051118     c                   If        Not %Found(Tabel00f) or
065980051125     c                             tblflg <> *Blanks
065990051118     c                   Eval      *In28 = *On
066000051130     c                   Eval      h2riga = 09
066010051118     c                   Eval      h2colo = 62
066020051118     c                   Eval      v2cmsg = msg(17)
066030051118     c                   Leavesr
066040051118     c                   EndIf
066050051125     c                   Eval      ds15 = tbluni
066060051118
066070051129      * --> controllo
066080151014     c**                 If        v2civa <> *Blanks
066090061030      * devo richiamare il nuovo driver fatto per controllare il codice
066100061030      * fiscale e la partita iva
066110151014     c                   clear                   xcfiva1ds
066120061030     c                   eval      xcfivamod = 'P'
066130061030     c                   eval      xcfivapar = v2civa
066140061030     c                   eval      xcfivanar = v2csta
066150061030     c                   eval      xcfivaprv = v2cprv
066160151014     c                   eval      xcfivacap = v2ccae
066170151014     c                   eval      xcfivaloc = v2ccit
066180151014     c                   eval      xcfivapiva= v2civa
066190151014     c                   call      'XCFIVAR1'
066200151014     c                   parm                    xcfiva1ds
066210061106      * --> errata forzabile
066220061106     c                   If        xcfivaflg = -9 and wforzaiva = *off
066230061106     c                   Eval      *In28 = *On
066240061106     c                   Eval      h2riga = 12
066250061106     c                   Eval      h2colo = 52
066260061106     c                   Eval      v2cmsg = xcfivamsg
066270061106     c                   Eval      v2cmsg = %trim(v2cmsg) + ' Enter x forzare'
066280061106     c                   eval      wforzaiva = *on
066290061106     c                   Leavesr
066300061106     c                   EndIf
066310061030      * --> errore
066320061106     c                   if        xcfivaflg < *zeros and xcfivaflg <> -9
066330051118     c                   Eval      *In28 = *On
066340051202     c                   Eval      h2riga = 12
066350051118     c                   Eval      h2colo = 52
066360061030     c                   eval      v2cmsg = xcfivamsg
066370051118     c                   Leavesr
066380051118     c                   EndIf
066390061106      * se non impostato devo riportare il codice iso della partita iva che
066400061106      * mi viene passato dal pgm di controllo
066410061114     c                   if        v2ivaeu <> ivaeu
066420061106     c                   eval      v2ivaeu = ivaeu
066430061106     c                   eval      *in39 = *on
066440061106     c                   eval      *in90 = *on
066450061114     c                   endif
066460151014      * PARTITA IVA
066470151014      * --> obbligatoria (se non è gestione visite/offerte)
066480151014      * --> non è però obbligatoria se cliente Italia e nazione del
066490151014      *     sottoconto intestazione fattura estera
066500151014      * Escludo i casi previsti di NON inserimento
066510151014     c                   If        v2civa = *Blanks and Not *In06
066520151014     c                             and xcfivaflg<>9
066530151014     c                   exsr      sr_repscfsta
066540151014     c                   if        v2csta<>*blanks or w_scfsta=*blanks
066550151014     c                   Eval      *In28 = *On
066560151014     c                   Eval      h2riga = 12
066570151014     c                   Eval      h2colo = 52
066580151014     c                   Eval      v2cmsg = msg(18)
066590151014     c                   Leavesr
066600151014     c                   EndIf
066610151014     c                   EndIf
066620151014
066630151014     c* Se provengo dalle visite e il relativo potenziale non ha p.iva
066640080306     c* errore forzabile se inserito una p.iva         già presente su altro
066650080306     c* potenziale
066660151014     c                   If        v2civa <> *Blanks
066670080306     c                   if        *in06 and v2ccpo<>*zeros
066680080306     c                             and v2ivaeu<>'$$'
066690080306     c     v2ccpo        Chain(n)  Tncpo01l
066700080306     c                   if        %found(tncpo01l) and cpopiv=*blanks
066710080306     c                             and v2civa<>saviva
066720080306     c                             and v2civa<>cod_forziva
066730080306     c                   clear                   savrag
066740080307     c                   if        v2ivaeu<>*blanks
066750080306     c                   eval      codice=v2civa
066760080306     c                   else
066770080306     c                   eval      codice=v2ivait
066780080306     c                   endif
066790080306     c                   exsr      sr_PivAltroP
066800080306     c                   exsr      sr_msgiva
066810080306     c   28              leavesr
066820080306     c                   endif
066830080307     c                   endif
066840061106
066850060105      * --> controllo se c'è già un cliente con la stessa p.IVA
066851171221      *     faccio il controllo sempre se NON è anagrafica provvisoria
066852180119      *     in immissione e convalida offerte
066853180119     c                   IF        *in06
066854180119     c                   leavesr
066855180119     c                   ENDIF
066856180119     c                   IF        V2ivaeu = '$$'
066857180119     c                   leavesr
066858180119     c                   ENDIF
066870180119if  1c*****              If        Not *In06 and (*In01 or *In07)
066880180119     c*****                        and v2ivaeu <> '$$'
066881180119     c                   IF        *in01 or *in07 or
066882180119     c                             (saviva <> V2civa and *in02)
066890060109     c                   If        v2ivaeu = *Blanks
066900060109     c                   Movel     v2ivait       kindiva
066910060109     c                   Else
066920060109     c                   Movel     v2civa        kindiva
066930060109     c                   EndIf
066940060105     c     kCnind        Setll     Cnind02l
066950060105      * non trovo nessun altra partita IVA esco
066960060105     c                   If        Not %Equal
066970060105     c                   Leavesr
066980060105     c                   EndIf
066990060105do  2c                   Do        *Hival
067000060105     c     kCnind        Reade     Cnind02l
067010060105      * fine file esco
067020060105     c                   If        %Eof(Cnind02l)
067030060105     c                   Leave
067040060105     c                   EndIf
067050060105      * deve avere codice cliente diverso
067060060301     c                   If        ww_indksc = v1cksc
067070060105     c                   Iter
067080060105     c                   EndIf
067090060105      * controllo lo stato del credito
067100081006      * lo faccio con una routine per emettere una window nel caso di
067110081006      * messaggio info
067120081006     c                   exsr      sr_ctrstc
067130171213     c                   if        wforzacon = *On or
067131171213     c                             ClienteSofferenza
067140081006     c                   leavesr
067150081006     c                   endif
067160060105    2c                   EndDo
067170060105    1c                   EndIf
067180151015     c
067190151015     c                   EndIf
067200051118
067210051118     c                   EndSr
067220081006
067230081006      *------------------------------------------------------------------------*
067240081006      * CONTROLLO STATO DEL CREDITO
067250081006      *------------------------------------------------------------------------*
067260081006     c     sr_ctrstc     begsr
067270081006
067280081006     c                   Clear                   Tibs69ds
067290081006     c                   Move      ww_indksc     I69kcp
067300081006     c                   Call      'TIBS69R'
067310081006     c                   Parm                    Tibs69ds
067320081006     c                   Parm                    ds_cnaco
067330081006     c                   Parm                    ds_cnind
067340081006     c                   Parm                    ds_cnclp
067350081006     c                   Parm                    ds_fncls
067360081006     c                   Eval      wtibs69 = *On
067370081006      * errore forzabile per stato del credito trovato
067380081006      * emesso con window e f6 di conferma
067381171213      * se stato del credito è SOFFERENZA >= "41" devo BLOCCARE
067382171213      * uso il campo §4Wtip <> *blanks
067390081006     c                   If        w_clpcon <> *Blanks and wforzacon = *Off
067400090803      * decodifico stato del credito
067410090803     c                   clear                   w_ds4w
067420090803     c                   eval      kcod = '4W'
067430090803     c                   eval      kkey = w_clpcon
067440090803     c                   eval      w5ccon = %subst(w_clpcon:2:2)
067450090803     c     ktabel        chain     tabel00f
067460090803     c                   if        %found(tabel00f) and
067470090803     c                             tblflg = *blanks
067480090803     c                   eval      w_ds4w = tbluni
067490090803     c                   endIf
067500090803     c                   Eval      w5dcon = w_§4wdes
067510081006     c                   do        *hival
067520081006     c                   exfmt     ta60w05
067530081006     c   kf              leave
067540081006     c                   enddo
067541171213     c                   IF        w_§4Wtip = *blanks
067550081006     c                   Eval      wforzacon = *On
067551171213     c                   ELSE
067552171213     c                   eval      ClienteSofferenza = *on
067553171213     c                   ENDIF
067560081006     c                   EndIf
067570081006
067580081006     c                   endsr
067590051122
067600051122      *------------------------------------------------------------------------*
067610051122      * CONTROLLO BLOCCO CLIENTE
067620051122      *------------------------------------------------------------------------*
067630051122     c     Sr_Ctrblc     BegSr
067640051118
067650051122      * STATO DEL CREDITO
067660051122      * --> Ricerca
067670051122     c                   Clear                   v2dcon
067680051122     c                   Eval      wpos = %scan('?':v2ccon)
067690051122     c                   If        wpos > 0
067700051122     c                   Eval      kcod = '4W'
067710051122     c                   call      'TRUL19R'
067720051122     c                   parm                    kcod
067730051122     c                   parm                    ordina
067740051122     c                   parm                    kkey
067750051122     c                   parm                    comando
067760051124     c                   Eval      w003a = kkey
067770051124     c                   Move      w003a         v2ccon
067780051202     c                   Eval      h2riga = 17
067790051122     c                   Eval      h2colo = 28
067800051124     c                   Eval      *In90 = *On
067810051122     c                   EndIf
067820051122
067830051122      * --> Controllo
067840051122     c                   Clear                   v2dcon
067850051122     c                   Clear                   ds4w
067860051122     c                   Eval      kcod = '4W'
067870051124     c                   Move(p)   v2ccon        w003a
067880051124     c                   Eval      kkey = w003a
067890051122     c     kTabel        Chain     Tabel00f
067900051122     c                   If        Not %Found(Tabel00f) or
067910051125     c                             tblflg <> *Blanks
067920051122     c                   Eval      *In28 = *On
067930051202     c                   Eval      h2riga = 17
067940051122     c                   Eval      h2colo = 28
067950051122     c                   Eval      v2cmsg = msg(23)
067960051122     c                   Leavesr
067970051122     c                   EndIf
067980051125     c                   Eval      ds4w = tbluni
067990051122     c                   Eval      v2dcon = §4wdes
068000051122
068010051122      * Se sono in filiale devo usare solo stati del credito autorizzati e
068020051122      * senza passaggio al contenzioso
068030130325      * da 25/03/2013 abbiamo deciso che anche in sede non è modificale, solo
068040130325      * da ProJ
068050051122     c                   If        (§4wmbl = 'N' or §4wctz = 'S') and
068060130325     c                             (v2ccon <> %subst(clpcon:2:2) or *In01)
068070051122     c                   Eval      *In28 = *On
068080051202     c                   Eval      h2riga = 17
068090051122     c                   Eval      h2colo = 28
068100051122     c                   Eval      v2cmsg = msg(24)
068110051122     c                   Leavesr
068120051122     c                   EndIf
068130051122
068140051122      * --> Imposto i blocchi
068150051122      * Se sono in filiale proteggo lo stato del credito se non è modificabile
068160120928      * da p.o.
068170130325      * PER TUTTI
068180130325     c                   If        §4wmbl = 'N'
068190060220     c                   Eval      *In19 = *On
068200051122     c                   EndIf
068210120928      * Se sono in filiale proteggo il blocco pagamenti se non è modificabile
068220120928      * da p.o.
068230120928     c                   If        Not *In09 and §4wmbp = 'N'
068240120928     c                   Eval      *In81 = *On
068250120928     c                   EndIf
068260120925      * Se sono in filiale proteggo il blocco cliente se non è modificabile
068270120925      * da p.o.
068280120928     c                   If        Not *In09 and §4wmtb = 'N'
068290120925     c                   Eval      *In79 = *On
068300120925     c                   EndIf
068310121002
068320121002      * se stato del credito variato
068330121002     c                   IF        V2Ccon <> %subst(CLPcon:2:2)
068340051122      * Se il cliente non è bloccato e lo stato del credito prevede il blocco
068350051122      * lo faccio io ora in automatico
068360051122     c                   If        v2cabl = *Blanks and §4wtbl <> *Blanks
068370051122     c                   Eval      v2cabl = §4wtbl
068380130322     c                   eval      V2Cabledp = §4wtbl
068390051122     c                   EndIf
068400051122
068410051122      * Se non c'è la causale del blocco la imposto io ora in automatico
068420051122     c                   If        (v2cblc = *Blanks or v2cblc = *Zeros) and
068430051122     c                              §4wblc <> *Blanks
068440051122     c                   Eval      v2cblc = §4wblc
068450051122     c                   EndIf
068460121002     c                   ENDIF
068470051122
068480051122      * BLOCCO CLIENTE
068490130322     c                   IF        V2Cabl <> *blanks and V2Cabl <> '8'
068500130322     c                             and not *in79
068510130322     c                   Eval      *In28 = *On
068520130322     c                   Eval      h2riga = 18
068530130322     c                   Eval      h2colo = 28
068540130322     c                   Eval      V2Cmsg = 'Blocco cliente errato'
068550130322     c                   Leavesr
068560130322     c                   ENDIF
068570051122      * --> ricerca
068580051122     c                   Clear                   v2dblc
068590051122     c                   Eval      wpos = %scan('?':v2cblc)
068600051122     c                   If        wpos > 0
068610121127     c                   goto      no19
068620051122     c                   Eval      kcod = 'BI'
068630051122     c                   call      'TRUL19R'
068640051122     c                   parm                    kcod
068650051122     c                   parm                    ordina
068660051122     c                   parm                    kkey
068670051122     c                   parm                    comando
068680051122     c                   Eval      v2cblc = kkey
068690121127     c     no19          tag
068700121127     c                   clear                   kpjbu
068710121127     c                   call      'TRTBBIR'
068720121127     c                   parm                    kpjba
068730121127     c                   eval      v2cblc = %subst(kpjbu:1:3)
068740051202     c                   Eval      h2riga = 18
068750051122     c                   Eval      h2colo = 42
068760051124     c                   Eval      *In90 = *On
068770051122     c                   EndIf
068780051122
068790051122      * --> Controllo
068800051122     c                   If        v2cblc <> *Blanks
068810110323     c                   Clear                   dsbi
068820051122     c                   Clear                   v2dblc
068830051122     c                   Eval      kcod = 'BI'
068840051122     c                   Eval      kkey = v2cblc
068850051122     c     kTabel        Chain     Tabel00f
068860051122     c                   If        Not %Found(Tabel00f) or
068870051125     c                             tblflg <> *Blanks
068880051122     c                   Eval      *In28 = *On
068890051202     c                   Eval      h2riga = 18
068900051122     c                   Eval      h2colo = 42
068910051122     c                   Eval      v2cmsg = msg(25)
068920051122     c                   Leavesr
068930051122     c                   EndIf
068940110323     c                   Eval      dsbi = tbluni
068950110323     c                   Eval      v2dblc = §bides
068960121127      * causale blocco non piuù utilizzabile
068970121127     c                   IF        §BIuso = 'N'
068980121127     c                   Eval      *In28 = *On
068990121127     c                   Eval      h2riga = 18
069000121127     c                   Eval      h2colo = 42
069010121127     c                   Eval      v2cmsg = 'Causale non più utilizzabile'
069020121127     c                   Leavesr
069030121127     c                   ENDIF
069040051124     c                   EndIf
069050051122
069060051122      * Se cliente bloccato la causale blocco è obbligatoria
069070051122     c                   If        v2cabl <> *Blanks and v2cblc = *Blanks
069080130325     c                             and v2cabl <> '*'
069090051122     c                   Eval      *In28 = *On
069100051202     c                   Eval      h2riga = 18
069110051122     c                   Eval      h2colo = 42
069120051122     c                   Eval      v2cmsg = msg(26)
069130051122     c                   Leavesr
069140051122     c                   EndIf
069150051122
069160051122      * Se immessa la causale blocco il cliente deve essere bloccato
069170051122     c                   If        v2cabl = *Blanks and v2cblc <> *Blanks
069180051122     c                   Eval      *In28 = *On
069190051202     c                   Eval      h2riga = 18
069200051122     c                   Eval      h2colo = 28
069210051122     c                   Eval      v2cmsg = msg(27)
069220051122     c                   Leavesr
069230051122     c                   EndIf
069240110407
069250110407      * Se pgm richiamato per blocco cliente il blocco deve esserci
069260110407     c                   If        $Blocco and V2cabl = *blanks
069270110407     c                   Eval      *In28 = *On
069280110407     c                   Eval      h2riga = 18
069290110407     c                   Eval      h2colo = 28
069300110407     c                   Eval      v2cmsg = 'Immettere il blocco cliente'
069310110407     c                   Leavesr
069320110407     c                   EndIf
069330051122
069340051122     c                   EndSr
069350090616
069360090616      *------------------------------------------------------------------------*
069370090616      * CONTROLLO SOLLECITO
069380090616      *------------------------------------------------------------------------*
069390090616     c     sr_ctrsol     begsr
069400090616
069410090616     c                   reset                   trulsoli1
069420090616     c                   eval      trulsoli1.kut = codut
069430090616     c                   eval      trulsoli1.kcc = v1ckcc
069440090616     c                   eval      trulsoli1.ksc = v1cksc
069450090616     c                   evalr     trulsoli1.con = v2ccon
069460090616     c                   eval      rqsdatasize = %size(trulsoli1)
069470090616     c                   eval      rpydatasize = %size(trulsolo1)
069480090616     c                   call      'TRULSOLR'
069490090616     c                   parm      *blanks       rqsopcode
069500090616     c                   parm      *zeros        rpyesito
069510090616     c                   parm      'TRULSOLI1'   rqsformatname
069520090616     c                   parm                    trulsoli1
069530090616     c                   parm                    rqsdatasize
069540090616     c                   parm      'TRULSOLO1'   rpyformatname
069550090616     c                   parm                    trulsolo1
069560090616     c                   parm                    rpydatasize
069570090617
069580090617      * Immissione
069590090617     c                   if        *in01
069600090617     c                   if        rpyesito = *zeros
069610090617     c                   eval      v2csol = trulsolo1.sol
069620090617     c                   else
069630090617     c                   eval      v2csol = 'S'
069640090617     c                   endif
069650090617     c                   endif
069660090617
069670090617      * Modifica
069680090617     c                   if        rpyesito = *zeros and not *in01 and
069690090617     c                             v2csol <> trulsolo1.sol and
069700090617     c                             trulsolo1.solmod = 'N'
069710090617     c                   Eval      *In28 = *On
069720111115     c                   Eval      h2riga = 19
069730090617     c                   Eval      h2colo = 28
069740090617     c                   if        trulsolo1.sol = 'S'
069750090617     c                   Eval      v2cmsg = msg(83)
069760090617     c                   Leavesr
069770090617     c                   else
069780090617     c                   Eval      v2cmsg = msg(84)
069790090617     c                   Leavesr
069800090617     c                   endif
069810090617     c                   endif
069820090616
069830090616     c                   endsr
069840051122
069850051118      *------------------------------------------------------------------------*
069860051118      * CONTROLLO CODICE COMMERCIALE
069870051118      *------------------------------------------------------------------------*
069880051118     c     Sr_Ctrage     BegSr
069890051118
069900051118      * Ricerca
069910051130     c                   Clear                   v3dage
069920051130     c                   Eval      wpos = %scan('?':v3cage)
069930051118     c                   If        wpos > 0
069940051130     c                   Eval      v3cage = *All'0'
069950130808     c                   clear                   TRMK43ds
069960130806     c  n06              movel     v1cksc        iMK43fil
069970130806     c   06              movel     ta60cli       iMK43fil
069980130806     c                   call      'TRMK43R'
069990130806     c                   parm                    kpjba
070000130806     c                   parm                    TRMK43ds
070010130806     c                   if        oMK43err = *off  and  oMK43fxx = *blank
070020130806     c                   movel     oMK43cmm      v3cage
070030130806     c                   eval      v3dage = oMK43des
070040130806     c                   endif
070050051130     c                   Eval      h3riga = 07
070060051130     c                   Eval      h3colo = 28
070070051124     c                   Eval      *In90 = *On
070080051118     c                   EndIf
070090051122
070100051118      * Controllo (obbligatorio)
070110051130     c                   Clear                   v3dage
070120130806     c                   If        %check(digits:V3Cage) > 0
070130130806     c                   eval      *In28 = *On
070140130806     c                   eval      h3riga = 07
070150130806     c                   eval      h3colo = 28
070160130806     c                   eval      V3Cmsg = Msg(20)
070170130806     c                   leavesr
070180130806     c                   EndIf
070190130806     c                   move      V3Cage        CMMcod
070200130806     c     CMMcod        chain     AZCMM000
070210130806     c                   If        Not %Found(AZCMM01L)
070220051122     c                   Eval      *In28 = *On
070230051130     c                   Eval      h3riga = 07
070240051130     c                   Eval      h3colo = 28
070250051130     c                   Eval      v3cmsg = msg(20)
070260051122     c                   Leavesr
070270051122     c                   EndIf
070280130806     c                   eval      V3Dage = CMMdes
070290051122
070300051122      * Codice commerciale vari valido solo per 8888 e 9999
070310051130     c                   Move      v3cage        w0040
070320130806     c                   IF        CMMpar = '1' and wksc04 <> 8888
070330130806     c                                          and wksc04 <> 9999
070340051122     c                   Eval      *In28 = *On
070350051130     c                   Eval      h3riga = 07
070360051130     c                   Eval      h3colo = 28
070370051130     c                   Eval      v3cmsg = msg(20)
070380051122     c                   Leavesr
070390051122     c                   EndIf
070400110216
070410110216      /free
070420111123       //?Commerciale "valido" data inizio e fine attività
070430130806         IF  CMMdtIni > dataoggi  or
070440130806             CMMdtFin < dataoggi;
070450111123           *in28 = *on;
070460111123           H3riga = 07;
070470111123           H3colo = 28;
070480111123           V3Cmsg = msg(20);
070490111123           leavesr;
070500111123         ENDIF;
070510111123
070520110216       //?Commerciale 'inattivi' possibile solo se
070530110217       //?immissione
070540110216       //?utente EDP
070550110216       //?cliente incassi da attribuire --> 0001 x 1°livello
070560110216       //?                              --> 1000 x 2°livello
070570110217       //?oppure se
070580110217       //?manutenzione
070590110217       //?NO anagrafica provvisoria
070600110217       //?cliente incassi da attribuire = a cod. in tab Y4O
070610130806         IF  CMMpar = '2';
070620110217           SELECT;
070630110217             WHEN  *in02 and not *in06 and %editc(V1Cksc:'X') = wCodIncAtt;
070640160429             WHEN  *in01 and *in20 and
070650121121                   ((sav_livello = '1' and wksc04 = 0001) or
070660121121                    (sav_livello = '2' and wksc04 = 1000) or
070670121121                    (sav_livello = *blanks and wCodIncAtt = *blanks and
070680121121                    (wksc04 = 1000  or wksc04 = 0001)));
070690110217             OTHER;
070700110216              *in28 = *on;
070710110216              H3riga = 07;
070720110216              H3colo = 28;
070730110216              V3Cmsg = msg(20);
070740110216              leavesr;
070750110217           ENDSL;
070760110217         ENDIF;
070770110216      /end-free
070780051122
070790051122      * P.o. del commerciale congruente con p.o. del cliente
070800051221      * solo se non è gestione anagrafica provvisoria di un nuovo cliente
070810051221     c                   If        Not *In06 or ta60cli > *Zeros
070820051122     c  n06              Movel     v1cksc        w0030
070830051122     c   06              Movel     ta60cli       w0030
070840051130     c                   Movel     v3cage        wpoage
070850051122     c                   If        w0030 <> wpoage
070860051122     c                   Eval      *In28 = *On
070870051130     c                   Eval      h3riga = 07
070880051130     c                   Eval      h3colo = 28
070890051130     c                   Eval      v3cmsg = msg(21)
070900051122     c                   Leavesr
070910051122     c                   EndIf
070920051221     c                   EndIf
070930051122
070940051122      * Se commerciale xxx0999 (cliente inattivo) il cliente va bloccato
070950110216      * salto il controllo
070960110216      * il commerciale 'inattivi' non va usato
070970110216     c                   leavesr
070980051130     c                   Move      v3cage        w0040
070990051122     c                   If        w0040 = 999 and v2cabl = *Blanks
071000051122     c                   Eval      *In28 = *On
071010051130     c                   Eval      v3cmsg = msg(22)
071020051122     c                   Leavesr
071030051122     c                   EndIf
071040051118
071050051118     c                   EndSr
071060051122
071070051122      *------------------------------------------------------------------------*
071080051122      * CONTROLLO CODICE IMPORTANZA CLIENTE
071090051122      *------------------------------------------------------------------------*
071100051122     c     Sr_Ctrclv     BegSr
071110100301
071120100301     c                   Clear                   v3dclv
071130100301
071140100301      * se anagrafica provvisoria da trattativa
071150100301      * non è obbligatorio inserire il codice, se a blank vado a fine controlli
071160100806     c                   if        *in06 and v3cclv = *blanks
071170100301     c                   leavesr
071180100301     c                   endif
071190051122
071200051122      * Ricerca
071210051130     c                   Eval      wpos = %scan('?':v3cclv)
071220051122     c                   If        wpos > 0
071230051122     c                   Eval      kcod = 'IC'
071240051122     c                   call      'TRUL19R'
071250051122     c                   parm                    kcod
071260051122     c                   parm                    ordina
071270051122     c                   parm                    kkey
071280051122     c                   parm                    comando
071290051130     c                   Eval      v3cclv = kkey
071300051130     c                   Eval      h3riga = 08
071310051130     c                   Eval      h3colo = 28
071320051124     c                   Eval      *In90 = *On
071330051122     c                   EndIf
071340051122
071350051124      * --> Controllo (obbligatorio)
071360051130     c                   If        v3cclv = *Blanks
071370051124     c                   Eval      *In28 = *On
071380051130     c                   Eval      h3riga = 08
071390051130     c                   Eval      h3colo = 28
071400051130     c                   Eval      v3cmsg = msg(28)
071410051124     c                   Leavesr
071420051124     c                   EndIf
071430051124
071440051130     c                   Clear                   v3dclv
071450051122     c                   Clear                   dsic
071460051122     c                   Eval      kcod = 'IC'
071470051130     c                   Eval      kkey = v3cclv
071480051122     c     kTabel        Chain     Tabel00f
071490051122     c                   If        Not %Found(Tabel00f) or
071500051125     c                             tblflg <> *Blanks
071510051122     c                   Eval      *In28 = *On
071520051130     c                   Eval      h3riga = 08
071530051130     c                   Eval      h3colo = 28
071540051130     c                   Eval      v3cmsg = msg(29)
071550051122     c                   Leavesr
071560051122     c                   EndIf
071570051125     c                   Eval      dsic = tbluni
071580051130     c                   Eval      v3dclv = §sicde
071590051122
071600051122     c                   EndSr
071610051122
071620051122      *------------------------------------------------------------------------*
071630051122      * CONTROLLO CODICE ISTAT
071640051122      *------------------------------------------------------------------------*
071650051122     c     Sr_Ctritc     BegSr
071660051122
071670051122      * Ricerca
071680051130     c                   Clear                   v3ditc
071690051130     c                   Eval      wpos = %scan('?':v3citc)
071700051122     c                   If        wpos > 0
071710051122     c                   Eval      kcod = '1L'
071720051122     c                   call      'TRUL19R'
071730051122     c                   parm                    kcod
071740051122     c                   parm                    ordina
071750051122     c                   parm                    kkey
071760051122     c                   parm                    comando
071770051130     c                   Eval      v3citc = kkey
071780051130     c                   Eval      h3riga = 09
071790051130     c                   Eval      h3colo = 28
071800051124     c                   Eval      *In90 = *On
071810051122     c                   EndIf
071820051122
071830051122      * --> Controllo (obbligatorio)
071840051130     c                   If        v3citc = *Blanks or v3citc = *Zeros
071850051122     c                   Eval      *In28 = *On
071860051130     c                   Eval      h3riga = 09
071870051130     c                   Eval      h3colo = 28
071880051130     c                   Eval      v3cmsg = msg(30)
071890051122     c                   Leavesr
071900051122     c                   EndIf
071910051122
071920051130     c                   Clear                   v3ditc
071930051122     c                   Clear                   ds1l
071940051124     c                   Eval      kcod = '1L'
071950051130     c                   Eval      kkey = v3citc
071960051122     c     kTabel        Chain     Tabel00f
071970051122     c                   If        Not %Found(Tabel00f) or
071980051125     c                             tblflg <> *Blanks
071990051122     c                   Eval      *In28 = *On
072000051130     c                   Eval      h3riga = 09
072010051130     c                   Eval      h3colo = 28
072020051130     c                   Eval      v3cmsg = msg(31)
072030051122     c                   Leavesr
072040051122     c                   EndIf
072050051125     c                   Eval      ds1l = tbluni
072060051130     c                   Eval      v3ditc = §1ldes
072070051122
072080051122     c                   EndSr
072090140722
072100140722      /free
072110140722
072120140722       //--------------------------------------------------------------
072130140722       //?Controllo Note 02/03       (News/Nominativo),                ?
072140140722       //?               05/06/T5/I5 (Responsabile Trasporti),         ?
072150140722       //?               07/08/T7/I7 (Responsabile Logistica).         ?
072160140722       //--------------------------------------------------------------
072170140722       BEGSR  sr_CtrNote;
072180140722
072190140722         // -?Se NON selezionate: esco?
072200140722         select;
072210140722           when  wRGR  = 'RN'  and  V3Cnt02 = *blank;
072220140722             leavesr;
072230140722           when  wRGR  = 'RT'  and  V3Cnt05 = *blank;
072240140722             leavesr;
072250140722           when  wRGR  = 'RL'  and  V3Cnt07 = *blank;
072260140722             leavesr;
072270140722         endsl;
072280140722
072290140722         // -?Se sono in interrogazione non posso interrogare?
072300140722         //  ?   se non ho dati?
072310140722         if  *in05  and  ( (wRGR = 'RN'  and  Not *in24)  or
072320140722                           (wRGR = 'RT'  and  Not *in26)  or
072330140722                           (wRGR = 'RL'  and  Not *in31) );
072340140722           *in28 = *on;
072350140722           select;
072360140722             when  wRGR = 'RN';
072370140722               H3riga = 15;
072380140722             when  wRGR = 'RT';
072390140722               H3riga = 17;
072400140722             when  wRGR = 'RL';
072410140722               H3riga = 19;
072420140722           endsl;
072430140722           H3colo = 35;
072440140722           V3Cmsg = Msg(52);
072450140722           leavesr;
072460140722         endif;
072470140722
072480140722         // -?Imposto se interrogazione o gestione?
072490140722         if  Not *in05;
072500140722           wRich = 'M';
072510140722         else;
072520140722           wRich = 'V';
072530140722         endif;
072540140722         wRag  = V3Crag;
072550140722         clear  wTnt;
072560140722         //wRGR  = 'RN'       ?(già impostato)?
072570140722
072580140722         exsr  sr_CallTNTA12;
072590140722
072600140722         if  TA12err <> *blank;
072610140722           *in28 = *on;
072620140722           select;
072630140722             when  wRGR = 'RN';
072640140722               H3riga = 15;
072650140722             when  wRGR = 'RT';
072660140722               H3riga = 17;
072670140722             when  wRGR = 'RL';
072680140722               H3riga = 19;
072690140722           endsl;
072700140722           H3colo = 35;
072710140722           V3Cmsg = TA12msg;
072720140722           leavesr;
072730140722         endif;
072740140722
072750140722         // -?Controllo se ora c'è o non c'è più alcuna nota?
072760140722         kNtcNk1 = %editc( DUTkci : 'X' );
072770140722         if  *in70  or  Not *in06;
072780140722           kNtcApl = 'C';
072790140722           kNtcNk1 = %trim(kNtcNk1) + %editc( V1Cksc : 'X' );
072800140722         else;
072810140722           kNtcApl = 'T';
072820140722           kNtcNk1 = %trim(kNtcNk1) + %editc( V1Cnrv : 'X' );
072830140722         endif;
072840140722         clear  kNtcNk2;
072850140722         select;
072860140722           // -?Note 02/03?
072870140722           when  wRGR  = 'RN';
072880140722             *in24 = *off;
072890140722             exsr  sr_CarNoteRN;
072900140722           // -?Note 05/06/T5/I5?
072910140722           when  wRGR  = 'RT';
072920140722             *in26 = *off;
072930140722             exsr  sr_CarNoteRT;
072940140722           // -?Note 07/08/T7/I7?
072950140722           when  wRGR  = 'RL';
072960140722             *in31 = *off;
072970140722             exsr  sr_CarNoteRL;
072980140722         endsl;
072990140722
073000140722         // -?Uscita?
073010140722         select;
073020140722           when  wRGR  = 'RN';
073030140722             clear  V3Cnt02;
073040140722             H3riga = 15;
073050140722           when  wRGR  = 'RT';
073060140722             clear  V3Cnt05;
073070140722             H3riga = 17;
073080140722           when  wRGR  = 'RL';
073090140722             clear  V3Cnt07;
073100140722             H3riga = 19;
073110140722         endsl;
073120140722         H3colo = 35;
073130140722
073140140722         *in90 = *on;
073150140722
073160140722       ENDSR;
073170140722
073180140722       //--------------------------------------------------------------
073190140722       //?Visualizzazione Contatti/Rubrica del cliente.                ?
073200140722       //--------------------------------------------------------------
073210140722       BEGSR  sr_CallTNTA12;
073220140722
073230140722         clear  TNTA12ds;
073240140722
073250140722         // -?Impostazione parametri?
073260140722         TA12ric  = wRich;
073270140722         if  *in70  or  Not *in06;
073280140722           TA12apl  = 'C';
073290140722           TA12ksc  = V1Cksc;
073300140722         else;
073310140722           TA12apl  = 'T';
073320140722           TA12nrv  = V1Cnrv;
073330140722         endif;
073340140722         TA12rag  = wRag;
073350140722         TA12tnt  = wTnt;
073360140722         TA12gcli = '1';
073370140722         if  *in01   or
073380140722            (Not *in01  and  Not *in05  and
073390140722             %parms() > 1    and
073400140722             TA60icli = '1');
073410140722           TA12icli = '1';
073420140722           TA12cmm  = %int(V3Cage);
073430140722         endif;
073440140722         TA12rgr  = wRGR;
073450140722
073460140722         // -?Richiamo *pgm TNTA12R?
073470140722         tnta12r ( KPJBA : TNTA12ds );
073480140722
073490140722       ENDSR;
073500140722
073510140722      /end-free
073520051125
073530051125      *------------------------------------------------------------------------*
073540051125      * CONTROLLO CODICE SOTTOCONTO FATTURAZIONE
073550051125      *------------------------------------------------------------------------*
073560051125     c     Sr_Ctrscf     BegSr
073570100127
073580100127      * gestisco il '?'
073590100729     c                   IF        %scan('?':v4cscf) > 0
073600100312      * e se c'è la partita IVA e non è $$
073610100312     c                             and v2civa <> *blanks and
073620100312     c                             v2ivaeu <> '$$'
073630100128     c                   exsr      sr_ta40
073640100127     c                   eval      h4riga = 07
073650100127     c                   eval      h4colo = 36
073660100127     c                   eval      *in90 = *on
073670100312     c                   leavesr
073680100127     c                   ENDIF
073690051125
073700051221      * Obbligatorio
073710100128     c                   If        (v4cscf = *Zeros or v4cscf = *blanks)
073720051221      * se non sono in gestione anagrafica provvisoria
073730051221     c                             and Not *In06
073740051125     c                   Eval      *In28 = *On
073750051130     c                   Eval      h4riga = 07
073760051130     c                   Eval      h4colo = 36
073770051130     c                   Eval      v4cmsg = msg(33)
073780051125     c                   Leavesr
073790051125     c                   EndIf
073800100127
073810100127      * deve essere numerico
073820100127     c                   IF        %check(digits:V4cscf) > 0
073830100127     c                   eval      *in28 = *on
073840100127     c                   eval      h4riga = 07
073850100127     c                   eval      h4colo = 36
073860100127     c                   eval      v4cmsg = msg(35)
073870100127     c                   leavesr
073880100127     c                   ENDIF
073890100316
073900100316      * se sono in immissione (non da trattativa)
073910100316      * non controllo esistenza del codice sottoconto fattura se = al
073920100316      * codice cliente che sto inserendo
073930170516      // non faccio questo controllo se cliente LOGISTICA
073940170516        IF  not ClienteLog;
073950100316     c                   if        *in01 and not *in06 and
073960100316     c                             %dec(v4cscf:7:0) = v1cksc
073970100316     c                   eval      v4dscf = v2crag
073980100316     c                   leavesr
073990100316     c                   endif
074000170516       ENDIF;
074010051125
074020051125      * Controllo se modificato il codice
074030170117      * o se convalida trattativa
074040100127     c                   If        v4cscf <> savscf
074050170117     c                             or *in07
074060051130     c                   Clear                   v4dscf
074070051125      * Controllo
074080051125     c                   Clear                   Tibs69ds
074090051130     c                   Move      v4cscf        I69kac
074100161129     c                   Move      v4cscf        I69kcp
074110051125     c                   Call      'TIBS69R'
074120051125     c                   Parm                    Tibs69ds
074130051125     c                   Parm                    ds_cnaco
074140051125     c                   Parm                    ds_cnind
074150051125     c                   Parm                    ds_cnclp
074160051125     c                   Parm                    ds_fncls
074170051125     c                   Eval      wtibs69 = *On
074180051125
074190051125      * Il sottoconto deve esistere
074200051125     c                   If        o69err <> *Blanks
074210051125     c                   Eval      *In28 = *On
074220051130     c                   Eval      h4riga = 07
074230051130     c                   Eval      h4colo = 36
074240051130     c                   Eval      v4cmsg = msg(35)
074250051125     c                   Leavesr
074260051125     c                   EndIf
074270051125
074280051125      * Non deve essere annullato
074290051125     c                   If        w_acoflg <> *Blanks
074300051125     c                   Eval      *In28 = *On
074310051130     c                   Eval      h4riga = 07
074320051130     c                   Eval      h4colo = 36
074330051130     c                   Eval      v4cmsg = msg(36)
074340051125     c                   Leavesr
074350051125     c                   EndIf
074360051125
074370051125      * Non si può inserire un sottoconto 8888 o 9999
074380070704      * E' possibile solo per clienti 8888 e 9999
074390070704     c                   Move      v4cscf        w0040
074400070704     c                   move      v1cksc        w0040ksc
074410070704     c                   if        *in06='1'
074420070704     c                             or (w0040ksc=8888 and w0040<>8888)
074430070704     c                             or (w0040ksc=9999 and w0040<>9999)
074440070704     c                             or (w0040ksc<>8888 and w0040ksc<>9999)
074450051125     c                   If        w0040 = 8888 or w0040 = 9999
074460051125     c                   Eval      *In28 = *On
074470051130     c                   Eval      h4riga = 07
074480051130     c                   Eval      h4colo = 36
074490051130     c                   Eval      v4cmsg = msg(34)
074500051125     c                   Leavesr
074510051125     c                   EndIf
074520070704     c                   EndIf
074530161130      /free
074540161130       //?Non posso immettere un codice sottoconto fattura in contenzioso
074550170117        IF  %dec(v4cscf:7:0) <> v1cksc;
074560170117          clear TIBS69DS;
074570170117          I69kcp = %dec(V4Cscf:7:0);
074580170117          tibs69r (TIBS69DS:ds_CNACO:ds_CNIND:ds_CNCLP:ds_FNCLS);
074590170117          wtibs69 = *on;
074600170117          IF  w_CLPcon <> *blanks;
074610170117            clear w_ds4W;
074620170117            kcod = '4W';
074630170117            kkey = w_CLPcon;
074640170117            chain (codut:kcod:kkey) TABEL00F;
074650170117            IF  %found(TABEL00F) and TBLflg = *blanks;
074660170117              w_ds4W = TBLuni;
074670170117            ENDIF;
074680170117            IF  w_§4Wtip <> *blanks;
074690170117              *in28 = *on;
074700170117              H4riga = 07;
074710170117              H4colo = 36;
074720170117              V4Cmsg = msg(73);
074730170117              leavesr;
074740170117            ENDIF;
074750170117          ENDIF;
074760161130        ENDIF;
074770161130      /end-free
074780051125
074790051125      * Cerco il network del codice
074800051130     c                   Movel     v4cscf        w0030
074810051125     c                   Clear                   og143
074820051125     c     w0030         Chain     Azorg01l
074830051125     c                   If        %Found(Azorg01l)
074840051125     c                   Eval      og143 = orgde3
074850051125     c                   EndIf
074860120806      * Se non trovo il network e sono in anafrafica provvisoria vado avanti
074870120806     c                   If        not %found(AZORG01L) and not *in06
074880120806     c                             and Not *In06
074890120806     c                   Eval      *In28 = *On
074900120806     c                   Eval      h4riga = 07
074910120806     c                   Eval      h4colo = 36
074920120806     c                   Eval      v4cmsg = msg(35)
074930120806     c                   Leavesr
074940120806     c                   ENDIF
074950081119     c                   eval      savntwscf = §ogntw
074960170516
074970170516       // Per i clienti LOG come sottoconto intestazione fattura accetto
074980170516       // solo codici di spedizione BRT
074990170516         IF  ClienteLog and §OGntw = 'LOG';
075000170516           *in28 = *on;
075010170516           H4riga = 07;
075020170516           H4colo = 36;
075030170516           V4Cmsg = msg(34);
075040170516           leavesr;
075050170516         ENDIF;
075060160905
075070160905      * Se non sono autorizzato non posso utilizzare lienti con ntw LOG
075080160905     c                   IF         §UTEKscLog <> 'S' and
075090160905     c                              §OGntw = 'LOG'
075100160905     c                   Eval      *In28 = *On
075110160905     c                   Eval      h4riga = 07
075120160905     c                   Eval      h4colo = 36
075130160905     c                   Eval      v4cmsg = msg(34)
075140160905     c                   Leavesr
075150160905     c                   ENDIF
075160051125
075170051125      * Se sono in filiale non posso inserire codici con ntw LOG o XXX
075180051125     c                   If        Not *In09 and
075190160905     c                             §ogntw = 'XXX'
075200051125     c                   Eval      *In28 = *On
075210051130     c                   Eval      h4riga = 07
075220051130     c                   Eval      h4colo = 36
075230051130     c                   Eval      v4cmsg = msg(34)
075240051125     c                   Leavesr
075250051125     c                   EndIf
075260051220
075270051220      * Non posso modificare un sottoconto fattura con p.o. 046
075280051220     c                   Movel     savscf        w0030
075290051220     c                   If        w0030 = 046
075300051220     c                   Eval      *In28 = *On
075310051220     c                   Eval      h4riga = 07
075320051220     c                   Eval      h4colo = 36
075330051220     c                   Eval      v4cmsg = msg(71)
075340051220     c                   Leavesr
075350051220     c                   EndIf
075360051220
075370081119      * Non posso inserire un sottoconto fattura con ntw LOG se ksc non
075380081119      * ha ntw LOG
075390081119     c                   Movel     v1cksc        w0030
075400081119     c                   Clear                   og143
075410081119     c     w0030         Chain     Azorg01l
075420081119     c                   If        %Found(Azorg01l)
075430081119     c                   Eval      og143 = orgde3
075440081119     c                   EndIf
075450081119     c                   if        §ogntw <> 'LOG' and savntwscf = 'LOG'
075460081119     c                   Eval      *In28 = *On
075470081119     c                   Eval      h4riga = 07
075480081119     c                   Eval      h4colo = 36
075490081119     c                   Eval      v4cmsg = msg(34)
075500081119     c                   Leavesr
075510081119     c                   EndIf
075520100127
075530100127      * devo ricontrollare la partita iva e il codice fiscale
075540100127      * perchè i controlli di questi 2 dati sono legati anche al codice
075550100127      * fatturazione
075560100127     c                   exsr      sr_ctrCDF
075570100127      * se torna errore avviso
075580100127     c                   IF        *in28
075590100127     c                   eval      v4cmsg = 'Messaggi in sospeso su -
075600100127     c                             altre videate.'
075610100127     c                   leavesr
075620100127     c                   ENDIF
075630100127     c                   exsr      sr_ctrIVA
075640100127      * se torna errore avviso
075650100127     c                   IF        *in28
075660100127     c                   eval      v4cmsg = 'Messaggi in sospeso su -
075670100127     c                             altre videate.'
075680100127     c                   leavesr
075690100127     c                   ENDIF
075700051125
075710051125      * Decodifico
075720051130     c                   Eval      v4dscf = w_acorag
075730051130     c                   Eval      savscf = v4cscf
075740051128
075750051125     c                   EndIf
075760051125
075770051125     c                   EndSr
075780051128
075790051128      *------------------------------------------------------------------------*
075800051128      * CONTROLLO FLAG FATTURA UNIFICATA
075810051128      *------------------------------------------------------------------------*
075820051128     c     Sr_Ctruni     BegSr
075830100127
075840100127     c                   move      v4cscf        w0070
075850051128
075860051128      * Se il codice sottoconto fatturazione è diverso dal codice cliente e
075870051128      * il flag di unificazione fattura non è stato impostato emetto un msg. inf
075880100127     c                   If        w0070  <> v1cksc and v4cuni = *Blanks and
075890100127     c                             wfscf = *Blanks and w0070  <> *Zeros
075900051128      * e non è da offerta
075910051128     c                             and not *In06
075920051128     c                   Eval      *In28 = *On
075930051130     c                   Eval      h4riga = 08
075940051130     c                   Eval      h4colo = 36
075950051130     c                   Eval      v4cmsg = msg(45)
075960051128     c                   Eval      wfscf = '1'
075970051128     c                   Leavesr
075980051128     c                   EndIf
075990051128
076000051128      * se modificato flag fattura unificata msg info x controllare anche il
076010051128      * flag nel sottoconto fattura
076020051128      * se immissione messaggio solo se codice cliente diverso da sottoconto fat
076030051128      * se manutenzione diversifico i messaggi
076040051130     c                   If        v4cuni <> savuni
076050051130     c                   Eval      savuni = v4cuni
076060051128     c                   Select
076070100127     c                   When      w0070  <> v1cksc
076080051128     c                   Eval      *In28 = *On
076090051130     c                   Eval      h4riga = 08
076100051130     c                   Eval      h4colo = 36
076110051130     c                   Eval      v4cmsg = msg(46)
076120051128     c                   Leavesr
076130100127     c                   When      w0070  = v1cksc and Not *In01
076140051128     c                             and Not *In07
076150051128     c                   Eval      *In28 = *On
076160051130     c                   Eval      h4riga = 08
076170051130     c                   Eval      h4colo = 36
076180051130     c                   Eval      v4cmsg = msg(47)
076190051128     c                   Leavesr
076200051128     c                   EndSl
076210051128     c                   EndIf
076220051128
076230051128     c                   EndSr
076240051125
076250051125      *------------------------------------------------------------------------*
076260051125      * CONTROLLO TIPO FATTURA
076270051125      *------------------------------------------------------------------------*
076280051125     c     Sr_Ctrtft     BegSr
076290051125
076300051125      * Ricerca
076310051130     c                   Clear                   v4dtft
076320051130     c                   If        v4ctft = '?'
076330051125     c                   Clear                   tibs02ds
076340051125     c                   Eval      t02mod = 'R'
076350051125     c                   Eval      t02sif = knsif
076360051125     c                   Eval      t02cod = 'TFT'
076370051125     c                   Call      'TIBS02R'
076380051125     c                   Parm                    kpjba
076390051125     c                   Parm                    tibs02ds
076400051125     c                   If        t02err = *Blanks
076410051130     c                   Eval      v4ctft = t02ke1
076420051125     c                   Eval      dtft = t02uni
076430051130     c                   Eval      v4dtft = §tftdes
076440051130     c                   Eval      h4riga = 09
076450051130     c                   Eval      h4colo = 36
076460051125     c                   Eval      *In90 = *On
076470051125     c                   Leavesr
076480051125     c                   EndIf
076490051128     c                   EndIf
076500051125
076510051125      * --> Controllo (obbligatorio in tabella non esiste il ' ')
076520051125     c                   Clear                   Tibs02ds
076530051125     c                   Eval      t02mod = 'C'
076540051125     c                   Eval      t02sif = knsif
076550051125     c                   Eval      t02cod = 'TFT'
076560051130     c                   Eval      t02ke1 = v4ctft
076570051125     c                   Call      'TIBS02R'
076580051125     c                   Parm                    kpjba
076590051125     c                   Parm                    Tibs02ds
076600051125     c                   If        t02err <> *Blanks
076610051125     c                   Eval      *In28 = *On
076620051130     c                   Eval      h4riga = 09
076630051130     c                   Eval      h4colo = 36
076640051130     c                   Eval      v4cmsg = msg(37)
076650051125     c                   Leavesr
076660051125     c                   EndIf
076670051125     c                   Eval      dtft = t02uni
076680051130     c                   Eval      v4dtft = §tftdes
076690051125
076700051125      * Controllo uso sede/p.o.
076710051125      * solo se variato
076720051130     c                   If        v4ctft <> savtft
076730051125     c                   Select
076740051125     c                   When      Not *In09 and §tftuti = 'S'
076750051125     c                   Eval      *In28 = *On
076760051130     c                   Eval      h4riga = 09
076770051130     c                   Eval      h4colo = 36
076780051130     c                   Eval      v4cmsg = msg(38)
076790051130     c                   Eval      v4cmsg = %trim(v4cmsg) + ' ' +
076800051125     c                             'filiale'
076810051125     c                   Leavesr
076820051125     c                   When      *In09 and §tftuti = 'F'
076830051125     c                   Eval      *In28 = *On
076840051130     c                   Eval      h4riga = 09
076850051130     c                   Eval      h4colo = 36
076860051130     c                   Eval      v4cmsg = msg(38)
076870051130     c                   Eval      v4cmsg = %trim(v4cmsg) + ' ' +
076880051125     c                             'sede'
076890051125     c                   Leavesr
076900051125     c                   EndSl
076910051125     c                   EndIf
076920090417
076930090417     c                   clear                   conta
076940090417
076950090417      * se non è anagrafica provvisoria
076960090417      * se tipo fattura è distinta verifico se il codice è un sottoconto
076970090417      * fattura e se i codici a lui collegati hanno la fatturazione
076980090417      * settimanale, in questo caso devo dare errore
076990090417     c                   if        not *in06 and
077000100127     c                             %editc(v1cksc:'X') = v4cscf
077010100127     c                             and v4ctft = '1'
077020090417     c/exec sql
077030090417     c+ select count(*) into :conta from cnclp00f where
077040090417     c+ clpkcc = 151 and clpscf = :v1cksc and clpfft = 1
077050090417     c/end-exec
077060090417      * se ho trovato almeno un rcd vuol dire che uno dei figli
077070090417      * legati al sottoconto fatturazione che sto modificando ha
077080090417      * la frequenza settimanale
077090090417     c                   if        conta > *zeros
077100090417     c                   eval      *in28 = *on
077110090417     c                   eval      h4riga = 09
077120090417     c                   eval      h4colo = 36
077130090417     c                   eval      v4cmsg = msg(82)
077140090417     c                   leavesr
077150090417     c                   endif
077160090417     c                   endif
077170051125
077180051125     c                   EndSr
077190051125
077200051125      *------------------------------------------------------------------------*
077210051125      * CONTROLLO FREQUENZA FATTURA
077220051125      *------------------------------------------------------------------------*
077230051125     c     Sr_Ctrfft     BegSr
077240051125
077250051125      * Ricerca
077260051130     c                   Clear                   v4dfft
077270051130     c                   If        v4cfft = '?'
077280051125     c                   Eval      kcod = 'FF'
077290051125     c                   call      'TRUL19R'
077300051125     c                   parm                    kcod
077310051125     c                   parm                    ordina
077320051125     c                   parm                    kkey
077330051125     c                   parm                    comando
077340051130     c                   Eval      v4cfft = kkey
077350051130     c                   Eval      h4riga = 10
077360051130     c                   Eval      h4colo = 36
077370051125     c                   Eval      *In90 = *On
077380051125     c                   EndIf
077390051125
077400051125      * --> Controllo (obbligatorio)
077410051130     c                   If        v4cfft = *Blanks
077420051125     c                   Eval      *In28 = *On
077430051130     c                   Eval      h4riga = 10
077440051130     c                   Eval      h4colo = 36
077450051130     c                   Eval      v4cmsg = msg(39)
077460051125     c                   Leavesr
077470051125     c                   EndIf
077480051125
077490051130     c                   Clear                   v4dfft
077500051125     c                   Eval      kcod = 'FF'
077510051130     c                   Eval      kkey = v4cfft
077520051125     c     kTabel        Chain     Tabel00f
077530051125     c                   If        Not %Found(Tabel00f) or
077540051125     c                             tblflg <> *Blanks
077550051125     c                   Eval      *In28 = *On
077560051130     c                   Eval      h4riga = 10
077570051130     c                   Eval      h4colo = 36
077580051130     c                   Eval      v4cmsg = msg(40)
077590051125     c                   Leavesr
077600051125     c                   EndIf
077610051125     c                   Eval      dsff = tbluni
077620051130     c                   Eval      v4dfft = §ffdes
077630051125
077640051125      * Se tipo fattura '1' non è ammessa la frequenza fattura <> dal mensile
077650051220     c                   If        v4ctft = '1' and v4cfft <> '4'
077660051125     c                   Eval      *In28 = *On
077670051130     c                   Eval      h4riga = 10
077680051130     c                   Eval      h4colo = 36
077690051130     c                   Eval      v4cmsg = msg(41)
077700051125     c                   Leavesr
077710051125     c                   EndIf
077720051125
077730120613      * La frequenza fattura settimanale (1) è ammessa solo se
077740120613      * utente abilitato
077750120613     c                   If        v4cfft = '1' and
077760120613     c                             §UTEclpfft <> 'S' and
077770051130     c                             v4cfft <> savfft
077780051125     c                   Eval      *In28 = *On
077790051130     c                   Eval      h4riga = 10
077800051130     c                   Eval      h4colo = 36
077810051130     c                   Eval      v4cmsg = msg(42)
077820051125     c                   Leavesr
077830051125     c                   EndIf
077840090416
077850090416      * se frequenza fattura settimanale controllo il tipo fattura del
077860090416      * sottoconto fatturazione (se diverso)
077870100127     c                   if        v4cfft = '1' and
077880100127     c                             v4cscf <> %editc(v1cksc:'X')
077890100127     c                   move      v4cscf        w0070
077900090416      * il sottoconto fatturazione non deve avere il tipo fattura distinta
077910090417     c/exec sql
077920090417     c+ select clptft into:w_clptft from cnclp00f where
077930100127     c+ clpkcc = 151 and clpksc = :w0070
077940090417     c/end-exec
077950090417     c                   if        sqlcod = 0 and w_clptft = 1
077960090416     c                   eval      *in28 = *on
077970090416     c                   eval      h4riga = 10
077980090416     c                   eval      h4colo = 36
077990090416     c                   eval      v4cmsg = msg(81)
078000090416     c                   leavesr
078010090417     c                   endif
078020090416     c                   endif
078030051125
078040051125     c                   EndSr
078050051125
078060051125      *------------------------------------------------------------------------*
078070051125      * CONTROLLO TIPO DATA FATTURA
078080051125      *------------------------------------------------------------------------*
078090051125     c     Sr_Ctrtdf     BegSr
078100051125
078110051125      * Ricerca
078120051130     c                   Clear                   v4dtdf
078130051130     c                   If        v4ctdf = '?'
078140051125     c                   Eval      kcod = '13'
078150051125     c                   call      'TRUL19R'
078160051125     c                   parm                    kcod
078170051125     c                   parm                    ordina
078180051125     c                   parm                    kkey
078190051125     c                   parm                    comando
078200051130     c                   Eval      v4ctdf = kkey
078210051130     c                   Eval      h4riga = 11
078220051130     c                   Eval      h4colo = 36
078230051125     c                   Eval      *In90 = *On
078240051125     c                   EndIf
078250051125
078260051125      * --> Controllo (obbligatorio)
078270051130     c                   If        v4ctdf = *Blanks
078280051125     c                   Eval      *In28 = *On
078290051130     c                   Eval      h4riga = 11
078300051130     c                   Eval      h4colo = 36
078310051130     c                   Eval      v4cmsg = msg(43)
078320051125     c                   Leavesr
078330051125     c                   EndIf
078340051125
078350051130     c                   Clear                   v4dtdf
078360051125     c                   Eval      kcod = '13'
078370051130     c                   Eval      kkey = v4ctdf
078380051125     c     kTabel        Chain     Tabel00f
078390051125     c                   If        Not %Found(Tabel00f) or
078400051125     c                             tblflg <> *Blanks
078410051125     c                   Eval      *In28 = *On
078420051130     c                   Eval      h4riga = 11
078430051130     c                   Eval      h4colo = 36
078440051130     c                   Eval      v4cmsg = msg(44)
078450051125     c                   Leavesr
078460051125     c                   EndIf
078470051125     c                   Eval      ds13 = tbluni
078480051130     c                   Eval      v4dtdf = §13des
078490051125
078500051125     c                   EndSr
078510051128
078520051128      *------------------------------------------------------------------------*
078530051128      * CONTROLLO SPESE INVIO FATTURA
078540051128      *------------------------------------------------------------------------*
078550051128     c     Sr_Ctrsin     BegSr
078560051128
078570051130     c                   If        v4csin > *Zeros
078580051128     c                   Reset                   xdecds
078590051130     c                   Z-add     v4csin        icdnum
078600051128     c                   Z-add     2             icdnrd
078610051128     c                   Call      'XDEC'
078620051128     c                   Parm                    xdecds
078630051128     c                   If        ocdesi <> *Off
078640051128     c                   Eval      *In28 = *On
078650051130     c                   Eval      h4riga = 12
078660051130     c                   Eval      h4colo = 36
078670051130     c                   Eval      v4cmsg = msg(48)
078680051128     c                   Leavesr
078690051128     c                   EndIf
078700051128     c                   EndIf
078710051128
078720051128     c                   EndSr
078730051128
078740051128      *------------------------------------------------------------------------*
078750051128      * CONTROLLO LUOGO 001 - INVIO FATTURA
078760051128      *------------------------------------------------------------------------*
078770051128     c     Sr_Ctrl001    BegSr
078780051128
078790051130if  1c                   If        v4cl001 <> *Blanks
078800051128
078810060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
078820060203     c                   If        *In05 and Not *In10
078830051128     c                   Eval      *In28 = *On
078840100727     c                   Eval      h4riga = 21
078850060203     c                   Eval      h4colo = 35
078860051130     c                   Eval      v4cmsg = msg(50)
078870051128     c                   Leavesr
078880051128     c                   EndIf
078890071128
078900071128      * se non è interrogazione controllo se luogo utilizzabile
078910071128     c                   if        not *in09 and not *in05 and $ufi001 <> 'S'
078920071128     c                   eval      *In28 = *on
078930100727     c                   eval      h4riga = 21
078940071128     c                   eval      h4colo = 35
078950071128     c                   eval      v4cmsg = msg(76)
078960071128     c                   leavesr
078970071128     c                   endif
078980051128
078990060203      * Imposto se interrogazione o gestione
079000051213     c                   Clear                   tnta81ds
079010060203     c   05              Eval      i81opz = '5'
079020060203     c  n05
079030060203     can 10              Eval      i81opz = '2'
079040051213     c                   Eval      i81cod = '001'
079050051213     c                   Eval      i81cli = v1cksc
079060060216     c                   Clear                   parmf11
079070060216     c                   Eval      i81gcli = '1'
079080060216     c   01              Eval      i81icli = '1'
079090060216     c                   Eval      kpjbu = parmf11
079100051213     c                   Call      'TNTA81R'
079110051213     c                   Parm                    kpjba
079120051213     c                   Parm                    tnta81ds
079130051128
079140051212      * Controllo se ora c'è o non c'è più il luogo
079150051216     c                   Eval      kspecli = v1cksc
079160051216     c                   Eval      kspecod = '001'
079170051128     c     kFnspe        Chain     Fnspe01l
079180051128     c                   If        %Found(Fnspe01l) and speflg = *Blanks
079190051128     c                   Eval      *In10 = *On
079200051212     c                   Else
079210051212     c                   Eval      *In10 = *Off
079220051128     c                   EndIf
079230051128
079240051130     c                   Clear                   v4cl001
079250100727     c                   Eval      h4riga = 21
079260060203     c                   Eval      h4colo = 35
079270051207     c                   Goto      emi04
079280051128
079290051129    1c                   EndIf
079300051128
079310051128     c                   EndSr
079320051122
079330051129      *------------------------------------------------------------------------*
079340051220      * CONTROLLO NOTA 84 - INVIO FATTURA
079350051129      *------------------------------------------------------------------------*
079360051129     c     Sr_Ctrnt84    BegSr
079370051129
079380051130if  1c                   If        v4cnt84 <> *Blanks
079390060203
079400060203      * Se sono in interrogazione non posso interrogare la nota se non c'è
079410060203     c                   If        *In05 and Not *In21
079420060203     c                   Eval      *In28 = *On
079430060203     c                   Eval      h4riga = 21
079440100727     c                   Eval      h4colo = 70
079450060203     c                   Eval      v4cmsg = msg(52)
079460060203     c                   Leavesr
079470060203     c                   EndIf
079480060203
079490060203      * Imposto se interrogazione o gestione
079500060203     c  n05              Eval      wrich = 'M'
079510060203     c   05              Eval      wrich = 'V'
079520060217     c                   Eval      wrag = v3crag
079530051212     c                   Eval      wtnt = '84'
079540051212     c                   ExSr      Sr_f02
079550060215
079560060215     c                   If        ta12err <> *Blanks
079570060215     c                   Eval      *In28 = *On
079580060215     c                   Eval      h4riga = 21
079590100727     c                   Eval      h4colo = 70
079600060215     c                   Eval      v4cmsg = ta12msg
079610060215     c                   Leavesr
079620060215     c                   EndIf
079630051129
079640060215      * Controllo se ora c'è o non c'è più la nota
079650091112     c   70
079660091112     corn06              Eval      kntcapl = 'C'
079670100806     c   06
079680091112     cann70              Eval      kntcapl = 'T'
079690051216     c                   Movel(p)  dutkci        kntcnk1
079700091112     c   70
079710091112     corn06              Move      v1cksc        kntcnk1
079720100806     c   06
079730100806     cann70              Move      v1cnrv        kntcnk1
079740051216     c                   Clear                   kntcnk2
079750051216     c                   Movel     '84'          kntctnt
079760091119     c     kTfntc        Chain(n)  Tfntc01l
079770051129     c                   If        %Found(Tfntc01l)
079780051129     c                   Eval      *In21 = *On
079790051212     c                   Else
079800051212     c                   Eval      *In21 = *Off
079810051129     c                   EndIf
079820051129
079830051130     c                   Clear                   v4cnt84
079840051212     c                   Eval      h4riga = 21
079850100727     c                   Eval      h4colo = 70
079860051212     c                   Goto      emi04
079870051129
079880051129    1c                   EndIf
079890051129
079900051129     c                   EndSr
079910051129
079920051129      *------------------------------------------------------------------------*
079930051130      * CONTROLLO CODICE PAGAMENTO FATTURE
079940051129      *------------------------------------------------------------------------*
079950051129     c     Sr_Ctrcdp     BegSr
079960051129
079970051129      * Ricerca
079980100727     c                   Eval      wpos = %scan('?':v4ccdp)
079990051129     c                   If        wpos > 0
080000100727     c                   Clear                   v4dcdp
080010100727     c                   Clear                   v4ccdp
080020051129     c                   Call      'XCDPAGR'
080030100727     c                   Parm                    v4ccdp
080040100727     c                   Parm                    v4dcdp
080050100728     c                   Eval      h4riga = 13
080060100728     c                   Eval      h4colo = 36
080070051129     c                   Eval      *In90 = *On
080080100727     c                   If        v4ccdp = '999'
080090100727     c                   Clear                   v4ccdp
080100051129     c                   EndIf
080110051129     c                   EndIf
080120051129
080130051129      * Controllo
080140100727     c                   Clear                   v4dcdp
080150051129     c                   Clear                   dsfa
080160051129     c                   Eval      kcod = 'FA'
080170100727     c                   Movel     v4ccdp        w004a
080180051129     c                   Move      '1'           w004a
080190051129     c                   Eval      kkey = w004a
080200051129     c     kTabel        Chain     Tabel00f
080210051129     c                   If        Not %Found(Tabel00f) or
080220051129     c                             tblflg <> *Blanks
080230051129     c                   Eval      *In28 = *On
080240100728     c                   Eval      h4riga = 13
080250100728     c                   Eval      h4colo = 36
080260100728     c                   Eval      v4cmsg = msg(53)
080270051129     c                   Leavesr
080280051129     c                   EndIf
080290051129     c                   Eval      dsfa = tbluni
080300100727     c                   Eval      v4dcdp = §fades
080310051129
080320051129      * Se capoconto cliente non posso utilizzare il codice 3
080330051129     c                   If        v1ckcc = 151 and §fatpg = '3'
080340051129     c                   Eval      *In28 = *On
080350100728     c                   Eval      h4riga = 13
080360100728     c                   Eval      h4colo = 36
080370100728     c                   Eval      v4cmsg = msg(53)
080380051129     c                   Leavesr
080390051129     c                   EndIf
080400080515
080410080515      * se utente di filiale non può inerire un pagamento con codice '2' RID
080420090416      * ora controllo il tipo utilizzo se S è solo SEDE
080430080515     c                   if        not *in09 and
080440100727     c                             v4ccdp <> %subst(indcdp:4:3) and
080450090421     c                             §fauti = 'S'
080460080515     c                   eval      *In28 = *On
080470100728     c                   eval      h4riga = 13
080480100728     c                   eval      h4colo = 36
080490100728     c                   eval      v4cmsg = msg(80)
080500080515     c                   leavesr
080510080515     c                   endif
080520150415      /free
080530160517       //?Se il cliente è sottoconto intestazione fattura
080540160517       //?controllo se OK RIBA
080550160518         IF  %editc(V1Cksc:'X') = V4Cscf and §FAtpg = '1';
080560160518       //?No RIBA (tipo pag. 1) se P.IVA estera
080570160518           IF  (V2ivaeu <> *blanks and V2ivaeu <> '$$' and
080580160518                V2ivaeu <> 'IT' and V2ivaeu <> 'SM') or
080590160518               (V2ivaeu = '$$' and V2Ccdf = *blanks);
080600160517             *in28 = *on;
080610160517             H4riga = 13;
080620160517             H4colo = 36;
080630160517             V4Cmsg = msg(89);
080640160517             leavesr;
080650160517           ENDIF;
080660160517       //?No RIBA (tipo pag. 1) su conto Bancoposta (ABI 07601)
080670160518           IF  V4Cabi = 07601;
080680160517             *in28 = *on;
080690160517             H4riga = 13;
080700160517             H4colo = 36;
080710160517             V4Cmsg = msg(90);
080720160517             leavesr;
080730160518           ENDIF;
080740160517         ENDIF;
080750160517
080760150415       //?Se utente di filiale e tipo pagamento utilizzabile solo da sede
080770150415       //?devo proteggere la Banca ABI/CAB del pagamento fatture
080780150415         *in82 = *off;
080790150415         IF  not *in09 and §FAuti = 'S';
080800150415           *in82 = *on;
080810150415         ENDIF;
080820150415      /end-free
080830051129
080840051129     c                   EndSr
080850051129
080860051129      *------------------------------------------------------------------------*
080870051129      * CONTROLLO BANCA APPOGGIO
080880051129      *------------------------------------------------------------------------*
080890051129     c     Sr_Ctrbna     BegSr
080900070216
080910070216     c                   clear                   wbanca
080920070216     c                   clear                   wagenzia
080930051129
080940051129      * Ricerca
080950100727     c                   Eval      wpos = %scan('?':v4cbna)
080960051129if  1c                   If        wpos > 0
080970100727     c                   Eval      pist = %subst(v4cbna:(wpos+1):(36-wpos))
080980100727     c                   Move      v4cabi        pabi
080990051129     c                   Clear                   pcab
081000051129     c                   Eval      ppro = v2cprv
081010051129     c                   Eval      pflg = 'R'
081020051129     c                   Eval      kpjbu = parmbanca
081030051129     c                   Call      'CNC592R'
081040051129     c                   Parm                    kpjba
081050051129     c                   Eval      parmbanca = kpjbu
081060051129      * Se è stato selezionato un codice
081070051129if  2c                   If        pcab <> *Zeros
081080100727     c                   Movel     pabi          v4cabi
081090100727     c                   Movel     pcab          v4ccab
081100100727     c                   Eval      kabi = v4cabi
081110100727     c                   Eval      kcab = v4ccab
081120100729     c                   exsr      DesBanca
081130100729     c                   eval      v4cbna = wDesBanca
081140051129    2c                   EndIf
081150051129     c                   Eval      *In90 = *On
081160100728     c                   Eval      h4riga = 14
081170100728     c                   Eval      h4colo = 36
081180051129     c                   Leavesr
081190051129    1c                   EndIf
081200051129
081210051129     c                   EndSr
081220051129
081230051129      *------------------------------------------------------------------------*
081240051130      * CONTROLLO ABI/CAB RELATIVO AL PAGAMENTO FATTURE
081250051129      *------------------------------------------------------------------------*
081260051130     c     Sr_Ctrabicabf BegSr
081270051129
081280051129      * Se non è da gestione visite/offerte controllo se abi e cab
081290051129      * obbligatori per pagamento con RB
081300051129     c                   If        Not *In06 and §fatpg = '1' and
081310100727     c                             v4cabi = *Zeros and v4ccab = *Zeros
081320051129     c                   Eval      *In28 = *On
081330100728     c                   Eval      h4riga = 15
081340100728     c                   Eval      h4colo = 36
081350100728     c                   Eval      v4cmsg = msg(54)
081360051129     c                   Leavesr
081370051129     c                   EndIf
081380090331
081390090331      * se pagamento con RB codice '1' non accetto ABI >= 90000
081400090331     c                   If        §fatpg = '1' and
081410100727     c                             v4cabi >= 90000
081420090331     c                   Eval      *In28 = *On
081430100728     c                   Eval      h4riga = 15
081440100728     c                   Eval      h4colo = 36
081450100728     c                   Eval      v4cmsg = msg(56)
081460090331     c                   Leavesr
081470090331     c                   EndIf
081480051130
081490051130      * Controllo
081500100727     c                   Eval      kabi = v4cabi
081510100727     c                   Eval      kcab = v4ccab
081520051130     c                   ExSr      Sr_Ctrabicab
081530051130     c                   If        *In28 = *On
081540100728     c                   Eval      h4riga = 15
081550100728     c                   Eval      h4colo = 36
081560051130     c                   Leavesr
081570051130     c                   EndIf
081580051130      * Creo descrizione della banca di appoggio
081590100727     c                   If        v4cabi <> 99999 or v4ccab <> 99999
081600100727     c                   Movel     wbanca        v4cbna
081610100727     c                   Move      wagenzia      v4cbna
081620051130     c                   EndIf
081630051130
081640051130     c                   EndSr
081650150424
081660150424      /free
081670150424       //--------------------------------------------------------------
081680150424       //?Controllo i pagamenti C/A - Danni - N.A.
081690150424       //--------------------------------------------------------------
081700150424       BEGSR CtrPagamenti;
081710150424
081720150424       //?Pagamenti CONTRASSEGNI
081730150424         clear ds4U;
081740150424         kcod = '4U';
081750150424         kkey = V5Cfpc;
081760150424         chain (codut:kcod:kkey) TABEL00F;
081770150424         IF  %found(TABEL00F);
081780150424           ds4U = TBLuni;
081790150424         ENDIF;
081800151112         IF  V5Cfpc <> '1';
081810150427         IF  not wForzaPag and §4Utip = 'I' and
081820150427            (§4Ucod <> 'B' or V5Ciban = *blanks);
081830150424           *in28 = *on;
081840150424           H5riga = 09;
081850150424           H5colo = 36;
081860150424           V5Cmsg = msg(87);
081870150424           wForzaPag = *on;
081880150424           leavesr;
081890150424         ENDIF;
081900151112         ENDIF;
081910150424
081920150424       //?Pagamenti DANNI
081930150424         clear ds4U;
081940150424         kcod = '4U';
081950150424         kkey = V5tpdn;
081960150424         chain (codut:kcod:kkey) TABEL00F;
081970150424         IF  %found(TABEL00F);
081980150424           ds4U = TBLuni;
081990150424         ENDIF;
082000151112         IF  V5tpdn <> '1';
082010150427         IF  not wForzaPag and §4Utip = 'I' and
082020150427            (§4Ucod <> 'B' or V5ibandn = *blanks);
082030150424           *in28 = *on;
082040150424           H5riga = 12;
082050150424           H5colo = 36;
082060150424           V5Cmsg = msg(87);
082070150424           wForzaPag = *on;
082080150424           leavesr;
082090150424         ENDIF;
082100151112         ENDIF;
082110150424
082120150424       //?Pagamenti NOTE ACCREDITO
082130150424         clear ds4U;
082140150424         kcod = '4U';
082150150424         kkey = V5tpna;
082160150424         chain (codut:kcod:kkey) TABEL00F;
082170150424         IF  %found(TABEL00F);
082180150424           ds4U = TBLuni;
082190150424         ENDIF;
082200151112         IF  V5tpna <> '1';
082210150427         IF  not wForzaPag and §4Utip = 'I' and
082220150427            (§4Ucod <> 'B' or V5ibanna = *blanks);
082230150424           *in28 = *on;
082240150424           H5riga = 15;
082250150427           H5colo = 37;
082260150424           V5Cmsg = msg(87);
082270150424           wForzaPag = *on;
082280150424           leavesr;
082290150424         ENDIF;
082300151112         ENDIF;
082310150424
082320150424       ENDSR;
082330150728
082340150728       //--------------------------------------------------------------
082350150728       //?Controllo i dati per la mail invio progetto di liquidazione.
082360150728       //--------------------------------------------------------------
082370150728       BEGSR CtrMailDanni;
082380150728
082390150728       //?Se esiste la nota 87 OK
082400150728         IF  *in18;
082410150728           leavesr;
082420150728         ENDIF;
082430150728
082440150728       //?Se esiste il luogo 008 e c'è la mail OK
082450150728         IF  *in17;
082460150728           kSPEcli = V1Cksc;
082470150728           kSPEcod = '008';
082480150728           chain (kSPEcli:kSPEcod:kSP2tpe) FNSP201L;
082490150728           IF  %Found(FNSP201L) and SP2flg = *blanks;
082500150728             leavesr;
082510150728           ENDIF;
082520150728         ENDIF;
082530150728
082540150728       //?Se arrivo qua vuol dire che non c'è la nota 87 oppure
082550150728       //?non c'è il luogo 008 o il luogo 008 non ha la mail
082560150728         IF  not wForzaMail;
082570150728           *in28 = *on;
082580150728           H7riga = 13;
082590150728           H7colo = 35;
082600150728           V7Cmsg = msg(88);
082610150728           wForzaMail = *on;
082620150728         ENDIF;
082630150728
082640150728       ENDSR;
082650150424      /end-free
082660051130
082670051130      *------------------------------------------------------------------------*
082680051130      * CONTROLLO CODICE PAGAMENTO CONTRASSEGNI
082690051130      *------------------------------------------------------------------------*
082700051130     c     Sr_Ctrfpc     BegSr
082710051219
082720051219     c                   Eval      *In22 = *Off
082730051219     c                   Eval      *In23 = *Off
082740051130
082750051130      * Ricerca
082760051130     c                   Clear                   v5dfpc
082770051130     c                   If        v5cfpc = '?'
082780051130     c                   Eval      kcod = '4U'
082790051130     c                   call      'TRUL19R'
082800051130     c                   parm                    kcod
082810051130     c                   parm                    ordina
082820051130     c                   parm                    kkey
082830051130     c                   parm                    comando
082840051130     c                   Eval      v5cfpc = kkey
082850100729     c                   Eval      h5riga = 09
082860100802     c                   Eval      h5colo = 36
082870051130     c                   Eval      *In90 = *On
082880051130     c                   EndIf
082890051130
082900051130      * Controllo
082910051130     c                   Clear                   v5dfpc
082920051130     c                   Clear                   ds4u
082930051130     c                   Eval      kcod = '4U'
082940051130     c                   Eval      kkey = v5cfpc
082950051130     c     kTabel        Chain     Tabel00f
082960051130     c                   If        Not %Found(Tabel00f) or
082970051130     c                             tblflg <> *Blanks
082980051130     c                   Eval      *In28 = *On
082990100729     c                   Eval      h5riga = 09
083000100802     c                   Eval      h5colo = 36
083010051130     c                   Eval      v5cmsg = msg(57)
083020051130     c                   Leavesr
083030051130     c                   EndIf
083040051130     c                   Eval      ds4u = tbluni
083050051130     c                   Eval      v5dfpc = §4udes
083060051130
083070051130      * Controllo se utilizzabile per i clienti vari
083080051130     c                   If        (wksc04 = 8888 or wksc04 = 9999) and
083090051130     c                              §4uvar = 'N'
083100051130     c                   Eval      *In28 = *On
083110100729     c                   Eval      h5riga = 09
083120100802     c                   Eval      h5colo = 36
083130051130     c                   Eval      v5cmsg = msg(58)
083140051130     c                   Leavesr
083150051130     c                   EndIf
083160151030
083170151030      /free
083180151030       //?Errore se utilizzabile solo da sede e NON sono sede
083190151112         IF  §4Usede <> *blanks and not *in09 and v5cfpc <> CLPfpc;
083200151030           *in28 = *on;
083210151030           H5riga = 09;
083220151030           H5colo = 36;
083230151030           V5Cmsg = msg(57);
083240151030           V5Cmsg = %trim(V5Cmsg) + '. Utilizzabili SOLO da SEDE';
083250151030           leavesr;
083260151030         ENDIF;
083270151030      /end-free
083280051219
083290051219      * Le coordinate bancare le visualizzo o le proteggo in base al
083300051219      * tipo pagamento
083310051219     c                   Select
083320051219      * Coordinate italiane, visualizzo
083330051219      * Immissione coordinate non richiesta, non visualizzo
083340051219     c                   When      §4uabi = 'N'
083350051219     c                   Eval      *In22 = *On
083360080116      * pulisco le coordinate bancarie
083370080116     c                   clear                   v5ciban
083380051219      * Se pagamento estero proteggo le coordinate bancarie perchè
083390051219      * gestite dalla sede.
083400080115     c                   when      §4utip = 'E'
083410051219     c                   Eval      *In23 = *On
083420051219     c                   EndSl
083430100729      * se il tipo pagamento del codice impostato a video è diverso da quello
083440100729      * memorizzato sul file ed è estero devo pulire il campo relativo all'IBAN
083450100729      * e avvisare con window per inviare mail alla sede
083460100729     c                   if        §4utip <> sav4utip and §4utip = 'E'
083470100729     c                   if        v5ciban <> *blanks
083480100729     c                   clear                   v5ciban
083490100729     c                   endif
083500100729      * window per inviare mail alla sede
083510100729      * solo se tipo pagamento prevede le coordinate bancarie
083520100729     c                   if        not $win and §4uabi = 'S'
083530101014     c                   eval      wuffsede = '"CONTRASSEGNI" di sede'
083540100729     c                   exsr      sr_winmail
083550100729     c                   eval      $win = *on
083560100729     c                   eval      *in90 = *on
083570100729     c                   leavesr
083580100729     c                   endif
083590100729     c                   endif
083600051130
083610051130     c                   EndSr
083620100729
083630100729      /free
083640100729       //--------------------------------------------------------------
083650100729       //?Controllo pagamento Danni.
083660100729       //--------------------------------------------------------------
083670100729       BEGSR CtrPagDN;
083680100729
083690100729         *in71 = *off;
083700100729         *in72 = *off;
083710100729
083720100729       //?Ricerco il codice pagamento
083730100729         clear v5dpdn;
083740100729         IF  %scan('?':v5tpdn) > 0;
083750100729           kcod = '4U';
083760100729           Trul19R (kcod:ordina:kkey:comando);
083770100729           v5tpdn = kkey;
083780100730           h5riga = 12;
083790100802           h5colo = 36;
083800100729           *in90 = *on;
083810100729         ENDIF;
083820100729
083830100729       //?Controllo
083840100729         clear v5dpdn;
083850100729         clear ds4u;
083860100729         kcod = '4U';
083870100729         kkey = v5tpdn;
083880100729         chain (codut:kcod:kkey) TABEL00F;
083890100729         IF  not %Found(TABEL00F) or TBLflg <> *blanks;
083900100729           *in28 = *on;
083910100730           h5riga = 12;
083920100802           h5colo = 36;
083930100729           v5cmsg = msg(57);
083940100729           leavesr;
083950100729         ENDIF;
083960100729         ds4u = TBLuni;
083970100730         v5dpdn = §4Udes;
083980100729
083990100729       //?Controllo se posso utilizzare per clienti vari
084000110131         IF  (wksc04 = 8888 or wksc04 = 9999) and §4Uvar = 'N';
084010100729           *in28 = *on;
084020100730           h5riga = 12;
084030100802           h5colo = 36;
084040100729           v5cmsg = msg(58);
084050100729           leavesr;
084060100729         ENDIF;
084070151030
084080151030       //?Errore se utilizzabile solo da sede e NON sono sede
084090151112         IF  §4Usede <> *blanks and not *in09 and
084100151112             v5tpdn <> %subst(CLStic:1:1);
084110151030           *in28 = *on;
084120151030           H5riga = 12;
084130151030           H5colo = 36;
084140151030           V5Cmsg = msg(57);
084150151030           V5Cmsg = %trim(V5Cmsg) + '. Utilizzabili SOLO da SEDE';
084160151030           leavesr;
084170151030         ENDIF;
084180151030
084190100729       //?Visualizzo il campo iban se coordinate bancarie obbligatorie
084200100730         *in71 = (§4Uabi = 'N');
084210100729
084220100729       //?Controllo se posso utilizzare in base alla tabella PAG
084230100729         clear TIBS02DS;
084240100729         clear dPAG;
084250100729         T02mod = 'C';
084260100729         T02cod = 'PAG';
084270100729         T02ke1 = 'DN';
084280100729         T02ke2 = v5tpdn;
084290100729         T02sif = KNSIF;
084300100729         TNTBE_RicercaControllo (kpjba : tibs02ds);
084310100729         IF  T02err <> *blanks;
084320100729           *in28 = *on;
084330100730           h5riga = 12;
084340100802           h5colo = 36;
084350100729           v5cmsg = msg(85);
084360100729           leavesr;
084370100729         ENDIF;
084380100729         IF  T02err = *blanks;
084390100729           dPAG = T02uni;
084400100729         ENDIF;
084410100729       //?controllo se la filiale può utilizzare il pagamento
084420151112         IF  d§PAGuti = 'NO' and not *in09 and
084430151112             v5tpdn <> %subst(CLStic:1:1);
084440100729           *in28 = *on;
084450100730           h5riga = 12;
084460100802           h5colo = 36;
084470100729           v5cmsg = msg(80);
084480100729           leavesr;
084490100729         ENDIF;
084500100730       //?proteggo il campo IBAN se pagamento estero
084510100730         *in72 = (§4Utip = 'E');
084520100909
084530100909       //?se pagamento con bonifico italia posso utilizzarlo solo
084540100909       //?per clienti 'T - D - A'
084550150506         //IF  not *in72 and d§PAGctr = 'SI' and
084560150506         //    v3cclv <> 'T' and  v3cclv <> 'D' and
084570150506         //    v3cclv <> 'A';
084580150506         //  *in28 = *on;
084590150506         //  h5riga = 12;
084600150506         //  h5colo = 36;
084610150506         //  v5cmsg = msg(85);
084620150506         //  v5cmsg = %trim(v5cmsg) + ' per clienti NON di tipo T-D-A';
084630150506         //  leavesr;
084640150506         //  ENDIF;
084650100730
084660100802       //?se è stato modificato il tipo pagamento
084670100909       //?e quello inserito non richiede le coordinate bancarie pulisco
084680100802       //?il campo dell'IBAN
084690100802         IF  v5tpdn <> savtipdn and d§PAGobb <> 'SI';
084700100802           clear v5ibandn;
084710100805           clear v5bicdn;
084720100802       //?se il tipo pagamento inserito è un tipo pagamento che lo richiede
084730100802       //?visualizzo window per invio mail
084740100802           IF  d§PAGobb = 'MA' and not $winDN;
084750101014             wuffsede = '"GESTIONE PAGAMENTI" di sede';
084760100802             exsr sr_winmail;
084770100802             $winDN = *on;
084780100802             *in90 = *on;
084790100802             leavesr;
084800100802           ENDIF;
084810100730         ENDIF;
084820100729
084830100729       ENDSR;
084840100729
084850100729       //--------------------------------------------------------------
084860100729       //?Controllo pagamento Note Accredito.
084870100729       //--------------------------------------------------------------
084880100729       BEGSR CtrPagNA;
084890100730
084900100730         *in73 = *off;
084910100730         *in74 = *off;
084920100730
084930100730       //?Ricerco il codice pagamento
084940100730         clear v5dpna;
084950100730         IF  %scan('?':v5tpna) > 0;
084960100730           kcod = '4U';
084970100730           Trul19R (kcod:ordina:kkey:comando);
084980100916           v5tpna = kkey;
084990100730           h5riga = 15;
085000150427           h5colo = 37;
085010100730           *in90 = *on;
085020100730         ENDIF;
085030100730
085040100730       //?Controllo
085050100730         clear v5dpna;
085060100730         clear ds4u;
085070100730         kcod = '4U';
085080100730         kkey = v5tpna;
085090100730         chain (codut:kcod:kkey) TABEL00F;
085100100730         IF  not %Found(TABEL00F) or TBLflg <> *blanks;
085110100730           *in28 = *on;
085120100730           h5riga = 15;
085130150427           h5colo = 37;
085140100730           v5cmsg = msg(57);
085150100730           leavesr;
085160100730         ENDIF;
085170100730         ds4u = TBLuni;
085180100730         v5dpna = §4Udes;
085190100730
085200100730       //?Controllo se posso utilizzare per clienti vari
085210110131         IF  (wksc04 = 8888 or wksc04 = 9999) and §4Uvar = 'N';
085220100730           *in28 = *on;
085230100730           h5riga = 15;
085240150427           h5colo = 37;
085250100730           v5cmsg = msg(58);
085260100730           leavesr;
085270100730         ENDIF;
085280151030
085290151030       //?Errore se utilizzabile solo da sede e NON sono sede
085300151112         IF  §4Usede <> *blanks and not *in09 and
085310151112             v5tpna <> %subst(CLStic:2:1);
085320151030           *in28 = *on;
085330151030           H5riga = 15;
085340151030           H5colo = 37;
085350151030           V5Cmsg = msg(57);
085360151030           V5Cmsg = %trim(V5Cmsg) + '. Utilizzabili SOLO da SEDE';
085370151030           leavesr;
085380151030         ENDIF;
085390151030
085400100730       //?Visualizzo il campo iban se coordinate bancarie obbligatorie
085410100730         *in73 = (§4Uabi = 'N');
085420100730
085430100730       //?Controllo se posso utilizzare in base alla tabella PAG
085440100730         clear TIBS02DS;
085450100730         clear dPAG;
085460100730         T02mod = 'C';
085470100730         T02cod = 'PAG';
085480100730         T02ke1 = 'NA';
085490100730         T02ke2 = v5tpna;
085500100730         T02sif = KNSIF;
085510100730         TNTBE_RicercaControllo (kpjba : tibs02ds);
085520100730         IF  T02err <> *blanks;
085530100730           *in28 = *on;
085540100730           h5riga = 15;
085550150427           h5colo = 37;
085560100730           v5cmsg = msg(85);
085570100730           leavesr;
085580100730         ENDIF;
085590100730         IF  T02err = *blanks;
085600100730           dPAG = T02uni;
085610100730         ENDIF;
085620100730       //?controllo se la filiale può utilizzare il pagamento
085630151112         IF  d§PAGuti = 'NO' and not *in09 and
085640151112             v5tpna <> %subst(CLStic:2:1);
085650100730           *in28 = *on;
085660100730           h5riga = 15;
085670150427           h5colo = 37;
085680100730           v5cmsg = msg(80);
085690100730           leavesr;
085700100730         ENDIF;
085710100730       //?proteggo il campo IBAN se pagamento estero
085720100730         *in74 = (§4Utip = 'E');
085730100730
085740100802       //?se è stato modificato il tipo pagamento
085750100909       //?e quello inserito non richiede le coordinate bancarie pulisco
085760100802       //?il campo dell'IBAN
085770100802         IF  v5tpna <> savtipna and d§PAGobb <> 'SI';
085780100802           clear v5ibanna;
085790100805           clear v5bicna;
085800100802       //?se il tipo pagamento inserito è un tipo pagamento che lo richiede
085810100802       //?visualizzo window per invio mail
085820100802           IF  d§PAGobb = 'MA' and not $winNA;
085830101220             wuffsede = '"GESTIONE PAGAMENTI" di sede';
085840100730             exsr sr_winmail;
085850100730             $winNA = *on;
085860100730             *in90 = *on;
085870100730             leavesr;
085880100730           ENDIF;
085890100730         ENDIF;
085900100729
085910100729       ENDSR;
085920100729      /end-free
085930080115
085940080115      *------------------------------------------------------------------------*
085950080115      * CONTROLLO CODICE IBAN
085960080115      *------------------------------------------------------------------------*
085970080115     c     sr_ctriban    begsr
085980080115
085990080115     c                   clear                   wbanca
086000080115     c                   clear                   wagenzia
086010080422
086020080422      * se tipo pagamento contrassegno italia posso accettare solo
086030090612      * un IBAN italia (IT)
086040080422     c                   if        §4utip = 'I' and
086050100729     c                             %subst(wiban:1:2) <> 'IT'
086060150609     c                             and %subst(wiban:1:2) <> 'SM'
086070150424     c                             and %subst(wiban:1:2) <> *blanks
086080080422     c                   eval      *in28 = *on
086090080422     c                   eval      v5cmsg = msg(79)
086100080422     c                   leavesr
086110080422     c                   endif
086120080116
086130080116      * richiamo programma di controllo codice IBAN (Proj)
086140080116     c                   clear                   parmiban
086150100730     c                   eval      parmstato = %subst(wiban:1:2)
086160100730     c                   eval      parmcheck = %subst(wiban:3:2)
086170100730     c                   eval      parmcoori = %subst(wiban:5:28)
086180080116     c                   call      'XCHKIBAN'
086190080116     c                   parm                    parmstato
086200080116     c                   parm                    parmcheck
086210080116     c                   parm                    parmcoori
086220080116     c                   parm                    erriban
086230080116      * se ritorna errore messaggio
086240080116     c                   if        erriban = *on
086250080116     c                   eval      *in28 = *on
086260080116     c                   eval      v5cmsg = msg(59)
086270080116     c                   leavesr
086280080116     c                   endif
086290080116      * decodifico la banca
086300100729     c                   clear                   wdesbanca
086310100729     c                   eval      kabi = %dec(%subst(wiban:6:5):5:0)
086320100729     c                   eval      kcab = %dec(%subst(wiban:11:5):5:0)
086330080116     c     kcnabi        chain     cnabi00f
086340080116     c                   if        %found(cnabi00f) and abiann <> '*'
086350080116      * compongo la descrizione della banca d'appoggio
086360080116     c                   exsr      sr_crebna
086370100730     c                   eval      wdesbanca = wbanca + wagenzia
086380080116     c                   endif
086390080115
086400080115     c                   endsr
086410051130
086420051130      *-------------------------------------------------------------------------
086430051130      *  Creo la descrizione della banca di appoggio in base ai codici ABI/CAB
086440051130      *-------------------------------------------------------------------------
086450051130     c     Sr_Crebna     BegSr
086460051130
086470051130if  1c                   If        abiabi <> 99999 or abicab <> 99999
086480051130     c                   Clear                   wagenzia
086490060221     c                   Clear                   wbanca
086500051130     c                   Eval      wbanca = abiist
086510051130      * Compongo il nome della banca con località o comune
086520051130if  2c                   If        abiage = *Blanks
086530051130     c                   Select
086540051130     c                   When      abiloc <> *Blanks
086550051130     c                   Eval      wagenzia = abiloc
086560051130     c                   When      abicom <> *Blanks
086570051130     c                   Eval      wagenzia = abicom
086580051130     c                   EndSl
086590051130      * Compongo il nome della banca con agenzia + località o comune
086600051130   x2c                   Else
086610051130      * Cerco il primo byte vuoto dell'agenzia
086620051130     c                   Eval      wpos = %checkr(' ':abiage)
086630051130
086640051130     c                   Eval      wagenzia = %subst(abiage:1:wpos)
086650051130     c                   Eval      wpos1 = wpos + 2
086660051130     c                   Eval      wpos2 = 16 - wpos1
086670060215      * Se wpos2 inferiore a zero lo imposto a 0
086680060215     c                   If        wpos2 < *Zeros
086690060215     c                   Clear                   wpos2
086700060215     c                   EndIf
086710051130      * Se posso aggiungere anche solo 1 carattere procedo
086720051130     c                   If        wpos > 1 and wpos < 16
086730051130      * Cerco di riempire quello che resta con la località o il comune
086740051130     c                   Select
086750051130     c                   When      abiloc <> *Blanks
086760060221     c                   Eval      wagenzia = %trim(wagenzia) + ' ' +
086770060221     c                             %subst(abiloc:1:wpos2)
086780051130     c                   When      abicom <> *Blanks
086790060221     c                   Eval      wagenzia = %trim(wagenzia) + ' ' +
086800060221     c                             %subst(abicom:1:wpos2)
086810051130     c                   EndSl
086820051130     c                   EndIf
086830051130
086840051130    2c                   EndIf
086850051130    1c                   EndIf
086860051130
086870051130     c                   EndSr
086880051207
086890051130      *------------------------------------------------------------------------*
086900051130      * CONTROLLO ABI/CAB GENERICO
086910051130      *------------------------------------------------------------------------*
086920051130     c     Sr_Ctrabicab  BegSr
086930051130
086940051130      * Controllo ABI/CAB inseriti
086950051130     c                   If        kabi = *Zeros and kcab = *Zeros
086960051130     c                   Leavesr
086970051130     c                   EndIf
086980051130
086990051130      * Se immesso il CAB immettere anche ABI
087000051130     c                   If        (kabi = *Zeros and kcab <> *Zeros) or
087010051130      * Se immesso il ABI immettere anche CAB
087020051130     c                             (kabi <> *Zeros and kcab = *Zeros)
087030051130     c                   Eval      *In28 = *On
087040101111     c                   Eval      v4cmsg = msg(56)
087050051130     c                   Leavesr
087060051130     c                   EndIf
087070051130      * Controllo se validi
087080051130     c     kCnabi        Chain     Cnabi00f
087090051130     c                   If        Not %Found(Cnabi00f) or abiann = '*'
087100051130     c                   Eval      *In28 = *On
087110101111     c                   Eval      v4cmsg = msg(56)
087120051130     c                   Leavesr
087130051130     c                   EndIf
087140051130
087150051130      * Compongo la descrizione della banca d'appoggio
087160051130     c                   ExSr      Sr_Crebna
087170051130
087180051130     c                   EndSr
087190051129
087200051206
087210051206      *------------------------------------------------------------------------*
087220051206      * CONTROLLO LUOGO 555 - INTESTAZIONE PAGAMENTO C/ASSEGNI
087230051206      *------------------------------------------------------------------------*
087240051206     c     Sr_Ctrl555    BegSr
087250051206
087260051206if  1c                   If        v5cl555 <> *Blanks
087270060203
087280060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
087290060203     c                   If        *In05 and Not *In11
087300051206     c                   Eval      *In28 = *On
087310100730     c                   Eval      h5riga = 19
087320060203     c                   Eval      h5colo = 35
087330051207     c                   Eval      v5cmsg = msg(50)
087340051206     c                   Leavesr
087350051206     c                   EndIf
087360071128
087370071128      * se non è interrogazione controllo se luogo utilizzabile
087380071128     c                   if        not *in09 and not *in05 and $ufi555 <> 'S'
087390071128     c                   eval      *In28 = *on
087400100730     c                   eval      h5riga = 19
087410071128     c                   eval      h5colo = 35
087420071128     c                   eval      v5cmsg = msg(76)
087430071128     c                   leavesr
087440071128     c                   endif
087450051206
087460060203      * Imposto se interrogazione o gestione
087470051213     c                   Clear                   tnta81ds
087480060203     c   05              Eval      i81opz = '5'
087490060203     c  n05
087500060214     can 11              Eval      i81opz = '2'
087510051213     c                   Eval      i81cod = '555'
087520051213     c                   Eval      i81cli = v1cksc
087530060216     c                   Eval      i81gcli = '1'
087540060216     c   01              Eval      i81icli = '1'
087550051213     c                   Call      'TNTA81R'
087560051213     c                   Parm                    kpjba
087570051213     c                   Parm                    tnta81ds
087580051206
087590051212      * Controllo se ora c'è o non c'è più il luogo
087600051216     c                   Eval      kspecli = v1cksc
087610051216     c                   Eval      kspecod = '555'
087620051206     c     kFnspe        Chain     Fnspe01l
087630051206     c                   If        %Found(Fnspe01l) and speflg = *Blanks
087640051212     c                   Eval      *In11 = *On
087650051212     c                   Else
087660051212     c                   Eval      *In11 = *Off
087670051206     c                   EndIf
087680051206
087690051206     c                   Clear                   v5cl555
087700100730     c                   Eval      h5riga = 19
087710060203     c                   Eval      h5colo = 35
087720051207     c                   Goto      emi05
087730051206
087740051206    1c                   EndIf
087750051206
087760051206     c                   EndSr
087770051207
087780051207      *------------------------------------------------------------------------*
087790051212      * CONTROLLO LUOGO 666 - LUOGO INVIO C/ASSEGNO MITTENTE
087800051207      *------------------------------------------------------------------------*
087810051207     c     Sr_Ctrl666    BegSr
087820051212
087830051212if  1c                   If        v5cl666 <> *Blanks
087840060203
087850060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
087860060203     c                   If        *In05 and Not *In12
087870051212     c                   Eval      *In28 = *On
087880100730     c                   Eval      h5riga = 20
087890060203     c                   Eval      h5colo = 35
087900051212     c                   Eval      v5cmsg = msg(50)
087910051212     c                   Leavesr
087920051212     c                   EndIf
087930071128
087940071128      * se non è interrogazione controllo se luogo utilizzabile
087950071128     c                   if        not *in09 and not *in05 and $ufi666 <> 'S'
087960071128     c                   eval      *In28 = *on
087970100730     c                   eval      h5riga = 20
087980071128     c                   eval      h5colo = 35
087990071128     c                   eval      v5cmsg = msg(76)
088000071128     c                   leavesr
088010071128     c                   endif
088020051212
088030060203      * Imposto se interrogazione o gestione
088040051213     c                   Clear                   tnta81ds
088050060203     c   05              Eval      i81opz = '5'
088060060203     c  n05
088070060214     can 12              Eval      i81opz = '2'
088080051213     c                   Eval      i81cod = '666'
088090051213     c                   Eval      i81cli = v1cksc
088100060216     c                   Eval      i81gcli = '1'
088110060216     c   01              Eval      i81icli = '1'
088120051213     c                   Call      'TNTA81R'
088130051213     c                   Parm                    kpjba
088140051213     c                   Parm                    tnta81ds
088150051212
088160051212      * Controllo se ora c'è o non c'è più il luogo
088170051216     c                   Eval      kspecli = v1cksc
088180051216     c                   Eval      kspecod = '666'
088190051212     c     kFnspe        Chain     Fnspe01l
088200051212     c                   If        %Found(Fnspe01l) and speflg = *Blanks
088210051212     c                   Eval      *In12 = *On
088220051212     c                   Else
088230051212     c                   Eval      *In12 = *Off
088240051212     c                   EndIf
088250051212
088260051212     c                   Clear                   v5cl666
088270100730     c                   Eval      h5riga = 20
088280060203     c                   Eval      h5colo = 35
088290051212     c                   Goto      emi05
088300051212
088310051212    1c                   EndIf
088320051207
088330051207     c                   EndSr
088340051206
088350051206      *------------------------------------------------------------------------*
088360051220      * CONTROLLO NOTA 85 - E-MAIL BONIFICI C/ASSEGNI
088370051206      *------------------------------------------------------------------------*
088380051212     c     Sr_Ctrnt85    BegSr
088390051206
088400051212if  1c                   If        v5cnt85 <> *Blanks
088410060203
088420060203      * Se sono in interrogazione non posso interrogare la nota se non c'è
088430060203     c                   If        *In05 and Not *In13
088440060203     c                   Eval      *In28 = *On
088450100730     c                   Eval      h5riga = 19
088460100730     c                   Eval      h5colo = 71
088470060203     c                   Eval      v5cmsg = msg(52)
088480060203     c                   Leavesr
088490060203     c                   EndIf
088500060203
088510060203      * Imposto se interrogazione o gestione
088520060203     c  n05              Eval      wrich = 'M'
088530060203     c   05              Eval      wrich = 'V'
088540060217     c                   Eval      wrag = v3crag
088550051212     c                   Eval      wtnt = '85'
088560051212     c                   ExSr      Sr_f02
088570060215
088580060215     c                   If        ta12err <> *Blanks
088590060215     c                   Eval      *In28 = *On
088600100730     c                   Eval      h5riga = 19
088610100730     c                   Eval      h5colo = 71
088620060215     c                   Eval      v5cmsg = ta12msg
088630060215     c                   Leavesr
088640060215     c                   EndIf
088650051206
088660051212      * Controllo se ora c'è o non c'è più la nota
088670091112     c   70
088680091112     corn06              Eval      kntcapl = 'C'
088690100806     c   06
088700091112     cann70              Eval      kntcapl = 'T'
088710051216     c                   Movel(p)  dutkci        kntcnk1
088720091112     c   70
088730091112     corn06              Move      v1cksc        kntcnk1
088740100806     c   06
088750100806     cann70              Move      v1cnrv        kntcnk1
088760051216     c                   Clear                   kntcnk2
088770051216     c                   Movel     '85'          kntctnt
088780091119     c     kTfntc        Chain(n)  Tfntc01l
088790051206     c                   If        %Found(Tfntc01l)
088800051212     c                   Eval      *In13 = *On
088810051212     c                   Else
088820051212     c                   Eval      *In13 = *Off
088830051206     c                   EndIf
088840051206
088850051212     c                   Clear                   v5cnt85
088860100730     c                   Eval      h5riga = 19
088870100730     c                   Eval      h5colo = 71
088880051212     c                   Goto      emi05
088890051206
088900051206    1c                   EndIf
088910051206
088920051206     c                   EndSr
088930090616
088940090616      *------------------------------------------------------------------------*
088950090616      * CONTROLLO NOTA 89 - E-MAIL RESPONSABILE AMMINISTRATIVO
088960090616      *------------------------------------------------------------------------*
088970090616     c     Sr_Ctrnt89    BegSr
088980090616
088990090616if  1c                   If        v5cnt89 <> *Blanks
089000090616
089010090616      * Se sono in interrogazione non posso interrogare la nota se non c'è
089020090616     c                   If        *In05 and Not *In40
089030090616     c                   Eval      *In28 = *On
089040100730     c                   Eval      h5riga = 20
089050100730     c                   Eval      h5colo = 71
089060090616     c                   Eval      v5cmsg = msg(52)
089070090616     c                   Leavesr
089080090616     c                   EndIf
089090090616
089100090616      * Imposto se interrogazione o gestione
089110090616     c  n05              Eval      wrich = 'M'
089120090616     c   05              Eval      wrich = 'V'
089130090616     c                   Eval      wrag = v3crag
089140090616     c                   Eval      wtnt = '89'
089150090616     c                   ExSr      Sr_f02
089160090616
089170090616     c                   If        ta12err <> *Blanks
089180090616     c                   Eval      *In28 = *On
089190100730     c                   Eval      h5riga = 20
089200100730     c                   Eval      h5colo = 71
089210090616     c                   Eval      v5cmsg = ta12msg
089220090616     c                   Leavesr
089230090616     c                   EndIf
089240090616
089250090616      * Controllo se ora c'è o non c'è più la nota
089260091112     c   70
089270091112     corn06              Eval      kntcapl = 'C'
089280100806     c   06
089290091112     cann70              Eval      kntcapl = 'T'
089300090616     c                   Movel(p)  dutkci        kntcnk1
089310091112     c   70
089320091112     corn06              Move      v1cksc        kntcnk1
089330100806     c   06
089340100806     cann70              Move      v1cnrv        kntcnk1
089350090616     c                   Clear                   kntcnk2
089360090616     c                   Movel     '89'          kntctnt
089370091119     c     kTfntc        Chain(n)  Tfntc01l
089380090616     c                   If        %Found(Tfntc01l)
089390090616     c                   Eval      *In40 = *On
089400090616     c                   Else
089410090616     c                   Eval      *In40 = *Off
089420090616     c                   EndIf
089430090616
089440090616     c                   Clear                   v5cnt89
089450100730     c                   Eval      h5riga = 20
089460100730     c                   Eval      h5colo = 71
089470090616     c                   Goto      emi05
089480090616
089490090616    1c                   EndIf
089500090616
089510090616     c                   EndSr
089520100729      /free
089530100729       //--------------------------------------------------------------
089540100729       //?Controllo nota 82 - E-mail Bonifico Danni
089550100729       //--------------------------------------------------------------
089560100729       BEGSR sr_CtrNt82;
089570100730
089580100730         IF  v5cnt82 <> *blanks;
089590100730           *in90 = *on;
089600100730       //?Se sono in interrogazione non posso interrogare la note
089610100730       //?se non è presente
089620100730           IF  *in05 and not *in68;
089630100730             *in28 = *on;
089640100730             h5riga = 21;
089650100730             h5colo = 71;
089660100730             v5cmsg = msg(52);
089670100730             leavesr;
089680100730           ENDIF;
089690100730       //?Imposto se interrogazione o gestione della nota
089700100730           IF  not *in05;
089710100730             wrich = 'M';
089720100730           ENDIF;
089730100730           IF  *in05;
089740100730             wrich = 'V';
089750100730           ENDIF;
089760100730           wrag = v3crag;
089770100730           wtnt = '82';
089780100730           exsr sr_f02;
089790100730           IF  ta12err <> *blanks;
089800100730             *in28 = *on;
089810100730             h5riga = 21;
089820100730             h5colo = 71;
089830100730             v5cmsg = ta12msg;
089840100730             leavesr;
089850100730           ENDIF;
089860100730       //?Controllo se ora c'è la nota oppure se è stata cancellata
089870100730           SELECT;
089880100730             WHEN  *in70 or not *in06;
089890100730               kntcapl = 'C';
089900100730               %subst(kntcnk1:5:7) = %editc(v1cksc:'X');
089910100806             WHEN  *in06 and not *in70;
089920100730               kntcapl = 'T';
089930100806               %subst(kntcnk1:5:7) = %editc(v1cnrv:'X');
089940100730           ENDSL;
089950100730           %subst(kntcnk1:1:4) = %editc(DUTkci:'X');
089960100730           clear kntcnk2;
089970100730           kntctnt = '82';
089980150515           chain(n) (kntcapl:kntcnk1:kntcnk2:kntctnt) TFNTC01L;
089990100730           IF  %found(TFNTC01L);
090000100730             *in68 = *on;
090010100730           ELSE;
090020100730             *in68 = *off;
090030100730           ENDIF;
090040100730           clear v5cnt82;
090050100730           h5riga = 21;
090060100730           h5colo = 71;
090070100730         ENDIF;
090080100729
090090100729       ENDSR;
090100100729      /end-free
090110051207
090120051207      *------------------------------------------------------------------------*
090130051212      * CONTROLLO TIPO COMUNICAZIONE MITTENTE
090140051207      *------------------------------------------------------------------------*
090150051207     c     Sr_Ctrtcm     BegSr
090160051212
090170051212      * Ricerca
090180051212     c                   Clear                   v6dtcm
090190051212     c                   If        v6ctcm = '?'
090200051212     c                   Eval      kcod = '2F'
090210051212     c                   call      'TRUL19R'
090220051212     c                   parm                    kcod
090230051212     c                   parm                    ordina
090240051212     c                   parm                    kkey
090250051212     c                   parm                    comando
090260051212     c                   Eval      v6ctcm = kkey
090270051212     c                   Eval      h6riga = 07
090280051212     c                   Eval      h6colo = 38
090290051212     c                   Eval      *In90 = *On
090300051212     c                   EndIf
090310120118
090320120118      /free
090330120118       //?Se anagrafica provvisoria accetto il campo vuoto
090340120118         IF  *in06 and V6Ctcm = *blanks;
090350120118           leavesr;
090360120118         ENDIF;
090370120118      /end-free
090380051212
090390051212      * Controllo
090400051212     c                   Clear                   v6dtcm
090410051212     c                   Clear                   ds2f
090420051212     c                   Eval      kcod = '2F'
090430051212     c                   Eval      kkey = v6ctcm
090440051212     c     kTabel        Chain     Tabel00f
090450051212     c                   If        Not %Found(Tabel00f) or
090460051212     c                             tblflg <> *Blanks
090470051212     c                   Eval      *In28 = *On
090480051212     c                   Eval      h6riga = 07
090490051212     c                   Eval      h6colo = 38
090500051212     c                   Eval      v6cmsg = msg(62)
090510051212     c                   Leavesr
090520051212     c                   EndIf
090530051212     c                   Eval      ds2f = tbluni
090540051212     c                   Eval      v6dtcm = §2fdes
090550051212
090560051212      * Se tipo comunicazione con invio tramite fax
090570051212if  1c                   If        §2fftp = 'F'
090580051212      * Non ammesso per i clienti vari
090590051212     c                   If        wksc04 = 8888 or wksc04 = 9999
090600051212     c                   Eval      *In28 = *On
090610051212     c                   Eval      h6riga = 07
090620051212     c                   Eval      h6colo = 38
090630051212     c                   Eval      v6cmsg = msg(63)
090640051212     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
090650051212     c                             'per clienti vari'
090660051212     c                   Leavesr
090670051212     c                   EndIf
090680051216      * Controllo se fax sul luogo 005 è esatto
090690051216if  2c                   If        *In14 and wfax005 <> *Blanks
090700051216     c                   Clear                   trul42ds
090710051216     c                   Eval      d42fax = wfax005
090720051216     c                   Call      'TRUL42R'
090730051216     c                   Parm                    trul42ds
090740051216if  3c                   If        d42err = '1'
090750051216     c                   Eval      *In28 = *On
090760051216     c                   Eval      h6riga = 07
090770051216     c                   Eval      h6colo = 38
090780051216     c                   Eval      v6cmsg = d42msg
090790051216     c                   Leavesr
090800051216    3c                   EndIf
090810051216    2c                   EndIf
090820051216      * Se non c'è il luogo o se il n. fax non è presente sul luogo
090830051216      * deve essere impostato sull'anagrafica
090840051216     c                   If        v2ctlf = *Blanks and wfax005 = *Blanks
090850051212     c                   Eval      *In28 = *On
090860051212     c                   Eval      h6riga = 07
090870051212     c                   Eval      h6colo = 38
090880051212     c                   Eval      v6cmsg = msg(63)
090890051212     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
090900051212     c                             'nr. fax obbligatorio'
090910051212     c                   Leavesr
090920051212     c                   EndIf
090930051212    1c                   EndIf
090940051219
090950051219      * Se tipo comunicazione con invio tramite mail
090960051219if  1c                   If        §2fftp = 'M'
090970051219      * Non ammesso per i clienti vari
090980051219     c                   If        wksc04 = 8888 or wksc04 = 9999
090990051219     c                   Eval      *In28 = *On
091000051219     c                   Eval      h6riga = 07
091010051219     c                   Eval      h6colo = 38
091020051219     c                   Eval      v6cmsg = msg(63)
091030051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
091040051219     c                             'per clienti vari'
091050051219     c                   Leavesr
091060051219     c                   EndIf
091070051219      * Controllo se c'è la mail nel luogo
091080051219     c                   If        *In14 and wmail005 = *Blanks
091090051219     c                   Eval      *In28 = *On
091100051219     c                   Eval      h6riga = 07
091110051219     c                   Eval      h6colo = 38
091120051219     c                   Eval      v6cmsg = msg(63)
091130051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
091140051219     c                             'mail non immessa nel luogo 005'
091150051219     c                   Leavesr
091160051219     c                   EndIf
091170051219      * Se non c'è il luogo deve esserci la nota 88
091180051219     c                   If        Not *In14 and Not *In15
091190051219     c                   Eval      *In28 = *On
091200051219     c                   Eval      h6riga = 07
091210051219     c                   Eval      h6colo = 38
091220051219     c                   Eval      v6cmsg = msg(63)
091230051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
091240051219     c                             'nota 88 non immessa'
091250051219     c                   Leavesr
091260051219     c                   EndIf
091270051219    1c                   EndIf
091280051207
091290051207     c                   EndSr
091300051207
091310051207      *------------------------------------------------------------------------*
091320051212      * CONTROLLO TIPO COMUNICAZIONE FINE GIACENZA
091330051207      *------------------------------------------------------------------------*
091340051207     c     Sr_Ctrtcf     BegSr
091350051212
091360051212      * Ricerca
091370051212     c                   Clear                   v6dtcf
091380051212     c                   If        v6ctcf = '?'
091390051212     c                   Eval      kcod = '2H'
091400051212     c                   call      'TRUL19R'
091410051212     c                   parm                    kcod
091420051212     c                   parm                    ordina
091430051212     c                   parm                    kkey
091440051212     c                   parm                    comando
091450051212     c                   Eval      v6ctcf = kkey
091460051212     c                   Eval      h6riga = 08
091470051212     c                   Eval      h6colo = 38
091480051212     c                   Eval      *In90 = *On
091490051212     c                   EndIf
091500051212
091510051212      * Controllo
091520051212     c                   Clear                   v6dtcf
091530051212     c                   Clear                   ds2h
091540051212     c                   Eval      kcod = '2H'
091550051212     c                   Eval      kkey = v6ctcf
091560051212     c     kTabel        Chain     Tabel00f
091570051212     c                   If        Not %Found(Tabel00f) or
091580051212     c                             tblflg <> *Blanks
091590051212     c                   Eval      *In28 = *On
091600051212     c                   Eval      h6riga = 08
091610051212     c                   Eval      h6colo = 38
091620060216     c                   Eval      v6cmsg = msg(70)
091630051212     c                   Leavesr
091640051212     c                   EndIf
091650051212     c                   Eval      ds2h = tbluni
091660051212     c                   Eval      v6dtcf = §2hdes
091670051212
091680051212      * Se tipo comunicazione con invio tramite fax
091690051212if  1c                   If        §2hfax = 'F'
091700051212      * Non ammesso per i clienti vari
091710051212     c                   If        wksc04 = 8888 or wksc04 = 9999
091720051212     c                   Eval      *In28 = *On
091730051212     c                   Eval      h6riga = 08
091740051212     c                   Eval      h6colo = 38
091750051212     c                   Eval      v6cmsg = msg(63)
091760051212     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
091770051212     c                             'per clienti vari'
091780051212     c                   Leavesr
091790051212     c                   EndIf
091800051216      * Controllo se fax sul luogo 005 è esatto
091810051216if  2c                   If        *In14 and wfax005 <> *Blanks
091820051216     c                   Clear                   trul42ds
091830051216     c                   Eval      d42fax = wfax005
091840051216     c                   Call      'TRUL42R'
091850051216     c                   Parm                    trul42ds
091860051216if  3c                   If        d42err = '1'
091870051216     c                   Eval      *In28 = *On
091880051216     c                   Eval      h6riga = 08
091890051216     c                   Eval      h6colo = 38
091900051216     c                   Eval      v6cmsg = d42msg
091910051216     c                   Leavesr
091920051216    3c                   EndIf
091930051216    2c                   EndIf
091940051216      * Se non c'è il luogo o se il n. fax non è presente sul luogo
091950051216      * deve essere impostato sull'anagrafica
091960051216     c                   If        v2ctlf = *Blanks and wfax005 = *Blanks
091970051212     c                   Eval      *In28 = *On
091980051212     c                   Eval      h6riga = 08
091990051212     c                   Eval      h6colo = 38
092000051212     c                   Eval      v6cmsg = msg(63)
092010051212     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
092020051212     c                             'nr. fax obbligatorio'
092030051212     c                   Leavesr
092040051212     c                   EndIf
092050051212    1c                   EndIf
092060051219      * Se tipo comunicazione con invio tramite mail
092070051219if  1c                   If        §2hfax = 'M'
092080051219      * Non ammesso per i clienti vari
092090051219     c                   If        wksc04 = 8888 or wksc04 = 9999
092100051219     c                   Eval      *In28 = *On
092110051219     c                   Eval      h6riga = 08
092120051219     c                   Eval      h6colo = 38
092130051219     c                   Eval      v6cmsg = msg(63)
092140051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
092150051219     c                             'per clienti vari'
092160051219     c                   Leavesr
092170051219     c                   EndIf
092180051219      * Controllo se c'è la mail nel luogo
092190051219     c                   If        *In14 and wmail005 = *Blanks
092200051219     c                   Eval      *In28 = *On
092210051219     c                   Eval      h6riga = 08
092220051219     c                   Eval      h6colo = 38
092230051219     c                   Eval      v6cmsg = msg(63)
092240051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
092250051219     c                             'mail non immessa nel luogo 005'
092260051219     c                   Leavesr
092270051219     c                   EndIf
092280051219      * Se non c'è il luogo deve esserci la nota 88
092290051219     c                   If        Not *In14 and Not *In15
092300051219     c                   Eval      *In28 = *On
092310051219     c                   Eval      h6riga = 08
092320051219     c                   Eval      h6colo = 38
092330051219     c                   Eval      v6cmsg = msg(63)
092340051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
092350051219     c                             'nota 88 non immessa'
092360051219     c                   Leavesr
092370051219     c                   EndIf
092380051219    1c                   EndIf
092390051207
092400051207     c                   EndSr
092410051207
092420051207      *------------------------------------------------------------------------*
092430051212      * CONTROLLO APERTURA AUTOMATICA GIACENZA
092440051207      *------------------------------------------------------------------------*
092450051207     c     Sr_Ctrapg     BegSr
092460051212
092470051212      * Ricerca
092480051212     c                   Clear                   v6dapg
092490051212     c                   If        v6capg = '?'
092500051212     c                   Eval      kcod = '2U'
092510051212     c                   call      'TRUL19R'
092520051212     c                   parm                    kcod
092530051212     c                   parm                    ordina
092540051212     c                   parm                    kkey
092550051212     c                   parm                    comando
092560051212     c                   Eval      v6capg = kkey
092570051212     c                   Eval      h6riga = 09
092580051212     c                   Eval      h6colo = 38
092590051212     c                   Eval      *In90 = *On
092600051212     c                   EndIf
092610051212
092620051212      * Controllo
092630051212     c                   Clear                   v6dapg
092640051212     c                   Clear                   ds2u
092650051212     c                   Eval      kcod = '2U'
092660051212     c                   Eval      kkey = v6capg
092670051212     c     kTabel        Chain     Tabel00f
092680051212     c                   If        Not %Found(Tabel00f) or
092690051212     c                             tblflg <> *Blanks
092700051212     c                   Eval      *In28 = *On
092710051212     c                   Eval      h6riga = 09
092720051212     c                   Eval      h6colo = 38
092730051212     c                   Eval      v6cmsg = msg(64)
092740051212     c                   Leavesr
092750051212     c                   EndIf
092760051212     c                   Eval      ds2u = tbluni
092770051212     c                   Eval      v6dapg = §2udes
092780051212
092790051212      * Non ammessa apertura automatica per i clienti vari
092800051212     c                   If        §2uape = 'S' and
092810051212     c                             (wksc04 = 8888 or wksc04 = 9999)
092820051212     c                   Eval      *In28 = *On
092830051212     c                   Eval      h6riga = 09
092840051212     c                   Eval      h6colo = 38
092850051212     c                   Eval      v6cmsg = msg(65)
092860051212     c                   Leavesr
092870051212     c                   EndIf
092880051207
092890051207     c                   EndSr
092900051207
092910051207      *------------------------------------------------------------------------*
092920051212      * CONTROLLO DISPOSIZIONI MITTENTE IN ARRIVO
092930051207      *------------------------------------------------------------------------*
092940051207     c     Sr_Ctrdmg     BegSr
092950051212
092960051212      * Non ammesse disposizioni mittente in arrivo per i clienti vari
092970051212     c                   If        v6cdmg = 'SI' and
092980051212     c                             (wksc04 = 8888 or wksc04 = 9999)
092990051212     c                   Eval      *In28 = *On
093000051212     c                   Eval      h6riga = 10
093010051212     c                   Eval      h6colo = 38
093020051212     c                   Eval      v6cmsg = msg(66)
093030051212     c                   Leavesr
093040051212     c                   EndIf
093050051207
093060051207     c                   EndSr
093070060801
093080060801      *------------------------------------------------------------------------*
093090060801      * CONTROLLO CHIUSURA AUTOMATICA GIACENZA
093100060801      *------------------------------------------------------------------------*
093110060801     c     Sr_Ctrcag     BegSr
093120060801
093130060801      * Non ammessa chiusura automatica giacenza per i clienti vari
093140060801     c                   If        v6ccag = 'SI' and
093150060801     c                             (wksc04 = 8888 or wksc04 = 9999)
093160060801     c                   Eval      *In28 = *On
093170060801     c                   Eval      h6riga = 11
093180060801     c                   Eval      h6colo = 38
093190060801     c                   Eval      v6cmsg = msg(74)
093200060801     c                   Leavesr
093210060801     c                   EndIf
093220060801
093230060801     c                   EndSr
093240051207
093250051207      *------------------------------------------------------------------------*
093260051220      * CONTROLLO LUOGO 005 - LUOGO SPEDIZIONE GIACENZA
093270051207      *------------------------------------------------------------------------*
093280051207     c     Sr_Ctrl005    BegSr
093290051212
093300051212if  1c                   If        v6cl005 <> *Blanks
093310060203
093320060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
093330060203     c                   If        *In05 and Not *In14
093340051212     c                   Eval      *In28 = *On
093350051212     c                   Eval      h6riga = 13
093360060203     c                   Eval      h6colo = 35
093370051212     c                   Eval      v6cmsg = msg(50)
093380051212     c                   Leavesr
093390051212     c                   EndIf
093400071128
093410071128      * se non è interrogazione controllo se luogo utilizzabile
093420071128     c                   if        not *in09 and not *in05 and $ufi005 <> 'S'
093430071128     c                   eval      *In28 = *on
093440071128     c                   eval      h6riga = 13
093450071128     c                   eval      h6colo = 35
093460071128     c                   eval      v6cmsg = msg(76)
093470071128     c                   leavesr
093480071128     c                   endif
093490051212
093500060203      * Imposto se interrogazione o gestione
093510051213     c                   Clear                   tnta81ds
093520060203     c   05              Eval      i81opz = '5'
093530060203     c  n05
093540060214     can 14              Eval      i81opz = '2'
093550051213     c                   Eval      i81cod = '005'
093560051213     c                   Eval      i81cli = v1cksc
093570060216     c                   Eval      i81gcli = '1'
093580060216     c   01              Eval      i81icli = '1'
093590051213     c                   Call      'TNTA81R'
093600051213     c                   Parm                    kpjba
093610051213     c                   Parm                    tnta81ds
093620051212
093630051212      * Controllo se ora c'è o non c'è più il luogo
093640060112     c                   Clear                   wfax005
093650060112     c                   Clear                   wmail005
093660051216     c                   Eval      kspecli = v1cksc
093670051216     c                   Eval      kspecod = '005'
093680051212     c     kFnspe        Chain     Fnspe01l
093690051212     c                   If        %Found(Fnspe01l) and speflg = *Blanks
093700051212     c                   Eval      *In14 = *On
093710101026     c                   Eval      wfax005 = spefax
093720101026     c     kFnsp2        Chain     Fnsp201l
093730101026     c                   If        %Found(Fnsp201l) and sp2flg = *Blanks
093740101026     c                   Eval      wmail005 = sp2est
093750101026     c                   EndIf
093760051212     c                   Else
093770051212     c                   Eval      *In14 = *Off
093780051212     c                   EndIf
093790051212
093800051212     c                   Clear                   v6cl005
093810051212     c                   Eval      h6riga = 13
093820060203     c                   Eval      h6colo = 35
093830051212     c                   Goto      emi06
093840051212
093850051212    1c                   EndIf
093860051207
093870051207     c                   EndSr
093880051207
093890051207      *------------------------------------------------------------------------*
093900051220      * CONTROLLO NOTA 88 - E-MAIL GIACENZE
093910051207      *------------------------------------------------------------------------*
093920051207     c     Sr_Ctrnt88    BegSr
093930051212
093940051212if  1c                   If        v6cnt88 <> *Blanks
093950060203
093960060203      * Se sono in interrogazione non posso interrogare la nota se non c'è
093970060203     c                   If        *In05 and Not *In15
093980051212     c                   Eval      *In28 = *On
093990051212     c                   Eval      h6riga = 14
094000060203     c                   Eval      h6colo = 35
094010051219     c                   Eval      v6cmsg = msg(52)
094020051212     c                   Leavesr
094030051212     c                   EndIf
094040051212
094050060203      * Imposto se interrogazione o gestione
094060060203     c  n05              Eval      wrich = 'M'
094070060203     c   05              Eval      wrich = 'V'
094080060217     c                   Eval      wrag = v3crag
094090051212     c                   Eval      wtnt = '88'
094100051212     c                   ExSr      Sr_f02
094110060215
094120060215     c                   If        ta12err <> *Blanks
094130060215     c                   Eval      *In28 = *On
094140060215     c                   Eval      h6riga = 14
094150060215     c                   Eval      h6colo = 35
094160060215     c                   Eval      v6cmsg = ta12msg
094170060215     c                   Leavesr
094180060215     c                   EndIf
094190051212
094200060215      * Controllo se ora c'è o non c'è più la nota
094210091112     c   70
094220091112     corn06              Eval      kntcapl = 'C'
094230100806     c   06
094240091112     cann70              Eval      kntcapl = 'T'
094250051216     c                   Movel(p)  dutkci        kntcnk1
094260091112     c   70
094270091112     corn06              Move      v1cksc        kntcnk1
094280100806     c   06
094290100806     cann70              Move      v1cnrv        kntcnk1
094300051216     c                   Clear                   kntcnk2
094310051216     c                   Movel     '88'          kntctnt
094320091119     c     kTfntc        Chain(n)  Tfntc01l
094330051212     c                   If        %Found(Tfntc01l)
094340051212     c                   Eval      *In15 = *On
094350051212     c                   Else
094360051212     c                   Eval      *In15 = *Off
094370051212     c                   EndIf
094380051212
094390051212     c                   Clear                   v6cnt88
094400051212     c                   Eval      h6riga = 14
094410060203     c                   Eval      h6colo = 35
094420051212     c                   Goto      emi06
094430051212
094440051212    1c                   EndIf
094450051207
094460051207     c                   EndSr
094470060208
094480060208      *------------------------------------------------------------------------*
094490060208      * CONTROLLO LUOGO 300 - MERCE RESA
094500060208      *------------------------------------------------------------------------*
094510060208     c     Sr_Ctrl300    BegSr
094520060208
094530060208if  1c                   If        v6cl300 <> *Blanks
094540060208
094550060208      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
094560060208     c                   If        *In05 and Not *In33
094570060208     c                   Eval      *In28 = *On
094580060208     c                   Eval      h6riga = 15
094590060208     c                   Eval      h6colo = 35
094600060208     c                   Eval      v6cmsg = msg(50)
094610060208     c                   Leavesr
094620060208     c                   EndIf
094630071128
094640071128      * se non è interrogazione controllo se luogo utilizzabile
094650071128     c                   if        not *in09 and not *in05 and $ufi300 <> 'S'
094660071128     c                   eval      *In28 = *on
094670071128     c                   eval      h6riga = 15
094680071128     c                   eval      h6colo = 35
094690071128     c                   eval      v6cmsg = msg(76)
094700071128     c                   leavesr
094710071128     c                   endif
094720060208
094730060208      * Imposto se interrogazione o gestione
094740060208     c                   Clear                   tnta81ds
094750060208     c   05              Eval      i81opz = '5'
094760060208     c  n05
094770060214     can 33              Eval      i81opz = '2'
094780060208     c                   Eval      i81cod = '300'
094790060208     c                   Eval      i81cli = v1cksc
094800060216     c                   Eval      i81gcli = '1'
094810060216     c   01              Eval      i81icli = '1'
094820060208     c                   Call      'TNTA81R'
094830060208     c                   Parm                    kpjba
094840060208     c                   Parm                    tnta81ds
094850060208
094860060208      * Controllo se ora c'è o non c'è più il luogo
094870060208     c                   Eval      kspecli = v1cksc
094880060208     c                   Eval      kspecod = '300'
094890060208     c     kFnspe        Chain     Fnspe01l
094900060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
094910060208     c                   Eval      *In33 = *On
094920060208     c                   Else
094930060208     c                   Eval      *In33 = *Off
094940060208     c                   EndIf
094950060208
094960060208     c                   Clear                   v6cl300
094970060208     c                   Eval      h6riga = 15
094980060208     c                   Eval      h6colo = 35
094990060208     c                   Goto      emi06
095000060208
095010060208    1c                   EndIf
095020060208
095030060208     c                   EndSr
095040051213
095050051213      *------------------------------------------------------------------------*
095060051213      * CONTROLLO LUOGO 006 - PREAVVISO/AVVISO DANNO
095070051213      *------------------------------------------------------------------------*
095080051213     c     Sr_Ctrl006    BegSr
095090051213
095100051213if  1c                   If        v7cl006 <> *Blanks
095110060203
095120060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
095130060203     c                   If        *In05 and Not *In16
095140051213     c                   Eval      *In28 = *On
095150051213     c                   Eval      h7riga = 12
095160060203     c                   Eval      h7colo = 35
095170051213     c                   Eval      v7cmsg = msg(50)
095180051213     c                   Leavesr
095190051213     c                   EndIf
095200071128
095210071128      * se non è interrogazione controllo se luogo utilizzabile
095220071128     c                   if        not *in09 and not *in05 and $ufi006 <> 'S'
095230071128     c                   eval      *In28 = *on
095240071128     c                   eval      h7riga = 12
095250071128     c                   eval      h7colo = 35
095260071128     c                   eval      v7cmsg = msg(76)
095270071128     c                   leavesr
095280071128     c                   endif
095290051213
095300060203      * Imposto se interrogazione o gestione
095310051213     c                   Clear                   tnta81ds
095320060203     c   05              Eval      i81opz = '5'
095330060203     c  n05
095340060214     can 16              Eval      i81opz = '2'
095350051213     c                   Eval      i81cod = '006'
095360051213     c                   Eval      i81cli = v1cksc
095370060216     c                   Eval      i81gcli = '1'
095380060216     c   01              Eval      i81icli = '1'
095390051213     c                   Call      'TNTA81R'
095400051213     c                   Parm                    kpjba
095410051213     c                   Parm                    tnta81ds
095420051213
095430051213      * Controllo se ora c'è o non c'è più il luogo
095440051216     c                   Eval      kspecli = v1cksc
095450051216     c                   Eval      kspecod = '006'
095460051213     c     kFnspe        Chain     Fnspe01l
095470051213     c                   If        %Found(Fnspe01l) and speflg = *Blanks
095480051213     c                   Eval      *In16 = *On
095490051213     c                   Else
095500051213     c                   Eval      *In16 = *Off
095510051213     c                   EndIf
095520051213
095530051213     c                   Clear                   v7cl006
095540051213     c                   Eval      h7riga = 12
095550060203     c                   Eval      h7colo = 35
095560051213     c                   Goto      emi07
095570051213
095580051213    1c                   EndIf
095590051213
095600051213     c                   EndSr
095610051213
095620051213      *------------------------------------------------------------------------*
095630051220      * CONTROLLO NOTA 87 - E-MAIL DANNI
095640051213      *------------------------------------------------------------------------*
095650051213     c     Sr_Ctrnt87    BegSr
095660051213
095670051213if  1c                   If        v7cnt87 <> *Blanks
095680060203
095690060203      * Se sono in interrogazione non posso interrogare la nota se non c'è
095700060203     c                   If        *In05 and Not *In18
095710051213     c                   Eval      *In28 = *On
095720051213     c                   Eval      h7riga = 13
095730060203     c                   Eval      h7colo = 35
095740051219     c                   Eval      v7cmsg = msg(52)
095750051213     c                   Leavesr
095760051213     c                   EndIf
095770051213
095780060203      * Imposto se interrogazione o gestione
095790060203     c  n05              Eval      wrich = 'M'
095800060203     c   05              Eval      wrich = 'V'
095810060217     c                   Eval      wrag = v3crag
095820051213     c                   Eval      wtnt = '87'
095830051213     c                   ExSr      Sr_f02
095840060215
095850060215     c                   If        ta12err <> *Blanks
095860060215     c                   Eval      *In28 = *On
095870060215     c                   Eval      h7riga = 13
095880060215     c                   Eval      h7colo = 35
095890060215     c                   Eval      v7cmsg = ta12msg
095900060215     c                   Leavesr
095910060215     c                   EndIf
095920051213
095930060215      * Controllo se ora c'è o non c'è più la nota
095940091112     c   70
095950091112     corn06              Eval      kntcapl = 'C'
095960100806     c   06
095970091112     cann70              Eval      kntcapl = 'T'
095980051216     c                   Movel(p)  dutkci        kntcnk1
095990091112     c   70
096000091112     corn06              Move      v1cksc        kntcnk1
096010100806     c   06
096020100806     cann70              Move      v1cnrv        kntcnk1
096030051216     c                   Clear                   kntcnk2
096040051216     c                   Movel     '87'          kntctnt
096050091119     c     kTfntc        Chain(n)  Tfntc01l
096060051213     c                   If        %Found(Tfntc01l)
096070051213     c                   Eval      *In18 = *On
096080051213     c                   Else
096090051213     c                   Eval      *In18 = *Off
096100051213     c                   EndIf
096110051213
096120051213     c                   Clear                   v7cnt87
096130051213     c                   Eval      h7riga = 13
096140060203     c                   Eval      h7colo = 35
096150051213     c                   Goto      emi07
096160051213
096170051213    1c                   EndIf
096180051213
096190051213     c                   EndSr
096200051213
096210051213      *------------------------------------------------------------------------*
096220051213      * CONTROLLO LUOGO 008 - PROGETTO DI LIQUIDAZIONE
096230051213      *------------------------------------------------------------------------*
096240051213     c     Sr_Ctrl008    BegSr
096250051213
096260051213if  1c                   If        v7cl008 <> *Blanks
096270060203
096280060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
096290060203     c                   If        *In05 and Not *In17
096300051213     c                   Eval      *In28 = *On
096310051213     c                   Eval      h7riga = 14
096320060203     c                   Eval      h7colo = 35
096330051213     c                   Eval      v7cmsg = msg(50)
096340051213     c                   Leavesr
096350051213     c                   EndIf
096360071128
096370071128      * se non è interrogazione controllo se luogo utilizzabile
096380071128     c                   if        not *in09 and not *in05 and $ufi008 <> 'S'
096390071128     c                   eval      *In28 = *on
096400071128     c                   eval      h7riga = 14
096410071128     c                   eval      h7colo = 35
096420071128     c                   eval      v7cmsg = msg(76)
096430071128     c                   leavesr
096440071128     c                   endif
096450051213
096460060203      * Imposto se interrogazione o gestione
096470051213     c                   Clear                   tnta81ds
096480060203     c   05              Eval      i81opz = '5'
096490060203     c  n05
096500060214     can 17              Eval      i81opz = '2'
096510051213     c                   Eval      i81cod = '008'
096520051213     c                   Eval      i81cli = v1cksc
096530060216     c                   Eval      i81gcli = '1'
096540060216     c   01              Eval      i81icli = '1'
096550051213     c                   Call      'TNTA81R'
096560051213     c                   Parm                    kpjba
096570051213     c                   Parm                    tnta81ds
096580051213
096590051213      * Controllo se ora c'è o non c'è più il luogo
096600051216     c                   Eval      kspecli = v1cksc
096610051216     c                   Eval      kspecod = '008'
096620051213     c     kFnspe        Chain     Fnspe01l
096630051213     c                   If        %Found(Fnspe01l) and speflg = *Blanks
096640051213     c                   Eval      *In17 = *On
096650051213     c                   Else
096660051213     c                   Eval      *In17 = *Off
096670051213     c                   EndIf
096680051213
096690051213     c                   Clear                   v7cl008
096700051213     c                   Eval      h7riga = 14
096710060203     c                   Eval      h7colo = 35
096720051213     c                   Goto      emi07
096730051213
096740051213    1c                   EndIf
096750051213
096760051213     c                   EndSr
096770051213
096780051213      *------------------------------------------------------------------------*
096790051213      * CONTROLLO NUMERO STOP PER RITIRO
096800051213      *------------------------------------------------------------------------*
096810051213     c     Sr_Ctrstp     BegSr
096820051213
096830051213     c                   If        v7cstp > 800
096840051213     c                   Eval      *In28 = *On
096850051213     c                   Eval      h7riga = 18
096860051213     c                   Eval      h7colo = 37
096870051213     c                   Eval      v7cmsg = msg(67)
096880051213     c                   Leavesr
096890051213     c                   EndIf
096900051213
096910051213     c                   EndSr
096920060208
096930060208      *------------------------------------------------------------------------*
096940060208      * CONTROLLO LUOGO 002 - INVIO ORM ESTERO
096950060208      *------------------------------------------------------------------------*
096960060208     c     Sr_Ctrl002    BegSr
096970060208
096980060208if  1c                   If        v7cl002 <> *Blanks
096990060208
097000060208      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
097010060208     c                   If        *In05 and Not *In35
097020060208     c                   Eval      *In28 = *On
097030060208     c                   Eval      h7riga = 20
097040060208     c                   Eval      h7colo = 35
097050060208     c                   Eval      v7cmsg = msg(50)
097060060208     c                   Leavesr
097070060208     c                   EndIf
097080071128
097090071128      * se non è interrogazione controllo se luogo utilizzabile
097100071128     c                   if        not *in09 and not *in05 and $ufi002 <> 'S'
097110071128     c                   eval      *In28 = *on
097120071128     c                   eval      h7riga = 20
097130071128     c                   eval      h7colo = 35
097140071128     c                   eval      v7cmsg = msg(76)
097150071128     c                   leavesr
097160071128     c                   endif
097170060208
097180060208      * Imposto se interrogazione o gestione
097190060208     c                   Clear                   tnta81ds
097200060208     c   05              Eval      i81opz = '5'
097210060208     c  n05
097220060214     can 35              Eval      i81opz = '2'
097230060208     c                   Eval      i81cod = '002'
097240060208     c                   Eval      i81cli = v1cksc
097250060216     c                   Eval      i81gcli = '1'
097260060216     c   01              Eval      i81icli = '1'
097270060208     c                   Call      'TNTA81R'
097280060208     c                   Parm                    kpjba
097290060208     c                   Parm                    tnta81ds
097300060208
097310060208      * Controllo se ora c'è o non c'è più il luogo
097320060208     c                   Eval      kspecli = v1cksc
097330060208     c                   Eval      kspecod = '002'
097340060208     c     kFnspe        Chain     Fnspe01l
097350060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
097360060208     c                   Eval      *In35 = *On
097370060208     c                   Else
097380060208     c                   Eval      *In35 = *Off
097390060208     c                   EndIf
097400060208
097410060208     c                   Clear                   v7cl002
097420060208     c                   Eval      h7riga = 20
097430060208     c                   Eval      h7colo = 35
097440060208     c                   Goto      emi07
097450060208
097460060208    1c                   EndIf
097470060208
097480060208     c                   EndSr
097490060208
097500060208      *------------------------------------------------------------------------*
097510060208      * CONTROLLO NOTA 83 - E-MAIL ORM ESTERO
097520060208      *------------------------------------------------------------------------*
097530060208     c     Sr_Ctrnt83    BegSr
097540060208
097550060208if  1c                   If        v7cnt83 <> *Blanks
097560060208
097570060208      * Se sono in interrogazione non posso interrogare la nota se non c'è
097580060208     c                   If        *In05 and Not *In34
097590060208     c                   Eval      *In28 = *On
097600060208     c                   Eval      h7riga = 21
097610060208     c                   Eval      h7colo = 35
097620060208     c                   Eval      v7cmsg = msg(52)
097630060208     c                   Leavesr
097640060208     c                   EndIf
097650060208
097660060208      * Imposto se interrogazione o gestione
097670060208     c  n05              Eval      wrich = 'M'
097680060208     c   05              Eval      wrich = 'V'
097690060217     c                   Eval      wrag = v3crag
097700060208     c                   Eval      wtnt = '83'
097710060208     c                   ExSr      Sr_f02
097720060215
097730060215     c                   If        ta12err <> *Blanks
097740060215     c                   Eval      *In28 = *On
097750060215     c                   Eval      h7riga = 21
097760060215     c                   Eval      h7colo = 35
097770060215     c                   Eval      v7cmsg = ta12msg
097780060215     c                   Leavesr
097790060215     c                   EndIf
097800060208
097810060215      * Controllo se ora c'è o non c'è più la nota
097820091112     c   70
097830091112     corn06              Eval      kntcapl = 'C'
097840100806     c   06
097850091112     cann70              Eval      kntcapl = 'T'
097860060208     c                   Movel(p)  dutkci        kntcnk1
097870091112     c   70
097880091112     corn06              Move      v1cksc        kntcnk1
097890100806     c   06
097900100806     cann70              Move      v1cnrv        kntcnk1
097910060208     c                   Clear                   kntcnk2
097920060208     c                   Movel     '83'          kntctnt
097930091119     c     kTfntc        Chain(n)  Tfntc01l
097940060208     c                   If        %Found(Tfntc01l)
097950060208     c                   Eval      *In34 = *On
097960060208     c                   Else
097970060208     c                   Eval      *In34 = *Off
097980060208     c                   EndIf
097990060208
098000060208     c                   Clear                   v7cnt83
098010060208     c                   Eval      h7riga = 21
098020060208     c                   Eval      h7colo = 35
098030060208     c                   Goto      emi07
098040060208
098050060208    1c                   EndIf
098060060208
098070060208     c                   EndSr
098080060208
098090060208      *------------------------------------------------------------------------*
098100060208      * CONTROLLO LUOGO 200 - INVIO DOC. DOGANA
098110060208      *------------------------------------------------------------------------*
098120060208     c     Sr_Ctrl200    BegSr
098130060208
098140060208if  1c                   If        v7cl200 <> *Blanks
098150060208
098160060208      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
098170060208     c                   If        *In05 and Not *In37
098180060208     c                   Eval      *In28 = *On
098190060208     c                   Eval      h7riga = 20
098200060208     c                   Eval      h7colo = 72
098210060208     c                   Eval      v7cmsg = msg(50)
098220060208     c                   Leavesr
098230060208     c                   EndIf
098240071128
098250071128      * se non è interrogazione controllo se luogo utilizzabile
098260071128     c                   if        not *in09 and not *in05 and $ufi200 <> 'S'
098270071128     c                   eval      *In28 = *on
098280071128     c                   eval      h7riga = 20
098290071128     c                   eval      h7colo = 72
098300071128     c                   eval      v7cmsg = msg(76)
098310071128     c                   leavesr
098320071128     c                   endif
098330060208
098340060208      * Imposto se interrogazione o gestione
098350060208     c                   Clear                   tnta81ds
098360060208     c   05              Eval      i81opz = '5'
098370060208     c  n05
098380060214     can 37              Eval      i81opz = '2'
098390060208     c                   Eval      i81cod = '200'
098400060208     c                   Eval      i81cli = v1cksc
098410060216     c                   Eval      i81gcli = '1'
098420060216     c   01              Eval      i81icli = '1'
098430060208     c                   Call      'TNTA81R'
098440060208     c                   Parm                    kpjba
098450060208     c                   Parm                    tnta81ds
098460060208
098470060208      * Controllo se ora c'è o non c'è più il luogo
098480060208     c                   Eval      kspecli = v1cksc
098490060208     c                   Eval      kspecod = '200'
098500060208     c     kFnspe        Chain     Fnspe01l
098510060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
098520060208     c                   Eval      *In37 = *On
098530060208     c                   Else
098540060208     c                   Eval      *In37 = *Off
098550060208     c                   EndIf
098560060208
098570060208     c                   Clear                   v7cl200
098580060208     c                   Eval      h7riga = 20
098590060208     c                   Eval      h7colo = 72
098600060208     c                   Goto      emi07
098610060208
098620060208    1c                   EndIf
098630060208
098640060208     c                   EndSr
098650060208
098660060208      *------------------------------------------------------------------------*
098670060208      * CONTROLLO NOTA 86 - E-MAIL MANIFEST
098680060208      *------------------------------------------------------------------------*
098690060208     c     Sr_Ctrnt86    BegSr
098700060208
098710060208if  1c                   If        v7cnt86 <> *Blanks
098720060208
098730060208      * Se sono in interrogazione non posso interrogare la nota se non c'è
098740060208     c                   If        *In05 and Not *In36
098750060208     c                   Eval      *In28 = *On
098760060208     c                   Eval      h7riga = 21
098770060208     c                   Eval      h7colo = 72
098780060208     c                   Eval      v7cmsg = msg(52)
098790060208     c                   Leavesr
098800060208     c                   EndIf
098810060208
098820060208      * Imposto se interrogazione o gestione
098830060208     c  n05              Eval      wrich = 'M'
098840060208     c   05              Eval      wrich = 'V'
098850060217     c                   Eval      wrag = v3crag
098860060208     c                   Eval      wtnt = '86'
098870060208     c                   ExSr      Sr_f02
098880060215
098890060215     c                   If        ta12err <> *Blanks
098900060215     c                   Eval      *In28 = *On
098910060215     c                   Eval      h7riga = 21
098920060215     c                   Eval      h7colo = 72
098930060215     c                   Eval      v7cmsg = ta12msg
098940060215     c                   Leavesr
098950060215     c                   EndIf
098960060208
098970060215      * Controllo se ora c'è o non c'è più la nota
098980091112     c   70
098990091112     corn06              Eval      kntcapl = 'C'
099000100806     c   06
099010091112     cann70              Eval      kntcapl = 'T'
099020060208     c                   Movel(p)  dutkci        kntcnk1
099030091112     c   70
099040091112     corn06              Move      v1cksc        kntcnk1
099050100806     c   06
099060100806     cann70              Move      v1cnrv        kntcnk1
099070060208     c                   Clear                   kntcnk2
099080060208     c                   Movel     '86'          kntctnt
099090091119     c     kTfntc        Chain(n)  Tfntc01l
099100060208     c                   If        %Found(Tfntc01l)
099110060208     c                   Eval      *In36 = *On
099120060208     c                   Else
099130060208     c                   Eval      *In36 = *Off
099140060208     c                   EndIf
099150060208
099160060208     c                   Clear                   v7cnt86
099170060208     c                   Eval      h7riga = 21
099180060208     c                   Eval      h7colo = 72
099190060208     c                   Goto      emi07
099200060208
099210060208    1c                   EndIf
099220060208
099230060208     c                   EndSr
099240051220
099250051220      *-------------------------------------------------------------------------
099260051220      * CONTROLLO IL BLOCCO/SBLOCCO DEL CLIENTE
099270051220      *-------------------------------------------------------------------------
099280051220     c     Sr_Ctrabl     BegSr
099290051220
099300051220     c                   Eval      wokabl = *Off
099310051220     c                   Clear                   w02tes
099320051220     c                   Clear                   w02msg
099330051220
099340051220      * se ho aggiornato blocco/sblocco i clienti di fnacr
099350051220if  1c                   If        acoabl <> *Blanks and savabl = *Blanks
099360051220     c                   Eval      w02tes = 'Il cliente è stato bloccato'
099370051220      * cliente bloccato richiamo pgm esterno per bloccare fnacr
099380051220     c                   Clear                   fior50ds
099390051220     c                   Move      acoksc        i50ksc
099400140113     c                   eval      I50abl = 'B'
099410051220     c                   Call      'FIOR50R'
099420051220     c                   Parm                    fior50ds
099430051220      * se torna errore emetto il msg di errore
099440051220if  2c                   If        o50err <> *Blanks
099450051220     c                   Eval      w02msg = o50msg
099460051220      * emetto il msg che ho bloccato i clienti su fnacr
099470051220   x2c                   Else
099480051220     c                   Eval      w02msg = 'Anagrafiche clienti ritiro collega-
099490051220     c                             te bloccate'
099500051220    2c                   EndIf
099510051220     c                   Eval      wokabl = *On
099520051220    1c                   EndIf
099530051220if  1c                   If        acoabl = *Blanks and savabl <> *Blanks
099540051220     c                   Eval      w02tes = 'Il cliente è stato sbloccato'
099550051220      * cliente sbloccato emetto solo il msg
099560051220     c                   Eval      w02msg = 'Anagrafiche clienti ritiro collega-
099570051220     c                             te non aggiornate'
099580051220     c                   Eval      wokabl = *On
099590051220    1c                   EndIf
099600051220
099610051220      * Emetto la window x avvisare l'utente
099620051220     c                   If        wokabl = *On
099630051220     c                   Exfmt     ta60w02
099640051220     c                   EndIf
099650051220
099660051220     c                   EndSr
099670100128
099680100128      *-------------------------------------------------------------------------
099690100128      * Richiamo interrogazione codici fatturazione con stessa p.IVA
099700100128      *-------------------------------------------------------------------------
099710100128     c     sr_ta40       begsr
099720100128
099730100128     c                   clear                   tnta40ds
099740100128     c                   IF        v2ivaeu = *blanks
099750100128     c                   eval      ITA40iva = v2ivait
099760100128     c                   else
099770100128     c                   eval      ITA40iva = v2civa
099780100128     c                   ENDIF
099790100128     c  n06              eval      ITA40ksc = v1cksc
099800100128     c   06              eval      ITA40ksc = ta60cli
099810100128     c                   eval      ITA40scf = clpscf
099820100128     c                   eval      ITA40rag = v2crag
099830100128     c                   call      'TNTA40R'
099840100128     c                   parm                    kpjba
099850100128     c                   parm                    tnta40ds
099860100128     c                   IF        OTA40err <> *blanks
099870100128     c                   eval      v4cmsg = OTA40msg
099880100128     c                   eval      *in28 = *on
099890100312     c                   leavesr
099900100128     c                   ENDIF
099910100312     c                   IF        OTA40scf = 0
099920100312     c                   Eval      v4cmsg = msg(33)
099930100312     c                   eval      *in28 = *on
099940100312     c                   leavesr
099950100312     c                   ENDIF
099960100128     c                   eval      v4cscf = %editc(OTA40scf:'X')
099970100128     c                   eval      wtnta40 = *on
099980100201     c                   clear                   v4dscf
099990100201     c                   Clear                   Tibs69ds
100000100201     c                   Move      v4cscf        I69kac
100010100201     c                   Call      'TIBS69R'
100020100201     c                   Parm                    Tibs69ds
100030100201     c                   Parm                    ds_cnaco
100040100201     c                   Parm                    ds_cnind
100050100201     c                   Parm                    ds_cnclp
100060100201     c                   Parm                    ds_fncls
100070100201     c                   Eval      wtibs69 = *On
100080100201     c                   Movel     w_acorag      v4dscf
100090100128
100100100128     c                   endsr
100110051216
100120080222      *-------------------------------------------------------------------------
100130080222      * Reperisco la nazione del sottoconto intestazione fattura
100140080222      *-------------------------------------------------------------------------
100150080222     c     Sr_Repscfsta  BegSr
100160080222
100170080222     c                   clear                   w_scfsta
100180080222      * Controllo
100190100127     c                   if        v4cscf <> %editc(v1cksc:'X')
100200110427     c                             and v4cscf > *zeros
100210080222     c                   Clear                   Tibs69ds
100220080222     c                   Move      v4cscf        I69kin
100230080222     c                   Call      'TIBS69R'
100240080222     c                   Parm                    Tibs69ds
100250080222     c                   Parm                    ds_cnaco
100260080222     c                   Parm                    ds_cnind
100270080222     c                   Parm                    ds_cnclp
100280080222     c                   Parm                    ds_fncls
100290080222     c                   Eval      wtibs69 = *On
100300080307     c                   if        o69err=*blanks and w_indflg=*blanks
100310080222     c                   eval      w_scfsta=w_indsta
100320080222     c                   endif
100330080227     c                   else
100340080227     c                   eval      w_scfsta=v2csta
100350080227     c                   endif
100360080222
100370080222     c                   EndSr
100380080306      *------------------------------------------------------------------------*
100390080306      * Controllo presenza codice su altro potenziale come cod.fiscale
100400080306      *------------------------------------------------------------------------*
100410080306     c     Sr_CdfAltroP  BegSr
100420080306     c     codice        setll     tncpo05l
100430080306     c     codice        reade     tncpo05l
100440080306    3c                   dow       not %eof(tncpo05l)
100450080306     c* se trovato un record "filiale memorizzo ragione sociale del potenz.
100460080306     c                   eval      savrag=c_cporag
100470080306     c     codice        reade     tncpo05l
100480080306    3c                   enddo
100490080306    3c                   if        not %eof (tncpo05l)
100500080306     c                   eval      savrag=c_cporag
100510080306    3c                   endif
100520080306     c                   endsr
100530080306      *------------------------------------------------------------------------*
100540080306      * Controllo presenza codice su altro potenziale come piva
100550080306      *------------------------------------------------------------------------*
100560080306     c     Sr_PivAltroP  BegSr
100570080306     c     codice        setll     tncpo06l
100580080306     c     codice        reade     tncpo06l
100590080306    3c                   dow       not %eof(tncpo06l)
100600080306     c* se trovato un record "filiale memorizzo ragione sociale del potenz.
100610080306     c                   eval      savrag=c_cporag
100620080306     c     codice        reade     tncpo06l
100630080306    3c                   enddo
100640080306    3c                   if        not %eof (tncpo06l)
100650080306     c                   eval      savrag=c_cporag
100660080306    3c                   endif
100670080306     c                   endsr
100680080306      *------------------------------------------------------------------------*
100690080306      * Messaggio di errore codice fiscal già presente per altro potenz
100700080306      *------------------------------------------------------------------------*
100710080306     c     Sr_msgcdf     BegSr
100720080306     c* Errore forzabile se trovato un altro potenziale con lo stesso
100730080306     c* codice fiscale
100740080306    3c                   if        savrag<>*blanks
100750080306     c                   eval      cod_forzcdf=codice
100760080306     c                   seton                                        28
100770080306     c                   eval      h2riga = 12
100780080306     c                   eval      h2colo = 28
100790080306     c                   Eval      v2cmsg = msg(77)
100800080306     c                   Leavesr
100810080306    3c                   endif
100820080306     c                   endsr
100830080306      *------------------------------------------------------------------------*
100840080306      * Messaggio di errore partita iva   già presente per altro potenz
100850080306      *------------------------------------------------------------------------*
100860080306     c     Sr_msgiva     BegSr
100870080306
100880080306    3c                   if        savrag<>*blanks
100890080306     c                   eval      cod_forziva=v2civa
100900080306     c                   seton                                        28
100910080306     c                   Eval      h2riga = 12
100920080306     c                   Eval      h2colo = 52
100930080306     c                   Eval      v2cmsg = msg(78)
100940080306     c                   Leavesr
100950080306    3c                   endif
100960080306     c                   endsr
100970080422
100980080422      *------------------------------------------------------------------------*
100990080422      * Emissione Window per inviare mail alla sede
101000080422      *------------------------------------------------------------------------*
101010080422     c     sr_winmail    begsr
101020080422
101030100729     c                   eval      w04uffsede = wuffsede
101040080422Do  2c                   do        *hival
101050080422     c                   exfmt     ta60w04
101060080422If  3c                   if        *inkl
101070080422     c                   leavesr
101080080422    3c                   endif
101090080422    2c                   enddo
101100080422
101110080422     c                   endsr
101120100729      /free
101130100729       //--------------------------------------------------------------
101140100729       //?Descrizione banca.
101150100729       //--------------------------------------------------------------
101160100729       BEGSR DesBanca;
101170100729
101180100729         clear wDesBanca;
101190100729         chain (kabi:kcab) CNABI00F;
101200100729         IF  %Found(CNABI00F) and ABIann <> '*';
101210100729           exsr sr_crebna;
101220100729           wDesbanca = wbanca + wagenzia;
101230100729         ENDIF;
101240100729
101250100729       ENDSR;
101260150427
101270150427       //--------------------------------------------------------------
101280150427       //?Memorizza immagine precedente.
101290150427       //--------------------------------------------------------------
101300150427       BEGSR ImmaginePrima;
101310150427
101320150427         clear TIBS73DS;
101330150427         IBS73immag ='P';
101340150427         tibs73r (TIBS73DS:cnaco_73:cnind_73:cnclp_73:fncls_73);
101350150427         wtibs73 = *on;
101360150427
101370150427       ENDSR;
101380150427
101390150427       //--------------------------------------------------------------
101400150427       //?Memorizza immagine sucessiva.
101410150427       //--------------------------------------------------------------
101420150427       BEGSR ImmagineDopo;
101430150427
101440150427         clear TIBS73DS;
101450150427         IBS73pru = knmus;
101460150427         IBS73noj = knmeb;
101470150427         ibs73pgm = 'TNTA60R';
101480150427         IBS73immag ='D';
101490150427         IF  *in02;
101500150427           IBS73cta = 'M';
101510150427         ENDIF;
101520150427         IF  *in01;
101530150427           IBS73cta = 'I';
101540150427         ENDIF;
101550150427         tibs73r (TIBS73DS:cnaco_73:cnind_73:cnclp_73:fncls_73);
101560150427         wtibs73 = *on;
101570150427
101580150427       ENDSR;
101590160803
101600160803       //--------------------------------------------------------------
101610160803       //?Scrivo l'immagine Precedente.
101620160803       //--------------------------------------------------------------
101630160803       BEGSR ImmaginePrimaCpo;
101640160803
101650160803         clear TRMK30DS;
101660160803         IMK30immag = 'P';
101670160803         IMK30tvp = 'B';
101680160803         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
101690160803
101700160803       ENDSR;
101710160803
101720160803       //--------------------------------------------------------------
101730160803       //?Scrivo Storico Variazioni.
101740160803       //--------------------------------------------------------------
101750160803       BEGSR ScriviVarCpo;
101760160803
101770160803         clear TRMK30DS;
101780160803         IMK30pru = knmus;
101790160803         IMK30noj = knmeb;
101800160803         IMK30pgm = 'TNTA60R';
101810160803         IMK30immag = 'D';
101820160803         IMK30cta = 'M';
101830160803         IMK30tvp = 'B';
101840160803
101850160803         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
101860160803
101870160803       ENDSR;
101880150427
101890100729      /end-free
101900080222
101910051107      *------------------------------------------------------------------------*
101920051107      * ROUTINE INIZIALE
101930051107      *------------------------------------------------------------------------*
101940051107     c     *Inzsr        BegSr
101950051107
101960051107     c     *Entry        Plist
101970051107     c                   Parm                    Kpjba
101980051115     c                   Parm                    Tnta60ds
101990110407     c                   Parm                    ParmCbl
102000091104
102010091104     c                   eval      $interroga = *off
102020091124     c                   eval      $modifica  = *off
102030091106     c                   eval      $pgmrich   = *off
102040110407     c                   eval      $Blocco    = *off
102041171006       savin05 = *off;
102050051115
102060091104      * PGM Richiamato
102070051115     c                   If        %Parms > 1
102080091106     c                   eval      $pgmrich = *on
102090100806      * trattativa
102100100806     c                   if        ta60flg = 'T'
102110090914     c                   Eval      *In06 = *on
102120091112     c                   eval      *in70 = (ta60cli > 0)
102130091112     c   70              eval      v1cksc = ta60cli
102140091104      * cliente per aggancio a cnaco
102150090914     c                   else
102160090914     c                   eval      *in06 = *off
102170091112     c                   eval      *in70 = *off
102180090914     c                   endif
102190091104      * convalida offerta
102200091104      * sono in manutenzione perchè il cliente viene scritto dal chiamante
102210051115     c                   Eval      *In07 = (ta60flg = 'C')
102220091104      * codice visita/trattativa da agganciare
102230051222     c   06              Eval      v1cnrv = ta60nrv
102240091104      * codice cliente nuovo da agganciare
102250051222     c   07              Eval      v1cksc = ta60nrv
102260091104
102270091104      * inibisco il richiamo all'interrogazione potenziali per errore
102280091104      * chiamata ricorsiva se provengo da gestione potenziali
102290091104     c                   eval      *in65 = (ta60pot = '1')
102300091104      * interrogazione se richiamato da interrogazione tariffe
102310060207     c                   Eval      *In05 = (ta60flg = 'I')
102311171006       savin05 = *in05;
102320091104
102330091104      * richiamato per visualizzazione (nuovo campo della DS)
102340091104     c                   if        ta60int = 'S'
102350091104     c                   eval      $interroga = *on
102360091104      * devo avere il codice cliente/visita/trattativa
102370091104      * visto che non passo dalla prima videata dove viene richiesto il codice
102380091104      * per comodità ricevo tutti e 3 i codici nello stesso campo
102390091104      * tanto o sono da visita o sono da trattativa o sono da clienti
102400091104      * se il codice è a 0 torno l'errore al chiamante
102410091104     c                   if        ta60nrv = 0
102420091104     c                   eval      ta60msg = 'Codice non passato'
102430091104     c                   eval      ta60err = '1'
102440091124     c                   Eval      *In28 = *On
102450091104     c                   leavesr
102460091104     c                   endif
102470091111      * accendo l'indicatore di interrogazione
102480091111     c                   eval      *in05 = *on
102481171006       savin05 = *in05;
102490091111      * se flag TA60FLG = 'I'
102500091111      * imposto il codice cliente, in quanto è l'unico caso non
102510091104      * previsto nelle specifiche precedenti, visita e trattativa
102520091104      * sono già impostate se TA60FLG = 'T' o 'V'
102530091106     c                   if        ta60flg = 'I'
102540091111     c                   eval      v1cksc = ta60nrv
102550091104     c                   endif
102560091104     c                   endif
102570091124      * in caso di pgm richiamato in manutenzione
102580091124     c                   if        ta60flg = 'M'
102590091124     c                   if        ta60cli = 0
102600091124     c                   eval      ta60msg = 'Codice non passato'
102610091124     c                   eval      ta60err = '1'
102620091124     c                   Eval      *In28 = *On
102630091124     c                   leavesr
102640091124     c                   endif
102650091124     c                   eval      $modifica = *on
102660091124     c                   eval      v1cksc = ta60cli
102670091124     c                   endif
102680110407
102690110407      * PGM Richiamato per blocco cliente
102700110407     c                   IF        %Parms > 2
102710110407     c                   IF        Parmcbl = *blanks or Parmcbl = '000'
102720110407     c                   eval      ta60msg = 'Causale blocco errata'
102730110407     c                   eval      ta60err = '1'
102740110407     c                   Eval      *In28 = *On
102750110407     c                   leavesr
102760110407     c                   ENDIF
102770110407     c                   eval      $Blocco = *on
102780110407     c                   ENDIF
102790091104
102800091104      * NON richiamato
102810051115     c                   Else
102820091104      * Interrogazione
102830051115     c                   If        %subst(kpjbu:256:1) = 'V'
102840051115     c                   Eval      *In05 = *On
102841171006       savin05 = *in05;
102850051115     c                   EndIf
102860051222     c                   Clear                   v1cnrv
102870060308     c                   eval      *in65 = *off
102880051115     c                   EndIf
102890101130
102900101130      * Se pgm in interrogazione imposto subito il titolo 'VISUALIZZA'
102910101130     c   05              Eval      vtcmod = mod(03)
102920051107
102930051107     c     *dtaara       define    §azute        azuteds
102940051107     c     *dtaara       define    §datiute      ddatiute
102950051107     c                   in(E)     *dtaara
102960051117     c                   If        %error  or rsut = *blanks
102970051107     c                   Clear                   Tibs34ds
102980051107     c                   Call      'TIBS34R'
102990051107     c                   Parm                    Tibs34ds
103000051107     c                   In        *dtaara
103010051107     c                   EndIf
103020051107
103030051107     c                   Clear                   wabi
103040051107     c                   Clear                   dLat
103050051107
103060051107      * Verifica errori e autorità profilo
103070051107s   1c                   Select
103080051107      * se ho errori nei dati utente esco dal pgm
103090051117w   1c                   When      duterr = 'E'
103100051115     c                   Eval      *In08 = *On
103110051115     c                   Eval      *In28 = *On
103120051115     c                   Eval      v1cmsg = msg(01)
103130051115      * se richiamato passo il messaggio
103140051115     c                   If        *In06
103150051115     c                   Eval      ta60msg = v1cmsg
103160051115     c                   Eval      ta60err = '1'
103170051115     c                   EndIf
103180051115     c                   Leavesr
103190051107      * se non c'è l'abilitazione
103200051107      * --> se 1° livello, abilitazioni al terminal
103210051107      *     se 2° livello, abilitazioni al punto operativo
103220051107      *     se sede è impossibile ma se capita mando a fine il pgm
103230051117w   1c                   When      uteaut = *Blanks
103240051117if  2c                   If        dutlpo = '1'
103250051107     c                   Eval      wabi   = 'TP'
103260051107e   2c                   EndIf
103270051117if  2c                   If        dutlpo = '2'
103280051107     c                   Eval      wabi   = 'PO'
103290051107e   2c                   EndIf
103300051117if  2c                   If        dutlpo = 'S'
103310051115     c                   Eval      *In08 = *On
103320051115     c                   Eval      *In28 = *On
103330051115     c                   Eval      v1cmsg = msg(01)
103340051115      * se richiamato passo il messaggio
103350051115     c                   If        *In06
103360051115     c                   Eval      ta60msg = v1cmsg
103370051115     c                   Eval      ta60err = '1'
103380051115     c                   EndIf
103390051115     c                   Leavesr
103400051107e   2c                   EndIf
103410051107      * carica le abilitazioni del profilo
103420051107x   1c                   Other
103430051117     c                   Movel     utefaf        dute01
103440051117if  2c                   If        §utecli <> *Blanks
103450051117     c                   Eval      wabi = §utecli
103460051107   x2c                   Else
103470051117     c                   Eval      wabi = uteaut
103480051107e   2c                   EndIf
103490051107e   1c                   EndSl
103500051107
103510051107      * controllo se ok l'abilitazione dell'utente
103520051107     c                   Clear                   Tibs02ds
103530051116     c                   Eval      t02mod = 'C'
103540051116     c                   Eval      t02sif = knsif
103550051116     c                   Eval      t02cod = 'LAT'
103560051116     c                   Movel(p)  wabi          t02ke1
103570051107     c                   Call      'TIBS02R'
103580051107     c                   Parm                    kpjba
103590051107     c                   Parm                    Tibs02ds
103600051117     c                   If        t02err = *Blanks
103610051116     c                   Eval      dLat = t02uni
103620051117     c                   EndIf
103630051107      * errore o non abilitato
103640051117     c                   If        t02err <> *Blanks or §latabi = 'S'
103650051107     c                   Eval      *In08 = *On
103660051107     c                   Eval      *In28 = *On
103670051108     c                   Eval      v1cmsg = msg(01)
103680051115      * se richiamato passo il messaggio
103690051115     c                   If        *In06
103700051115     c                   Eval      ta60msg = v1cmsg
103710051115     c                   Eval      ta60err = '1'
103720051115     c                   EndIf
103730051115     c                   Leavesr
103740051117     c                   EndIf
103750051107
103760170914      * Reperimento delle Filiali da proporre a video per la Ricerca
103770170914     c                   clear                   TRUL31ds
103780170914     c*//*no:*           if        *in05  = *on   and
103790170914     c*// no:                      wAbi  <> 'DI'  and
103800170914     c*// no:                      wAbi  <> 'AZ'
103810170914if  1c                   if        ( wAbi = 'TP'  or
103820170914     c                               wAbi = 'PO' )
103830170914     c                   eval      i31abi = 'RA'
103840170915x   1c                   else
103850170915     c                   eval      i31abi = wabi
103860170915e   1c                   endif
103870170914     c                   eval      i31cdi = DUTdis
103880170914     c                   eval      i31car = DUTare
103890170914     c                   eval      i31cpo = DUTpou
103900170914     c                   call      'TRUL31R'
103910170914     c                   parm                    kpjba
103920170914     c                   parm                    TRUL31ds
103930170914if  2c                   if        o31pog > *zeros
103940170914     c                   movea     o31pog        skPog_RA
103950170914x   2c                   else
103960170914     c                   eval      *in08 = *On
103970170914     c                   eval      *in28 = *On
103980170914     c                   eval      V1Cmsg = Msg(01)
103990170914      * se richiamato passo il messaggio
104000170914if  3c                   if        *in06
104010170914     c                   eval      TA60msg = V1Cmsg
104020170914     c                   eval      TA60err = '1'
104030170914e   3c                   endif
104040170914     c                   leavesr
104050170914e   2c                   endif
104060170915e   1c*//*               endif
104070170914
104080051117      * Reperimento dei P.O. gestibili dall'utente per i codici clienti
104090051107     c                   Clear                   Trul31ds
104100051117     c                   Eval      i31abi = wabi
104110051117     c                   Eval      i31cdi = dutdis
104120051117     c                   Eval      i31car = dutare
104130051117     c                   Eval      i31cpo = dutpou
104140051107     c                   Call      'TRUL31R'
104150051117     c                   Parm                    kpjba
104160051107     c                   Parm                    Trul31ds
104170051117     c                   If        o31pog > *zeros
104180051117     c                   Movea     o31pog        skpog
104190051117     c                   Else
104200051107     c                   Eval      *In08 = *On
104210051107     c                   Eval      *In28 = *On
104220051107     c                   Eval      v1cmsg = msg(01)
104230051115      * se richiamato passo il messaggio
104240051115     c                   If        *In06
104250051115     c                   Eval      ta60msg = v1cmsg
104260051115     c                   Eval      ta60err = '1'
104270051115     c                   EndIf
104280051115     c                   Leavesr
104290051117     c                   EndIf
104300160905      /free
104310160905       //?Se utente abilitato alla gestione clienti logistica aggiungo
104320160905       //?alla sk SKPOG le filiali con NTW LOG
104330160905         IF  §UTEKscLog = 'S';
104340160905           exsr CaricaFilLog;
104350160905         ENDIF;
104360160905      /end-free
104370051115
104380051117      * Reperimento dei P.O. gestibili dall'utente per i codici potenziali
104390051117     c                   If        §utepot <> *Blanks
104400051117     c                   Eval      wabi = §utepot
104410051117     c                   EndIf
104420051117     c                   Clear                   Trul31ds
104430051117     c                   Eval      i31abi = wabi
104440051117     c                   Eval      i31cdi = dutdis
104450051117     c                   Eval      i31car = dutare
104460051117     c                   Eval      i31cpo = dutpou
104470051117     c                   Call      'TRUL31R'
104480051117     c                   Parm                    kpjba
104490051117     c                   Parm                    Trul31ds
104500051117     c                   If        O31pog > *zeros
104510051117     c                   Movea     O31pog        skpogcpo
104520051117     c                   EndIf
104530051117
104540051115      * Controllo se sono in sede
104550051115     c                   Eval      *In09 = (simfel = *zeros)
104560051128
104570051128      * Controllo se utente EDP
104580051129     c                   Eval      *In20 = (%subst(knmus:1:3) = 'EDP')
104590051116
104600060110      * Se sono in filiale
104610060110if  1c                   If        Not *In09
104620060110      * Verifico se il P.O. utente può manutenzionare l'anagrafico clienti
104630051116      * deve esistere la tabella MTC per il p.o. utente
104640051116     c                   Clear                   Tibs02ds
104650051116     c                   Clear                   dmtc
104660051116     c                   Eval      t02mod = 'C'
104670051116     c                   Eval      t02sif = knsif
104680051116     c                   Eval      t02cod = 'MTC'
104690051116     c                   Movel(p)  dutpou        t02ke1
104700051116     c                   Call      'TIBS02R'
104710051116     c                   Parm                    kpjba
104720051116     c                   Parm                    Tibs02ds
104730060110if  2c                   If        t02err <> *Blanks
104740051116     c                   Eval      *In28 = *On
104750051116     c                   Eval      v1cmsg = msg(01)
104760051116      * se richiamato passo il messaggio
104770051116     c                   If        *In06
104780051116     c                   Eval      ta60msg = v1cmsg
104790051116     c                   Eval      ta60err = '1'
104800051116     c                   EndIf
104810051116     c                   Leavesr
104820060110   x2c                   Else
104830051125     c                   Eval      dmtc = t02uni
104840060110    2c                   EndIf
104850060110
104860060110      * Se sono in sede deve esserci la tabella MTC key sede
104870060110   x1c                   Else
104880060110     c                   Clear                   Tibs02ds
104890060110     c                   Clear                   dmtc
104900060110     c                   Eval      t02mod = 'C'
104910060110     c                   Eval      t02sif = knsif
104920060110     c                   Eval      t02cod = 'MTC'
104930060110     c                   Movel(p)  'SEDE'        t02ke1
104940060110     c                   Call      'TIBS02R'
104950060110     c                   Parm                    kpjba
104960060110     c                   Parm                    Tibs02ds
104970060110if  2c                   If        t02err <> *Blanks
104980060110     c                   Eval      *In28 = *On
104990060110     c                   Eval      v1cmsg = msg(01)
105000060110      * se richiamato passo il messaggio
105010091106     c                   If        *In06
105020060110     c                   Eval      ta60msg = v1cmsg
105030060110     c                   Eval      ta60err = '1'
105040060110     c                   EndIf
105050060110     c                   Leavesr
105060060110   x2c                   Else
105070060110     c                   Eval      dmtc = t02uni
105080060110    2c                   EndIf
105090060110    1c                   EndIf
105100051115
105110051117      * Apro i file delle anagrafiche provvisorie se richiamato da gest.visite
105120051115     c                   If        *In06
105130051115     c                   Open      Tfaco00f
105140051115     c                   Open      Tfind00f
105150051115     c                   Open      Tfclp00f
105160051117     c                   Open      Tfcls01l
105170080306     c                   Open      Tncpo05l
105180080306     c                   Open      Tncpo06l
105190051115     c                   EndIf
105200051117
105210051117      * Apro i file delle anagrafiche effettive se non richiamato da gest.visite
105220051117     c                   If        Not *In06
105230051117     c                   Open      Cnind00f
105240060105     c                   Open      Cnind02l
105250051117     c                   Open      Cnclp00f
105260051220     c                   Open      Fnacr01l
105270051117     c                   Open      Fncls01l
105280051117     c                   Open      Fnspe01l
105290051219     c                   Open      Fnsp201l
105300060526     c                   EndIf
105310091112
105320091112      * Apro i file dei luoghi se trattativa su cliente
105330091112     c                   If        *In70
105340091112     c                   Open      Fnspe01l
105350091112     c                   Open      Fnsp201l
105360091112     c                   EndIf
105370051115
105380051222      * Passo i dati del capoconto se sono richiamato da visite/offerte
105390091106      * visto che salto la prima videata di richiesta codice
105400091124     c                   If        $pgmrich
105410051115     c                   Eval      v1ckcc = dutkci
105420051115     c                   EndIf
105430051107
105440051107      * Carico £4 capoconti gestiti solo in filiale e non in sede
105450051107     c                   Clear                   £4
105460051107     c                   Eval      kcod = '£4'
105470051108     c                   Eval      kkey = '       1'
105480051108     c     kTabel        Chain     Tabel00f
105490051107     c                   If        %Found(Tabel00f) and
105500051125     c                             tblflg = *Blanks
105510051125     c                   Movea     tbluni        £4
105520051107     c                   EndIf
105530051117
105540051117      * Recupero la divisa da tabella 'GED'
105550051117     c                   Clear                   Tibs02ds
105560051117     c                   Clear                   dged
105570051117     c                   Eval      t02mod = 'C'
105580051117     c                   Eval      t02sif = knsif
105590051117     c                   Eval      t02cod = 'GED'
105600051117     c                   Eval      t02ke1 = '1'
105610051117     c                   Call      'TIBS02R'
105620051117     c                   Parm                    kpjba
105630051117     c                   Parm                    Tibs02ds
105640051117     c                   If        t02err = *Blanks
105650051125     c                   Eval      dged = t02uni
105660051117     c                   EndIf
105670051117
105680051117      * Recupero il diritto di fatturazione di cartello
105690051117     c                   Clear                   Tibs02ds
105700051117     c                   Clear                   dgei
105710051117     c                   Eval      t02mod = 'C'
105720051117     c                   Eval      t02sif = knsif
105730051117     c                   Eval      t02cod = 'GEI'
105740051117     c                   Eval      t02ke1 = §gedcn
105750051117     c                   Call      'TIBS02R'
105760051117     c                   Parm                    kpjba
105770051117     c                   Parm                    Tibs02ds
105780051117     c                   If        t02err = *Blanks
105790051125     c                   Eval      dgei = t02uni
105800051117     c                   EndIf
105810051221
105820051221      * Recupero i dft tipo comunicazioni giacenze
105830051221     c                   Clear                   ds2g
105840051221     c                   Eval      kcod = '2G'
105850051221     c                   Eval      kkey = '1'
105860051221     c     kTabel        Chain     Tabel00f
105870051221     c                   If        %Found(Tabel00f) and
105880051221     c                             tblflg = *Blanks
105890051221     c                   Eval      ds2g = tbluni
105900051221     c                   EndIf
105910051219
105920051219      * Aggancio il capoconto su Cnaco per recuperare acoopz e acotic
105930051219      * uguali per tutti i clienti
105940051219     c     kCnaco1       Chain(n)  Cnaco00f
105950051219     c                   If        %Found(Cnaco00f)
105960051219     c                   Eval      wopz = acoopz
105970051219     c                   Eval      wtic = acotic
105980051219     c                   Else
105990051219     c                   Eval      wopz = '1100100001'
106000051219     c                   Eval      wtic = 'CG'
106010051219     c                   EndIf
106020051219
106030051129      * Imposto la data odierna
106040051129     c                   Z-add     *date         dataoggi
106050051129      * Imposto la data odierna -1 anno
106060051129     c                   Move      dataoggi      dataiso
106070051129     c                   Subdur    1:*y          dataiso
106080051129     c                   Move      dataiso       dataoggi1
106090100805
106100150424       //?cerco il tipo pag. di default da impostare in immissione
106110100805         exec sql
106120100805         declare PAG cursor for
106130100805         select TBEke1, TBEke2 from TNTBE00F
106140100805         where  TBEcod = 'PAG' and substr(TBEuni, 37, 2) = 'SI';
106150100805
106160100805         exec sql
106170100805           open PAG;
106180100805           IF sqlcode < 0;
106190100805             $End = *on;
106200100805           ENDIF;
106210100805
106220100805         DOW not $End;
106230100805           exec sql
106240100805             fetch next from PAG into :TBEke1, :TBEke2;
106250100805             IF sqlcod = 100 or sqlcod < 0;
106260100805               $End = *on;
106270100805               leave;
106280100805             ENDIF;
106290150424             IF  TBEke1 = 'DN';
106300100805               dfttipdn = TBEke2;
106310100805             ENDIF;
106320150424             IF  TBEke1 = 'NA';
106330100805               dfttipna = TBEke2;
106340100805             ENDIF;
106350100805         ENDDO;
106360100805
106370100805         exec sql close PAG;
106380170421
106390170421       //?carico in sk i clienti IMPORT DPD
106400170421         $End = *off;
106410170421         clear ww;
106420170421         exec sql
106430170421         declare TabLPD cursor for
106440170421         select TBEuni from TNTBE00F
106450170421         where TBEcod = 'LPD';
106460170421         exec sql open TabLPD;
106470170421         IF  sqlcode < 0;
106480170421           $End = *on;
106490170421         ENDIF;
106500170421         DOW  not $End;
106510170421           exec sql fetch next from TABLPD into :TBEuni;
106520170421           IF  sqlcod = 100 or sqlcod < 0;
106530170421             $End = *on;
106540170421             leave;
106550170421           ENDIF;
106560170421           dLPD = TBEUni;
106570170421           IF  §LPDksc = 0;
106580170421             iter;
106590170421           ENDIF;
106600170421           ww += 1;
106610170421           CliImport(ww) = §LPDksc;
106620170421         ENDDO;
106630170421         exec sql close TabLPD;
106640100805
106650100805      /end-free
106660051107
106670051107      * KLIST
106680051129     c     kCnabi        Klist
106690051129     c                   Kfld                    kabi
106700051129     c                   Kfld                    kcab
106710051129
106720051108     c     kCnaco        Klist
106730051108     c                   Kfld                    codut
106740051107     c                   Kfld                    v1ckcc
106750051222     c                   Kfld                    kksc
106760051116
106770051116     c     kCnaco1       Klist
106780051116     c                   Kfld                    codut
106790051116     c                   Kfld                    v1ckcc
106800051116     c                   Kfld                    wksc
106810060105
106820060105     c     kCnind        Klist
106830060105     c                   Kfld                    codut
106840060105     c                   Kfld                    v1ckcc
106850060109     c                   Kfld                    kindiva
106860051108
106870051108     c     kFnspe        Klist
106880051216     c                   Kfld                    kspefls
106890051216     c                   Kfld                    kspecli
106900051216     c                   Kfld                    kspecod
106910051219
106920051219     c     kFnsp2        Klist
106930051219     c                   Kfld                    kspecli
106940051219     c                   Kfld                    kspecod
106950051219     c                   Kfld                    ksp2tpe
106960051129
106970051129     c     kTabel        Klist
106980051129     c                   Kfld                    codut
106990051129     c                   Kfld                    kcod
107000051129     c                   Kfld                    kkey
107010051108
107020051129     c     kTfntc        klist
107030051216     c                   kfld                    kntcapl
107040051216     c                   kfld                    kntcnk1
107050051216     c                   kfld                    kntcnk2
107060051216     c                   kfld                    kntctnt
107070051107
107080051107     c                   EndSr
107090160905      /free
107100160905       //--------------------------------------------------------------
107110160905       //?Carico le filiali Logistica nella SKPOG.
107120160905       //--------------------------------------------------------------
107130160905       BEGSR CaricaFilLog;
107140160905
107150160905         $End = *off;
107160160905
107170160905         exec sql
107180160905         declare WLOG cursor for
107190160905         select ORGfil from AZORGLOG;
107200160905
107210160905         exec sql
107220160905         open WLOG;
107230160905         IF  sqlcode < 0;
107240160905           $End = *on;
107250160905         ENDIF;
107260160905
107270160905         DOW not $End;
107280160905           exec sql
107290160905           fetch next from WLOG into :ORGfil;
107300160905           IF  sqlcod = 100 or sqlcod < 0;
107310160905             $End = *on;
107320160905             leave;
107330160905           ENDIF;
107340160905
107350160905           IF  %lookup(%editc(ORGfil:'X'):SKpog) = 0;
107360160905             ww = %lookup(*zeros:SKpog);
107370170925             IF  ww > 0 and ww < 250;
107380160905               SKpog(ww) = %editc(ORGfil:'X');
107390160905             ENDIF;
107400160905           ENDIF;
107410170914
107420170914           If  %lookup( %editc(ORGfil:'X') : SKpog_RA ) = *zero;
107430170914             ww = %lookup( *zeros : SKpog_RA );
107440170925             if  ww > *zero  and  ww < %elem(skPog_RA);
107450170914               SKpog_RA(ww) = %editc(ORGfil:'X');
107460170914             endif;
107470170914           EndIf;
107480160905
107490160905         ENDDO;
107500160905
107510160905         exec sql close WLOG;
107520160905
107530160905       ENDSR;
107540160905      /end-free
107550051107      *------------------------------------------------------------------------*
107560051212** CAR  Lungh. 10
107570051212!:§@
107580051107** MOD  Lungh. 15                                                            *
107590051107  IMMISSIONE                                                                  01
107600051107 MANUTENZIONE                                                                 02
107610051107VISUALIZZAZIONE                                                               03
107620051107   ANNULLATO                                                                  04
107630051107** MSG  Lungh. 78                                                            *
107640051107Utente non autorizzato alla Gestione anagrafico clienti                       01
107650051107Immettere almeno un dato                                                      02
107660051107Immettere il capoconto                                                        03
107670051107Capoconto non gestibile                                                       04
107680051115Cliente inesistente                                                           05
107690051116Password errata                                                               06
107700051116Immettere la password                                                         07
107710051117Password scaduta                                                              08
107720051117Manca tabella di controllo scadenza password. TEL CED SEDE !!!!!              09
107730051117Cliente non gestibile dall'utente                                             10
107740051117Immettere la ragione sociale                                                  11
107750051118Immettere il codice potenziale                                                12
107760051118Codice potenziale errato                                                      13
107770051118Cliente potenziale non gestibile dall'utente                                  14
107780051118Stato che non prevede il cappario: utilizzabile solo per clienti bloccati     15
107790051118Impossibile utilizzare cap o localita' obsoleti                               16
107800051118Stato errato                                                                  17
107810051118Immettere la partita IVA                                                      18
107820051118Codice fiscale formalmente errato                                             19
107830051122Codice commerciale errato                                                     20
107840090617Fil. del codice commerciale incongruente con la fil. del cliente              21
107850051122Il cliente è da bloccare perchè inattivo                                      22
107860051122Stato del credito errato                                                      23
107870090617Immettere stato del credito gest. da fil. e senza passaggio al contenzioso    24
107880051122Causale blocco cliente errata                                                 25
107890051122Immettere la causale blocco perchè il cliente è bloccato                      26
107900051122Per immettere la causale blocco occorre bloccare il cliente                   27
107910051128Immettere il codice importanza cliente                                        28
107920051128Codice importanza cliente errata                                              29
107930080313Immettere la categoria Merceologica                                           30
107940080313Categoria Merceologica errata                                                 31
107950051212La ragione sociale contiene uno dei seguenti caratteri non validi -- ! : § @  32
107960051128Codice Sottoconto fattura non impostato                                       33
107970051128Codice Sottoconto fattura non utilizzabile                                    34
107980051128Codice Sottoconto fattura errato                                              35
107990051128Codice Sottoconto fattura annullato                                           36
108000051128Tipo fattura errato                                                           37
108010051128Tipo fattura non utilizzabile in                                              38
108020051128Immettere la frequenza fattura                                                39
108030051128Frequenza fattura errato                                                      40
108040051128Per tipo fattura "1" è ammessa solo la frequenza mensile                      41
108050120613Per frequenza settimanale chiedere autorizzazione alla DIR. AMMINISTRATIVA!!  42
108060051128Immettere il tipo data fattura                                                43
108070051128Tipo data fattura errato                                                      44
108080051128Sottoconto fattura diverso da codice cliente è possibile unificare            45
108090051128Verificare il flag Fattura Unificata anche nel sottoconto fattura             46
108100051128Verificare il flag Fattura Unificata anche nei codici collegati               47
108110051128Spese invio fattura errato. Troppi decimali, massimo 2                        48
108120060208Per interrogare le info.commerciali inserire il cod. potenziale               49
108130060203Il luogo non è stato immesso. Impossibile Visualizzare                        50
108140061030Immettere il codice fiscale                                                   51
108150060203La nota non è stata immessa. Impossibile Visualizzare                         52
108160051129Codice pagamento errato                                                       53
108170051129Codice ABI/CAB obbligatorio se pagamento con RB                               54
108180051129Codice ABI/CAB obbligatorio se immessa descrizione banca o nr. C/C            55
108190051130Codice ABI/CAB non valido                                                     56
108200100729Tipo pagamento errato                                                         57
108210100729Tipo pagamento non utilizzabile per clienti vari                              58
108220051130Coordinate bancarie errate. Correggere i dati immessi o contattare il cliente 59
108230051130Per l'estero l'ABI deve essere 99999 e il CAB deve essere 99999               60
108240051206Immettere i codici ABI/CAB                                                    61
108250051212Tipo Comunicazione al Mittente errato                                         62
108260051212Tipo invio comunicazione non utilizzabile                                     63
108270051212Apertura automatica giacenza errato                                           64
108280051212Apertura automatica giacenza non possibile per clienti vari                   65
108290051212Disposizione automatica giacenza non possibile per clienti vari               66
108300051213Immettere un numero stop ritiro minore di 800                                 67
108310051213Il codice richiesto è già in uso da altro utente. Riprovare più tardi         68
108320051216Impossibile confermare. Messaggi in sospeso su altre videate                  69
108330060216Tipo Comunicazione Fine Giacenza errato                                       70
108340051220Impossibile modificare cod.intestazione fattura inserito dalla SEDE           71
108350051220ATTENZIONE!!! Codice già presente in anagrafica clienti ritiro                72
108360170117Impossibile intestare fattura ad un codice in sofferenza.                     73
108370060801Chiusura automatica giacenza non possibile per clienti vari                   74
108380061113Inserire Codice Fiscale solo se diverso dalla Partita IVA. Enter per forzare. 75
108390071128Luogo non utilizzabile                                                        76
108400080306Cod.fiscale già presente su altri potenziali. Enter per forzare               77
108410080306Partita Iva già presente su altri potenziali. Enter per forzare               78
108420080422Non è possibile inserire coordinate bancarie estere se tipo pagamento italia. 79
108430080515Codice pagamento utilizzabile SOLO dalla SEDE                                 80
108440090421Non consentita fattura settimanale se cod.int.fattura con autofattura         81
108450090421No autofattura un codice collegato al cod.int.fattura ha frequenza settimanale82
108460090616Il cliente deve essere incluso nella procedura di sollecito automatico        83
108470090616Il cliente deve essere escluso dalla procedura di sollecito automatico        84
108480100729Codice pagamento NON utilizzabile                                             85
108490100730Per pagamento DANNI con Bonifico immettere la relativa e-mail                 86
108500150427Mancano i codici IBAN per i pagamenti tramite Bonifico. Enter x forzare.      87
108510150805Manca indirizzo mail per inoltro Progetto di Liquidazione. Enter x forzare.   88
108520160517Pagamento con RB non ammesso per cliente estero                               89
108530160517Pagamento con RB non ammesso su conto BancoPosta                              90
108540160914Filiale errata o non in gestione.                                             91
108550060731** TF04
108560060731F4=Abilitazioni  #
108570100524** TF22
108580100524F22=Richiesta Contatto  #
108590100507** TF05
108600100507F5=Attività  #
108610060203** TF12
108620060203F12=Ritorno  #
108630051206** TF14
108640051206F14=InfoCommerciali  #
108650051122** TF15
108660051129F15=Codici Collegati  #
108670060731** TF19
108680060526F19=Variazioni  #
108690100407** TF20
108700100407F20=Potenziali  #
108710101213** TF17
108720101213F17=Dati Fatturazione  #
108730110223** TF21
108740110223F21=Blocco Cliente  #
