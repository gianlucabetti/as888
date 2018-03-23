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
003360051122     d skpogcpo        s              3    dim(250) inz(*Zeros)
003370060731     d tf04            s              1    dim(25) ctdata perrcd(25)
003380100524     d tf22            s              1    dim(25) ctdata perrcd(25)
003390100507     d tf05            s              1    dim(25) ctdata perrcd(25)
003400060203     d tf12            s              1    dim(25) ctdata perrcd(25)
003410051206     d tf14            s              1    dim(25) ctdata perrcd(25)
003420051124     d tf15            s              1    dim(25) ctdata perrcd(25)
003430060526     d tf19            s              1    dim(25) ctdata perrcd(25)
003440100406     d tf20            s              1    dim(25) ctdata perrcd(25)
003450101213     d tf17            s              1    dim(25) ctdata perrcd(25)
003460110223     d tf21            s              1    dim(25) ctdata perrcd(25)
003470051122     d £4              s              4    dim(20)
003480051124     d $tf             s              1    dim(25)
003490051107
003500051107      *   D S   I N T E R N E / E S T E R N E
003510051109
003520051109     d                 ds
003530051109     d aa1                     1      2  0
003540051109     d mm1                     3      4  0
003550051109     d gg1                     5      6  0
003560051109     d dataamg                 1      6  0
003570051109
003580051109     d                 ds
003590051109     d gg2                     1      2  0
003600051109     d mm2                     3      4  0
003610051109     d aa2                     5      6  0
003620051109     d datagma                 1      6  0
003630051129
003640051129     d parmbanca       ds
003650051129     d  pabi                   1      5  0
003660051129     d  pcab                   6     10  0
003670051129     d  pist                  11     50
003680051129     d  ppro                  51     52
003690051129     d  pflg                  53     53
003700051124
003710051124     d parmf11         ds
003720051124     d  paropz                 1      1
003730051124     d  parcli                 2      8
003740060216     d  partnta60              9      9
003750060216     d  parimta60             10     10
003760051124
003770051124     d parmf15         ds
003780051124     d  parcli1                       7  0
003790080116
003800080116     d parmiban        ds
003810080116     d  parmstato              1      2
003820080116     d  parmcheck              3      4
003830080206     d  parmcoori              5     34
003840051107
003850051107     d                 ds
003860051107     d v1cf01                  1      3
003870051107     d v1cf02                  4      6
003880051107     d v1cf03                  7      9
003890051107     d v1cf04                 10     12
003900051107     d v1cf05                 13     15
003910051107     d v1cf06                 16     18
003920051107     d v1cf07                 19     21
003930051107     d v1cf08                 22     24
003940051107     d v1cf09                 25     27
003950051107     d v1cf10                 28     30
003960051107     d v1cf11                 31     33
003970051107     d v1cf12                 34     36
003980051107     d v1cf13                 37     39
003990051107     d v1cf14                 40     42
004000051107     d v1cf15                 43     45
004010051107     d v1cf16                 46     48
004020051107     d v1cf17                 49     51
004030051107     d v1cf18                 52     54
004040051107     d v1cf19                 55     57
004050051107     d v1cf20                 58     60
004060051107     d v1cf21                 61     63
004070051107     d v1cf22                 64     66
004080051107     d v1cf23                 67     69
004090051108     d v1cf24                 70     72
004100051108     d skfil                   1     72    dim(24)
004110051108
004120051108     d                 ds
004130051108     d v1ivaeu                 1      2
004140051108     d v1ivait                 3     16
004150051108     d v1civa                  1     16
004160051109
004170051109     d                 ds
004180051109     d v2ivaeu                 1      2
004190051109     d v2ivait                 3     16
004200051109     d v2civa                  1     16
004210080115
004220080115     d v5ciban         ds
004230080115     d  v5ciban1               1      4
004240080115     d  v5ciban2               5      8
004250080115     d  v5ciban3               9     12
004260080115     d  v5ciban4              13     16
004270080115     d  v5ciban5              17     20
004280080115     d  v5ciban6              21     24
004290080115     d  v5ciban7              25     28
004300080208     d  v5ciban8              29     32
004310100728
004320100728     d v5ibandn        ds
004330100728     d  v5ibandn1              1      4
004340100728     d  v5ibandn2              5      8
004350100728     d  v5ibandn3              9     12
004360100728     d  v5ibandn4             13     16
004370100728     d  v5ibandn5             17     20
004380100728     d  v5ibandn6             21     24
004390100728     d  v5ibandn7             25     28
004400100728     d  v5ibandn8             29     32
004410100728
004420100728     d v5ibanna        ds
004430100728     d  v5ibanna1              1      4
004440100728     d  v5ibanna2              5      8
004450100728     d  v5ibanna3              9     12
004460100728     d  v5ibanna4             13     16
004470100728     d  v5ibanna5             17     20
004480100728     d  v5ibanna6             21     24
004490100728     d  v5ibanna7             25     28
004500100728     d  v5ibanna8             29     32
004510051116
004520051116     d wlbdat          ds
004530051116     d  g02dat                 1      8  0
004540051116     d  g02inv                 9     16  0
004550051116     d  g02err                17     17
004560051116     d  g02tgi                18     22  0
004570051107
004580051107     d                sds
004590051107     d  Vtcpgm                 1     10
004600051107
004610051107     d Azuteds       e ds                  Extname(Azute00f)
004620051107     d dDatiute      e ds
004630100729     d dPag          e ds
004640051117     d dged          e ds
004650051117     d dgei          e ds
004660051116     d dlat          e ds
004670170421     d dLPD          e ds
004680051116     d dmtc          e ds
004690051108     d ds_cnaco      e ds                  inz  extname(Cnaco00f) prefix(w_)
004700051108     d ds_cnind      e ds                  inz  extname(Cnind00f) prefix(w_)
004710051108     d ds_cnclp      e ds                  inz  extname(Cnclp00f) prefix(w_)
004720051108     d ds_fncls      e ds                  inz  extname(Fncls00f) prefix(w_)
004730060531     d cnaco_73      e ds                  inz  extname(Cnaco00f)
004740060531     d cnind_73      e ds                  inz  extname(Cnind00f)
004750060531     d cnclp_73      e ds                  inz  extname(Cnclp00f)
004760060531     d fncls_73      e ds                  inz  extname(Fncls00f)
004770121105
004780121105      // - CPORST file TNCPO
004790121105     d dCPO01        e ds                  inz
004800121105
004810110323     d dsbi          e ds
004820051108     d dsfa          e ds
004830051108     d dsff          e ds
004840051108     d dsic          e ds
004850051108     d ds1l          e ds
004860051108     d ds1t          e ds
004870051125     d ds13          e ds
004880051118     d ds15          e ds
004890051108     d ds2f          e ds
004900051207     d ds2g          e ds
004910051108     d ds2h          e ds
004920051108     d ds2u          e ds
004930051108     d ds4l          e ds
004940051108     d ds4u          e ds
004950051115     d ds4w          e ds
004960090803     d w_ds4w        e ds                  extname(ds4w) prefix(w_)
004970051125     d dtft          e ds
004980051108     d dute01        e ds
004990110217     d dY4O          e ds
005000051118     d Fnlv13ds      e ds
005010051118     d Fnlv14ds      e ds
005020070809     d FIOR37ds      e ds                  inz
005030070809     d  I37cgi       e                     inz(*all'9')
005040051220     d Fior50ds      e ds
005050051108     d kpjba         e ds
005060100406     d OG141         e ds
005070051125     d og143         e ds
005080051220     d og148         e ds
005090051107     d Tibs02ds      e ds
005100051122     d Tibs12ds      e ds
005110051107     d Tibs34ds      e ds
005120051108     d Tibs69ds      e ds
005130060531     d Tibs73ds      e ds
005140051118     d Tisi95ds      e ds
005150051207     d Tnta12ds      e ds
005160100127     d TNTA40DS      e ds
005170100604     d TNTA43DS      e ds
005180051124     d Tnta60ds      e ds
005190060731     d Tnta61ds      e ds
005200051213     d Tnta81ds      e ds
005210100406     d TRMK01DS      e ds
005220110223     d Trmk05ds      e ds
005230100406     d TRMK17DS      e ds
005240100406     d TRMK20ds      e ds
005250100507     d TRMK21DS      e ds
005260130808     d TRMK43ds      e ds                  inz
005270081020     d Trmk50ds      e ds
005280100407     d TRMK28DS      e ds
005290051108     d Trul31ds      e ds
005300051118     d Trul42ds      e ds
005310101213     d Trul57ds      e ds
005320100730     d TrulIbanI0    e ds                  qualified
005330100728     d TrulIbanI1    e ds                  qualified
005340100728     d TrulIbanO0    e ds                  qualified
005350100728     d TrulIbanO1    e ds                  qualified
005360090616     d trulsoli1     e ds                  qualified
005370090616     d trulsolo1     e ds                  qualified
005380060529     d TNTA73DS      e ds
005390061012     d Tisie5ds      e ds
005400051128     d Xdecds        e ds                  inz
005410051128     d   ocdesi      e                     inz(*Off)
005420151014     d xcfiva1ds     e ds
005430061106     d  ivaeu                  3      4
005440061106     d  ivait                  5     18
005450100805     d TNTBE00F      e ds                  extname(TNTBE00F)
005460160803
005470160803       // -?Storicizzazione variazioni
005480160803     d TRMK30DS      e ds
005490160803     d TNCPO_30      e ds                  extname(TNCPO00F) inz
005500160803     d TNCPO1_30     e ds                  extname(TNCPO10F) inz
005510160803     d TICPI_30      e ds                  extname(TICPI00F) inz
005520051107
005530051107      *   C O S T A N T I
005540060203      * titolo videata (lunghezza massima 35)
005550060203     d VtcTit          c                   const('** GESTIONE ANAGRAFICHE CLIEN-
005560060203     d                                     TI ** ')
005570091027     d VtcTitv         c                   const('****  Anagrafica Provvisoria -
005580091027     d                                      **** ')
005590051122     d VtcTitc         c                   const('**      CONVALIDA OFFERTE    -
005600060203     d                                        ** ')
005610051122     d cf2413          c                   const('F24=AlTasti(1/3)')
005620051122     d cf2423          c                   const('F24=AlTasti(2/3)')
005630051122     d cf2433          c                   const('F24=AlTasti(3/3)')
005640051122     d cf2412          c                   const('F24=AlTasti(1/2)')
005650051122     d cf2422          c                   const('F24=AlTasti(2/2)')
005660100127
005670100127     d Digits          c                   const('0123456789')
005680100406
005690100406      //---------------------------------------------------------------
005700100406      //?Definizione procedure esterne.
005710100406      //---------------------------------------------------------------
005720100604      // - Controllo se trattativa aperta
005730100604     d Tnta43R         pr                  extpgm('TNTA43R')
005740100604     d  tnta43ds                           likeds(tnta43ds)
005750110223
005760110223      // - Calcolo categoria potenziale
005770110223     d trmk05r         pr                  extpgm('TRMK05R')
005780110223     d  kpjba                              likeds(KPJBA)
005790110223     d  trmk05ds                           likeds(TRMK05ds)
005800100604
005810100406      // - Inserimento attività
005820100406     d trmk17r         pr                  extpgm('TRMK17R')
005830100406     d  kpjba                              likeds(KPJBA)
005840100406     d  trmk17ds                           likeds(TRMK17ds)
005850100507
005860100507      // - Interrogazione attività
005870100507     d trmk21r         pr                  extpgm('TRMK21R')
005880100507     d  kpjba                              likeds(KPJBA)
005890100407
005900100407      // - Modifica potenziale
005910100407     d trmk28r         pr                  extpgm('TRMK28R')
005920100407     d  trmk28ds                           likeds(TRMK28DS)
005930120510
005940120510      // - Pgm gestione INFO commerciali
005950120510     d trmk50r         pr                  extpgm('TRMK50R')
005960120510     d  kpjba                              likeds(KPJBA)
005970120510     d  trmk50ds                           likeds(TRMK50ds)
005980100728
005990100728      // - Coordinate bancarie
006000100728     d TrulIbanR       pr                  extpgm('TRULIBANR')
006010100729     d  rqsOpCode                    10a   const
006020100729     d  rpyEsito                     10i 0
006030100729     d  rqsFormatName                10a   const
006040100729     d  rqsData                     256a   options(*varsize)
006050100729     d  rqsDataSize                  10i 0 const
006060100729     d  rpyFormatName                10a   const
006070100729     d  rpyData                     256a   options(*varsize)
006080100729     d  rpyDataSize                  10i 0 const
006090100729
006100100729      // - Interrogazione tabella
006110100729     d Trul19R         pr                  extpgm('TRUL19R')
006120100729     d  kcod                          2a
006130100729     d  ordina                        1a
006140100729     d  kkey                          8a
006150100729     d  comando                       1a
006160101213
006170101213      // - Dati Fatturazione
006180101213     d trul57r         pr                  extpgm('TRUL57R')
006190101213     d  kpjba                              likeds(KPJBA)
006200101213     d  trul57ds                           likeds(TRUL57ds)
006210160803
006220160803       // -?Storicizzazione Variazioni Potenziale
006230160803     d TRMK30R         pr                  extpgm('TRMK30R')
006240160803     d  trmk30ds                           likeds(trmk30ds)
006250160803     d  tncpo_30                           likeds(tncpo_30)
006260160803     d  tncpo1_30                          likeds(tncpo1_30)
006270160803     d  ticpi_30                           likeds(ticpi_30)
006280100406
006290100406      //---------------------------------------------------------------
006300100406      //?prototipi
006310100406      //---------------------------------------------------------------
006320100729      /copy gaitrasrc/srcprotopr,tibs02r
006330150427      /copy gaitrasrc/srcprotopr,tibs73r
006340161129      /copy gaitrasrc/srcprotopr,tibs69r
006350100406      /copy gaitrasrc/srcprotopr,trmk01r
006360100406      /copy gaitrasrc/srcprotopr,trmk02r
006370100406      /copy gaitrasrc/srcprotopr,trmk20r
006380140722      /copy gaitrasrc/srcProtoPR,TNTA12R
006390051107
006400051107      *------------------------------------------------------------------------*
006410090417
006420090417     c/exec sql
006430090417     c+ set option dynusrprf = *owner, closqlcsr = *endmod
006440090417     c/end-exec
006450051107
006460051117      * Richiamato da Gestione visite/offerte o da Comferma offerte
006470051117      * salto la videata di richiesta del codice
006480091104      * anche nel caso di richiamo per sola interrogazione (nuovo campo DS)
006490091124      * oppure manutenzione di un codice specifico (cliente-CNACO)
006500091124     c                   If        (*In06 or *In07
006510091124     c                              or $interroga or $modifica)
006520091124     c                              and not *in28
006530051118     c                   Goto      vid02
006540051115     c                   EndIf
006550091124
006560091124     c   28              goto      fine
006570051124
006580051124     * §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ * ?
006590051124     ?*  DATI DI RICERCA    ?
006600051124     ?*  Prima videata      ?
006610051124
006620060110     c                   ExSr      Sr_Pulvid
006630051118
006640051118     c     emi01         Tag
006650051107
006660051107     c                   Write     Ta60t01
006670051107     c                   Exfmt     Ta60d01
006680051107     c                   Eval      *In28 = *Off
006690051107     c                   Eval      *In90 = *Off
006700051107     c                   Clear                   v1cmsg
006710051219     c                   Eval      $d01 = *On
006720051107
006730051124     ?* F3=Fine  ?
006740051107     c                   If        *InKc or
006750051107      * 08=utente non abilitato
006760051107     c                             *In08
006770051107     c                   Goto      Fine
006780051107     c                   EndIf
006790051107
006800051124     ?* Controllo  ?
006810051107     c                   ExSr      Sr_Ctrd01
006820051118     c   28              Goto      emi01
006830051124
006840051130     c     vid02         Tag
006850060221     c                   Eval      wokpot = *Off
006860051130
006870051130      * Carico i dati nelle varie videate
006880051216     c                   ExSr      Sr_Cardati
006890091112     c   70
006900091112     corn06              ExSr      Sr_Carluoghi
006910051216     c                   ExSr      Sr_Carnote
006920051130
006930051130     * §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ * ?
006940051130     ?*  DATI ANAGRAFICI/BLOCCO   ?
006950051130     ?*  Seconda videata      ?
006960051107
006970051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
006980051221     c                   Eval      *In29 = *On
006990051207     c                   ExSr      Sr_Tastifun
007000051207
007010051118     c     emi02         Tag
007020051129
007030051107     c                   Write     Ta60t01
007040051122     c                   Write     Ta60z01
007050051107     c                   Exfmt     Ta60d02
007060051107     c                   Eval      *In28 = *Off
007070051124     c                   Eval      *In90 = *Off
007080051108     c                   Clear                   v2cmsg
007090051213     c                   Eval      wvideo = '02'
007100051107
007110051124     ?* F3=Fine  ?
007120051107     c                   If        *InKc
007130060217     c                   ExSr      Sr_F03
007140060217     c   80              Goto      emi02
007150051107     c                   EndIf
007160051114
007170051124     ?* F12=Ritorno  ?
007180051118     c                   If        *Inkl
007190060110     c                   ExSr      Sr_Pulvid
007200091106      * se richiamato in interrogazione deve uscire dal pgm
007210091124     c                   if        $interroga or $modifica
007220091106     c                   ExSr      Sr_F03
007230091106     c                   else
007240100208      * se non è interrogazione e non è anagrafica provvisoria
007250100208      * con F12 devo sbloccare i file
007260100208     c                   if        not *in05 and not *in06
007270100208     c                   unlock    cnaco00f
007280100208     c                   unlock    cnind00f
007290100208     c                   unlock    cnclp00f
007300100208     c                   unlock    fncls01l
007310100208     c                   endif
007320051118     c                   Goto      emi01
007330091106     c                   endif
007340051118     c                   EndIf
007350090717
007360090717     ?* F2=Rubrica  ?
007370090717     c                   If        *Inkb
007380090717     c                   Eval      wrag = v2crag
007390090717     c  n05              Eval      wrich = 'M'
007400090717     c   05              Eval      wrich = 'V'
007410090717     c                   clear                   wtnt
007420090717     c                   ExSr      Sr_F02
007430090717     c                   Goto      emi02
007440090717     c                   EndIf
007450060731
007460060731     ?* F4=Abilitazioni  ?
007470060731     c                   if        *inkd
007480060731     c                   exsr      sr_f04
007490060731     c                   goto      emi02
007500060731     c                   endif
007510100507
007520100507       //?F5=Attività
007530100507     c                   IF        *inke
007540100507     c                   exsr      sr_F05
007550100507     c                   goto      emi02
007560100507     c                   ENDIF
007570051128
007580051128     ?* F6=Conferma  ?
007590051128     c                   If        *InKf
007600051128     c                   ExSr      Sr_Conferma
007610051213     c   28              Goto      emi02
007620051222     c                   If        Not *In06 and Not *In07
007630091125     c                             and not $modifica
007640060110     c                   ExSr      Sr_Pulvid
007650051222     c                   Goto      emi01
007660051222     c                   Else
007670051222     c                   Goto      fine
007680051222     c                   EndIf
007690051128     c                   EndIf
007700051122
007710051124     ?* F7=Cliente Unificante  ?
007720051122     c                   If        *Inkg
007730051122     c                   ExSr      Sr_F07
007740051122     c                   Goto      emi02
007750051122     c                   EndIf
007760051206
007770100407     ?* F20=Int.Cli.Potenziali  ?
007780100407     c                   If        *Inku
007790051206      * Imposto i primi 5 caratteri della ragione sociale
007800051206     c                   Eval      w005a = v2crag
007810100407     c                   ExSr      Sr_F20
007820051206     c                   Goto      emi02
007830051206     c                   EndIf
007840051122
007850051124     ?* F11=Luoghi  ?
007860051122     c                   If        *Inkk
007870051122     c                   ExSr      Sr_F11
007880051122     c                   Goto      emi02
007890051122     c                   EndIf
007900051206
007910051206     ?* F14=Info Commerciali  ?
007920051207     c                   If        *Inkn
007930060208     c                   If        v2ccpo = *Zeros
007940060208     c                   Eval      *In28 = *On
007950060208     c                   Eval      v2cmsg = msg(49)
007960060208     c                   Else
007970051207     c                   Eval      wrag = v2crag
007980051206     c                   ExSr      Sr_F14
007990060208     c                   EndIf
008000051206     c                   Goto      emi02
008010051206     c                   EndIf
008020051122
008030051124     ?* F15=Codici Collegati  ?
008040051122     c                   If        *Inkp
008050051124     c                   If        clpscf = *Zeros
008060051124     c                   Eval      *In28 = *On
008070051128     c                   Eval      v2cmsg = msg(33)
008080051125     c                   Eval      v2cmsg = %trim(v2cmsg) + ' ' +
008090051125     c                             'non è possibile interrogare'
008100051124     c                   Else
008110051124     c                   Eval      w0070 = clpscf
008120051122     c                   ExSr      Sr_F15
008130051124     c                   EndIf
008140051122     c                   Goto      emi02
008150051122     c                   EndIf
008160051122
008170051124     ?* F18=Note  ?
008180051122     c                   If        *Inks
008190101111     c                   If        v2ccpo = *Zeros
008200101111     c                   Eval      *In28 = *On
008210101111     c                   Eval      h2riga = 14
008220101111     c                   Eval      h2colo = 28
008230101111     c                   Eval      v2cmsg = msg(12)
008240101111     c                   Else
008250051124     c                   Eval      wrag = v2crag
008260051122     c                   ExSr      Sr_F18
008270101111     c                   EndIf
008280051122     c                   Goto      emi02
008290051122     c                   EndIf
008300090717
008310060526     ?* F19=Variazioni  ?
008320060526     c                   If        *Inkt
008330060526     c                   ExSr      Sr_F19
008340060526     c                   Goto      emi02
008350060526     c                   EndIf
008360100406
008370100524       //?F22=Richiesta Contatto
008380100524     c                   IF        *inkw
008390101111     c                   If        v2ccpo = *Zeros
008400101111     c                   Eval      *In28 = *On
008410101111     c                   Eval      h2riga = 14
008420101111     c                   Eval      h2colo = 28
008430101111     c                   Eval      v2cmsg = msg(12)
008440101111     c                   Else
008450100524     c                   exsr      sr_F22
008460101111     c                   EndIf
008470100406     c                   goto      emi02
008480100406     c                   ENDIF
008490101213
008500101213       //?F17=Dati Fatturazione
008510101213     c                   IF        *inkr
008520101213     c                   exsr      sr_F17
008530101213     c                   goto      emi02
008540101213     c                   ENDIF
008550110223
008560110223       //?F21=Blocco clienti
008570110223     c                   IF        *inkv
008580110223     c                   If        v2ccpo = *Zeros
008590110223     c                   Eval      *In28 = *On
008600110223     c                   Eval      h2riga = 14
008610110223     c                   Eval      h2colo = 28
008620110223     c                   Eval      v2cmsg = msg(12)
008630110223     c                   else
008640110223     c                   exsr      sr_F21
008650110310      /free
008660110310       //?Se ho pianificato l'attività devo richiamare la gestione
008670110310       //?delle info commerciali
008680110310         IF  OMK17f12 = *blanks and OMK17err = *blanks;
008690110310           exsr sr_f14;
008700110310         ENDIF;
008710110310      /end-free
008720110223     c                   Endif
008730110223     c                   goto      emi02
008740110223     c                   ENDIF
008750051122
008760051124     ?* F24=Altri tasti  ?
008770051124     c                   If        *Inky and $loop > 1
008780051124     c                   ExSr      Sr_F24
008790051124     c                   Goto      emi02
008800051124     c                   EndIf
008810051107
008820051124     ?* Controllo  ?
008830051117     c  n05              ExSr      Sr_Ctrd02
008840051124     c   28
008850051124     cor 90              Goto      emi02
008860051130
008870051130     * §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ * ?
008880051130     ?*  DATI COMMERCIALI   ?
008890051130     ?*  Terza videata      ?
008900051130
008910051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
008920051221     c                   Eval      *In29 = *Off
008930051207     c                   ExSr      Sr_Tastifun
008940051219      * Ragione sociale x le videate dalla terza in poi
008950051219     c                   Eval      v3crag = v2crag
008960080313      * Se immissione imposto la categoria merceologica da potenziale
008970060110     c                   If        *In01 and savitc <> *Blanks and
008980060110     c                             v3citc = *Blanks
008990060110     c                   Eval      v3citc = savitc
009000060110     c                   EndIf
009010051207
009020051130     c     emi03         Tag
009030051130
009040051130     c                   Write     Ta60t01
009050051130     c                   Write     Ta60z01
009060051130     c                   Exfmt     Ta60d03
009070051130     c                   Eval      *In28 = *Off
009080051130     c                   Eval      *In90 = *Off
009090051130     c                   Clear                   v3cmsg
009100051213     c                   Eval      wvideo = '03'
009110051130
009120051130     ?* F3=Fine  ?
009130051130     c                   If        *InKc
009140060217     c                   ExSr      Sr_F03
009150060217     c   80              Goto      emi03
009160051130     c                   EndIf
009170051130
009180051130     ?* F12=Ritorno  ?
009190051207     c                   If        *Inkl
009200091111      * se richiamato in interrogazione deve uscire dal pgm
009210091124     c                   if        $interroga or $modifica
009220091111     c                   ExSr      Sr_F03
009230091111     c                   else
009240051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
009250051221     c                   Eval      *In29 = *On
009260051207     c                   ExSr      Sr_Tastifun
009270051207     c                   Goto      emi02
009280091111     c                   endif
009290051207     c                   EndIf
009300090717
009310090717     ?* F2=Rubrica  ?
009320090717     c                   If        *Inkb
009330090717     c                   Eval      wrag = v2crag
009340090717     c  n05              Eval      wrich = 'M'
009350090717     c   05              Eval      wrich = 'V'
009360090717     c                   clear                   wtnt
009370090717     c                   ExSr      Sr_F02
009380100607     c                   Goto      emi03
009390090717     c                   EndIf
009400060731
009410060731     ?* F4=Abilitazioni  ?
009420060731     c                   if        *inkd
009430060731     c                   exsr      sr_f04
009440060731     c                   goto      emi03
009450060731     c                   endif
009460100507
009470100507       //?F5=Attività
009480100507     c                   IF        *inke
009490100507     c                   exsr      sr_F05
009500100507     c                   goto      emi03
009510100507     c                   ENDIF
009520051130
009530051130     ?* F6=Conferma  ?
009540051130     c                   If        *InKf
009550051130     c                   ExSr      Sr_Conferma
009560051213     c   28              Goto      emi03
009570051222     c                   If        Not *In06 and Not *In07
009580091125     c                             and not $modifica
009590060110     c                   ExSr      Sr_Pulvid
009600051130     c                   Goto      emi01
009610051222     c                   Else
009620051222     c                   Goto      fine
009630051222     c                   EndIf
009640051130     c                   EndIf
009650051130
009660051130     ?* F7=Cliente Unificante  ?
009670051130     c                   If        *Inkg
009680051130     c                   ExSr      Sr_F07
009690051130     c                   Goto      emi03
009700051130     c                   EndIf
009710051130
009720051130     ?* F11=Luoghi  ?
009730051130     c                   If        *Inkk
009740051130     c                   ExSr      Sr_F11
009750051130     c                   Goto      emi03
009760051130     c                   EndIf
009770051206
009780051206     ?* F14=Info Commerciali  ?
009790051207     c                   If        *Inkn
009800060208     c                   If        v2ccpo = *Zeros
009810060208     c                   Eval      *In28 = *On
009820060208     c                   Eval      v3cmsg = msg(49)
009830060208     c                   Else
009840051207     c                   Eval      wrag = v3crag
009850051206     c                   ExSr      Sr_F14
009860060208     c                   EndIf
009870051206     c                   Goto      emi03
009880051206     c                   EndIf
009890051130
009900051130     ?* F15=Codici Collegati  ?
009910051130     c                   If        *Inkp
009920051130     c                   If        clpscf = *Zeros
009930051130     c                   Eval      *In28 = *On
009940051130     c                   Eval      v3cmsg = msg(33)
009950051130     c                   Eval      v3cmsg = %trim(v3cmsg) + ' ' +
009960051130     c                             'non è possibile interrogare'
009970051130     c                   Else
009980051130     c                   Eval      w0070 = clpscf
009990051130     c                   ExSr      Sr_F15
010000051130     c                   EndIf
010010051130     c                   Goto      emi03
010020051130     c                   EndIf
010030051130
010040051130     ?* F18=Note  ?
010050051130     c                   If        *Inks
010060051130     c                   Eval      wrag = v3crag
010070051130     c                   ExSr      Sr_F18
010080051130     c                   Goto      emi03
010090051130     c                   EndIf
010100060529
010110060529     ?* F19=Variazioni  ?
010120060529     c                   If        *Inkt
010130060529     c                   ExSr      Sr_F19
010140060529     c                   Goto      emi03
010150060529     c                   EndIf
010160100406
010170100524       //?F22=Richiesta Contatto
010180100524     c                   IF        *inkw
010190100524     c                   exsr      sr_F22
010200100406     c                   goto      emi03
010210100406     c                   ENDIF
010220101213
010230101213       //?F17=Dati Fatturazione
010240101213     c                   IF        *inkr
010250101213     c                   exsr      sr_F17
010260101213     c                   goto      emi03
010270101213     c                   ENDIF
010280110223
010290110223       //?F21=Blocco clienti
010300110223     c                   IF        *inkv
010310110223     c                   exsr      sr_F21
010320110310      /free
010330110310       //?Se ho pianificato l'attività devo richiamare la gestione
010340110310       //?delle info commerciali
010350110310         IF  OMK17f12 = *blanks and OMK17err = *blanks;
010360110310           exsr sr_f14;
010370110310         ENDIF;
010380110310      /end-free
010390110223     c                   goto      emi03
010400110223     c                   ENDIF
010410051130
010420051130     ?* F24=Altri tasti  ?
010430051130     c                   If        *Inky and $loop > 1
010440051130     c                   ExSr      Sr_F24
010450051130     c                   Goto      emi03
010460051130     c                   EndIf
010470051130
010480051130     ?* Controllo  ?
010490051130     c  n05              ExSr      Sr_Ctrd03
010500051219     c   28
010510051219     cor 90              Goto      emi03
010520051219      * se interrogazione controllo i luoghi e le note
010530051219     c                   If        *In05
010540140722     c                   eval      wRGR = 'RN'
010550140722     c                   exsr      sr_CtrNote
010560140722     c   28
010570140722     cor 90              goto      Emi03
010580140722     c                   eval      wRGR = 'RT'
010590140722     c                   exsr      sr_CtrNote
010600140722     c   28
010610140722     cor 90              goto      Emi03
010620140722     c                   eval      wRGR = 'RL'
010630140722     c                   exsr      sr_CtrNote
010640140722     c   28
010650140722     cor 90              goto      Emi03
010660051219     c                   EndIf
010670051108
010680051129
010690051124     * §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ * ?
010700100728     ?*  DATI FATTURAZIONE + PAGAMENTO FATTURA  ?
010710051130     ?*  Quarta videata      ?
010720051118
010730051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
010740051207     c                   ExSr      Sr_Tastifun
010750051219      * Se cliente non nuovo imposto la data prima spedizione fattura
010760051219      * uguale a oggi meno 1 anno
010770121119     c                   If        v3cnew = 'S' and *In01
010780051219     c                   Z-add     dataoggi1     dataamg
010790051219     c                   Z-add     aa1           aa2
010800051219     c                   Z-add     mm1           mm2
010810051219     c                   Z-add     gg1           gg2
010820051219     c                   Z-add     datagma       v4cdps
010830051219     c                   EndIf
010840051207
010850051130     c     emi04         Tag
010860100128
010870100128      * se sono da convalida offerta e non c'è il codice fatturazione
010880100129      * impostato imposto "?" nel campo del codice fatturazione
010890100129      * così all'enter si apre la window con i clienti trovati
010900100129     c                   IF        (v4cscf = *zeros or v4cscf = *blanks) and
010910100129     c                              *in07
010920100129     c                   eval      v4cscf = '000000?'
010930100128     c                   ENDIF
010940051129
010950051108     c                   Write     Ta60t01
010960051124     c                   Write     Ta60z01
010970051130     c                   Exfmt     Ta60d04
010980051108     c                   Eval      *In28 = *Off
010990051124     c                   Eval      *In90 = *Off
011000051130     c                   Clear                   v4cmsg
011010051213     c                   Eval      wvideo = '04'
011020051108
011030051124     ?* F3=Fine  ?
011040051108     c                   If        *InKc
011050060217     c                   ExSr      Sr_F03
011060060217     c   80              Goto      emi04
011070051108     c                   EndIf
011080051114
011090051124     ?* F12=Ritorno  ?
011100051207     c                   If        *InKl
011110091111      * se richiamato in interrogazione deve uscire dal pgm
011120091124     c                   if        $interroga or $modifica
011130091111     c                   ExSr      Sr_F03
011140091111     c                   else
011150051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
011160051221     c                   Eval      *In29 = *Off
011170051207     c                   ExSr      Sr_Tastifun
011180051207     c                   Goto      emi03
011190091111     c                   EndIf
011200051207     c                   EndIf
011210090717
011220090717     ?* F2=Rubrica  ?
011230090717     c                   If        *Inkb
011240090717     c                   Eval      wrag = v2crag
011250090717     c  n05              Eval      wrich = 'M'
011260090717     c   05              Eval      wrich = 'V'
011270090717     c                   clear                   wtnt
011280090717     c                   ExSr      Sr_F02
011290100607     c                   Goto      emi04
011300090717     c                   EndIf
011310060731
011320060731     ?* F4=Abilitazioni  ?
011330060731     c                   if        *inkd
011340060731     c                   exsr      sr_f04
011350060731     c                   goto      emi04
011360060731     c                   endif
011370100507
011380100507       //?F5=Attività
011390100507     c                   IF        *inke
011400100507     c                   exsr      sr_F05
011410100507     c                   goto      emi04
011420100507     c                   ENDIF
011430051128
011440051128     ?* F6=Conferma  ?
011450051128     c                   If        *InKf
011460051128     c                   ExSr      Sr_Conferma
011470051213     c   28              Goto      emi04
011480051222     c                   If        Not *In06 and Not *In07
011490091125     c                             and not $modifica
011500060110     c                   ExSr      Sr_Pulvid
011510051222     c                   Goto      emi01
011520051222     c                   Else
011530051222     c                   Goto      fine
011540051222     c                   EndIf
011550051128     c                   EndIf
011560051124
011570051124     ?* F7=Cliente Unificante  ?
011580051124     c                   If        *Inkg
011590051124     c                   ExSr      Sr_F07
011600051206     c                   Goto      emi04
011610051124     c                   EndIf
011620051124
011630051124     ?* F11=Luoghi  ?
011640051124     c                   If        *Inkk
011650051124     c                   ExSr      Sr_F11
011660051206     c                   Goto      emi04
011670051124     c                   EndIf
011680051206
011690051206     ?* F14=Info Commerciali  ?
011700051207     c                   If        *Inkn
011710060208     c                   If        v2ccpo = *Zeros
011720060208     c                   Eval      *In28 = *On
011730060208     c                   Eval      v4cmsg = msg(49)
011740060208     c                   Else
011750051207     c                   Eval      wrag = v3crag
011760051206     c                   ExSr      Sr_F14
011770060208     c                   EndIf
011780051206     c                   Goto      emi04
011790051206     c                   EndIf
011800051124
011810051124     ?* F15=Codici Collegati  ?
011820051124     c                   If        *Inkp
011830100127     c                   If        v4cscf = *Zeros or v4cscf = *blanks
011840100409     c                             or v4cscf = '000000?'
011850051124     c                   Eval      *In28 = *On
011860051130     c                   Eval      v4cmsg = msg(33)
011870051130     c                   Eval      v4cmsg = %trim(v4cmsg) + ' ' +
011880051125     c                             'non è possibile interrogare'
011890051124     c                   Else
011900100127     c                   Eval      w0070 = %dec(v4cscf:7:0)
011910051124     c                   ExSr      Sr_F15
011920051124     c                   EndIf
011930051206     c                   Goto      emi04
011940051124     c                   EndIf
011950051124
011960051124     ?* F18=Note  ?
011970051124     c                   If        *Inks
011980051130     c                   Eval      wrag = v3crag
011990051124     c                   ExSr      Sr_F18
012000051206     c                   Goto      emi04
012010051124     c                   EndIf
012020060529
012030060529     ?* F19=Variazioni  ?
012040060529     c                   If        *Inkt
012050060529     c                   ExSr      Sr_F19
012060060529     c                   Goto      emi04
012070060529     c                   EndIf
012080100406
012090100524       //?F22=Richiesta Contatto
012100100524     c                   IF        *inkw
012110100524     c                   exsr      sr_F22
012120100406     c                   goto      emi04
012130100406     c                   ENDIF
012140101213
012150101213       //?F17=Dati Fatturazione
012160101213     c                   IF        *inkr
012170101213     c                   exsr      sr_F17
012180101213     c                   goto      emi04
012190101213     c                   ENDIF
012200110223
012210110223       //?F21=Blocco clienti
012220110223     c                   IF        *inkv
012230110223     c                   exsr      sr_F21
012240110310      /free
012250110310       //?Se ho pianificato l'attività devo richiamare la gestione
012260110310       //?delle info commerciali
012270110310         IF  OMK17f12 = *blanks and OMK17err = *blanks;
012280110310           exsr sr_f14;
012290110310         ENDIF;
012300110310      /end-free
012310110223     c                   goto      emi04
012320110223     c                   ENDIF
012330051124
012340051124     ?* F24=Altri tasti  ?
012350051124     c                   If        *Inky and $loop > 1
012360051124     c                   ExSr      Sr_F24
012370051206     c                   Goto      emi04
012380051124     c                   EndIf
012390051108
012400051124     ?* Controllo  ?
012410051213     c  n05              ExSr      Sr_Ctrd04
012420051124     c   28
012430051130     cor 90              Goto      emi04
012440051219      * se interrogazione controllo i luoghi e le note
012450051219     c                   If        *In05
012460091112     c   70
012470091112     corn06              ExSr      Sr_Ctrl001
012480051219     c   28              Goto      emi04
012490051219     c                   ExSr      Sr_Ctrnt84
012500051219     c   28              Goto      emi04
012510051219     c                   EndIf
012520051107
012530051124     * §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ * ?
012540100728     ?*  PAGAMENTI VARI   ?
012550051130     ?*  Quinta videata      ?
012560051118
012570051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
012580051207     c                   ExSr      Sr_Tastifun
012590090612      * Blocco pagamenti
012600130322      * se Blocco pagamento NO e da stato del credito devo bloccare
012610130322     c                   If        wbloc = '0' and §4Wpbl = 'S'
012620130322      * se non sono in interrogazione
012630121002     c                             and not *in05 and
012640130322      * ed è stato variato lo stato del credito
012650121002     c                             V2Ccon <> %subst(CLPcon:2:2)
012660130322      * imposto il blocco dello stato del credito
012670130322     c                   Eval      wbloc = %subst(§4Wpjbkamm:1:1)
012680130322     c                   eval      V5Cblpedp = wbloc
012690060317     c                   EndIf
012700130322      * Imposto il blocco a video
012710130322     c                   If        wbloc = '0'
012720130322     c                   Eval      v5cblp = 'NO'
012730130322     c                   Else
012740100728     c                   Eval      v5cblp = 'SI'
012750060317     c                   EndIf
012760051207
012770051130     c     emi05         Tag
012780051129
012790051108     c                   Write     Ta60t01
012800051129     c                   Write     Ta60z01
012810051130     c                   Exfmt     Ta60d05
012820051108     c                   Eval      *In28 = *Off
012830051129     c                   Eval      *In90 = *Off
012840051130     c                   Clear                   v5cmsg
012850051213     c                   Eval      wvideo = '05'
012860051108
012870051129     ?* F3=Fine  ?
012880051108     c                   If        *InKc
012890060217     c                   ExSr      Sr_F03
012900060217     c   80              Goto      emi05
012910051108     c                   EndIf
012920051114
012930051129     ?* F12=Ritorno  ?
012940051207     c                   If        *InKl
012950091111      * se richiamato in interrogazione deve uscire dal pgm
012960091124     c                   if        $interroga or $modifica
012970091111     c                   ExSr      Sr_F03
012980091111     c                   else
012990051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
013000051207     c                   ExSr      Sr_Tastifun
013010051207     c                   Goto      emi04
013020091111     c                   EndIf
013030051207     c                   EndIf
013040090717
013050090717     ?* F2=Rubrica  ?
013060090717     c                   If        *Inkb
013070090717     c                   Eval      wrag = v2crag
013080090717     c  n05              Eval      wrich = 'M'
013090090717     c   05              Eval      wrich = 'V'
013100090717     c                   clear                   wtnt
013110090717     c                   ExSr      Sr_F02
013120100607     c                   Goto      emi05
013130090717     c                   EndIf
013140060731
013150060731     ?* F4=Abilitazioni  ?
013160060731     c                   if        *inkd
013170060731     c                   exsr      sr_f04
013180060731     c                   goto      emi05
013190060731     c                   endif
013200100507
013210100507       //?F5=Attività
013220100507     c                   IF        *inke
013230100507     c                   exsr      sr_F05
013240100507     c                   goto      emi05
013250100507     c                   ENDIF
013260051129
013270051129     ?* F6=Conferma  ?
013280051129     c                   If        *InKf
013290051129     c                   ExSr      Sr_Conferma
013300051213     c   28              Goto      emi05
013310051222     c                   If        Not *In06 and Not *In07
013320091125     c                             and not $modifica
013330060110     c                   ExSr      Sr_Pulvid
013340051222     c                   Goto      emi01
013350051222     c                   Else
013360051222     c                   Goto      fine
013370051222     c                   EndIf
013380051129     c                   EndIf
013390051129
013400051129     ?* F7=Cliente Unificante  ?
013410051129     c                   If        *Inkg
013420051129     c                   ExSr      Sr_F07
013430051130     c                   Goto      emi05
013440051129     c                   EndIf
013450051129
013460051129     ?* F11=Luoghi  ?
013470051129     c                   If        *Inkk
013480051129     c                   ExSr      Sr_F11
013490051130     c                   Goto      emi05
013500051129     c                   EndIf
013510051206
013520051206     ?* F14=Info Commerciali  ?
013530051207     c                   If        *Inkn
013540060208     c                   If        v2ccpo = *Zeros
013550060208     c                   Eval      *In28 = *On
013560060208     c                   Eval      v5cmsg = msg(49)
013570060208     c                   Else
013580051207     c                   Eval      wrag = v3crag
013590051206     c                   ExSr      Sr_F14
013600060208     c                   EndIf
013610051206     c                   Goto      emi05
013620051206     c                   EndIf
013630051129
013640051129     ?* F15=Codici Collegati  ?
013650051129     c                   If        *Inkp
013660100127     c                   If        v4cscf = *Zeros or v4cscf = *blanks
013670051129     c                   Eval      *In28 = *On
013680051130     c                   Eval      v5cmsg = msg(33)
013690051130     c                   Eval      v5cmsg = %trim(v5cmsg) + ' ' +
013700051129     c                             'non è possibile interrogare'
013710051129     c                   Else
013720100127     c                   Eval      w0070 = %dec(v4cscf:7:0)
013730051129     c                   ExSr      Sr_F15
013740051129     c                   EndIf
013750051130     c                   Goto      emi05
013760051129     c                   EndIf
013770051129
013780051129     ?* F18=Note  ?
013790051129     c                   If        *Inks
013800051130     c                   Eval      wrag = v3crag
013810051129     c                   ExSr      Sr_F18
013820051130     c                   Goto      emi05
013830051129     c                   EndIf
013840060529
013850060529     ?* F19=Variazioni  ?
013860060529     c                   If        *Inkt
013870060529     c                   ExSr      Sr_F19
013880060529     c                   Goto      emi05
013890060529     c                   EndIf
013900100406
013910100524       //?F22=Richiesta Contatto
013920100524     c                   IF        *inkw
013930100524     c                   exsr      sr_F22
013940100406     c                   goto      emi05
013950100406     c                   ENDIF
013960101213
013970101213       //?F17=Dati Fatturazione
013980101213     c                   IF        *inkr
013990101213     c                   exsr      sr_F17
014000101213     c                   goto      emi05
014010101213     c                   ENDIF
014020110223
014030110223       //?F21=Blocco clienti
014040110223     c                   IF        *inkv
014050110223     c                   exsr      sr_F21
014060110310      /free
014070110310       //?Se ho pianificato l'attività devo richiamare la gestione
014080110310       //?delle info commerciali
014090110310         IF  OMK17f12 = *blanks and OMK17err = *blanks;
014100110310           exsr sr_f14;
014110110310         ENDIF;
014120110310      /end-free
014130110223     c                   goto      emi04
014140110223     c                   ENDIF
014150051129
014160051129     ?* F24=Altri tasti  ?
014170051129     c                   If        *Inky and $loop > 1
014180051129     c                   ExSr      Sr_F24
014190051130     c                   Goto      emi05
014200051129     c                   EndIf
014210051129
014220051129     ?* Controllo  ?
014230051213     c  n05              ExSr      Sr_Ctrd05
014240051129     c   28
014250051130     cor 90              Goto      emi05
014260051219      * se interrogazione controllo i luoghi e le note
014270051219     c                   If        *In05
014280091112     c   70
014290091112     corn06              ExSr      Sr_Ctrl555
014300051219     c   28              Goto      emi05
014310091112     c   70
014320091112     corn06              ExSr      Sr_Ctrl666
014330051219     c   28              Goto      emi05
014340051219     c                   ExSr      Sr_Ctrnt85
014350051219     c   28              Goto      emi05
014360090616     c                   ExSr      Sr_Ctrnt89
014370090616     c   28              Goto      emi05
014380150424     c                   ExSr      Sr_Ctrnt82
014390100730     c   90
014400100730     cor 28              Goto      emi05
014410051219     c                   EndIf
014420051129
014430051129     * §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ * ?
014440051129     ?*  GESTIONE GIACENZE   ?
014450051130     ?*  Sesta videata      ?
014460051118
014470051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
014480051207     c                   ExSr      Sr_Tastifun
014490051207
014500120118     c     emi06         Tag
014510120118
014520120118      /free
014530120118       //?Se sono in convalida offerta e il campo tipo comunicazione al
014540120118       //?mittente non è impostato lo recupero dalla tabella '2G'
014550120118       //?valore di default
014560120118         IF  V6Ctcm = *blanks and *in07;
014570120118           V6Ctcm = §2Gtcm;
014580120118         ENDIF;
014590120118      /end-free
014600051206
014610051108     c                   Write     Ta60t01
014620051206     c                   Write     Ta60z01
014630051130     c                   Exfmt     Ta60d06
014640051108     c                   Eval      *In28 = *Off
014650051213     c                   Eval      *In90 = *Off
014660051130     c                   Clear                   v6cmsg
014670051213     c                   Eval      wvideo = '06'
014680051108
014690051206     ?* F3=Fine  ?
014700051108     c                   If        *InKc
014710060217     c                   ExSr      Sr_F03
014720060217     c   80              Goto      emi06
014730051108     c                   EndIf
014740051114
014750051206     ?* F12=Ritorno  ?
014760051207     c                   If        *InKl
014770091111      * se richiamato in interrogazione deve uscire dal pgm
014780091124     c                   if        $interroga or $modifica
014790091111     c                   ExSr      Sr_F03
014800091111     c                   else
014810051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
014820051207     c                   ExSr      Sr_Tastifun
014830051207     c                   Goto      emi05
014840091111     c                   EndIf
014850051207     c                   EndIf
014860090717
014870090717     ?* F2=Rubrica  ?
014880090717     c                   If        *Inkb
014890090717     c                   Eval      wrag = v2crag
014900090717     c  n05              Eval      wrich = 'M'
014910090717     c   05              Eval      wrich = 'V'
014920090717     c                   clear                   wtnt
014930090717     c                   ExSr      Sr_F02
014940100607     c                   Goto      emi06
014950090717     c                   EndIf
014960060731
014970060731     ?* F4=Abilitazioni  ?
014980060731     c                   if        *inkd
014990060731     c                   exsr      sr_f04
015000060731     c                   goto      emi06
015010060731     c                   endif
015020100507
015030100507       //?F5=Attività
015040100507     c                   IF        *inke
015050100507     c                   exsr      sr_F05
015060100507     c                   goto      emi06
015070100507     c                   ENDIF
015080051206
015090051206     ?* F6=Conferma  ?
015100051206     c                   If        *InKf
015110051206     c                   ExSr      Sr_Conferma
015120051213     c   28              Goto      emi06
015130051222     c                   If        Not *In06 and Not *In07
015140091125     c                             and not $modifica
015150060110     c                   ExSr      Sr_Pulvid
015160051222     c                   Goto      emi01
015170051222     c                   Else
015180051222     c                   Goto      fine
015190051222     c                   EndIf
015200051206     c                   EndIf
015210051206
015220051206     ?* F7=Cliente Unificante  ?
015230051206     c                   If        *Inkg
015240051206     c                   ExSr      Sr_F07
015250051206     c                   Goto      emi06
015260051206     c                   EndIf
015270051206
015280051206     ?* F11=Luoghi  ?
015290051206     c                   If        *Inkk
015300051206     c                   ExSr      Sr_F11
015310051206     c                   Goto      emi06
015320051206     c                   EndIf
015330051206
015340051206     ?* F14=Info Commerciali  ?
015350051207     c                   If        *Inkn
015360060208     c                   If        v2ccpo = *Zeros
015370060208     c                   Eval      *In28 = *On
015380060208     c                   Eval      v6cmsg = msg(49)
015390060208     c                   Else
015400051207     c                   Eval      wrag = v3crag
015410051206     c                   ExSr      Sr_F14
015420060208     c                   EndIf
015430051206     c                   Goto      emi06
015440051206     c                   EndIf
015450051206
015460051206     ?* F15=Codici Collegati  ?
015470051206     c                   If        *Inkp
015480100127     c                   If        v4cscf = *Zeros or v4cscf = *blanks
015490051206     c                   Eval      *In28 = *On
015500051206     c                   Eval      v6cmsg = msg(33)
015510051206     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
015520051206     c                             'non è possibile interrogare'
015530051206     c                   Else
015540100127     c                   Eval      w0070 = %dec(v4cscf:7:0)
015550051206     c                   ExSr      Sr_F15
015560051206     c                   EndIf
015570051206     c                   Goto      emi06
015580051206     c                   EndIf
015590051206
015600051206     ?* F18=Note  ?
015610051206     c                   If        *Inks
015620051206     c                   Eval      wrag = v3crag
015630051206     c                   ExSr      Sr_F18
015640051206     c                   Goto      emi06
015650051206     c                   EndIf
015660060529
015670060529     ?* F19=Variazioni  ?
015680060529     c                   If        *Inkt
015690060529     c                   ExSr      Sr_F19
015700060529     c                   Goto      emi06
015710060529     c                   EndIf
015720100406
015730100524       //?F22=Richiesta Contatto
015740100524     c                   IF        *inkw
015750100524     c                   exsr      sr_F22
015760100406     c                   goto      emi06
015770100406     c                   ENDIF
015780101213
015790101213       //?F17=Dati Fatturazione
015800101213     c                   IF        *inkr
015810101213     c                   exsr      sr_F17
015820101213     c                   goto      emi06
015830101213     c                   ENDIF
015840110223
015850110223       //?F21=Blocco clienti
015860110223     c                   IF        *inkv
015870110223     c                   exsr      sr_F21
015880110310      /free
015890110310       //?Se ho pianificato l'attività devo richiamare la gestione
015900110310       //?delle info commerciali
015910110310         IF  OMK17f12 = *blanks and OMK17err = *blanks;
015920110310           exsr sr_f14;
015930110310         ENDIF;
015940110310      /end-free
015950110223     c                   goto      emi06
015960110223     c                   ENDIF
015970051206
015980051206     ?* F24=Altri tasti  ?
015990051206     c                   If        *Inky and $loop > 1
016000051206     c                   ExSr      Sr_F24
016010051206     c                   Goto      emi06
016020051206     c                   EndIf
016030051206
016040051206     ?* Controllo  ?
016050051213     c  n05              ExSr      Sr_Ctrd06
016060051206     c   28
016070051206     cor 90              Goto      emi06
016080051219      * se interrogazione controllo i luoghi e le note
016090051219     c                   If        *In05
016100091112     c   70
016110091112     corn06              ExSr      Sr_Ctrl005
016120051219     c   28              Goto      emi06
016130051219     c                   ExSr      Sr_Ctrnt88
016140051219     c   28              Goto      emi06
016150091112     c   70
016160091112     corn06              ExSr      Sr_Ctrl300
016170060215     c   28              Goto      emi06
016180051219     c                   EndIf
016190051108
016200051130     * §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ * ?
016210051130     ?*  GESTIONE DANNI/TASSAZIONE PORTI ASSEGNATI/STOP AUTOTRASPORTATORI   ?
016220051130     ?*  Settima videata      ?
016230051213
016240051213      * Imposto il campo unico in fondo alla videata con i tasti funzione
016250051213     c                   ExSr      Sr_Tastifun
016260051118
016270051130     c     emi07         Tag
016280051108
016290051108     c                   Write     Ta60t01
016300051213     c                   Write     Ta60z01
016310051130     c                   Exfmt     Ta60d07
016320051108     c                   Eval      *In28 = *Off
016330051213     c                   Eval      *In90 = *Off
016340051130     c                   Clear                   v7cmsg
016350051213     c                   Eval      wvideo = '07'
016360051108
016370051213     ?* F3=Fine  ?
016380051108     c                   If        *InKc
016390060217     c                   ExSr      Sr_F03
016400060217     c   80              Goto      emi07
016410051108     c                   EndIf
016420051114
016430051213     ?* F12=Ritorno  ?
016440051213     c                   If        *InKl
016450091111      * se richiamato in interrogazione deve uscire dal pgm
016460091124     c                   if        $interroga or $modifica
016470091111     c                   ExSr      Sr_F03
016480091111     c                   else
016490051213      * Imposto il campo unico in fondo alla videata con i tasti funzione
016500051213     c                   ExSr      Sr_Tastifun
016510051213     c                   Goto      emi06
016520091111     c                   EndIf
016530051213     c                   EndIf
016540090717
016550090717     ?* F2=Rubrica  ?
016560090717     c                   If        *Inkb
016570090717     c                   Eval      wrag = v2crag
016580090717     c  n05              Eval      wrich = 'M'
016590090717     c   05              Eval      wrich = 'V'
016600090717     c                   clear                   wtnt
016610090717     c                   ExSr      Sr_F02
016620100607     c                   Goto      emi07
016630090717     c                   EndIf
016640060731
016650060731     ?* F4=Abilitazioni  ?
016660060731     c                   if        *inkd
016670060731     c                   exsr      sr_f04
016680060731     c                   goto      emi07
016690060731     c                   endif
016700100507
016710100507       //?F5=Attività
016720100507     c                   IF        *inke
016730100507     c                   exsr      sr_F05
016740100507     c                   goto      emi07
016750100507     c                   ENDIF
016760051213
016770051213     ?* F6=Conferma  ?
016780051213     c                   If        *InKf
016790051213     c                   ExSr      Sr_Conferma
016800051213     c   28              Goto      emi07
016810051222     c                   If        Not *In06 and Not *In07
016820091125     c                             and not $modifica
016830060110     c                   ExSr      Sr_Pulvid
016840051222     c                   Goto      emi01
016850051222     c                   Else
016860051222     c                   Goto      fine
016870051222     c                   EndIf
016880051213     c                   EndIf
016890051213
016900051213     ?* F7=Cliente Unificante  ?
016910051213     c                   If        *Inkg
016920051213     c                   ExSr      Sr_F07
016930051213     c                   Goto      emi07
016940051213     c                   EndIf
016950051213
016960051213     ?* F11=Luoghi  ?
016970051213     c                   If        *Inkk
016980051213     c                   ExSr      Sr_F11
016990051213     c                   Goto      emi07
017000051213     c                   EndIf
017010051213
017020051213     ?* F14=Info Commerciali  ?
017030051213     c                   If        *Inkn
017040060208     c                   If        v2ccpo = *Zeros
017050060208     c                   Eval      *In28 = *On
017060060208     c                   Eval      v7cmsg = msg(49)
017070060208     c                   Else
017080051213     c                   Eval      wrag = v3crag
017090051213     c                   ExSr      Sr_F14
017100060208     c                   EndIf
017110051213     c                   Goto      emi07
017120051213     c                   EndIf
017130051213
017140051213     ?* F15=Codici Collegati  ?
017150051213     c                   If        *Inkp
017160100127     c                   If        v4cscf = *Zeros or v4cscf = *blanks
017170051213     c                   Eval      *In28 = *On
017180051213     c                   Eval      v7cmsg = msg(33)
017190051213     c                   Eval      v7cmsg = %trim(v7cmsg) + ' ' +
017200051213     c                             'non è possibile interrogare'
017210051213     c                   Else
017220100127     c                   Eval      w0070 = %dec(v4cscf:7:0)
017230051213     c                   ExSr      Sr_F15
017240051213     c                   EndIf
017250051213     c                   Goto      emi07
017260051213     c                   EndIf
017270051213
017280051213     ?* F18=Note  ?
017290051213     c                   If        *Inks
017300051213     c                   Eval      wrag = v3crag
017310051213     c                   ExSr      Sr_F18
017320051213     c                   Goto      emi07
017330051213     c                   EndIf
017340060529
017350060529     ?* F19=Variazioni  ?
017360060529     c                   If        *Inkt
017370060529     c                   ExSr      Sr_F19
017380060529     c                   Goto      emi07
017390060529     c                   EndIf
017400100406
017410100524       //?F22=Richiesta Contatto
017420100524     c                   IF        *inkw
017430100524     c                   exsr      sr_F22
017440100406     c                   goto      emi07
017450100406     c                   ENDIF
017460101213
017470101213       //?F17=Dati Fatturazione
017480101213     c                   IF        *inkr
017490101213     c                   exsr      sr_F17
017500101213     c                   goto      emi07
017510101213     c                   ENDIF
017520110223
017530110223       //?F21=Blocco clienti
017540110223     c                   IF        *inkv
017550110223     c                   exsr      sr_F21
017560110310      /free
017570110310       //?Se ho pianificato l'attività devo richiamare la gestione
017580110310       //?delle info commerciali
017590110310         IF  OMK17f12 = *blanks and OMK17err = *blanks;
017600110310           exsr sr_f14;
017610110310         ENDIF;
017620110310      /end-free
017630110223     c                   goto      emi07
017640110223     c                   ENDIF
017650051213
017660051213     ?* F24=Altri tasti  ?
017670051213     c                   If        *Inky and $loop > 1
017680051213     c                   ExSr      Sr_F24
017690051213     c                   Goto      emi07
017700051213     c                   EndIf
017710051213
017720051213     ?* Controllo  ?
017730051213     c  n05              ExSr      Sr_Ctrd07
017740051219     c   28              Goto      emi07
017750051219      * se interrogazione controllo i luoghi e le note
017760051219     c                   If        *In05
017770091112     c   70
017780091112     corn06              ExSr      Sr_Ctrl006
017790051219     c   28              Goto      emi07
017800051219     c                   ExSr      Sr_Ctrnt87
017810051219     c   28              Goto      emi07
017820091112     c   70
017830091112     corn06              ExSr      Sr_Ctrl008
017840051219     c   28              Goto      emi07
017850091112     c   70
017860091112     corn06              ExSr      Sr_Ctrl002
017870060215     c   28              Goto      emi07
017880060215     c                   ExSr      Sr_Ctrnt83
017890060215     c   28              Goto      emi07
017900091112     c   70
017910091112     corn06              ExSr      Sr_Ctrl200
017920060215     c   28              Goto      emi07
017930060215     c                   ExSr      Sr_Ctrnt86
017940060215     c   28              Goto      emi07
017950051219     c                   EndIf
017960051213
017970051220     c  n05              Goto      emi07
017980051220
017990051220      * Se sono in interrogazione devo far vedere l'anagrafica clienti ritiro
018000051220      * visto che non è previsto il tasto F6 di conferma
018010051220      * Se però non è attiva la procedura ORM ritorno alla videata
018020160905      * se sto gestendo/interrogando cliente LOGISTICA no anagrafica clienti
018030160905      * ritiro
018040051220     c                   Movel     acoksc        w0030
018050051220     c                   Clear                   og148
018060051220     c     w0030         Chain     Azorg01l
018070051220     c                   If        %Found(Azorg01l)
018080051220     c                   Eval      og148 = orgde8
018090051220     c                   EndIf
018100120313     c                   If        §ogorm = *blanks
018110051220     c                   Goto      emi07
018120051220     c                   EndIf
018130160905     c                   IF        ClienteLog
018140160905     c                   goto      Fine
018150160905     c                   ENDIF
018160160905      /end-free
018170051220      * Richiamo l'interrogazione anagrafica clienti ritiro
018180070809     c                   reset                   FIOR37ds
018190070809     c                   eval      I37opz  = '5'
018200070809     c                   movel     ACOksc        I37fgs
018210070809     c                   movel     ACOksc        I37cro
018220070809     c                   movel(p)  FIOR37ds      KPJBU
018230070809     c                   call      'FIOR37R'
018240070809     c                   parm                    KPJBA
018250091111     c                   eval      fior37ds = kpjbu
018260051220
018270091124      * Se non richiamato neanche per interrogazione/modifica diretta
018280051220     c                   If        Not *In06 and Not *In07
018290091124     c                             and not $interroga
018300091124     c                             and not $modifica
018310060110     c                   ExSr      Sr_Pulvid
018320051220     c                   Goto      emi01
018330051220     c                   EndIf
018340051108
018350051107     c     Fine          Tag
018360060215
018370060215      * Se premuto F03 ritorno il dato al pgm chiamante
018380091113     c                   if        $pgmrich and
018390091113     c                             (*Inkc or w03cok = 'SI')
018400060215     c                   Eval      ta60f03 = '1'
018410060215     c                   EndIf
018420051221
018430051221      * Se richiamato da gestione visite passo i dati alla DS
018440051221     c                   If        *In06
018450051221     c                   Eval      ta60cpo = acolib
018460060607     c                   Eval      ta60cmm = clpage
018470051221     c                   EndIf
018480051108
018490051108     c                   If        wtibs69 = *On
018500051108     c                   Eval      i69tla = 'C'
018510051108     c                   Call      'TIBS69R'
018520051108     c                   Parm                    Tibs69ds
018530051108     c                   EndIf
018540060531     c                   If        wtibs73 = *On
018550060531     c                   Eval      ibs73tla = 'C'
018560060531     c                   Call      'TIBS73R'
018570060531     c                   Parm                    Tibs73ds
018580060531     c                   EndIf
018590081020     c                   If        wtrmk50 = *On
018600081020     c                   Eval      i50tla=    'C'
018610081020     c                   Call      'TRMK50R'
018620081020     c                   Parm                    kpjba
018630081020     c                   Parm                    TRMK50ds
018640081020     c                   EndIf
018650100127
018660100127     c                   IF        wtnta40
018670100127     c                   eval      ITA40tla = 'C'
018680100127     c                   call      'TNTA40R'
018690100127     c                   parm                    kpjba
018700100127     c                   parm                    TNTA40DS
018710100127     c                   ENDIF
018720160803
018730160803      /free
018740160803       //?Chiudo file storicizzazione variazioni potenziale
018750160803         clear TRMK30DS;
018760160803         clear tncpo_30;
018770160803         clear tncpo1_30;
018780160803         clear ticpi_30;
018790160803         IMK30tla = 'C';
018800160803         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
018810160803      /end-free
018820051107
018830051107     c                   Eval      *InLr = *On
018840051108
018850051108      *------------------------------------------------------------------------*
018860060110      * PULIZIA DELLE VIDEATE
018870051108      *------------------------------------------------------------------------*
018880060110     c     Sr_Pulvid     BegSr
018890060110
018900060110     c                   Clear                   h1riga
018910060110     c                   Clear                   h1colo
018920060110     c                   Clear                   h2riga
018930060110     c                   Clear                   h2colo
018940060110     c                   Clear                   h3riga
018950060110     c                   Clear                   h3colo
018960060110     c                   Clear                   h4riga
018970060110     c                   Clear                   h4colo
018980060110     c                   Clear                   h5riga
018990060110     c                   Clear                   h5colo
019000060110     c                   Clear                   h6riga
019010060110     c                   Clear                   h6colo
019020060110     c                   Clear                   h7riga
019030060110     c                   Clear                   h7colo
019040051108
019050060110      * Prima videata
019060051108     c                   Eval      v1ckcc = dutkci
019070051108     c                   Clear                   v1cksc
019080051108     c                   Clear                   v1crag
019090051108     c                   Clear                   skfil
019100051108     c                   Clear                   v1ccdf
019110051108     c                   Clear                   v1civa
019120051108
019130051108      * Imposto la sk dei p.o. di ricerca
019140051108      * se autorizzazzione diversa da distretto e se sono in filiale
019150051115     c                   If        wabi <> 'DI' and Not *In09
019160051108     c                   Clear                   yy
019170051108     c                   Do        24            xx
019180051108     c                   If        skpog(xx) <> *Blanks and skpog(xx) > *Zeros
019190051108     c                   Add       1             yy
019200051108     c                   Move      skpog(xx)     skfil(yy)
019210051108     c                   EndIf
019220051108     c                   EndDo
019230051108     c                   EndIf
019240060110
019250060110      * Seconda videata
019260060110     c                   Clear                   v2cduv
019270060110     c                   Clear                   v2ddtr
019280060110     c                   Clear                   v2cdtr
019290060110     c                   Clear                   v2copz
019300060110     c                   Clear                   v2ctic
019310060110     c                   Clear                   v2clin
019320060110     c                   Clear                   v2crag
019330060110     c                   Clear                   v2cvia
019340060110     c                   Clear                   v2ccit
019350060110     c                   Clear                   v2ccae
019360060110     c                   Clear                   v2cprv
019370060110     c                   Clear                   v2csta
019380060110     c                   Clear                   v2ctel
019390060110     c                   Clear                   v2ctlf
019400060110     c                   Clear                   v2ccdf
019410060110     c                   Clear                   v2civa
019420060110     c                   Clear                   v2cabl
019430060110     c                   Clear                   v2cblc
019440060110     c                   Clear                   v2ccon
019450060110     c                   Clear                   v2tb02
019460060110     c                   Clear                   v2ccpo
019470090616     c                   Clear                   v2csol
019480090616     c                   Clear                   v2fsol
019490091119     c                   clear                   v2ntcdc
019500091119     c                   clear                   savntcdc
019510060110      * Terza videata
019520060110     c                   Clear                   v3cage
019530060110     c                   Clear                   v3cclv
019540060110     c                   Clear                   v3citc
019550121119     c                   clear                   v3cnew
019560060110      * Quarta videata
019570060110     c                   Clear                   v4cscf
019580060110     c                   Clear                   savscf
019590060110     c                   Clear                   v4cuni
019600060110     c                   Clear                   savuni
019610060110     c                   Clear                   v4ctft
019620060110     c                   Clear                   savtft
019630060110     c                   Clear                   v4cfft
019640060110     c                   Clear                   savfft
019650060110     c                   Clear                   v4ctdf
019660060110     c                   Clear                   v4csin
019670060110     c                   Clear                   v4cdps
019680060110     c                   Clear                   v4cdus
019690060110     c                   Clear                   v4cnpn
019700060110     c                   Clear                   v4cnpr
019710060110     c                   Clear                   v4cdpr
019720100727     c                   Clear                   v4ccdp
019730100727     c                   Clear                   v4cbna
019740100727     c                   Clear                   v4cabi
019750100727     c                   Clear                   v4ccab
019760121105     c                   clear                   V4Ctpf
019770060110      * Quinta videata
019780060110     c                   Clear                   wbloc
019790100728     c                   Clear                   v5cblp
019800060110     c                   Clear                   v5cfpc
019810080115     c                   clear                   v5ciban
019820060110     c                   Clear                   v5cbab
019830100729     c                   clear                   v5tpdn
019840100728     c                   clear                   v5ibandn
019850100728     c                   clear                   v5babdn
019860140203     c                   clear                   v5bicdn
019870100729     c                   clear                   v5tpna
019880100728     c                   clear                   v5ibanna
019890100728     c                   clear                   v5babna
019900140203     c                   clear                   v5bicna
019910150424     c                   eval      BonificoCA = *off
019920150424     c                   eval      BonificoDN = *off
019930150424     c                   eval      BonificoNA = *off
019940150427     c                   eval      PagEsteroCA = *off
019950150427     c                   eval      PagEsteroDN = *off
019960150427     c                   eval      PagEsteroNA = *off
019970150427     c                   clear                   CodPagCA
019980150427     c                   clear                   CodPagDN
019990150427     c                   clear                   CodPagNA
020000060110      * Sesta videata
020010060110     c                   Clear                   v6ctcm
020020060110     c                   Clear                   v6ctcf
020030060110     c                   Clear                   v6capg
020040060110     c                   Clear                   v6cdmg
020050060801     c                   Clear                   v6ccag
020060060110      * Settima videata
020070060110     c                   Clear                   v7csdn
020080060110     c                   Clear                   v7csin
020090060110     c                   Clear                   v7ctpa
020100060110     c                   Clear                   v7cstp
020110060317
020120060317      * Pulisco la ds dello stato del credito per non sporcarmi i dati
020130060317      * del blocco tra un cliente e l'altro
020140060317     c                   Clear                   ds4w
020150100805
020160100805     c                   clear                   wbic
020170100805     c                   clear                   wcodpn
020180100805     c                   clear                   wcodpo
020190100805     c                   clear                   wiban
020200100805     c                   clear                   wpgm
020210060110
020220051108     c                   EndSr
020230051107
020240051107      *------------------------------------------------------------------------*
020250051107      * CONTROLLO PRIMA VIDEATA - DATI DI RICERCA
020260051107      *------------------------------------------------------------------------*
020270051107     c     Sr_Ctrd01     BegSr
020280051117
020290051117      * Pulisco i campi del posizionamento cursore
020300051117     c                   Clear                   h1riga
020310051117     c                   Clear                   h1colo
020320051117      * Spengo indicatori di posizionamento cursore
020330051117     c                   Setoff                                       414243
020340051117     c                   Setoff                                       444546
020350051117     c                   Setoff                                       474849
020360051117     c                   Setoff                                       505152
020370051117     c                   Setoff                                       535455
020380051117     c                   Setoff                                       565758
020390051117     c                   Setoff                                       596061
020400051117     c                   Setoff                                       626364
020410060110      * Spengo indicatori di comodo
020420090914     c                   Setoff                                       0102
020430060110     c                   Setoff                                       04
020440060711
020450060711     c                   eval      wforzaksc = *off
020460051107
020470051107      * Almeno un dato deve essere inserito
020480051107     c                   If        v1ckcc = *Zeros and v1cksc = *Zeros and
020490051108     c                             v1crag = *Blanks and
020500051107     c                             v1ccdf = *Blanks and v1civa = *Blanks
020510051111     c                   Eval      *In28 = *On
020520060109     c                   Eval      h1riga = 06
020530051118     c                   Eval      h1colo = 04
020540051111     c                   Eval      v1cmsg = msg(02)
020550051107     c                   Leavesr
020560051107     c                   EndIf
020570051107
020580051107      * CAPOCONTO
020590051107     c                   If        v1ckcc = *zeros
020600051107     c                   Eval      *In28 = *On
020610060109     c                   Eval      h1riga = 06
020620051118     c                   Eval      h1colo = 04
020630051107     c                   Eval      v1cmsg = msg(03)
020640051107     c                   Leavesr
020650051107     c                   EndIf
020660051107
020670051219      * Se sono in filiale posso solo gestire i capoconti presenti in £4
020680051219      * per ora c'è solo il capoconto clienti (151)
020690051219if  1c                   If        Not *In09
020700051107     c                   Move      v1ckcc        w004a
020710051107     c     w004a         Lookup    £4                                     30
020720051107     c                   If        Not *In30
020730051107     c                   Eval      *In28 = *On
020740060109     c                   Eval      h1riga = 06
020750051118     c                   Eval      h1colo = 04
020760051107     c                   Eval      v1cmsg = msg(04)
020770051107     c                   Leavesr
020780051107     c                   EndIf
020790051219    1c                   EndIf
020800051219
020810051219      * CODICE CLIENTE NON IMMESSO
020820051219if  1c                   If        v1cksc = *Zeros
020830060207      * se non c'è una ricerca per ragione sociale errore
020840060227     c                   If        v1crag = *Blanks and
020850060227     c                             v1ccdf = *Blanks and v1civa = *Blanks
020860060207     c                   Eval      *In28 = *On
020870060207     c                   Eval      h1riga = 06
020880060207     c                   Eval      h1colo = 12
020890060207     c                   Eval      v1cmsg = msg(02)
020900060207     c                   Leavesr
020910060207     c                   EndIf
020920051219
020930051219      * Controllo la sk dei p.o. di ricerca
020940051219     c                   ExSr      Sr_Ctrfil
020950051219
020960051219s   2c                   Select
020970051219      * Ricerca Alfabetica
020980051219     c                   When      v1crag <> *Blanks
020990051219     c                   Movel     rsut          pardut
021000051219     c                   Movea     skfil         parfil
021010051219     c                   Movel     v1ckcc        parkcc
021020051219     c                   Clear                   pardit
021030051219     c                   Z-add     1             paxnum
021040051219      * 2 = richiesta di visualizzazione del cliente unificante
021050051219     c                   Z-add     2             kkut
021060051219     c                   Call      'XALFA3BR'
021070051219     c                   Parm                    pardut
021080051219     c                   Parm                    kkut
021090051219     c                   Parm                    v1crag
021100051219     c                   Parm                    parkcc
021110051219     c                   Parm                    parsta
021120051219     c                   Parm                    parfil
021130051219     c                   Parm                    pardit
021140051219     c                   Parm                    paxnum
021150051219     c                   Parm                    paxkcm
021160051219     c                   Parm                    paxksm
021170051219     c                   Parm                    paxkdm
021180051219     c                   If        parsta = -1
021190051219     c                   Goto      emi01
021200051219     c                   EndIf
021210051219     c                   Movel     paxksm        v1cksc
021220051219     c                   If        parkcc = *Zeros
021230051219     c                   Movel     paxkcm        v1ckcc
021240051219     c                   EndIf
021250051219      * Ricerca per Codice Fiscale
021260051219     c                   When      v1ccdf <> *Blanks
021270051219     c                   Movel     rsut          pardut
021280051219     c                   Movea     skfil         parfil
021290051219     c                   Movel     v1ckcc        parkcc
021300051219     c                   Clear                   pardit
021310051219     c                   Z-add     1             paxnum
021320051219      * 2 = richiesta di visualizzazione del cliente unificante
021330051219     c                   Z-add     2             kkut
021340051219     c                   Clear                   pariva
021350051219     c                   Movel     v1ccdf        parcdf
021360051219     c                   Call      'XALFA3BR'
021370051219     c                   Parm                    pardut
021380051219     c                   Parm                    kkut
021390051219     c                   Parm                    v1crag
021400051219     c                   Parm                    parkcc
021410051219     c                   Parm                    parsta
021420051219     c                   Parm                    parfil
021430051219     c                   Parm                    pardit
021440051219     c                   Parm                    paxnum
021450051219     c                   Parm                    paxkcm
021460051219     c                   Parm                    paxksm
021470051219     c                   Parm                    paxkdm
021480051219     c                   Parm                    paresci
021490051219     c                   Parm                    parerr
021500051219     c                   Parm                    pariva
021510051219     c                   Parm                    parcdf
021520051219     c                   If        parsta = -1
021530051219     c                   Goto      emi01
021540051219     c                   EndIf
021550051219     c                   Movel     paxksm        v1cksc
021560051219     c                   Clear                   v1ccdf
021570051219     c                   If        parkcc = *Zeros
021580051219     c                   Movel     paxkcm        v1ckcc
021590051219     c                   EndIf
021600051219      * Ricerca per Partita IVA
021610051219     c                   When      v1civa <> *Blanks
021620051219     c                   Movel     rsut          pardut
021630051219     c                   Movea     skfil         parfil
021640051219     c                   Movel     v1ckcc        parkcc
021650051219     c                   Clear                   pardit
021660051219     c                   Z-add     1             paxnum
021670051219      * 2 = richiesta di visualizzazione del cliente unificante
021680051219     c                   Z-add     2             kkut
021690051219      * Se non ho impostato il codice nazione vado in ricerca con la sola
021700060109      * partita Iva altrimenti P.IVa + Nazione
021710051219     c                   If        v1ivaeu = *Blanks
021720051219     c                   Movel     v1ivait       pariva
021730051219     c                   Else
021740051219     c                   Movel     v1civa        pariva
021750051219     c                   EndIf
021760051219     c                   Clear                   parcdf
021770051219     c                   Call      'XALFA3BR'
021780051219     c                   Parm                    pardut
021790051219     c                   Parm                    kkut
021800051219     c                   Parm                    v1crag
021810051219     c                   Parm                    parkcc
021820051219     c                   Parm                    parsta
021830051219     c                   Parm                    parfil
021840051219     c                   Parm                    pardit
021850051219     c                   Parm                    paxnum
021860051219     c                   Parm                    paxkcm
021870051219     c                   Parm                    paxksm
021880051219     c                   Parm                    paxkdm
021890051219     c                   Parm                    paresci
021900051219     c                   Parm                    parerr
021910051219     c                   Parm                    pariva
021920051219     c                   Parm                    parcdf
021930051219     c                   If        parsta = -1
021940051219     c                   Goto      emi01
021950051219     c                   EndIf
021960051219     c                   Movel     paxksm        v1cksc
021970051219     c                   Clear                   v1civa
021980051219     c                   If        parkcc = *Zeros
021990051219     c                   Movel     paxkcm        v1ckcc
022000051219     c                   EndIf
022010051219    2c                   EndSl
022020051219    1c                   EndIf
022030051107
022040051107      * CODICE CLIENTE IMMESSO
022050051108if  1c                   If        v1cksc <> *Zeros
022060060711      * per prima cosa cerco il commerciale unificante del
022070060711      * cliente immesso
022080060711      * aggancio cnlcp effettivo xchè sono in prima videata quindi sono
022090060711      * per forza nell'anagrafica effettiva e non nella provvisoria
022100060711     c                   Eval      kksc = v1cksc
022110060711     c     kcnaco        chain(n)  cnclp00f
022120060711    2c                   if        %found(cnclp00f)
022130130806     c     CLPage        chain     AZCMM000
022140130806    3c                   if        Not  %found(AZCMM01L)
022150130806     c                   clear                   CMMuni
022160060711    3c                   endif
022170130806     c                   movel     CMMuni        wpoageuni
022180060711    2c                   endif
022190160905      * Cerco network del cliente
022200160905     c                   Clear                   og143
022210160905     c                   Movel     v1cksc        w0030
022220160905     c     w0030         Chain     Azorg01l
022230160905     c                   If        %Found(Azorg01l)
022240160905     c                   Eval      og143 = orgde3
022250160905     c                   EndIf
022260160905     c                   eval      ClienteLog = (§OGntw = 'LOG')
022270051117      * Controllo se cliente gestito dall'utente
022280051219      * se non sono in interrogazione
022290051219if  2c                   If        Not *In05
022300161005     c                   IF        §UTEksclog <> 'S' and
022310161005     c                             ClienteLog
022320161005     c                   Eval      *In28 = *On
022330161005     c                   Eval      h1riga = 06
022340161005     c                   Eval      h1colo = 12
022350161005     c                   Eval      v1cmsg = msg(10)
022360161005     c                   Leavesr
022370161005     c                   ENDIF
022380051117     c                   Movel     v1cksc        w003a
022390051117     c     w003a         Lookup    skpog                                  30
022400060711     c                   If        Not *In30
022410060711    4c                   if        wpoageuni <> *zeros
022420060711     c     wpoageuni     lookup    skpog                                  30
022430060711    5c                   if        not *In30
022440060711     c                   eval      *In28 = *On
022450060711     c                   eval      h1riga = 06
022460060711     c                   eval      h1colo = 12
022470060711     c                   eval      v1cmsg = msg(10)
022480060711     c                   leavesr
022490060711    5c                   endif
022500060711     c                   eval      wforzaksc = *on
022510060711    4c                   endif
022520060711    4c                   if        wforzaksc = *off
022530051117     c                   Eval      *In28 = *On
022540060109     c                   Eval      h1riga = 06
022550051117     c                   Eval      h1colo = 12
022560051117     c                   Eval      v1cmsg = msg(10)
022570051117     c                   Leavesr
022580060711    4c                   endif
022590051117     c                   EndIf
022600051219   x2c                   Else
022610051219      * Se sono in interrogazione e non sono utente di sede non posso
022620051219      * interrogare i clienti logistica o XXX
022630051219if  3c                   If        Not *In09
022640051219     c                   Move      v1ckcc        w004a
022650051219     c     w004a         Lookup    £4                                     30
022660051219if  4c                   If        *In30
022670161005     c                   If        §ogntw = 'LOG' or §ogntw = 'XXX'
022680051219     c                   Eval      *In28 = *On
022690060109     c                   Eval      h1riga = 06
022700051219     c                   Eval      h1colo = 12
022710051219     c                   Eval      v1cmsg = msg(10)
022720051219     c                   Leavesr
022730051219     c                   EndIf
022740051219    4c                   EndIf
022750051219    3c                   EndIf
022760051219    2c                   EndIf
022770051219      * Aggancio l'anagrafica
022780051219      * solo cnaco xchè se siamo in prima videata è x forza anagrafica
022790051219      * effettiva, la provvisoria è solo se richiamato
022800051222     c                   Eval      kksc = v1cksc
022810060111     c  n05kCnaco        Chain(e)  Cnaco00f
022820060111     c   05kCnaco        Chain(n)  Cnaco00f
022830051219      * Allocato errore
022840060220if  2c                   If        %Error and Not *In05
022850051219     c                   Eval      *In28 = *on
022860060109     c                   Eval      h1riga = 06
022870051219     c                   Eval      h1colo = 12
022880051219     c                   Eval      v1cmsg = msg(68)
022890051219     c                   Leavesr
022900051219    2c                   EndIf
022910051108if  2c                   If        %Found(Cnaco00f)
022920051213      * Cliente esistente - Manutenzione/Visualizzazione
022930051115     c                   If        acoflg <> '*'
022940051115     c  n05              Eval      *In02 = *On
022950051115     c  n05              Eval      vtcmod = mod(02)
022960051115     c   05              Eval      vtcmod = mod(03)
022970051107      * Cliente annullato
022980051107     c                   Else
022990051107     c                   Eval      *In04 = *On
023000051213     c                   Eval      *In05 = *On
023010051108     c                   Eval      vtcmod = mod(04)
023020051107     c                   EndIf
023030051107      * Cliente nuovo - immissione
023040051108   x2c                   Else
023050051116      * --> Se gestione clienti inserisco
023060051115if  3c                   If        Not *In05
023070051107     c                   Eval      *In01 = *On
023080051108     c                   Eval      vtcmod = mod(01)
023090051116      * --> Se interrogazione clienti errore
023100051115   x3c                   Else
023110051115     c                   Eval      *In28 = *On
023120060109     c                   Eval      h1riga = 06
023130051117     c                   Eval      h1colo = 12
023140051115     c                   Eval      v1cmsg = msg(05)
023150051115     c                   Leavesr
023160051115    3c                   EndIf
023170051108    2c                   EndIf
023180051219    1c                   EndIf
023190051116
023200051213      * Se non sono in interrogazione
023210051213     c                   If        Not *In05
023220051116      * PASSWORD
023230051116     c                   If        v1cpwd = *Blanks
023240051116     c                   Eval      *In28 = *On
023250051117     c                   Eval      h1riga = 21
023260051117     c                   Eval      h1colo = 29
023270060109     c                   Eval      v1cmsg = msg(07)
023280051116     c                   Leavesr
023290051116     c                   EndIf
023300051116      * Controllo se esatta
023310051116     c                   If        v1cpwd <> §mtcpwd
023320051116     c                   Eval      *In28 = *On
023330051117     c                   Eval      h1riga = 21
023340051117     c                   Eval      h1colo = 29
023350060109     c                   Eval      v1cmsg = msg(06)
023360051116     c                   Leavesr
023370051116     c                   EndIf
023380051116      * Controllo la validità se non sono in sede
023390051213if  2c                   If        Not *In09
023400051116      * Data immissione/variazione password
023410051116     c                   Clear                   wlbdat
023420051116     c                   Z-Add     §mtcdta       g02dat
023430051116     c                   Call      'XSRDA8'
023440051116     c                   Parm                    wlbdat
023450051116     c                   Movel     g02inv        dataiso
023460051116      * Scadenza password
023470051116     c                   Clear                   Tibs02ds
023480051116     c                   Eval      t02mod = 'C'
023490051116     c                   Eval      t02sif = knsif
023500051116     c                   Eval      t02cod = 'MTC'
023510051125     c                   Eval      t02ke1 = 'PSW'
023520051116     c                   Call      'TIBS02R'
023530051116     c                   Parm                    kpjba
023540051116     c                   Parm                    Tibs02ds
023550051213if  3c                   If        t02err <> *Blanks
023560051116     c                   Eval      *In28 = *On
023570051117     c                   Eval      h1riga = 21
023580051117     c                   Eval      h1colo = 29
023590051116     c                   Eval      v1cmsg = msg(09)
023600051116     c                   Leavesr
023610051213   x3c                   Else
023620051116     c                   Movel     t02uni        wgiorni
023630051116     c                   Adddur    wgiorni:*d    dataiso
023640051116     c                   Move      dataiso       datascad
023650051213if  4c                   If        dataoggi > dataScad
023660051116     c                   Eval      *In28 = *On
023670051117     c                   Eval      h1riga = 21
023680051117     c                   Eval      h1colo = 29
023690051116     c                   Eval      v1cmsg = msg(08)
023700051116     c                   Leavesr
023710051213    4c                   EndIf
023720051213    3c                   EndIf
023730051213    2c                   EndIf
023740051213    1c                   EndIf
023750051107
023760051107     c                   EndSr
023770051108
023780051108      *------------------------------------------------------------------------*
023790051108      * CONTROLLO SK DEI P.O. DI RICERCA
023800051108      *------------------------------------------------------------------------*
023810051108     c     Sr_Ctrfil     BegSr
023820051108
023830051108do  1c                   Do        24            xx
023840051108if  2c                   If        skfil(xx) <> *Blanks
023850051108     c     '?'           Scan      skfil(xx)                              30
023860051108if  3c                   If        *In30
023870051108     c                   Eval      §tip = 'E'
023880051108     c                   call      'TNSD23R'
023890051108     c                   Parm                    §tip
023900051108     c                   Parm                    §fil
023910051108
023920051108      * Imposto i p.o. selezionati
023930121130if  4c                   If        §fil <> *Blanks and §fil > *Zeros
023940051108     c                   Movea     §fil          skpo
023950051108     c                   Eval      yy = 1
023960051108if  5c                   Dow       skpo(yy) >*Zeros and xx <= 24
023970051108     c     '?'           Scan      skfil(xx)                              30
023980051108if  6c                   If        *In30 or skfil(xx) <= *Zeros
023990051108     c                   Movel     skpo(yy)      skfil(xx)
024000051108     c                   Add       1             yy
024010051108    6c                   EndIf
024020051108     c                   Add       1             xx
024030051108    5c                   EndDo
024040051108   x4c                   Else
024050051108     c                   Clear                   skfil(xx)
024060051108    4c                   EndIf
024070051108     c                   Leavesr
024080051108    3c                   EndIf
024090051108
024100051108      * Controllo i p.o. di ricerca immessi
024110121130if  3c                   If        skfil(xx) <> *Blanks and skfil(xx) > *Zeros
024120051108     c                   Move      skfil(xx)     w0030
024130160905     c                   clear                   og143
024140051108     c     w0030         Chain     Azorg01l
024150160905if  4c                   If        Not %Found(Azorg01l)
024160160905     c******                       (orgfag <> 'A' and orgfag <> 'F')
024170051108     c     xx            Add       40            zz
024180051108     c                   Eval      *In(zz) = *On
024190160906     c                   eval      *in28 = *on
024200160914     c                   eval      V1Cmsg = msg(91)
024210051118     c                   Goto      emi01
024220051108    4c                   EndIf
024230160905     c                   eval      OG143 = ORGde3
024240160905     c                   IF        §OGntw = 'XXX' or
024250160905     c                             (§UTEksclog <> 'S' and
024260160905     c                              §OGntw = 'LOG')
024270160905     c     xx            Add       40            zz
024280160905     c                   Eval      *In(zz) = *On
024290160906     c                   eval      *in28 = *on
024300160914     c                   eval      V1Cmsg = msg(91)
024310160905     c                   Goto      emi01
024320160905     c                   ENDIF
024330051108    3c                   EndIf
024340051108    2c                   EndIf
024350051108    1c                   EndDo
024360051108
024370051108     c                   EndSr
024380051216
024390051216      *------------------------------------------------------------------------*
024400051216      * CARICAMENTO DATI DAI FILE ALLE VIDEATE
024410051216      *------------------------------------------------------------------------*
024420051216     c     Sr_Cardati    BegSr
024430051216
024440051220     c                   Clear                   savabl
024450051220     c                   Clear                   savcpo
024460051220     c                   Clear                   savfft
024470051220     c                   Clear                   savitc
024480051220     c                   Clear                   savscf
024490051220     c                   Clear                   savtft
024500051220     c                   Clear                   savuni
024510080422     c                   clear                   sav4utip
024520100802     c                   clear                   savtipdn
024530100802     c                   clear                   savtipna
024540051216     c                   Clear                   wfscf
024550060105     c                   Eval      wforzacon = *Off
024560060215     c                   Clear                   savin22
024570060215     c                   Clear                   savin23
024580100728     c                   Clear                   savin71
024590100728     c                   Clear                   savin72
024600100728     c                   Clear                   savin73
024610100728     c                   Clear                   savin74
024620091119     c                   clear                   savntcdc
024630061030     c                   eval      wforzacdf = *off
024640061106     c                   eval      wforzaiva = *off
024650061106     c                   eval      *in39 = *off
024660080422     c                   eval      $win = *off
024670100729     c                   eval      $windn = *off
024680100729     c                   eval      $winna = *off
024690100729     c                   clear                   wbanca
024700100729     c                   clear                   wagenzia
024710100729     c                   clear                   savabidn
024720100729     c                   clear                   savabina
024730100729     c                   clear                   savcabdn
024740100729     c                   clear                   savcabna
024750100730     c                   clear                   savibandn
024760100730     c                   clear                   savibanna
024770100803     c                   eval      *in75 = *off
024780100803     c                   eval      *in76 = *off
024790100803     c                   eval      *in77 = *off
024800130322     c                   eval      *in79 = *off
024810130322     c                   eval      *in81 = *off
024820150415     c                   eval      *in82 = *off
024830170317     c                   eval      *in83 = *off
024840170320     c                   eval      *in84 = *off
024850170421     c                   eval      *in85 = *off
024860150424     c                   eval      wForzaPag = *off
024870150728     c                   eval      wForzaMail = *off
024880170516       wForzaPotenziale = *off;
024890110217
024900110217      /free
024910110217       //?pulisco campi di comodo per controlli da fare sul commerciale
024920110217       //?quando NON è anagrafica provvisoria
024930110217        clear sav_livello;
024940110217        clear wCodIncAtt;
024950110217      /end-free
024960051216
024970051219      * Se non sono passata dalla prima videata
024980051219      * aggancio l'anagrafica
024990051219if  1c                   If        $d01 = *Off
025000060110      * Spengo indicatori di comodo
025010090914     c                   Setoff                                       0102
025020060110     c                   Setoff                                       04
025030051222     c                   eval      kksc = v1cksc
025040051219      * Se non è richiamato da gestione visite e non è interrogazione
025050051219      * aggancio Cnaco00f per allocare il rcd
025060051219if  2c                   If        Not *In06 and Not *In05
025070060111     c  n05kCnaco        Chain(e)  Cnaco00f
025080051216      * Allocato errore
025090051219if  3c                   If        %error
025100051219      * Torno in prima videata se non è conferma visita
025110051216     c                   If        Not *In07
025120051216     c                   Eval      *In28 = *on
025130051216     c                   Eval      v1cmsg = msg(68)
025140051216     c                   Goto      emi01
025150051216     c                   EndIf
025160051219      * Torno al chiamante se è conferma visita
025170051216     c                   If        *In07
025180051216     c                   Goto      fine
025190051216     c                   EndIf
025200051219    3c                   EndIf
025210051219    2c                   EndIf
025220051219
025230091111      * Se è Interrogazione
025240091111     c                   if        *in05
025250091111      * visita/trattativa
025260091111     c                   if        *in06
025270091111     c                   eval      kksc = v1cnrv
025280091111     c     kCnaco        Chain(n)  Tfaco00f
025290091111      * cliente
025300091111     c                   else
025310051222     c                   eval      kksc = v1cksc
025320091111     c     kCnaco        Chain(n)  Cnaco00f
025330091111     c                   endif
025340091111     c                   endif
025350051216
025360091111      * Richiamato da gestione visite non in interrogazione
025370051219      * aggancio Tfaco00f e alloco il record
025380091111if  2c                   If        *In06 and not *in05
025390051222     c                   eval      kksc = v1cnrv
025400051216     c     kCnaco        Chain(e)  Tfaco00f
025410051216      * Allocato errore
025420051216     c                   If        %error
025430051216     c                   Goto      fine
025440051216     c                   EndIf
025450051219    2c                   EndIf
025460051219
025470051216      * Record trovato
025480051219if  2c                   If        %Found
025490051216      * Cliente esistente - manutenzione/Visualizzazione
025500051219if  3c                   If        acoflg <> '*'
025510051216     c  n05              Eval      *In02 = *On
025520051216     c  n05              Eval      vtcmod = mod(02)
025530051216     c   05              Eval      vtcmod = mod(03)
025540051216      * Cliente annullato
025550051219   x3c                   Else
025560051216     c                   Eval      *In04 = *On
025570051216     c                   Eval      vtcmod = mod(04)
025580051219    3c                   EndIf
025590051219      * Cliente nuovo - immissione
025600051219      * non essendo passata dalla prima videata l'inserimento avviene
025610051219      * solo se sono richiamata da gestione visite/offerte
025620051219      * quindi non controllo se sono in interrogazione
025630051219   x2c                   Else
025640051216     c                   Eval      *In01 = *On
025650051216     c                   Eval      vtcmod = mod(01)
025660051219    2c                   EndIf
025670051219    1c                   EndIf
025680051216
025690051216      * Se non è immissione imposto i dati dei file a video
025700051216if  1c                   If        Not *In01
025710060111     c  n06
025720060111     cann05kCnaco        Chain     Cnind00f
025730060111     c  n06
025740060111     can 05kCnaco        Chain(n)  Cnind00f
025750091111     c   06
025760091111     cann05kCnaco        Chain     Tfind00f
025770091111     c   06
025780091111     can 05kCnaco        Chain(n)  Tfind00f
025790060111     c  n06
025800060111     cann05kCnaco        Chain     Cnclp00f
025810060111     c  n06
025820060111     can 05kCnaco        Chain(n)  Cnclp00f
025830091111     c   06
025840091111     cann05kCnaco        Chain     Tfclp00f
025850091111     c   06
025860091111     can 05kCnaco        Chain(n)  Tfclp00f
025870060111     c  n06
025880060111     cann05v1cksc        Chain     Fncls01l
025890060111     c  n06
025900060111     can 05v1cksc        Chain(n)  Fncls01l
025910091111     c   06
025920091111     cann05v1cnrv        Chain     Tfcls01l
025930091111     c   06
025940091111     can 05v1cnrv        Chain(n)  Tfcls01l
025950051216
025960051216      * Data ultima variazione
025970051216     c                   Z-add     acoduv        dataamg
025980051216     c                   Z-add     aa1           aa2
025990051216     c                   Z-add     mm1           mm2
026000051216     c                   Z-add     gg1           gg2
026010051216     c                   Z-add     datagma       v2cduv
026020051216      * Allineato rcd da sede o da p.o.
026030051216     c                   If        acoftr = 'R'
026040051216     c                   Eval      v2ddtr = 'Ricevuto da Sede'
026050080115     c                   endif
026060080115     c                   if        acoftr = 'T'
026070051216     c                   Eval      v2ddtr = 'Trasmesso a Sede'
026080051216     c                   EndIf
026090051216      * Data allineamento rcd
026100051216     c                   Z-add     acodtr        dataamg
026110051216     c                   Z-add     aa1           aa2
026120051216     c                   Z-add     mm1           mm2
026130051216     c                   Z-add     gg1           gg2
026140051216     c                   Z-add     datagma       v2cdtr
026150051219      * Dati non visualizzati a video
026160051219     c                   Eval      v2copz = acoopz
026170051219     c                   Eval      v2ctic = acotic
026180051219     c                   Eval      v2clin = indlin
026190051216      * Dati anagrafici
026200051216     c                   Eval      v2crag = acorag
026210051216     c                   Eval      v2cvia = indvia
026220051216     c                   Eval      v2ccit = indcit
026230051216     c                   Eval      v2ccae = indcae
026240051216     c                   Eval      v2cprv = indprv
026250051216     c                   Eval      v2csta = indsta
026260051216     c                   Eval      v2ctel = indtel
026270051216     c                   Eval      v2ctlf = indtlf
026280051216     c                   Eval      v2ccdf = indcdf
026290080306     c                   Eval      savcdf = v2ccdf
026300051216     c                   Movel     indiva        w002a
026310051216     c                   If        w002a >= '00'
026320051216     c                   Movel     indiva        v2ivait
026330051216     c                   Else
026340051216     c                   Movel     indiva        v2civa
026350051216     c                   EndIf
026360080306     c                   Eval      saviva = v2civa
026370051216      * Blocco cliente
026380051216     c                   Eval      v2cabl = acoabl
026390130322     c                   eval      V2Cabledp = ACOabl
026400130322      * se blocco del cliente è '*' (blocco contabile) oppure
026410130322      * '7' (blocco automatico)
026420130322      * proteggo il campo del blocco + causale blocco
026430130322      * sempre, filiale e sede
026440130322     c                   IF        ACOabl = '*' or ACOabl = '7'
026450130322     c                   eval      *in79 = *on
026460130322     c                   ENDIF
026470130322     c                   IF        ACOabl = '7'
026480130322     c                   eval      V2Cabl = '8'
026490130322     c                   ENDIF
026500051220     c                   Eval      savabl = acoabl
026510051216      * Causale blocco cliente
026520051216     c                   Eval      v2cblc = clpnar
026530051216      * Stato del credito
026540051216     c                   Move      clpcon        v2ccon
026550110407      * Se il pgm è stato richiamato per il blocco cliente
026560110407      * imposto il blocco e la causale
026570110407     c                   IF        $Blocco
026580110407     c                   eval      v2cabl = '8'
026590110407     c                   eval      v2cblc = Parmcbl
026600110407     c                   ENDIF
026610051216      * Cliente annullabile
026620051216     c                   Eval      v2tb02 = atb02
026630051216      * Codice Potenziale
026640051216     c                   Eval      v2ccpo = acolib
026650051216     c                   Eval      savcpo = acolib
026660090616      * Cliente da sollecitare
026670090616     c                   Eval      v2csol = clpsol
026680090616      * sollecito in inglese
026690090616     c                   Eval      v2fsol = %subst(clsflo:4:1)
026700051216      * Codice commerciale
026710051216     c                   Movel     clpage        v3cage
026720051216      * Codice importanza cliente
026730051216     c                   Eval      v3cclv = clpclv
026740080313      * categoria merceologica
026750051216     c                   Move      acoitc        v3citc
026760051216      * Codice sottoconto fatturazione
026770100127     c                   Eval      v4cscf = %editc(clpscf:'X')
026780100329      * se sono da convalida offerta e non c'è il codice fatturazione
026790100329      * e la partita IVA è $$ imposto come codice fatturazione il codice
026800100329      * cliente che sto generando
026810100329     c                   IF        (v4cscf = *zeros or v4cscf = *blanks) and
026820100329     c                              *in07 and v2ivaeu = '$$'
026830100329     c                   eval      v4cscf = %editc(v1cksc:'X')
026840100329     c                   ENDIF
026850100127     c                   Eval      savscf = v4cscf
026860051216      * Fattura unificata
026870051216     c                   Eval      v4cuni = %subst(clsflo:2:1)
026880051216     c                   Eval      savuni = v4cuni
026890051216      * Tipo fattura
026900051216     c                   Move      clptft        v4ctft
026910051216     c                   Move      clptft        savtft
026920051216      * Frequenza fattura
026930051216     c                   Move      clpfft        v4cfft
026940051216     c                   Move      clpfft        savfft
026950051216      * Tipo data fattura (Inizio mese/Fine mese)
026960051216     c                   Eval      v4ctdf = clpfun
026970051216      * Spese invio fattura (nel file il campo non ha decimali solo a video)
026980051216     c                   Move      indsin        v4csin
026990051216      * Stampa mittente originale in fattura
027000051216     c                   Eval      v4cstm = %subst(clsflo:6:1)
027010051216      * Data prima spedizione fattura (solo visualizzazione viene impostato
027020051216      *                                dalla fatturazione)
027030051216     c                   Z-add     clpdps        dataamg
027040051216     c                   Z-add     aa1           aa2
027050051216     c                   Z-add     mm1           mm2
027060051216     c                   Z-add     gg1           gg2
027070051216     c                   Z-add     datagma       v4cdps
027080051216      * Data ultima spedizione fattura (solo visualizzazione viene impostato
027090051216      *                                 dalla fatturazione)
027100051216     c                   Z-add     clpdus        dataamg
027110051216     c                   Z-add     aa1           aa2
027120051216     c                   Z-add     mm1           mm2
027130051216     c                   Z-add     gg1           gg2
027140051216     c                   Z-add     datagma       v4cdus
027150051216      * Numero protocollo interno (solo visualizzazione viene impostato da Proj)
027160051216     c                   Eval      v4cnpn = indnpn
027170051216      * Numero protocollo cliente (solo visualizzazione viene impostato da Proj)
027180051216     c                   Eval      v4cnpr = indnpr
027190051216      * Data protocollo cliente   (solo visualizzazione viene impostato da Proj)
027200051216     c                   Z-add     inddpr        dataamg
027210051216     c                   Z-add     aa1           aa2
027220051216     c                   Z-add     mm1           mm2
027230051216     c                   Z-add     gg1           gg2
027240051216     c                   Z-add     datagma       v4cdpr
027250051216      * Codice pagamento
027260100727     c                   Move      indcdp        v4ccdp
027270051216      * Abi
027280100727     c                   Eval      v4cabi = indabi
027290051216      * Cab
027300100727     c                   Eval      v4ccab = indcab
027310121105      * Tassazione Post Fatturazopme
027320121105     c                   eval      V4Ctpf = %subst(CLSflo:3:1)
027330090612      * Blocco pagamenti
027340051216     c                   Eval      wbloc = %subst(indopz:1:1)
027350130322     c                   eval      V5Cblpedp = %subst(indopz:1:1)
027360130322      * se blocco pagamenti NON è 'B' (blocco manuale)
027370130322      * proteggo il campo del blocco pagamenti
027380130322      * sempre, filiale e sede
027390130322     c                   IF        V5Cblpedp <> 'B' and
027400130322     c                             V5Cblpedp <> '0' and
027410130322     c                             V5Cblpedp <> *blanks
027420130322     c                   eval      *in81 = *on
027430130322     c                   ENDIF
027440130322     c                   If        wbloc = '0'
027450130322     c                   Eval      v5cblp = 'NO'
027460130322     c                   Else
027470130322     c                   Eval      v5cblp = 'SI'
027480130322     c                   EndIf
027490100728
027500051216      * Codice pagamento contrassegni
027510051216     c                   Eval      v5cfpc = clpfpc
027520080115      * Codice IBAN
027530080115     c                   eval      v5ciban = clpbab
027540100803      * il BIC per i contrassegni per ora non ce l'ho mai quindi non lo faccio
027550100803      * mai vedere
027560100803     c                   eval      *in75 = *on
027570100728
027580100728      /free
027590100728       //?Nuovi tipi pagamento
027600100729       //?Pagamento DANNI --> DN
027610100728         v5tpdn = %subst(clstic:1:1);
027620100728       //?Recupero le coordinate bancarie
027630100728         wpag = 'DN';
027640100728         rqsOpCode = 'SEARCH';
027650100728         exsr Coordinate;
027660100728         IF  rpyEsito = 0;
027670100728           IF not *in06;
027680100730             v5ibandn = TrulIbanO0.IBAN;
027690100730             savabidn = TrulIbanO0.ABI;
027700100730             savcabdn = TrulIbanO0.CAB;
027710100730             v5bicdn  = TrulIbanO0.BIC;
027720100728           ELSE;
027730100730             v5ibandn = TrulIbanO1.IBAN;
027740100730             savabidn = TrulIbanO1.ABI;
027750100730             savcabdn = TrulIbanO1.CAB;
027760100730             v5bicdn  = TrulIbanO1.BIC;
027770100728           ENDIF;
027780100728         ENDIF;
027790100803         IF  v5bicdn = *blanks;
027800100803           *in76 = *on;
027810100803         ENDIF;
027820100729       //?Pagamento NOTE ACCREDITO --> NA
027830100728         v5tpna = %subst(clstic:2:1);
027840100728       //?Recupero le coordinate bancarie
027850100728         wpag = 'NA';
027860100728         exsr Coordinate;
027870100728         IF  rpyEsito = 0;
027880100728           IF not *in06;
027890100730             v5ibanna = TrulIbanO0.IBAN;
027900100730             savabina = TrulIbanO0.ABI;
027910100730             savcabna = TrulIbanO0.CAB;
027920100730             v5bicna  = TrulIbanO0.BIC;
027930100728           ELSE;
027940100730             v5ibanna = TrulIbanO1.IBAN;
027950100730             savabina = TrulIbanO1.ABI;
027960100730             savcabna = TrulIbanO1.CAB;
027970100730             v5bicna  = TrulIbanO1.BIC;
027980100728           ENDIF;
027990100728         ENDIF;
028000100803         IF  v5bicna = *blanks;
028010100803           *in77 = *on;
028020100803         ENDIF;
028030100730         savibandn = v5ibandn;
028040100730         savibanna = v5ibanna;
028050100728      /end-free
028060100728
028070051216      * Tipo comunicazione al mittente
028080051216     c                   Eval      v6ctcm = %subst(clsflo:10:1)
028090051216      * Tipo comunicazione fine giacenza
028100051216     c                   Eval      v6ctcf = %subst(clsflo:1:1)
028110051216      * Apertura automatica giacenza
028120051216     c                   Eval      v6capg = %subst(clsflo:9:1)
028130051216      * Disposizione mittente in arrivo
028140051216     c                   If        acorx1 = *Zeros
028150051216     c                   Eval      v6cdmg = 'SI'
028160051216     c                   Else
028170051216     c                   Eval      v6cdmg = 'NO'
028180051216     c                   EndIf
028190060801      * Chiusura automatica giacenza
028200060801     c                   If        acory1 = *Zeros
028210060801     c                   Eval      v6ccag = 'SI'
028220060801     c                   Else
028230060801     c                   Eval      v6ccag = 'NO'
028240060801     c                   EndIf
028250051216      * Rimborso danni
028260051216      * Stampa copia denuncia al cliente
028270051216     c                   Eval      v7csdn = %subst(clsflo:5:1)
028280051216      * Comunicazioni in inglese
028290051216     c                   Eval      v7csin = %subst(clsflo:7:1)
028300051216      * Escludi da tassazione diretta porti assegnati
028310051216     c                   Eval      v7ctpa = %subst(clsflo:8:1)
028320051216      * Nr. stop autotrasportatori
028330051216     c                   Eval      v7cstp = clsstp
028340051216
028350051216      * Se immissione imposto dei campi di default
028360051216   x1c                   Else
028370051219      * Campi non visualizzati
028380051219     c                   Eval      v2copz = wopz
028390051219     c                   Eval      v2ctic = wtic
028400051216     c                   Eval      v2clin = '1'
028410051219      * Campi visualizzati
028420051216     c                   Clear                   v2ccpo
028430051216      * Se provengo da visite/offerte
028440051216if  2c                   If        *In06
028450051216      * Carico i dati del potenziale
028460051220     c     ta60cpo       Chain(n)  Tncpo01l
028470051216     c                   If        %Found(Tncpo01l)
028480051216     c                   ExSr      Sr_Carcpo
028490051216     c                   EndIf
028500051216    2c                   EndIf
028510090612      * Cliente da sollecitare
028520090616      * imposto il dft
028530090616     c                   ExSr      Sr_Ctrsol
028540051216      * Commerciale
028550051216     c                   Clear                   v3cage
028560051216      * Imposto il codice commerciale della visita
028570051216     c   06              Movel     ta60cmm       v3cage
028580080313      * categoria merceologica
028590051216     c                   Clear                   v3citc
028600080313      * Se impostata dal potenziale riporto la categoria merceologica
028610051216     c                   If        savitc <> *Blanks
028620051216     c                   Eval      v3citc = savitc
028630051216     c                   EndIf
028640051219      * Codice importanza cliente
028650100301      * lo imposto se non è immissione anagrafica provvisoria da trattativa
028660100806     c  n06              Eval      v3cclv = 'C'
028670051216      * Codice sottoconto fatturazione
028680100729      * imposto a 0 così l'utente è obbligato ad inserirlo
028690100303     c                   eval      v4cscf = '0000000'
028700051216      * Tipo fattura
028710051216     c                   Eval      v4ctft = '0'
028720051216     c                   Eval      savtft = '0'
028730051216      * Frequenza fattura
028740051216     c                   Eval      v4cfft = '4'
028750051216     c                   Eval      savfft = '4'
028760091109      * Tipo data fattura (Inizio mese/Fine mese)
028770091125      * se da trattativa
028780100806     c   06              eval      v4ctdf = 'F'
028790051216      * Spese invio fatturazione
028800051216     c                   Z-add     §gedrf        v4csin
028810060210      * se non è anagrafica provvisoria e cliente 8888 o 9999 non devo
028820060210      * imputare le spese di invio fattura
028830060210     c                   Move      v1cksc        w0040
028840060210     c                   If        Not *In06 and
028850060210     c                             (w0040 = 8888 or w0040 = 9999)
028860060210     c                   Clear                   v4csin
028870060210     c                   EndIf
028880051216      * Protocollo
028890051216     c                   Clear                   v4cnpn
028900051216     c                   Clear                   v4cnpr
028910051216     c                   Clear                   v4cdpr
028920051221      * Codice Pagamento
028930100727     c                   Move      *Zeros        v4ccdp
028940130322      * Blocco pagamento campo di work
028950130322     c                   eval      wbloc = '0'
028960080115      * Tipo Pagamento contrassegni
028970091125      * se NON è da trattativa
028980100806     c  n06              eval      v5cfpc = '2'
028990100805
029000100805      /free
029010100805       //?Tipo pagamento Danni
029020150424         v5tpdn = dfttipdn;
029030100805       //?Tipo pagamento Note Accredito
029040150424         v5tpna = dfttipna;
029050100805      /end-free
029060100728
029070051216      * Tipo comunicazione al mittente
029080120118      * lo imposto se non è immissione anagrafica provvisoria da trattativa
029090120118     c  n06              Eval      v6ctcm = §2gtcm
029100051216      * Tipo comunicazione fine giacenza
029110051216     c                   Eval      v6ctcf = §2gtfg
029120051216      * Apertura automatica giacenza
029130051216     c                   Clear                   v6capg
029140051216      * Disposizione mittente in arrivo
029150051216     c                   Eval      v6cdmg = 'SI'
029160060801      * Chiusura automatica giacenza
029170060801     c                   Eval      v6ccag = 'SI'
029180051216
029190051216    1c                   EndIf
029200110217
029210110217      /free
029220110217       //?Se non è anagrafica provvisoria
029230110217         IF  not *in06;
029240110217       //?Aggancio l'organigramma x controllo se filiale cliente
029250110217       //?è un primo o secondo livello
029260110217           clear og148;
029270110217           w0030 = %dec(%subst(%editc(v1cksc:'X'):1:3):3:0);
029280110217           chain  w0030  azorg01l;
029290110217           IF  %found(azorg01l);
029300110217             og148 = ORGde8;
029310110217           ENDIF;
029320110217           sav_Livello = §OGlpo;
029330110217       //?Recupero da tabella Y4O il codice cliente 'incassi da attribuire'
029340110217           clear TIBS02DS;
029350110217           clear dY4O;
029360110217           T02mod = 'C';
029370110217           T02cod = 'Y4O';
029380110217           T02ke1 = '201';
029390110217           T02ke2 = %editc(w0030:'X');
029400110217           TNTBE_RicercaControllo (kpjba : tibs02ds);
029410110217           IF  T02err = *blanks;
029420110217             dY4O = T02uni;
029430110217           ENDIF;
029440110217           wCodIncAtt = %subst(§4Osc1:2:7);
029450110217         ENDIF;
029460170421
029470170421       //?Se sono in manutenzione
029480170421       //?Se non sono utente EDP e il cliente che sto manutenzionando
029490170421       //?un cliente IMPORT DPD proteggo i flag relativi alla
029500170421       //?Gestione Giacenze
029510170421         IF  *in02 and not *in20 and %lookup(V1Cksc:CliImport) > 0;
029520170421           *in85 = *on;
029530170421         ENDIF;
029540170421
029550110217      /end-free
029560051216
029570051216      * Decodifico
029580051216     c                   ExSr      Sr_Decdati
029590060531     c
029600060531     c* Memorizzo l'immagine precedente per  verificare se dati variati
029610150427      /free
029620150427         exsr ImmaginePrima;
029630150427      /end-free
029640051216
029650051216     c                   EndSr
029660100728
029670100728      /free
029680100728       //--------------------------------------------------------------
029690100728       //?Operazioni relative alle coordinate bancarie.
029700100728       //--------------------------------------------------------------
029710100728       BEGSR Coordinate;
029720100802
029730100802         clear rpyEsito;
029740100728
029750100728       //?FNCBA00F
029760100728         IF  not *in06;
029770100730           clear TrulIbanI0;
029780100730           clear TrulIbanO0;
029790100728           TrulIbanI0.KSC = v1cksc;
029800100728           TrulIbanI0.PAG = wpag;
029810100729         //?Se non è ricerca passo i dati del video
029820100729           IF  rqsOpCode <> 'SEARCH';
029830100805             TrulIbanI0.IBAN  = wiban;
029840100805             TrulIbanI0.BIC   = wbic;
029850100805             TrulIbanI0.CODPO = wcodpo;
029860100805             TrulIbanI0.CODPN = wcodpn;
029870100805             TrulIbanI0.PGM   = wpgm;
029880100729           ENDIF;
029890100728           rqsFormatName = 'TRULIBANI0';
029900100728           rpyFormatName = 'TRULIBANO0';
029910100728           rqsDataSize   = %size(TrulIbanI0);
029920100728           rpyDataSize   = %size(TrulIbanO0);
029930100728           TrulIbanR (rqsOpCode:rpyEsito:
029940100729                      rqsFormatName:TrulIbanI0:rqsDataSize:
029950100729                      rpyFormatname:TrulIbanO0:rpyDataSize);
029960100728       //?TFCBA00F
029970100728         ELSE;
029980100730           clear TrulIbanI1;
029990100730           clear TrulIbanO1;
030000100729           TrulIbanI1.NRV = v1cnrv;
030010100728           TrulIbanI1.PAG = wpag;
030020100729         //?Se non è ricerca passo i dati del video
030030100729           IF  rqsOpCode <> 'SEARCH';
030040100730             TrulIbanI1.IBAN = wiban;
030050100730             TrulIbanI1.BIC  = wbic;
030060100729           ENDIF;
030070100728           rqsFormatName = 'TRULIBANI1';
030080100728           rpyFormatName = 'TRULIBANO1';
030090100728           rqsDataSize   = %size(TrulIbanI1);
030100100728           rpyDataSize   = %size(TrulIbanO1);
030110100728           TrulIbanR (rqsOpCode:rpyEsito:
030120100728                      rqsFormatName:TrulIbanI1:rqsDataSize:
030130100729                      rpyFormatname:TrulIbanO1:rpyDataSize);
030140100728         ENDIF;
030150100728
030160100728       ENDSR;
030170100728      /end-free
030180051216
030190051216      *------------------------------------------------------------------------*
030200051216      * DECODIFICO I DATI DELLE VIDEATE
030210051216      *------------------------------------------------------------------------*
030220051216     c     Sr_Decdati    BegSr
030230051216
030240060220     c                   Eval      *In19 = *Off
030250051216     c                   Eval      *In22 = *Off
030260051216     c                   Eval      *In23 = *Off
030270100728     c                   Eval      *In71 = *Off
030280100728     c                   Eval      *In72 = *Off
030290100728     c                   Eval      *In73 = *Off
030300100728     c                   Eval      *In74 = *Off
030310150415     c                   Eval      *In82 = *Off
030320051216
030330051216      * Decodifico lo stato del credito
030340051216     c                   Clear                   v2dcon
030350051216     c                   Clear                   ds4w
030360051216     c                   Eval      kcod = '4W'
030370051216     c                   Move(p)   v2ccon        w003a
030380051216     c                   Eval      kkey = w003a
030390051216     c     kTabel        Chain     Tabel00f
030400051216     c                   If        %Found(Tabel00f) and
030410051216     c                             tblflg = *Blanks
030420051216     c                   Eval      ds4w = tbluni
030430051216     c                   EndIf
030440051216     c                   Eval      v2dcon = §4wdes
030450051216
030460051216      * Se sono in filiale proteggo lo stato del credito se non è modificabile
030470120928      * da p.o.
030480130325      * da 25/03/2013 abbiamo deciso che anche in sede non è modificale, solo
030490130325      * da ProJ
030500130325     c                   If        §4wmbl = 'N'
030510060220     c                   Eval      *In19 = *On
030520051216     c                   EndIf
030530130322
030540120928      * Se sono in filiale proteggo il blocco pagamenti se non è modificabile
030550120928      * da p.o.
030560120928     c                   If        Not *In09 and §4wmbp = 'N'
030570120928     c                   Eval      *In81 = *On
030580120928     c                   EndIf
030590120925      * Se sono in filiale proteggo il blocco cliente se non è modificabile
030600120925      * da p.o.
030610120928     c                   If        Not *In09 and §4wmtb = 'N'
030620120925     c                   Eval      *In79 = *On
030630120925     c                   EndIf
030640170317      /free
030650170330       //?Se sono un utente NON abilitato alla variazione
030660170317       //?e il blocco clienti è protetto
030670170317       //?e il cliente è in contenzioso
030680170330       //?devo bloccare anche la frequenza fattura
030690170330         IF  §UTEclpfft <> 'S' and *in79 and
030700170330             §4Wtip <> *blanks;
030710170317           *in83 = *on;
030720170317         ENDIF;
030730170317      /end-free
030740051216
030750051216      * Decodifico la causale blocco cliente
030760110323     c                   clear                   dsbi
030770051216     c                   Clear                   v2dblc
030780051216     c                   Eval      kcod = 'BI'
030790051216     c                   Eval      kkey = v2cblc
030800051216     c     kTabel        Chain     Tabel00f
030810051216     c                   If        %Found(Tabel00f) and
030820051216     c                             tblflg = *Blanks
030830110323     c                   Eval      dsbi = tbluni
030840051216     c                   EndIf
030850110323     c                   Eval      v2dblc = §bides
030860051216
030870051216      * Decodifico il codice potenziale
030880051216     c                   Clear                   v2dcpo
030890110223     c                   clear                   v2hfls
030900051216     c                   If        v2ccpo <> *Zeros
030910051220     c     v2ccpo        Chain(n)  Tncpo01l
030920051216     c                   If        %Found(Tncpo01l)
030930051216     c                   Eval      v2dcpo = cporag
030940110223     c                   eval      v2hfls = CPOfls
030950121105     c                   eval      dCPO01 = CPOrst
030960051216     c                   EndIf
030970051216     c                   EndIf
030980051216
030990051216      * Decodifico il codice commerciale
031000051216     c                   Clear                   v3dage
031010130806     c                   If        %check(digits:V3Cage) = 0
031020130806     c                   move      V3Cage        CMMcod
031030130806     c     CMMcod        chain     AZCMM000
031040130806     c                   if        %Found(AZCMM01L)
031050130806     c                   movel     CMMdes        v3dage
031060130806     c                   endif
031070130806     c                   EndIf
031080051216
031090051216      * Decodifico il codice importanza cliente
031100051216     c                   Clear                   v3dclv
031110051216     c                   Clear                   dsic
031120051216     c                   Eval      kcod = 'IC'
031130051216     c                   Eval      kkey = v3cclv
031140051216     c     kTabel        Chain     Tabel00f
031150051216     c                   If        %Found(Tabel00f) and
031160051216     c                             tblflg = *Blanks
031170051216     c                   Eval      dsic = tbluni
031180051216     c                   EndIf
031190051216     c                   Eval      v3dclv = §sicde
031200051216
031210080313      * Decodifico categoria merceologica
031220051216     c                   Clear                   v3ditc
031230051216     c                   Clear                   ds1l
031240051216     c                   Eval      kcod = '1L'
031250051216     c                   Movel     v3citc        kkey
031260051216     c     kTabel        Chain     Tabel00f
031270051216     c                   If        %Found(Tabel00f) and
031280051216     c                             tblflg = *Blanks
031290051216     c                   Eval      ds1l = tbluni
031300051216     c                   EndIf
031310051216     c                   Eval      v3ditc = §1ldes
031320051216
031330051216      * Decodifico sottoconto fatturazione
031340051216     c                   Clear                   Tibs69ds
031350051216     c                   Move      v4cscf        I69kac
031360051216     c                   Call      'TIBS69R'
031370051216     c                   Parm                    Tibs69ds
031380051216     c                   Parm                    ds_cnaco
031390051216     c                   Parm                    ds_cnind
031400051216     c                   Parm                    ds_cnclp
031410051216     c                   Parm                    ds_fncls
031420051216     c                   Eval      wtibs69 = *On
031430051216     c                   Movel     w_acorag      v4dscf
031440051216
031450051216      * Decodifico il tipo fattura
031460051216     c                   Clear                   Tibs02ds
031470051216     c                   Eval      t02mod = 'C'
031480051216     c                   Eval      t02sif = knsif
031490051216     c                   Eval      t02cod = 'TFT'
031500051216     c                   Eval      t02ke1 = v4ctft
031510051216     c                   Call      'TIBS02R'
031520051216     c                   Parm                    kpjba
031530051216     c                   Parm                    Tibs02ds
031540051216     c                   If        t02err = *Blanks
031550051216     c                   Eval      dtft = t02uni
031560051216     c                   EndIf
031570051216     c                   Eval      v4dtft = §tftdes
031580051216
031590051216      * Decodifico la frequenza fattura
031600051216     c                   Clear                   v4dfft
031610051216     c                   Clear                   dsff
031620051216     c                   Eval      kcod = 'FF'
031630051216     c                   Eval      kkey = v4cfft
031640051216     c     kTabel        Chain     Tabel00f
031650051216     c                   If        %Found(Tabel00f) and
031660051216     c                             tblflg = *Blanks
031670051216     c                   Eval      dsff = tbluni
031680051216     c                   EndIf
031690051216     c                   Eval      v4dfft = §ffdes
031700051216
031710051216      * Decodifico il tipo data fattura
031720051216     c                   If        v4ctdf <> *Blanks
031730051216     c                   Clear                   v4dtdf
031740051216     c                   Eval      kcod = '13'
031750051216     c                   Eval      kkey = v4ctdf
031760051216     c     kTabel        Chain     Tabel00f
031770051216     c                   If        %Found(Tabel00f) and
031780051216     c                             tblflg = *Blanks
031790051216     c                   Eval      v4dtdf = tbluni
031800051216     c                   EndIf
031810051216     c                   EndIf
031820051216
031830051216      * Decodifico codice di pagamento
031840100727     c                   Clear                   v4dcdp
031850051216     c                   Clear                   dsfa
031860051216     c                   Eval      kcod = 'FA'
031870100727     c                   Movel     v4ccdp        w004a
031880051216     c                   Move      '1'           w004a
031890051216     c                   Eval      kkey = w004a
031900051216     c     kTabel        Chain     Tabel00f
031910051216     c                   If        %Found(Tabel00f) and
031920051216     c                             tblflg = *Blanks
031930051216     c                   Eval      dsfa = tbluni
031940051216     c                   EndIf
031950100727     c                   Eval      v4dcdp = §fades
031960150415
031970150415      /free
031980150415       //?Se utente di filiale e tipo pagamento utilizzabile solo da sede
031990150415       //?devo proteggere la Banca ABI/CAB del pagamento fatture
032000150415         IF  not *in09 and §FAuti = 'S';
032010150415           *in82 = *on;
032020150415         ENDIF;
032030170320       //?Se sono un utente di filiale
032040170320       //?e il blocco clienti è protetto
032050170320       //?e il cliente è in contenzioso
032060170330       //?devo bloccare il codice pagamento
032070170330         IF  not *in09 and *in79 and §4Wtip <> *blanks;
032080170320           *in84 = *on;
032090170320         ENDIF;
032100150415      /end-free
032110080206
032120080206      * decodifico le coordinate bancarie abi-cab
032130100729      * riscossione fattura
032140100729     c                   clear                   v4cbna
032150100727     c                   eval      kabi = v4cabi
032160100727     c                   eval      kcab = v4ccab
032170100729     c                   exsr      DesBanca
032180100729     c                   eval      v4cbna = wdesbanca
032190051216
032200051216      * Decodifico codice pagamanto contrassegni
032210051216     c                   Clear                   v5dfpc
032220051216     c                   Clear                   ds4u
032230051216     c                   Eval      kcod = '4U'
032240051216     c                   Eval      kkey = v5cfpc
032250051216     c     kTabel        Chain     Tabel00f
032260051216     c                   If        %Found(Tabel00f) and
032270051216     c                             tblflg = *Blanks
032280051216     c                   Eval      ds4u = tbluni
032290051216     c                   EndIf
032300051216     c                   Eval      v5dfpc = §4udes
032310100729      * tipo pagamento contrassegni, salvo il flag di tipo pagamento i/e
032320100729     c                   eval      sav4utip = §4utip
032330051216      * Le coordinate bancare le visualizzo o le proteggo in base al
032340051216      * tipo pagamento
032350051216     c                   Select
032360051216      * Coordinate italiane, visualizzo
032370051216      * Immissione coordinate non richiesta, non visualizzo
032380051216     c                   When      §4uabi = 'N'
032390051216     c                   Eval      *In22 = *On
032400060215     c                   Eval      savin22 = *In22
032410051216      * Se pagamento estero proteggo le coordinate bancarie perchè
032420051216      * gestite dalla sede.
032430080115     c                   when      §4utip = 'E'
032440051216     c                   Eval      *In23 = *On
032450060215     c                   Eval      savin23 = *In23
032460051216     c                   EndSl
032470080115      * se le visualizzo ed è un tipo pagamento italia decodifico la banca
032480080115     c                   if        not *in22 and not *in23
032490080206     c                             and v5ciban <> *blanks
032500080213     c                             and (%subst(v5ciban:1:2) = 'IT'
032510080213     c                              or %subst(v5ciban:1:2) = 'SM')
032520080116     c                   clear                   v5cbab
032530080213     c                   monitor
032540080115     c                   eval      kabi = %dec(%subst(v5ciban:6:5):5:0)
032550080115     c                   eval      kcab = %dec(%subst(v5ciban:11:5):5:0)
032560080213     c                   on-error
032570080213     c                   clear                   kabi
032580080213     c                   clear                   kcab
032590080213     c                   endmon
032600100729     c                   exsr      DesBanca
032610100729     c                   eval      v5cbab = wdesbanca
032620080115     c                   endif
032630090401      * se le visualizzo ed è un pagemento estero senza IBAN con
032640090401      * abi e cab no a 99999 decodifico la banca
032650090401     c                   if        not *in22 and *in23
032660090401     c                             and v5ciban = *blanks
032670090401     c                             and clpabi <> *all'9'
032680090401     c                             and clpcab <> *all'9'
032690090401     c                   monitor
032700090401     c                   eval      kabi = clpabi
032710090401     c                   eval      kcab = clpcab
032720090401     c                   on-error
032730090401     c                   clear                   kabi
032740090401     c                   clear                   kcab
032750090401     c                   endmon
032760100729     c                   exsr      DesBanca
032770100729     c                   eval      v5cbab = wdesbanca
032780090401     c                   endif
032790100728
032800100728      /free
032810100728       //?Pagamenti DANNI --> DN
032820100728       //?tipo pagamento
032830100728         clear v5dpdn;
032840100728         clear ds4u;
032850100728         kcod = '4U';
032860100730         kkey = v5tpdn;
032870100728         chain (codut:kcod:kkey) TABEL00F;
032880100728         IF  %found(TABEL00F) and TBLflg = *blanks;
032890100728           ds4u = TBLuni;
032900100728         ENDIF;
032910100730         v5dpdn = §4Udes;
032920100802       //?salvo il tipo pagamento
032930100802         savtipdn = v5tpdn;
032940100728       //?coordinate bancarie
032950100728         SELECT;
032960100728         //?se abi cab non obbligatori non visualizzo
032970100730           WHEN  §4Uabi = 'N';
032980100728             *in71 = *on;
032990100728             savin71 = *in71;
033000100805             *in76 = *off;
033010100728         //?se pagamento estero proteggo perchè solo la sede le gestisce
033020100730           WHEN  §4Utip = 'E';
033030100728             *in72 = *on;
033040100728             savin72 = *in72;
033050100728         ENDSL;
033060100729       //?se IBAN IT o SM (IBAN italia) visualizzo la descrizione banca
033070100729       //?recuperandola con ABI e CAB dell'IBAN
033080100729         IF  not *in71 and not *in72 and v5ibandn <> *blanks and
033090100729            (%subst(v5ibandn:1:2) = 'IT' or %subst(v5ibandn:1:2) = 'SM');
033100100729           clear v5babdn;
033110100729           monitor;
033120100729           kabi = %dec(%subst(v5ibandn:6:5):5:0);
033130100729           kcab = %dec(%subst(v5ibandn:11:5):5:0);
033140100729           on-error;
033150100729           clear kabi;
033160100729           clear kcab;
033170100729           endmon;
033180100729           exsr DesBanca;
033190100729           v5babdn = wDesBanca;
033200100729         ENDIF;
033210100729       //?se IBAN estero (campo iban vuoto e ABI e CAB impostati <> 99999)
033220100729       //?visualizzo la descrizione della banca recuperandola con ABI e CAB
033230100729       //?memorizzati sul file
033240100729         IF  not *in71 and not *in72 and v5ibandn = *blanks and
033250100729             savabidn <> *all'9' and savcabdn <> *all'9';
033260100729           clear v5babdn;
033270100729           monitor;
033280100729           kabi = savabidn;
033290100729           kcab = savcabdn;
033300100729           on-error;
033310100729           clear kabi;
033320100729           clear kcab;
033330100729           endmon;
033340100729           exsr DesBanca;
033350100729           v5babdn = wDesBanca;
033360100729         ENDIF;
033370100729
033380100729       //?Pagamenti NOTE ACCREDITO --> NA
033390100729       //?tipo pagamento
033400100729         clear v5dpna;
033410100729         clear ds4u;
033420100729         kcod = '4U';
033430100730         kkey = v5tpna;
033440100729         chain (codut:kcod:kkey) TABEL00F;
033450100729         IF  %found(TABEL00F) and TBLflg = *blanks;
033460100729           ds4u = TBLuni;
033470100729         ENDIF;
033480100730         v5dpna = §4Udes;
033490100802       //?salvo il tipo pagamento
033500100802         savtipna = v5tpna;
033510100729       //?coordinate bancarie
033520100729         SELECT;
033530100729         //?se abi cab non obbligatori non visualizzo
033540100730           WHEN  §4Uabi = 'N';
033550100729             *in73 = *on;
033560100729             savin73 = *in73;
033570100805             *in77 = *off;
033580100729         //?se pagamento estero proteggo perchè solo la sede le gestisce
033590100730           WHEN  §4Utip = 'E';
033600100729             *in74 = *on;
033610100729             savin74 = *in74;
033620100729         ENDSL;
033630100729       //?se IBAN IT o SM (IBAN italia) visualizzo la descrizione banca
033640100729       //?recuperandola con ABI e CAB dell'IBAN
033650100729         IF  not *in73 and not *in74 and v5ibanna <> *blanks and
033660100729            (%subst(v5ibanna:1:2) = 'IT' or %subst(v5ibanna:1:2) = 'SM');
033670100729           clear v5babna;
033680100729           monitor;
033690100729           kabi = %dec(%subst(v5ibanna:6:5):5:0);
033700100729           kcab = %dec(%subst(v5ibanna:11:5):5:0);
033710100729           on-error;
033720100729           clear kabi;
033730100729           clear kcab;
033740100729           endmon;
033750100729           exsr DesBanca;
033760100729           v5babna = wDesbanca;
033770100729         ENDIF;
033780100729       //?se IBAN estero (campo iban vuoto e ABI e CAB impostati <> 99999)
033790100729       //?visualizzo la descrizione della banca recuperandola con ABI e CAB
033800100729       //?memorizzati sul file
033810100729         IF  not *in73 and not *in74 and v5ibanna = *blanks and
033820100729             savabina <> *all'9' and savcabna <> *all'9';
033830100729           clear v5babna;
033840100729           monitor;
033850100729           kabi = savabina;
033860100729           kcab = savcabna;
033870100729           on-error;
033880100729           clear kabi;
033890100729           clear kcab;
033900100729           endmon;
033910100729           exsr DesBanca;
033920100729           v5babna = wDesbanca;
033930100729         ENDIF;
033940100729
033950100728      /end-free
033960051216
033970051216      * Decodifico tipo comunicazioni al mittente
033980051216     c                   Clear                   v6dtcm
033990051216     c                   Clear                   ds2f
034000051216     c                   Eval      kcod = '2F'
034010051216     c                   Eval      kkey = v6ctcm
034020051216     c     kTabel        Chain     Tabel00f
034030051216     c                   If        %Found(Tabel00f) and
034040051216     c                             tblflg = *Blanks
034050051216     c                   Eval      ds2f = tbluni
034060051216     c                   EndIf
034070051216     c                   Eval      v6dtcm = §2fdes
034080051216
034090051216      * Decodifico tipo comunicazioni fine giacenza
034100051216     c                   Clear                   v6dtcf
034110051216     c                   Clear                   ds2h
034120051216     c                   Eval      kcod = '2H'
034130051216     c                   Eval      kkey = v6ctcf
034140051216     c     kTabel        Chain     Tabel00f
034150051216     c                   If        %Found(Tabel00f) and
034160051216     c                             tblflg = *Blanks
034170051216     c                   Eval      ds2h = tbluni
034180051216     c                   EndIf
034190051216     c                   Eval      v6dtcf = §2hdes
034200051216
034210051216      * Decodifico apertura automatica giacenza
034220051216     c                   Clear                   v6dapg
034230051216     c                   Clear                   ds2u
034240051216     c                   Eval      kcod = '2U'
034250051216     c                   Eval      kkey = v6capg
034260051216     c     kTabel        Chain     Tabel00f
034270051216     c                   If        %Found(Tabel00f) and
034280051216     c                             tblflg = *Blanks
034290051216     c                   Eval      ds2u = tbluni
034300051216     c                   EndIf
034310051216     c                   Eval      v6dapg = §2udes
034320051216
034330051216     c                   EndSr
034340051216
034350051216      *------------------------------------------------------------------------*
034360051216      * CARICO I LUOGHI PREVISTI
034370051216      *------------------------------------------------------------------------*
034380051216     c     Sr_Carluoghi  BegSr
034390051216
034400051216     c                   Eval      *In10 = *Off
034410051216     c                   Eval      *In11 = *Off
034420051216     c                   Eval      *In12 = *Off
034430051216     c                   Eval      *In14 = *Off
034440051216     c                   Eval      *In16 = *Off
034450051216     c                   Eval      *In17 = *Off
034460060208     c                   Eval      *In33 = *Off
034470060208     c                   Eval      *In35 = *Off
034480060208     c                   Eval      *In37 = *Off
034490051216
034500051216      * Luogo invio fattura
034510051216     c                   Eval      kspecli = v1cksc
034520051216     c                   Eval      kspecod = '001'
034530051216     c     kFnspe        Chain     Fnspe01l
034540051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
034550051216     c                   Eval      *In10 = *On
034560051216     c                   EndIf
034570051216
034580051216      * Luogo intestatario pagamento contrassegno
034590051216     c                   Eval      kspecod = '555'
034600051216     c     kFnspe        Chain     Fnspe01l
034610051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
034620051216     c                   Eval      *In11 = *On
034630051216     c                   EndIf
034640051216
034650051216      * Luogo invio contrassegno mittente
034660051216     c                   Eval      kspecod = '666'
034670051216     c     kFnspe        Chain     Fnspe01l
034680051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
034690051216     c                   Eval      *In12 = *On
034700051216     c                   EndIf
034710051216
034720051216      * Luogo spedizione giacenza
034730051219     c                   Clear                   wfax005
034740051219     c                   Clear                   wmail005
034750051216     c                   Eval      kspecod = '005'
034760051216     c     kFnspe        Chain     Fnspe01l
034770051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
034780051216     c                   Eval      *In14 = *On
034790051216     c                   Eval      wfax005 = spefax
034800051219     c     kFnsp2        Chain     Fnsp201l
034810051219     c                   If        %Found(Fnsp201l) and sp2flg = *Blanks
034820051219     c                   Eval      wmail005 = sp2est
034830051219     c                   EndIf
034840051216     c                   EndIf
034850060208
034860060208      * Luogo invio merce resa
034870060208     c                   Eval      kspecod = '300'
034880060208     c     kFnspe        Chain     Fnspe01l
034890060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
034900060208     c                   Eval      *In33 = *On
034910060208     c                   EndIf
034920051216
034930051216      * Luogo preavviso/avviso danno
034940051216     c                   Eval      kspecod = '006'
034950051216     c     kFnspe        Chain     Fnspe01l
034960051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
034970051216     c                   Eval      *In16 = *On
034980051216     c                   EndIf
034990051216
035000051216      * Luogo progetto di liquidazione
035010051216     c                   Eval      kspecod = '008'
035020051216     c     kFnspe        Chain     Fnspe01l
035030051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
035040051216     c                   Eval      *In17 = *On
035050051216     c                   EndIf
035060060208
035070060208      * Luogo invio orm estero
035080060208     c                   Eval      kspecod = '002'
035090060208     c     kFnspe        Chain     Fnspe01l
035100060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
035110060208     c                   Eval      *In35 = *On
035120060208     c                   EndIf
035130060208
035140060208      * Luogo invio doc.a dogana
035150060208     c                   Eval      kspecod = '200'
035160060208     c     kFnspe        Chain     Fnspe01l
035170060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
035180060208     c                   Eval      *In37 = *On
035190060208     c                   EndIf
035200051216
035210051216      * Decodifico i luoghi caricati
035220051216     c                   ExSr      Sr_Decluoghi
035230051216
035240051216     c                   EndSr
035250051216
035260051216      *------------------------------------------------------------------------*
035270051216      * DECODIFICO I LUOGHI CARICATI
035280051216      *------------------------------------------------------------------------*
035290051216     c     Sr_Decluoghi  BegSr
035300051216
035310051216     c                   Clear                   v4dl001
035320051216     c                   Clear                   v4cl001
035330051216     c                   Clear                   v5dl555
035340051216     c                   Clear                   v5cl555
035350051216     c                   Clear                   v5dl666
035360051216     c                   Clear                   v5cl666
035370051216     c                   Clear                   v6dl005
035380051216     c                   Clear                   v6cl005
035390060208     c                   Clear                   v6dl300
035400060208     c                   Clear                   v6cl300
035410051216     c                   Clear                   v7dl006
035420051216     c                   Clear                   v7cl006
035430051216     c                   Clear                   v7dl008
035440051216     c                   Clear                   v7cl008
035450060208     c                   Clear                   v7dl002
035460060208     c                   Clear                   v7cl002
035470060208     c                   Clear                   v7dl200
035480060208     c                   Clear                   v7cl200
035490051216
035500051216      * Decodifico luogo 001
035510071128     c                   clear                   ds4l
035520051216     c                   Eval      kcod = '4L'
035530051216     c                   Eval      kkey = '001'
035540051216     c     kTabel        Chain     Tabel00f
035550051216     c                   If        %Found(Tabel00f) and
035560051216     c                             tblflg = *Blanks
035570051216     c                   Eval      ds4l = tbluni
035580051216     c                   EndIf
035590051216      * Imposto il campo di descrizione
035600060203     c                   Eval      v4dl001 = '001 - ' + §4ldes
035610071128     c                   eval      $ufi001 = §4lufi
035620051216
035630051216      * Decodifico luogo 555
035640071128     c                   clear                   ds4l
035650051216     c                   Eval      kkey = '555'
035660051216     c     kTabel        Chain     Tabel00f
035670051216     c                   If        %Found(Tabel00f) and
035680051216     c                             tblflg = *Blanks
035690051216     c                   Eval      ds4l = tbluni
035700051216     c                   EndIf
035710051216      * Imposto il campo di descrizione
035720060203     c                   Eval      v5dl555 = '555 - ' + §4ldes
035730071128     c                   eval      $ufi555 = §4lufi
035740051216
035750051216      * Decodifico luogo 666
035760071128     c                   clear                   ds4l
035770051216     c                   Eval      kkey = '666'
035780051216     c     kTabel        Chain     Tabel00f
035790051216     c                   If        %Found(Tabel00f) and
035800051216     c                             tblflg = *Blanks
035810051216     c                   Eval      ds4l = tbluni
035820051216     c                   EndIf
035830051216      * Imposto il campo di descrizione
035840060203     c                   Eval      v5dl666 = '666 - ' + §4ldes
035850071128     c                   eval      $ufi666 = §4lufi
035860051216
035870051216      * Decodifico luogo 005
035880071128     c                   clear                   ds4l
035890051216     c                   Eval      kkey = '005'
035900051216     c     kTabel        Chain     Tabel00f
035910051216     c                   If        %Found(Tabel00f) and
035920051216     c                             tblflg = *Blanks
035930051216     c                   Eval      ds4l = tbluni
035940051216     c                   EndIf
035950051216      * Imposto il campo di descrizione
035960060203     c                   Eval      v6dl005 = '005 - ' + §4ldes
035970071128     c                   eval      $ufi005 = §4lufi
035980060208
035990060208      * Decodifico luogo 300
036000071128     c                   clear                   ds4l
036010060208     c                   Eval      kkey = '300'
036020060208     c     kTabel        Chain     Tabel00f
036030060208     c                   If        %Found(Tabel00f) and
036040060208     c                             tblflg = *Blanks
036050060208     c                   Eval      ds4l = tbluni
036060060208     c                   EndIf
036070060208      * Imposto il campo di descrizione
036080060208     c                   Eval      v6dl300 = '300 - ' + §4ldes
036090071128     c                   eval      $ufi300 = §4lufi
036100051216
036110051216      * Decodifico luogo 006
036120071128     c                   clear                   ds4l
036130051216     c                   Eval      kkey = '006'
036140051216     c     kTabel        Chain     Tabel00f
036150051216     c                   If        %Found(Tabel00f) and
036160051216     c                             tblflg = *Blanks
036170051216     c                   Eval      ds4l = tbluni
036180051216     c                   EndIf
036190051216      * Imposto il campo di descrizione
036200060203     c                   Eval      v7dl006 = '006 - ' + §4ldes
036210071128     c                   eval      $ufi006 = §4lufi
036220051216
036230051216      * Decodifico luogo 008
036240071128     c                   clear                   ds4l
036250051216     c                   Eval      kkey = '008'
036260051216     c     kTabel        Chain     Tabel00f
036270051216     c                   If        %Found(Tabel00f) and
036280051216     c                             tblflg = *Blanks
036290051216     c                   Eval      ds4l = tbluni
036300051216     c                   EndIf
036310051216      * Imposto il campo di descrizione
036320060203     c                   Eval      v7dl008 = '008 - ' + §4ldes
036330071128     c                   eval      $ufi008 = §4lufi
036340060208
036350060208      * Decodifico luogo 002
036360071128     c                   clear                   ds4l
036370060208     c                   Eval      kkey = '002'
036380060208     c     kTabel        Chain     Tabel00f
036390060208     c                   If        %Found(Tabel00f) and
036400060208     c                             tblflg = *Blanks
036410060208     c                   Eval      ds4l = tbluni
036420060208     c                   EndIf
036430060208      * Imposto il campo di descrizione
036440060208     c                   Eval      v7dl002 = '002 - ' + §4ldes
036450071128     c                   eval      $ufi002 = §4lufi
036460060208
036470060208      * Decodifico luogo 200
036480071128     c                   clear                   ds4l
036490060208     c                   Eval      kkey = '200'
036500060208     c     kTabel        Chain     Tabel00f
036510060208     c                   If        %Found(Tabel00f) and
036520060208     c                             tblflg = *Blanks
036530060208     c                   Eval      ds4l = tbluni
036540060208     c                   EndIf
036550060208      * Imposto il campo di descrizione
036560060208     c                   Eval      v7dl200 = '200 - ' + §4ldes
036570071128     c                   eval      $ufi200 = §4lufi
036580051216
036590051216     c                   EndSr
036600051216
036610051216      *------------------------------------------------------------------------*
036620051216      * CARICO LE NOTE PREVISTE
036630051216      *------------------------------------------------------------------------*
036640051216     c     Sr_Carnote    BegSr
036650051216
036660051216     c                   Eval      *In24 = *Off
036670051216     c                   Eval      *In26 = *Off
036680051216     c                   Eval      *In31 = *Off
036690051216     c                   Eval      *In21 = *Off
036700051216     c                   Eval      *In13 = *Off
036710051216     c                   Eval      *In15 = *Off
036720051216     c                   Eval      *In18 = *Off
036730060208     c                   Eval      *In34 = *Off
036740060208     c                   Eval      *In36 = *Off
036750090612     c                   Eval      *In40 = *Off
036760100729     c                   Eval      *In68 = *Off
036770051216
036780091112     c   70
036790091112     corn06              Eval      kntcapl = 'C'
036800100806     c   06
036810091112     cann70              Eval      kntcapl = 'T'
036820051216     c                   Movel(p)  dutkci        kntcnk1
036830091112     c   70
036840091112     corn06              Move      v1cksc        kntcnk1
036850100806     c   06
036860100806     cann70              Move      v1cnrv        kntcnk1
036870051216     c                   Clear                   kntcnk2
036880091119
036890091119      * Nota DC
036900091119     c                   clear                   v2ntcdc
036910091119     c                   Movel     'DC'          kntctnt
036920091119     c     kTfntc        Chain(n)  Tfntc01l
036930091119     c                   If        %Found(Tfntc01l)
036940091119     c                   Eval      v2ntcdc = ntcrnt
036950091119     c                   Eval      savntcdc = ntcrnt
036960091119     c                   EndIf
036970091119
036980140722      * Note 02/03
036990140722     c                   exsr      sr_CarNoteRN
037000051216
037010140722      * Note 05/06/T5/I5
037020140722     c                   exsr      sr_CarNoteRT
037030051216
037040140722      * Note 07/08/T7/I7
037050140722     c                   exsr      sr_CarNoteRL
037060051216
037070051216      * Nota 84
037080051216     c                   Movel     '84'          kntctnt
037090091119     c     kTfntc        Chain(n)  Tfntc01l
037100051216     c                   If        %Found(Tfntc01l)
037110051216     c                   Eval      *In21 = *On
037120051216     c                   EndIf
037130051216
037140051216      * Nota 85
037150051216     c                   Movel     '85'          kntctnt
037160091119     c     kTfntc        Chain(n)  Tfntc01l
037170051216     c                   If        %Found(Tfntc01l)
037180051216     c                   Eval      *In13 = *On
037190051216     c                   EndIf
037200051216
037210051216      * Nota 88
037220051216     c                   Movel     '88'          kntctnt
037230091119     c     kTfntc        Chain(n)  Tfntc01l
037240051216     c                   If        %Found(Tfntc01l)
037250051216     c                   Eval      *In15 = *On
037260051216     c                   EndIf
037270051216
037280051216      * Nota 87
037290051216     c                   Movel     '87'          kntctnt
037300091119     c     kTfntc        Chain(n)  Tfntc01l
037310051216     c                   If        %Found(Tfntc01l)
037320051216     c                   Eval      *In18 = *On
037330051216     c                   EndIf
037340060208
037350060208      * Nota 83
037360060208     c                   Movel     '83'          kntctnt
037370091119     c     kTfntc        Chain(n)  Tfntc01l
037380060208     c                   If        %Found(Tfntc01l)
037390060208     c                   Eval      *In34 = *On
037400060208     c                   EndIf
037410060208
037420060208      * Nota 86
037430060208     c                   Movel     '86'          kntctnt
037440091119     c     kTfntc        Chain(n)  Tfntc01l
037450060208     c                   If        %Found(Tfntc01l)
037460060208     c                   Eval      *In36 = *On
037470060208     c                   EndIf
037480090612
037490090612      * Nota 89
037500090612     c                   Movel     '89'          kntctnt
037510091119     c     kTfntc        Chain(n)  Tfntc01l
037520090612     c                   If        %Found(Tfntc01l)
037530090612     c                   Eval      *In40 = *On
037540090612     c                   EndIf
037550100729
037560100729      * Nota 82
037570100729     c                   Movel     '82'          kntctnt
037580100729     c     kTfntc        Chain(n)  Tfntc01l
037590100729     c                   If        %Found(Tfntc01l)
037600100729     c                   Eval      *In68 = *On
037610100729     c                   EndIf
037620051216
037630051216      * Decodifico le note caricate
037640051216     c                   ExSr      Sr_Decnote
037650051216
037660051216     c                   EndSr
037670140722
037680140722      *------------------------------------------------------------------------*
037690140722      * CARICO LE NOTE 02/03 (NEWS NOMINATIVO)
037700140722      *------------------------------------------------------------------------*
037710140722     c     Sr_CarNoteRN  BegSr
037720140722      *
037730140722      * Nota 02
037740140722     c                   movel     '02'          kNtcTnt
037750140722     c     kTfntc        chain(n)  TFNTC
037760140722     c                   if        %found(TFNTC01L)
037770140722     c                   eval      *in24 = *on
037780140722     c                   endif
037790140722      *
037800140722      * Nota 03
037810140722     c                   Movel     '03'          kNtcTnt
037820140722     c     kTfntc        chain(n)  TFNTC
037830140722     c                   if        %found(TFNTC01L)
037840140722     c                   eval      *in24 = *on
037850140722     c                   endif
037860140722      *
037870140722     c                   EndSr
037880140722
037890140722      *------------------------------------------------------------------------*
037900140722      * CARICO LE NOTE 05/06/T5/I5 (RESPONSABILE TRASPORTI)
037910140722      *------------------------------------------------------------------------*
037920140722     c     Sr_CarNoteRT  BegSr
037930140722      *
037940140722      * Nota 05
037950140722     c                   movel     '05'          kNtcTnt
037960140722     c     kTfntc        chain(n)  TFNTC
037970140722     c                   if        %found(TFNTC01L)
037980140722     c                   eval      *in26 = *On
037990140722     c                   endif
038000140722      *
038010140722      * Nota 06
038020140722     c                   movel     '06'          kNtcTnt
038030140722     c     kTfntc        chain(n)  TFNTC
038040140722     c                   if        %found(TFNTC01L)
038050140722     c                   eval      *in26 = *On
038060140722     c                   endif
038070140722      *
038080140722      * Nota T5
038090140722     c                   movel     'T5'          kNtcTnt
038100140722     c     kTfntc        chain(n)  TFNTC
038110140722     c                   if        %found(TFNTC01L)
038120140722     c                   eval      *in26 = *On
038130140722     c                   endif
038140140722      *
038150140722      * Nota I5
038160140722     c                   movel     'I5'          kNtcTnt
038170140722     c     kTfntc        chain(n)  TFNTC
038180140722     c                   if        %found(TFNTC01L)
038190140722     c                   eval      *in26 = *On
038200140722     c                   endif
038210140722      *
038220140722     c                   EndSr
038230140722
038240140722      *------------------------------------------------------------------------*
038250140722      * CARICO LE NOTE 07/08/T7/I7 (RESPONSABILE LOGISTICA)
038260140722      *------------------------------------------------------------------------*
038270140722     c     Sr_CarNoteRL  BegSr
038280140722      *
038290140722      * Nota 07
038300140722     c                   movel     '07'          kNtcTnt
038310140722     c     kTfntc        chain(n)  TFNTC
038320140722     c                   if        %found(TFNTC01L)
038330140722     c                   eval      *in31 = *On
038340140722     c                   endif
038350140722      *
038360140722      * Nota 08
038370140722     c                   movel     '08'          kNtcTnt
038380140722     c     kTfntc        chain(n)  TFNTC
038390140722     c                   if        %found(TFNTC01L)
038400140722     c                   eval      *in31 = *On
038410140722     c                   endif
038420140722      *
038430140722      * Nota T7
038440140722     c                   movel     'T7'          kNtcTnt
038450140722     c     kTfntc        chain(n)  TFNTC
038460140722     c                   if        %found(TFNTC01L)
038470140722     c                   eval      *in31 = *On
038480140722     c                   endif
038490140722      *
038500140722      * Nota I5
038510140722     c                   movel     'I7'          kNtcTnt
038520140722     c     kTfntc        chain(n)  TFNTC
038530140722     c                   if        %found(TFNTC01L)
038540140722     c                   eval      *in31 = *On
038550140722     c                   endif
038560140722      *
038570140722     c                   EndSr
038580051216
038590051216      *------------------------------------------------------------------------*
038600051216      * DECODIFICO LE NOTE CARICATE
038610051216      *------------------------------------------------------------------------*
038620051216     c     Sr_Decnote    BegSr
038630051216
038640051216     c                   Clear                   v4dnt84
038650051216     c                   Clear                   v4cnt84
038660051216     c                   Clear                   v5dnt85
038670051216     c                   Clear                   v5cnt85
038680090612     c                   Clear                   v5dnt89
038690090612     c                   Clear                   v5cnt89
038700100729     c                   Clear                   v5cnt82
038710051216     c                   Clear                   v6dnt88
038720051216     c                   Clear                   v6cnt88
038730051216     c                   Clear                   v7dnt87
038740051216     c                   Clear                   v7cnt87
038750060208     c                   Clear                   v7dnt83
038760060208     c                   Clear                   v7cnt83
038770060208     c                   Clear                   v7dnt86
038780060208     c                   Clear                   v7cnt86
038790140722
038800140722     c                   Eval      kcod = '1T'
038810051216
038820051216      * Decodifico nota 84
038830051216     c                   Eval      kkey = '84'
038840051216     c     kTabel        Chain     Tabel00f
038850051216     c                   If        %Found(Tabel00f) and
038860051216     c                             tblflg = *Blanks
038870051216     c                   Eval      ds1t = tbluni
038880051216     c                   EndIf
038890051216      * Imposto il campo di descrizione
038900060203     c                   Eval      v4dnt84 = ' 84 - ' + §1tdes
038910051216
038920051216      * Decodifico nota 85
038930051216     c                   Eval      kkey = '85'
038940051216     c     kTabel        Chain     Tabel00f
038950051216     c                   If        %Found(Tabel00f) and
038960051216     c                             tblflg = *Blanks
038970051216     c                   Eval      ds1t = tbluni
038980051216     c                   EndIf
038990051216      * Imposto il campo di descrizione
039000060203     c                   Eval      v5dnt85 = ' 85 - ' + §1tdes
039010090612
039020090612      * Decodifico nota 89
039030090612     c                   Eval      kkey = '89'
039040090612     c     kTabel        Chain     Tabel00f
039050090612     c                   If        %Found(Tabel00f) and
039060090612     c                             tblflg = *Blanks
039070090612     c                   Eval      ds1t = tbluni
039080090612     c                   EndIf
039090090612      * Imposto il campo di descrizione
039100090612     c                   Eval      v5dnt89 = ' 89 - ' + §1tdes
039110100729
039120100729      * Decodifico nota 82
039130100729     c                   Eval      kkey = '82'
039140100729     c     kTabel        Chain     Tabel00f
039150100729     c                   If        %Found(Tabel00f) and
039160100729     c                             tblflg = *Blanks
039170100729     c                   Eval      ds1t = tbluni
039180100729     c                   EndIf
039190100729      * Imposto il campo di descrizione
039200100729     c                   Eval      v5dnt82 = ' 82 - ' + §1tdes
039210051216
039220051216      * Decodifico nota 88
039230051216     c                   Eval      kkey = '88'
039240051216     c     kTabel        Chain     Tabel00f
039250051216     c                   If        %Found(Tabel00f) and
039260051216     c                             tblflg = *Blanks
039270051216     c                   Eval      ds1t = tbluni
039280051216     c                   EndIf
039290051216      * Imposto il campo di descrizione
039300060203     c                   Eval      v6dnt88 = ' 88 - ' + §1tdes
039310051216
039320051216      * Decodifico nota 87
039330051216     c                   Eval      kkey = '87'
039340051216     c     kTabel        Chain     Tabel00f
039350051216     c                   If        %Found(Tabel00f) and
039360051216     c                             tblflg = *Blanks
039370051216     c                   Eval      ds1t = tbluni
039380051216     c                   EndIf
039390051216      * Imposto il campo di descrizione
039400060203     c                   Eval      v7dnt87 = ' 87 - ' + §1tdes
039410060208
039420060208      * Decodifico nota 83
039430060208     c                   Eval      kkey = '83'
039440060208     c     kTabel        Chain     Tabel00f
039450060208     c                   If        %Found(Tabel00f) and
039460060208     c                             tblflg = *Blanks
039470060208     c                   Eval      ds1t = tbluni
039480060208     c                   EndIf
039490060208      * Imposto il campo di descrizione
039500060208     c                   Eval      v7dnt83 = ' 83 - ' + §1tdes
039510060208
039520060208      * Decodifico nota 86
039530060208     c                   Eval      kkey = '86'
039540060208     c     kTabel        Chain     Tabel00f
039550060208     c                   If        %Found(Tabel00f) and
039560060208     c                             tblflg = *Blanks
039570060208     c                   Eval      ds1t = tbluni
039580060208     c                   EndIf
039590060208      * Imposto il campo di descrizione
039600060208     c                   Eval      v7dnt86 = ' 86 - ' + §1tdes
039610051216
039620051216     c                   EndSr
039630051216
039640051216      *------------------------------------------------------------------------*
039650051216      *  Imposto descrizione tasti funzione
039660051216      *------------------------------------------------------------------------*
039670051216     c     Sr_Tastifun   BegSr
039680051216
039690051216      * Conta i caratteri riempiti dalle RigaTf1
039700051216     c                   Clear                   $z
039710051216      * Conta i caratteri riempiti dalle RigaTf2
039720051216     c                   Clear                   $k
039730051216      * Conta i caratteri riempiti dalle RigaTf3
039740051216     c                   Clear                   $j
039750051216      * Conta la posizione del campone da cui partire per impostare
039760051216      * la descrizione del tasto funzione
039770051216     c                   Clear                   $y
039780051216
039790051216      * Stringhe che contengono le descrizioni dei tasti funzione
039800051216     c                   Reset                   rigatf1
039810051216     c                   Reset                   rigatf2
039820051216     c                   Reset                   rigatf3
039830060203
039840060731      * F4 - Abilitazioni
039850120117       //?Se no angrafica provvisoria e no immissione
039860111007     c                   eval      *in78 = *off
039870110907     c                   if        not *in01 and not *in06
039880111117     c                   eval      *in78 = *on
039890060731     c                   reset                   $tf
039900060731     c                   movea     tf04          $tf
039910060731     c                   exsr      rie_$tf
039920060731     c                   endif
039930100407
039940100524       //?F22-Richiesta Contatto
039950100806       //?Se anagrafica clienti abilito F22=Richiesta Contatto
039960100407       //?e se utente di filiale
039970100419       //?e se pgm da menù
039980100507       //?e se NON immissione
039990100806     c                   IF        not *in06 and
040000100407     c                             not *in07 and
040010100419     c                             not *in09 and
040020100507     c                             not *in01 and
040030100419     c                             not $pgmrich
040040100407     c                   reset                   $tf
040050100524     c                   Movea     tf22          $tf
040060100407     c                   ExSr      Rie_$tf
040070100407     c                   ENDIF
040080100507
040090100507       //?F5 - Attività
040100120124       //?se utente di sede solo in interrogazione
040110120124       //?se non sono in anagrafica provvisoria,
040120100507       //?se non è convalida offerta e se ci sono attività sul cliente
040130100507       //?e non sono in immissione
040140100507     c                   eval      *in67 = *off
040150100806     c                   IF        not *in06 and
040160100507     c                             not *in07 and
040170120124     c                             not *in01
040180100507      * cerco se ci sono attività sul cliente
040190110516     c     V1Cksc        setll     TIATC07L
040200110516     c                   IF        %equal(TIATC07L)
040210100507     c                   eval      *in67 = *on
040220120124      * se utente di sede e NON è interrogazione spengo F5
040230120124     c                   IF        *in09 and not *in05
040240120124     c                   eval      *in67 = *off
040250120124     c                   ENDIF
040260120124     c                   IF        *in67
040270100507     c                   reset                   $tf
040280100507     c                   movea     tf05          $tf
040290100507     c                   exsr      rie_$tf
040300120124     c                   ENDIF
040310100507     c                   ENDIF
040320100507     c                   ENDIF
040330100507
040340100507      * F15 - Codici collegati
040350100507     c                   If        Not *In01 and Not *In06
040360100507     c                   Reset                   $tf
040370100507     c                   Movea     tf15          $tf
040380100507     c                   ExSr      Rie_$tf
040390100507     c                   EndIf
040400100507
040410060731      * F14 - Info Commerciali
040420100806     c                   If        Not *In06
040430060731     c                   Reset                   $tf
040440060731     c                   Movea     tf14          $tf
040450060731     c                   ExSr      Rie_$tf
040460090914     c                   endif
040470060526      * F19 - Variazioni
040480060526     c                   If        Not *In01 and Not *In06
040490060526     c                   Reset                   $tf
040500060526     c                   Movea     tf19          $tf
040510060526     c                   ExSr      Rie_$tf
040520060526     c                   endif
040530100407      * F20 - Interrogazione clienti potenziali
040540100407     c                   If        *In29 = *On and not *in65 and
040550100806     c                             not *in06
040560100407     c                   Reset                   $tf
040570100407     c                   Movea     tf20          $tf
040580100407     c                   ExSr      Rie_$tf
040590100407     c                   EndIf
040600101213
040610101213       //?F17-Dati Fatturazione
040620110322     c                   IF        not *in06
040630101213     c                   reset                   $tf
040640101213     c                   Movea     tf17          $tf
040650101213     c                   ExSr      Rie_$tf
040660101213     c                   ENDIF
040670110223
040680110223       //?F21-Blocco cliente
040690110223       //?Abilito se anagrafica clienti
040700110223       //?se utente di filiale
040710110223       //?se pgm da menù
040720110223       //?se NON immissione
040730110223       //?se codice non bloccato
040740110223     c                   IF        not *in06 and
040750110223     c                             not *in07 and
040760110223     c                             not *in09 and
040770110223     c                             not *in01 and
040780110223     c                             v2cabl = *blanks and
040790110223     c                             not $pgmrich
040800110223     c                   reset                   $tf
040810110223     c                   Movea     tf21          $tf
040820110223     c                   ExSr      Rie_$tf
040830110223     c                   ENDIF
040840110223
040850060203      * F12 - Ritorno
040860060206     c                   If        (Not *In29 and *In06) or
040870060206     c                             (Not *In29 and *In07) or
040880060206     c                             (Not *In06 and Not *In07)
040890060203     c                   Reset                   $tf
040900060203     c                   Movea     tf12          $tf
040910060203     c                   ExSr      Rie_$tf
040920060206     c                   EndIf
040930051216
040940051216      * Pulisco la stringa
040950051216     c                   Eval      $h = 1
040960051216     c                   For       $h by 1 to 62
040970051216     c                   If        rigatf1($h) = '#'
040980051216     c                   Clear                   rigatf1($h)
040990051216     c                   EndIf
041000051216     c                   EndFor
041010051216     c                   Eval      $h = 1
041020051216     c                   For       $h by 1 to 62
041030051216     c                   If        rigatf2($h) = '#'
041040051216     c                   Clear                   rigatf2($h)
041050051216     c                   EndIf
041060051216     c                   EndFor
041070051216     c                   Eval      $h = 1
041080051216     c                   For       $h by 1 to 62
041090051216     c                   If        rigatf3($h) = '#'
041100051216     c                   Clear                   rigatf3($h)
041110051216     c                   EndIf
041120051216     c                   EndFor
041130051216
041140051216      * Imposto la descrizione del tasto funzione F24 e segnalo
041150051216      * quante righe ho riempito e quale devo emettere
041160051216     c                   Select
041170051216
041180051216     c                   When      $k <> *Zeros and $y <> *Zeros
041190051216     c                   Eval      $loop = 3
041200051216     c                   Eval      vzfd02 = cf2413
041210051216
041220051216     c                   When      $k <> *Zeros and $y = *Zeros
041230051216     c                   Eval      $loop = 2
041240051216     c                   Eval      vzfd02 = cf2412
041250051216
041260051216     c                   Other
041270051216
041280051216     c                   Eval      $loop = 1
041290051216     c                   Clear                   vzfd02
041300051216     c                   EndSl
041310051216
041320051216      * Prima riga di tasti funzione
041330051216     c                   Movea     rigatf1       vzfd01
041340051216     c                   Eval      $rig = 1
041350051216
041360051216     c                   EndSr
041370051216
041380051216      *------------------------------------------------------------------------*
041390051216      *  IMPOSTO LE STRINGHE CON I CAMPI FUNZIONE COMPATTATI
041400051216      *------------------------------------------------------------------------*
041410051216     c     Rie_$tf       BegSr
041420051216      *
041430051216     c                   Eval      $x = 1
041440051216     c     '#'           Lookup    $tf($x)                                30
041450051216     c                   Add       $x            $z
041460051216     c                   If        $Z <= 63
041470051216     c                   Eval      $j = $Z - $x + 1
041480051216     c                   Movea     $tf           rigatf1($j)
041490051216     c                   Else
041500051216     c                   Add       $x            $k
041510051216     c                   If        $k <= 63
041520051216     c                   Eval      $j = $K - $x + 1
041530051216     c                   Movea     $tf           rigatf2($j)
041540051216     c                   Else
041550051216     c                   Add       $x            $y
041560051216     c                   If        $y <= 63
041570051216     c                   Eval      $j = $y - $x + 1
041580051216     c                   Movea     $tf           rigatf3($j)
041590051216     c                   EndIf
041600051216     c                   EndIf
041610051216     c                   EndIf
041620051216
041630051216     c                   EndSr
041640051216
041650051108      *------------------------------------------------------------------------*
041660051130      * CONTROLLO SECONDA VIDEATA - DATI ANAGRAFICI/BLOCCO
041670051108      *------------------------------------------------------------------------*
041680051108     c     Sr_Ctrd02     BegSr
041690051117
041700051117      * Pulisco i campi del posizionamento cursore
041710051117     c                   Clear                   h2riga
041720051117     c                   Clear                   h2colo
041730051125
041740051118      * Imposto gli ulti 4 byte del codice cliente
041750051118     c                   Move      v1cksc        wksc04
041760051117
041770051117      * RAGIONE SOCIALE
041780051117      * --> Obbligatoria
041790051117     c                   If        v2crag = *Blanks
041800051117     c                   Eval      *In28 = *On
041810051130     c                   Eval      h2riga = 07
041820051117     c                   Eval      h2colo = 28
041830051117     c                   Eval      v2cmsg = msg(11)
041840051117     c                   Leavesr
041850051117     c                   EndIf
041860051117
041870051117      * CODICE POTENZIALE
041880051125     c                   ExSr      Sr_Ctrcpo
041890051125     c   28              Leavesr
041900051118
041910051118      * --> Caratteri non validi
041920051212     c                   Eval      xx = 1
041930051212     c                   For       xx by 1 to 10
041940051212     c                   If        car(xx) <> *Blanks
041950051212     c                   Eval      wpos = %scan(car(xx):v2crag)
041960051212     c                   If        wpos > 0
041970051212     c                   Eval      *In28 = *On
041980051212     c                   Eval      h2riga = 07
041990051212     c                   Eval      h2colo = 28
042000051212     c                   Eval      v2cmsg = msg(32)
042010051212     c                   Leavesr
042020051212     c                   EndIf
042030051212     c                   EndIf
042040051212     c                   EndFor
042050051118
042060051117      * INDIRIZZO
042070051118      * --> Controllo se cliente
042080051118     c                   If        v1ckcc = 151
042090051118     c                   Clear                   fnlv13ds
042100051118     c                   Eval      i14rsc = v2crag
042110051118     c                   Eval      i14ind = v2cvia
042120051118     c                   Eval      e14cap = v2ccae
042130051118     c                   Eval      e14loc = v2ccit
042140051118     c                   Eval      e14prv = v2cprv
042150051118     c                   Eval      e14nar = v2csta
042160051118     c                   ExSr      Sr_Ctrind
042170051118     c                   Eval      v2ccae = e14cap
042180051118     c                   Eval      v2ccit = e14loc
042190051118     c                   Eval      v2cprv = e14prv
042200051118     c                   Eval      v2csta = e14nar
042210051118     c   28              Leavesr
042220051118     c                   EndIf
042230051117
042240051117      * TELEFONO
042250051117      * --> Controllo (forzabile)
042260051124      *     Solo se variato il campo a video quindi al primo errore se non
042270051124      *     vario il campo non controllo più
042280051118     c                   If        v2ctel <> wtel and v2ctel <> *Blanks
042290051118     c                   Clear                   trul42ds
042300051118     c                   Eval      d42fax = v2ctel
042310051118     c                   Call      'TRUL42R'
042320051118     c                   Parm                    trul42ds
042330051118     c                   If        d42err = '1'
042340051118     c                   Eval      *In28 = *On
042350051202     c                   Eval      h2riga = 11
042360051118     c                   Eval      h2colo = 28
042370051118     c                   Eval      v2cmsg = d42msg
042380051118     c                   Leavesr
042390051118     c                   Eval      wtel = v2ctel
042400051117     c                   EndIf
042410051118     c                   EndIf
042420051118
042430051117      * FAX
042440051125      * --> Controllo (forzabile)
042450051125      *     Solo se variato il campo a video quindi al primo errore se non
042460051125      *     vario il campo non controllo più
042470051118     c                   If        v2ctlf <> wtlf and v2ctlf <> *Blanks
042480051118     c                   Clear                   trul42ds
042490051118     c                   Eval      d42fax = v2ctlf
042500051118     c                   Call      'TRUL42R'
042510051118     c                   Parm                    trul42ds
042520051118     c                   If        d42err = '1'
042530051118     c                   Eval      *In28 = *On
042540051202     c                   Eval      h2riga = 11
042550051118     c                   Eval      h2colo = 52
042560051118     c                   Eval      v2cmsg = d42msg
042570051118     c                   Leavesr
042580051118     c                   Eval      wtlf = v2ctlf
042590051118     c                   EndIf
042600051118     c                   EndIf
042610051118
042620051118      * CODICE FISCALE
042630051118     c                   ExSr      Sr_Ctrcdf
042640051118     c   28              Leavesr
042650051118
042660051118      * PARTITA IVA + STATO
042670051118     c                   ExSr      Sr_Ctriva
042680051118     c   28              Leavesr
042690051122
042700051122      * BLOCCO CLIENTE
042710051122     c                   ExSr      Sr_Ctrblc
042720051122     c   28              Leavesr
042730090616      * sollecito
042740090616     c                   ExSr      Sr_Ctrsol
042750090617     c   28              Leavesr
042760051108
042770051108     c                   EndSr
042780051130
042790051130      *------------------------------------------------------------------------*
042800051130      * CONTROLLO TERZA VIDEATA - DATI COMMERCIALI
042810051130      *------------------------------------------------------------------------*
042820051130     c     Sr_Ctrd03     BegSr
042830051130
042840051130      * Pulisco i campi del posizionamento cursore
042850051130     c                   Clear                   h3riga
042860051130     c                   Clear                   h3colo
042870051130
042880051130      * CODICE COMMERCIALE
042890051130     c                   ExSr      Sr_Ctrage
042900051130     c   28              Leavesr
042910051130
042920051130      * CODICE IMPORTANZA CLIENTE
042930051130     c                   ExSr      Sr_Ctrclv
042940051130     c   28              Leavesr
042950051130
042960051130      * CODICE ISTAT
042970051130     c                   ExSr      Sr_Ctritc
042980051130     c   28              Leavesr
042990051207
043000140722      * NOTE   02/03 - NEWS NOMINATIVO
043010140722     c                   eval      wRGR = 'RN'
043020140722     c                   exsr      sr_CtrNote
043030140722     c   28
043040140722     cor 90              leavesr
043050051207
043060140722      * NOTE   05/06/T5/I5 - RESPONSABILE TRASPORTI
043070140722     c                   eval      wRGR = 'RT'
043080140722     c                   exsr      sr_CtrNote
043090140722     c   28
043100140722     cor 90              leavesr
043110051207
043120140722      * NOTE   07/08/T7/I7 - RESPONSABILE LOGISTICA
043130140722     c                   eval      wRGR = 'RL'
043140140722     c                   exsr      sr_CtrNote
043150140722     c   28
043160140722     cor 90              leavesr
043170051130
043180051130     c                   EndSr
043190051108
043200051108      *------------------------------------------------------------------------*
043210051130      * CONTROLLO QUARTA VIDEATA - DATI FATTURAZIONE
043220051108      *------------------------------------------------------------------------*
043230051130     c     Sr_Ctrd04     BegSr
043240051125
043250051125      * Pulisco i campi del posizionamento cursore
043260051130     c                   Clear                   h4riga
043270051130     c                   Clear                   h4colo
043280051125
043290051125      * CODICE SOTTOCONTO FATTURAZIONE
043300051125     c                   ExSr      Sr_Ctrscf
043310100127     c   90
043320100127     cor 28              Leavesr
043330051128
043340051128      * FLAG FATTURA UNIFICATA
043350051128     c                   ExSr      Sr_Ctruni
043360051128     c   28              Leavesr
043370051125
043380051125      * TIPO FATTURA
043390051125     c                   ExSr      Sr_Ctrtft
043400051125     c   28              Leavesr
043410051125
043420051125      * FREQUENZA FATTURA
043430051125     c                   ExSr      Sr_Ctrfft
043440051125     c   28              Leavesr
043450051125
043460051125      * TIPO DATA FATTURA
043470051125     c                   ExSr      Sr_Ctrtdf
043480051125     c   28              Leavesr
043490051128
043500051128      * SPESE INVIO FATTURA
043510051128     c                   ExSr      Sr_Ctrsin
043520051128     c   28              Leavesr
043530100728
043540100728      * CODICE PAGAMENTO FATTURA
043550100728     c                   ExSr      Sr_Ctrcdp
043560100728     c   28              Leavesr
043570100728
043580100728      * BANCA APPOGGIO
043590100728     c                   ExSr      Sr_Ctrbna
043600100728     c   28              Leavesr
043610100728
043620100728      * ABI/CAB
043630100728     c                   ExSr      Sr_Ctrabicabf
043640100728     c   28              Leavesr
043650051128
043660051206      * LUOGO 001 - INVIO FATTURA
043670051212      * Se non sono in visite/offerte
043680051128     c                   If        Not *In06
043690091112      * o se trattativa da cliente
043700091112     c                             or *in70
043710051128     c                   ExSr      Sr_Ctrl001
043720051128     c   28              Leavesr
043730051128     c                   EndIf
043740051129
043750051206      * NOTA   84 - INVIO FATTURA
043760051129     c                   ExSr      Sr_Ctrnt84
043770051129     c   28              Leavesr
043780051108
043790051108     c                   EndSr
043800051216
043810051108      *------------------------------------------------------------------------*
043820051130      * CONTROLLO QUINTA VIDEATA - CONDIZIONI PAGAMENTO/PAG. CONTRASSEGNI
043830051108      *------------------------------------------------------------------------*
043840051130     c     Sr_Ctrd05     BegSr
043850051129
043860051129      * Pulisco i campi del posizionamento cursore
043870051130     c                   Clear                   h5riga
043880051130     c                   Clear                   h5colo
043890100728     c                   clear                   wbanca
043900100728     c                   clear                   wagenzia
043910100728
043920100728      * BLOCCO PAGAMENTI
043930130322      * Se impostato SI dall'utente
043940130322     c                   IF        not *in81 and V5Cblp = 'SI'
043950130325     c                   eval      wbloc = 'B'
043960130322     c                   ENDIF
043970130322      * Se impostato NO dall'utente
043980130322     c                   IF        not *in81 and V5Cblp = 'NO'
043990130325     c                   eval      wbloc = '0'
044000130322     c                   ENDIF
044010150424      /free
044020150424       //?Come prima cosa devo avvisare se anche solo 1 dei 3 pagamenti
044030150424       //?non è un pagamento tramite Bonifico
044040150424         exsr CtrPagamenti;
044050150424         IF  *in28;
044060150424           leavesr;
044070150424         ENDIF;
044080150424      /end-free
044090051108
044100051130      * CODICE PAGAMENTO CONTRASSEGNI
044110051130     c                   ExSr      Sr_Ctrfpc
044120060215     c                   If        *In28
044130060215     c                   Eval      *In22 = savin22
044140060215     c                   Eval      *In23 = savin23
044150060215     c                   Leavesr
044160060215     c                   EndIf
044170100729     c                   IF        *in90 and $Win
044180100729     c                   leavesr
044190100729     c                   ENDIF
044200150424
044210150424      /free
044220150427       //?Memorizzo se pagamento tramite Bonifico
044230150424         IF  §4Ucod = 'B';
044240150424           BonificoCA = *on;
044250150424         ELSE;
044260150424           BonificoCA = *off;
044270150424         ENDIF;
044280150427       //?Memorizzo se pagamento Estero
044290150427         IF  §4Utip = 'E';
044300150427           PagEsteroCA = *on;
044310150427         ELSE;
044320150427           PagEsteroCA = *off;
044330150427         ENDIF;
044340150427       //?Memorizzo il codice pagamento
044350150427         CodPagCA = V5Cfpc;
044360150424      /end-free
044370150424
044380080115      * CODICE IBAN
044390080115      * solo se richieste obbligatorie le coordinate
044400080115     c                   if        not *in22 and not *in23
044410100729     c                   eval      wiban = v5ciban
044420080115     c                   exsr      sr_ctriban
044430100729     c                   if        *in28
044440100729     c                   eval      h5riga = 10
044450100804     c                   eval      h5colo = 29
044460100729     c                   leavesr
044470100729     c                   endif
044480100729     c                   eval      v5cbab = wdesbanca
044490080115     c                   endif
044500100729
044510100729      /free
044520100729       //?PAGAMENTO DANNI
044530100729         exsr CtrPagDN;
044540100729         IF  *in28;
044550100729           *in71 = savin71;
044560100729           *in72 = savin72;
044570100729           leavesr;
044580100729         ENDIF;
044590150427       //?Memorizzo se pagamento tramite Bonifico
044600150424         IF  §4Ucod = 'B';
044610150424           BonificoDN = *on;
044620150424         ELSE;
044630150424           BonificoDN = *off;
044640150424         ENDIF;
044650150427       //?Memorizzo se pagamento Estero
044660150427         IF  §4Utip = 'E';
044670150427           PagEsteroDN = *on;
044680150427         ELSE;
044690150427           PagEsteroDN = *off;
044700150427         ENDIF;
044710150427       //?Memorizzo il codice pagamento
044720150427         CodPagDN = V5tpdn;
044730100729       //?se window per invio dati alla sede ritorno ad emettere la videata
044740100729       //?senza andare avanti con i controlli
044750100729         IF  $windn and *in90;
044760100729           leavesr;
044770100729         ENDIF;
044780100729       //?controllo le coordinate bancarie
044790100730         IF  not *in71 and not *in72 and d§PAGctr = 'SI';
044800100729           wiban = v5ibandn;
044810100729           exsr sr_ctriban;
044820100729           IF  *in28;
044830100729             h5riga = 13;
044840100804             h5colo = 29;
044850100729             leavesr;
044860100729           ENDIF;
044870100729           v5babdn = wDesBanca;
044880100729         ENDIF;
044890100730       //?se pagamento tramite bonifico deve essere inserito la nota 82-mail
044900100730       //?bonifici danni
044910100730         IF §4ucod = 'B' and not *in68 and v5cnt82 = *blanks;
044920100730           *in28 = *on;
044930100730           v5cmsg = msg(86);
044940100730           h5riga = 21;
044950100730           h5colo = 71;
044960100730           leavesr;
044970100730         ENDIF;
044980100729
044990100729       //?PAGAMENTO NOTE ACCREDITO
045000100729         exsr CtrPagNA;
045010100729         IF  *in28;
045020100729           *in73 = savin73;
045030100729           *in74 = savin74;
045040100729           leavesr;
045050100729         ENDIF;
045060150427       //?Memorizzo se pagamento tramite Bonifico
045070150424         IF  §4Ucod = 'B';
045080150424           BonificoNA = *on;
045090150424         ELSE;
045100150424           BonificoNA = *off;
045110150424         ENDIF;
045120150427       //?Memorizzo se pagamento Estero
045130150427         IF  §4Utip = 'E';
045140150427           PagEsteroNA = *on;
045150150427         ELSE;
045160150427           PagEsteroNA = *off;
045170150427         ENDIF;
045180150427       //?Memorizzo il codice pagamento
045190150427         CodPagNA = V5tpna;
045200150424      /end-free
045210100729       //?se window per invio dati alla sede ritorno ad emettere la videata
045220100729       //?senza andare avanti con i controlli
045230100729         IF  $winna and *in90;
045240100729           leavesr;
045250100729         ENDIF;
045260100729       //?controllo le coordinate bancarie
045270100804         IF  not *in73 and not *in74 and d§PAGctr = 'SI';
045280100729           wiban = v5ibanna;
045290100729           exsr sr_ctriban;
045300100729           IF  *in28;
045310100729             h5riga = 16;
045320100804             h5colo = 29;
045330100729             leavesr;
045340100729           ENDIF;
045350100729           v5babna = wDesBanca;
045360100729         ENDIF;
045370100729      /end-free
045380051206
045390051212      * Se non sono in visite/offerte
045400051212if  1c                   If        Not *In06
045410091112      * o se trattativa da cliente
045420091112     c                             or *in70
045430051206      * LUOGO 555 - INTESTAZIONE PAGAMENTO C/ASSEGNI
045440051206     c                   ExSr      Sr_Ctrl555
045450051206     c   28              Leavesr
045460051206
045470051206      * LUOGO 666 - LUOGO INVIO BONIFICI C/ASSEGNI
045480051206     c                   ExSr      Sr_Ctrl666
045490051206     c   28              Leavesr
045500051212    1c                   EndIf
045510051206
045520051206      * NOTA   85 - E-MAIL BONIFICI C/ASSEGNI
045530051206     c                   ExSr      Sr_Ctrnt85
045540051206     c   28              Leavesr
045550090616
045560090616      * NOTA   89 - E-MAIL RESPONSABILE AMMINISTRATIVO
045570090616     c                   ExSr      Sr_Ctrnt89
045580090616     c   28              Leavesr
045590100729
045600100729      * NOTA   82 - E-MAIL BONIFICO DANNI
045610150424     c                   ExSr      Sr_Ctrnt82
045620100730     c   90              goto      emi05
045630100729     c   28              Leavesr
045640051130
045650051108     c                   EndSr
045660051108
045670051108      *------------------------------------------------------------------------*
045680051130      * CONTROLLO SESTA VIDEATA - GESTIONE GIACENZE
045690051108      *------------------------------------------------------------------------*
045700051130     c     Sr_Ctrd06     BegSr
045710051206
045720051206      * Pulisco i campi del posizionamento cursore
045730051206     c                   Clear                   h6riga
045740051206     c                   Clear                   h6colo
045750060216
045760060216      * LUOGO 005 - SPEDIZIONE GIACENZA
045770060216     c                   If        Not *In06
045780091112     c                             or *in70
045790060216     c                   ExSr      Sr_Ctrl005
045800060216     c   28              Leavesr
045810060216     c                   EndIf
045820060216
045830060216      * NOTA   88 - E-MAIL GIACENZE
045840060216     c                   ExSr      Sr_Ctrnt88
045850060216     c   28              Leavesr
045860060216
045870060216      * LUOGO 300 - MERCE RESA
045880060216     c                   If        Not *In06
045890091112     c                             or *in70
045900060216     c                   ExSr      Sr_Ctrl300
045910060216     c   28              Leavesr
045920060216     c                   EndIf
045930051206
045940051206      * TIPO COMUNICAZIONE AL MITTENTE
045950051206     c                   ExSr      Sr_Ctrtcm
045960051206     c   28              Leavesr
045970051206
045980051206      * TIPO COMUNICAZIONE FINE GIACENZA
045990051206     c                   ExSr      Sr_Ctrtcf
046000051206     c   28              Leavesr
046010051206
046020051206      * APERTURA AUTOMATICA GIACENZA
046030051206     c                   ExSr      Sr_Ctrapg
046040051206     c   28              Leavesr
046050051206
046060051206      * DISPOSIZIONE MITTENTE IN ARRIVO
046070051206     c                   ExSr      Sr_Ctrdmg
046080051206     c   28              Leavesr
046090060801
046100060801      * CHIUSURA AUTOMATICA GIACENZA
046110060801     c                   ExSr      Sr_Ctrcag
046120060801     c   28              Leavesr
046130051108
046140051108     c                   EndSr
046150051108
046160051108      *------------------------------------------------------------------------*
046170051130      * CONTROLLO SETTIMA VIDEATA - GESTIONE DANNI/TASSAZIONE P.A./STOP
046180051108      *------------------------------------------------------------------------*
046190051130     c     Sr_Ctrd07     BegSr
046200051213
046210051213      * Pulisco i campi del posizionamento cursore
046220051213     c                   Clear                   h7riga
046230051213     c                   Clear                   h7colo
046240150728
046250150728      /free
046260150728       //?Come prima cosa devo avvisare se manca la mail per invio
046270150728       //?progetto di liquidazione
046280150728       //?NON lo faccio se anagrafica provvisoria
046290150728         IF  not *in06;
046300150728           exsr CtrMailDanni;
046310150728           IF  *in28;
046320150728             leavesr;
046330150728           ENDIF;
046340150728         ENDIF;
046350150728      /end-free
046360051213
046370051213      * LUOGO 006 - LUOGO PREAVVISO/AVVISO DANNO
046380051213     c                   If        Not *In06
046390091112     c                             or *in70
046400051213     c                   ExSr      Sr_Ctrl006
046410051213     c   28              Leavesr
046420051213     c                   EndIf
046430051213
046440051213      * NOTA   87 - E-MAIL DANNI
046450051213     c                   ExSr      Sr_Ctrnt87
046460051213     c   28              Leavesr
046470051213
046480051213      * LUOGO 008 - PROGETTO DI LIQUIDAZIONE
046490051213     c                   If        Not *In06
046500091112     c                             or *in70
046510051213     c                   ExSr      Sr_Ctrl008
046520051213     c   28              Leavesr
046530051213     c                   EndIf
046540051213
046550051213      * NR. STOP RITIRO
046560051213     c                   ExSr      Sr_Ctrstp
046570051213     c   28              Leavesr
046580060208
046590060208      * NOTA   83 - MAIL ORM ESTERO
046600060208     c                   ExSr      Sr_Ctrnt83
046610060208     c   28              Leavesr
046620060208
046630060208      * LUOGO 002 - INVIO ORM ESTERO
046640060208     c                   If        Not *In06
046650091112     c                             or *in70
046660060208     c                   ExSr      Sr_Ctrl002
046670060208     c   28              Leavesr
046680060208     c                   EndIf
046690060208
046700060208      * NOTA   86 - MANIFEST
046710060208     c                   ExSr      Sr_Ctrnt86
046720060208     c   28              Leavesr
046730060208
046740060208      * LUOGO 200 - DOC. DOGANA
046750060208     c                   If        Not *In06
046760091112     c                             or *in70
046770060208     c                   ExSr      Sr_Ctrl200
046780060208     c   28              Leavesr
046790060208     c                   EndIf
046800051108
046810051108     c                   EndSr
046820051216
046830051216      *------------------------------------------------------------------------*
046840051216      * TASTO FUNZIONE F02 - CONTATTI
046850051216      *------------------------------------------------------------------------*
046860051216     c     Sr_F02        BegSr
046870051216
046880051216     c                   Clear                   tnta12ds
046890051216     c                   Eval      ta12ric = wrich
046900091112     c   70
046910091112     corn06              Eval      ta12apl = 'C'
046920091112     c   70
046930091112     corn06              Eval      ta12ksc = v1cksc
046940100806     c   06
046950091112     cann70              Eval      ta12apl = 'T'
046960100806     c   06
046970100806     cann70              Eval      ta12nrv = v1cnrv
046980060217     c                   Eval      ta12rag = wrag
046990051216     c                   Eval      ta12tnt = wtnt
047000060216     c                   Eval      ta12gcli = '1'
047010060216     c   01              Eval      ta12icli = '1'
047020060307     c   01              Move      v3cage        ta12cmm
047030060227     c                   If        %Parms > 1 and
047040060227     c                             Not *In01 and Not *In05 and ta60icli = '1'
047050060221     c                   Eval      ta12icli = '1'
047060060307     c                   Move      v3cage        ta12cmm
047070060221     c                   EndIf
047080051216     c                   Call      'TNTA12R'
047090051216     c                   Parm                    kpjba
047100051216     c                   Parm                    tnta12ds
047110051216
047120051216     c                   ExSr      Sr_Carnote
047130051216
047140051216     c                   EndSr
047150060217
047160060217      *------------------------------------------------------------------------*
047170060217      * TASTO FUNZIONE F03 - FINE LAVORO
047180060217      *------------------------------------------------------------------------*
047190060217     c     Sr_F03        BegSr
047200060217
047210060217     c                   Eval      *In80 = *Off
047220060217
047230060217      * Se immissione emetto videata per chiedere conferma di uscita dal
047240060217      * pgm informando l'utente che i dati andranno persi
047250060217     c                   If        *In01
047260060217     c                   Eval      w03cok = 'NO'
047270060217     c                   Exfmt     ta60w03
047280060217      * Se ok per uscire dal pgm vado a fine
047290060217if  1c                   If        w03cok = 'SI'
047300060217     c                   Goto      fine
047310060217     c                   Else
047320060217     c                   Eval      *In80 = *On
047330060217     c                   Leavesr
047340060217    1c                   EndIf
047350060217     c                   EndIf
047360110302      * Imposto F3 fine se pgm richiamato
047370110302     c                   IF        $Pgmrich
047380110301     c                   eval      TA60f03 = '1'
047390110302     c                   ENDIF
047400110301
047410060217      * Se arrivo qua non è immissione e quindi vado a fine
047420060217     c                   Goto      fine
047430060217
047440060217     c                   EndSr
047450060731
047460060731      *------------------------------------------------------------------------*
047470060731      * TASTO FUNZIONE F04 - ABILITAZIONI
047480060731      *------------------------------------------------------------------------*
047490060731     c     sr_f04        begsr
047500060731
047510060731     c                   clear                   tnta61ds
047520060731     c                   eval      ta61tla = 'V'
047530060731     c                   eval      ta61ksc = v1cksc
047540060731     c                   call      'TNTA61R'
047550060731     c                   parm                    kpjba
047560060731     c                   parm                    tnta61ds
047570060731
047580060731     c                   endsr
047590100507
047600100507      /free
047610100507       //--------------------------------------------------------------
047620100507       //?Tasto Funzione F5 - Attività.
047630100507       //--------------------------------------------------------------
047640100507       BEGSR sr_F05;
047650100507
047660100507         clear trmk21ds;
047670100507         I21mod = 'I';
047680100507         I21ksc = V1Cksc;
047690120124         I21rsc = V2Crag;
047700100507         kpjbu = trmk21ds;
047710100507
047720100507         trmk21r (kpjba);
047730100507
047740100507       ENDSR;
047750100507      /end-free
047760060731
047770051216      *------------------------------------------------------------------------*
047780051216      * TASTO FUNZIONE F07 - CLIENTE UNIFICANTE
047790051216      *------------------------------------------------------------------------*
047800051216     c     Sr_F07        BegSr
047810051216
047820051216     c                   Clear                   tibs12ds
047830051216     c  n05              Eval      k12op0 = 'P02'
047840051216     c   05              Eval      k12op0 = 'P05'
047850051216     c                   Eval      k12op1 = 'O09'
047860051216     c                   Eval      k12f03 = '0'
047870051216     c                   Eval      k12f12 = '0'
047880051216     c                   Eval      k12err = '0'
047890051216     c                   Move      v1cksc        k12cop
047900051216     c                   Eval      kpjbu = tibs12ds
047910051216     c                   Call      'TIBS12R'
047920051216     c                   Parm                    kpjba
047930051216
047940051216     c                   EndSr
047950100407
047960100407      /free
047970100407       //--------------------------------------------------------------
047980100524       //?Tasto Funzione F22 - Richiesta Contatto.
047990100407       //--------------------------------------------------------------
048000100524       BEGSR sr_F22;
048010110223
048020110223       //?Non posso fare una nuova attività su cliente con potenziale in
048030110223       //?categoria PERSO, devo partire dal potenziale
048040110223         IF  V2Hfls = 'P';
048050110223         *in28 = *on;
048060110223         V2Cmsg = 'Nuova attività non possibile per Potenziale PERSO';
048070110223         leavesr;
048080110223         ENDIF;
048090100407
048100100407         clear trmk17ds;
048110100407         IMK17patt = 'S';
048120100407         IMK17att  = 'T';
048130100407         IMK17pcau = 'S';
048140100407         IMK17cau  = '20';
048150100407         IMK17cpo  = v2ccpo;
048160100407         IMK17ksc  = v1cksc;
048170100407         IF  v3cage > *zeros;
048180100407           IMK17cmm  = %dec(v3cage:7:0);
048190100407         ENDIF;
048200100407
048210100407         trmk17r (kpjba : TRMK17DS);
048220100507
048230100507       //?Ricarico i tasti funzione, nel caso venga inserita l'attività sul
048240100507       //?cliente così faccio vedere il tasto F5-Attività
048250100507         exsr Sr_Tastifun;
048260100407
048270100407       ENDSR;
048280101213
048290101213       //--------------------------------------------------------------
048300101213       //?Tasto Funzione F17 - Dati Fatturazione.
048310101213       //--------------------------------------------------------------
048320101213       BEGSR sr_F17;
048330101213
048340101213         clear trul57ds;
048350101213         D57ikscb = V1Cksc;
048360101213         IF  %check(digits:V4Cscf) > 0;
048370101213           D57ikscf = 0;
048380101213         ELSE;
048390101213           D57ikscf = %dec(V4Cscf:7:0);
048400101213         ENDIF;
048410101213         D57iunib = V4Cuni;
048420101213         D57itftb = V4Ctft;
048430101213         D57ifftb = V4Cfft;
048440101213         D57idftb = V4Ctdf;
048450101213         D57ipgm  = 'TNTA60R';
048460101213         D57isifb = V4Csin;
048470101213         D57icdpb = V4Ccdp;
048480101213         D57iabib = V4Cabi;
048490101213         D57icabb = V4Ccab;
048500101213
048510101213         trul57r (kpjba : TRUL57DS);
048520101213
048530101213       ENDSR;
048540110223
048550110223       //--------------------------------------------------------------
048560110223       //?Tasto Funzione F21 - Blocco Cliente.
048570110223       //--------------------------------------------------------------
048580110223       BEGSR sr_F21;
048590110223
048600110223         clear trmk17ds;
048610110223         IMK17patt = 'S';
048620110223         IMK17att  = 'T';
048630110223         IMK17pcau = 'S';
048640110223         IMK17cau  = '71';
048650110223         IMK17cpo  = v2ccpo;
048660110223         IMK17ksc  = v1cksc;
048670110223         IF  v3cage > *zeros;
048680110223           IMK17cmm  = %dec(v3cage:7:0);
048690110223         ENDIF;
048700110223
048710110223         trmk17r (kpjba : TRMK17DS);
048720110223
048730110223       //?Ricarico i tasti funzione
048740110223         exsr Sr_Tastifun;
048750110310
048760110223
048770110223       ENDSR;
048780110223
048790100407      /end-free
048800051216
048810051216      *------------------------------------------------------------------------*
048820051216      * TASTO FUNZIONE F11 - LUOGHI
048830051216      *------------------------------------------------------------------------*
048840051216     c     Sr_F11        BegSr
048850051216
048860051216     c                   Clear                   parmf11
048870051216     c  n05              Eval      paropz = '2'
048880051216     c   05              Eval      paropz = '5'
048890051216     c                   Move      v1cksc        parcli
048900060216     c                   Eval      partnta60 = '1'
048910060216     c   01              Eval      parimta60 = '1'
048920051216     c                   Eval      kpjbu = parmf11
048930051216     c                   Call      'TNTA80R'
048940051216     c                   Parm                    kpjba
048950051216
048960051216     c                   ExSr      Sr_Carluoghi
048970051216
048980051216     c                   EndSr
048990051216
049000051216      *------------------------------------------------------------------------*
049010051216      * TASTO FUNZIONE F14 - INFO COMMERCIALI
049020051216      *------------------------------------------------------------------------*
049030051216     c     Sr_F14        BegSr
049040051216
049050081020     c                   Clear                   trmk50ds
049060081020     c                   Eval      i50pgm = 'TNTA60R'
049070081020     c                   Movel     v2ccpo        i50cpo
049080081020     c                   Eval      i50rag = wrag
049090110310     c                   IF        *in02 and not *in06 and not *in07
049100110310     c                   Eval      i50mod = 'G'
049110110310     c                   ELSE
049120110310     c                   Eval      i50mod = 'I'
049130110310     c                   ENDIF
049140081020     c                   Call      'TRMK50R'
049150051216     c                   Parm                    kpjba
049160081020     c                   Parm                    trmk50ds
049170051216
049180081020     c                   Eval      wtrmk50 = *On
049190051216     c                   EndSr
049200051216
049210051216      *------------------------------------------------------------------------*
049220051216      * TASTO FUNZIONE F15 -
049230051216      *------------------------------------------------------------------------*
049240051216     c     Sr_F15        BegSr
049250051216
049260051216     c                   Clear                   parmf15
049270051216     c                   Move      w0070         parcli1
049280051216     c                   Eval      kpjbu = parmf15
049290051216     c                   Call      'TNTA83R'
049300051216     c                   Parm                    kpjba
049310051216
049320051216     c                   EndSr
049330051216
049340051216      *------------------------------------------------------------------------*
049350051216      * TASTO FUNZIONE F18 -
049360051216      *------------------------------------------------------------------------*
049370051216     c     Sr_F18        BegSr
049380100406
049390100406      /free
049400100806       //?Richiamo gestione note
049410100406          clear trmk20ds;
049420110511          IMK20tla  = 'L';
049430100406
049440100406         //?Se interrogazione richiamo in interrogazione
049450100406          IF  *in05;
049460100406            IMK20flm = 'I';
049470100406          ENDIF;
049480100406
049490101112          IMK20cpo = v2ccpo;
049500100806          IMK20ksc = v1cksc;
049510100406          IMK20rsc = v1crag;
049520100406
049530100406          trmk20r (kpjba : TRMK20DS);
049540100406
049550100406      /end-free
049560051216
049570051216     c                   EndSr
049580060526      *------------------------------------------------------------------------*
049590060526      * TASTO FUNZIONE F19 - Variazioni
049600060526      *------------------------------------------------------------------------*
049610060526     c     Sr_F19        BegSr
049620060526
049630060526     c                   Clear                   tnta73ds
049640060526     c                   Eval      i73kcc=acokcc
049650060526     c                   Eval      i73ksc=acoksc
049660060529     c                   Movel     v1crag        i73rag
049670060526     c                   movel     tnta73ds      kpjbu
049680060526     c                   Call      'TNTA73R'
049690060526     c                   Parm                    kpjba
049700060526     c                   EndSr
049710100407
049720100407      *------------------------------------------------------------------------*
049730100407      * TASTO FUNZIONE F20 - INTERROGAZIONE CLIENTI POTENZIALI
049740100407      *------------------------------------------------------------------------*
049750100407     c     Sr_F20        BegSr
049760100407
049770100407      /free
049780100806       //?Richiamo gestione potenziali
049790100407          clear trmk01ds;
049800100407
049810100407         //?Se non c'è il codice potenziale vado in ricerca
049820100407          IF  v2ccpo = 0;
049830100407            MK01k11  = '1';
049840100407            MK01rag  = w005a;
049850100407            kpjbu    = TRMK01DS;
049860100407            trmk01r (kpjba);
049870100407            TRMK01DS = kpjbu;
049880100407          ELSE;
049890100407         //?Se c'è il codice potenziale vado in gestione
049900100407            MK01k10 = 'N';
049910100407            MK01cpo = v2ccpo;
049920100407            trmk02r (kpjba : TRMK01DS);
049930100407          ENDIF;
049940100407
049950100407          IF MK01cpo > 0 and MK01cpo <> v2ccpo;
049960100407            v2ccpo = MK01cpo;
049970100407            v2dcpo = MK01rag;
049980100407            exsr Sr_Ctrcpo;
049990100407          ENDIF;
050000100407
050010100407      /end-free
050020100407
050030100407     c                   EndSr
050040051216
050050051216      *------------------------------------------------------------------------*
050060051216      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
050070051216      *------------------------------------------------------------------------*
050080051216     c     Sr_F24        begsr
050090051216
050100051216     c                   Select
050110051216
050120051216     c                   When      $loop = 2 and $rig =2  or
050130051216     c                             $loop = 3 and $rig =3
050140051216     c                   Movea     rigatf1       vzfd01
050150051216     c                   Z-add     1             $rig
050160051216
050170051216     c                   If        $loop = 2
050180051216     c                   Eval      vzfd02= cf2412
050190051216     c                   Else
050200051216     c                   Eval      vzfd02= cf2413
050210051216     c                   EndIf
050220051216
050230051216     c                   When      $loop = 2 and $rig =1 or
050240051216     c                             $loop = 3 and $rig =1
050250051216     c                   Movea     rigatf2       vzfd01
050260051216     c                   Z-add     2             $rig
050270051216
050280051216     c                   If        $loop = 2
050290051216     c                   Eval      vzfd02 = cf2422
050300051216     c                   Else
050310051216     c                   Eval      vzfd02 = cf2423
050320051216     c                   EndIf
050330051216
050340051216     c                   When      $loop = 3 and $rig =2
050350051216     c                   Movea     rigatf3       vzfd01
050360051216     c                   Z-add     3             $rig
050370051216     c                   Eval      vzfd02 = cf2433
050380051216     c                   EndSl
050390051216
050400051216     c                   EndSr
050410051216
050420051216      *------------------------------------------------------------------------*
050430051216      * CONFERMO I DATI
050440051216      *------------------------------------------------------------------------*
050450051216     c     Sr_Conferma   BegSr
050460051220
050470051220     c                   Eval      wokimm = *Off
050480051216
050490051216      * In base alla videata emessa faccio i controlli che servono per
050500051216      * poter confermare i dati del video
050510051216
050520051216     c                   Select
050530051216
050540051216      * F6 dato in seconda videata
050550051216     c                   When      wvideo = '02'
050560051216     c                   ExSr      Sr_Ctrd02
050570051216     c   28              Leavesr
050580051216     c                   ExSr      Sr_Ctrd03
050590051216     c  n28              ExSr      Sr_Ctrd04
050600051216     c  n28              ExSr      Sr_Ctrd05
050610051216     c  n28              ExSr      Sr_Ctrd06
050620051216     c  n28              ExSr      Sr_Ctrd07
050630051216     c   28              Eval      v2cmsg = msg(69)
050640150424     c   28              eval      wForzaPag = *off
050650150728     c   28              eval      wForzaMail = *off
050660051216     c   28              Leavesr
050670051216
050680051216      * F6 dato in terza videata
050690051216     c                   When      wvideo = '03'
050700051216     c                   ExSr      Sr_Ctrd03
050710051216     c   28              Leavesr
050720051216     c                   ExSr      Sr_Ctrd04
050730051216     c  n28              ExSr      Sr_Ctrd05
050740051216     c  n28              ExSr      Sr_Ctrd06
050750051216     c  n28              ExSr      Sr_Ctrd07
050760051216     c   28              Eval      v3cmsg = msg(69)
050770150424     c   28              eval      wForzaPag = *off
050780150728     c   28              eval      wForzaMail = *off
050790051216     c   28              Leavesr
050800051216
050810051216      * F6 dato in quarta videata
050820051216     c                   When      wvideo = '04'
050830051216     c                   ExSr      Sr_Ctrd04
050840051216     c   28              Leavesr
050850051216     c                   ExSr      Sr_Ctrd05
050860051216     c  n28              ExSr      Sr_Ctrd06
050870051216     c  n28              ExSr      Sr_Ctrd07
050880051216     c   28              Eval      v4cmsg = msg(69)
050890150424     c   28              eval      wForzaPag = *off
050900150728     c   28              eval      wForzaMail = *off
050910051216     c   28              Leavesr
050920051216
050930051216      * F6 dato in quinta videata
050940051216     c                   When      wvideo = '05'
050950051216     c                   ExSr      Sr_Ctrd05
050960051216     c   28              Leavesr
050970051216     c                   ExSr      Sr_Ctrd06
050980051216     c  n28              ExSr      Sr_Ctrd07
050990051216     c   28              Eval      v5cmsg = msg(69)
051000150728     c   28              eval      wForzaMail = *off
051010051216     c   28              Leavesr
051020051216
051030051216      * F6 dato in sesta videata
051040051216     c                   When      wvideo = '06'
051050051216     c                   ExSr      Sr_Ctrd06
051060051216     c   28              Leavesr
051070051216     c                   ExSr      Sr_Ctrd07
051080051216     c   28              Eval      v6cmsg = msg(69)
051090150728     c   28              eval      wForzaMail = *off
051100051216     c   28              Leavesr
051110051216
051120051216      * F6 dato in settima videata
051130051216     c                   When      wvideo = '07'
051140051216     c                   ExSr      Sr_Ctrd07
051150051216     c   28              Leavesr
051160051216
051170051216     c                   EndSl
051180051216
051190051216      * Se arrivo a questo punto vuol dire che tutte le videate sono a posto
051200051220
051210051220      * Recupero il flag di procedura ORM attiva
051220060731     c                   Movel     v1cksc        w0030
051230051220     c                   Clear                   og148
051240061012     c                   Clear                   og143
051250051220     c     w0030         Chain     Azorg01l
051260051220     c                   If        %Found(Azorg01l)
051270051220     c                   Eval      og148 = orgde8
051280061012     c                   Eval      og143 = orgde3
051290051220     c                   EndIf
051300160905
051310160905      * se cliente logistica no anagrafica provvisoria
051320160905     c                   IF        ClienteLog
051330160905     c                   goto      NoImmACR
051340160905     c                   ENDIF
051350051220
051360051220      * Se non sono in anagrafica provvisoria, la procedura ORM è attiva e
051370051220      * provengo da convalida offerte
051380120313if  1c                   If        Not *In06 and *In07 and §ogorm <> *blanks
051390051220      * Controllo se devo creare l'anagrafica clienti ritiro
051400051220     c                   Eval      wokimm = *On
051410060731     c                   Movel     v1cksc        w0100
051420051220     c     w0100         Setll     Fnacr01l
051430051220do  2c                   Do        *Hival
051440051220     c                   Read      Fnacr01l
051450051220      * Fine file esco
051460051220     c                   If        %Eof(Fnacr01l)
051470051220     c                   Leave
051480051220     c                   EndIf
051490051220     c                   Movel     acrcro        w0070
051500051220      * Codice diverso esco
051510060731     c                   If        w0070 > v1cksc
051520051220     c                   Leave
051530051220     c                   EndIf
051540051220      * Se trovo non devo inserire l'anagrafico clienti ritiro
051550060731     c                   If        w0070 = v1cksc
051560051220     c                   Eval      wokimm = *Off
051570051220     c                   Leave
051580051220     c                   EndIf
051590051220    2c                   EndDo
051600051220
051610051220      * Se il cliente è già sull'anagrafico clienti ritiro emetto la videata
051620051220      * con un msg info
051630051221     c                   Eval      *In29 = *Off
051640051220if  2c                   If        wokimm = *Off and wforza = *Off
051650051220     c                   Eval      *In28 = *On
051660051220     c                   Eval      wforza = *On
051670051220     c                   Select
051680051220     c                   When      wvideo = '02'
051690051220     c                   Eval      v2cmsg = msg(72)
051700051221     c                   Eval      *In29 = *On
051710051220     c                   Goto      emi02
051720051220     c                   When      wvideo = '03'
051730051220     c                   Eval      v3cmsg = msg(72)
051740051220     c                   Goto      emi03
051750051220     c                   When      wvideo = '04'
051760051220     c                   Eval      v4cmsg = msg(72)
051770051220     c                   Goto      emi04
051780051220     c                   When      wvideo = '05'
051790051220     c                   Eval      v5cmsg = msg(72)
051800051220     c                   Goto      emi05
051810051220     c                   When      wvideo = '06'
051820051220     c                   Eval      v6cmsg = msg(72)
051830051220     c                   Goto      emi06
051840051220     c                   When      wvideo = '07'
051850051220     c                   Eval      v7cmsg = msg(72)
051860051220     c                   Goto      emi07
051870051220     c                   EndSl
051880051220    2c                   EndIf
051890051220    1c                   EndIf
051900160905
051910160905     c     NoImmACR      tag
051920051220
051930150424      * aggiorno la nota "DC"
051940091119     c                   if        v2ntcdc <> savntcdc
051950091119     c                   ExSr      Sr_AggntcDC
051960091119     c                   endif
051970051220
051980051220      * Quindi posso passare i dati dal video ai vari file e aggiornare
051990051216
052000051216      * Immissione pulisco i rcd file e imposto la chiave
052010051219if  1c                   If        *In01
052020051216     c  n06              Clear                   Cnaco000
052030051216     c   06              Clear                   Tfaco000
052040051216     c  n06              Clear                   Cnind000
052050051216     c   06              Clear                   Tfind000
052060051216     c  n06              Clear                   Cnclp000
052070051216     c   06              Clear                   Tfclp000
052080051216     c  n06              Clear                   Fncls000
052090051216     c   06              Clear                   Tfcls000
052100051220      * Chiave ACO
052110051216     c                   Eval      acokut = codut
052120051216     c                   Eval      acokcc = dutkci
052130051222     c  n06              Eval      acoksc = v1cksc
052140051222     c   06              Eval      acoksc = v1cnrv
052150051220      * Chiave IND
052160051216     c                   Eval      indkut = acokut
052170051216     c                   Eval      indkcc = acokcc
052180051216     c                   Eval      indksc = acoksc
052190051220      * Chiave CLP
052200051216     c                   Eval      clpkut = acokut
052210051216     c                   Eval      clpkcc = acokcc
052220051216     c                   Eval      clpksc = acoksc
052230051220      * Chiave CLS
052240051216     c                   Eval      clsksc = acoksc
052250051219      * Campi non visualizzati solo di immissione (non vengono aggiornati)
052260051219     c                   Eval      acoopz = v2copz
052270051219     c                   Eval      acotic = v2ctic
052280051219     c                   Eval      indlin = v2clin
052290051219      * Codice filiale di trasmissione sempre uguale ai primi 3 byte del codice
052300051219      * cliente
052310060330     c                   Movel     acoksc        acoflt
052320051219      * Flag tipo trasmissione sempre a '3' xchè se sono in immissione vuol dire
052330051219      * che sto immettendo un cliente o un'anagrafica provvisoria
052340051219     c                   Eval      acoftt = '3'
052350051219      * Data prima spedizione fattura (solo immissione xchè la imposto io
052360051219      * nel caso di cliente non nuovo
052370051219     c                   Z-add     v4cdps        datagma
052380051219     c                   Eval      gg1 = gg2
052390051219     c                   Eval      mm1 = mm2
052400051219     c                   Eval      aa1 = aa2
052410051219     c                   Z-add     dataamg       clpdps
052420051219      * Cliente da sollecitare
052430090616     c                   Movel     v2csol        clpsol
052440051219    1c                   EndIf
052450051216
052460051219      * Campi che vengono aggiornati a video
052470051219      * ACO
052480051219     c                   Eval      atb02 = v2tb02
052490051219     c                   Eval      acorag = v2crag
052500051219     c                   If        v6cdmg = 'SI'
052510051219     c                   Clear                   acorx1
052520051219     c                   Else
052530051219     c                   Eval      acorx1 = 1
052540051219     c                   EndIf
052550060801     c                   If        v6ccag = 'SI'
052560060801     c                   Clear                   acory1
052570060801     c                   Else
052580060801     c                   Eval      acory1 = 1
052590060801     c                   EndIf
052600051219     c                   Z-add     *date         acoduv
052610130325     c   79              eval      ACOabl = V2Cabledp
052620130325     c  n79              eval      ACOabl = V2Cabl
052630051219     c                   Eval      acolib = v2ccpo
052640051219     c                   Move      v3citc        acoitc
052650051216     c                   Clear                   acodtr
052660051219     c                   Clear                   acoftr
052670051219      * IND
052680051216     c                   Eval      indvia = v2cvia
052690051219     c                   Eval      indcit = v2ccit
052700051216      * Se Cap numerico e non supera i 5 caratteri scrivo anche indcap
052710051216if  1c                   If        %subst(v2ccae:6:1) = *Blanks
052720051216     c                   Eval      wokcap = *On
052730051216     c                   Eval      xx = 1
052740051216     c                   For       xx by 1 to 5
052750051216     c                   If        %subst(v2ccae:(xx):1) < *zeros
052760051216     c                   Eval      wokcap = *Off
052770051216     c                   Leave
052780051216     c                   EndIf
052790051216     c                   EndFor
052800051216     c                   If        wokcap = *On
052810060208     c                   Movel     v2ccae        indcap
052820051216     c                   EndIf
052830051216    1c                   EndIf
052840051216     c                   Eval      indprv = v2cprv
052850051216     c                   Eval      indsta = v2csta
052860051216     c                   Eval      indtel = v2ctel
052870051219     c                   Eval      indcdf = v2ccdf
052880051219     c                   If        v2ivaeu = *Blanks
052890051219     c                   Movel     v2ivait       indiva
052900051219     c                   Else
052910051219     c                   Movel     v2civa        indiva
052920051219     c                   EndIf
052930051219     c                   Eval      indcdp = *all'0'
052940100727     c                   Move      v4ccdp        indcdp
052950051219     c                   Eval      %subst(indopz:1:1) = wbloc
052960100727     c                   Eval      indabi = v4cabi
052970100727     c                   Eval      indcab = v4ccab
052980051216     c                   Eval      indtlf = v2ctlf
052990051219     c                   Move      v4csin        indsin
053000051219     c                   Eval      indcae = v2ccae
053010051219      * CLP
053020100127     c                   Eval      clpscf = %dec(v4cscf:7:0)
053030051219     c                   Move      v4cfft        clpfft
053040051219     c                   Move      v4ctft        clptft
053050051219     c                   Clear                   clpfun
053060051219     c                   Eval      %subst(clpfun:1:1) = v4ctdf
053070051219     c                   Move      v3cage        clpage
053080051219     c                   Clear                   clpcon
053090051219     c                   Eval      %subst(clpcon:2:2) = v2ccon
053100051219     c                   Eval      clpnar = v2cblc
053110051219     c                   Eval      clpclv = v3cclv
053120080422     c                   eval      clpbab = v5ciban
053130080213      * se IBAN non impostato a video
053140080213      * pulisco i dati relativi a abi/cab e c/c
053150080213     c                   if        v5ciban = *blanks
053160090331      * ma solo se ABI NON è > 90000
053170090331     c                   if        clpabi < 90000
053180080213     c                   clear                   clpccb
053190080213     c                   clear                   clpabi
053200080213     c                   clear                   clpcab
053210090331     c                   endif
053220080213     c                   else
053230080213      * se IBAN italia memorizzo abi/cab e c/c
053240080213     c                   if        %subst(v5ciban:1:2) = 'IT' or
053250080213     c                             %subst(v5ciban:1:2) = 'SM'
053260080115     c                   eval      %subst(clpccb:1:12) =
053270080115     c                             %subst(v5ciban:16:12)
053280080115     c                   eval      %subst(clpccb:16:1) =
053290080115     c                             %subst(v5ciban:5:1)
053300080115     c                   eval      clpabi = %dec(%subst(v5ciban:6:5):5:0)
053310080115     c                   eval      clpcab = %dec(%subst(v5ciban:11:5):5:0)
053320080213     c                   endif
053330080213     c                   endif
053340080422     c                   Eval      clpfpc = v5cfpc
053350090617      * Cliente da sollecitare
053360090617     c                   Movel     v2csol        clpsol
053370051219      * CLS
053380051219     c                   Eval      clsstp = v7cstp
053390100729     c                   eval      %subst(CLStic:1:1) = v5tpdn
053400100729     c                   eval      %subst(CLStic:2:1) = v5tpna
053410051219     c                   Eval      %subst(clsflo:1:1) = v6ctcf
053420051219     c                   Eval      %subst(clsflo:2:1) = v4cuni
053430121105     c                   eval      %subst(CLSflo:3:1) = V4Ctpf
053440090616     c                   Eval      %subst(clsflo:4:1) = v2fsol
053450051219     c                   Eval      %subst(clsflo:5:1) = v7csdn
053460051219     c                   Eval      %subst(clsflo:6:1) = v4cstm
053470051219     c                   Eval      %subst(clsflo:7:1) = v7csin
053480051219     c                   Eval      %subst(clsflo:8:1) = v7ctpa
053490051219     c                   Eval      %subst(clsflo:9:1) = v6capg
053500051219     c                   Eval      %subst(clsflo:10:1) = v6ctcm
053510051219
053520051219      * Immissione
053530051219     c                   If        *In01
053540051219     c  n06              Write     Fncls000
053550051219     c   06              Write     Tfcls000
053560051219     c  n06              Write     Cnclp000
053570051219     c   06              Write     Tfclp000
053580051219     c  n06              Write     Cnind000
053590051219     c   06              Write     Tfind000
053600051219     c  n06              Write     Cnaco000
053610051219     c   06              Write     Tfaco000
053620060526     c* Scrivo file variazioni
053630060526     c  n06              exsr      ScriviACV
053640060526     c                   EndIf
053650060526     c
053660051219      * Manutenzione
053670051219     c                   If        *In02
053680051219     c  n06              Update    Fncls000
053690051219     c   06              Update    Tfcls000
053700051219     c  n06              Update    Cnclp000
053710051219     c   06              Update    Tfclp000
053720051219     c  n06              Update    Cnind000
053730051219     c   06              Update    Tfind000
053740051219     c  n06              Update    Cnaco000
053750051219     c   06              Update    Tfaco000
053760060526     c*
053770060526     c* Scrivo file variazioni
053780060526     c  n06              exsr      ScriviACV
053790051219     c                   EndIf
053800110927
053810110927      * Aggiorno il potenziale
053820160906      * solo se NON sono utente di sede
053830110927     c                   If        v2ccpo > *Zeros
053840160906     c                             and not *in09
053850110927     c                   ExSr      Sr_Aggcpo
053860110927     c                   EndIf
053870100729
053880100729      /free
053890100729       //?Memorizzo le coordinate bancarie
053900100730       //?Pagamento DANNI --> DN
053910100730         clear rqsOpCode;
053920100729         wpag = 'DN';
053930100729         wiban = v5ibandn;
053940100730         wbic  = v5bicdn;
053950100805         wcodpo = savtipdn;
053960100805         wcodpn = v5tpdn;
053970100805         wpgm  = 'TNTA60R';
053980100730         SELECT;
053990100730           WHEN  v5ibandn <> *blanks and savibandn = *blanks;
054000100805             rqsOpCode = 'INSERT';
054010100805           WHEN  v5ibandn = *blanks and savibandn <> *blanks;
054020100805             rqsOpCode = 'DELETE';
054030100805           OTHER;
054040100805             rqsOpCode = 'UPDATE';
054050100730         ENDSL;
054060100730         IF  rqsOpCode <> *blanks;
054070100730           exsr Coordinate;
054080100730         ENDIF;
054090100729       //?Non faccio controlli sul codice di ritorno
054100100729       //?ormai l'anagrafica è scritta
054110100729
054120100729       //?Pagamento NOTE ACCREDITO --> NA
054130100730         clear rqsOpCode;
054140100729         wpag = 'NA';
054150100729         wiban = v5ibanna;
054160100730         wbic  = v5bicna;
054170100805         wcodpo = savtipna;
054180100805         wcodpn = v5tpna;
054190100805         wpgm  = 'TNTA60R';
054200100730         SELECT;
054210100730           WHEN  v5ibanna <> *blanks and savibanna = *blanks;
054220100805             rqsOpCode = 'INSERT';
054230100805           WHEN  v5ibanna = *blanks and savibanna <> *blanks;
054240100805             rqsOpCode = 'DELETE';
054250100805           OTHER;
054260100805             rqsOpCode = 'UPDATE';
054270100730         ENDSL;
054280100730         IF  rqsOpCode <> *blanks;
054290100730           exsr Coordinate;
054300100729         ENDIF;
054310100729       //?Non faccio controlli sul codice di ritorno
054320100729       //?ormai l'anagrafica è scritta
054330110223
054340110223       //?Se NON è anagrafica provvisoria:
054350110223       //?il cliente è stato bloccato
054360110223       //?e il potenziale NON è in categoria PERSO
054370110310       //?oppure
054380110310       //?il cliente NON è più bloccato
054390110310       //?e il potenziale è in categoria PERSO
054400110223       //?oppure
054410110223       //?arrivo da convalida
054420110223       //?e il potenziale NON è in categoria CODIFICATO
054430110223         IF  not *in06 and
054440110223             ((ACOabl <> *blanks and
054450110223               savabl = *blanks and V2Hfls <> 'P') or
054460110310              (ACOabl = *blanks and
054470110310               savabl <> *blanks and V2Hfls = 'P') or
054480110223              (*in07 and V2Hfls <> 'C'));
054490110223       //?richiamo pgm di calcolo categoria potenziale
054500110223           clear trmk05ds;
054510110223           IMK05cpo = ACOlib;
054520110223           trmk05r (kpjba : TRMK05DS);
054530110223         //?se categoria variata aggiorno potenziale
054540110304           IF  OMK05err = *blanks and OMK05cat <> V2Hfls;
054550110223             chain(e) ACOlib TNCPO01L;
054560110223             IF  not %error and %found(TNCPO01L);
054570110223               CPOfls = OMK05cat;
054580110223               update TNCPO000;
054590110223             ENDIF;
054600110223           ENDIF;
054610110223         ENDIF;
054620170523
054630170523      * Se cliente logistica no anagrfica clienti ritiro
054640170523      * e no spalmatura coordinate IBAN
054650170523     c                   IF        ClienteLog
054660170523     c                   goto      ByPassaAcr
054670170523     c                   ENDIF
054680150424
054690150424       //?Devo spalmare le coordinate bancarie e i cod.pagamenti
054700150424         WinInfo = *off;
054710150424         EoFW06 = *off;
054720150424         SELECT;
054730150424       //?spalmo da pag. NA a CA e DN
054740150424         WHEN  BonificoNA and
054750150424               (not BonificoCA or not BonificoDN);
054760150424           exsr SpalmaNA;
054770150424         WHEN  BonificoDN and
054780150424               (not BonificoCA or not BonificoNA);
054790150424           exsr SpalmaDN;
054800150424         WHEN  BonificoCA and
054810150424               (not BonificoDN or not BonificoNA);
054820150424           exsr SpalmaCA;
054830150424         ENDSL;
054840150424         IF  WinInfo;
054850150424           DOW not EoFW06;
054860150424             exfmt TA60W06;
054870150424             IF  *inkf;
054880150424               EoFW06 = *on;
054890150424               leave;
054900150424             ENDIF;
054910150424           ENDDO;
054920150424         ENDIF;
054930150424
054940100729      /end-free
054950051220
054960051220      * Se non sono in anagrafica provvisoria
054970120313if  1c                   If        Not *In06 and §ogorm <> *blanks
054980051220
054990051220      * Controllo il blocco/sblocco dei clienti per aggiornare anche
055000051220      * l'anagrafica clienti ritiro (solo in caso di blocco)
055010121121      * lo faccio solo se modifica perchè se è immissione non c'è
055020121121      * ancora l'anagrafica clienti ritiro
055030121121     c                   IF        *in02
055040051220     c                   ExSr      Sr_Ctrabl
055050121121     c                   ENDIF
055060051220
055070051220      * Richiamo l'immissione anagrafica clienti ritiro
055080051220if  2c                   If        *In01 or wokimm = *On
055090121121
055100121121      * NON lo faccio se immissione di codici 8888 - 9999 o incassi
055110121121      * da attribuire --> 0001 1° livello
055120121121      *               --> 1000 2° livello
055130160429      * immissione fatta da profilo EDP
055140121121     c                   SELECT
055150121121     c                   WHEN      wksc04 = 8888 or wksc04 = 9999
055160121121     c                   goto      noacr
055170160429     c                   WHEN       *in20 and
055180121121     c                             ((sav_livello = '1' and wksc04 = 0001) or
055190121121     c                              (sav_livello = '2' and wksc04 = 1000) or
055200121121     c                              (sav_livello = *blanks and
055210121121     c                               wCodIncAtt  = *blanks and
055220121121     c                              (wksc04 = 1000  or  wksc04 = 0001)))
055230121121     c                   goto      noacr
055240121121     c                   ENDSL
055250070809     c                   reset                   FIOR37ds
055260070810     c                   eval      I37opz  = '3'
055270070809     c                   movel     ACOksc        I37fgs
055280070809     c                   movel     ACOksc        I37cro
055290070809     c                   movel     ACOksc        I37ksc
055300070809     c                   movel(p)  FIOR37ds      KPJBU
055310070810     c                   call      'FIOR37R'
055320070809     c                   parm                    KPJBA
055330051220    2c                   EndIf
055340121121     c     noacr         tag
055350051220      * Richiamo la modifica dell'anagrafica clienti ritiro
055360060217if  2c                   If        *In02 and Not *In07
055370070809     c                   reset                   FIOR37ds
055380070809     c                   eval      I37opz  = 'A'
055390070809     c                   movel     ACOksc        I37fgs
055400070809     c                   movel     ACOksc        I37cro
055410070809     c                   movel(p)  FIOR37ds      KPJBU
055420070809     c                   call      'FIOR37R'
055430070809     c                   parm                    KPJBA
055440051220    2c                   EndIf
055450051216
055460051220    1c                   EndIf
055470160905
055480160905     c     ByPassaAcr    tag
055490160905
055500061012     c*
055510061012     c* se cliente di network dpd richiamo pgm gestione legami depot/cod.PdC
055520061012     c* per permettere l'aggiornamento del codice cliente sul file dei depot
055530061012     c                   if        not *in06 and §ogntw='DPD'
055540061012     c* imposta la ds di procedura
055550061012     c                   CLEAR                   tisie5ds
055560061012     c                   MOVE      '02'          de5op0
055570061012     c                   MOVEL     *blanks       de5op1
055580061012     c                   MOVEL     '0'           de5f03
055590061012     c                   MOVEL     '0'           de5f12
055600061012     c                   MOVEL     '0'           de5err
055610061012     c                   MOVEL     *blanks       de5msg
055620061012     c                   movel     'N'           de5sel
055630061012     c* lancia il programma
055640061012     c                   CALL      'TISIE5R'
055650061012     c                   PARM                    kpjba
055660061012     c                   PARM                    tisie5ds
055670061012     c                   endif
055680120510
055690120510      /free
055700120510       //?Se il cliente è stato bloccato devo richiamare il pgm delle INFO
055710120510       //?per obbligo di azzeramento della % affido a BRT
055720120510       //?Il richiamo lo faccio solo se:
055730120510       //?--> è appena stato immesso il blocco
055740120510       //?--> il pgm non è stato richiamto dal CMR x impostare il blocco
055750120510       //?--> c'è il codice del potenziale
055760120510         IF  not $Blocco and
055770120510             ACOabl <> *blanks and ACOabl <> savabl and
055780120510             ACOlib > 0;
055790120510           IF  not %open(CNACO16L);
055800120510             open CNACO16L;
055810120510           ENDIF;
055820120510         //?tutto questo solo se sto bloccando l'ultimo cliente della catena
055830120510         //?se ne ho ancora dei NON bloccati non devo azzerare
055840120510           setll ACOlib CNACO16L;
055850120510           reade ACOlib CNACO16L;
055860120510           DOW not %eof(CNACO16L);
055870120510             IF  ww_ACOabl = *blanks;
055880120510               leavesr;
055890120510             ENDIF;
055900120510             reade ACOlib CNACO16L;
055910120510           ENDDO;
055920120510         //?Aggancio le INFO commerciali
055930120510         //?se la % affido a BRT è = 0 non devo azzerare
055940120510           chain (ACOlib:'10 ':'_BAR') TICPI01L;
055950120510 1         IF  not %found(ticpi01l) or CPIval = 0;
055960120510             leavesr;
055970120510           ENDIF;
055980120510
055990120510         //?pgm INFO
056000120510           clear TRMK50DS;
056010120510           I50pgm = 'TNTA60R';
056020120510           I50cpo = ACOlib;
056030120510           I50rag = ACOrag;
056040120510           I50mod = 'G';
056050120510           I50atb = '0';
056060120510           TRMK50R (kpjba : TRMK50DS);
056070120510
056080120510           wTRMK50 = *on;
056090120510
056100120510         ENDIF;
056110120510
056120120510      /end-free
056130051220
056140051216     c                   EndSr
056150051220
056160060526      *------------------------------------------------------------------------*
056170060526      * Scrittura file variazioni
056180060526      *------------------------------------------------------------------------*
056190060526     c     ScriviACV     BegSr
056200060531     c                   Clear                   Tibs73ds
056210060531     c                   eval      ibs73pru=knmus
056220060531     c                   eval      ibs73noj=knmeb
056230060601     c                   eval      ibs73pgm='TNTA60R'
056240060531     c                   eval      ibs73immag='D'
056250060526     c* Immissione
056260060526     c                   if        *in01
056270060531     c                   eval      ibs73cta='I'
056280060531     c                   Call      'TIBS73R'
056290060531     c                   Parm                    Tibs73ds
056300060531     c                   Parm                    cnaco_73
056310060531     c                   Parm                    cnind_73
056320060531     c                   Parm                    cnclp_73
056330060531     c                   Parm                    fncls_73
056340060526     c                   endif
056350060526     c* Manutenzione
056360060526     C                   IF        *IN02
056370060531     c                   eval      ibs73cta='M'
056380060531     c                   Call      'TIBS73R'
056390060531     c                   Parm                    Tibs73ds
056400060531     c                   Parm                    cnaco_73
056410060531     c                   Parm                    cnind_73
056420060531     c                   Parm                    cnclp_73
056430060531     c                   Parm                    fncls_73
056440060531     c                   endif
056450060526     c
056460060531     c                   Eval      wtibs73 = *On
056470060526     c                   EndSr
056480150424
056490150424      /free
056500150424       //--------------------------------------------------------------
056510150427       //?Spalmo coordinate bancarie e pagamenti CA.
056520150424       //--------------------------------------------------------------
056530150424       BEGSR SpalmaCA;
056540150427
056550150512       //?Se il pagamento CA è estero
056560150427       //?spalmo solo il codice senza le coordinate
056570150427         IF  PagEsteroCA;
056580150427           clear CLPbab;
056590150427         ENDIF;
056600150427
056610150427       //?Anagrafica clienti Reale
056620150427         IF  not *in06;
056630150427       //?Aggancio FNCLS
056640150427           chain V1Cksc FNCLS01L;
056650150427           IF  not %found(FNCLS01L);
056660150427             leavesr;
056670150427           ENDIF;
056680150427       //?Spalmo pagamento e coordinate per DN
056690151112           IF  CodPagDN <> '1';
056700150427           IF  not BonificoDN;
056710150427         //?coordinate bancarie
056720150427             clear rqsOpCode;
056730150427             wpag = 'DN';
056740150427             wiban = CLPbab;
056750150427             wcodpo = %subst(CLStic:1:1);
056760150427             wcodpn = CLPfpc;
056770150427             wpgm  = 'TNTA60R';
056780150427             rqsOpCode = 'INSERT';
056790150427             exsr Coordinate;
056800150427             %subst(CLStic:1:1) = CLPfpc;
056810151112             WinInfo = *on;
056820150427           ENDIF;
056830151112           ENDIF;
056840150427       //?Spalmo pagamento e coordinate per NA
056850151112           IF  CodPagNA <> '1';
056860150427           IF  not BonificoNA;
056870150427         //?coordinate bancarie
056880150427             clear rqsOpCode;
056890150427             wpag = 'NA';
056900150427             wiban = CLPbab;
056910150427             wcodpo = %subst(CLStic:2:1);
056920150427             wcodpn = CLPfpc;
056930150427             wpgm  = 'TNTA60R';
056940150427             rqsOpCode = 'INSERT';
056950150427             exsr Coordinate;
056960150427             %subst(CLStic:2:1) = CLPfpc;
056970151112             WinInfo = *on;
056980150427           ENDIF;
056990151112           ENDIF;
057000150427       //?Aggiorno anagrafica
057010150427           update FNCLS000;
057020150427         ENDIF;
057030150427
057040150427       //?Anagrafica clienti Provvisoria
057050150427         IF  *in06;
057060150427       //?Aggancio TFCLS
057070150427           chain V1Cnrv TFCLS01L;
057080150427           IF  not %found(TFCLS01L);
057090150427             leavesr;
057100150427           ENDIF;
057110150427       //?Spalmo pagamento e coordinate per DN
057120151112           IF  CodPagDN <> '1';
057130150427           IF  not BonificoDN;
057140150427             clear rqsOpCode;
057150150427             wpag = 'DN';
057160150427             wiban = CLPbab;
057170150427             wcodpo = %subst(CLStic:1:1);
057180150427             wcodpn = CLPfpc;
057190150427             wpgm  = 'TNTA60R';
057200150427             rqsOpCode = 'INSERT';
057210150427             exsr Coordinate;
057220150427             %subst(CLStic:1:1) = CLPfpc;
057230151112             WinInfo = *on;
057240150427           ENDIF;
057250151112           ENDIF;
057260150427       //?Spalmo pagamento e coordinate per NA
057270151112           IF  CodPagNA <> '1';
057280150427           IF  not BonificoNA;
057290150427             clear rqsOpCode;
057300150427             wpag = 'NA';
057310150427             wiban = CLPbab;
057320150427             wcodpo = %subst(CLStic:2:1);
057330150427             wcodpn = CLPfpc;
057340150427             wpgm  = 'TNTA60R';
057350150427             rqsOpCode = 'INSERT';
057360150427             exsr Coordinate;
057370150427             %subst(CLStic:2:1) = CLPfpc;
057380151112             WinInfo = *on;
057390150427           ENDIF;
057400151112           ENDIF;
057410150427       //?Aggiorno anagrafica
057420150427           update TFCLS000;
057430150427         ENDIF;
057440150424
057450150424       ENDSR;
057460150424
057470150424       //--------------------------------------------------------------
057480150427       //?Spalmo coordinate bancarie e pagamenti DN.
057490150424       //--------------------------------------------------------------
057500150424       BEGSR SpalmaDN;
057510150427
057520150427         SELECT;
057530150427
057540150427       //?Se il pagamento DN è estero ed è
057550150427       //?Anagrafica clienti Reale
057560150427       //?spalmo solo il codice senza le coordinate
057570150427         WHEN  PagEsteroDN and not *in06;
057580150427       //?Memorizzo immagine precedente
057590150427           exsr ImmaginePrima;
057600150427       //?Spalmo pagamento per CA
057610151112           IF  CodPagCA <> '1';
057620150427           IF  not BonificoCA;
057630150427         //?Aggancio CNCLP
057640150427             chain (codut:V1Ckcc:V1Cksc) CNCLP00F;
057650150427             IF  not %found(CNCLP00F);
057660150427               leavesr;
057670150427             ENDIF;
057680150427             CLPfpc = %subst(CLStic:1:1);
057690150427             update CNCLP000;
057700151112             WinInfo = *on;
057710150427           ENDIF;
057720151112           ENDIF;
057730150427       //?Spalmo pagamento per NA
057740151112           IF  CodPagNA <> '1';
057750150427           IF  not BonificoNA;
057760150427         //?Aggancio FNCLS
057770150427             chain V1Cksc FNCLS01L;
057780150427             IF  not %found(FNCLS01L);
057790150427               leavesr;
057800150427             ENDIF;
057810150427             %subst(CLStic:2:1) = %subst(CLStic:1:1);
057820150427             update FNCLS000;
057830151112             WinInfo = *on;
057840150427           ENDIF;
057850151112           ENDIF;
057860150427       //?Immagine Dopo
057870150427           exsr ImmagineDopo;
057880150427
057890150427       //?Se il pagamento DN è estero ed è
057900150427       //?Anagrafica clienti Provvisoria
057910150427       //?spalmo solo il codice senza le coordinate
057920150427         WHEN  PagEsteroDN and *in06;
057930150427       //?Spalmo pagamento per CA
057940151112           IF  CodPagCA <> '1';
057950150427           IF  not BonificoCA;
057960150427         //?Aggancio TFCLP
057970150427             chain (codut:V1Ckcc:V1Cnrv) TFCLP00F;
057980150427             IF  not %found(TFCLP00F);
057990150427               leavesr;
058000150427             ENDIF;
058010150427             CLPfpc = %subst(CLStic:1:1);
058020150427             update TFCLP000;
058030151112             WinInfo = *on;
058040150427           ENDIF;
058050151112           ENDIF;
058060150427       //?Spalmo pagamento per NA
058070151112           IF  CodPagNA <> '1';
058080150427           IF  not BonificoNA;
058090150427         //?Aggancio TFCLS
058100150427             chain V1Cnrv TFCLS01L;
058110150427             IF  not %found(TFCLS01L);
058120150427               leavesr;
058130150427             ENDIF;
058140150427             %subst(CLStic:2:1) = %subst(CLStic:1:1);
058150150427             update TFCLS000;
058160151112             WinInfo = *on;
058170150427           ENDIF;
058180151112           ENDIF;
058190150427
058200150427         OTHER;
058210150427       //?Se arrivo qua il pagamento non è estero quindi spalmo tutto
058220150424       //?Recupero le coordinate bancarie DN
058230150427           wpag = 'DN';
058240150427           rqsOpCode = 'SEARCH';
058250150427           exsr Coordinate;
058260150424       //?Spalmo coordinate e pagamento per CA
058270151112           IF  CodPagCA <> '1';
058280150427           IF  not BonificoCA;
058290150424       //?Memorizzo immagine precedente
058300150427             IF  not *in06;
058310150427               exsr ImmaginePrima;
058320150427         //?Aggancio CNCLP
058330150427               chain (codut:V1Ckcc:V1Cksc) CNCLP00F;
058340150427               IF  not %found(CNCLP00F);
058350150427                 leavesr;
058360150427               ENDIF;
058370150427             ELSE;
058380150427         //?Aggancio TFCLP
058390150427               chain (codut:V1Ckcc:V1Cnrv) TFCLP00F;
058400150427               IF  not %found(TFCLP00F);
058410150427                 leavesr;
058420150427               ENDIF;
058430150427             ENDIF;
058440150427             CLPfpc = %subst(CLStic:1:1);
058450150427             IF  not *in06;
058460150427               CLPbab = TrulIbanO0.IBAN;
058470150427               CLPabi = TrulIbanO0.ABI;
058480150427               CLPcab = TrulIbanO0.CAB;
058490150427               update CNCLP000;
058500150427         //?Memorizzo immagine sucessiva
058510150427               exsr ImmagineDopo;
058520150427             ELSE;
058530150427               CLPbab = TrulIbanO1.IBAN;
058540150427               CLPabi = TrulIbanO1.ABI;
058550150427               CLPcab = TrulIbanO1.CAB;
058560150427               update TFCLP000;
058570150427             ENDIF;
058580151112             WinInfo = *on;
058590150427           ENDIF;
058600151112           ENDIF;
058610150424
058620150424       //?Spalmo coordinate e pagamento per NA
058630151112           IF  CodPagNA <> '1';
058640150427           IF  not BonificoNA;
058650150427             clear rqsOpCode;
058660150427             wpag = 'NA';
058670150427         //?Aggancio FNCLS
058680150427             IF  not *in06;
058690150427               chain V1Cksc FNCLS01L;
058700150427               IF  not %found(FNCLS01L);
058710150427                 leavesr;
058720150427               ENDIF;
058730150427               wiban = TrulIbanO0.IBAN;
058740150427               wbic  = TrulIbanO0.BIC;
058750150427               wcodpo = %subst(CLStic:2:1);
058760150427               wcodpn = %subst(CLStic:1:1);
058770150427             ELSE;
058780150427         //?Aggancio TFCLS
058790150427               chain V1Cnrv TFCLS01L;
058800150427               IF  not %found(TFCLS01L);
058810150427                 leavesr;
058820150427               ENDIF;
058830150427               wiban = TrulIbanO1.IBAN;
058840150427               wbic  = TrulIbanO1.BIC;
058850150427               wcodpo = %subst(CLStic:2:1);
058860150427               wcodpn = %subst(CLStic:1:1);
058870150427             ENDIF;
058880150427             wpgm  = 'TNTA60R';
058890150427             rqsOpCode = 'INSERT';
058900150427             exsr Coordinate;
058910150427             %subst(CLStic:2:1) = %subst(CLStic:1:1);
058920150427         //?Aggiorno anagrafica
058930150427             IF  not *in06;
058940150427               update FNCLS000;
058950150427             ELSE;
058960150427               update TFCLS000;
058970150427             ENDIF;
058980151112             WinInfo = *on;
058990150427           ENDIF;
059000151112           ENDIF;
059010150427         ENDSL;
059020150424
059030150424       ENDSR;
059040150424
059050150424       //--------------------------------------------------------------
059060150427       //?Spalmo coordinate bancarie e pagamenti NA.
059070150424       //--------------------------------------------------------------
059080150424       BEGSR SpalmaNA;
059090150427
059100150427         SELECT;
059110150427
059120150427       //?Se il pagamento NA è estero ed è
059130150427       //?Anagrafica clienti Reale
059140150427       //?spalmo solo il codice senza le coordinate
059150150427         WHEN  PagEsteroNA and not *in06;
059160150427       //?Memorizzo immagine precedente
059170150427           exsr ImmaginePrima;
059180150427       //?Spalmo pagamento per CA
059190151112           IF  CodPagCA <> '1';
059200150427           IF  not BonificoCA;
059210150427         //?Aggancio CNCLP
059220150427             chain (codut:V1Ckcc:V1Cksc) CNCLP00F;
059230150427             IF  not %found(CNCLP00F);
059240150427               leavesr;
059250150427             ENDIF;
059260150427             CLPfpc = %subst(CLStic:2:1);
059270150427             update CNCLP000;
059280151112             WinInfo = *on;
059290150427           ENDIF;
059300151112           ENDIF;
059310150427       //?Spalmo pagamento per DN
059320151112           IF  CodPagDN <> '1';
059330150427           IF  not BonificoDN;
059340150427         //?Aggancio FNCLS
059350150427             chain V1Cksc FNCLS01L;
059360150427             IF  not %found(FNCLS01L);
059370150427               leavesr;
059380150427             ENDIF;
059390150427             %subst(CLStic:1:1) = %subst(CLStic:2:1);
059400150427             update FNCLS000;
059410151112             WinInfo = *on;
059420150427           ENDIF;
059430151112           ENDIF;
059440150427       //?Immagine Dopo
059450150427           exsr ImmagineDopo;
059460150427
059470150427       //?Se il pagamento NA è estero ed è
059480150427       //?Anagrafica clienti Provvisoria
059490150427       //?spalmo solo il codice senza le coordinate
059500150427         WHEN  PagEsteroNA and *in06;
059510150427       //?Spalmo pagamento per CA
059520151112           IF  CodPagCA <> '1';
059530150427           IF  not BonificoCA;
059540150427         //?Aggancio TFCLP
059550150427             chain (codut:V1Ckcc:V1Cnrv) TFCLP00F;
059560150427             IF  not %found(TFCLP00F);
059570150427               leavesr;
059580150427             ENDIF;
059590150427             CLPfpc = %subst(CLStic:2:1);
059600150427             update TFCLP000;
059610151112             WinInfo = *on;
059620150427           ENDIF;
059630151112           ENDIF;
059640150427       //?Spalmo pagamento per DN
059650151112           IF  CodPagDN <> '1';
059660150427           IF  not BonificoDN;
059670150427         //?Aggancio TFCLS
059680150427             chain V1Cnrv TFCLS01L;
059690150427             IF  not %found(TFCLS01L);
059700150427               leavesr;
059710150427             ENDIF;
059720150427             %subst(CLStic:1:1) = %subst(CLStic:2:1);
059730150427             update TFCLS000;
059740151112             WinInfo = *on;
059750150427           ENDIF;
059760151112           ENDIF;
059770150427
059780150427         OTHER;
059790150427       //?Se arrivo qua il pagamento non è estero quindi spalmo tutto
059800150424       //?Recupero le coordinate bancarie NA
059810150427           wpag = 'NA';
059820150427           rqsOpCode = 'SEARCH';
059830150427           exsr Coordinate;
059840150424       //?Spalmo coordinate e pagamento per CA
059850151112           IF  CodPagCA <> '1';
059860150427           IF  not BonificoCA;
059870150424       //?Memorizzo immagine precedente
059880150427             IF  not *in06;
059890150427               exsr ImmaginePrima;
059900150424       //?Aggancio CNCLP
059910150427               chain (codut:V1Ckcc:V1Cksc) CNCLP00F;
059920150427               IF  not %found(CNCLP00F);
059930150427                 leavesr;
059940150427               ENDIF;
059950150427             ELSE;
059960150427       //?Aggancio TFCLP
059970150427               chain (codut:V1Ckcc:V1Cnrv) TFCLP00F;
059980150427               IF  not %found(TFCLP00F);
059990150427                 leavesr;
060000150427               ENDIF;
060010150427             ENDIF;
060020150427             CLPfpc = %subst(CLStic:2:1);
060030150427             IF  not *in06;
060040150427               CLPbab = TrulIbanO0.IBAN;
060050150427               CLPabi = TrulIbanO0.ABI;
060060150427               CLPcab = TrulIbanO0.CAB;
060070150427               update CNCLP000;
060080150427         //?Memorizzo immagine sucessiva
060090150427               exsr ImmagineDopo;
060100150427             ELSE;
060110150427               CLPbab = TrulIbanO1.IBAN;
060120150427               CLPabi = TrulIbanO1.ABI;
060130150427               CLPcab = TrulIbanO1.CAB;
060140150427               update TFCLP000;
060150150427             ENDIF;
060160151112             WinInfo = *on;
060170150427           ENDIF;
060180151112           ENDIF;
060190150424
060200150424       //?Spalmo coordinate e pagamento per DN
060210151112           IF  CodPagDN <> '1';
060220150427           IF  not BonificoDN;
060230150427             clear rqsOpCode;
060240150427             wpag = 'DN';
060250150427         //?Aggancio FNCLS
060260150427             IF  not *in06;
060270150427               chain V1Cksc FNCLS01L;
060280150427               IF  not %found(FNCLS01L);
060290150427                 leavesr;
060300150427               ENDIF;
060310150427               wiban = TrulIbanO0.IBAN;
060320150427               wbic  = TrulIbanO0.BIC;
060330150427               wcodpo = %subst(CLStic:1:1);
060340150427               wcodpn = %subst(CLStic:2:1);
060350150427             ELSE;
060360150427         //?Aggancio TFCLS
060370150427               chain V1Cnrv TFCLS01L;
060380150427               IF  not %found(TFCLS01L);
060390150427                 leavesr;
060400150427               ENDIF;
060410150427               wiban = TrulIbanO1.IBAN;
060420150427               wbic  = TrulIbanO1.BIC;
060430150427               wcodpo = %subst(CLStic:1:1);
060440150427               wcodpn = %subst(CLStic:2:1);
060450150427             ENDIF;
060460150427             wpgm  = 'TNTA60R';
060470150427             rqsOpCode = 'INSERT';
060480150427             exsr Coordinate;
060490150427             %subst(CLStic:1:1) = %subst(CLStic:2:1);
060500150427         //?Aggiorno anagrafica
060510150427             IF  not *in06;
060520150427               update FNCLS000;
060530150427             ELSE;
060540150427               update TFCLS000;
060550150427             ENDIF;
060560151112             WinInfo = *on;
060570150427           ENDIF;
060580151112           ENDIF;
060590150424
060600150427         ENDSL;
060610150424
060620150424       ENDSR;
060630150424      /end-free
060640150424
060650051220      *------------------------------------------------------------------------*
060660051220      * AGGIORNAMENTO DEL CODICE POTENZIALE
060670051220      *------------------------------------------------------------------------*
060680051220     c     Sr_Aggcpo     BegSr
060690100407
060700100407      /free
060710100806       //?Se variato codice potenziale
060720110928       //?o se immissione di nuovo codice
060730110928        IF  (v2ccpo <> savcpo and savcpo > 0) or *in01;
060740100407         //?richiamo programma di comodo per sistemare potenziale
060750100407         //?sui vari archivi (trattativa, anagrafica provvisoria, attività)
060760100407         //?nel caso di trattativa aperta
060770110223         //?in ogni caso ricalcola e aggiorna sempre la categoria
060780110223         //?del vecchio e nuovo potenziale
060790100407          clear TRMK28DS;
060800100407          IMK28cpo   = v2ccpo;
060810100407          IMK28cpoex = savcpo;
060820100407          IMK28ksc   = v1cksc;
060830100407          trmk28r (TRMK28DS);
060840100407        ENDIF;
060850121105
060860121105       //?Se Immissione di nuovo cliente (no anagrafica provvisoria)
060870121105       //?devo aggiornare la data primo contatto sul Potenziale
060880121105        IF  *in01 and not *in06 and V2Ccpo > 0 and
060890121105           (§CPOdtpcon = *blanks or §CPOdtpcon = *zeros);
060900121105          chain(e) CPOcpo TNCPO01L;
060910121105          IF  not %error and %found(TNCPO01L);
060920121105            dCPO01 = CPOrst;
060930121105            §CPOdtpcon = %editc(dataoggi:'X');
060940121105            CPOrst = dCPO01;
060950121105            update TNCPO000;
060960121105          ENDIF;
060970121105        ENDIF;
060980121105
060990100407      /end-free
061000080307
061010080307      * se sono in anagrafica provvisoria aggiorno codice fiscale e p.iva
061020080307      * sul potenziale se sul potenziale mancano
061030080307     c                   if        *in06  and *in01
061040110516     c     v2ccpo        Chain(e)  Tncpo01l
061050110516     c                   if        %error
061060110516     c                   leavesr
061070110516     c                   endif
061080080307     c                   if        %found(tncpo01l)
061090160803      /free
061100160803       //?Salvo l'immagine precedente dell'anagrafica potenziale
061110160803         exsr ImmaginePrimaCpo;
061120160803      /end-free
061130080307     c                   if        (cpopiv=*blanks and v2civa<>*blanks)
061140080307     c                   eval      cpopiv=v2civa
061150080307     c                   z-add     *date         cpodtr
061160080307     c                   endif
061170080307     c                   if        (cpocdf=*blanks and v2ccdf<>*blanks)
061180080307     c                   eval      cpocdf=v2ccdf
061190080307     c                   z-add     *date         cpodtr
061200080307     c                   endif
061210080307     c                   update    tncpo000
061220160803      /free
061230160803       //?Scrivo la Variazione fatta sull'anagrafica potenziale
061240160803         exsr ScriviVarCpo;
061250160803      /end-free
061260080307     c                   endif
061270080307     c                   endif
061280051220
061290051220     c                   EndSr
061300051216
061310051216      *------------------------------------------------------------------------*
061320051125      * CONTROLLO DEL CODICE POTENZIALE
061330051125      *------------------------------------------------------------------------*
061340051125     c     Sr_Ctrcpo     BegSr
061350051125
061360111122      * Obbligatorio se cliente, se non è un 8888/9999
061370110223      * e se non è un 'incassi da attribuire'
061380111122      * se non è utente EDP e codice nuovo x 'incassi da attribuire'
061390051125     c                   If        v2ccpo = *Zeros and
061400110223     c                             v1ckcc = 151 and
061410051125     c                             wksc04 <> 8888 and wksc04 <> 9999
061420110223     c                             and %editc(v1cksc:'X') <> wCodIncAtt
061430170516      // Se cliente potenziale emetto un messaggio forzabile
061440170516       IF  (ClienteLog and not wForzaPotenziale) or
061450170516            not ClienteLog;
061460051125     c                   Eval      *In28 = *On
061470051202     c                   Eval      h2riga = 14
061480051125     c                   Eval      h2colo = 28
061490051125     c                   Eval      v2cmsg = msg(12)
061500170516       IF  ClienteLog;
061510170516         V2Cmsg = %trim(V2Cmsg) + '. Enter per forzare.';
061520170516         wForzaPotenziale = *on;
061530170516       ENDIF;
061540160429     c                   IF        *in20 and *in01
061550111122     c                             and sav_livello = *blanks
061560111122     c                             and wCodIncAtt = *blanks and
061570111122     c                             (wksc04 = 0001 or wksc04 = 1000)
061580111122     c                   clear                   v2cmsg
061590111122     c                   eval      *in28 = *off
061600111122     c                   ENDIF
061610051125     c                   Leavesr
061620170516       ENDIF;
061630051125     c                   EndIf
061640051125      * Carico i dati del potenziale se inserito
061650051125if  1c                   If        v2ccpo <> *Zeros and
061660051125      * e se variato il codice potenziale
061670051125     c                             v2ccpo <> savcpo
061680060221     c                             and wokpot = *off
061690051125      * Controllo se esite
061700060221     c     v2ccpo        Chain(n)  Tncpo01l
061710051125     c                   If        Not %Found(Tncpo01l)
061720051125     c                   Eval      *In28 = *On
061730051202     c                   Eval      h2riga = 14
061740051125     c                   Eval      h2colo = 28
061750051125     c                   Eval      v2cmsg = msg(13)
061760051125     c                   Leavesr
061770051125     c                   EndIf
061780051125      * Controllo se valido
061790051125     c                   If        cpoatb <> *Blanks
061800051125     c                   Eval      *In28 = *On
061810150427     c                   Eval      h2riga = 14
061820051125     c                   Eval      h2colo = 28
061830051125     c                   Eval      v2cmsg = msg(13)
061840051125     c                   Leavesr
061850051125     c                   EndIf
061860051125      * Controllo se l'utente può gestire il potenziale
061870051125     c                   Move      cpoflt        w003a
061880051125     c     w003a         Lookup    skpogcpo                               30
061890051125     c                   If        Not *In30
061900051125     c                   Eval      *In28 = *On
061910051202     c                   Eval      h2riga = 14
061920051125     c                   Eval      h2colo = 28
061930060110     c                   Eval      v2cmsg = msg(14)
061940051125     c                   Leavesr
061950051125     c                   EndIf
061960100604
061970100604      /free
061980100604       //?Se NON sono in ambiente di filiale non posso variare il potenziale
061990100604       IF  *in09 and v2ccpo <> savcpo and savcpo > 0;
062000100604         *in28 = *on;
062010100604         h2riga = 07;
062020100604         h2colo = 28;
062030100604         v2cmsg = 'Potenziale modificabile SOLO da utente di filiale';
062040100604         leavesr;
062050100604       ENDIF;
062060100806       //?controllo se con il vecchio potenziale c'è una
062070100604       //?trattativa aperta, in questo caso controllo che non ne esista già
062080100604       //?una con il nuovo potenziale
062090100806       IF  v2ccpo <> savcpo and savcpo > 0;
062100100604       //?con vecchio potenziale
062110100604         clear TNTA43ds;
062120100604         ITA43cpo = savcpo;
062130100604         ITA43ksc = v1cksc;
062140100604         ITA43cmm = %dec(v3cage:7:0);
062150100604         tnta43r (tnta43ds);
062160100604       //?trovo trattativa aperta
062170100604         IF  OTA43nrv > 0;
062180100604         //?rifaccio il controllo con nuovo potenziale
062190100604           clear TNTA43ds;
062200100604           ITA43cpo = v2ccpo;
062210100604           ITA43ksc = v1cksc;
062220100604           ITA43cmm = %dec(v3cage:7:0);
062230100604           tnta43r (tnta43ds);
062240100604         //?trovo trattativa aperta
062250100604           IF  OTA43nrv > 0;
062260100604             *in28 = *on;
062270100604             h2riga = 07;
062280100604             h2colo = 28;
062290100604             v2cmsg = 'Modifica non possibile, trattativa aperta sul +
062300100604                       potenziale immesso';
062310100604             leavesr;
062320100604           ENDIF;
062330100604         ENDIF ;
062340100604       ENDIF;
062350100604      /end-free
062360051212
062370051206      * Carico a video i dati del potenziale se sono in immissione
062380051206     c   01              ExSr      Sr_Carcpo
062390051125    1c                   EndIf
062400051125
062410051125     c                   EndSr
062420051116
062430051116      *------------------------------------------------------------------------*
062440051116      * CARICO I DATI DEL CODICE POTENZIALE
062450051116      *------------------------------------------------------------------------*
062460051116     c     Sr_Carcpo     BegSr
062470140714
062480140714     c     CPOcpo        Chain     Tncpo11l
062490051116
062500051116     c                   Eval      v2crag = cporag
062510051116     c                   Eval      v2cvia = cpovia
062520051116     c                   Eval      v2ccae = cpocap
062530051117     c                   Eval      v2csta = cponaz
062540051116     c                   Eval      v2ccit = cpocit
062550051116     c                   Eval      v2cprv = cpoprv
062560140714     c                   Eval      v2ctel = cpo1tel
062570140714     c                   Eval      v2ctlf = cpo1fax
062580051117     c                   Eval      v2ccpo = cpocpo
062590051117     c                   Eval      savcpo = cpocpo
062600051130     c                   Move      cposct        savitc
062610060208     c                   Eval      v2dcpo = cporag
062620080306     c                   Eval      v2ccdf = cpocdf
062630080306     c                   Eval      savcdf = v2ccdf
062640080306    5c                   if        %subst(cpopiv:1:2)>='00'
062650080306     c                   Movel     cpopiv        v2ivait
062660080306     c                   else
062670080306     c                   Movel     cpopiv        v2civa
062680080306    5c                   endif
062690080306     c                   Eval      saviva = v2civa
062700051116
062710051116     c                   EndSr
062720091119
062730091119      *------------------------------------------------------------------------*
062740091119      * AGGIORNAMENTO NOTA DC
062750091119      *------------------------------------------------------------------------*
062760091119     c     Sr_AggntcDC   BegSr
062770091119
062780091119     c   70
062790091119     corn06              Eval      kntcapl = 'C'
062800100806     c   06
062810091119     cann70              Eval      kntcapl = 'T'
062820091119     c                   Movel(p)  dutkci        kntcnk1
062830091119     c   70
062840091119     corn06              Move      v1cksc        kntcnk1
062850100806     c   06
062860100806     cann70              Move      v1cnrv        kntcnk1
062870091119     c                   Clear                   kntcnk2
062880091119     c                   Movel     'DC'          kntctnt
062890091119     c     kTfntc        Chain     Tfntc01l
062900091119     c                   If        %Found(Tfntc01l)
062910091119      * devo cancellarla perchè campo vuoto
062920091119     c                   if        v2ntcdc = *blanks
062930091119     c                   delete    tfntc
062940091119     c                   endif
062950091119      * aggiorno la nota
062960091119     c                   if        v2ntcdc <> *blanks
062970091119     c                   eval      ntcrnt = v2ntcdc
062980091119     c                   move      *date         ntcntr
062990091119     c                   update    tfntc
063000091119     c                   endif
063010091119     c                   endif
063020091119      * scrivo la nota
063030091119     c                   If        not %Found(Tfntc01l)
063040091119     c                   clear                   tfntc
063050091119     c                   eval      ntcapl = kntcapl
063060091119     c                   eval      ntcnk1 = kntcnk1
063070091119     c                   eval      ntctnt = kntctnt
063080091119     c                   eval      ntcrnt = v2ntcdc
063090091119     c                   eval      ntcsns = 'S'
063100091119     c                   move      *date         ntcntr
063110091119     c                   write     tfntc
063120091119     c                   endif
063130091119
063140091119     c                   EndSr
063150051212
063160051212      *--------------------------------------------------------------------*
063170051212      * WINDOW X CONFERMA DI VARIAZIONE POTENZIALE
063180051212      *--------------------------------------------------------------------*
063190051212     c     Sr_Winpot     begsr
063200051212
063210051212     c                   Reset                   wokpot
063220051212     c                   Eval      w01cok = 'NO'
063230051212     c                   Exfmt     ta60w01
063240051212      * Ok il nuovo potenziale è da tenere
063250051212if  1c                   If        w01cok = 'SI'
063260051212     c                   Eval      wokpot = *On
063270051212    1c                   EndIf
063280051212
063290051212     c                   EndSr
063300051118
063310051118      *------------------------------------------------------------------------*
063320051118      * CONTROLLO INDIRIZZO
063330051118      *------------------------------------------------------------------------*
063340051118     c     Sr_Ctrind     BegSr
063350051118
063360051118     c                   Clear                   fnlv13ds
063370051118     c                   Clear                   tisi95ds
063380051124     c                   Eval      *In90 = *Off
063390051118
063400051118      * Indirizzo completo
063410051118     c                   Z-add     *date         i14dri
063420051118     c                   Call      'FNLV14R'
063430051118     c                   Parm                    kpjba
063440051118     c                   Parm                    fnlv14ds
063450051118
063460051118s   1c                   Select
063470051118     c                   When      o14err = '1'
063480051124     c                   Eval      *In90 = *On
063490051130     c                   Eval      h2riga = 07
063500051118     c                   Eval      h2colo = 28
063510051124     c                   If        o14msg <> *Blanks
063520051124     c                   Eval      *In28 = *On
063530051118     c                   Eval      v2cmsg = o14msg
063540051124     c                   EndIf
063550051124     c                   Leavesr
063560051118     c                   When      o14err = '2'
063570051124     c                   Eval      *In90 = *On
063580051130     c                   Eval      h2riga = 08
063590051118     c                   Eval      h2colo = 28
063600051124     c                   If        o14msg <> *Blanks
063610051124     c                   Eval      *In28 = *On
063620051118     c                   Eval      v2cmsg = o14msg
063630051124     c                   EndIf
063640051124     c                   Leavesr
063650051118     c                   When      o14err = '5'
063660051124     c                   Eval      *In90 = *On
063670051130     c                   Eval      h2riga = 09
063680051118     c                   Eval      h2colo = 18
063690051124     c                   If        o14msg <> *Blanks
063700051124     c                   Eval      *In28 = *On
063710051118     c                   Eval      v2cmsg = o14msg
063720051124     c                   EndIf
063730051124     c                   Leavesr
063740051118     c                   When      o14err = '3'
063750051124     c                   Eval      *In90 = *On
063760051130     c                   Eval      h2riga = 09
063770051118     c                   Eval      h2colo = 28
063780051124     c                   If        o14msg <> *Blanks
063790051124     c                   Eval      *In28 = *On
063800051118     c                   Eval      v2cmsg = o14msg
063810051124     c                   EndIf
063820051124     c                   Leavesr
063830051118     c                   When      o14err = '4'
063840051124     c                   Eval      *In90 = *On
063850051130     c                   Eval      h2riga = 09
063860051118     c                   Eval      h2colo = 59
063870051124     c                   If        o14msg <> *Blanks
063880051124     c                   Eval      *In28 = *On
063890051118     c                   Eval      v2cmsg = o14msg
063900051124     c                   EndIf
063910051124     c                   Leavesr
063920051118     c                   When      o14err = '6'
063930051124     c                   Eval      *In90 = *On
063940051130     c                   Eval      h2riga = 09
063950051118     c                   Eval      h2colo = 62
063960051124     c                   If        o14msg <> *Blanks
063970051124     c                   Eval      *In28 = *On
063980051118     c                   Eval      v2cmsg = o14msg
063990051124     c                   EndIf
064000051124     c                   Leavesr
064010051118     c                   When      o14err <> *Blanks and o14msg = *Blanks
064020051118     c                   Goto      emi02
064030051118    1c                   EndSl
064040051118
064050051118      * Nazione con cappario controllo Cap
064060051118if  1c                   If        o14cpp = *Blanks
064070051118     c                   Eval      i95tcn = '7'
064080051118     c                   Eval      i95cap = e14cap
064090051118     c                   Eval      i95loc = e14loc
064100051118     c                   Eval      i95prv = e14prv
064110051118     c                   Eval      i95nar = e14nar
064120051118     c                   Z-add     *date         i95dat
064130051118     c                   Eval      i13af0 = 'S'
064140051118     c                   Eval      i13cnv = 'S'
064150051118     c                   Eval      i13af1 = 'S'
064160051118     c                   Eval      i13la3 = 'S'
064170051118      * Se dati variati controllo se congruenti
064180051118if  2c                   If        e14cap <> wcae or e14loc <> wcit or
064190051118     c                             e14prv <> wprv or e14nar <> wsta
064200051118     c                   Clear                   wfz3
064210051118     c                   Eval      wcae = e14cap
064220051118     c                   Eval      wcit = e14loc
064230051118     c                   Eval      wprv = e14prv
064240051118     c                   Eval      wsta = e14nar
064250051118    2c                   EndIf
064260051118     c                   Eval      E13fz3 = wfz3
064270051118     c                   Call      'FNLV13R'
064280051118     c                   Parm                    kpjba
064290051118     c                   Parm                    fnlv13ds
064300051118     c                   Parm                    tisi95ds
064310051118
064320051118     c                   If        o13ron = 'S'
064330051118     c                   Eval      e14nar = o95nar
064340051118     c                   EndIf
064350051118     c                   If        o13roc = 'S'
064360051118     c                   Eval      e14cap = o95cap
064370051118     c                   EndIf
064380051118     c                   If        o13rop = 'S'
064390051118     c                   Eval      e14prv = o95prv
064400051118     c                   EndIf
064410051118     c                   If        o13rol = 'S'
064420051118     c                   Eval      e14loc = o95loc
064430051118     c                   EndIf
064440051118
064450051124      * Reimposto le forzature
064460051118     c                   Eval      wfz3 = E13fz3
064470051118
064480051118s   2c                   Select
064490051118     c                   When      o13err = '5'
064500051118     c                   Eval      *In28 = *On
064510051130     c                   Eval      h2riga = 09
064520051118     c                   Eval      h2colo = 18
064530051118     c                   Eval      v2cmsg = o13msg
064540051118     c                   Leavesr
064550051118     c                   When      o13err = '3'
064560051118     c                   Eval      *In28 = *On
064570051130     c                   Eval      h2riga = 09
064580051118     c                   Eval      h2colo = 28
064590051118     c                   Eval      v2cmsg = o13msg
064600051118     c                   Leavesr
064610051118     c                   When      o13err = '4'
064620051118     c                   Eval      *In28 = *On
064630051130     c                   Eval      h2riga = 09
064640051118     c                   Eval      h2colo = 59
064650051118     c                   Eval      v2cmsg = o13msg
064660051118     c                   Leavesr
064670051118     c                   When      o13err = '6'
064680051118     c                   Eval      *In28 = *On
064690051130     c                   Eval      h2riga = 09
064700051118     c                   Eval      h2colo = 62
064710051118     c                   Eval      v2cmsg = o13msg
064720051118     c                   Leavesr
064730051118     c                   When      o13err <> *Blanks and o13msg = *Blanks
064740051118     c                   Goto      emi02
064750051118    2c                   EndSl
064760051118
064770051118      * Se nazione senza cappario
064780051118   x1c                   Else
064790051122      * Il cliente deve essere bloccato con 8 (si può usare solo x fatturare e
064800051122      *                                        non per bollettare)
064810051118if  2c                   If        v2cabl <> '8'
064820051118     c                   Eval      *In28 = *On
064830051130     c                   Eval      h2riga = 09
064840051118     c                   Eval      h2colo = 62
064850051118     c                   Eval      v2cmsg = msg(15)
064860051118     c                   Leavesr
064870051118    2c                   EndIf
064880051118    1c                   EndIf
064890051118
064900051118      * Cap obsoleto
064910051118     c                   If        o95cf1 = 'O'
064920051118     c                   Eval      *In28 = *On
064930051130     c                   Eval      h2riga = 09
064940051118     c                   Eval      h2colo = 18
064950051118     c                   Eval      v2cmsg = msg(16)
064960051118     c                   Leavesr
064970051118     c                   EndIf
064980051118
064990051118     c                   EndSr
065000051118
065010051118      *------------------------------------------------------------------------*
065020051118      * CONTROLLO CODICE FISCALE
065030051118      *------------------------------------------------------------------------*
065040051118     c     Sr_Ctrcdf     BegSr
065050151014     c                   clear                   w_scfsta
065060100729
065070061030      * devo richiamare il nuovo driver fatto per controllare il codice
065080061030      * fiscale e la partita iva
065090080421      * rihciamo anche per codice vuoto, per controllare i casi
065100080421      *  di obbligo non inserimento
065110151014     c                   clear                   xcfiva1ds
065120061030     c                   eval      xcfivapar = v2ccdf
065130061030     c                   eval      xcfivanar = v2csta
065140061030     c                   eval      xcfivaprv = v2cprv
065150151014     c                   eval      xcfivacap = v2ccae
065160151014     c                   eval      xcfivaloc = v2ccit
065170151014     c                   eval      xcfivapiva= v2civa
065180151014     c                   call      'XCFIVAR1'
065190151014     c                   parm                    xcfiva1ds
065200080421     c
065210080421    1c                   if        xcfivaflg < *zeros
065220051118     c                   Eval      *In28 = *On
065230051202     c                   Eval      h2riga = 12
065240051118     c                   Eval      h2colo = 28
065250080421     c                   Eval      v2cmsg = xcfivamsg
065260051118     c                   Leavesr
065270080421    1c                   EndIf
065280080421
065290080421      * il codice fiscale è obbligatorio se sto inserendo un codice nuovo
065300080421      * cliente italia a partire dalla data decorrenza per p.iva sarà
065310080421      * obbligatorio non forzabile anche in caso di codice non nuovo
065320080421
065330080421      * Escludo i casi previsti di NON inserimento
065340080421    1c  n06              if        xcfivaflg<>9 and
065350080421     c                             v2csta=*blanks and v2ccdf=*blanks
065360080421     c
065370080421     c* prima di dare errore verifico se cliente del sottoconto intestazione
065380080421     c* fattura ha la nazione ed in questo caso non do errore
065390080421     c                   exsr      sr_repscfsta
065400080421    2c                   if        w_scfsta=*blanks
065410080421     c                   eval      *In28 = *On
065420080421     c                   eval      h2riga = 12
065430080421     c                   eval      h2colo = 28
065440080421     c                   eval      v2cmsg = msg(51)
065450080421     c                   leavesr
065460080421    2c                   endif
065470080421    1c                   endif
065480080306     c* Se provengo dalle visite e il relativo potenziale non ha il cod.fisc
065490080306     c* errore forzabile se inserito un codice fiscale già presente su altro
065500080306     c* potenziale
065510080421    1c                   if        *in06 and v2ccpo<>*zeros and v2ccdf<>*blanks
065520080306     c     v2ccpo        Chain(n)  Tncpo01l
065530080421    2c                   if        %found(tncpo01l) and cpocdf=*blanks
065540080306     c                             and v2ccdf<>savcdf
065550080306     c                             and v2ccdf<>cod_forzcdf
065560080306     c                   clear                   savrag
065570080307     c                   eval      codice=v2ccdf
065580080306     c                   exsr      sr_CdfAltroP
065590080306     c                   exsr      sr_msgcdf
065600080306     c   28              leavesr
065610080421    2c                   endif
065620080421    1c                   endif
065630051118
065640051118     c                   EndSr
065650051118
065660051118      *------------------------------------------------------------------------*
065670051118      * CONTROLLO PARTITA IVA E STATO
065680051118      *------------------------------------------------------------------------*
065690051118     c     Sr_Ctriva     BegSr
065700051118
065710051118      * STATO
065720051118      * --> ricerca
065730051118     c                   Eval      wpos = %scan('?':v2csta)
065740051118     c                   If        wpos > 0
065750051118     c                   Eval      kcod = '15'
065760051118     c                   call      'TRUL19R'
065770051118     c                   parm                    kcod
065780051118     c                   parm                    ordina
065790051118     c                   parm                    kkey
065800051118     c                   parm                    comando
065810051118     c                   Eval      v2csta = kkey
065820051130     c                   Eval      h2riga = 09
065830051118     c                   Eval      h2colo = 62
065840051124     c                   Eval      *In90 = *On
065850051118     c                   EndIf
065860051118      * --> controllo
065870051118     c                   Clear                   ds15
065880051118     c                   Eval      kcod = '15'
065890051118     c                   Eval      kkey = v2csta
065900051118     c     kTabel        Chain     Tabel00f
065910051118     c                   If        Not %Found(Tabel00f) or
065920051125     c                             tblflg <> *Blanks
065930051118     c                   Eval      *In28 = *On
065940051130     c                   Eval      h2riga = 09
065950051118     c                   Eval      h2colo = 62
065960051118     c                   Eval      v2cmsg = msg(17)
065970051118     c                   Leavesr
065980051118     c                   EndIf
065990051125     c                   Eval      ds15 = tbluni
066000051118
066010051129      * --> controllo
066020151014     c**                 If        v2civa <> *Blanks
066030061030      * devo richiamare il nuovo driver fatto per controllare il codice
066040061030      * fiscale e la partita iva
066050151014     c                   clear                   xcfiva1ds
066060061030     c                   eval      xcfivamod = 'P'
066070061030     c                   eval      xcfivapar = v2civa
066080061030     c                   eval      xcfivanar = v2csta
066090061030     c                   eval      xcfivaprv = v2cprv
066100151014     c                   eval      xcfivacap = v2ccae
066110151014     c                   eval      xcfivaloc = v2ccit
066120151014     c                   eval      xcfivapiva= v2civa
066130151014     c                   call      'XCFIVAR1'
066140151014     c                   parm                    xcfiva1ds
066150061106      * --> errata forzabile
066160061106     c                   If        xcfivaflg = -9 and wforzaiva = *off
066170061106     c                   Eval      *In28 = *On
066180061106     c                   Eval      h2riga = 12
066190061106     c                   Eval      h2colo = 52
066200061106     c                   Eval      v2cmsg = xcfivamsg
066210061106     c                   Eval      v2cmsg = %trim(v2cmsg) + ' Enter x forzare'
066220061106     c                   eval      wforzaiva = *on
066230061106     c                   Leavesr
066240061106     c                   EndIf
066250061030      * --> errore
066260061106     c                   if        xcfivaflg < *zeros and xcfivaflg <> -9
066270051118     c                   Eval      *In28 = *On
066280051202     c                   Eval      h2riga = 12
066290051118     c                   Eval      h2colo = 52
066300061030     c                   eval      v2cmsg = xcfivamsg
066310051118     c                   Leavesr
066320051118     c                   EndIf
066330061106      * se non impostato devo riportare il codice iso della partita iva che
066340061106      * mi viene passato dal pgm di controllo
066350061114     c                   if        v2ivaeu <> ivaeu
066360061106     c                   eval      v2ivaeu = ivaeu
066370061106     c                   eval      *in39 = *on
066380061106     c                   eval      *in90 = *on
066390061114     c                   endif
066400151014      * PARTITA IVA
066410151014      * --> obbligatoria (se non è gestione visite/offerte)
066420151014      * --> non è però obbligatoria se cliente Italia e nazione del
066430151014      *     sottoconto intestazione fattura estera
066440151014      * Escludo i casi previsti di NON inserimento
066450151014     c                   If        v2civa = *Blanks and Not *In06
066460151014     c                             and xcfivaflg<>9
066470151014     c                   exsr      sr_repscfsta
066480151014     c                   if        v2csta<>*blanks or w_scfsta=*blanks
066490151014     c                   Eval      *In28 = *On
066500151014     c                   Eval      h2riga = 12
066510151014     c                   Eval      h2colo = 52
066520151014     c                   Eval      v2cmsg = msg(18)
066530151014     c                   Leavesr
066540151014     c                   EndIf
066550151014     c                   EndIf
066560151014
066570151014     c* Se provengo dalle visite e il relativo potenziale non ha p.iva
066580080306     c* errore forzabile se inserito una p.iva         già presente su altro
066590080306     c* potenziale
066600151014     c                   If        v2civa <> *Blanks
066610080306     c                   if        *in06 and v2ccpo<>*zeros
066620080306     c                             and v2ivaeu<>'$$'
066630080306     c     v2ccpo        Chain(n)  Tncpo01l
066640080306     c                   if        %found(tncpo01l) and cpopiv=*blanks
066650080306     c                             and v2civa<>saviva
066660080306     c                             and v2civa<>cod_forziva
066670080306     c                   clear                   savrag
066680080307     c                   if        v2ivaeu<>*blanks
066690080306     c                   eval      codice=v2civa
066700080306     c                   else
066710080306     c                   eval      codice=v2ivait
066720080306     c                   endif
066730080306     c                   exsr      sr_PivAltroP
066740080306     c                   exsr      sr_msgiva
066750080306     c   28              leavesr
066760080306     c                   endif
066770080307     c                   endif
066780061106
066790060105      * --> controllo se c'è già un cliente con la stessa p.IVA
066800060105      *     se immissione di un nuovo codice o se conferma offerta
066810060119if  1c                   If        Not *In06 and (*In01 or *In07)
066820060221     c                             and v2ivaeu <> '$$'
066830060109     c                   If        v2ivaeu = *Blanks
066840060109     c                   Movel     v2ivait       kindiva
066850060109     c                   Else
066860060109     c                   Movel     v2civa        kindiva
066870060109     c                   EndIf
066880060105     c     kCnind        Setll     Cnind02l
066890060105      * non trovo nessun altra partita IVA esco
066900060105     c                   If        Not %Equal
066910060105     c                   Leavesr
066920060105     c                   EndIf
066930060105do  2c                   Do        *Hival
066940060105     c     kCnind        Reade     Cnind02l
066950060105      * fine file esco
066960060105     c                   If        %Eof(Cnind02l)
066970060105     c                   Leave
066980060105     c                   EndIf
066990060105      * deve avere codice cliente diverso
067000060301     c                   If        ww_indksc = v1cksc
067010060105     c                   Iter
067020060105     c                   EndIf
067030060105      * controllo lo stato del credito
067040081006      * lo faccio con una routine per emettere una window nel caso di
067050081006      * messaggio info
067060081006     c                   exsr      sr_ctrstc
067070081006     c                   if        wforzacon = *On
067080081006     c                   leavesr
067090081006     c                   endif
067100060105    2c                   EndDo
067110060105    1c                   EndIf
067120151015     c
067130151015     c                   EndIf
067140051118
067150051118     c                   EndSr
067160081006
067170081006      *------------------------------------------------------------------------*
067180081006      * CONTROLLO STATO DEL CREDITO
067190081006      *------------------------------------------------------------------------*
067200081006     c     sr_ctrstc     begsr
067210081006
067220081006     c                   Clear                   Tibs69ds
067230081006     c                   Move      ww_indksc     I69kcp
067240081006     c                   Call      'TIBS69R'
067250081006     c                   Parm                    Tibs69ds
067260081006     c                   Parm                    ds_cnaco
067270081006     c                   Parm                    ds_cnind
067280081006     c                   Parm                    ds_cnclp
067290081006     c                   Parm                    ds_fncls
067300081006     c                   Eval      wtibs69 = *On
067310081006      * errore forzabile per stato del credito trovato
067320081006      * emesso con window e f6 di conferma
067330081006     c                   If        w_clpcon <> *Blanks and wforzacon = *Off
067340090803      * decodifico stato del credito
067350090803     c                   clear                   w_ds4w
067360090803     c                   eval      kcod = '4W'
067370090803     c                   eval      kkey = w_clpcon
067380090803     c                   eval      w5ccon = %subst(w_clpcon:2:2)
067390090803     c     ktabel        chain     tabel00f
067400090803     c                   if        %found(tabel00f) and
067410090803     c                             tblflg = *blanks
067420090803     c                   eval      w_ds4w = tbluni
067430090803     c                   endIf
067440090803     c                   Eval      w5dcon = w_§4wdes
067450081006     c                   do        *hival
067460081006     c                   exfmt     ta60w05
067470081006     c   kf              leave
067480081006     c                   enddo
067490081006     c                   Eval      wforzacon = *On
067500081006     c                   EndIf
067510081006
067520081006     c                   endsr
067530051122
067540051122      *------------------------------------------------------------------------*
067550051122      * CONTROLLO BLOCCO CLIENTE
067560051122      *------------------------------------------------------------------------*
067570051122     c     Sr_Ctrblc     BegSr
067580051118
067590051122      * STATO DEL CREDITO
067600051122      * --> Ricerca
067610051122     c                   Clear                   v2dcon
067620051122     c                   Eval      wpos = %scan('?':v2ccon)
067630051122     c                   If        wpos > 0
067640051122     c                   Eval      kcod = '4W'
067650051122     c                   call      'TRUL19R'
067660051122     c                   parm                    kcod
067670051122     c                   parm                    ordina
067680051122     c                   parm                    kkey
067690051122     c                   parm                    comando
067700051124     c                   Eval      w003a = kkey
067710051124     c                   Move      w003a         v2ccon
067720051202     c                   Eval      h2riga = 17
067730051122     c                   Eval      h2colo = 28
067740051124     c                   Eval      *In90 = *On
067750051122     c                   EndIf
067760051122
067770051122      * --> Controllo
067780051122     c                   Clear                   v2dcon
067790051122     c                   Clear                   ds4w
067800051122     c                   Eval      kcod = '4W'
067810051124     c                   Move(p)   v2ccon        w003a
067820051124     c                   Eval      kkey = w003a
067830051122     c     kTabel        Chain     Tabel00f
067840051122     c                   If        Not %Found(Tabel00f) or
067850051125     c                             tblflg <> *Blanks
067860051122     c                   Eval      *In28 = *On
067870051202     c                   Eval      h2riga = 17
067880051122     c                   Eval      h2colo = 28
067890051122     c                   Eval      v2cmsg = msg(23)
067900051122     c                   Leavesr
067910051122     c                   EndIf
067920051125     c                   Eval      ds4w = tbluni
067930051122     c                   Eval      v2dcon = §4wdes
067940051122
067950051122      * Se sono in filiale devo usare solo stati del credito autorizzati e
067960051122      * senza passaggio al contenzioso
067970130325      * da 25/03/2013 abbiamo deciso che anche in sede non è modificale, solo
067980130325      * da ProJ
067990051122     c                   If        (§4wmbl = 'N' or §4wctz = 'S') and
068000130325     c                             (v2ccon <> %subst(clpcon:2:2) or *In01)
068010051122     c                   Eval      *In28 = *On
068020051202     c                   Eval      h2riga = 17
068030051122     c                   Eval      h2colo = 28
068040051122     c                   Eval      v2cmsg = msg(24)
068050051122     c                   Leavesr
068060051122     c                   EndIf
068070051122
068080051122      * --> Imposto i blocchi
068090051122      * Se sono in filiale proteggo lo stato del credito se non è modificabile
068100120928      * da p.o.
068110130325      * PER TUTTI
068120130325     c                   If        §4wmbl = 'N'
068130060220     c                   Eval      *In19 = *On
068140051122     c                   EndIf
068150120928      * Se sono in filiale proteggo il blocco pagamenti se non è modificabile
068160120928      * da p.o.
068170120928     c                   If        Not *In09 and §4wmbp = 'N'
068180120928     c                   Eval      *In81 = *On
068190120928     c                   EndIf
068200120925      * Se sono in filiale proteggo il blocco cliente se non è modificabile
068210120925      * da p.o.
068220120928     c                   If        Not *In09 and §4wmtb = 'N'
068230120925     c                   Eval      *In79 = *On
068240120925     c                   EndIf
068250121002
068260121002      * se stato del credito variato
068270121002     c                   IF        V2Ccon <> %subst(CLPcon:2:2)
068280051122      * Se il cliente non è bloccato e lo stato del credito prevede il blocco
068290051122      * lo faccio io ora in automatico
068300051122     c                   If        v2cabl = *Blanks and §4wtbl <> *Blanks
068310051122     c                   Eval      v2cabl = §4wtbl
068320130322     c                   eval      V2Cabledp = §4wtbl
068330051122     c                   EndIf
068340051122
068350051122      * Se non c'è la causale del blocco la imposto io ora in automatico
068360051122     c                   If        (v2cblc = *Blanks or v2cblc = *Zeros) and
068370051122     c                              §4wblc <> *Blanks
068380051122     c                   Eval      v2cblc = §4wblc
068390051122     c                   EndIf
068400121002     c                   ENDIF
068410051122
068420051122      * BLOCCO CLIENTE
068430130322     c                   IF        V2Cabl <> *blanks and V2Cabl <> '8'
068440130322     c                             and not *in79
068450130322     c                   Eval      *In28 = *On
068460130322     c                   Eval      h2riga = 18
068470130322     c                   Eval      h2colo = 28
068480130322     c                   Eval      V2Cmsg = 'Blocco cliente errato'
068490130322     c                   Leavesr
068500130322     c                   ENDIF
068510051122      * --> ricerca
068520051122     c                   Clear                   v2dblc
068530051122     c                   Eval      wpos = %scan('?':v2cblc)
068540051122     c                   If        wpos > 0
068550121127     c                   goto      no19
068560051122     c                   Eval      kcod = 'BI'
068570051122     c                   call      'TRUL19R'
068580051122     c                   parm                    kcod
068590051122     c                   parm                    ordina
068600051122     c                   parm                    kkey
068610051122     c                   parm                    comando
068620051122     c                   Eval      v2cblc = kkey
068630121127     c     no19          tag
068640121127     c                   clear                   kpjbu
068650121127     c                   call      'TRTBBIR'
068660121127     c                   parm                    kpjba
068670121127     c                   eval      v2cblc = %subst(kpjbu:1:3)
068680051202     c                   Eval      h2riga = 18
068690051122     c                   Eval      h2colo = 42
068700051124     c                   Eval      *In90 = *On
068710051122     c                   EndIf
068720051122
068730051122      * --> Controllo
068740051122     c                   If        v2cblc <> *Blanks
068750110323     c                   Clear                   dsbi
068760051122     c                   Clear                   v2dblc
068770051122     c                   Eval      kcod = 'BI'
068780051122     c                   Eval      kkey = v2cblc
068790051122     c     kTabel        Chain     Tabel00f
068800051122     c                   If        Not %Found(Tabel00f) or
068810051125     c                             tblflg <> *Blanks
068820051122     c                   Eval      *In28 = *On
068830051202     c                   Eval      h2riga = 18
068840051122     c                   Eval      h2colo = 42
068850051122     c                   Eval      v2cmsg = msg(25)
068860051122     c                   Leavesr
068870051122     c                   EndIf
068880110323     c                   Eval      dsbi = tbluni
068890110323     c                   Eval      v2dblc = §bides
068900121127      * causale blocco non piuù utilizzabile
068910121127     c                   IF        §BIuso = 'N'
068920121127     c                   Eval      *In28 = *On
068930121127     c                   Eval      h2riga = 18
068940121127     c                   Eval      h2colo = 42
068950121127     c                   Eval      v2cmsg = 'Causale non più utilizzabile'
068960121127     c                   Leavesr
068970121127     c                   ENDIF
068980051124     c                   EndIf
068990051122
069000051122      * Se cliente bloccato la causale blocco è obbligatoria
069010051122     c                   If        v2cabl <> *Blanks and v2cblc = *Blanks
069020130325     c                             and v2cabl <> '*'
069030051122     c                   Eval      *In28 = *On
069040051202     c                   Eval      h2riga = 18
069050051122     c                   Eval      h2colo = 42
069060051122     c                   Eval      v2cmsg = msg(26)
069070051122     c                   Leavesr
069080051122     c                   EndIf
069090051122
069100051122      * Se immessa la causale blocco il cliente deve essere bloccato
069110051122     c                   If        v2cabl = *Blanks and v2cblc <> *Blanks
069120051122     c                   Eval      *In28 = *On
069130051202     c                   Eval      h2riga = 18
069140051122     c                   Eval      h2colo = 28
069150051122     c                   Eval      v2cmsg = msg(27)
069160051122     c                   Leavesr
069170051122     c                   EndIf
069180110407
069190110407      * Se pgm richiamato per blocco cliente il blocco deve esserci
069200110407     c                   If        $Blocco and V2cabl = *blanks
069210110407     c                   Eval      *In28 = *On
069220110407     c                   Eval      h2riga = 18
069230110407     c                   Eval      h2colo = 28
069240110407     c                   Eval      v2cmsg = 'Immettere il blocco cliente'
069250110407     c                   Leavesr
069260110407     c                   EndIf
069270051122
069280051122     c                   EndSr
069290090616
069300090616      *------------------------------------------------------------------------*
069310090616      * CONTROLLO SOLLECITO
069320090616      *------------------------------------------------------------------------*
069330090616     c     sr_ctrsol     begsr
069340090616
069350090616     c                   reset                   trulsoli1
069360090616     c                   eval      trulsoli1.kut = codut
069370090616     c                   eval      trulsoli1.kcc = v1ckcc
069380090616     c                   eval      trulsoli1.ksc = v1cksc
069390090616     c                   evalr     trulsoli1.con = v2ccon
069400090616     c                   eval      rqsdatasize = %size(trulsoli1)
069410090616     c                   eval      rpydatasize = %size(trulsolo1)
069420090616     c                   call      'TRULSOLR'
069430090616     c                   parm      *blanks       rqsopcode
069440090616     c                   parm      *zeros        rpyesito
069450090616     c                   parm      'TRULSOLI1'   rqsformatname
069460090616     c                   parm                    trulsoli1
069470090616     c                   parm                    rqsdatasize
069480090616     c                   parm      'TRULSOLO1'   rpyformatname
069490090616     c                   parm                    trulsolo1
069500090616     c                   parm                    rpydatasize
069510090617
069520090617      * Immissione
069530090617     c                   if        *in01
069540090617     c                   if        rpyesito = *zeros
069550090617     c                   eval      v2csol = trulsolo1.sol
069560090617     c                   else
069570090617     c                   eval      v2csol = 'S'
069580090617     c                   endif
069590090617     c                   endif
069600090617
069610090617      * Modifica
069620090617     c                   if        rpyesito = *zeros and not *in01 and
069630090617     c                             v2csol <> trulsolo1.sol and
069640090617     c                             trulsolo1.solmod = 'N'
069650090617     c                   Eval      *In28 = *On
069660111115     c                   Eval      h2riga = 19
069670090617     c                   Eval      h2colo = 28
069680090617     c                   if        trulsolo1.sol = 'S'
069690090617     c                   Eval      v2cmsg = msg(83)
069700090617     c                   Leavesr
069710090617     c                   else
069720090617     c                   Eval      v2cmsg = msg(84)
069730090617     c                   Leavesr
069740090617     c                   endif
069750090617     c                   endif
069760090616
069770090616     c                   endsr
069780051122
069790051118      *------------------------------------------------------------------------*
069800051118      * CONTROLLO CODICE COMMERCIALE
069810051118      *------------------------------------------------------------------------*
069820051118     c     Sr_Ctrage     BegSr
069830051118
069840051118      * Ricerca
069850051130     c                   Clear                   v3dage
069860051130     c                   Eval      wpos = %scan('?':v3cage)
069870051118     c                   If        wpos > 0
069880051130     c                   Eval      v3cage = *All'0'
069890130808     c                   clear                   TRMK43ds
069900130806     c  n06              movel     v1cksc        iMK43fil
069910130806     c   06              movel     ta60cli       iMK43fil
069920130806     c                   call      'TRMK43R'
069930130806     c                   parm                    kpjba
069940130806     c                   parm                    TRMK43ds
069950130806     c                   if        oMK43err = *off  and  oMK43fxx = *blank
069960130806     c                   movel     oMK43cmm      v3cage
069970130806     c                   eval      v3dage = oMK43des
069980130806     c                   endif
069990051130     c                   Eval      h3riga = 07
070000051130     c                   Eval      h3colo = 28
070010051124     c                   Eval      *In90 = *On
070020051118     c                   EndIf
070030051122
070040051118      * Controllo (obbligatorio)
070050051130     c                   Clear                   v3dage
070060130806     c                   If        %check(digits:V3Cage) > 0
070070130806     c                   eval      *In28 = *On
070080130806     c                   eval      h3riga = 07
070090130806     c                   eval      h3colo = 28
070100130806     c                   eval      V3Cmsg = Msg(20)
070110130806     c                   leavesr
070120130806     c                   EndIf
070130130806     c                   move      V3Cage        CMMcod
070140130806     c     CMMcod        chain     AZCMM000
070150130806     c                   If        Not %Found(AZCMM01L)
070160051122     c                   Eval      *In28 = *On
070170051130     c                   Eval      h3riga = 07
070180051130     c                   Eval      h3colo = 28
070190051130     c                   Eval      v3cmsg = msg(20)
070200051122     c                   Leavesr
070210051122     c                   EndIf
070220130806     c                   eval      V3Dage = CMMdes
070230051122
070240051122      * Codice commerciale vari valido solo per 8888 e 9999
070250051130     c                   Move      v3cage        w0040
070260130806     c                   IF        CMMpar = '1' and wksc04 <> 8888
070270130806     c                                          and wksc04 <> 9999
070280051122     c                   Eval      *In28 = *On
070290051130     c                   Eval      h3riga = 07
070300051130     c                   Eval      h3colo = 28
070310051130     c                   Eval      v3cmsg = msg(20)
070320051122     c                   Leavesr
070330051122     c                   EndIf
070340110216
070350110216      /free
070360111123       //?Commerciale "valido" data inizio e fine attività
070370130806         IF  CMMdtIni > dataoggi  or
070380130806             CMMdtFin < dataoggi;
070390111123           *in28 = *on;
070400111123           H3riga = 07;
070410111123           H3colo = 28;
070420111123           V3Cmsg = msg(20);
070430111123           leavesr;
070440111123         ENDIF;
070450111123
070460110216       //?Commerciale 'inattivi' possibile solo se
070470110217       //?immissione
070480110216       //?utente EDP
070490110216       //?cliente incassi da attribuire --> 0001 x 1°livello
070500110216       //?                              --> 1000 x 2°livello
070510110217       //?oppure se
070520110217       //?manutenzione
070530110217       //?NO anagrafica provvisoria
070540110217       //?cliente incassi da attribuire = a cod. in tab Y4O
070550130806         IF  CMMpar = '2';
070560110217           SELECT;
070570110217             WHEN  *in02 and not *in06 and %editc(V1Cksc:'X') = wCodIncAtt;
070580160429             WHEN  *in01 and *in20 and
070590121121                   ((sav_livello = '1' and wksc04 = 0001) or
070600121121                    (sav_livello = '2' and wksc04 = 1000) or
070610121121                    (sav_livello = *blanks and wCodIncAtt = *blanks and
070620121121                    (wksc04 = 1000  or wksc04 = 0001)));
070630110217             OTHER;
070640110216              *in28 = *on;
070650110216              H3riga = 07;
070660110216              H3colo = 28;
070670110216              V3Cmsg = msg(20);
070680110216              leavesr;
070690110217           ENDSL;
070700110217         ENDIF;
070710110216      /end-free
070720051122
070730051122      * P.o. del commerciale congruente con p.o. del cliente
070740051221      * solo se non è gestione anagrafica provvisoria di un nuovo cliente
070750051221     c                   If        Not *In06 or ta60cli > *Zeros
070760051122     c  n06              Movel     v1cksc        w0030
070770051122     c   06              Movel     ta60cli       w0030
070780051130     c                   Movel     v3cage        wpoage
070790051122     c                   If        w0030 <> wpoage
070800051122     c                   Eval      *In28 = *On
070810051130     c                   Eval      h3riga = 07
070820051130     c                   Eval      h3colo = 28
070830051130     c                   Eval      v3cmsg = msg(21)
070840051122     c                   Leavesr
070850051122     c                   EndIf
070860051221     c                   EndIf
070870051122
070880051122      * Se commerciale xxx0999 (cliente inattivo) il cliente va bloccato
070890110216      * salto il controllo
070900110216      * il commerciale 'inattivi' non va usato
070910110216     c                   leavesr
070920051130     c                   Move      v3cage        w0040
070930051122     c                   If        w0040 = 999 and v2cabl = *Blanks
070940051122     c                   Eval      *In28 = *On
070950051130     c                   Eval      v3cmsg = msg(22)
070960051122     c                   Leavesr
070970051122     c                   EndIf
070980051118
070990051118     c                   EndSr
071000051122
071010051122      *------------------------------------------------------------------------*
071020051122      * CONTROLLO CODICE IMPORTANZA CLIENTE
071030051122      *------------------------------------------------------------------------*
071040051122     c     Sr_Ctrclv     BegSr
071050100301
071060100301     c                   Clear                   v3dclv
071070100301
071080100301      * se anagrafica provvisoria da trattativa
071090100301      * non è obbligatorio inserire il codice, se a blank vado a fine controlli
071100100806     c                   if        *in06 and v3cclv = *blanks
071110100301     c                   leavesr
071120100301     c                   endif
071130051122
071140051122      * Ricerca
071150051130     c                   Eval      wpos = %scan('?':v3cclv)
071160051122     c                   If        wpos > 0
071170051122     c                   Eval      kcod = 'IC'
071180051122     c                   call      'TRUL19R'
071190051122     c                   parm                    kcod
071200051122     c                   parm                    ordina
071210051122     c                   parm                    kkey
071220051122     c                   parm                    comando
071230051130     c                   Eval      v3cclv = kkey
071240051130     c                   Eval      h3riga = 08
071250051130     c                   Eval      h3colo = 28
071260051124     c                   Eval      *In90 = *On
071270051122     c                   EndIf
071280051122
071290051124      * --> Controllo (obbligatorio)
071300051130     c                   If        v3cclv = *Blanks
071310051124     c                   Eval      *In28 = *On
071320051130     c                   Eval      h3riga = 08
071330051130     c                   Eval      h3colo = 28
071340051130     c                   Eval      v3cmsg = msg(28)
071350051124     c                   Leavesr
071360051124     c                   EndIf
071370051124
071380051130     c                   Clear                   v3dclv
071390051122     c                   Clear                   dsic
071400051122     c                   Eval      kcod = 'IC'
071410051130     c                   Eval      kkey = v3cclv
071420051122     c     kTabel        Chain     Tabel00f
071430051122     c                   If        Not %Found(Tabel00f) or
071440051125     c                             tblflg <> *Blanks
071450051122     c                   Eval      *In28 = *On
071460051130     c                   Eval      h3riga = 08
071470051130     c                   Eval      h3colo = 28
071480051130     c                   Eval      v3cmsg = msg(29)
071490051122     c                   Leavesr
071500051122     c                   EndIf
071510051125     c                   Eval      dsic = tbluni
071520051130     c                   Eval      v3dclv = §sicde
071530051122
071540051122     c                   EndSr
071550051122
071560051122      *------------------------------------------------------------------------*
071570051122      * CONTROLLO CODICE ISTAT
071580051122      *------------------------------------------------------------------------*
071590051122     c     Sr_Ctritc     BegSr
071600051122
071610051122      * Ricerca
071620051130     c                   Clear                   v3ditc
071630051130     c                   Eval      wpos = %scan('?':v3citc)
071640051122     c                   If        wpos > 0
071650051122     c                   Eval      kcod = '1L'
071660051122     c                   call      'TRUL19R'
071670051122     c                   parm                    kcod
071680051122     c                   parm                    ordina
071690051122     c                   parm                    kkey
071700051122     c                   parm                    comando
071710051130     c                   Eval      v3citc = kkey
071720051130     c                   Eval      h3riga = 09
071730051130     c                   Eval      h3colo = 28
071740051124     c                   Eval      *In90 = *On
071750051122     c                   EndIf
071760051122
071770051122      * --> Controllo (obbligatorio)
071780051130     c                   If        v3citc = *Blanks or v3citc = *Zeros
071790051122     c                   Eval      *In28 = *On
071800051130     c                   Eval      h3riga = 09
071810051130     c                   Eval      h3colo = 28
071820051130     c                   Eval      v3cmsg = msg(30)
071830051122     c                   Leavesr
071840051122     c                   EndIf
071850051122
071860051130     c                   Clear                   v3ditc
071870051122     c                   Clear                   ds1l
071880051124     c                   Eval      kcod = '1L'
071890051130     c                   Eval      kkey = v3citc
071900051122     c     kTabel        Chain     Tabel00f
071910051122     c                   If        Not %Found(Tabel00f) or
071920051125     c                             tblflg <> *Blanks
071930051122     c                   Eval      *In28 = *On
071940051130     c                   Eval      h3riga = 09
071950051130     c                   Eval      h3colo = 28
071960051130     c                   Eval      v3cmsg = msg(31)
071970051122     c                   Leavesr
071980051122     c                   EndIf
071990051125     c                   Eval      ds1l = tbluni
072000051130     c                   Eval      v3ditc = §1ldes
072010051122
072020051122     c                   EndSr
072030140722
072040140722      /free
072050140722
072060140722       //--------------------------------------------------------------
072070140722       //?Controllo Note 02/03       (News/Nominativo),                ?
072080140722       //?               05/06/T5/I5 (Responsabile Trasporti),         ?
072090140722       //?               07/08/T7/I7 (Responsabile Logistica).         ?
072100140722       //--------------------------------------------------------------
072110140722       BEGSR  sr_CtrNote;
072120140722
072130140722         // -?Se NON selezionate: esco?
072140140722         select;
072150140722           when  wRGR  = 'RN'  and  V3Cnt02 = *blank;
072160140722             leavesr;
072170140722           when  wRGR  = 'RT'  and  V3Cnt05 = *blank;
072180140722             leavesr;
072190140722           when  wRGR  = 'RL'  and  V3Cnt07 = *blank;
072200140722             leavesr;
072210140722         endsl;
072220140722
072230140722         // -?Se sono in interrogazione non posso interrogare?
072240140722         //  ?   se non ho dati?
072250140722         if  *in05  and  ( (wRGR = 'RN'  and  Not *in24)  or
072260140722                           (wRGR = 'RT'  and  Not *in26)  or
072270140722                           (wRGR = 'RL'  and  Not *in31) );
072280140722           *in28 = *on;
072290140722           select;
072300140722             when  wRGR = 'RN';
072310140722               H3riga = 15;
072320140722             when  wRGR = 'RT';
072330140722               H3riga = 17;
072340140722             when  wRGR = 'RL';
072350140722               H3riga = 19;
072360140722           endsl;
072370140722           H3colo = 35;
072380140722           V3Cmsg = Msg(52);
072390140722           leavesr;
072400140722         endif;
072410140722
072420140722         // -?Imposto se interrogazione o gestione?
072430140722         if  Not *in05;
072440140722           wRich = 'M';
072450140722         else;
072460140722           wRich = 'V';
072470140722         endif;
072480140722         wRag  = V3Crag;
072490140722         clear  wTnt;
072500140722         //wRGR  = 'RN'       ?(già impostato)?
072510140722
072520140722         exsr  sr_CallTNTA12;
072530140722
072540140722         if  TA12err <> *blank;
072550140722           *in28 = *on;
072560140722           select;
072570140722             when  wRGR = 'RN';
072580140722               H3riga = 15;
072590140722             when  wRGR = 'RT';
072600140722               H3riga = 17;
072610140722             when  wRGR = 'RL';
072620140722               H3riga = 19;
072630140722           endsl;
072640140722           H3colo = 35;
072650140722           V3Cmsg = TA12msg;
072660140722           leavesr;
072670140722         endif;
072680140722
072690140722         // -?Controllo se ora c'è o non c'è più alcuna nota?
072700140722         kNtcNk1 = %editc( DUTkci : 'X' );
072710140722         if  *in70  or  Not *in06;
072720140722           kNtcApl = 'C';
072730140722           kNtcNk1 = %trim(kNtcNk1) + %editc( V1Cksc : 'X' );
072740140722         else;
072750140722           kNtcApl = 'T';
072760140722           kNtcNk1 = %trim(kNtcNk1) + %editc( V1Cnrv : 'X' );
072770140722         endif;
072780140722         clear  kNtcNk2;
072790140722         select;
072800140722           // -?Note 02/03?
072810140722           when  wRGR  = 'RN';
072820140722             *in24 = *off;
072830140722             exsr  sr_CarNoteRN;
072840140722           // -?Note 05/06/T5/I5?
072850140722           when  wRGR  = 'RT';
072860140722             *in26 = *off;
072870140722             exsr  sr_CarNoteRT;
072880140722           // -?Note 07/08/T7/I7?
072890140722           when  wRGR  = 'RL';
072900140722             *in31 = *off;
072910140722             exsr  sr_CarNoteRL;
072920140722         endsl;
072930140722
072940140722         // -?Uscita?
072950140722         select;
072960140722           when  wRGR  = 'RN';
072970140722             clear  V3Cnt02;
072980140722             H3riga = 15;
072990140722           when  wRGR  = 'RT';
073000140722             clear  V3Cnt05;
073010140722             H3riga = 17;
073020140722           when  wRGR  = 'RL';
073030140722             clear  V3Cnt07;
073040140722             H3riga = 19;
073050140722         endsl;
073060140722         H3colo = 35;
073070140722
073080140722         *in90 = *on;
073090140722
073100140722       ENDSR;
073110140722
073120140722       //--------------------------------------------------------------
073130140722       //?Visualizzazione Contatti/Rubrica del cliente.                ?
073140140722       //--------------------------------------------------------------
073150140722       BEGSR  sr_CallTNTA12;
073160140722
073170140722         clear  TNTA12ds;
073180140722
073190140722         // -?Impostazione parametri?
073200140722         TA12ric  = wRich;
073210140722         if  *in70  or  Not *in06;
073220140722           TA12apl  = 'C';
073230140722           TA12ksc  = V1Cksc;
073240140722         else;
073250140722           TA12apl  = 'T';
073260140722           TA12nrv  = V1Cnrv;
073270140722         endif;
073280140722         TA12rag  = wRag;
073290140722         TA12tnt  = wTnt;
073300140722         TA12gcli = '1';
073310140722         if  *in01   or
073320140722            (Not *in01  and  Not *in05  and
073330140722             %parms() > 1    and
073340140722             TA60icli = '1');
073350140722           TA12icli = '1';
073360140722           TA12cmm  = %int(V3Cage);
073370140722         endif;
073380140722         TA12rgr  = wRGR;
073390140722
073400140722         // -?Richiamo *pgm TNTA12R?
073410140722         tnta12r ( KPJBA : TNTA12ds );
073420140722
073430140722       ENDSR;
073440140722
073450140722      /end-free
073460051125
073470051125      *------------------------------------------------------------------------*
073480051125      * CONTROLLO CODICE SOTTOCONTO FATTURAZIONE
073490051125      *------------------------------------------------------------------------*
073500051125     c     Sr_Ctrscf     BegSr
073510100127
073520100127      * gestisco il '?'
073530100729     c                   IF        %scan('?':v4cscf) > 0
073540100312      * e se c'è la partita IVA e non è $$
073550100312     c                             and v2civa <> *blanks and
073560100312     c                             v2ivaeu <> '$$'
073570100128     c                   exsr      sr_ta40
073580100127     c                   eval      h4riga = 07
073590100127     c                   eval      h4colo = 36
073600100127     c                   eval      *in90 = *on
073610100312     c                   leavesr
073620100127     c                   ENDIF
073630051125
073640051221      * Obbligatorio
073650100128     c                   If        (v4cscf = *Zeros or v4cscf = *blanks)
073660051221      * se non sono in gestione anagrafica provvisoria
073670051221     c                             and Not *In06
073680051125     c                   Eval      *In28 = *On
073690051130     c                   Eval      h4riga = 07
073700051130     c                   Eval      h4colo = 36
073710051130     c                   Eval      v4cmsg = msg(33)
073720051125     c                   Leavesr
073730051125     c                   EndIf
073740100127
073750100127      * deve essere numerico
073760100127     c                   IF        %check(digits:V4cscf) > 0
073770100127     c                   eval      *in28 = *on
073780100127     c                   eval      h4riga = 07
073790100127     c                   eval      h4colo = 36
073800100127     c                   eval      v4cmsg = msg(35)
073810100127     c                   leavesr
073820100127     c                   ENDIF
073830100316
073840100316      * se sono in immissione (non da trattativa)
073850100316      * non controllo esistenza del codice sottoconto fattura se = al
073860100316      * codice cliente che sto inserendo
073870170516      // non faccio questo controllo se cliente LOGISTICA
073880170516        IF  not ClienteLog;
073890100316     c                   if        *in01 and not *in06 and
073900100316     c                             %dec(v4cscf:7:0) = v1cksc
073910100316     c                   eval      v4dscf = v2crag
073920100316     c                   leavesr
073930100316     c                   endif
073940170516       ENDIF;
073950051125
073960051125      * Controllo se modificato il codice
073970170117      * o se convalida trattativa
073980100127     c                   If        v4cscf <> savscf
073990170117     c                             or *in07
074000051130     c                   Clear                   v4dscf
074010051125      * Controllo
074020051125     c                   Clear                   Tibs69ds
074030051130     c                   Move      v4cscf        I69kac
074040161129     c                   Move      v4cscf        I69kcp
074050051125     c                   Call      'TIBS69R'
074060051125     c                   Parm                    Tibs69ds
074070051125     c                   Parm                    ds_cnaco
074080051125     c                   Parm                    ds_cnind
074090051125     c                   Parm                    ds_cnclp
074100051125     c                   Parm                    ds_fncls
074110051125     c                   Eval      wtibs69 = *On
074120051125
074130051125      * Il sottoconto deve esistere
074140051125     c                   If        o69err <> *Blanks
074150051125     c                   Eval      *In28 = *On
074160051130     c                   Eval      h4riga = 07
074170051130     c                   Eval      h4colo = 36
074180051130     c                   Eval      v4cmsg = msg(35)
074190051125     c                   Leavesr
074200051125     c                   EndIf
074210051125
074220051125      * Non deve essere annullato
074230051125     c                   If        w_acoflg <> *Blanks
074240051125     c                   Eval      *In28 = *On
074250051130     c                   Eval      h4riga = 07
074260051130     c                   Eval      h4colo = 36
074270051130     c                   Eval      v4cmsg = msg(36)
074280051125     c                   Leavesr
074290051125     c                   EndIf
074300051125
074310051125      * Non si può inserire un sottoconto 8888 o 9999
074320070704      * E' possibile solo per clienti 8888 e 9999
074330070704     c                   Move      v4cscf        w0040
074340070704     c                   move      v1cksc        w0040ksc
074350070704     c                   if        *in06='1'
074360070704     c                             or (w0040ksc=8888 and w0040<>8888)
074370070704     c                             or (w0040ksc=9999 and w0040<>9999)
074380070704     c                             or (w0040ksc<>8888 and w0040ksc<>9999)
074390051125     c                   If        w0040 = 8888 or w0040 = 9999
074400051125     c                   Eval      *In28 = *On
074410051130     c                   Eval      h4riga = 07
074420051130     c                   Eval      h4colo = 36
074430051130     c                   Eval      v4cmsg = msg(34)
074440051125     c                   Leavesr
074450051125     c                   EndIf
074460070704     c                   EndIf
074470161130      /free
074480161130       //?Non posso immettere un codice sottoconto fattura in contenzioso
074490170117        IF  %dec(v4cscf:7:0) <> v1cksc;
074500170117          clear TIBS69DS;
074510170117          I69kcp = %dec(V4Cscf:7:0);
074520170117          tibs69r (TIBS69DS:ds_CNACO:ds_CNIND:ds_CNCLP:ds_FNCLS);
074530170117          wtibs69 = *on;
074540170117          IF  w_CLPcon <> *blanks;
074550170117            clear w_ds4W;
074560170117            kcod = '4W';
074570170117            kkey = w_CLPcon;
074580170117            chain (codut:kcod:kkey) TABEL00F;
074590170117            IF  %found(TABEL00F) and TBLflg = *blanks;
074600170117              w_ds4W = TBLuni;
074610170117            ENDIF;
074620170117            IF  w_§4Wtip <> *blanks;
074630170117              *in28 = *on;
074640170117              H4riga = 07;
074650170117              H4colo = 36;
074660170117              V4Cmsg = msg(73);
074670170117              leavesr;
074680170117            ENDIF;
074690170117          ENDIF;
074700161130        ENDIF;
074710161130      /end-free
074720051125
074730051125      * Cerco il network del codice
074740051130     c                   Movel     v4cscf        w0030
074750051125     c                   Clear                   og143
074760051125     c     w0030         Chain     Azorg01l
074770051125     c                   If        %Found(Azorg01l)
074780051125     c                   Eval      og143 = orgde3
074790051125     c                   EndIf
074800120806      * Se non trovo il network e sono in anafrafica provvisoria vado avanti
074810120806     c                   If        not %found(AZORG01L) and not *in06
074820120806     c                             and Not *In06
074830120806     c                   Eval      *In28 = *On
074840120806     c                   Eval      h4riga = 07
074850120806     c                   Eval      h4colo = 36
074860120806     c                   Eval      v4cmsg = msg(35)
074870120806     c                   Leavesr
074880120806     c                   ENDIF
074890081119     c                   eval      savntwscf = §ogntw
074900170516
074910170516       // Per i clienti LOG come sottoconto intestazione fattura accetto
074920170516       // solo codici di spedizione BRT
074930170516         IF  ClienteLog and §OGntw = 'LOG';
074940170516           *in28 = *on;
074950170516           H4riga = 07;
074960170516           H4colo = 36;
074970170516           V4Cmsg = msg(34);
074980170516           leavesr;
074990170516         ENDIF;
075000160905
075010160905      * Se non sono autorizzato non posso utilizzare lienti con ntw LOG
075020160905     c                   IF         §UTEKscLog <> 'S' and
075030160905     c                              §OGntw = 'LOG'
075040160905     c                   Eval      *In28 = *On
075050160905     c                   Eval      h4riga = 07
075060160905     c                   Eval      h4colo = 36
075070160905     c                   Eval      v4cmsg = msg(34)
075080160905     c                   Leavesr
075090160905     c                   ENDIF
075100051125
075110051125      * Se sono in filiale non posso inserire codici con ntw LOG o XXX
075120051125     c                   If        Not *In09 and
075130160905     c                             §ogntw = 'XXX'
075140051125     c                   Eval      *In28 = *On
075150051130     c                   Eval      h4riga = 07
075160051130     c                   Eval      h4colo = 36
075170051130     c                   Eval      v4cmsg = msg(34)
075180051125     c                   Leavesr
075190051125     c                   EndIf
075200051220
075210051220      * Non posso modificare un sottoconto fattura con p.o. 046
075220051220     c                   Movel     savscf        w0030
075230051220     c                   If        w0030 = 046
075240051220     c                   Eval      *In28 = *On
075250051220     c                   Eval      h4riga = 07
075260051220     c                   Eval      h4colo = 36
075270051220     c                   Eval      v4cmsg = msg(71)
075280051220     c                   Leavesr
075290051220     c                   EndIf
075300051220
075310081119      * Non posso inserire un sottoconto fattura con ntw LOG se ksc non
075320081119      * ha ntw LOG
075330081119     c                   Movel     v1cksc        w0030
075340081119     c                   Clear                   og143
075350081119     c     w0030         Chain     Azorg01l
075360081119     c                   If        %Found(Azorg01l)
075370081119     c                   Eval      og143 = orgde3
075380081119     c                   EndIf
075390081119     c                   if        §ogntw <> 'LOG' and savntwscf = 'LOG'
075400081119     c                   Eval      *In28 = *On
075410081119     c                   Eval      h4riga = 07
075420081119     c                   Eval      h4colo = 36
075430081119     c                   Eval      v4cmsg = msg(34)
075440081119     c                   Leavesr
075450081119     c                   EndIf
075460100127
075470100127      * devo ricontrollare la partita iva e il codice fiscale
075480100127      * perchè i controlli di questi 2 dati sono legati anche al codice
075490100127      * fatturazione
075500100127     c                   exsr      sr_ctrCDF
075510100127      * se torna errore avviso
075520100127     c                   IF        *in28
075530100127     c                   eval      v4cmsg = 'Messaggi in sospeso su -
075540100127     c                             altre videate.'
075550100127     c                   leavesr
075560100127     c                   ENDIF
075570100127     c                   exsr      sr_ctrIVA
075580100127      * se torna errore avviso
075590100127     c                   IF        *in28
075600100127     c                   eval      v4cmsg = 'Messaggi in sospeso su -
075610100127     c                             altre videate.'
075620100127     c                   leavesr
075630100127     c                   ENDIF
075640051125
075650051125      * Decodifico
075660051130     c                   Eval      v4dscf = w_acorag
075670051130     c                   Eval      savscf = v4cscf
075680051128
075690051125     c                   EndIf
075700051125
075710051125     c                   EndSr
075720051128
075730051128      *------------------------------------------------------------------------*
075740051128      * CONTROLLO FLAG FATTURA UNIFICATA
075750051128      *------------------------------------------------------------------------*
075760051128     c     Sr_Ctruni     BegSr
075770100127
075780100127     c                   move      v4cscf        w0070
075790051128
075800051128      * Se il codice sottoconto fatturazione è diverso dal codice cliente e
075810051128      * il flag di unificazione fattura non è stato impostato emetto un msg. inf
075820100127     c                   If        w0070  <> v1cksc and v4cuni = *Blanks and
075830100127     c                             wfscf = *Blanks and w0070  <> *Zeros
075840051128      * e non è da offerta
075850051128     c                             and not *In06
075860051128     c                   Eval      *In28 = *On
075870051130     c                   Eval      h4riga = 08
075880051130     c                   Eval      h4colo = 36
075890051130     c                   Eval      v4cmsg = msg(45)
075900051128     c                   Eval      wfscf = '1'
075910051128     c                   Leavesr
075920051128     c                   EndIf
075930051128
075940051128      * se modificato flag fattura unificata msg info x controllare anche il
075950051128      * flag nel sottoconto fattura
075960051128      * se immissione messaggio solo se codice cliente diverso da sottoconto fat
075970051128      * se manutenzione diversifico i messaggi
075980051130     c                   If        v4cuni <> savuni
075990051130     c                   Eval      savuni = v4cuni
076000051128     c                   Select
076010100127     c                   When      w0070  <> v1cksc
076020051128     c                   Eval      *In28 = *On
076030051130     c                   Eval      h4riga = 08
076040051130     c                   Eval      h4colo = 36
076050051130     c                   Eval      v4cmsg = msg(46)
076060051128     c                   Leavesr
076070100127     c                   When      w0070  = v1cksc and Not *In01
076080051128     c                             and Not *In07
076090051128     c                   Eval      *In28 = *On
076100051130     c                   Eval      h4riga = 08
076110051130     c                   Eval      h4colo = 36
076120051130     c                   Eval      v4cmsg = msg(47)
076130051128     c                   Leavesr
076140051128     c                   EndSl
076150051128     c                   EndIf
076160051128
076170051128     c                   EndSr
076180051125
076190051125      *------------------------------------------------------------------------*
076200051125      * CONTROLLO TIPO FATTURA
076210051125      *------------------------------------------------------------------------*
076220051125     c     Sr_Ctrtft     BegSr
076230051125
076240051125      * Ricerca
076250051130     c                   Clear                   v4dtft
076260051130     c                   If        v4ctft = '?'
076270051125     c                   Clear                   tibs02ds
076280051125     c                   Eval      t02mod = 'R'
076290051125     c                   Eval      t02sif = knsif
076300051125     c                   Eval      t02cod = 'TFT'
076310051125     c                   Call      'TIBS02R'
076320051125     c                   Parm                    kpjba
076330051125     c                   Parm                    tibs02ds
076340051125     c                   If        t02err = *Blanks
076350051130     c                   Eval      v4ctft = t02ke1
076360051125     c                   Eval      dtft = t02uni
076370051130     c                   Eval      v4dtft = §tftdes
076380051130     c                   Eval      h4riga = 09
076390051130     c                   Eval      h4colo = 36
076400051125     c                   Eval      *In90 = *On
076410051125     c                   Leavesr
076420051125     c                   EndIf
076430051128     c                   EndIf
076440051125
076450051125      * --> Controllo (obbligatorio in tabella non esiste il ' ')
076460051125     c                   Clear                   Tibs02ds
076470051125     c                   Eval      t02mod = 'C'
076480051125     c                   Eval      t02sif = knsif
076490051125     c                   Eval      t02cod = 'TFT'
076500051130     c                   Eval      t02ke1 = v4ctft
076510051125     c                   Call      'TIBS02R'
076520051125     c                   Parm                    kpjba
076530051125     c                   Parm                    Tibs02ds
076540051125     c                   If        t02err <> *Blanks
076550051125     c                   Eval      *In28 = *On
076560051130     c                   Eval      h4riga = 09
076570051130     c                   Eval      h4colo = 36
076580051130     c                   Eval      v4cmsg = msg(37)
076590051125     c                   Leavesr
076600051125     c                   EndIf
076610051125     c                   Eval      dtft = t02uni
076620051130     c                   Eval      v4dtft = §tftdes
076630051125
076640051125      * Controllo uso sede/p.o.
076650051125      * solo se variato
076660051130     c                   If        v4ctft <> savtft
076670051125     c                   Select
076680051125     c                   When      Not *In09 and §tftuti = 'S'
076690051125     c                   Eval      *In28 = *On
076700051130     c                   Eval      h4riga = 09
076710051130     c                   Eval      h4colo = 36
076720051130     c                   Eval      v4cmsg = msg(38)
076730051130     c                   Eval      v4cmsg = %trim(v4cmsg) + ' ' +
076740051125     c                             'filiale'
076750051125     c                   Leavesr
076760051125     c                   When      *In09 and §tftuti = 'F'
076770051125     c                   Eval      *In28 = *On
076780051130     c                   Eval      h4riga = 09
076790051130     c                   Eval      h4colo = 36
076800051130     c                   Eval      v4cmsg = msg(38)
076810051130     c                   Eval      v4cmsg = %trim(v4cmsg) + ' ' +
076820051125     c                             'sede'
076830051125     c                   Leavesr
076840051125     c                   EndSl
076850051125     c                   EndIf
076860090417
076870090417     c                   clear                   conta
076880090417
076890090417      * se non è anagrafica provvisoria
076900090417      * se tipo fattura è distinta verifico se il codice è un sottoconto
076910090417      * fattura e se i codici a lui collegati hanno la fatturazione
076920090417      * settimanale, in questo caso devo dare errore
076930090417     c                   if        not *in06 and
076940100127     c                             %editc(v1cksc:'X') = v4cscf
076950100127     c                             and v4ctft = '1'
076960090417     c/exec sql
076970090417     c+ select count(*) into :conta from cnclp00f where
076980090417     c+ clpkcc = 151 and clpscf = :v1cksc and clpfft = 1
076990090417     c/end-exec
077000090417      * se ho trovato almeno un rcd vuol dire che uno dei figli
077010090417      * legati al sottoconto fatturazione che sto modificando ha
077020090417      * la frequenza settimanale
077030090417     c                   if        conta > *zeros
077040090417     c                   eval      *in28 = *on
077050090417     c                   eval      h4riga = 09
077060090417     c                   eval      h4colo = 36
077070090417     c                   eval      v4cmsg = msg(82)
077080090417     c                   leavesr
077090090417     c                   endif
077100090417     c                   endif
077110051125
077120051125     c                   EndSr
077130051125
077140051125      *------------------------------------------------------------------------*
077150051125      * CONTROLLO FREQUENZA FATTURA
077160051125      *------------------------------------------------------------------------*
077170051125     c     Sr_Ctrfft     BegSr
077180051125
077190051125      * Ricerca
077200051130     c                   Clear                   v4dfft
077210051130     c                   If        v4cfft = '?'
077220051125     c                   Eval      kcod = 'FF'
077230051125     c                   call      'TRUL19R'
077240051125     c                   parm                    kcod
077250051125     c                   parm                    ordina
077260051125     c                   parm                    kkey
077270051125     c                   parm                    comando
077280051130     c                   Eval      v4cfft = kkey
077290051130     c                   Eval      h4riga = 10
077300051130     c                   Eval      h4colo = 36
077310051125     c                   Eval      *In90 = *On
077320051125     c                   EndIf
077330051125
077340051125      * --> Controllo (obbligatorio)
077350051130     c                   If        v4cfft = *Blanks
077360051125     c                   Eval      *In28 = *On
077370051130     c                   Eval      h4riga = 10
077380051130     c                   Eval      h4colo = 36
077390051130     c                   Eval      v4cmsg = msg(39)
077400051125     c                   Leavesr
077410051125     c                   EndIf
077420051125
077430051130     c                   Clear                   v4dfft
077440051125     c                   Eval      kcod = 'FF'
077450051130     c                   Eval      kkey = v4cfft
077460051125     c     kTabel        Chain     Tabel00f
077470051125     c                   If        Not %Found(Tabel00f) or
077480051125     c                             tblflg <> *Blanks
077490051125     c                   Eval      *In28 = *On
077500051130     c                   Eval      h4riga = 10
077510051130     c                   Eval      h4colo = 36
077520051130     c                   Eval      v4cmsg = msg(40)
077530051125     c                   Leavesr
077540051125     c                   EndIf
077550051125     c                   Eval      dsff = tbluni
077560051130     c                   Eval      v4dfft = §ffdes
077570051125
077580051125      * Se tipo fattura '1' non è ammessa la frequenza fattura <> dal mensile
077590051220     c                   If        v4ctft = '1' and v4cfft <> '4'
077600051125     c                   Eval      *In28 = *On
077610051130     c                   Eval      h4riga = 10
077620051130     c                   Eval      h4colo = 36
077630051130     c                   Eval      v4cmsg = msg(41)
077640051125     c                   Leavesr
077650051125     c                   EndIf
077660051125
077670120613      * La frequenza fattura settimanale (1) è ammessa solo se
077680120613      * utente abilitato
077690120613     c                   If        v4cfft = '1' and
077700120613     c                             §UTEclpfft <> 'S' and
077710051130     c                             v4cfft <> savfft
077720051125     c                   Eval      *In28 = *On
077730051130     c                   Eval      h4riga = 10
077740051130     c                   Eval      h4colo = 36
077750051130     c                   Eval      v4cmsg = msg(42)
077760051125     c                   Leavesr
077770051125     c                   EndIf
077780090416
077790090416      * se frequenza fattura settimanale controllo il tipo fattura del
077800090416      * sottoconto fatturazione (se diverso)
077810100127     c                   if        v4cfft = '1' and
077820100127     c                             v4cscf <> %editc(v1cksc:'X')
077830100127     c                   move      v4cscf        w0070
077840090416      * il sottoconto fatturazione non deve avere il tipo fattura distinta
077850090417     c/exec sql
077860090417     c+ select clptft into:w_clptft from cnclp00f where
077870100127     c+ clpkcc = 151 and clpksc = :w0070
077880090417     c/end-exec
077890090417     c                   if        sqlcod = 0 and w_clptft = 1
077900090416     c                   eval      *in28 = *on
077910090416     c                   eval      h4riga = 10
077920090416     c                   eval      h4colo = 36
077930090416     c                   eval      v4cmsg = msg(81)
077940090416     c                   leavesr
077950090417     c                   endif
077960090416     c                   endif
077970051125
077980051125     c                   EndSr
077990051125
078000051125      *------------------------------------------------------------------------*
078010051125      * CONTROLLO TIPO DATA FATTURA
078020051125      *------------------------------------------------------------------------*
078030051125     c     Sr_Ctrtdf     BegSr
078040051125
078050051125      * Ricerca
078060051130     c                   Clear                   v4dtdf
078070051130     c                   If        v4ctdf = '?'
078080051125     c                   Eval      kcod = '13'
078090051125     c                   call      'TRUL19R'
078100051125     c                   parm                    kcod
078110051125     c                   parm                    ordina
078120051125     c                   parm                    kkey
078130051125     c                   parm                    comando
078140051130     c                   Eval      v4ctdf = kkey
078150051130     c                   Eval      h4riga = 11
078160051130     c                   Eval      h4colo = 36
078170051125     c                   Eval      *In90 = *On
078180051125     c                   EndIf
078190051125
078200051125      * --> Controllo (obbligatorio)
078210051130     c                   If        v4ctdf = *Blanks
078220051125     c                   Eval      *In28 = *On
078230051130     c                   Eval      h4riga = 11
078240051130     c                   Eval      h4colo = 36
078250051130     c                   Eval      v4cmsg = msg(43)
078260051125     c                   Leavesr
078270051125     c                   EndIf
078280051125
078290051130     c                   Clear                   v4dtdf
078300051125     c                   Eval      kcod = '13'
078310051130     c                   Eval      kkey = v4ctdf
078320051125     c     kTabel        Chain     Tabel00f
078330051125     c                   If        Not %Found(Tabel00f) or
078340051125     c                             tblflg <> *Blanks
078350051125     c                   Eval      *In28 = *On
078360051130     c                   Eval      h4riga = 11
078370051130     c                   Eval      h4colo = 36
078380051130     c                   Eval      v4cmsg = msg(44)
078390051125     c                   Leavesr
078400051125     c                   EndIf
078410051125     c                   Eval      ds13 = tbluni
078420051130     c                   Eval      v4dtdf = §13des
078430051125
078440051125     c                   EndSr
078450051128
078460051128      *------------------------------------------------------------------------*
078470051128      * CONTROLLO SPESE INVIO FATTURA
078480051128      *------------------------------------------------------------------------*
078490051128     c     Sr_Ctrsin     BegSr
078500051128
078510051130     c                   If        v4csin > *Zeros
078520051128     c                   Reset                   xdecds
078530051130     c                   Z-add     v4csin        icdnum
078540051128     c                   Z-add     2             icdnrd
078550051128     c                   Call      'XDEC'
078560051128     c                   Parm                    xdecds
078570051128     c                   If        ocdesi <> *Off
078580051128     c                   Eval      *In28 = *On
078590051130     c                   Eval      h4riga = 12
078600051130     c                   Eval      h4colo = 36
078610051130     c                   Eval      v4cmsg = msg(48)
078620051128     c                   Leavesr
078630051128     c                   EndIf
078640051128     c                   EndIf
078650051128
078660051128     c                   EndSr
078670051128
078680051128      *------------------------------------------------------------------------*
078690051128      * CONTROLLO LUOGO 001 - INVIO FATTURA
078700051128      *------------------------------------------------------------------------*
078710051128     c     Sr_Ctrl001    BegSr
078720051128
078730051130if  1c                   If        v4cl001 <> *Blanks
078740051128
078750060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
078760060203     c                   If        *In05 and Not *In10
078770051128     c                   Eval      *In28 = *On
078780100727     c                   Eval      h4riga = 21
078790060203     c                   Eval      h4colo = 35
078800051130     c                   Eval      v4cmsg = msg(50)
078810051128     c                   Leavesr
078820051128     c                   EndIf
078830071128
078840071128      * se non è interrogazione controllo se luogo utilizzabile
078850071128     c                   if        not *in09 and not *in05 and $ufi001 <> 'S'
078860071128     c                   eval      *In28 = *on
078870100727     c                   eval      h4riga = 21
078880071128     c                   eval      h4colo = 35
078890071128     c                   eval      v4cmsg = msg(76)
078900071128     c                   leavesr
078910071128     c                   endif
078920051128
078930060203      * Imposto se interrogazione o gestione
078940051213     c                   Clear                   tnta81ds
078950060203     c   05              Eval      i81opz = '5'
078960060203     c  n05
078970060203     can 10              Eval      i81opz = '2'
078980051213     c                   Eval      i81cod = '001'
078990051213     c                   Eval      i81cli = v1cksc
079000060216     c                   Clear                   parmf11
079010060216     c                   Eval      i81gcli = '1'
079020060216     c   01              Eval      i81icli = '1'
079030060216     c                   Eval      kpjbu = parmf11
079040051213     c                   Call      'TNTA81R'
079050051213     c                   Parm                    kpjba
079060051213     c                   Parm                    tnta81ds
079070051128
079080051212      * Controllo se ora c'è o non c'è più il luogo
079090051216     c                   Eval      kspecli = v1cksc
079100051216     c                   Eval      kspecod = '001'
079110051128     c     kFnspe        Chain     Fnspe01l
079120051128     c                   If        %Found(Fnspe01l) and speflg = *Blanks
079130051128     c                   Eval      *In10 = *On
079140051212     c                   Else
079150051212     c                   Eval      *In10 = *Off
079160051128     c                   EndIf
079170051128
079180051130     c                   Clear                   v4cl001
079190100727     c                   Eval      h4riga = 21
079200060203     c                   Eval      h4colo = 35
079210051207     c                   Goto      emi04
079220051128
079230051129    1c                   EndIf
079240051128
079250051128     c                   EndSr
079260051122
079270051129      *------------------------------------------------------------------------*
079280051220      * CONTROLLO NOTA 84 - INVIO FATTURA
079290051129      *------------------------------------------------------------------------*
079300051129     c     Sr_Ctrnt84    BegSr
079310051129
079320051130if  1c                   If        v4cnt84 <> *Blanks
079330060203
079340060203      * Se sono in interrogazione non posso interrogare la nota se non c'è
079350060203     c                   If        *In05 and Not *In21
079360060203     c                   Eval      *In28 = *On
079370060203     c                   Eval      h4riga = 21
079380100727     c                   Eval      h4colo = 70
079390060203     c                   Eval      v4cmsg = msg(52)
079400060203     c                   Leavesr
079410060203     c                   EndIf
079420060203
079430060203      * Imposto se interrogazione o gestione
079440060203     c  n05              Eval      wrich = 'M'
079450060203     c   05              Eval      wrich = 'V'
079460060217     c                   Eval      wrag = v3crag
079470051212     c                   Eval      wtnt = '84'
079480051212     c                   ExSr      Sr_f02
079490060215
079500060215     c                   If        ta12err <> *Blanks
079510060215     c                   Eval      *In28 = *On
079520060215     c                   Eval      h4riga = 21
079530100727     c                   Eval      h4colo = 70
079540060215     c                   Eval      v4cmsg = ta12msg
079550060215     c                   Leavesr
079560060215     c                   EndIf
079570051129
079580060215      * Controllo se ora c'è o non c'è più la nota
079590091112     c   70
079600091112     corn06              Eval      kntcapl = 'C'
079610100806     c   06
079620091112     cann70              Eval      kntcapl = 'T'
079630051216     c                   Movel(p)  dutkci        kntcnk1
079640091112     c   70
079650091112     corn06              Move      v1cksc        kntcnk1
079660100806     c   06
079670100806     cann70              Move      v1cnrv        kntcnk1
079680051216     c                   Clear                   kntcnk2
079690051216     c                   Movel     '84'          kntctnt
079700091119     c     kTfntc        Chain(n)  Tfntc01l
079710051129     c                   If        %Found(Tfntc01l)
079720051129     c                   Eval      *In21 = *On
079730051212     c                   Else
079740051212     c                   Eval      *In21 = *Off
079750051129     c                   EndIf
079760051129
079770051130     c                   Clear                   v4cnt84
079780051212     c                   Eval      h4riga = 21
079790100727     c                   Eval      h4colo = 70
079800051212     c                   Goto      emi04
079810051129
079820051129    1c                   EndIf
079830051129
079840051129     c                   EndSr
079850051129
079860051129      *------------------------------------------------------------------------*
079870051130      * CONTROLLO CODICE PAGAMENTO FATTURE
079880051129      *------------------------------------------------------------------------*
079890051129     c     Sr_Ctrcdp     BegSr
079900051129
079910051129      * Ricerca
079920100727     c                   Eval      wpos = %scan('?':v4ccdp)
079930051129     c                   If        wpos > 0
079940100727     c                   Clear                   v4dcdp
079950100727     c                   Clear                   v4ccdp
079960051129     c                   Call      'XCDPAGR'
079970100727     c                   Parm                    v4ccdp
079980100727     c                   Parm                    v4dcdp
079990100728     c                   Eval      h4riga = 13
080000100728     c                   Eval      h4colo = 36
080010051129     c                   Eval      *In90 = *On
080020100727     c                   If        v4ccdp = '999'
080030100727     c                   Clear                   v4ccdp
080040051129     c                   EndIf
080050051129     c                   EndIf
080060051129
080070051129      * Controllo
080080100727     c                   Clear                   v4dcdp
080090051129     c                   Clear                   dsfa
080100051129     c                   Eval      kcod = 'FA'
080110100727     c                   Movel     v4ccdp        w004a
080120051129     c                   Move      '1'           w004a
080130051129     c                   Eval      kkey = w004a
080140051129     c     kTabel        Chain     Tabel00f
080150051129     c                   If        Not %Found(Tabel00f) or
080160051129     c                             tblflg <> *Blanks
080170051129     c                   Eval      *In28 = *On
080180100728     c                   Eval      h4riga = 13
080190100728     c                   Eval      h4colo = 36
080200100728     c                   Eval      v4cmsg = msg(53)
080210051129     c                   Leavesr
080220051129     c                   EndIf
080230051129     c                   Eval      dsfa = tbluni
080240100727     c                   Eval      v4dcdp = §fades
080250051129
080260051129      * Se capoconto cliente non posso utilizzare il codice 3
080270051129     c                   If        v1ckcc = 151 and §fatpg = '3'
080280051129     c                   Eval      *In28 = *On
080290100728     c                   Eval      h4riga = 13
080300100728     c                   Eval      h4colo = 36
080310100728     c                   Eval      v4cmsg = msg(53)
080320051129     c                   Leavesr
080330051129     c                   EndIf
080340080515
080350080515      * se utente di filiale non può inerire un pagamento con codice '2' RID
080360090416      * ora controllo il tipo utilizzo se S è solo SEDE
080370080515     c                   if        not *in09 and
080380100727     c                             v4ccdp <> %subst(indcdp:4:3) and
080390090421     c                             §fauti = 'S'
080400080515     c                   eval      *In28 = *On
080410100728     c                   eval      h4riga = 13
080420100728     c                   eval      h4colo = 36
080430100728     c                   eval      v4cmsg = msg(80)
080440080515     c                   leavesr
080450080515     c                   endif
080460150415      /free
080470160517       //?Se il cliente è sottoconto intestazione fattura
080480160517       //?controllo se OK RIBA
080490160518         IF  %editc(V1Cksc:'X') = V4Cscf and §FAtpg = '1';
080500160518       //?No RIBA (tipo pag. 1) se P.IVA estera
080510160518           IF  (V2ivaeu <> *blanks and V2ivaeu <> '$$' and
080520160518                V2ivaeu <> 'IT' and V2ivaeu <> 'SM') or
080530160518               (V2ivaeu = '$$' and V2Ccdf = *blanks);
080540160517             *in28 = *on;
080550160517             H4riga = 13;
080560160517             H4colo = 36;
080570160517             V4Cmsg = msg(89);
080580160517             leavesr;
080590160517           ENDIF;
080600160517       //?No RIBA (tipo pag. 1) su conto Bancoposta (ABI 07601)
080610160518           IF  V4Cabi = 07601;
080620160517             *in28 = *on;
080630160517             H4riga = 13;
080640160517             H4colo = 36;
080650160517             V4Cmsg = msg(90);
080660160517             leavesr;
080670160518           ENDIF;
080680160517         ENDIF;
080690160517
080700150415       //?Se utente di filiale e tipo pagamento utilizzabile solo da sede
080710150415       //?devo proteggere la Banca ABI/CAB del pagamento fatture
080720150415         *in82 = *off;
080730150415         IF  not *in09 and §FAuti = 'S';
080740150415           *in82 = *on;
080750150415         ENDIF;
080760150415      /end-free
080770051129
080780051129     c                   EndSr
080790051129
080800051129      *------------------------------------------------------------------------*
080810051129      * CONTROLLO BANCA APPOGGIO
080820051129      *------------------------------------------------------------------------*
080830051129     c     Sr_Ctrbna     BegSr
080840070216
080850070216     c                   clear                   wbanca
080860070216     c                   clear                   wagenzia
080870051129
080880051129      * Ricerca
080890100727     c                   Eval      wpos = %scan('?':v4cbna)
080900051129if  1c                   If        wpos > 0
080910100727     c                   Eval      pist = %subst(v4cbna:(wpos+1):(36-wpos))
080920100727     c                   Move      v4cabi        pabi
080930051129     c                   Clear                   pcab
080940051129     c                   Eval      ppro = v2cprv
080950051129     c                   Eval      pflg = 'R'
080960051129     c                   Eval      kpjbu = parmbanca
080970051129     c                   Call      'CNC592R'
080980051129     c                   Parm                    kpjba
080990051129     c                   Eval      parmbanca = kpjbu
081000051129      * Se è stato selezionato un codice
081010051129if  2c                   If        pcab <> *Zeros
081020100727     c                   Movel     pabi          v4cabi
081030100727     c                   Movel     pcab          v4ccab
081040100727     c                   Eval      kabi = v4cabi
081050100727     c                   Eval      kcab = v4ccab
081060100729     c                   exsr      DesBanca
081070100729     c                   eval      v4cbna = wDesBanca
081080051129    2c                   EndIf
081090051129     c                   Eval      *In90 = *On
081100100728     c                   Eval      h4riga = 14
081110100728     c                   Eval      h4colo = 36
081120051129     c                   Leavesr
081130051129    1c                   EndIf
081140051129
081150051129     c                   EndSr
081160051129
081170051129      *------------------------------------------------------------------------*
081180051130      * CONTROLLO ABI/CAB RELATIVO AL PAGAMENTO FATTURE
081190051129      *------------------------------------------------------------------------*
081200051130     c     Sr_Ctrabicabf BegSr
081210051129
081220051129      * Se non è da gestione visite/offerte controllo se abi e cab
081230051129      * obbligatori per pagamento con RB
081240051129     c                   If        Not *In06 and §fatpg = '1' and
081250100727     c                             v4cabi = *Zeros and v4ccab = *Zeros
081260051129     c                   Eval      *In28 = *On
081270100728     c                   Eval      h4riga = 15
081280100728     c                   Eval      h4colo = 36
081290100728     c                   Eval      v4cmsg = msg(54)
081300051129     c                   Leavesr
081310051129     c                   EndIf
081320090331
081330090331      * se pagamento con RB codice '1' non accetto ABI >= 90000
081340090331     c                   If        §fatpg = '1' and
081350100727     c                             v4cabi >= 90000
081360090331     c                   Eval      *In28 = *On
081370100728     c                   Eval      h4riga = 15
081380100728     c                   Eval      h4colo = 36
081390100728     c                   Eval      v4cmsg = msg(56)
081400090331     c                   Leavesr
081410090331     c                   EndIf
081420051130
081430051130      * Controllo
081440100727     c                   Eval      kabi = v4cabi
081450100727     c                   Eval      kcab = v4ccab
081460051130     c                   ExSr      Sr_Ctrabicab
081470051130     c                   If        *In28 = *On
081480100728     c                   Eval      h4riga = 15
081490100728     c                   Eval      h4colo = 36
081500051130     c                   Leavesr
081510051130     c                   EndIf
081520051130      * Creo descrizione della banca di appoggio
081530100727     c                   If        v4cabi <> 99999 or v4ccab <> 99999
081540100727     c                   Movel     wbanca        v4cbna
081550100727     c                   Move      wagenzia      v4cbna
081560051130     c                   EndIf
081570051130
081580051130     c                   EndSr
081590150424
081600150424      /free
081610150424       //--------------------------------------------------------------
081620150424       //?Controllo i pagamenti C/A - Danni - N.A.
081630150424       //--------------------------------------------------------------
081640150424       BEGSR CtrPagamenti;
081650150424
081660150424       //?Pagamenti CONTRASSEGNI
081670150424         clear ds4U;
081680150424         kcod = '4U';
081690150424         kkey = V5Cfpc;
081700150424         chain (codut:kcod:kkey) TABEL00F;
081710150424         IF  %found(TABEL00F);
081720150424           ds4U = TBLuni;
081730150424         ENDIF;
081740151112         IF  V5Cfpc <> '1';
081750150427         IF  not wForzaPag and §4Utip = 'I' and
081760150427            (§4Ucod <> 'B' or V5Ciban = *blanks);
081770150424           *in28 = *on;
081780150424           H5riga = 09;
081790150424           H5colo = 36;
081800150424           V5Cmsg = msg(87);
081810150424           wForzaPag = *on;
081820150424           leavesr;
081830150424         ENDIF;
081840151112         ENDIF;
081850150424
081860150424       //?Pagamenti DANNI
081870150424         clear ds4U;
081880150424         kcod = '4U';
081890150424         kkey = V5tpdn;
081900150424         chain (codut:kcod:kkey) TABEL00F;
081910150424         IF  %found(TABEL00F);
081920150424           ds4U = TBLuni;
081930150424         ENDIF;
081940151112         IF  V5tpdn <> '1';
081950150427         IF  not wForzaPag and §4Utip = 'I' and
081960150427            (§4Ucod <> 'B' or V5ibandn = *blanks);
081970150424           *in28 = *on;
081980150424           H5riga = 12;
081990150424           H5colo = 36;
082000150424           V5Cmsg = msg(87);
082010150424           wForzaPag = *on;
082020150424           leavesr;
082030150424         ENDIF;
082040151112         ENDIF;
082050150424
082060150424       //?Pagamenti NOTE ACCREDITO
082070150424         clear ds4U;
082080150424         kcod = '4U';
082090150424         kkey = V5tpna;
082100150424         chain (codut:kcod:kkey) TABEL00F;
082110150424         IF  %found(TABEL00F);
082120150424           ds4U = TBLuni;
082130150424         ENDIF;
082140151112         IF  V5tpna <> '1';
082150150427         IF  not wForzaPag and §4Utip = 'I' and
082160150427            (§4Ucod <> 'B' or V5ibanna = *blanks);
082170150424           *in28 = *on;
082180150424           H5riga = 15;
082190150427           H5colo = 37;
082200150424           V5Cmsg = msg(87);
082210150424           wForzaPag = *on;
082220150424           leavesr;
082230150424         ENDIF;
082240151112         ENDIF;
082250150424
082260150424       ENDSR;
082270150728
082280150728       //--------------------------------------------------------------
082290150728       //?Controllo i dati per la mail invio progetto di liquidazione.
082300150728       //--------------------------------------------------------------
082310150728       BEGSR CtrMailDanni;
082320150728
082330150728       //?Se esiste la nota 87 OK
082340150728         IF  *in18;
082350150728           leavesr;
082360150728         ENDIF;
082370150728
082380150728       //?Se esiste il luogo 008 e c'è la mail OK
082390150728         IF  *in17;
082400150728           kSPEcli = V1Cksc;
082410150728           kSPEcod = '008';
082420150728           chain (kSPEcli:kSPEcod:kSP2tpe) FNSP201L;
082430150728           IF  %Found(FNSP201L) and SP2flg = *blanks;
082440150728             leavesr;
082450150728           ENDIF;
082460150728         ENDIF;
082470150728
082480150728       //?Se arrivo qua vuol dire che non c'è la nota 87 oppure
082490150728       //?non c'è il luogo 008 o il luogo 008 non ha la mail
082500150728         IF  not wForzaMail;
082510150728           *in28 = *on;
082520150728           H7riga = 13;
082530150728           H7colo = 35;
082540150728           V7Cmsg = msg(88);
082550150728           wForzaMail = *on;
082560150728         ENDIF;
082570150728
082580150728       ENDSR;
082590150424      /end-free
082600051130
082610051130      *------------------------------------------------------------------------*
082620051130      * CONTROLLO CODICE PAGAMENTO CONTRASSEGNI
082630051130      *------------------------------------------------------------------------*
082640051130     c     Sr_Ctrfpc     BegSr
082650051219
082660051219     c                   Eval      *In22 = *Off
082670051219     c                   Eval      *In23 = *Off
082680051130
082690051130      * Ricerca
082700051130     c                   Clear                   v5dfpc
082710051130     c                   If        v5cfpc = '?'
082720051130     c                   Eval      kcod = '4U'
082730051130     c                   call      'TRUL19R'
082740051130     c                   parm                    kcod
082750051130     c                   parm                    ordina
082760051130     c                   parm                    kkey
082770051130     c                   parm                    comando
082780051130     c                   Eval      v5cfpc = kkey
082790100729     c                   Eval      h5riga = 09
082800100802     c                   Eval      h5colo = 36
082810051130     c                   Eval      *In90 = *On
082820051130     c                   EndIf
082830051130
082840051130      * Controllo
082850051130     c                   Clear                   v5dfpc
082860051130     c                   Clear                   ds4u
082870051130     c                   Eval      kcod = '4U'
082880051130     c                   Eval      kkey = v5cfpc
082890051130     c     kTabel        Chain     Tabel00f
082900051130     c                   If        Not %Found(Tabel00f) or
082910051130     c                             tblflg <> *Blanks
082920051130     c                   Eval      *In28 = *On
082930100729     c                   Eval      h5riga = 09
082940100802     c                   Eval      h5colo = 36
082950051130     c                   Eval      v5cmsg = msg(57)
082960051130     c                   Leavesr
082970051130     c                   EndIf
082980051130     c                   Eval      ds4u = tbluni
082990051130     c                   Eval      v5dfpc = §4udes
083000051130
083010051130      * Controllo se utilizzabile per i clienti vari
083020051130     c                   If        (wksc04 = 8888 or wksc04 = 9999) and
083030051130     c                              §4uvar = 'N'
083040051130     c                   Eval      *In28 = *On
083050100729     c                   Eval      h5riga = 09
083060100802     c                   Eval      h5colo = 36
083070051130     c                   Eval      v5cmsg = msg(58)
083080051130     c                   Leavesr
083090051130     c                   EndIf
083100151030
083110151030      /free
083120151030       //?Errore se utilizzabile solo da sede e NON sono sede
083130151112         IF  §4Usede <> *blanks and not *in09 and v5cfpc <> CLPfpc;
083140151030           *in28 = *on;
083150151030           H5riga = 09;
083160151030           H5colo = 36;
083170151030           V5Cmsg = msg(57);
083180151030           V5Cmsg = %trim(V5Cmsg) + '. Utilizzabili SOLO da SEDE';
083190151030           leavesr;
083200151030         ENDIF;
083210151030      /end-free
083220051219
083230051219      * Le coordinate bancare le visualizzo o le proteggo in base al
083240051219      * tipo pagamento
083250051219     c                   Select
083260051219      * Coordinate italiane, visualizzo
083270051219      * Immissione coordinate non richiesta, non visualizzo
083280051219     c                   When      §4uabi = 'N'
083290051219     c                   Eval      *In22 = *On
083300080116      * pulisco le coordinate bancarie
083310080116     c                   clear                   v5ciban
083320051219      * Se pagamento estero proteggo le coordinate bancarie perchè
083330051219      * gestite dalla sede.
083340080115     c                   when      §4utip = 'E'
083350051219     c                   Eval      *In23 = *On
083360051219     c                   EndSl
083370100729      * se il tipo pagamento del codice impostato a video è diverso da quello
083380100729      * memorizzato sul file ed è estero devo pulire il campo relativo all'IBAN
083390100729      * e avvisare con window per inviare mail alla sede
083400100729     c                   if        §4utip <> sav4utip and §4utip = 'E'
083410100729     c                   if        v5ciban <> *blanks
083420100729     c                   clear                   v5ciban
083430100729     c                   endif
083440100729      * window per inviare mail alla sede
083450100729      * solo se tipo pagamento prevede le coordinate bancarie
083460100729     c                   if        not $win and §4uabi = 'S'
083470101014     c                   eval      wuffsede = '"CONTRASSEGNI" di sede'
083480100729     c                   exsr      sr_winmail
083490100729     c                   eval      $win = *on
083500100729     c                   eval      *in90 = *on
083510100729     c                   leavesr
083520100729     c                   endif
083530100729     c                   endif
083540051130
083550051130     c                   EndSr
083560100729
083570100729      /free
083580100729       //--------------------------------------------------------------
083590100729       //?Controllo pagamento Danni.
083600100729       //--------------------------------------------------------------
083610100729       BEGSR CtrPagDN;
083620100729
083630100729         *in71 = *off;
083640100729         *in72 = *off;
083650100729
083660100729       //?Ricerco il codice pagamento
083670100729         clear v5dpdn;
083680100729         IF  %scan('?':v5tpdn) > 0;
083690100729           kcod = '4U';
083700100729           Trul19R (kcod:ordina:kkey:comando);
083710100729           v5tpdn = kkey;
083720100730           h5riga = 12;
083730100802           h5colo = 36;
083740100729           *in90 = *on;
083750100729         ENDIF;
083760100729
083770100729       //?Controllo
083780100729         clear v5dpdn;
083790100729         clear ds4u;
083800100729         kcod = '4U';
083810100729         kkey = v5tpdn;
083820100729         chain (codut:kcod:kkey) TABEL00F;
083830100729         IF  not %Found(TABEL00F) or TBLflg <> *blanks;
083840100729           *in28 = *on;
083850100730           h5riga = 12;
083860100802           h5colo = 36;
083870100729           v5cmsg = msg(57);
083880100729           leavesr;
083890100729         ENDIF;
083900100729         ds4u = TBLuni;
083910100730         v5dpdn = §4Udes;
083920100729
083930100729       //?Controllo se posso utilizzare per clienti vari
083940110131         IF  (wksc04 = 8888 or wksc04 = 9999) and §4Uvar = 'N';
083950100729           *in28 = *on;
083960100730           h5riga = 12;
083970100802           h5colo = 36;
083980100729           v5cmsg = msg(58);
083990100729           leavesr;
084000100729         ENDIF;
084010151030
084020151030       //?Errore se utilizzabile solo da sede e NON sono sede
084030151112         IF  §4Usede <> *blanks and not *in09 and
084040151112             v5tpdn <> %subst(CLStic:1:1);
084050151030           *in28 = *on;
084060151030           H5riga = 12;
084070151030           H5colo = 36;
084080151030           V5Cmsg = msg(57);
084090151030           V5Cmsg = %trim(V5Cmsg) + '. Utilizzabili SOLO da SEDE';
084100151030           leavesr;
084110151030         ENDIF;
084120151030
084130100729       //?Visualizzo il campo iban se coordinate bancarie obbligatorie
084140100730         *in71 = (§4Uabi = 'N');
084150100729
084160100729       //?Controllo se posso utilizzare in base alla tabella PAG
084170100729         clear TIBS02DS;
084180100729         clear dPAG;
084190100729         T02mod = 'C';
084200100729         T02cod = 'PAG';
084210100729         T02ke1 = 'DN';
084220100729         T02ke2 = v5tpdn;
084230100729         T02sif = KNSIF;
084240100729         TNTBE_RicercaControllo (kpjba : tibs02ds);
084250100729         IF  T02err <> *blanks;
084260100729           *in28 = *on;
084270100730           h5riga = 12;
084280100802           h5colo = 36;
084290100729           v5cmsg = msg(85);
084300100729           leavesr;
084310100729         ENDIF;
084320100729         IF  T02err = *blanks;
084330100729           dPAG = T02uni;
084340100729         ENDIF;
084350100729       //?controllo se la filiale può utilizzare il pagamento
084360151112         IF  d§PAGuti = 'NO' and not *in09 and
084370151112             v5tpdn <> %subst(CLStic:1:1);
084380100729           *in28 = *on;
084390100730           h5riga = 12;
084400100802           h5colo = 36;
084410100729           v5cmsg = msg(80);
084420100729           leavesr;
084430100729         ENDIF;
084440100730       //?proteggo il campo IBAN se pagamento estero
084450100730         *in72 = (§4Utip = 'E');
084460100909
084470100909       //?se pagamento con bonifico italia posso utilizzarlo solo
084480100909       //?per clienti 'T - D - A'
084490150506         //IF  not *in72 and d§PAGctr = 'SI' and
084500150506         //    v3cclv <> 'T' and  v3cclv <> 'D' and
084510150506         //    v3cclv <> 'A';
084520150506         //  *in28 = *on;
084530150506         //  h5riga = 12;
084540150506         //  h5colo = 36;
084550150506         //  v5cmsg = msg(85);
084560150506         //  v5cmsg = %trim(v5cmsg) + ' per clienti NON di tipo T-D-A';
084570150506         //  leavesr;
084580150506         //  ENDIF;
084590100730
084600100802       //?se è stato modificato il tipo pagamento
084610100909       //?e quello inserito non richiede le coordinate bancarie pulisco
084620100802       //?il campo dell'IBAN
084630100802         IF  v5tpdn <> savtipdn and d§PAGobb <> 'SI';
084640100802           clear v5ibandn;
084650100805           clear v5bicdn;
084660100802       //?se il tipo pagamento inserito è un tipo pagamento che lo richiede
084670100802       //?visualizzo window per invio mail
084680100802           IF  d§PAGobb = 'MA' and not $winDN;
084690101014             wuffsede = '"GESTIONE PAGAMENTI" di sede';
084700100802             exsr sr_winmail;
084710100802             $winDN = *on;
084720100802             *in90 = *on;
084730100802             leavesr;
084740100802           ENDIF;
084750100730         ENDIF;
084760100729
084770100729       ENDSR;
084780100729
084790100729       //--------------------------------------------------------------
084800100729       //?Controllo pagamento Note Accredito.
084810100729       //--------------------------------------------------------------
084820100729       BEGSR CtrPagNA;
084830100730
084840100730         *in73 = *off;
084850100730         *in74 = *off;
084860100730
084870100730       //?Ricerco il codice pagamento
084880100730         clear v5dpna;
084890100730         IF  %scan('?':v5tpna) > 0;
084900100730           kcod = '4U';
084910100730           Trul19R (kcod:ordina:kkey:comando);
084920100916           v5tpna = kkey;
084930100730           h5riga = 15;
084940150427           h5colo = 37;
084950100730           *in90 = *on;
084960100730         ENDIF;
084970100730
084980100730       //?Controllo
084990100730         clear v5dpna;
085000100730         clear ds4u;
085010100730         kcod = '4U';
085020100730         kkey = v5tpna;
085030100730         chain (codut:kcod:kkey) TABEL00F;
085040100730         IF  not %Found(TABEL00F) or TBLflg <> *blanks;
085050100730           *in28 = *on;
085060100730           h5riga = 15;
085070150427           h5colo = 37;
085080100730           v5cmsg = msg(57);
085090100730           leavesr;
085100100730         ENDIF;
085110100730         ds4u = TBLuni;
085120100730         v5dpna = §4Udes;
085130100730
085140100730       //?Controllo se posso utilizzare per clienti vari
085150110131         IF  (wksc04 = 8888 or wksc04 = 9999) and §4Uvar = 'N';
085160100730           *in28 = *on;
085170100730           h5riga = 15;
085180150427           h5colo = 37;
085190100730           v5cmsg = msg(58);
085200100730           leavesr;
085210100730         ENDIF;
085220151030
085230151030       //?Errore se utilizzabile solo da sede e NON sono sede
085240151112         IF  §4Usede <> *blanks and not *in09 and
085250151112             v5tpna <> %subst(CLStic:2:1);
085260151030           *in28 = *on;
085270151030           H5riga = 15;
085280151030           H5colo = 37;
085290151030           V5Cmsg = msg(57);
085300151030           V5Cmsg = %trim(V5Cmsg) + '. Utilizzabili SOLO da SEDE';
085310151030           leavesr;
085320151030         ENDIF;
085330151030
085340100730       //?Visualizzo il campo iban se coordinate bancarie obbligatorie
085350100730         *in73 = (§4Uabi = 'N');
085360100730
085370100730       //?Controllo se posso utilizzare in base alla tabella PAG
085380100730         clear TIBS02DS;
085390100730         clear dPAG;
085400100730         T02mod = 'C';
085410100730         T02cod = 'PAG';
085420100730         T02ke1 = 'NA';
085430100730         T02ke2 = v5tpna;
085440100730         T02sif = KNSIF;
085450100730         TNTBE_RicercaControllo (kpjba : tibs02ds);
085460100730         IF  T02err <> *blanks;
085470100730           *in28 = *on;
085480100730           h5riga = 15;
085490150427           h5colo = 37;
085500100730           v5cmsg = msg(85);
085510100730           leavesr;
085520100730         ENDIF;
085530100730         IF  T02err = *blanks;
085540100730           dPAG = T02uni;
085550100730         ENDIF;
085560100730       //?controllo se la filiale può utilizzare il pagamento
085570151112         IF  d§PAGuti = 'NO' and not *in09 and
085580151112             v5tpna <> %subst(CLStic:2:1);
085590100730           *in28 = *on;
085600100730           h5riga = 15;
085610150427           h5colo = 37;
085620100730           v5cmsg = msg(80);
085630100730           leavesr;
085640100730         ENDIF;
085650100730       //?proteggo il campo IBAN se pagamento estero
085660100730         *in74 = (§4Utip = 'E');
085670100730
085680100802       //?se è stato modificato il tipo pagamento
085690100909       //?e quello inserito non richiede le coordinate bancarie pulisco
085700100802       //?il campo dell'IBAN
085710100802         IF  v5tpna <> savtipna and d§PAGobb <> 'SI';
085720100802           clear v5ibanna;
085730100805           clear v5bicna;
085740100802       //?se il tipo pagamento inserito è un tipo pagamento che lo richiede
085750100802       //?visualizzo window per invio mail
085760100802           IF  d§PAGobb = 'MA' and not $winNA;
085770101220             wuffsede = '"GESTIONE PAGAMENTI" di sede';
085780100730             exsr sr_winmail;
085790100730             $winNA = *on;
085800100730             *in90 = *on;
085810100730             leavesr;
085820100730           ENDIF;
085830100730         ENDIF;
085840100729
085850100729       ENDSR;
085860100729      /end-free
085870080115
085880080115      *------------------------------------------------------------------------*
085890080115      * CONTROLLO CODICE IBAN
085900080115      *------------------------------------------------------------------------*
085910080115     c     sr_ctriban    begsr
085920080115
085930080115     c                   clear                   wbanca
085940080115     c                   clear                   wagenzia
085950080422
085960080422      * se tipo pagamento contrassegno italia posso accettare solo
085970090612      * un IBAN italia (IT)
085980080422     c                   if        §4utip = 'I' and
085990100729     c                             %subst(wiban:1:2) <> 'IT'
086000150609     c                             and %subst(wiban:1:2) <> 'SM'
086010150424     c                             and %subst(wiban:1:2) <> *blanks
086020080422     c                   eval      *in28 = *on
086030080422     c                   eval      v5cmsg = msg(79)
086040080422     c                   leavesr
086050080422     c                   endif
086060080116
086070080116      * richiamo programma di controllo codice IBAN (Proj)
086080080116     c                   clear                   parmiban
086090100730     c                   eval      parmstato = %subst(wiban:1:2)
086100100730     c                   eval      parmcheck = %subst(wiban:3:2)
086110100730     c                   eval      parmcoori = %subst(wiban:5:28)
086120080116     c                   call      'XCHKIBAN'
086130080116     c                   parm                    parmstato
086140080116     c                   parm                    parmcheck
086150080116     c                   parm                    parmcoori
086160080116     c                   parm                    erriban
086170080116      * se ritorna errore messaggio
086180080116     c                   if        erriban = *on
086190080116     c                   eval      *in28 = *on
086200080116     c                   eval      v5cmsg = msg(59)
086210080116     c                   leavesr
086220080116     c                   endif
086230080116      * decodifico la banca
086240100729     c                   clear                   wdesbanca
086250100729     c                   eval      kabi = %dec(%subst(wiban:6:5):5:0)
086260100729     c                   eval      kcab = %dec(%subst(wiban:11:5):5:0)
086270080116     c     kcnabi        chain     cnabi00f
086280080116     c                   if        %found(cnabi00f) and abiann <> '*'
086290080116      * compongo la descrizione della banca d'appoggio
086300080116     c                   exsr      sr_crebna
086310100730     c                   eval      wdesbanca = wbanca + wagenzia
086320080116     c                   endif
086330080115
086340080115     c                   endsr
086350051130
086360051130      *-------------------------------------------------------------------------
086370051130      *  Creo la descrizione della banca di appoggio in base ai codici ABI/CAB
086380051130      *-------------------------------------------------------------------------
086390051130     c     Sr_Crebna     BegSr
086400051130
086410051130if  1c                   If        abiabi <> 99999 or abicab <> 99999
086420051130     c                   Clear                   wagenzia
086430060221     c                   Clear                   wbanca
086440051130     c                   Eval      wbanca = abiist
086450051130      * Compongo il nome della banca con località o comune
086460051130if  2c                   If        abiage = *Blanks
086470051130     c                   Select
086480051130     c                   When      abiloc <> *Blanks
086490051130     c                   Eval      wagenzia = abiloc
086500051130     c                   When      abicom <> *Blanks
086510051130     c                   Eval      wagenzia = abicom
086520051130     c                   EndSl
086530051130      * Compongo il nome della banca con agenzia + località o comune
086540051130   x2c                   Else
086550051130      * Cerco il primo byte vuoto dell'agenzia
086560051130     c                   Eval      wpos = %checkr(' ':abiage)
086570051130
086580051130     c                   Eval      wagenzia = %subst(abiage:1:wpos)
086590051130     c                   Eval      wpos1 = wpos + 2
086600051130     c                   Eval      wpos2 = 16 - wpos1
086610060215      * Se wpos2 inferiore a zero lo imposto a 0
086620060215     c                   If        wpos2 < *Zeros
086630060215     c                   Clear                   wpos2
086640060215     c                   EndIf
086650051130      * Se posso aggiungere anche solo 1 carattere procedo
086660051130     c                   If        wpos > 1 and wpos < 16
086670051130      * Cerco di riempire quello che resta con la località o il comune
086680051130     c                   Select
086690051130     c                   When      abiloc <> *Blanks
086700060221     c                   Eval      wagenzia = %trim(wagenzia) + ' ' +
086710060221     c                             %subst(abiloc:1:wpos2)
086720051130     c                   When      abicom <> *Blanks
086730060221     c                   Eval      wagenzia = %trim(wagenzia) + ' ' +
086740060221     c                             %subst(abicom:1:wpos2)
086750051130     c                   EndSl
086760051130     c                   EndIf
086770051130
086780051130    2c                   EndIf
086790051130    1c                   EndIf
086800051130
086810051130     c                   EndSr
086820051207
086830051130      *------------------------------------------------------------------------*
086840051130      * CONTROLLO ABI/CAB GENERICO
086850051130      *------------------------------------------------------------------------*
086860051130     c     Sr_Ctrabicab  BegSr
086870051130
086880051130      * Controllo ABI/CAB inseriti
086890051130     c                   If        kabi = *Zeros and kcab = *Zeros
086900051130     c                   Leavesr
086910051130     c                   EndIf
086920051130
086930051130      * Se immesso il CAB immettere anche ABI
086940051130     c                   If        (kabi = *Zeros and kcab <> *Zeros) or
086950051130      * Se immesso il ABI immettere anche CAB
086960051130     c                             (kabi <> *Zeros and kcab = *Zeros)
086970051130     c                   Eval      *In28 = *On
086980101111     c                   Eval      v4cmsg = msg(56)
086990051130     c                   Leavesr
087000051130     c                   EndIf
087010051130      * Controllo se validi
087020051130     c     kCnabi        Chain     Cnabi00f
087030051130     c                   If        Not %Found(Cnabi00f) or abiann = '*'
087040051130     c                   Eval      *In28 = *On
087050101111     c                   Eval      v4cmsg = msg(56)
087060051130     c                   Leavesr
087070051130     c                   EndIf
087080051130
087090051130      * Compongo la descrizione della banca d'appoggio
087100051130     c                   ExSr      Sr_Crebna
087110051130
087120051130     c                   EndSr
087130051129
087140051206
087150051206      *------------------------------------------------------------------------*
087160051206      * CONTROLLO LUOGO 555 - INTESTAZIONE PAGAMENTO C/ASSEGNI
087170051206      *------------------------------------------------------------------------*
087180051206     c     Sr_Ctrl555    BegSr
087190051206
087200051206if  1c                   If        v5cl555 <> *Blanks
087210060203
087220060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
087230060203     c                   If        *In05 and Not *In11
087240051206     c                   Eval      *In28 = *On
087250100730     c                   Eval      h5riga = 19
087260060203     c                   Eval      h5colo = 35
087270051207     c                   Eval      v5cmsg = msg(50)
087280051206     c                   Leavesr
087290051206     c                   EndIf
087300071128
087310071128      * se non è interrogazione controllo se luogo utilizzabile
087320071128     c                   if        not *in09 and not *in05 and $ufi555 <> 'S'
087330071128     c                   eval      *In28 = *on
087340100730     c                   eval      h5riga = 19
087350071128     c                   eval      h5colo = 35
087360071128     c                   eval      v5cmsg = msg(76)
087370071128     c                   leavesr
087380071128     c                   endif
087390051206
087400060203      * Imposto se interrogazione o gestione
087410051213     c                   Clear                   tnta81ds
087420060203     c   05              Eval      i81opz = '5'
087430060203     c  n05
087440060214     can 11              Eval      i81opz = '2'
087450051213     c                   Eval      i81cod = '555'
087460051213     c                   Eval      i81cli = v1cksc
087470060216     c                   Eval      i81gcli = '1'
087480060216     c   01              Eval      i81icli = '1'
087490051213     c                   Call      'TNTA81R'
087500051213     c                   Parm                    kpjba
087510051213     c                   Parm                    tnta81ds
087520051206
087530051212      * Controllo se ora c'è o non c'è più il luogo
087540051216     c                   Eval      kspecli = v1cksc
087550051216     c                   Eval      kspecod = '555'
087560051206     c     kFnspe        Chain     Fnspe01l
087570051206     c                   If        %Found(Fnspe01l) and speflg = *Blanks
087580051212     c                   Eval      *In11 = *On
087590051212     c                   Else
087600051212     c                   Eval      *In11 = *Off
087610051206     c                   EndIf
087620051206
087630051206     c                   Clear                   v5cl555
087640100730     c                   Eval      h5riga = 19
087650060203     c                   Eval      h5colo = 35
087660051207     c                   Goto      emi05
087670051206
087680051206    1c                   EndIf
087690051206
087700051206     c                   EndSr
087710051207
087720051207      *------------------------------------------------------------------------*
087730051212      * CONTROLLO LUOGO 666 - LUOGO INVIO C/ASSEGNO MITTENTE
087740051207      *------------------------------------------------------------------------*
087750051207     c     Sr_Ctrl666    BegSr
087760051212
087770051212if  1c                   If        v5cl666 <> *Blanks
087780060203
087790060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
087800060203     c                   If        *In05 and Not *In12
087810051212     c                   Eval      *In28 = *On
087820100730     c                   Eval      h5riga = 20
087830060203     c                   Eval      h5colo = 35
087840051212     c                   Eval      v5cmsg = msg(50)
087850051212     c                   Leavesr
087860051212     c                   EndIf
087870071128
087880071128      * se non è interrogazione controllo se luogo utilizzabile
087890071128     c                   if        not *in09 and not *in05 and $ufi666 <> 'S'
087900071128     c                   eval      *In28 = *on
087910100730     c                   eval      h5riga = 20
087920071128     c                   eval      h5colo = 35
087930071128     c                   eval      v5cmsg = msg(76)
087940071128     c                   leavesr
087950071128     c                   endif
087960051212
087970060203      * Imposto se interrogazione o gestione
087980051213     c                   Clear                   tnta81ds
087990060203     c   05              Eval      i81opz = '5'
088000060203     c  n05
088010060214     can 12              Eval      i81opz = '2'
088020051213     c                   Eval      i81cod = '666'
088030051213     c                   Eval      i81cli = v1cksc
088040060216     c                   Eval      i81gcli = '1'
088050060216     c   01              Eval      i81icli = '1'
088060051213     c                   Call      'TNTA81R'
088070051213     c                   Parm                    kpjba
088080051213     c                   Parm                    tnta81ds
088090051212
088100051212      * Controllo se ora c'è o non c'è più il luogo
088110051216     c                   Eval      kspecli = v1cksc
088120051216     c                   Eval      kspecod = '666'
088130051212     c     kFnspe        Chain     Fnspe01l
088140051212     c                   If        %Found(Fnspe01l) and speflg = *Blanks
088150051212     c                   Eval      *In12 = *On
088160051212     c                   Else
088170051212     c                   Eval      *In12 = *Off
088180051212     c                   EndIf
088190051212
088200051212     c                   Clear                   v5cl666
088210100730     c                   Eval      h5riga = 20
088220060203     c                   Eval      h5colo = 35
088230051212     c                   Goto      emi05
088240051212
088250051212    1c                   EndIf
088260051207
088270051207     c                   EndSr
088280051206
088290051206      *------------------------------------------------------------------------*
088300051220      * CONTROLLO NOTA 85 - E-MAIL BONIFICI C/ASSEGNI
088310051206      *------------------------------------------------------------------------*
088320051212     c     Sr_Ctrnt85    BegSr
088330051206
088340051212if  1c                   If        v5cnt85 <> *Blanks
088350060203
088360060203      * Se sono in interrogazione non posso interrogare la nota se non c'è
088370060203     c                   If        *In05 and Not *In13
088380060203     c                   Eval      *In28 = *On
088390100730     c                   Eval      h5riga = 19
088400100730     c                   Eval      h5colo = 71
088410060203     c                   Eval      v5cmsg = msg(52)
088420060203     c                   Leavesr
088430060203     c                   EndIf
088440060203
088450060203      * Imposto se interrogazione o gestione
088460060203     c  n05              Eval      wrich = 'M'
088470060203     c   05              Eval      wrich = 'V'
088480060217     c                   Eval      wrag = v3crag
088490051212     c                   Eval      wtnt = '85'
088500051212     c                   ExSr      Sr_f02
088510060215
088520060215     c                   If        ta12err <> *Blanks
088530060215     c                   Eval      *In28 = *On
088540100730     c                   Eval      h5riga = 19
088550100730     c                   Eval      h5colo = 71
088560060215     c                   Eval      v5cmsg = ta12msg
088570060215     c                   Leavesr
088580060215     c                   EndIf
088590051206
088600051212      * Controllo se ora c'è o non c'è più la nota
088610091112     c   70
088620091112     corn06              Eval      kntcapl = 'C'
088630100806     c   06
088640091112     cann70              Eval      kntcapl = 'T'
088650051216     c                   Movel(p)  dutkci        kntcnk1
088660091112     c   70
088670091112     corn06              Move      v1cksc        kntcnk1
088680100806     c   06
088690100806     cann70              Move      v1cnrv        kntcnk1
088700051216     c                   Clear                   kntcnk2
088710051216     c                   Movel     '85'          kntctnt
088720091119     c     kTfntc        Chain(n)  Tfntc01l
088730051206     c                   If        %Found(Tfntc01l)
088740051212     c                   Eval      *In13 = *On
088750051212     c                   Else
088760051212     c                   Eval      *In13 = *Off
088770051206     c                   EndIf
088780051206
088790051212     c                   Clear                   v5cnt85
088800100730     c                   Eval      h5riga = 19
088810100730     c                   Eval      h5colo = 71
088820051212     c                   Goto      emi05
088830051206
088840051206    1c                   EndIf
088850051206
088860051206     c                   EndSr
088870090616
088880090616      *------------------------------------------------------------------------*
088890090616      * CONTROLLO NOTA 89 - E-MAIL RESPONSABILE AMMINISTRATIVO
088900090616      *------------------------------------------------------------------------*
088910090616     c     Sr_Ctrnt89    BegSr
088920090616
088930090616if  1c                   If        v5cnt89 <> *Blanks
088940090616
088950090616      * Se sono in interrogazione non posso interrogare la nota se non c'è
088960090616     c                   If        *In05 and Not *In40
088970090616     c                   Eval      *In28 = *On
088980100730     c                   Eval      h5riga = 20
088990100730     c                   Eval      h5colo = 71
089000090616     c                   Eval      v5cmsg = msg(52)
089010090616     c                   Leavesr
089020090616     c                   EndIf
089030090616
089040090616      * Imposto se interrogazione o gestione
089050090616     c  n05              Eval      wrich = 'M'
089060090616     c   05              Eval      wrich = 'V'
089070090616     c                   Eval      wrag = v3crag
089080090616     c                   Eval      wtnt = '89'
089090090616     c                   ExSr      Sr_f02
089100090616
089110090616     c                   If        ta12err <> *Blanks
089120090616     c                   Eval      *In28 = *On
089130100730     c                   Eval      h5riga = 20
089140100730     c                   Eval      h5colo = 71
089150090616     c                   Eval      v5cmsg = ta12msg
089160090616     c                   Leavesr
089170090616     c                   EndIf
089180090616
089190090616      * Controllo se ora c'è o non c'è più la nota
089200091112     c   70
089210091112     corn06              Eval      kntcapl = 'C'
089220100806     c   06
089230091112     cann70              Eval      kntcapl = 'T'
089240090616     c                   Movel(p)  dutkci        kntcnk1
089250091112     c   70
089260091112     corn06              Move      v1cksc        kntcnk1
089270100806     c   06
089280100806     cann70              Move      v1cnrv        kntcnk1
089290090616     c                   Clear                   kntcnk2
089300090616     c                   Movel     '89'          kntctnt
089310091119     c     kTfntc        Chain(n)  Tfntc01l
089320090616     c                   If        %Found(Tfntc01l)
089330090616     c                   Eval      *In40 = *On
089340090616     c                   Else
089350090616     c                   Eval      *In40 = *Off
089360090616     c                   EndIf
089370090616
089380090616     c                   Clear                   v5cnt89
089390100730     c                   Eval      h5riga = 20
089400100730     c                   Eval      h5colo = 71
089410090616     c                   Goto      emi05
089420090616
089430090616    1c                   EndIf
089440090616
089450090616     c                   EndSr
089460100729      /free
089470100729       //--------------------------------------------------------------
089480100729       //?Controllo nota 82 - E-mail Bonifico Danni
089490100729       //--------------------------------------------------------------
089500100729       BEGSR sr_CtrNt82;
089510100730
089520100730         IF  v5cnt82 <> *blanks;
089530100730           *in90 = *on;
089540100730       //?Se sono in interrogazione non posso interrogare la note
089550100730       //?se non è presente
089560100730           IF  *in05 and not *in68;
089570100730             *in28 = *on;
089580100730             h5riga = 21;
089590100730             h5colo = 71;
089600100730             v5cmsg = msg(52);
089610100730             leavesr;
089620100730           ENDIF;
089630100730       //?Imposto se interrogazione o gestione della nota
089640100730           IF  not *in05;
089650100730             wrich = 'M';
089660100730           ENDIF;
089670100730           IF  *in05;
089680100730             wrich = 'V';
089690100730           ENDIF;
089700100730           wrag = v3crag;
089710100730           wtnt = '82';
089720100730           exsr sr_f02;
089730100730           IF  ta12err <> *blanks;
089740100730             *in28 = *on;
089750100730             h5riga = 21;
089760100730             h5colo = 71;
089770100730             v5cmsg = ta12msg;
089780100730             leavesr;
089790100730           ENDIF;
089800100730       //?Controllo se ora c'è la nota oppure se è stata cancellata
089810100730           SELECT;
089820100730             WHEN  *in70 or not *in06;
089830100730               kntcapl = 'C';
089840100730               %subst(kntcnk1:5:7) = %editc(v1cksc:'X');
089850100806             WHEN  *in06 and not *in70;
089860100730               kntcapl = 'T';
089870100806               %subst(kntcnk1:5:7) = %editc(v1cnrv:'X');
089880100730           ENDSL;
089890100730           %subst(kntcnk1:1:4) = %editc(DUTkci:'X');
089900100730           clear kntcnk2;
089910100730           kntctnt = '82';
089920150515           chain(n) (kntcapl:kntcnk1:kntcnk2:kntctnt) TFNTC01L;
089930100730           IF  %found(TFNTC01L);
089940100730             *in68 = *on;
089950100730           ELSE;
089960100730             *in68 = *off;
089970100730           ENDIF;
089980100730           clear v5cnt82;
089990100730           h5riga = 21;
090000100730           h5colo = 71;
090010100730         ENDIF;
090020100729
090030100729       ENDSR;
090040100729      /end-free
090050051207
090060051207      *------------------------------------------------------------------------*
090070051212      * CONTROLLO TIPO COMUNICAZIONE MITTENTE
090080051207      *------------------------------------------------------------------------*
090090051207     c     Sr_Ctrtcm     BegSr
090100051212
090110051212      * Ricerca
090120051212     c                   Clear                   v6dtcm
090130051212     c                   If        v6ctcm = '?'
090140051212     c                   Eval      kcod = '2F'
090150051212     c                   call      'TRUL19R'
090160051212     c                   parm                    kcod
090170051212     c                   parm                    ordina
090180051212     c                   parm                    kkey
090190051212     c                   parm                    comando
090200051212     c                   Eval      v6ctcm = kkey
090210051212     c                   Eval      h6riga = 07
090220051212     c                   Eval      h6colo = 38
090230051212     c                   Eval      *In90 = *On
090240051212     c                   EndIf
090250120118
090260120118      /free
090270120118       //?Se anagrafica provvisoria accetto il campo vuoto
090280120118         IF  *in06 and V6Ctcm = *blanks;
090290120118           leavesr;
090300120118         ENDIF;
090310120118      /end-free
090320051212
090330051212      * Controllo
090340051212     c                   Clear                   v6dtcm
090350051212     c                   Clear                   ds2f
090360051212     c                   Eval      kcod = '2F'
090370051212     c                   Eval      kkey = v6ctcm
090380051212     c     kTabel        Chain     Tabel00f
090390051212     c                   If        Not %Found(Tabel00f) or
090400051212     c                             tblflg <> *Blanks
090410051212     c                   Eval      *In28 = *On
090420051212     c                   Eval      h6riga = 07
090430051212     c                   Eval      h6colo = 38
090440051212     c                   Eval      v6cmsg = msg(62)
090450051212     c                   Leavesr
090460051212     c                   EndIf
090470051212     c                   Eval      ds2f = tbluni
090480051212     c                   Eval      v6dtcm = §2fdes
090490051212
090500051212      * Se tipo comunicazione con invio tramite fax
090510051212if  1c                   If        §2fftp = 'F'
090520051212      * Non ammesso per i clienti vari
090530051212     c                   If        wksc04 = 8888 or wksc04 = 9999
090540051212     c                   Eval      *In28 = *On
090550051212     c                   Eval      h6riga = 07
090560051212     c                   Eval      h6colo = 38
090570051212     c                   Eval      v6cmsg = msg(63)
090580051212     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
090590051212     c                             'per clienti vari'
090600051212     c                   Leavesr
090610051212     c                   EndIf
090620051216      * Controllo se fax sul luogo 005 è esatto
090630051216if  2c                   If        *In14 and wfax005 <> *Blanks
090640051216     c                   Clear                   trul42ds
090650051216     c                   Eval      d42fax = wfax005
090660051216     c                   Call      'TRUL42R'
090670051216     c                   Parm                    trul42ds
090680051216if  3c                   If        d42err = '1'
090690051216     c                   Eval      *In28 = *On
090700051216     c                   Eval      h6riga = 07
090710051216     c                   Eval      h6colo = 38
090720051216     c                   Eval      v6cmsg = d42msg
090730051216     c                   Leavesr
090740051216    3c                   EndIf
090750051216    2c                   EndIf
090760051216      * Se non c'è il luogo o se il n. fax non è presente sul luogo
090770051216      * deve essere impostato sull'anagrafica
090780051216     c                   If        v2ctlf = *Blanks and wfax005 = *Blanks
090790051212     c                   Eval      *In28 = *On
090800051212     c                   Eval      h6riga = 07
090810051212     c                   Eval      h6colo = 38
090820051212     c                   Eval      v6cmsg = msg(63)
090830051212     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
090840051212     c                             'nr. fax obbligatorio'
090850051212     c                   Leavesr
090860051212     c                   EndIf
090870051212    1c                   EndIf
090880051219
090890051219      * Se tipo comunicazione con invio tramite mail
090900051219if  1c                   If        §2fftp = 'M'
090910051219      * Non ammesso per i clienti vari
090920051219     c                   If        wksc04 = 8888 or wksc04 = 9999
090930051219     c                   Eval      *In28 = *On
090940051219     c                   Eval      h6riga = 07
090950051219     c                   Eval      h6colo = 38
090960051219     c                   Eval      v6cmsg = msg(63)
090970051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
090980051219     c                             'per clienti vari'
090990051219     c                   Leavesr
091000051219     c                   EndIf
091010051219      * Controllo se c'è la mail nel luogo
091020051219     c                   If        *In14 and wmail005 = *Blanks
091030051219     c                   Eval      *In28 = *On
091040051219     c                   Eval      h6riga = 07
091050051219     c                   Eval      h6colo = 38
091060051219     c                   Eval      v6cmsg = msg(63)
091070051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
091080051219     c                             'mail non immessa nel luogo 005'
091090051219     c                   Leavesr
091100051219     c                   EndIf
091110051219      * Se non c'è il luogo deve esserci la nota 88
091120051219     c                   If        Not *In14 and Not *In15
091130051219     c                   Eval      *In28 = *On
091140051219     c                   Eval      h6riga = 07
091150051219     c                   Eval      h6colo = 38
091160051219     c                   Eval      v6cmsg = msg(63)
091170051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
091180051219     c                             'nota 88 non immessa'
091190051219     c                   Leavesr
091200051219     c                   EndIf
091210051219    1c                   EndIf
091220051207
091230051207     c                   EndSr
091240051207
091250051207      *------------------------------------------------------------------------*
091260051212      * CONTROLLO TIPO COMUNICAZIONE FINE GIACENZA
091270051207      *------------------------------------------------------------------------*
091280051207     c     Sr_Ctrtcf     BegSr
091290051212
091300051212      * Ricerca
091310051212     c                   Clear                   v6dtcf
091320051212     c                   If        v6ctcf = '?'
091330051212     c                   Eval      kcod = '2H'
091340051212     c                   call      'TRUL19R'
091350051212     c                   parm                    kcod
091360051212     c                   parm                    ordina
091370051212     c                   parm                    kkey
091380051212     c                   parm                    comando
091390051212     c                   Eval      v6ctcf = kkey
091400051212     c                   Eval      h6riga = 08
091410051212     c                   Eval      h6colo = 38
091420051212     c                   Eval      *In90 = *On
091430051212     c                   EndIf
091440051212
091450051212      * Controllo
091460051212     c                   Clear                   v6dtcf
091470051212     c                   Clear                   ds2h
091480051212     c                   Eval      kcod = '2H'
091490051212     c                   Eval      kkey = v6ctcf
091500051212     c     kTabel        Chain     Tabel00f
091510051212     c                   If        Not %Found(Tabel00f) or
091520051212     c                             tblflg <> *Blanks
091530051212     c                   Eval      *In28 = *On
091540051212     c                   Eval      h6riga = 08
091550051212     c                   Eval      h6colo = 38
091560060216     c                   Eval      v6cmsg = msg(70)
091570051212     c                   Leavesr
091580051212     c                   EndIf
091590051212     c                   Eval      ds2h = tbluni
091600051212     c                   Eval      v6dtcf = §2hdes
091610051212
091620051212      * Se tipo comunicazione con invio tramite fax
091630051212if  1c                   If        §2hfax = 'F'
091640051212      * Non ammesso per i clienti vari
091650051212     c                   If        wksc04 = 8888 or wksc04 = 9999
091660051212     c                   Eval      *In28 = *On
091670051212     c                   Eval      h6riga = 08
091680051212     c                   Eval      h6colo = 38
091690051212     c                   Eval      v6cmsg = msg(63)
091700051212     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
091710051212     c                             'per clienti vari'
091720051212     c                   Leavesr
091730051212     c                   EndIf
091740051216      * Controllo se fax sul luogo 005 è esatto
091750051216if  2c                   If        *In14 and wfax005 <> *Blanks
091760051216     c                   Clear                   trul42ds
091770051216     c                   Eval      d42fax = wfax005
091780051216     c                   Call      'TRUL42R'
091790051216     c                   Parm                    trul42ds
091800051216if  3c                   If        d42err = '1'
091810051216     c                   Eval      *In28 = *On
091820051216     c                   Eval      h6riga = 08
091830051216     c                   Eval      h6colo = 38
091840051216     c                   Eval      v6cmsg = d42msg
091850051216     c                   Leavesr
091860051216    3c                   EndIf
091870051216    2c                   EndIf
091880051216      * Se non c'è il luogo o se il n. fax non è presente sul luogo
091890051216      * deve essere impostato sull'anagrafica
091900051216     c                   If        v2ctlf = *Blanks and wfax005 = *Blanks
091910051212     c                   Eval      *In28 = *On
091920051212     c                   Eval      h6riga = 08
091930051212     c                   Eval      h6colo = 38
091940051212     c                   Eval      v6cmsg = msg(63)
091950051212     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
091960051212     c                             'nr. fax obbligatorio'
091970051212     c                   Leavesr
091980051212     c                   EndIf
091990051212    1c                   EndIf
092000051219      * Se tipo comunicazione con invio tramite mail
092010051219if  1c                   If        §2hfax = 'M'
092020051219      * Non ammesso per i clienti vari
092030051219     c                   If        wksc04 = 8888 or wksc04 = 9999
092040051219     c                   Eval      *In28 = *On
092050051219     c                   Eval      h6riga = 08
092060051219     c                   Eval      h6colo = 38
092070051219     c                   Eval      v6cmsg = msg(63)
092080051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
092090051219     c                             'per clienti vari'
092100051219     c                   Leavesr
092110051219     c                   EndIf
092120051219      * Controllo se c'è la mail nel luogo
092130051219     c                   If        *In14 and wmail005 = *Blanks
092140051219     c                   Eval      *In28 = *On
092150051219     c                   Eval      h6riga = 08
092160051219     c                   Eval      h6colo = 38
092170051219     c                   Eval      v6cmsg = msg(63)
092180051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
092190051219     c                             'mail non immessa nel luogo 005'
092200051219     c                   Leavesr
092210051219     c                   EndIf
092220051219      * Se non c'è il luogo deve esserci la nota 88
092230051219     c                   If        Not *In14 and Not *In15
092240051219     c                   Eval      *In28 = *On
092250051219     c                   Eval      h6riga = 08
092260051219     c                   Eval      h6colo = 38
092270051219     c                   Eval      v6cmsg = msg(63)
092280051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
092290051219     c                             'nota 88 non immessa'
092300051219     c                   Leavesr
092310051219     c                   EndIf
092320051219    1c                   EndIf
092330051207
092340051207     c                   EndSr
092350051207
092360051207      *------------------------------------------------------------------------*
092370051212      * CONTROLLO APERTURA AUTOMATICA GIACENZA
092380051207      *------------------------------------------------------------------------*
092390051207     c     Sr_Ctrapg     BegSr
092400051212
092410051212      * Ricerca
092420051212     c                   Clear                   v6dapg
092430051212     c                   If        v6capg = '?'
092440051212     c                   Eval      kcod = '2U'
092450051212     c                   call      'TRUL19R'
092460051212     c                   parm                    kcod
092470051212     c                   parm                    ordina
092480051212     c                   parm                    kkey
092490051212     c                   parm                    comando
092500051212     c                   Eval      v6capg = kkey
092510051212     c                   Eval      h6riga = 09
092520051212     c                   Eval      h6colo = 38
092530051212     c                   Eval      *In90 = *On
092540051212     c                   EndIf
092550051212
092560051212      * Controllo
092570051212     c                   Clear                   v6dapg
092580051212     c                   Clear                   ds2u
092590051212     c                   Eval      kcod = '2U'
092600051212     c                   Eval      kkey = v6capg
092610051212     c     kTabel        Chain     Tabel00f
092620051212     c                   If        Not %Found(Tabel00f) or
092630051212     c                             tblflg <> *Blanks
092640051212     c                   Eval      *In28 = *On
092650051212     c                   Eval      h6riga = 09
092660051212     c                   Eval      h6colo = 38
092670051212     c                   Eval      v6cmsg = msg(64)
092680051212     c                   Leavesr
092690051212     c                   EndIf
092700051212     c                   Eval      ds2u = tbluni
092710051212     c                   Eval      v6dapg = §2udes
092720051212
092730051212      * Non ammessa apertura automatica per i clienti vari
092740051212     c                   If        §2uape = 'S' and
092750051212     c                             (wksc04 = 8888 or wksc04 = 9999)
092760051212     c                   Eval      *In28 = *On
092770051212     c                   Eval      h6riga = 09
092780051212     c                   Eval      h6colo = 38
092790051212     c                   Eval      v6cmsg = msg(65)
092800051212     c                   Leavesr
092810051212     c                   EndIf
092820051207
092830051207     c                   EndSr
092840051207
092850051207      *------------------------------------------------------------------------*
092860051212      * CONTROLLO DISPOSIZIONI MITTENTE IN ARRIVO
092870051207      *------------------------------------------------------------------------*
092880051207     c     Sr_Ctrdmg     BegSr
092890051212
092900051212      * Non ammesse disposizioni mittente in arrivo per i clienti vari
092910051212     c                   If        v6cdmg = 'SI' and
092920051212     c                             (wksc04 = 8888 or wksc04 = 9999)
092930051212     c                   Eval      *In28 = *On
092940051212     c                   Eval      h6riga = 10
092950051212     c                   Eval      h6colo = 38
092960051212     c                   Eval      v6cmsg = msg(66)
092970051212     c                   Leavesr
092980051212     c                   EndIf
092990051207
093000051207     c                   EndSr
093010060801
093020060801      *------------------------------------------------------------------------*
093030060801      * CONTROLLO CHIUSURA AUTOMATICA GIACENZA
093040060801      *------------------------------------------------------------------------*
093050060801     c     Sr_Ctrcag     BegSr
093060060801
093070060801      * Non ammessa chiusura automatica giacenza per i clienti vari
093080060801     c                   If        v6ccag = 'SI' and
093090060801     c                             (wksc04 = 8888 or wksc04 = 9999)
093100060801     c                   Eval      *In28 = *On
093110060801     c                   Eval      h6riga = 11
093120060801     c                   Eval      h6colo = 38
093130060801     c                   Eval      v6cmsg = msg(74)
093140060801     c                   Leavesr
093150060801     c                   EndIf
093160060801
093170060801     c                   EndSr
093180051207
093190051207      *------------------------------------------------------------------------*
093200051220      * CONTROLLO LUOGO 005 - LUOGO SPEDIZIONE GIACENZA
093210051207      *------------------------------------------------------------------------*
093220051207     c     Sr_Ctrl005    BegSr
093230051212
093240051212if  1c                   If        v6cl005 <> *Blanks
093250060203
093260060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
093270060203     c                   If        *In05 and Not *In14
093280051212     c                   Eval      *In28 = *On
093290051212     c                   Eval      h6riga = 13
093300060203     c                   Eval      h6colo = 35
093310051212     c                   Eval      v6cmsg = msg(50)
093320051212     c                   Leavesr
093330051212     c                   EndIf
093340071128
093350071128      * se non è interrogazione controllo se luogo utilizzabile
093360071128     c                   if        not *in09 and not *in05 and $ufi005 <> 'S'
093370071128     c                   eval      *In28 = *on
093380071128     c                   eval      h6riga = 13
093390071128     c                   eval      h6colo = 35
093400071128     c                   eval      v6cmsg = msg(76)
093410071128     c                   leavesr
093420071128     c                   endif
093430051212
093440060203      * Imposto se interrogazione o gestione
093450051213     c                   Clear                   tnta81ds
093460060203     c   05              Eval      i81opz = '5'
093470060203     c  n05
093480060214     can 14              Eval      i81opz = '2'
093490051213     c                   Eval      i81cod = '005'
093500051213     c                   Eval      i81cli = v1cksc
093510060216     c                   Eval      i81gcli = '1'
093520060216     c   01              Eval      i81icli = '1'
093530051213     c                   Call      'TNTA81R'
093540051213     c                   Parm                    kpjba
093550051213     c                   Parm                    tnta81ds
093560051212
093570051212      * Controllo se ora c'è o non c'è più il luogo
093580060112     c                   Clear                   wfax005
093590060112     c                   Clear                   wmail005
093600051216     c                   Eval      kspecli = v1cksc
093610051216     c                   Eval      kspecod = '005'
093620051212     c     kFnspe        Chain     Fnspe01l
093630051212     c                   If        %Found(Fnspe01l) and speflg = *Blanks
093640051212     c                   Eval      *In14 = *On
093650101026     c                   Eval      wfax005 = spefax
093660101026     c     kFnsp2        Chain     Fnsp201l
093670101026     c                   If        %Found(Fnsp201l) and sp2flg = *Blanks
093680101026     c                   Eval      wmail005 = sp2est
093690101026     c                   EndIf
093700051212     c                   Else
093710051212     c                   Eval      *In14 = *Off
093720051212     c                   EndIf
093730051212
093740051212     c                   Clear                   v6cl005
093750051212     c                   Eval      h6riga = 13
093760060203     c                   Eval      h6colo = 35
093770051212     c                   Goto      emi06
093780051212
093790051212    1c                   EndIf
093800051207
093810051207     c                   EndSr
093820051207
093830051207      *------------------------------------------------------------------------*
093840051220      * CONTROLLO NOTA 88 - E-MAIL GIACENZE
093850051207      *------------------------------------------------------------------------*
093860051207     c     Sr_Ctrnt88    BegSr
093870051212
093880051212if  1c                   If        v6cnt88 <> *Blanks
093890060203
093900060203      * Se sono in interrogazione non posso interrogare la nota se non c'è
093910060203     c                   If        *In05 and Not *In15
093920051212     c                   Eval      *In28 = *On
093930051212     c                   Eval      h6riga = 14
093940060203     c                   Eval      h6colo = 35
093950051219     c                   Eval      v6cmsg = msg(52)
093960051212     c                   Leavesr
093970051212     c                   EndIf
093980051212
093990060203      * Imposto se interrogazione o gestione
094000060203     c  n05              Eval      wrich = 'M'
094010060203     c   05              Eval      wrich = 'V'
094020060217     c                   Eval      wrag = v3crag
094030051212     c                   Eval      wtnt = '88'
094040051212     c                   ExSr      Sr_f02
094050060215
094060060215     c                   If        ta12err <> *Blanks
094070060215     c                   Eval      *In28 = *On
094080060215     c                   Eval      h6riga = 14
094090060215     c                   Eval      h6colo = 35
094100060215     c                   Eval      v6cmsg = ta12msg
094110060215     c                   Leavesr
094120060215     c                   EndIf
094130051212
094140060215      * Controllo se ora c'è o non c'è più la nota
094150091112     c   70
094160091112     corn06              Eval      kntcapl = 'C'
094170100806     c   06
094180091112     cann70              Eval      kntcapl = 'T'
094190051216     c                   Movel(p)  dutkci        kntcnk1
094200091112     c   70
094210091112     corn06              Move      v1cksc        kntcnk1
094220100806     c   06
094230100806     cann70              Move      v1cnrv        kntcnk1
094240051216     c                   Clear                   kntcnk2
094250051216     c                   Movel     '88'          kntctnt
094260091119     c     kTfntc        Chain(n)  Tfntc01l
094270051212     c                   If        %Found(Tfntc01l)
094280051212     c                   Eval      *In15 = *On
094290051212     c                   Else
094300051212     c                   Eval      *In15 = *Off
094310051212     c                   EndIf
094320051212
094330051212     c                   Clear                   v6cnt88
094340051212     c                   Eval      h6riga = 14
094350060203     c                   Eval      h6colo = 35
094360051212     c                   Goto      emi06
094370051212
094380051212    1c                   EndIf
094390051207
094400051207     c                   EndSr
094410060208
094420060208      *------------------------------------------------------------------------*
094430060208      * CONTROLLO LUOGO 300 - MERCE RESA
094440060208      *------------------------------------------------------------------------*
094450060208     c     Sr_Ctrl300    BegSr
094460060208
094470060208if  1c                   If        v6cl300 <> *Blanks
094480060208
094490060208      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
094500060208     c                   If        *In05 and Not *In33
094510060208     c                   Eval      *In28 = *On
094520060208     c                   Eval      h6riga = 15
094530060208     c                   Eval      h6colo = 35
094540060208     c                   Eval      v6cmsg = msg(50)
094550060208     c                   Leavesr
094560060208     c                   EndIf
094570071128
094580071128      * se non è interrogazione controllo se luogo utilizzabile
094590071128     c                   if        not *in09 and not *in05 and $ufi300 <> 'S'
094600071128     c                   eval      *In28 = *on
094610071128     c                   eval      h6riga = 15
094620071128     c                   eval      h6colo = 35
094630071128     c                   eval      v6cmsg = msg(76)
094640071128     c                   leavesr
094650071128     c                   endif
094660060208
094670060208      * Imposto se interrogazione o gestione
094680060208     c                   Clear                   tnta81ds
094690060208     c   05              Eval      i81opz = '5'
094700060208     c  n05
094710060214     can 33              Eval      i81opz = '2'
094720060208     c                   Eval      i81cod = '300'
094730060208     c                   Eval      i81cli = v1cksc
094740060216     c                   Eval      i81gcli = '1'
094750060216     c   01              Eval      i81icli = '1'
094760060208     c                   Call      'TNTA81R'
094770060208     c                   Parm                    kpjba
094780060208     c                   Parm                    tnta81ds
094790060208
094800060208      * Controllo se ora c'è o non c'è più il luogo
094810060208     c                   Eval      kspecli = v1cksc
094820060208     c                   Eval      kspecod = '300'
094830060208     c     kFnspe        Chain     Fnspe01l
094840060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
094850060208     c                   Eval      *In33 = *On
094860060208     c                   Else
094870060208     c                   Eval      *In33 = *Off
094880060208     c                   EndIf
094890060208
094900060208     c                   Clear                   v6cl300
094910060208     c                   Eval      h6riga = 15
094920060208     c                   Eval      h6colo = 35
094930060208     c                   Goto      emi06
094940060208
094950060208    1c                   EndIf
094960060208
094970060208     c                   EndSr
094980051213
094990051213      *------------------------------------------------------------------------*
095000051213      * CONTROLLO LUOGO 006 - PREAVVISO/AVVISO DANNO
095010051213      *------------------------------------------------------------------------*
095020051213     c     Sr_Ctrl006    BegSr
095030051213
095040051213if  1c                   If        v7cl006 <> *Blanks
095050060203
095060060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
095070060203     c                   If        *In05 and Not *In16
095080051213     c                   Eval      *In28 = *On
095090051213     c                   Eval      h7riga = 12
095100060203     c                   Eval      h7colo = 35
095110051213     c                   Eval      v7cmsg = msg(50)
095120051213     c                   Leavesr
095130051213     c                   EndIf
095140071128
095150071128      * se non è interrogazione controllo se luogo utilizzabile
095160071128     c                   if        not *in09 and not *in05 and $ufi006 <> 'S'
095170071128     c                   eval      *In28 = *on
095180071128     c                   eval      h7riga = 12
095190071128     c                   eval      h7colo = 35
095200071128     c                   eval      v7cmsg = msg(76)
095210071128     c                   leavesr
095220071128     c                   endif
095230051213
095240060203      * Imposto se interrogazione o gestione
095250051213     c                   Clear                   tnta81ds
095260060203     c   05              Eval      i81opz = '5'
095270060203     c  n05
095280060214     can 16              Eval      i81opz = '2'
095290051213     c                   Eval      i81cod = '006'
095300051213     c                   Eval      i81cli = v1cksc
095310060216     c                   Eval      i81gcli = '1'
095320060216     c   01              Eval      i81icli = '1'
095330051213     c                   Call      'TNTA81R'
095340051213     c                   Parm                    kpjba
095350051213     c                   Parm                    tnta81ds
095360051213
095370051213      * Controllo se ora c'è o non c'è più il luogo
095380051216     c                   Eval      kspecli = v1cksc
095390051216     c                   Eval      kspecod = '006'
095400051213     c     kFnspe        Chain     Fnspe01l
095410051213     c                   If        %Found(Fnspe01l) and speflg = *Blanks
095420051213     c                   Eval      *In16 = *On
095430051213     c                   Else
095440051213     c                   Eval      *In16 = *Off
095450051213     c                   EndIf
095460051213
095470051213     c                   Clear                   v7cl006
095480051213     c                   Eval      h7riga = 12
095490060203     c                   Eval      h7colo = 35
095500051213     c                   Goto      emi07
095510051213
095520051213    1c                   EndIf
095530051213
095540051213     c                   EndSr
095550051213
095560051213      *------------------------------------------------------------------------*
095570051220      * CONTROLLO NOTA 87 - E-MAIL DANNI
095580051213      *------------------------------------------------------------------------*
095590051213     c     Sr_Ctrnt87    BegSr
095600051213
095610051213if  1c                   If        v7cnt87 <> *Blanks
095620060203
095630060203      * Se sono in interrogazione non posso interrogare la nota se non c'è
095640060203     c                   If        *In05 and Not *In18
095650051213     c                   Eval      *In28 = *On
095660051213     c                   Eval      h7riga = 13
095670060203     c                   Eval      h7colo = 35
095680051219     c                   Eval      v7cmsg = msg(52)
095690051213     c                   Leavesr
095700051213     c                   EndIf
095710051213
095720060203      * Imposto se interrogazione o gestione
095730060203     c  n05              Eval      wrich = 'M'
095740060203     c   05              Eval      wrich = 'V'
095750060217     c                   Eval      wrag = v3crag
095760051213     c                   Eval      wtnt = '87'
095770051213     c                   ExSr      Sr_f02
095780060215
095790060215     c                   If        ta12err <> *Blanks
095800060215     c                   Eval      *In28 = *On
095810060215     c                   Eval      h7riga = 13
095820060215     c                   Eval      h7colo = 35
095830060215     c                   Eval      v7cmsg = ta12msg
095840060215     c                   Leavesr
095850060215     c                   EndIf
095860051213
095870060215      * Controllo se ora c'è o non c'è più la nota
095880091112     c   70
095890091112     corn06              Eval      kntcapl = 'C'
095900100806     c   06
095910091112     cann70              Eval      kntcapl = 'T'
095920051216     c                   Movel(p)  dutkci        kntcnk1
095930091112     c   70
095940091112     corn06              Move      v1cksc        kntcnk1
095950100806     c   06
095960100806     cann70              Move      v1cnrv        kntcnk1
095970051216     c                   Clear                   kntcnk2
095980051216     c                   Movel     '87'          kntctnt
095990091119     c     kTfntc        Chain(n)  Tfntc01l
096000051213     c                   If        %Found(Tfntc01l)
096010051213     c                   Eval      *In18 = *On
096020051213     c                   Else
096030051213     c                   Eval      *In18 = *Off
096040051213     c                   EndIf
096050051213
096060051213     c                   Clear                   v7cnt87
096070051213     c                   Eval      h7riga = 13
096080060203     c                   Eval      h7colo = 35
096090051213     c                   Goto      emi07
096100051213
096110051213    1c                   EndIf
096120051213
096130051213     c                   EndSr
096140051213
096150051213      *------------------------------------------------------------------------*
096160051213      * CONTROLLO LUOGO 008 - PROGETTO DI LIQUIDAZIONE
096170051213      *------------------------------------------------------------------------*
096180051213     c     Sr_Ctrl008    BegSr
096190051213
096200051213if  1c                   If        v7cl008 <> *Blanks
096210060203
096220060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
096230060203     c                   If        *In05 and Not *In17
096240051213     c                   Eval      *In28 = *On
096250051213     c                   Eval      h7riga = 14
096260060203     c                   Eval      h7colo = 35
096270051213     c                   Eval      v7cmsg = msg(50)
096280051213     c                   Leavesr
096290051213     c                   EndIf
096300071128
096310071128      * se non è interrogazione controllo se luogo utilizzabile
096320071128     c                   if        not *in09 and not *in05 and $ufi008 <> 'S'
096330071128     c                   eval      *In28 = *on
096340071128     c                   eval      h7riga = 14
096350071128     c                   eval      h7colo = 35
096360071128     c                   eval      v7cmsg = msg(76)
096370071128     c                   leavesr
096380071128     c                   endif
096390051213
096400060203      * Imposto se interrogazione o gestione
096410051213     c                   Clear                   tnta81ds
096420060203     c   05              Eval      i81opz = '5'
096430060203     c  n05
096440060214     can 17              Eval      i81opz = '2'
096450051213     c                   Eval      i81cod = '008'
096460051213     c                   Eval      i81cli = v1cksc
096470060216     c                   Eval      i81gcli = '1'
096480060216     c   01              Eval      i81icli = '1'
096490051213     c                   Call      'TNTA81R'
096500051213     c                   Parm                    kpjba
096510051213     c                   Parm                    tnta81ds
096520051213
096530051213      * Controllo se ora c'è o non c'è più il luogo
096540051216     c                   Eval      kspecli = v1cksc
096550051216     c                   Eval      kspecod = '008'
096560051213     c     kFnspe        Chain     Fnspe01l
096570051213     c                   If        %Found(Fnspe01l) and speflg = *Blanks
096580051213     c                   Eval      *In17 = *On
096590051213     c                   Else
096600051213     c                   Eval      *In17 = *Off
096610051213     c                   EndIf
096620051213
096630051213     c                   Clear                   v7cl008
096640051213     c                   Eval      h7riga = 14
096650060203     c                   Eval      h7colo = 35
096660051213     c                   Goto      emi07
096670051213
096680051213    1c                   EndIf
096690051213
096700051213     c                   EndSr
096710051213
096720051213      *------------------------------------------------------------------------*
096730051213      * CONTROLLO NUMERO STOP PER RITIRO
096740051213      *------------------------------------------------------------------------*
096750051213     c     Sr_Ctrstp     BegSr
096760051213
096770051213     c                   If        v7cstp > 800
096780051213     c                   Eval      *In28 = *On
096790051213     c                   Eval      h7riga = 18
096800051213     c                   Eval      h7colo = 37
096810051213     c                   Eval      v7cmsg = msg(67)
096820051213     c                   Leavesr
096830051213     c                   EndIf
096840051213
096850051213     c                   EndSr
096860060208
096870060208      *------------------------------------------------------------------------*
096880060208      * CONTROLLO LUOGO 002 - INVIO ORM ESTERO
096890060208      *------------------------------------------------------------------------*
096900060208     c     Sr_Ctrl002    BegSr
096910060208
096920060208if  1c                   If        v7cl002 <> *Blanks
096930060208
096940060208      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
096950060208     c                   If        *In05 and Not *In35
096960060208     c                   Eval      *In28 = *On
096970060208     c                   Eval      h7riga = 20
096980060208     c                   Eval      h7colo = 35
096990060208     c                   Eval      v7cmsg = msg(50)
097000060208     c                   Leavesr
097010060208     c                   EndIf
097020071128
097030071128      * se non è interrogazione controllo se luogo utilizzabile
097040071128     c                   if        not *in09 and not *in05 and $ufi002 <> 'S'
097050071128     c                   eval      *In28 = *on
097060071128     c                   eval      h7riga = 20
097070071128     c                   eval      h7colo = 35
097080071128     c                   eval      v7cmsg = msg(76)
097090071128     c                   leavesr
097100071128     c                   endif
097110060208
097120060208      * Imposto se interrogazione o gestione
097130060208     c                   Clear                   tnta81ds
097140060208     c   05              Eval      i81opz = '5'
097150060208     c  n05
097160060214     can 35              Eval      i81opz = '2'
097170060208     c                   Eval      i81cod = '002'
097180060208     c                   Eval      i81cli = v1cksc
097190060216     c                   Eval      i81gcli = '1'
097200060216     c   01              Eval      i81icli = '1'
097210060208     c                   Call      'TNTA81R'
097220060208     c                   Parm                    kpjba
097230060208     c                   Parm                    tnta81ds
097240060208
097250060208      * Controllo se ora c'è o non c'è più il luogo
097260060208     c                   Eval      kspecli = v1cksc
097270060208     c                   Eval      kspecod = '002'
097280060208     c     kFnspe        Chain     Fnspe01l
097290060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
097300060208     c                   Eval      *In35 = *On
097310060208     c                   Else
097320060208     c                   Eval      *In35 = *Off
097330060208     c                   EndIf
097340060208
097350060208     c                   Clear                   v7cl002
097360060208     c                   Eval      h7riga = 20
097370060208     c                   Eval      h7colo = 35
097380060208     c                   Goto      emi07
097390060208
097400060208    1c                   EndIf
097410060208
097420060208     c                   EndSr
097430060208
097440060208      *------------------------------------------------------------------------*
097450060208      * CONTROLLO NOTA 83 - E-MAIL ORM ESTERO
097460060208      *------------------------------------------------------------------------*
097470060208     c     Sr_Ctrnt83    BegSr
097480060208
097490060208if  1c                   If        v7cnt83 <> *Blanks
097500060208
097510060208      * Se sono in interrogazione non posso interrogare la nota se non c'è
097520060208     c                   If        *In05 and Not *In34
097530060208     c                   Eval      *In28 = *On
097540060208     c                   Eval      h7riga = 21
097550060208     c                   Eval      h7colo = 35
097560060208     c                   Eval      v7cmsg = msg(52)
097570060208     c                   Leavesr
097580060208     c                   EndIf
097590060208
097600060208      * Imposto se interrogazione o gestione
097610060208     c  n05              Eval      wrich = 'M'
097620060208     c   05              Eval      wrich = 'V'
097630060217     c                   Eval      wrag = v3crag
097640060208     c                   Eval      wtnt = '83'
097650060208     c                   ExSr      Sr_f02
097660060215
097670060215     c                   If        ta12err <> *Blanks
097680060215     c                   Eval      *In28 = *On
097690060215     c                   Eval      h7riga = 21
097700060215     c                   Eval      h7colo = 35
097710060215     c                   Eval      v7cmsg = ta12msg
097720060215     c                   Leavesr
097730060215     c                   EndIf
097740060208
097750060215      * Controllo se ora c'è o non c'è più la nota
097760091112     c   70
097770091112     corn06              Eval      kntcapl = 'C'
097780100806     c   06
097790091112     cann70              Eval      kntcapl = 'T'
097800060208     c                   Movel(p)  dutkci        kntcnk1
097810091112     c   70
097820091112     corn06              Move      v1cksc        kntcnk1
097830100806     c   06
097840100806     cann70              Move      v1cnrv        kntcnk1
097850060208     c                   Clear                   kntcnk2
097860060208     c                   Movel     '83'          kntctnt
097870091119     c     kTfntc        Chain(n)  Tfntc01l
097880060208     c                   If        %Found(Tfntc01l)
097890060208     c                   Eval      *In34 = *On
097900060208     c                   Else
097910060208     c                   Eval      *In34 = *Off
097920060208     c                   EndIf
097930060208
097940060208     c                   Clear                   v7cnt83
097950060208     c                   Eval      h7riga = 21
097960060208     c                   Eval      h7colo = 35
097970060208     c                   Goto      emi07
097980060208
097990060208    1c                   EndIf
098000060208
098010060208     c                   EndSr
098020060208
098030060208      *------------------------------------------------------------------------*
098040060208      * CONTROLLO LUOGO 200 - INVIO DOC. DOGANA
098050060208      *------------------------------------------------------------------------*
098060060208     c     Sr_Ctrl200    BegSr
098070060208
098080060208if  1c                   If        v7cl200 <> *Blanks
098090060208
098100060208      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
098110060208     c                   If        *In05 and Not *In37
098120060208     c                   Eval      *In28 = *On
098130060208     c                   Eval      h7riga = 20
098140060208     c                   Eval      h7colo = 72
098150060208     c                   Eval      v7cmsg = msg(50)
098160060208     c                   Leavesr
098170060208     c                   EndIf
098180071128
098190071128      * se non è interrogazione controllo se luogo utilizzabile
098200071128     c                   if        not *in09 and not *in05 and $ufi200 <> 'S'
098210071128     c                   eval      *In28 = *on
098220071128     c                   eval      h7riga = 20
098230071128     c                   eval      h7colo = 72
098240071128     c                   eval      v7cmsg = msg(76)
098250071128     c                   leavesr
098260071128     c                   endif
098270060208
098280060208      * Imposto se interrogazione o gestione
098290060208     c                   Clear                   tnta81ds
098300060208     c   05              Eval      i81opz = '5'
098310060208     c  n05
098320060214     can 37              Eval      i81opz = '2'
098330060208     c                   Eval      i81cod = '200'
098340060208     c                   Eval      i81cli = v1cksc
098350060216     c                   Eval      i81gcli = '1'
098360060216     c   01              Eval      i81icli = '1'
098370060208     c                   Call      'TNTA81R'
098380060208     c                   Parm                    kpjba
098390060208     c                   Parm                    tnta81ds
098400060208
098410060208      * Controllo se ora c'è o non c'è più il luogo
098420060208     c                   Eval      kspecli = v1cksc
098430060208     c                   Eval      kspecod = '200'
098440060208     c     kFnspe        Chain     Fnspe01l
098450060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
098460060208     c                   Eval      *In37 = *On
098470060208     c                   Else
098480060208     c                   Eval      *In37 = *Off
098490060208     c                   EndIf
098500060208
098510060208     c                   Clear                   v7cl200
098520060208     c                   Eval      h7riga = 20
098530060208     c                   Eval      h7colo = 72
098540060208     c                   Goto      emi07
098550060208
098560060208    1c                   EndIf
098570060208
098580060208     c                   EndSr
098590060208
098600060208      *------------------------------------------------------------------------*
098610060208      * CONTROLLO NOTA 86 - E-MAIL MANIFEST
098620060208      *------------------------------------------------------------------------*
098630060208     c     Sr_Ctrnt86    BegSr
098640060208
098650060208if  1c                   If        v7cnt86 <> *Blanks
098660060208
098670060208      * Se sono in interrogazione non posso interrogare la nota se non c'è
098680060208     c                   If        *In05 and Not *In36
098690060208     c                   Eval      *In28 = *On
098700060208     c                   Eval      h7riga = 21
098710060208     c                   Eval      h7colo = 72
098720060208     c                   Eval      v7cmsg = msg(52)
098730060208     c                   Leavesr
098740060208     c                   EndIf
098750060208
098760060208      * Imposto se interrogazione o gestione
098770060208     c  n05              Eval      wrich = 'M'
098780060208     c   05              Eval      wrich = 'V'
098790060217     c                   Eval      wrag = v3crag
098800060208     c                   Eval      wtnt = '86'
098810060208     c                   ExSr      Sr_f02
098820060215
098830060215     c                   If        ta12err <> *Blanks
098840060215     c                   Eval      *In28 = *On
098850060215     c                   Eval      h7riga = 21
098860060215     c                   Eval      h7colo = 72
098870060215     c                   Eval      v7cmsg = ta12msg
098880060215     c                   Leavesr
098890060215     c                   EndIf
098900060208
098910060215      * Controllo se ora c'è o non c'è più la nota
098920091112     c   70
098930091112     corn06              Eval      kntcapl = 'C'
098940100806     c   06
098950091112     cann70              Eval      kntcapl = 'T'
098960060208     c                   Movel(p)  dutkci        kntcnk1
098970091112     c   70
098980091112     corn06              Move      v1cksc        kntcnk1
098990100806     c   06
099000100806     cann70              Move      v1cnrv        kntcnk1
099010060208     c                   Clear                   kntcnk2
099020060208     c                   Movel     '86'          kntctnt
099030091119     c     kTfntc        Chain(n)  Tfntc01l
099040060208     c                   If        %Found(Tfntc01l)
099050060208     c                   Eval      *In36 = *On
099060060208     c                   Else
099070060208     c                   Eval      *In36 = *Off
099080060208     c                   EndIf
099090060208
099100060208     c                   Clear                   v7cnt86
099110060208     c                   Eval      h7riga = 21
099120060208     c                   Eval      h7colo = 72
099130060208     c                   Goto      emi07
099140060208
099150060208    1c                   EndIf
099160060208
099170060208     c                   EndSr
099180051220
099190051220      *-------------------------------------------------------------------------
099200051220      * CONTROLLO IL BLOCCO/SBLOCCO DEL CLIENTE
099210051220      *-------------------------------------------------------------------------
099220051220     c     Sr_Ctrabl     BegSr
099230051220
099240051220     c                   Eval      wokabl = *Off
099250051220     c                   Clear                   w02tes
099260051220     c                   Clear                   w02msg
099270051220
099280051220      * se ho aggiornato blocco/sblocco i clienti di fnacr
099290051220if  1c                   If        acoabl <> *Blanks and savabl = *Blanks
099300051220     c                   Eval      w02tes = 'Il cliente è stato bloccato'
099310051220      * cliente bloccato richiamo pgm esterno per bloccare fnacr
099320051220     c                   Clear                   fior50ds
099330051220     c                   Move      acoksc        i50ksc
099340140113     c                   eval      I50abl = 'B'
099350051220     c                   Call      'FIOR50R'
099360051220     c                   Parm                    fior50ds
099370051220      * se torna errore emetto il msg di errore
099380051220if  2c                   If        o50err <> *Blanks
099390051220     c                   Eval      w02msg = o50msg
099400051220      * emetto il msg che ho bloccato i clienti su fnacr
099410051220   x2c                   Else
099420051220     c                   Eval      w02msg = 'Anagrafiche clienti ritiro collega-
099430051220     c                             te bloccate'
099440051220    2c                   EndIf
099450051220     c                   Eval      wokabl = *On
099460051220    1c                   EndIf
099470051220if  1c                   If        acoabl = *Blanks and savabl <> *Blanks
099480051220     c                   Eval      w02tes = 'Il cliente è stato sbloccato'
099490051220      * cliente sbloccato emetto solo il msg
099500051220     c                   Eval      w02msg = 'Anagrafiche clienti ritiro collega-
099510051220     c                             te non aggiornate'
099520051220     c                   Eval      wokabl = *On
099530051220    1c                   EndIf
099540051220
099550051220      * Emetto la window x avvisare l'utente
099560051220     c                   If        wokabl = *On
099570051220     c                   Exfmt     ta60w02
099580051220     c                   EndIf
099590051220
099600051220     c                   EndSr
099610100128
099620100128      *-------------------------------------------------------------------------
099630100128      * Richiamo interrogazione codici fatturazione con stessa p.IVA
099640100128      *-------------------------------------------------------------------------
099650100128     c     sr_ta40       begsr
099660100128
099670100128     c                   clear                   tnta40ds
099680100128     c                   IF        v2ivaeu = *blanks
099690100128     c                   eval      ITA40iva = v2ivait
099700100128     c                   else
099710100128     c                   eval      ITA40iva = v2civa
099720100128     c                   ENDIF
099730100128     c  n06              eval      ITA40ksc = v1cksc
099740100128     c   06              eval      ITA40ksc = ta60cli
099750100128     c                   eval      ITA40scf = clpscf
099760100128     c                   eval      ITA40rag = v2crag
099770100128     c                   call      'TNTA40R'
099780100128     c                   parm                    kpjba
099790100128     c                   parm                    tnta40ds
099800100128     c                   IF        OTA40err <> *blanks
099810100128     c                   eval      v4cmsg = OTA40msg
099820100128     c                   eval      *in28 = *on
099830100312     c                   leavesr
099840100128     c                   ENDIF
099850100312     c                   IF        OTA40scf = 0
099860100312     c                   Eval      v4cmsg = msg(33)
099870100312     c                   eval      *in28 = *on
099880100312     c                   leavesr
099890100312     c                   ENDIF
099900100128     c                   eval      v4cscf = %editc(OTA40scf:'X')
099910100128     c                   eval      wtnta40 = *on
099920100201     c                   clear                   v4dscf
099930100201     c                   Clear                   Tibs69ds
099940100201     c                   Move      v4cscf        I69kac
099950100201     c                   Call      'TIBS69R'
099960100201     c                   Parm                    Tibs69ds
099970100201     c                   Parm                    ds_cnaco
099980100201     c                   Parm                    ds_cnind
099990100201     c                   Parm                    ds_cnclp
100000100201     c                   Parm                    ds_fncls
100010100201     c                   Eval      wtibs69 = *On
100020100201     c                   Movel     w_acorag      v4dscf
100030100128
100040100128     c                   endsr
100050051216
100060080222      *-------------------------------------------------------------------------
100070080222      * Reperisco la nazione del sottoconto intestazione fattura
100080080222      *-------------------------------------------------------------------------
100090080222     c     Sr_Repscfsta  BegSr
100100080222
100110080222     c                   clear                   w_scfsta
100120080222      * Controllo
100130100127     c                   if        v4cscf <> %editc(v1cksc:'X')
100140110427     c                             and v4cscf > *zeros
100150080222     c                   Clear                   Tibs69ds
100160080222     c                   Move      v4cscf        I69kin
100170080222     c                   Call      'TIBS69R'
100180080222     c                   Parm                    Tibs69ds
100190080222     c                   Parm                    ds_cnaco
100200080222     c                   Parm                    ds_cnind
100210080222     c                   Parm                    ds_cnclp
100220080222     c                   Parm                    ds_fncls
100230080222     c                   Eval      wtibs69 = *On
100240080307     c                   if        o69err=*blanks and w_indflg=*blanks
100250080222     c                   eval      w_scfsta=w_indsta
100260080222     c                   endif
100270080227     c                   else
100280080227     c                   eval      w_scfsta=v2csta
100290080227     c                   endif
100300080222
100310080222     c                   EndSr
100320080306      *------------------------------------------------------------------------*
100330080306      * Controllo presenza codice su altro potenziale come cod.fiscale
100340080306      *------------------------------------------------------------------------*
100350080306     c     Sr_CdfAltroP  BegSr
100360080306     c     codice        setll     tncpo05l
100370080306     c     codice        reade     tncpo05l
100380080306    3c                   dow       not %eof(tncpo05l)
100390080306     c* se trovato un record "filiale memorizzo ragione sociale del potenz.
100400080306     c                   eval      savrag=c_cporag
100410080306     c     codice        reade     tncpo05l
100420080306    3c                   enddo
100430080306    3c                   if        not %eof (tncpo05l)
100440080306     c                   eval      savrag=c_cporag
100450080306    3c                   endif
100460080306     c                   endsr
100470080306      *------------------------------------------------------------------------*
100480080306      * Controllo presenza codice su altro potenziale come piva
100490080306      *------------------------------------------------------------------------*
100500080306     c     Sr_PivAltroP  BegSr
100510080306     c     codice        setll     tncpo06l
100520080306     c     codice        reade     tncpo06l
100530080306    3c                   dow       not %eof(tncpo06l)
100540080306     c* se trovato un record "filiale memorizzo ragione sociale del potenz.
100550080306     c                   eval      savrag=c_cporag
100560080306     c     codice        reade     tncpo06l
100570080306    3c                   enddo
100580080306    3c                   if        not %eof (tncpo06l)
100590080306     c                   eval      savrag=c_cporag
100600080306    3c                   endif
100610080306     c                   endsr
100620080306      *------------------------------------------------------------------------*
100630080306      * Messaggio di errore codice fiscal già presente per altro potenz
100640080306      *------------------------------------------------------------------------*
100650080306     c     Sr_msgcdf     BegSr
100660080306     c* Errore forzabile se trovato un altro potenziale con lo stesso
100670080306     c* codice fiscale
100680080306    3c                   if        savrag<>*blanks
100690080306     c                   eval      cod_forzcdf=codice
100700080306     c                   seton                                        28
100710080306     c                   eval      h2riga = 12
100720080306     c                   eval      h2colo = 28
100730080306     c                   Eval      v2cmsg = msg(77)
100740080306     c                   Leavesr
100750080306    3c                   endif
100760080306     c                   endsr
100770080306      *------------------------------------------------------------------------*
100780080306      * Messaggio di errore partita iva   già presente per altro potenz
100790080306      *------------------------------------------------------------------------*
100800080306     c     Sr_msgiva     BegSr
100810080306
100820080306    3c                   if        savrag<>*blanks
100830080306     c                   eval      cod_forziva=v2civa
100840080306     c                   seton                                        28
100850080306     c                   Eval      h2riga = 12
100860080306     c                   Eval      h2colo = 52
100870080306     c                   Eval      v2cmsg = msg(78)
100880080306     c                   Leavesr
100890080306    3c                   endif
100900080306     c                   endsr
100910080422
100920080422      *------------------------------------------------------------------------*
100930080422      * Emissione Window per inviare mail alla sede
100940080422      *------------------------------------------------------------------------*
100950080422     c     sr_winmail    begsr
100960080422
100970100729     c                   eval      w04uffsede = wuffsede
100980080422Do  2c                   do        *hival
100990080422     c                   exfmt     ta60w04
101000080422If  3c                   if        *inkl
101010080422     c                   leavesr
101020080422    3c                   endif
101030080422    2c                   enddo
101040080422
101050080422     c                   endsr
101060100729      /free
101070100729       //--------------------------------------------------------------
101080100729       //?Descrizione banca.
101090100729       //--------------------------------------------------------------
101100100729       BEGSR DesBanca;
101110100729
101120100729         clear wDesBanca;
101130100729         chain (kabi:kcab) CNABI00F;
101140100729         IF  %Found(CNABI00F) and ABIann <> '*';
101150100729           exsr sr_crebna;
101160100729           wDesbanca = wbanca + wagenzia;
101170100729         ENDIF;
101180100729
101190100729       ENDSR;
101200150427
101210150427       //--------------------------------------------------------------
101220150427       //?Memorizza immagine precedente.
101230150427       //--------------------------------------------------------------
101240150427       BEGSR ImmaginePrima;
101250150427
101260150427         clear TIBS73DS;
101270150427         IBS73immag ='P';
101280150427         tibs73r (TIBS73DS:cnaco_73:cnind_73:cnclp_73:fncls_73);
101290150427         wtibs73 = *on;
101300150427
101310150427       ENDSR;
101320150427
101330150427       //--------------------------------------------------------------
101340150427       //?Memorizza immagine sucessiva.
101350150427       //--------------------------------------------------------------
101360150427       BEGSR ImmagineDopo;
101370150427
101380150427         clear TIBS73DS;
101390150427         IBS73pru = knmus;
101400150427         IBS73noj = knmeb;
101410150427         ibs73pgm = 'TNTA60R';
101420150427         IBS73immag ='D';
101430150427         IF  *in02;
101440150427           IBS73cta = 'M';
101450150427         ENDIF;
101460150427         IF  *in01;
101470150427           IBS73cta = 'I';
101480150427         ENDIF;
101490150427         tibs73r (TIBS73DS:cnaco_73:cnind_73:cnclp_73:fncls_73);
101500150427         wtibs73 = *on;
101510150427
101520150427       ENDSR;
101530160803
101540160803       //--------------------------------------------------------------
101550160803       //?Scrivo l'immagine Precedente.
101560160803       //--------------------------------------------------------------
101570160803       BEGSR ImmaginePrimaCpo;
101580160803
101590160803         clear TRMK30DS;
101600160803         IMK30immag = 'P';
101610160803         IMK30tvp = 'B';
101620160803         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
101630160803
101640160803       ENDSR;
101650160803
101660160803       //--------------------------------------------------------------
101670160803       //?Scrivo Storico Variazioni.
101680160803       //--------------------------------------------------------------
101690160803       BEGSR ScriviVarCpo;
101700160803
101710160803         clear TRMK30DS;
101720160803         IMK30pru = knmus;
101730160803         IMK30noj = knmeb;
101740160803         IMK30pgm = 'TNTA60R';
101750160803         IMK30immag = 'D';
101760160803         IMK30cta = 'M';
101770160803         IMK30tvp = 'B';
101780160803
101790160803         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
101800160803
101810160803       ENDSR;
101820150427
101830100729      /end-free
101840080222
101850051107      *------------------------------------------------------------------------*
101860051107      * ROUTINE INIZIALE
101870051107      *------------------------------------------------------------------------*
101880051107     c     *Inzsr        BegSr
101890051107
101900051107     c     *Entry        Plist
101910051107     c                   Parm                    Kpjba
101920051115     c                   Parm                    Tnta60ds
101930110407     c                   Parm                    ParmCbl
101940091104
101950091104     c                   eval      $interroga = *off
101960091124     c                   eval      $modifica  = *off
101970091106     c                   eval      $pgmrich   = *off
101980110407     c                   eval      $Blocco    = *off
101990051115
102000091104      * PGM Richiamato
102010051115     c                   If        %Parms > 1
102020091106     c                   eval      $pgmrich = *on
102030100806      * trattativa
102040100806     c                   if        ta60flg = 'T'
102050090914     c                   Eval      *In06 = *on
102060091112     c                   eval      *in70 = (ta60cli > 0)
102070091112     c   70              eval      v1cksc = ta60cli
102080091104      * cliente per aggancio a cnaco
102090090914     c                   else
102100090914     c                   eval      *in06 = *off
102110091112     c                   eval      *in70 = *off
102120090914     c                   endif
102130091104      * convalida offerta
102140091104      * sono in manutenzione perchè il cliente viene scritto dal chiamante
102150051115     c                   Eval      *In07 = (ta60flg = 'C')
102160091104      * codice visita/trattativa da agganciare
102170051222     c   06              Eval      v1cnrv = ta60nrv
102180091104      * codice cliente nuovo da agganciare
102190051222     c   07              Eval      v1cksc = ta60nrv
102200091104
102210091104      * inibisco il richiamo all'interrogazione potenziali per errore
102220091104      * chiamata ricorsiva se provengo da gestione potenziali
102230091104     c                   eval      *in65 = (ta60pot = '1')
102240091104      * interrogazione se richiamato da interrogazione tariffe
102250060207     c                   Eval      *In05 = (ta60flg = 'I')
102260091104
102270091104      * richiamato per visualizzazione (nuovo campo della DS)
102280091104     c                   if        ta60int = 'S'
102290091104     c                   eval      $interroga = *on
102300091104      * devo avere il codice cliente/visita/trattativa
102310091104      * visto che non passo dalla prima videata dove viene richiesto il codice
102320091104      * per comodità ricevo tutti e 3 i codici nello stesso campo
102330091104      * tanto o sono da visita o sono da trattativa o sono da clienti
102340091104      * se il codice è a 0 torno l'errore al chiamante
102350091104     c                   if        ta60nrv = 0
102360091104     c                   eval      ta60msg = 'Codice non passato'
102370091104     c                   eval      ta60err = '1'
102380091124     c                   Eval      *In28 = *On
102390091104     c                   leavesr
102400091104     c                   endif
102410091111      * accendo l'indicatore di interrogazione
102420091111     c                   eval      *in05 = *on
102430091111      * se flag TA60FLG = 'I'
102440091111      * imposto il codice cliente, in quanto è l'unico caso non
102450091104      * previsto nelle specifiche precedenti, visita e trattativa
102460091104      * sono già impostate se TA60FLG = 'T' o 'V'
102470091106     c                   if        ta60flg = 'I'
102480091111     c                   eval      v1cksc = ta60nrv
102490091104     c                   endif
102500091104     c                   endif
102510091124      * in caso di pgm richiamato in manutenzione
102520091124     c                   if        ta60flg = 'M'
102530091124     c                   if        ta60cli = 0
102540091124     c                   eval      ta60msg = 'Codice non passato'
102550091124     c                   eval      ta60err = '1'
102560091124     c                   Eval      *In28 = *On
102570091124     c                   leavesr
102580091124     c                   endif
102590091124     c                   eval      $modifica = *on
102600091124     c                   eval      v1cksc = ta60cli
102610091124     c                   endif
102620110407
102630110407      * PGM Richiamato per blocco cliente
102640110407     c                   IF        %Parms > 2
102650110407     c                   IF        Parmcbl = *blanks or Parmcbl = '000'
102660110407     c                   eval      ta60msg = 'Causale blocco errata'
102670110407     c                   eval      ta60err = '1'
102680110407     c                   Eval      *In28 = *On
102690110407     c                   leavesr
102700110407     c                   ENDIF
102710110407     c                   eval      $Blocco = *on
102720110407     c                   ENDIF
102730091104
102740091104      * NON richiamato
102750051115     c                   Else
102760091104      * Interrogazione
102770051115     c                   If        %subst(kpjbu:256:1) = 'V'
102780051115     c                   Eval      *In05 = *On
102790051115     c                   EndIf
102800051222     c                   Clear                   v1cnrv
102810060308     c                   eval      *in65 = *off
102820051115     c                   EndIf
102830101130
102840101130      * Se pgm in interrogazione imposto subito il titolo 'VISUALIZZA'
102850101130     c   05              Eval      vtcmod = mod(03)
102860051107
102870051107     c     *dtaara       define    §azute        azuteds
102880051107     c     *dtaara       define    §datiute      ddatiute
102890051107     c                   in(E)     *dtaara
102900051117     c                   If        %error  or rsut = *blanks
102910051107     c                   Clear                   Tibs34ds
102920051107     c                   Call      'TIBS34R'
102930051107     c                   Parm                    Tibs34ds
102940051107     c                   In        *dtaara
102950051107     c                   EndIf
102960051107
102970051107     c                   Clear                   wabi
102980051107     c                   Clear                   dLat
102990051107
103000051107      * Verifica errori e autorità profilo
103010051107s   1c                   Select
103020051107      * se ho errori nei dati utente esco dal pgm
103030051117w   1c                   When      duterr = 'E'
103040051115     c                   Eval      *In08 = *On
103050051115     c                   Eval      *In28 = *On
103060051115     c                   Eval      v1cmsg = msg(01)
103070051115      * se richiamato passo il messaggio
103080051115     c                   If        *In06
103090051115     c                   Eval      ta60msg = v1cmsg
103100051115     c                   Eval      ta60err = '1'
103110051115     c                   EndIf
103120051115     c                   Leavesr
103130051107      * se non c'è l'abilitazione
103140051107      * --> se 1° livello, abilitazioni al terminal
103150051107      *     se 2° livello, abilitazioni al punto operativo
103160051107      *     se sede è impossibile ma se capita mando a fine il pgm
103170051117w   1c                   When      uteaut = *Blanks
103180051117if  2c                   If        dutlpo = '1'
103190051107     c                   Eval      wabi   = 'TP'
103200051107e   2c                   EndIf
103210051117if  2c                   If        dutlpo = '2'
103220051107     c                   Eval      wabi   = 'PO'
103230051107e   2c                   EndIf
103240051117if  2c                   If        dutlpo = 'S'
103250051115     c                   Eval      *In08 = *On
103260051115     c                   Eval      *In28 = *On
103270051115     c                   Eval      v1cmsg = msg(01)
103280051115      * se richiamato passo il messaggio
103290051115     c                   If        *In06
103300051115     c                   Eval      ta60msg = v1cmsg
103310051115     c                   Eval      ta60err = '1'
103320051115     c                   EndIf
103330051115     c                   Leavesr
103340051107e   2c                   EndIf
103350051107      * carica le abilitazioni del profilo
103360051107x   1c                   Other
103370051117     c                   Movel     utefaf        dute01
103380051117if  2c                   If        §utecli <> *Blanks
103390051117     c                   Eval      wabi = §utecli
103400051107   x2c                   Else
103410051117     c                   Eval      wabi = uteaut
103420051107e   2c                   EndIf
103430051107e   1c                   EndSl
103440051107
103450051107      * controllo se ok l'abilitazione dell'utente
103460051107     c                   Clear                   Tibs02ds
103470051116     c                   Eval      t02mod = 'C'
103480051116     c                   Eval      t02sif = knsif
103490051116     c                   Eval      t02cod = 'LAT'
103500051116     c                   Movel(p)  wabi          t02ke1
103510051107     c                   Call      'TIBS02R'
103520051107     c                   Parm                    kpjba
103530051107     c                   Parm                    Tibs02ds
103540051117     c                   If        t02err = *Blanks
103550051116     c                   Eval      dLat = t02uni
103560051117     c                   EndIf
103570051107      * errore o non abilitato
103580051117     c                   If        t02err <> *Blanks or §latabi = 'S'
103590051107     c                   Eval      *In08 = *On
103600051107     c                   Eval      *In28 = *On
103610051108     c                   Eval      v1cmsg = msg(01)
103620051115      * se richiamato passo il messaggio
103630051115     c                   If        *In06
103640051115     c                   Eval      ta60msg = v1cmsg
103650051115     c                   Eval      ta60err = '1'
103660051115     c                   EndIf
103670051115     c                   Leavesr
103680051117     c                   EndIf
103690051107
103700051117      * Reperimento dei P.O. gestibili dall'utente per i codici clienti
103710051107     c                   Clear                   Trul31ds
103720051117     c                   Eval      i31abi = wabi
103730051117     c                   Eval      i31cdi = dutdis
103740051117     c                   Eval      i31car = dutare
103750051117     c                   Eval      i31cpo = dutpou
103760051107     c                   Call      'TRUL31R'
103770051117     c                   Parm                    kpjba
103780051107     c                   Parm                    Trul31ds
103790051117     c                   If        o31pog > *zeros
103800051117     c                   Movea     o31pog        skpog
103810051117     c                   Else
103820051107     c                   Eval      *In08 = *On
103830051107     c                   Eval      *In28 = *On
103840051107     c                   Eval      v1cmsg = msg(01)
103850051115      * se richiamato passo il messaggio
103860051115     c                   If        *In06
103870051115     c                   Eval      ta60msg = v1cmsg
103880051115     c                   Eval      ta60err = '1'
103890051115     c                   EndIf
103900051115     c                   Leavesr
103910051117     c                   EndIf
103920160905      /free
103930160905       //?Se utente abilitato alla gestione clienti logistica aggiungo
103940160905       //?alla sk SKPOG le filiali con NTW LOG
103950160905         IF  §UTEKscLog = 'S';
103960160905           exsr CaricaFilLog;
103970160905         ENDIF;
103980160905      /end-free
103990051115
104000051117      * Reperimento dei P.O. gestibili dall'utente per i codici potenziali
104010051117     c                   If        §utepot <> *Blanks
104020051117     c                   Eval      wabi = §utepot
104030051117     c                   EndIf
104040051117     c                   Clear                   Trul31ds
104050051117     c                   Eval      i31abi = wabi
104060051117     c                   Eval      i31cdi = dutdis
104070051117     c                   Eval      i31car = dutare
104080051117     c                   Eval      i31cpo = dutpou
104090051117     c                   Call      'TRUL31R'
104100051117     c                   Parm                    kpjba
104110051117     c                   Parm                    Trul31ds
104120051117     c                   If        O31pog > *zeros
104130051117     c                   Movea     O31pog        skpogcpo
104140051117     c                   EndIf
104150051117
104160051115      * Controllo se sono in sede
104170051115     c                   Eval      *In09 = (simfel = *zeros)
104180051128
104190051128      * Controllo se utente EDP
104200051129     c                   Eval      *In20 = (%subst(knmus:1:3) = 'EDP')
104210051116
104220060110      * Se sono in filiale
104230060110if  1c                   If        Not *In09
104240060110      * Verifico se il P.O. utente può manutenzionare l'anagrafico clienti
104250051116      * deve esistere la tabella MTC per il p.o. utente
104260051116     c                   Clear                   Tibs02ds
104270051116     c                   Clear                   dmtc
104280051116     c                   Eval      t02mod = 'C'
104290051116     c                   Eval      t02sif = knsif
104300051116     c                   Eval      t02cod = 'MTC'
104310051116     c                   Movel(p)  dutpou        t02ke1
104320051116     c                   Call      'TIBS02R'
104330051116     c                   Parm                    kpjba
104340051116     c                   Parm                    Tibs02ds
104350060110if  2c                   If        t02err <> *Blanks
104360051116     c                   Eval      *In28 = *On
104370051116     c                   Eval      v1cmsg = msg(01)
104380051116      * se richiamato passo il messaggio
104390051116     c                   If        *In06
104400051116     c                   Eval      ta60msg = v1cmsg
104410051116     c                   Eval      ta60err = '1'
104420051116     c                   EndIf
104430051116     c                   Leavesr
104440060110   x2c                   Else
104450051125     c                   Eval      dmtc = t02uni
104460060110    2c                   EndIf
104470060110
104480060110      * Se sono in sede deve esserci la tabella MTC key sede
104490060110   x1c                   Else
104500060110     c                   Clear                   Tibs02ds
104510060110     c                   Clear                   dmtc
104520060110     c                   Eval      t02mod = 'C'
104530060110     c                   Eval      t02sif = knsif
104540060110     c                   Eval      t02cod = 'MTC'
104550060110     c                   Movel(p)  'SEDE'        t02ke1
104560060110     c                   Call      'TIBS02R'
104570060110     c                   Parm                    kpjba
104580060110     c                   Parm                    Tibs02ds
104590060110if  2c                   If        t02err <> *Blanks
104600060110     c                   Eval      *In28 = *On
104610060110     c                   Eval      v1cmsg = msg(01)
104620060110      * se richiamato passo il messaggio
104630091106     c                   If        *In06
104640060110     c                   Eval      ta60msg = v1cmsg
104650060110     c                   Eval      ta60err = '1'
104660060110     c                   EndIf
104670060110     c                   Leavesr
104680060110   x2c                   Else
104690060110     c                   Eval      dmtc = t02uni
104700060110    2c                   EndIf
104710060110    1c                   EndIf
104720051115
104730051117      * Apro i file delle anagrafiche provvisorie se richiamato da gest.visite
104740051115     c                   If        *In06
104750051115     c                   Open      Tfaco00f
104760051115     c                   Open      Tfind00f
104770051115     c                   Open      Tfclp00f
104780051117     c                   Open      Tfcls01l
104790080306     c                   Open      Tncpo05l
104800080306     c                   Open      Tncpo06l
104810051115     c                   EndIf
104820051117
104830051117      * Apro i file delle anagrafiche effettive se non richiamato da gest.visite
104840051117     c                   If        Not *In06
104850051117     c                   Open      Cnind00f
104860060105     c                   Open      Cnind02l
104870051117     c                   Open      Cnclp00f
104880051220     c                   Open      Fnacr01l
104890051117     c                   Open      Fncls01l
104900051117     c                   Open      Fnspe01l
104910051219     c                   Open      Fnsp201l
104920060526     c                   EndIf
104930091112
104940091112      * Apro i file dei luoghi se trattativa su cliente
104950091112     c                   If        *In70
104960091112     c                   Open      Fnspe01l
104970091112     c                   Open      Fnsp201l
104980091112     c                   EndIf
104990051115
105000051222      * Passo i dati del capoconto se sono richiamato da visite/offerte
105010091106      * visto che salto la prima videata di richiesta codice
105020091124     c                   If        $pgmrich
105030051115     c                   Eval      v1ckcc = dutkci
105040051115     c                   EndIf
105050051107
105060051107      * Carico £4 capoconti gestiti solo in filiale e non in sede
105070051107     c                   Clear                   £4
105080051107     c                   Eval      kcod = '£4'
105090051108     c                   Eval      kkey = '       1'
105100051108     c     kTabel        Chain     Tabel00f
105110051107     c                   If        %Found(Tabel00f) and
105120051125     c                             tblflg = *Blanks
105130051125     c                   Movea     tbluni        £4
105140051107     c                   EndIf
105150051117
105160051117      * Recupero la divisa da tabella 'GED'
105170051117     c                   Clear                   Tibs02ds
105180051117     c                   Clear                   dged
105190051117     c                   Eval      t02mod = 'C'
105200051117     c                   Eval      t02sif = knsif
105210051117     c                   Eval      t02cod = 'GED'
105220051117     c                   Eval      t02ke1 = '1'
105230051117     c                   Call      'TIBS02R'
105240051117     c                   Parm                    kpjba
105250051117     c                   Parm                    Tibs02ds
105260051117     c                   If        t02err = *Blanks
105270051125     c                   Eval      dged = t02uni
105280051117     c                   EndIf
105290051117
105300051117      * Recupero il diritto di fatturazione di cartello
105310051117     c                   Clear                   Tibs02ds
105320051117     c                   Clear                   dgei
105330051117     c                   Eval      t02mod = 'C'
105340051117     c                   Eval      t02sif = knsif
105350051117     c                   Eval      t02cod = 'GEI'
105360051117     c                   Eval      t02ke1 = §gedcn
105370051117     c                   Call      'TIBS02R'
105380051117     c                   Parm                    kpjba
105390051117     c                   Parm                    Tibs02ds
105400051117     c                   If        t02err = *Blanks
105410051125     c                   Eval      dgei = t02uni
105420051117     c                   EndIf
105430051221
105440051221      * Recupero i dft tipo comunicazioni giacenze
105450051221     c                   Clear                   ds2g
105460051221     c                   Eval      kcod = '2G'
105470051221     c                   Eval      kkey = '1'
105480051221     c     kTabel        Chain     Tabel00f
105490051221     c                   If        %Found(Tabel00f) and
105500051221     c                             tblflg = *Blanks
105510051221     c                   Eval      ds2g = tbluni
105520051221     c                   EndIf
105530051219
105540051219      * Aggancio il capoconto su Cnaco per recuperare acoopz e acotic
105550051219      * uguali per tutti i clienti
105560051219     c     kCnaco1       Chain(n)  Cnaco00f
105570051219     c                   If        %Found(Cnaco00f)
105580051219     c                   Eval      wopz = acoopz
105590051219     c                   Eval      wtic = acotic
105600051219     c                   Else
105610051219     c                   Eval      wopz = '1100100001'
105620051219     c                   Eval      wtic = 'CG'
105630051219     c                   EndIf
105640051219
105650051129      * Imposto la data odierna
105660051129     c                   Z-add     *date         dataoggi
105670051129      * Imposto la data odierna -1 anno
105680051129     c                   Move      dataoggi      dataiso
105690051129     c                   Subdur    1:*y          dataiso
105700051129     c                   Move      dataiso       dataoggi1
105710100805
105720150424       //?cerco il tipo pag. di default da impostare in immissione
105730100805         exec sql
105740100805         declare PAG cursor for
105750100805         select TBEke1, TBEke2 from TNTBE00F
105760100805         where  TBEcod = 'PAG' and substr(TBEuni, 37, 2) = 'SI';
105770100805
105780100805         exec sql
105790100805           open PAG;
105800100805           IF sqlcode < 0;
105810100805             $End = *on;
105820100805           ENDIF;
105830100805
105840100805         DOW not $End;
105850100805           exec sql
105860100805             fetch next from PAG into :TBEke1, :TBEke2;
105870100805             IF sqlcod = 100 or sqlcod < 0;
105880100805               $End = *on;
105890100805               leave;
105900100805             ENDIF;
105910150424             IF  TBEke1 = 'DN';
105920100805               dfttipdn = TBEke2;
105930100805             ENDIF;
105940150424             IF  TBEke1 = 'NA';
105950100805               dfttipna = TBEke2;
105960100805             ENDIF;
105970100805         ENDDO;
105980100805
105990100805         exec sql close PAG;
106000170421
106010170421       //?carico in sk i clienti IMPORT DPD
106020170421         $End = *off;
106030170421         clear ww;
106040170421         exec sql
106050170421         declare TabLPD cursor for
106060170421         select TBEuni from TNTBE00F
106070170421         where TBEcod = 'LPD';
106080170421         exec sql open TabLPD;
106090170421         IF  sqlcode < 0;
106100170421           $End = *on;
106110170421         ENDIF;
106120170421         DOW  not $End;
106130170421           exec sql fetch next from TABLPD into :TBEuni;
106140170421           IF  sqlcod = 100 or sqlcod < 0;
106150170421             $End = *on;
106160170421             leave;
106170170421           ENDIF;
106180170421           dLPD = TBEUni;
106190170421           IF  §LPDksc = 0;
106200170421             iter;
106210170421           ENDIF;
106220170421           ww += 1;
106230170421           CliImport(ww) = §LPDksc;
106240170421         ENDDO;
106250170421         exec sql close TabLPD;
106260100805
106270100805      /end-free
106280051107
106290051107      * KLIST
106300051129     c     kCnabi        Klist
106310051129     c                   Kfld                    kabi
106320051129     c                   Kfld                    kcab
106330051129
106340051108     c     kCnaco        Klist
106350051108     c                   Kfld                    codut
106360051107     c                   Kfld                    v1ckcc
106370051222     c                   Kfld                    kksc
106380051116
106390051116     c     kCnaco1       Klist
106400051116     c                   Kfld                    codut
106410051116     c                   Kfld                    v1ckcc
106420051116     c                   Kfld                    wksc
106430060105
106440060105     c     kCnind        Klist
106450060105     c                   Kfld                    codut
106460060105     c                   Kfld                    v1ckcc
106470060109     c                   Kfld                    kindiva
106480051108
106490051108     c     kFnspe        Klist
106500051216     c                   Kfld                    kspefls
106510051216     c                   Kfld                    kspecli
106520051216     c                   Kfld                    kspecod
106530051219
106540051219     c     kFnsp2        Klist
106550051219     c                   Kfld                    kspecli
106560051219     c                   Kfld                    kspecod
106570051219     c                   Kfld                    ksp2tpe
106580051129
106590051129     c     kTabel        Klist
106600051129     c                   Kfld                    codut
106610051129     c                   Kfld                    kcod
106620051129     c                   Kfld                    kkey
106630051108
106640051129     c     kTfntc        klist
106650051216     c                   kfld                    kntcapl
106660051216     c                   kfld                    kntcnk1
106670051216     c                   kfld                    kntcnk2
106680051216     c                   kfld                    kntctnt
106690051107
106700051107     c                   EndSr
106710160905      /free
106720160905       //--------------------------------------------------------------
106730160905       //?Carico le filiali Logistica nella SKPOG.
106740160905       //--------------------------------------------------------------
106750160905       BEGSR CaricaFilLog;
106760160905
106770160905         $End = *off;
106780160905
106790160905         exec sql
106800160905         declare WLOG cursor for
106810160905         select ORGfil from AZORGLOG;
106820160905
106830160905         exec sql
106840160905         open WLOG;
106850160905         IF  sqlcode < 0;
106860160905           $End = *on;
106870160905         ENDIF;
106880160905
106890160905         DOW not $End;
106900160905           exec sql
106910160905           fetch next from WLOG into :ORGfil;
106920160905           IF  sqlcod = 100 or sqlcod < 0;
106930160905             $End = *on;
106940160905             leave;
106950160905           ENDIF;
106960160905
106970160905           IF  %lookup(%editc(ORGfil:'X'):SKpog) = 0;
106980160905             ww = %lookup(*zeros:SKpog);
106990160905             IF  ww > 0 and xx < 250;
107000160905               SKpog(ww) = %editc(ORGfil:'X');
107010160905             ENDIF;
107020160905           ENDIF;
107030160905
107040160905         ENDDO;
107050160905
107060160905         exec sql close WLOG;
107070160905
107080160905       ENDSR;
107090160905      /end-free
107100051107      *------------------------------------------------------------------------*
107110051212** CAR  Lungh. 10
107120051212!:§@
107130051107** MOD  Lungh. 15                                                            *
107140051107  IMMISSIONE                                                                  01
107150051107 MANUTENZIONE                                                                 02
107160051107VISUALIZZAZIONE                                                               03
107170051107   ANNULLATO                                                                  04
107180051107** MSG  Lungh. 78                                                            *
107190051107Utente non autorizzato alla Gestione anagrafico clienti                       01
107200051107Immettere almeno un dato                                                      02
107210051107Immettere il capoconto                                                        03
107220051107Capoconto non gestibile                                                       04
107230051115Cliente inesistente                                                           05
107240051116Password errata                                                               06
107250051116Immettere la password                                                         07
107260051117Password scaduta                                                              08
107270051117Manca tabella di controllo scadenza password. TEL CED SEDE !!!!!              09
107280051117Cliente non gestibile dall'utente                                             10
107290051117Immettere la ragione sociale                                                  11
107300051118Immettere il codice potenziale                                                12
107310051118Codice potenziale errato                                                      13
107320051118Cliente potenziale non gestibile dall'utente                                  14
107330051118Stato che non prevede il cappario: utilizzabile solo per clienti bloccati     15
107340051118Impossibile utilizzare cap o localita' obsoleti                               16
107350051118Stato errato                                                                  17
107360051118Immettere la partita IVA                                                      18
107370051118Codice fiscale formalmente errato                                             19
107380051122Codice commerciale errato                                                     20
107390090617Fil. del codice commerciale incongruente con la fil. del cliente              21
107400051122Il cliente è da bloccare perchè inattivo                                      22
107410051122Stato del credito errato                                                      23
107420090617Immettere stato del credito gest. da fil. e senza passaggio al contenzioso    24
107430051122Causale blocco cliente errata                                                 25
107440051122Immettere la causale blocco perchè il cliente è bloccato                      26
107450051122Per immettere la causale blocco occorre bloccare il cliente                   27
107460051128Immettere il codice importanza cliente                                        28
107470051128Codice importanza cliente errata                                              29
107480080313Immettere la categoria Merceologica                                           30
107490080313Categoria Merceologica errata                                                 31
107500051212La ragione sociale contiene uno dei seguenti caratteri non validi -- ! : § @  32
107510051128Codice Sottoconto fattura non impostato                                       33
107520051128Codice Sottoconto fattura non utilizzabile                                    34
107530051128Codice Sottoconto fattura errato                                              35
107540051128Codice Sottoconto fattura annullato                                           36
107550051128Tipo fattura errato                                                           37
107560051128Tipo fattura non utilizzabile in                                              38
107570051128Immettere la frequenza fattura                                                39
107580051128Frequenza fattura errato                                                      40
107590051128Per tipo fattura "1" è ammessa solo la frequenza mensile                      41
107600120613Per frequenza settimanale chiedere autorizzazione alla DIR. AMMINISTRATIVA!!  42
107610051128Immettere il tipo data fattura                                                43
107620051128Tipo data fattura errato                                                      44
107630051128Sottoconto fattura diverso da codice cliente è possibile unificare            45
107640051128Verificare il flag Fattura Unificata anche nel sottoconto fattura             46
107650051128Verificare il flag Fattura Unificata anche nei codici collegati               47
107660051128Spese invio fattura errato. Troppi decimali, massimo 2                        48
107670060208Per interrogare le info.commerciali inserire il cod. potenziale               49
107680060203Il luogo non è stato immesso. Impossibile Visualizzare                        50
107690061030Immettere il codice fiscale                                                   51
107700060203La nota non è stata immessa. Impossibile Visualizzare                         52
107710051129Codice pagamento errato                                                       53
107720051129Codice ABI/CAB obbligatorio se pagamento con RB                               54
107730051129Codice ABI/CAB obbligatorio se immessa descrizione banca o nr. C/C            55
107740051130Codice ABI/CAB non valido                                                     56
107750100729Tipo pagamento errato                                                         57
107760100729Tipo pagamento non utilizzabile per clienti vari                              58
107770051130Coordinate bancarie errate. Correggere i dati immessi o contattare il cliente 59
107780051130Per l'estero l'ABI deve essere 99999 e il CAB deve essere 99999               60
107790051206Immettere i codici ABI/CAB                                                    61
107800051212Tipo Comunicazione al Mittente errato                                         62
107810051212Tipo invio comunicazione non utilizzabile                                     63
107820051212Apertura automatica giacenza errato                                           64
107830051212Apertura automatica giacenza non possibile per clienti vari                   65
107840051212Disposizione automatica giacenza non possibile per clienti vari               66
107850051213Immettere un numero stop ritiro minore di 800                                 67
107860051213Il codice richiesto è già in uso da altro utente. Riprovare più tardi         68
107870051216Impossibile confermare. Messaggi in sospeso su altre videate                  69
107880060216Tipo Comunicazione Fine Giacenza errato                                       70
107890051220Impossibile modificare cod.intestazione fattura inserito dalla SEDE           71
107900051220ATTENZIONE!!! Codice già presente in anagrafica clienti ritiro                72
107910170117Impossibile intestare fattura ad un codice in sofferenza.                     73
107920060801Chiusura automatica giacenza non possibile per clienti vari                   74
107930061113Inserire Codice Fiscale solo se diverso dalla Partita IVA. Enter per forzare. 75
107940071128Luogo non utilizzabile                                                        76
107950080306Cod.fiscale già presente su altri potenziali. Enter per forzare               77
107960080306Partita Iva già presente su altri potenziali. Enter per forzare               78
107970080422Non è possibile inserire coordinate bancarie estere se tipo pagamento italia. 79
107980080515Codice pagamento utilizzabile SOLO dalla SEDE                                 80
107990090421Non consentita fattura settimanale se cod.int.fattura con autofattura         81
108000090421No autofattura un codice collegato al cod.int.fattura ha frequenza settimanale82
108010090616Il cliente deve essere incluso nella procedura di sollecito automatico        83
108020090616Il cliente deve essere escluso dalla procedura di sollecito automatico        84
108030100729Codice pagamento NON utilizzabile                                             85
108040100730Per pagamento DANNI con Bonifico immettere la relativa e-mail                 86
108050150427Mancano i codici IBAN per i pagamenti tramite Bonifico. Enter x forzare.      87
108060150805Manca indirizzo mail per inoltro Progetto di Liquidazione. Enter x forzare.   88
108070160517Pagamento con RB non ammesso per cliente estero                               89
108080160517Pagamento con RB non ammesso su conto BancoPosta                              90
108090160914Filiale errata o non in gestione.                                             91
108100060731** TF04
108110060731F4=Abilitazioni  #
108120100524** TF22
108130100524F22=Richiesta Contatto  #
108140100507** TF05
108150100507F5=Attività  #
108160060203** TF12
108170060203F12=Ritorno  #
108180051206** TF14
108190051206F14=InfoCommerciali  #
108200051122** TF15
108210051129F15=Codici Collegati  #
108220060731** TF19
108230060526F19=Variazioni  #
108240100407** TF20
108250100407F20=Potenziali  #
108260101213** TF17
108270101213F17=Dati Fatturazione  #
108280110223** TF21
108290110223F21=Blocco Cliente  #
