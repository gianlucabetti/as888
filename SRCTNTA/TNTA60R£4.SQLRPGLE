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
024320160905     c                   IF        §OGntw = 'XXX' or
024330160905     c                             (§UTEksclog <> 'S' and
024340160905     c                              §OGntw = 'LOG')
024350160905     c     xx            Add       40            zz
024360160905     c                   Eval      *In(zz) = *On
024370160905     c                   eval      *in28 = *on
024380160914     c                   eval      V1Cmsg = msg(91)
024390160905     c                   Goto      emi01
024400160905     c                   ENDIF
024410051108    3c                   EndIf
024420051108    2c                   EndIf
024430051108    1c                   EndDo
024440051108
024450051108     c                   EndSr
024460051216
024470051216      *------------------------------------------------------------------------*
024480051216      * CARICAMENTO DATI DAI FILE ALLE VIDEATE
024490051216      *------------------------------------------------------------------------*
024500051216     c     Sr_Cardati    BegSr
024510051216
024520051220     c                   Clear                   savabl
024530051220     c                   Clear                   savcpo
024540051220     c                   Clear                   savfft
024550051220     c                   Clear                   savitc
024560051220     c                   Clear                   savscf
024570051220     c                   Clear                   savtft
024580051220     c                   Clear                   savuni
024590080422     c                   clear                   sav4utip
024600100802     c                   clear                   savtipdn
024610100802     c                   clear                   savtipna
024620051216     c                   Clear                   wfscf
024630060105     c                   Eval      wforzacon = *Off
024640060215     c                   Clear                   savin22
024650060215     c                   Clear                   savin23
024660100728     c                   Clear                   savin71
024670100728     c                   Clear                   savin72
024680100728     c                   Clear                   savin73
024690100728     c                   Clear                   savin74
024700091119     c                   clear                   savntcdc
024710061030     c                   eval      wforzacdf = *off
024720061106     c                   eval      wforzaiva = *off
024730061106     c                   eval      *in39 = *off
024740080422     c                   eval      $win = *off
024750100729     c                   eval      $windn = *off
024760100729     c                   eval      $winna = *off
024770100729     c                   clear                   wbanca
024780100729     c                   clear                   wagenzia
024790100729     c                   clear                   savabidn
024800100729     c                   clear                   savabina
024810100729     c                   clear                   savcabdn
024820100729     c                   clear                   savcabna
024830100730     c                   clear                   savibandn
024840100730     c                   clear                   savibanna
024850100803     c                   eval      *in75 = *off
024860100803     c                   eval      *in76 = *off
024870100803     c                   eval      *in77 = *off
024880130322     c                   eval      *in79 = *off
024890130322     c                   eval      *in81 = *off
024900150415     c                   eval      *in82 = *off
024910170317     c                   eval      *in83 = *off
024920170320     c                   eval      *in84 = *off
024930170421     c                   eval      *in85 = *off
024940150424     c                   eval      wForzaPag = *off
024950150728     c                   eval      wForzaMail = *off
024960170516       wForzaPotenziale = *off;
024970110217
024980110217      /free
024990110217       //?pulisco campi di comodo per controlli da fare sul commerciale
025000110217       //?quando NON è anagrafica provvisoria
025010110217        clear sav_livello;
025020110217        clear wCodIncAtt;
025030110217      /end-free
025040051216
025050051219      * Se non sono passata dalla prima videata
025060051219      * aggancio l'anagrafica
025070051219if  1c                   If        $d01 = *Off
025080060110      * Spengo indicatori di comodo
025090090914     c                   Setoff                                       0102
025100060110     c                   Setoff                                       04
025110051222     c                   eval      kksc = v1cksc
025120051219      * Se non è richiamato da gestione visite e non è interrogazione
025130051219      * aggancio Cnaco00f per allocare il rcd
025140051219if  2c                   If        Not *In06 and Not *In05
025150060111     c  n05kCnaco        Chain(e)  Cnaco00f
025160051216      * Allocato errore
025170051219if  3c                   If        %error
025180051219      * Torno in prima videata se non è conferma visita
025190051216     c                   If        Not *In07
025200051216     c                   Eval      *In28 = *on
025210051216     c                   Eval      v1cmsg = msg(68)
025220051216     c                   Goto      emi01
025230051216     c                   EndIf
025240051219      * Torno al chiamante se è conferma visita
025250051216     c                   If        *In07
025260051216     c                   Goto      fine
025270051216     c                   EndIf
025280051219    3c                   EndIf
025290051219    2c                   EndIf
025300051219
025310091111      * Se è Interrogazione
025320091111     c                   if        *in05
025330091111      * visita/trattativa
025340091111     c                   if        *in06
025350091111     c                   eval      kksc = v1cnrv
025360091111     c     kCnaco        Chain(n)  Tfaco00f
025370091111      * cliente
025380091111     c                   else
025390051222     c                   eval      kksc = v1cksc
025400091111     c     kCnaco        Chain(n)  Cnaco00f
025410091111     c                   endif
025420091111     c                   endif
025430051216
025440091111      * Richiamato da gestione visite non in interrogazione
025450051219      * aggancio Tfaco00f e alloco il record
025460091111if  2c                   If        *In06 and not *in05
025470051222     c                   eval      kksc = v1cnrv
025480051216     c     kCnaco        Chain(e)  Tfaco00f
025490051216      * Allocato errore
025500051216     c                   If        %error
025510051216     c                   Goto      fine
025520051216     c                   EndIf
025530051219    2c                   EndIf
025540051219
025550051216      * Record trovato
025560051219if  2c                   If        %Found
025570051216      * Cliente esistente - manutenzione/Visualizzazione
025580051219if  3c                   If        acoflg <> '*'
025590051216     c  n05              Eval      *In02 = *On
025600051216     c  n05              Eval      vtcmod = mod(02)
025610051216     c   05              Eval      vtcmod = mod(03)
025620051216      * Cliente annullato
025630051219   x3c                   Else
025640051216     c                   Eval      *In04 = *On
025650051216     c                   Eval      vtcmod = mod(04)
025660051219    3c                   EndIf
025670051219      * Cliente nuovo - immissione
025680051219      * non essendo passata dalla prima videata l'inserimento avviene
025690051219      * solo se sono richiamata da gestione visite/offerte
025700051219      * quindi non controllo se sono in interrogazione
025710051219   x2c                   Else
025720051216     c                   Eval      *In01 = *On
025730051216     c                   Eval      vtcmod = mod(01)
025740051219    2c                   EndIf
025750051219    1c                   EndIf
025760051216
025770051216      * Se non è immissione imposto i dati dei file a video
025780051216if  1c                   If        Not *In01
025790060111     c  n06
025800060111     cann05kCnaco        Chain     Cnind00f
025810060111     c  n06
025820060111     can 05kCnaco        Chain(n)  Cnind00f
025830091111     c   06
025840091111     cann05kCnaco        Chain     Tfind00f
025850091111     c   06
025860091111     can 05kCnaco        Chain(n)  Tfind00f
025870060111     c  n06
025880060111     cann05kCnaco        Chain     Cnclp00f
025890060111     c  n06
025900060111     can 05kCnaco        Chain(n)  Cnclp00f
025910091111     c   06
025920091111     cann05kCnaco        Chain     Tfclp00f
025930091111     c   06
025940091111     can 05kCnaco        Chain(n)  Tfclp00f
025950060111     c  n06
025960060111     cann05v1cksc        Chain     Fncls01l
025970060111     c  n06
025980060111     can 05v1cksc        Chain(n)  Fncls01l
025990091111     c   06
026000091111     cann05v1cnrv        Chain     Tfcls01l
026010091111     c   06
026020091111     can 05v1cnrv        Chain(n)  Tfcls01l
026030051216
026040051216      * Data ultima variazione
026050051216     c                   Z-add     acoduv        dataamg
026060051216     c                   Z-add     aa1           aa2
026070051216     c                   Z-add     mm1           mm2
026080051216     c                   Z-add     gg1           gg2
026090051216     c                   Z-add     datagma       v2cduv
026100051216      * Allineato rcd da sede o da p.o.
026110051216     c                   If        acoftr = 'R'
026120051216     c                   Eval      v2ddtr = 'Ricevuto da Sede'
026130080115     c                   endif
026140080115     c                   if        acoftr = 'T'
026150051216     c                   Eval      v2ddtr = 'Trasmesso a Sede'
026160051216     c                   EndIf
026170051216      * Data allineamento rcd
026180051216     c                   Z-add     acodtr        dataamg
026190051216     c                   Z-add     aa1           aa2
026200051216     c                   Z-add     mm1           mm2
026210051216     c                   Z-add     gg1           gg2
026220051216     c                   Z-add     datagma       v2cdtr
026230051219      * Dati non visualizzati a video
026240051219     c                   Eval      v2copz = acoopz
026250051219     c                   Eval      v2ctic = acotic
026260051219     c                   Eval      v2clin = indlin
026270051216      * Dati anagrafici
026280051216     c                   Eval      v2crag = acorag
026290051216     c                   Eval      v2cvia = indvia
026300051216     c                   Eval      v2ccit = indcit
026310051216     c                   Eval      v2ccae = indcae
026320051216     c                   Eval      v2cprv = indprv
026330051216     c                   Eval      v2csta = indsta
026340051216     c                   Eval      v2ctel = indtel
026350051216     c                   Eval      v2ctlf = indtlf
026360051216     c                   Eval      v2ccdf = indcdf
026370080306     c                   Eval      savcdf = v2ccdf
026380051216     c                   Movel     indiva        w002a
026390051216     c                   If        w002a >= '00'
026400051216     c                   Movel     indiva        v2ivait
026410051216     c                   Else
026420051216     c                   Movel     indiva        v2civa
026430051216     c                   EndIf
026440080306     c                   Eval      saviva = v2civa
026450051216      * Blocco cliente
026460051216     c                   Eval      v2cabl = acoabl
026470130322     c                   eval      V2Cabledp = ACOabl
026480130322      * se blocco del cliente è '*' (blocco contabile) oppure
026490130322      * '7' (blocco automatico)
026500130322      * proteggo il campo del blocco + causale blocco
026510130322      * sempre, filiale e sede
026520130322     c                   IF        ACOabl = '*' or ACOabl = '7'
026530130322     c                   eval      *in79 = *on
026540130322     c                   ENDIF
026550130322     c                   IF        ACOabl = '7'
026560130322     c                   eval      V2Cabl = '8'
026570130322     c                   ENDIF
026580051220     c                   Eval      savabl = acoabl
026590051216      * Causale blocco cliente
026600051216     c                   Eval      v2cblc = clpnar
026610051216      * Stato del credito
026620051216     c                   Move      clpcon        v2ccon
026630110407      * Se il pgm è stato richiamato per il blocco cliente
026640110407      * imposto il blocco e la causale
026650110407     c                   IF        $Blocco
026660110407     c                   eval      v2cabl = '8'
026670110407     c                   eval      v2cblc = Parmcbl
026680110407     c                   ENDIF
026690051216      * Cliente annullabile
026700051216     c                   Eval      v2tb02 = atb02
026710051216      * Codice Potenziale
026720051216     c                   Eval      v2ccpo = acolib
026730051216     c                   Eval      savcpo = acolib
026740090616      * Cliente da sollecitare
026750090616     c                   Eval      v2csol = clpsol
026760090616      * sollecito in inglese
026770090616     c                   Eval      v2fsol = %subst(clsflo:4:1)
026780051216      * Codice commerciale
026790051216     c                   Movel     clpage        v3cage
026800051216      * Codice importanza cliente
026810051216     c                   Eval      v3cclv = clpclv
026820080313      * categoria merceologica
026830051216     c                   Move      acoitc        v3citc
026840051216      * Codice sottoconto fatturazione
026850100127     c                   Eval      v4cscf = %editc(clpscf:'X')
026860100329      * se sono da convalida offerta e non c'è il codice fatturazione
026870100329      * e la partita IVA è $$ imposto come codice fatturazione il codice
026880100329      * cliente che sto generando
026890100329     c                   IF        (v4cscf = *zeros or v4cscf = *blanks) and
026900100329     c                              *in07 and v2ivaeu = '$$'
026910100329     c                   eval      v4cscf = %editc(v1cksc:'X')
026920100329     c                   ENDIF
026930100127     c                   Eval      savscf = v4cscf
026940051216      * Fattura unificata
026950051216     c                   Eval      v4cuni = %subst(clsflo:2:1)
026960051216     c                   Eval      savuni = v4cuni
026970051216      * Tipo fattura
026980051216     c                   Move      clptft        v4ctft
026990051216     c                   Move      clptft        savtft
027000051216      * Frequenza fattura
027010051216     c                   Move      clpfft        v4cfft
027020051216     c                   Move      clpfft        savfft
027030051216      * Tipo data fattura (Inizio mese/Fine mese)
027040051216     c                   Eval      v4ctdf = clpfun
027050051216      * Spese invio fattura (nel file il campo non ha decimali solo a video)
027060051216     c                   Move      indsin        v4csin
027070051216      * Stampa mittente originale in fattura
027080051216     c                   Eval      v4cstm = %subst(clsflo:6:1)
027090051216      * Data prima spedizione fattura (solo visualizzazione viene impostato
027100051216      *                                dalla fatturazione)
027110051216     c                   Z-add     clpdps        dataamg
027120051216     c                   Z-add     aa1           aa2
027130051216     c                   Z-add     mm1           mm2
027140051216     c                   Z-add     gg1           gg2
027150051216     c                   Z-add     datagma       v4cdps
027160051216      * Data ultima spedizione fattura (solo visualizzazione viene impostato
027170051216      *                                 dalla fatturazione)
027180051216     c                   Z-add     clpdus        dataamg
027190051216     c                   Z-add     aa1           aa2
027200051216     c                   Z-add     mm1           mm2
027210051216     c                   Z-add     gg1           gg2
027220051216     c                   Z-add     datagma       v4cdus
027230051216      * Numero protocollo interno (solo visualizzazione viene impostato da Proj)
027240051216     c                   Eval      v4cnpn = indnpn
027250051216      * Numero protocollo cliente (solo visualizzazione viene impostato da Proj)
027260051216     c                   Eval      v4cnpr = indnpr
027270051216      * Data protocollo cliente   (solo visualizzazione viene impostato da Proj)
027280051216     c                   Z-add     inddpr        dataamg
027290051216     c                   Z-add     aa1           aa2
027300051216     c                   Z-add     mm1           mm2
027310051216     c                   Z-add     gg1           gg2
027320051216     c                   Z-add     datagma       v4cdpr
027330051216      * Codice pagamento
027340100727     c                   Move      indcdp        v4ccdp
027350051216      * Abi
027360100727     c                   Eval      v4cabi = indabi
027370051216      * Cab
027380100727     c                   Eval      v4ccab = indcab
027390121105      * Tassazione Post Fatturazopme
027400121105     c                   eval      V4Ctpf = %subst(CLSflo:3:1)
027410090612      * Blocco pagamenti
027420051216     c                   Eval      wbloc = %subst(indopz:1:1)
027430130322     c                   eval      V5Cblpedp = %subst(indopz:1:1)
027440130322      * se blocco pagamenti NON è 'B' (blocco manuale)
027450130322      * proteggo il campo del blocco pagamenti
027460130322      * sempre, filiale e sede
027470130322     c                   IF        V5Cblpedp <> 'B' and
027480130322     c                             V5Cblpedp <> '0' and
027490130322     c                             V5Cblpedp <> *blanks
027500130322     c                   eval      *in81 = *on
027510130322     c                   ENDIF
027520130322     c                   If        wbloc = '0'
027530130322     c                   Eval      v5cblp = 'NO'
027540130322     c                   Else
027550130322     c                   Eval      v5cblp = 'SI'
027560130322     c                   EndIf
027570100728
027580051216      * Codice pagamento contrassegni
027590051216     c                   Eval      v5cfpc = clpfpc
027600080115      * Codice IBAN
027610080115     c                   eval      v5ciban = clpbab
027620100803      * il BIC per i contrassegni per ora non ce l'ho mai quindi non lo faccio
027630100803      * mai vedere
027640100803     c                   eval      *in75 = *on
027650100728
027660100728      /free
027670100728       //?Nuovi tipi pagamento
027680100729       //?Pagamento DANNI --> DN
027690100728         v5tpdn = %subst(clstic:1:1);
027700100728       //?Recupero le coordinate bancarie
027710100728         wpag = 'DN';
027720100728         rqsOpCode = 'SEARCH';
027730100728         exsr Coordinate;
027740100728         IF  rpyEsito = 0;
027750100728           IF not *in06;
027760100730             v5ibandn = TrulIbanO0.IBAN;
027770100730             savabidn = TrulIbanO0.ABI;
027780100730             savcabdn = TrulIbanO0.CAB;
027790100730             v5bicdn  = TrulIbanO0.BIC;
027800100728           ELSE;
027810100730             v5ibandn = TrulIbanO1.IBAN;
027820100730             savabidn = TrulIbanO1.ABI;
027830100730             savcabdn = TrulIbanO1.CAB;
027840100730             v5bicdn  = TrulIbanO1.BIC;
027850100728           ENDIF;
027860100728         ENDIF;
027870100803         IF  v5bicdn = *blanks;
027880100803           *in76 = *on;
027890100803         ENDIF;
027900100729       //?Pagamento NOTE ACCREDITO --> NA
027910100728         v5tpna = %subst(clstic:2:1);
027920100728       //?Recupero le coordinate bancarie
027930100728         wpag = 'NA';
027940100728         exsr Coordinate;
027950100728         IF  rpyEsito = 0;
027960100728           IF not *in06;
027970100730             v5ibanna = TrulIbanO0.IBAN;
027980100730             savabina = TrulIbanO0.ABI;
027990100730             savcabna = TrulIbanO0.CAB;
028000100730             v5bicna  = TrulIbanO0.BIC;
028010100728           ELSE;
028020100730             v5ibanna = TrulIbanO1.IBAN;
028030100730             savabina = TrulIbanO1.ABI;
028040100730             savcabna = TrulIbanO1.CAB;
028050100730             v5bicna  = TrulIbanO1.BIC;
028060100728           ENDIF;
028070100728         ENDIF;
028080100803         IF  v5bicna = *blanks;
028090100803           *in77 = *on;
028100100803         ENDIF;
028110100730         savibandn = v5ibandn;
028120100730         savibanna = v5ibanna;
028130100728      /end-free
028140100728
028150051216      * Tipo comunicazione al mittente
028160051216     c                   Eval      v6ctcm = %subst(clsflo:10:1)
028170051216      * Tipo comunicazione fine giacenza
028180051216     c                   Eval      v6ctcf = %subst(clsflo:1:1)
028190051216      * Apertura automatica giacenza
028200051216     c                   Eval      v6capg = %subst(clsflo:9:1)
028210051216      * Disposizione mittente in arrivo
028220051216     c                   If        acorx1 = *Zeros
028230051216     c                   Eval      v6cdmg = 'SI'
028240051216     c                   Else
028250051216     c                   Eval      v6cdmg = 'NO'
028260051216     c                   EndIf
028270060801      * Chiusura automatica giacenza
028280060801     c                   If        acory1 = *Zeros
028290060801     c                   Eval      v6ccag = 'SI'
028300060801     c                   Else
028310060801     c                   Eval      v6ccag = 'NO'
028320060801     c                   EndIf
028330051216      * Rimborso danni
028340051216      * Stampa copia denuncia al cliente
028350051216     c                   Eval      v7csdn = %subst(clsflo:5:1)
028360051216      * Comunicazioni in inglese
028370051216     c                   Eval      v7csin = %subst(clsflo:7:1)
028380051216      * Escludi da tassazione diretta porti assegnati
028390051216     c                   Eval      v7ctpa = %subst(clsflo:8:1)
028400051216      * Nr. stop autotrasportatori
028410051216     c                   Eval      v7cstp = clsstp
028420051216
028430051216      * Se immissione imposto dei campi di default
028440051216   x1c                   Else
028450051219      * Campi non visualizzati
028460051219     c                   Eval      v2copz = wopz
028470051219     c                   Eval      v2ctic = wtic
028480051216     c                   Eval      v2clin = '1'
028490051219      * Campi visualizzati
028500051216     c                   Clear                   v2ccpo
028510051216      * Se provengo da visite/offerte
028520051216if  2c                   If        *In06
028530051216      * Carico i dati del potenziale
028540051220     c     ta60cpo       Chain(n)  Tncpo01l
028550051216     c                   If        %Found(Tncpo01l)
028560051216     c                   ExSr      Sr_Carcpo
028570051216     c                   EndIf
028580051216    2c                   EndIf
028590090612      * Cliente da sollecitare
028600090616      * imposto il dft
028610090616     c                   ExSr      Sr_Ctrsol
028620051216      * Commerciale
028630051216     c                   Clear                   v3cage
028640051216      * Imposto il codice commerciale della visita
028650051216     c   06              Movel     ta60cmm       v3cage
028660080313      * categoria merceologica
028670051216     c                   Clear                   v3citc
028680080313      * Se impostata dal potenziale riporto la categoria merceologica
028690051216     c                   If        savitc <> *Blanks
028700051216     c                   Eval      v3citc = savitc
028710051216     c                   EndIf
028720051219      * Codice importanza cliente
028730100301      * lo imposto se non è immissione anagrafica provvisoria da trattativa
028740100806     c  n06              Eval      v3cclv = 'C'
028750051216      * Codice sottoconto fatturazione
028760100729      * imposto a 0 così l'utente è obbligato ad inserirlo
028770100303     c                   eval      v4cscf = '0000000'
028780051216      * Tipo fattura
028790051216     c                   Eval      v4ctft = '0'
028800051216     c                   Eval      savtft = '0'
028810051216      * Frequenza fattura
028820051216     c                   Eval      v4cfft = '4'
028830051216     c                   Eval      savfft = '4'
028840091109      * Tipo data fattura (Inizio mese/Fine mese)
028850091125      * se da trattativa
028860100806     c   06              eval      v4ctdf = 'F'
028870051216      * Spese invio fatturazione
028880051216     c                   Z-add     §gedrf        v4csin
028890060210      * se non è anagrafica provvisoria e cliente 8888 o 9999 non devo
028900060210      * imputare le spese di invio fattura
028910060210     c                   Move      v1cksc        w0040
028920060210     c                   If        Not *In06 and
028930060210     c                             (w0040 = 8888 or w0040 = 9999)
028940060210     c                   Clear                   v4csin
028950060210     c                   EndIf
028960051216      * Protocollo
028970051216     c                   Clear                   v4cnpn
028980051216     c                   Clear                   v4cnpr
028990051216     c                   Clear                   v4cdpr
029000051221      * Codice Pagamento
029010100727     c                   Move      *Zeros        v4ccdp
029020130322      * Blocco pagamento campo di work
029030130322     c                   eval      wbloc = '0'
029040080115      * Tipo Pagamento contrassegni
029050091125      * se NON è da trattativa
029060100806     c  n06              eval      v5cfpc = '2'
029070100805
029080100805      /free
029090100805       //?Tipo pagamento Danni
029100150424         v5tpdn = dfttipdn;
029110100805       //?Tipo pagamento Note Accredito
029120150424         v5tpna = dfttipna;
029130100805      /end-free
029140100728
029150051216      * Tipo comunicazione al mittente
029160120118      * lo imposto se non è immissione anagrafica provvisoria da trattativa
029170120118     c  n06              Eval      v6ctcm = §2gtcm
029180051216      * Tipo comunicazione fine giacenza
029190051216     c                   Eval      v6ctcf = §2gtfg
029200051216      * Apertura automatica giacenza
029210051216     c                   Clear                   v6capg
029220051216      * Disposizione mittente in arrivo
029230051216     c                   Eval      v6cdmg = 'SI'
029240060801      * Chiusura automatica giacenza
029250060801     c                   Eval      v6ccag = 'SI'
029260051216
029270051216    1c                   EndIf
029280110217
029290110217      /free
029300110217       //?Se non è anagrafica provvisoria
029310110217         IF  not *in06;
029320110217       //?Aggancio l'organigramma x controllo se filiale cliente
029330110217       //?è un primo o secondo livello
029340110217           clear og148;
029350110217           w0030 = %dec(%subst(%editc(v1cksc:'X'):1:3):3:0);
029360110217           chain  w0030  azorg01l;
029370110217           IF  %found(azorg01l);
029380110217             og148 = ORGde8;
029390110217           ENDIF;
029400110217           sav_Livello = §OGlpo;
029410110217       //?Recupero da tabella Y4O il codice cliente 'incassi da attribuire'
029420110217           clear TIBS02DS;
029430110217           clear dY4O;
029440110217           T02mod = 'C';
029450110217           T02cod = 'Y4O';
029460110217           T02ke1 = '201';
029470110217           T02ke2 = %editc(w0030:'X');
029480110217           TNTBE_RicercaControllo (kpjba : tibs02ds);
029490110217           IF  T02err = *blanks;
029500110217             dY4O = T02uni;
029510110217           ENDIF;
029520110217           wCodIncAtt = %subst(§4Osc1:2:7);
029530110217         ENDIF;
029540170421
029550170421       //?Se sono in manutenzione
029560170421       //?Se non sono utente EDP e il cliente che sto manutenzionando
029570170421       //?un cliente IMPORT DPD proteggo i flag relativi alla
029580170421       //?Gestione Giacenze
029590170421         IF  *in02 and not *in20 and %lookup(V1Cksc:CliImport) > 0;
029600170421           *in85 = *on;
029610170421         ENDIF;
029620170421
029630110217      /end-free
029640051216
029650051216      * Decodifico
029660051216     c                   ExSr      Sr_Decdati
029670060531     c
029680060531     c* Memorizzo l'immagine precedente per  verificare se dati variati
029690150427      /free
029700150427         exsr ImmaginePrima;
029710150427      /end-free
029720051216
029730051216     c                   EndSr
029740100728
029750100728      /free
029760100728       //--------------------------------------------------------------
029770100728       //?Operazioni relative alle coordinate bancarie.
029780100728       //--------------------------------------------------------------
029790100728       BEGSR Coordinate;
029800100802
029810100802         clear rpyEsito;
029820100728
029830100728       //?FNCBA00F
029840100728         IF  not *in06;
029850100730           clear TrulIbanI0;
029860100730           clear TrulIbanO0;
029870100728           TrulIbanI0.KSC = v1cksc;
029880100728           TrulIbanI0.PAG = wpag;
029890100729         //?Se non è ricerca passo i dati del video
029900100729           IF  rqsOpCode <> 'SEARCH';
029910100805             TrulIbanI0.IBAN  = wiban;
029920100805             TrulIbanI0.BIC   = wbic;
029930100805             TrulIbanI0.CODPO = wcodpo;
029940100805             TrulIbanI0.CODPN = wcodpn;
029950100805             TrulIbanI0.PGM   = wpgm;
029960100729           ENDIF;
029970100728           rqsFormatName = 'TRULIBANI0';
029980100728           rpyFormatName = 'TRULIBANO0';
029990100728           rqsDataSize   = %size(TrulIbanI0);
030000100728           rpyDataSize   = %size(TrulIbanO0);
030010100728           TrulIbanR (rqsOpCode:rpyEsito:
030020100729                      rqsFormatName:TrulIbanI0:rqsDataSize:
030030100729                      rpyFormatname:TrulIbanO0:rpyDataSize);
030040100728       //?TFCBA00F
030050100728         ELSE;
030060100730           clear TrulIbanI1;
030070100730           clear TrulIbanO1;
030080100729           TrulIbanI1.NRV = v1cnrv;
030090100728           TrulIbanI1.PAG = wpag;
030100100729         //?Se non è ricerca passo i dati del video
030110100729           IF  rqsOpCode <> 'SEARCH';
030120100730             TrulIbanI1.IBAN = wiban;
030130100730             TrulIbanI1.BIC  = wbic;
030140100729           ENDIF;
030150100728           rqsFormatName = 'TRULIBANI1';
030160100728           rpyFormatName = 'TRULIBANO1';
030170100728           rqsDataSize   = %size(TrulIbanI1);
030180100728           rpyDataSize   = %size(TrulIbanO1);
030190100728           TrulIbanR (rqsOpCode:rpyEsito:
030200100728                      rqsFormatName:TrulIbanI1:rqsDataSize:
030210100729                      rpyFormatname:TrulIbanO1:rpyDataSize);
030220100728         ENDIF;
030230100728
030240100728       ENDSR;
030250100728      /end-free
030260051216
030270051216      *------------------------------------------------------------------------*
030280051216      * DECODIFICO I DATI DELLE VIDEATE
030290051216      *------------------------------------------------------------------------*
030300051216     c     Sr_Decdati    BegSr
030310051216
030320060220     c                   Eval      *In19 = *Off
030330051216     c                   Eval      *In22 = *Off
030340051216     c                   Eval      *In23 = *Off
030350100728     c                   Eval      *In71 = *Off
030360100728     c                   Eval      *In72 = *Off
030370100728     c                   Eval      *In73 = *Off
030380100728     c                   Eval      *In74 = *Off
030390150415     c                   Eval      *In82 = *Off
030400051216
030410051216      * Decodifico lo stato del credito
030420051216     c                   Clear                   v2dcon
030430051216     c                   Clear                   ds4w
030440051216     c                   Eval      kcod = '4W'
030450051216     c                   Move(p)   v2ccon        w003a
030460051216     c                   Eval      kkey = w003a
030470051216     c     kTabel        Chain     Tabel00f
030480051216     c                   If        %Found(Tabel00f) and
030490051216     c                             tblflg = *Blanks
030500051216     c                   Eval      ds4w = tbluni
030510051216     c                   EndIf
030520051216     c                   Eval      v2dcon = §4wdes
030530051216
030540051216      * Se sono in filiale proteggo lo stato del credito se non è modificabile
030550120928      * da p.o.
030560130325      * da 25/03/2013 abbiamo deciso che anche in sede non è modificale, solo
030570130325      * da ProJ
030580130325     c                   If        §4wmbl = 'N'
030590060220     c                   Eval      *In19 = *On
030600051216     c                   EndIf
030610130322
030620120928      * Se sono in filiale proteggo il blocco pagamenti se non è modificabile
030630120928      * da p.o.
030640120928     c                   If        Not *In09 and §4wmbp = 'N'
030650120928     c                   Eval      *In81 = *On
030660120928     c                   EndIf
030670120925      * Se sono in filiale proteggo il blocco cliente se non è modificabile
030680120925      * da p.o.
030690120928     c                   If        Not *In09 and §4wmtb = 'N'
030700120925     c                   Eval      *In79 = *On
030710120925     c                   EndIf
030720170317      /free
030730170330       //?Se sono un utente NON abilitato alla variazione
030740170317       //?e il blocco clienti è protetto
030750170317       //?e il cliente è in contenzioso
030760170330       //?devo bloccare anche la frequenza fattura
030770170330         IF  §UTEclpfft <> 'S' and *in79 and
030780170330             §4Wtip <> *blanks;
030790170317           *in83 = *on;
030800170317         ENDIF;
030810170317      /end-free
030820051216
030830051216      * Decodifico la causale blocco cliente
030840110323     c                   clear                   dsbi
030850051216     c                   Clear                   v2dblc
030860051216     c                   Eval      kcod = 'BI'
030870051216     c                   Eval      kkey = v2cblc
030880051216     c     kTabel        Chain     Tabel00f
030890051216     c                   If        %Found(Tabel00f) and
030900051216     c                             tblflg = *Blanks
030910110323     c                   Eval      dsbi = tbluni
030920051216     c                   EndIf
030930110323     c                   Eval      v2dblc = §bides
030940051216
030950051216      * Decodifico il codice potenziale
030960051216     c                   Clear                   v2dcpo
030970110223     c                   clear                   v2hfls
030980051216     c                   If        v2ccpo <> *Zeros
030990051220     c     v2ccpo        Chain(n)  Tncpo01l
031000051216     c                   If        %Found(Tncpo01l)
031010051216     c                   Eval      v2dcpo = cporag
031020110223     c                   eval      v2hfls = CPOfls
031030121105     c                   eval      dCPO01 = CPOrst
031040051216     c                   EndIf
031050051216     c                   EndIf
031060051216
031070051216      * Decodifico il codice commerciale
031080051216     c                   Clear                   v3dage
031090130806     c                   If        %check(digits:V3Cage) = 0
031100130806     c                   move      V3Cage        CMMcod
031110130806     c     CMMcod        chain     AZCMM000
031120130806     c                   if        %Found(AZCMM01L)
031130130806     c                   movel     CMMdes        v3dage
031140130806     c                   endif
031150130806     c                   EndIf
031160051216
031170051216      * Decodifico il codice importanza cliente
031180051216     c                   Clear                   v3dclv
031190051216     c                   Clear                   dsic
031200051216     c                   Eval      kcod = 'IC'
031210051216     c                   Eval      kkey = v3cclv
031220051216     c     kTabel        Chain     Tabel00f
031230051216     c                   If        %Found(Tabel00f) and
031240051216     c                             tblflg = *Blanks
031250051216     c                   Eval      dsic = tbluni
031260051216     c                   EndIf
031270051216     c                   Eval      v3dclv = §sicde
031280051216
031290080313      * Decodifico categoria merceologica
031300051216     c                   Clear                   v3ditc
031310051216     c                   Clear                   ds1l
031320051216     c                   Eval      kcod = '1L'
031330051216     c                   Movel     v3citc        kkey
031340051216     c     kTabel        Chain     Tabel00f
031350051216     c                   If        %Found(Tabel00f) and
031360051216     c                             tblflg = *Blanks
031370051216     c                   Eval      ds1l = tbluni
031380051216     c                   EndIf
031390051216     c                   Eval      v3ditc = §1ldes
031400051216
031410051216      * Decodifico sottoconto fatturazione
031420051216     c                   Clear                   Tibs69ds
031430051216     c                   Move      v4cscf        I69kac
031440051216     c                   Call      'TIBS69R'
031450051216     c                   Parm                    Tibs69ds
031460051216     c                   Parm                    ds_cnaco
031470051216     c                   Parm                    ds_cnind
031480051216     c                   Parm                    ds_cnclp
031490051216     c                   Parm                    ds_fncls
031500051216     c                   Eval      wtibs69 = *On
031510051216     c                   Movel     w_acorag      v4dscf
031520051216
031530051216      * Decodifico il tipo fattura
031540051216     c                   Clear                   Tibs02ds
031550051216     c                   Eval      t02mod = 'C'
031560051216     c                   Eval      t02sif = knsif
031570051216     c                   Eval      t02cod = 'TFT'
031580051216     c                   Eval      t02ke1 = v4ctft
031590051216     c                   Call      'TIBS02R'
031600051216     c                   Parm                    kpjba
031610051216     c                   Parm                    Tibs02ds
031620051216     c                   If        t02err = *Blanks
031630051216     c                   Eval      dtft = t02uni
031640051216     c                   EndIf
031650051216     c                   Eval      v4dtft = §tftdes
031660051216
031670051216      * Decodifico la frequenza fattura
031680051216     c                   Clear                   v4dfft
031690051216     c                   Clear                   dsff
031700051216     c                   Eval      kcod = 'FF'
031710051216     c                   Eval      kkey = v4cfft
031720051216     c     kTabel        Chain     Tabel00f
031730051216     c                   If        %Found(Tabel00f) and
031740051216     c                             tblflg = *Blanks
031750051216     c                   Eval      dsff = tbluni
031760051216     c                   EndIf
031770051216     c                   Eval      v4dfft = §ffdes
031780051216
031790051216      * Decodifico il tipo data fattura
031800051216     c                   If        v4ctdf <> *Blanks
031810051216     c                   Clear                   v4dtdf
031820051216     c                   Eval      kcod = '13'
031830051216     c                   Eval      kkey = v4ctdf
031840051216     c     kTabel        Chain     Tabel00f
031850051216     c                   If        %Found(Tabel00f) and
031860051216     c                             tblflg = *Blanks
031870051216     c                   Eval      v4dtdf = tbluni
031880051216     c                   EndIf
031890051216     c                   EndIf
031900051216
031910051216      * Decodifico codice di pagamento
031920100727     c                   Clear                   v4dcdp
031930051216     c                   Clear                   dsfa
031940051216     c                   Eval      kcod = 'FA'
031950100727     c                   Movel     v4ccdp        w004a
031960051216     c                   Move      '1'           w004a
031970051216     c                   Eval      kkey = w004a
031980051216     c     kTabel        Chain     Tabel00f
031990051216     c                   If        %Found(Tabel00f) and
032000051216     c                             tblflg = *Blanks
032010051216     c                   Eval      dsfa = tbluni
032020051216     c                   EndIf
032030100727     c                   Eval      v4dcdp = §fades
032040150415
032050150415      /free
032060150415       //?Se utente di filiale e tipo pagamento utilizzabile solo da sede
032070150415       //?devo proteggere la Banca ABI/CAB del pagamento fatture
032080150415         IF  not *in09 and §FAuti = 'S';
032090150415           *in82 = *on;
032100150415         ENDIF;
032110170320       //?Se sono un utente di filiale
032120170320       //?e il blocco clienti è protetto
032130170320       //?e il cliente è in contenzioso
032140170330       //?devo bloccare il codice pagamento
032150170330         IF  not *in09 and *in79 and §4Wtip <> *blanks;
032160170320           *in84 = *on;
032170170320         ENDIF;
032180150415      /end-free
032190080206
032200080206      * decodifico le coordinate bancarie abi-cab
032210100729      * riscossione fattura
032220100729     c                   clear                   v4cbna
032230100727     c                   eval      kabi = v4cabi
032240100727     c                   eval      kcab = v4ccab
032250100729     c                   exsr      DesBanca
032260100729     c                   eval      v4cbna = wdesbanca
032270051216
032280051216      * Decodifico codice pagamanto contrassegni
032290051216     c                   Clear                   v5dfpc
032300051216     c                   Clear                   ds4u
032310051216     c                   Eval      kcod = '4U'
032320051216     c                   Eval      kkey = v5cfpc
032330051216     c     kTabel        Chain     Tabel00f
032340051216     c                   If        %Found(Tabel00f) and
032350051216     c                             tblflg = *Blanks
032360051216     c                   Eval      ds4u = tbluni
032370051216     c                   EndIf
032380051216     c                   Eval      v5dfpc = §4udes
032390100729      * tipo pagamento contrassegni, salvo il flag di tipo pagamento i/e
032400100729     c                   eval      sav4utip = §4utip
032410051216      * Le coordinate bancare le visualizzo o le proteggo in base al
032420051216      * tipo pagamento
032430051216     c                   Select
032440051216      * Coordinate italiane, visualizzo
032450051216      * Immissione coordinate non richiesta, non visualizzo
032460051216     c                   When      §4uabi = 'N'
032470051216     c                   Eval      *In22 = *On
032480060215     c                   Eval      savin22 = *In22
032490051216      * Se pagamento estero proteggo le coordinate bancarie perchè
032500051216      * gestite dalla sede.
032510080115     c                   when      §4utip = 'E'
032520051216     c                   Eval      *In23 = *On
032530060215     c                   Eval      savin23 = *In23
032540051216     c                   EndSl
032550080115      * se le visualizzo ed è un tipo pagamento italia decodifico la banca
032560080115     c                   if        not *in22 and not *in23
032570080206     c                             and v5ciban <> *blanks
032580080213     c                             and (%subst(v5ciban:1:2) = 'IT'
032590080213     c                              or %subst(v5ciban:1:2) = 'SM')
032600080116     c                   clear                   v5cbab
032610080213     c                   monitor
032620080115     c                   eval      kabi = %dec(%subst(v5ciban:6:5):5:0)
032630080115     c                   eval      kcab = %dec(%subst(v5ciban:11:5):5:0)
032640080213     c                   on-error
032650080213     c                   clear                   kabi
032660080213     c                   clear                   kcab
032670080213     c                   endmon
032680100729     c                   exsr      DesBanca
032690100729     c                   eval      v5cbab = wdesbanca
032700080115     c                   endif
032710090401      * se le visualizzo ed è un pagemento estero senza IBAN con
032720090401      * abi e cab no a 99999 decodifico la banca
032730090401     c                   if        not *in22 and *in23
032740090401     c                             and v5ciban = *blanks
032750090401     c                             and clpabi <> *all'9'
032760090401     c                             and clpcab <> *all'9'
032770090401     c                   monitor
032780090401     c                   eval      kabi = clpabi
032790090401     c                   eval      kcab = clpcab
032800090401     c                   on-error
032810090401     c                   clear                   kabi
032820090401     c                   clear                   kcab
032830090401     c                   endmon
032840100729     c                   exsr      DesBanca
032850100729     c                   eval      v5cbab = wdesbanca
032860090401     c                   endif
032870100728
032880100728      /free
032890100728       //?Pagamenti DANNI --> DN
032900100728       //?tipo pagamento
032910100728         clear v5dpdn;
032920100728         clear ds4u;
032930100728         kcod = '4U';
032940100730         kkey = v5tpdn;
032950100728         chain (codut:kcod:kkey) TABEL00F;
032960100728         IF  %found(TABEL00F) and TBLflg = *blanks;
032970100728           ds4u = TBLuni;
032980100728         ENDIF;
032990100730         v5dpdn = §4Udes;
033000100802       //?salvo il tipo pagamento
033010100802         savtipdn = v5tpdn;
033020100728       //?coordinate bancarie
033030100728         SELECT;
033040100728         //?se abi cab non obbligatori non visualizzo
033050100730           WHEN  §4Uabi = 'N';
033060100728             *in71 = *on;
033070100728             savin71 = *in71;
033080100805             *in76 = *off;
033090100728         //?se pagamento estero proteggo perchè solo la sede le gestisce
033100100730           WHEN  §4Utip = 'E';
033110100728             *in72 = *on;
033120100728             savin72 = *in72;
033130100728         ENDSL;
033140100729       //?se IBAN IT o SM (IBAN italia) visualizzo la descrizione banca
033150100729       //?recuperandola con ABI e CAB dell'IBAN
033160100729         IF  not *in71 and not *in72 and v5ibandn <> *blanks and
033170100729            (%subst(v5ibandn:1:2) = 'IT' or %subst(v5ibandn:1:2) = 'SM');
033180100729           clear v5babdn;
033190100729           monitor;
033200100729           kabi = %dec(%subst(v5ibandn:6:5):5:0);
033210100729           kcab = %dec(%subst(v5ibandn:11:5):5:0);
033220100729           on-error;
033230100729           clear kabi;
033240100729           clear kcab;
033250100729           endmon;
033260100729           exsr DesBanca;
033270100729           v5babdn = wDesBanca;
033280100729         ENDIF;
033290100729       //?se IBAN estero (campo iban vuoto e ABI e CAB impostati <> 99999)
033300100729       //?visualizzo la descrizione della banca recuperandola con ABI e CAB
033310100729       //?memorizzati sul file
033320100729         IF  not *in71 and not *in72 and v5ibandn = *blanks and
033330100729             savabidn <> *all'9' and savcabdn <> *all'9';
033340100729           clear v5babdn;
033350100729           monitor;
033360100729           kabi = savabidn;
033370100729           kcab = savcabdn;
033380100729           on-error;
033390100729           clear kabi;
033400100729           clear kcab;
033410100729           endmon;
033420100729           exsr DesBanca;
033430100729           v5babdn = wDesBanca;
033440100729         ENDIF;
033450100729
033460100729       //?Pagamenti NOTE ACCREDITO --> NA
033470100729       //?tipo pagamento
033480100729         clear v5dpna;
033490100729         clear ds4u;
033500100729         kcod = '4U';
033510100730         kkey = v5tpna;
033520100729         chain (codut:kcod:kkey) TABEL00F;
033530100729         IF  %found(TABEL00F) and TBLflg = *blanks;
033540100729           ds4u = TBLuni;
033550100729         ENDIF;
033560100730         v5dpna = §4Udes;
033570100802       //?salvo il tipo pagamento
033580100802         savtipna = v5tpna;
033590100729       //?coordinate bancarie
033600100729         SELECT;
033610100729         //?se abi cab non obbligatori non visualizzo
033620100730           WHEN  §4Uabi = 'N';
033630100729             *in73 = *on;
033640100729             savin73 = *in73;
033650100805             *in77 = *off;
033660100729         //?se pagamento estero proteggo perchè solo la sede le gestisce
033670100730           WHEN  §4Utip = 'E';
033680100729             *in74 = *on;
033690100729             savin74 = *in74;
033700100729         ENDSL;
033710100729       //?se IBAN IT o SM (IBAN italia) visualizzo la descrizione banca
033720100729       //?recuperandola con ABI e CAB dell'IBAN
033730100729         IF  not *in73 and not *in74 and v5ibanna <> *blanks and
033740100729            (%subst(v5ibanna:1:2) = 'IT' or %subst(v5ibanna:1:2) = 'SM');
033750100729           clear v5babna;
033760100729           monitor;
033770100729           kabi = %dec(%subst(v5ibanna:6:5):5:0);
033780100729           kcab = %dec(%subst(v5ibanna:11:5):5:0);
033790100729           on-error;
033800100729           clear kabi;
033810100729           clear kcab;
033820100729           endmon;
033830100729           exsr DesBanca;
033840100729           v5babna = wDesbanca;
033850100729         ENDIF;
033860100729       //?se IBAN estero (campo iban vuoto e ABI e CAB impostati <> 99999)
033870100729       //?visualizzo la descrizione della banca recuperandola con ABI e CAB
033880100729       //?memorizzati sul file
033890100729         IF  not *in73 and not *in74 and v5ibanna = *blanks and
033900100729             savabina <> *all'9' and savcabna <> *all'9';
033910100729           clear v5babna;
033920100729           monitor;
033930100729           kabi = savabina;
033940100729           kcab = savcabna;
033950100729           on-error;
033960100729           clear kabi;
033970100729           clear kcab;
033980100729           endmon;
033990100729           exsr DesBanca;
034000100729           v5babna = wDesbanca;
034010100729         ENDIF;
034020100729
034030100728      /end-free
034040051216
034050051216      * Decodifico tipo comunicazioni al mittente
034060051216     c                   Clear                   v6dtcm
034070051216     c                   Clear                   ds2f
034080051216     c                   Eval      kcod = '2F'
034090051216     c                   Eval      kkey = v6ctcm
034100051216     c     kTabel        Chain     Tabel00f
034110051216     c                   If        %Found(Tabel00f) and
034120051216     c                             tblflg = *Blanks
034130051216     c                   Eval      ds2f = tbluni
034140051216     c                   EndIf
034150051216     c                   Eval      v6dtcm = §2fdes
034160051216
034170051216      * Decodifico tipo comunicazioni fine giacenza
034180051216     c                   Clear                   v6dtcf
034190051216     c                   Clear                   ds2h
034200051216     c                   Eval      kcod = '2H'
034210051216     c                   Eval      kkey = v6ctcf
034220051216     c     kTabel        Chain     Tabel00f
034230051216     c                   If        %Found(Tabel00f) and
034240051216     c                             tblflg = *Blanks
034250051216     c                   Eval      ds2h = tbluni
034260051216     c                   EndIf
034270051216     c                   Eval      v6dtcf = §2hdes
034280051216
034290051216      * Decodifico apertura automatica giacenza
034300051216     c                   Clear                   v6dapg
034310051216     c                   Clear                   ds2u
034320051216     c                   Eval      kcod = '2U'
034330051216     c                   Eval      kkey = v6capg
034340051216     c     kTabel        Chain     Tabel00f
034350051216     c                   If        %Found(Tabel00f) and
034360051216     c                             tblflg = *Blanks
034370051216     c                   Eval      ds2u = tbluni
034380051216     c                   EndIf
034390051216     c                   Eval      v6dapg = §2udes
034400051216
034410051216     c                   EndSr
034420051216
034430051216      *------------------------------------------------------------------------*
034440051216      * CARICO I LUOGHI PREVISTI
034450051216      *------------------------------------------------------------------------*
034460051216     c     Sr_Carluoghi  BegSr
034470051216
034480051216     c                   Eval      *In10 = *Off
034490051216     c                   Eval      *In11 = *Off
034500051216     c                   Eval      *In12 = *Off
034510051216     c                   Eval      *In14 = *Off
034520051216     c                   Eval      *In16 = *Off
034530051216     c                   Eval      *In17 = *Off
034540060208     c                   Eval      *In33 = *Off
034550060208     c                   Eval      *In35 = *Off
034560060208     c                   Eval      *In37 = *Off
034570051216
034580051216      * Luogo invio fattura
034590051216     c                   Eval      kspecli = v1cksc
034600051216     c                   Eval      kspecod = '001'
034610051216     c     kFnspe        Chain     Fnspe01l
034620051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
034630051216     c                   Eval      *In10 = *On
034640051216     c                   EndIf
034650051216
034660051216      * Luogo intestatario pagamento contrassegno
034670051216     c                   Eval      kspecod = '555'
034680051216     c     kFnspe        Chain     Fnspe01l
034690051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
034700051216     c                   Eval      *In11 = *On
034710051216     c                   EndIf
034720051216
034730051216      * Luogo invio contrassegno mittente
034740051216     c                   Eval      kspecod = '666'
034750051216     c     kFnspe        Chain     Fnspe01l
034760051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
034770051216     c                   Eval      *In12 = *On
034780051216     c                   EndIf
034790051216
034800051216      * Luogo spedizione giacenza
034810051219     c                   Clear                   wfax005
034820051219     c                   Clear                   wmail005
034830051216     c                   Eval      kspecod = '005'
034840051216     c     kFnspe        Chain     Fnspe01l
034850051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
034860051216     c                   Eval      *In14 = *On
034870051216     c                   Eval      wfax005 = spefax
034880051219     c     kFnsp2        Chain     Fnsp201l
034890051219     c                   If        %Found(Fnsp201l) and sp2flg = *Blanks
034900051219     c                   Eval      wmail005 = sp2est
034910051219     c                   EndIf
034920051216     c                   EndIf
034930060208
034940060208      * Luogo invio merce resa
034950060208     c                   Eval      kspecod = '300'
034960060208     c     kFnspe        Chain     Fnspe01l
034970060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
034980060208     c                   Eval      *In33 = *On
034990060208     c                   EndIf
035000051216
035010051216      * Luogo preavviso/avviso danno
035020051216     c                   Eval      kspecod = '006'
035030051216     c     kFnspe        Chain     Fnspe01l
035040051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
035050051216     c                   Eval      *In16 = *On
035060051216     c                   EndIf
035070051216
035080051216      * Luogo progetto di liquidazione
035090051216     c                   Eval      kspecod = '008'
035100051216     c     kFnspe        Chain     Fnspe01l
035110051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
035120051216     c                   Eval      *In17 = *On
035130051216     c                   EndIf
035140060208
035150060208      * Luogo invio orm estero
035160060208     c                   Eval      kspecod = '002'
035170060208     c     kFnspe        Chain     Fnspe01l
035180060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
035190060208     c                   Eval      *In35 = *On
035200060208     c                   EndIf
035210060208
035220060208      * Luogo invio doc.a dogana
035230060208     c                   Eval      kspecod = '200'
035240060208     c     kFnspe        Chain     Fnspe01l
035250060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
035260060208     c                   Eval      *In37 = *On
035270060208     c                   EndIf
035280051216
035290051216      * Decodifico i luoghi caricati
035300051216     c                   ExSr      Sr_Decluoghi
035310051216
035320051216     c                   EndSr
035330051216
035340051216      *------------------------------------------------------------------------*
035350051216      * DECODIFICO I LUOGHI CARICATI
035360051216      *------------------------------------------------------------------------*
035370051216     c     Sr_Decluoghi  BegSr
035380051216
035390051216     c                   Clear                   v4dl001
035400051216     c                   Clear                   v4cl001
035410051216     c                   Clear                   v5dl555
035420051216     c                   Clear                   v5cl555
035430051216     c                   Clear                   v5dl666
035440051216     c                   Clear                   v5cl666
035450051216     c                   Clear                   v6dl005
035460051216     c                   Clear                   v6cl005
035470060208     c                   Clear                   v6dl300
035480060208     c                   Clear                   v6cl300
035490051216     c                   Clear                   v7dl006
035500051216     c                   Clear                   v7cl006
035510051216     c                   Clear                   v7dl008
035520051216     c                   Clear                   v7cl008
035530060208     c                   Clear                   v7dl002
035540060208     c                   Clear                   v7cl002
035550060208     c                   Clear                   v7dl200
035560060208     c                   Clear                   v7cl200
035570051216
035580051216      * Decodifico luogo 001
035590071128     c                   clear                   ds4l
035600051216     c                   Eval      kcod = '4L'
035610051216     c                   Eval      kkey = '001'
035620051216     c     kTabel        Chain     Tabel00f
035630051216     c                   If        %Found(Tabel00f) and
035640051216     c                             tblflg = *Blanks
035650051216     c                   Eval      ds4l = tbluni
035660051216     c                   EndIf
035670051216      * Imposto il campo di descrizione
035680060203     c                   Eval      v4dl001 = '001 - ' + §4ldes
035690071128     c                   eval      $ufi001 = §4lufi
035700051216
035710051216      * Decodifico luogo 555
035720071128     c                   clear                   ds4l
035730051216     c                   Eval      kkey = '555'
035740051216     c     kTabel        Chain     Tabel00f
035750051216     c                   If        %Found(Tabel00f) and
035760051216     c                             tblflg = *Blanks
035770051216     c                   Eval      ds4l = tbluni
035780051216     c                   EndIf
035790051216      * Imposto il campo di descrizione
035800060203     c                   Eval      v5dl555 = '555 - ' + §4ldes
035810071128     c                   eval      $ufi555 = §4lufi
035820051216
035830051216      * Decodifico luogo 666
035840071128     c                   clear                   ds4l
035850051216     c                   Eval      kkey = '666'
035860051216     c     kTabel        Chain     Tabel00f
035870051216     c                   If        %Found(Tabel00f) and
035880051216     c                             tblflg = *Blanks
035890051216     c                   Eval      ds4l = tbluni
035900051216     c                   EndIf
035910051216      * Imposto il campo di descrizione
035920060203     c                   Eval      v5dl666 = '666 - ' + §4ldes
035930071128     c                   eval      $ufi666 = §4lufi
035940051216
035950051216      * Decodifico luogo 005
035960071128     c                   clear                   ds4l
035970051216     c                   Eval      kkey = '005'
035980051216     c     kTabel        Chain     Tabel00f
035990051216     c                   If        %Found(Tabel00f) and
036000051216     c                             tblflg = *Blanks
036010051216     c                   Eval      ds4l = tbluni
036020051216     c                   EndIf
036030051216      * Imposto il campo di descrizione
036040060203     c                   Eval      v6dl005 = '005 - ' + §4ldes
036050071128     c                   eval      $ufi005 = §4lufi
036060060208
036070060208      * Decodifico luogo 300
036080071128     c                   clear                   ds4l
036090060208     c                   Eval      kkey = '300'
036100060208     c     kTabel        Chain     Tabel00f
036110060208     c                   If        %Found(Tabel00f) and
036120060208     c                             tblflg = *Blanks
036130060208     c                   Eval      ds4l = tbluni
036140060208     c                   EndIf
036150060208      * Imposto il campo di descrizione
036160060208     c                   Eval      v6dl300 = '300 - ' + §4ldes
036170071128     c                   eval      $ufi300 = §4lufi
036180051216
036190051216      * Decodifico luogo 006
036200071128     c                   clear                   ds4l
036210051216     c                   Eval      kkey = '006'
036220051216     c     kTabel        Chain     Tabel00f
036230051216     c                   If        %Found(Tabel00f) and
036240051216     c                             tblflg = *Blanks
036250051216     c                   Eval      ds4l = tbluni
036260051216     c                   EndIf
036270051216      * Imposto il campo di descrizione
036280060203     c                   Eval      v7dl006 = '006 - ' + §4ldes
036290071128     c                   eval      $ufi006 = §4lufi
036300051216
036310051216      * Decodifico luogo 008
036320071128     c                   clear                   ds4l
036330051216     c                   Eval      kkey = '008'
036340051216     c     kTabel        Chain     Tabel00f
036350051216     c                   If        %Found(Tabel00f) and
036360051216     c                             tblflg = *Blanks
036370051216     c                   Eval      ds4l = tbluni
036380051216     c                   EndIf
036390051216      * Imposto il campo di descrizione
036400060203     c                   Eval      v7dl008 = '008 - ' + §4ldes
036410071128     c                   eval      $ufi008 = §4lufi
036420060208
036430060208      * Decodifico luogo 002
036440071128     c                   clear                   ds4l
036450060208     c                   Eval      kkey = '002'
036460060208     c     kTabel        Chain     Tabel00f
036470060208     c                   If        %Found(Tabel00f) and
036480060208     c                             tblflg = *Blanks
036490060208     c                   Eval      ds4l = tbluni
036500060208     c                   EndIf
036510060208      * Imposto il campo di descrizione
036520060208     c                   Eval      v7dl002 = '002 - ' + §4ldes
036530071128     c                   eval      $ufi002 = §4lufi
036540060208
036550060208      * Decodifico luogo 200
036560071128     c                   clear                   ds4l
036570060208     c                   Eval      kkey = '200'
036580060208     c     kTabel        Chain     Tabel00f
036590060208     c                   If        %Found(Tabel00f) and
036600060208     c                             tblflg = *Blanks
036610060208     c                   Eval      ds4l = tbluni
036620060208     c                   EndIf
036630060208      * Imposto il campo di descrizione
036640060208     c                   Eval      v7dl200 = '200 - ' + §4ldes
036650071128     c                   eval      $ufi200 = §4lufi
036660051216
036670051216     c                   EndSr
036680051216
036690051216      *------------------------------------------------------------------------*
036700051216      * CARICO LE NOTE PREVISTE
036710051216      *------------------------------------------------------------------------*
036720051216     c     Sr_Carnote    BegSr
036730051216
036740051216     c                   Eval      *In24 = *Off
036750051216     c                   Eval      *In26 = *Off
036760051216     c                   Eval      *In31 = *Off
036770051216     c                   Eval      *In21 = *Off
036780051216     c                   Eval      *In13 = *Off
036790051216     c                   Eval      *In15 = *Off
036800051216     c                   Eval      *In18 = *Off
036810060208     c                   Eval      *In34 = *Off
036820060208     c                   Eval      *In36 = *Off
036830090612     c                   Eval      *In40 = *Off
036840100729     c                   Eval      *In68 = *Off
036850051216
036860091112     c   70
036870091112     corn06              Eval      kntcapl = 'C'
036880100806     c   06
036890091112     cann70              Eval      kntcapl = 'T'
036900051216     c                   Movel(p)  dutkci        kntcnk1
036910091112     c   70
036920091112     corn06              Move      v1cksc        kntcnk1
036930100806     c   06
036940100806     cann70              Move      v1cnrv        kntcnk1
036950051216     c                   Clear                   kntcnk2
036960091119
036970091119      * Nota DC
036980091119     c                   clear                   v2ntcdc
036990091119     c                   Movel     'DC'          kntctnt
037000091119     c     kTfntc        Chain(n)  Tfntc01l
037010091119     c                   If        %Found(Tfntc01l)
037020091119     c                   Eval      v2ntcdc = ntcrnt
037030091119     c                   Eval      savntcdc = ntcrnt
037040091119     c                   EndIf
037050091119
037060140722      * Note 02/03
037070140722     c                   exsr      sr_CarNoteRN
037080051216
037090140722      * Note 05/06/T5/I5
037100140722     c                   exsr      sr_CarNoteRT
037110051216
037120140722      * Note 07/08/T7/I7
037130140722     c                   exsr      sr_CarNoteRL
037140051216
037150051216      * Nota 84
037160051216     c                   Movel     '84'          kntctnt
037170091119     c     kTfntc        Chain(n)  Tfntc01l
037180051216     c                   If        %Found(Tfntc01l)
037190051216     c                   Eval      *In21 = *On
037200051216     c                   EndIf
037210051216
037220051216      * Nota 85
037230051216     c                   Movel     '85'          kntctnt
037240091119     c     kTfntc        Chain(n)  Tfntc01l
037250051216     c                   If        %Found(Tfntc01l)
037260051216     c                   Eval      *In13 = *On
037270051216     c                   EndIf
037280051216
037290051216      * Nota 88
037300051216     c                   Movel     '88'          kntctnt
037310091119     c     kTfntc        Chain(n)  Tfntc01l
037320051216     c                   If        %Found(Tfntc01l)
037330051216     c                   Eval      *In15 = *On
037340051216     c                   EndIf
037350051216
037360051216      * Nota 87
037370051216     c                   Movel     '87'          kntctnt
037380091119     c     kTfntc        Chain(n)  Tfntc01l
037390051216     c                   If        %Found(Tfntc01l)
037400051216     c                   Eval      *In18 = *On
037410051216     c                   EndIf
037420060208
037430060208      * Nota 83
037440060208     c                   Movel     '83'          kntctnt
037450091119     c     kTfntc        Chain(n)  Tfntc01l
037460060208     c                   If        %Found(Tfntc01l)
037470060208     c                   Eval      *In34 = *On
037480060208     c                   EndIf
037490060208
037500060208      * Nota 86
037510060208     c                   Movel     '86'          kntctnt
037520091119     c     kTfntc        Chain(n)  Tfntc01l
037530060208     c                   If        %Found(Tfntc01l)
037540060208     c                   Eval      *In36 = *On
037550060208     c                   EndIf
037560090612
037570090612      * Nota 89
037580090612     c                   Movel     '89'          kntctnt
037590091119     c     kTfntc        Chain(n)  Tfntc01l
037600090612     c                   If        %Found(Tfntc01l)
037610090612     c                   Eval      *In40 = *On
037620090612     c                   EndIf
037630100729
037640100729      * Nota 82
037650100729     c                   Movel     '82'          kntctnt
037660100729     c     kTfntc        Chain(n)  Tfntc01l
037670100729     c                   If        %Found(Tfntc01l)
037680100729     c                   Eval      *In68 = *On
037690100729     c                   EndIf
037700051216
037710051216      * Decodifico le note caricate
037720051216     c                   ExSr      Sr_Decnote
037730051216
037740051216     c                   EndSr
037750140722
037760140722      *------------------------------------------------------------------------*
037770140722      * CARICO LE NOTE 02/03 (NEWS NOMINATIVO)
037780140722      *------------------------------------------------------------------------*
037790140722     c     Sr_CarNoteRN  BegSr
037800140722      *
037810140722      * Nota 02
037820140722     c                   movel     '02'          kNtcTnt
037830140722     c     kTfntc        chain(n)  TFNTC
037840140722     c                   if        %found(TFNTC01L)
037850140722     c                   eval      *in24 = *on
037860140722     c                   endif
037870140722      *
037880140722      * Nota 03
037890140722     c                   Movel     '03'          kNtcTnt
037900140722     c     kTfntc        chain(n)  TFNTC
037910140722     c                   if        %found(TFNTC01L)
037920140722     c                   eval      *in24 = *on
037930140722     c                   endif
037940140722      *
037950140722     c                   EndSr
037960140722
037970140722      *------------------------------------------------------------------------*
037980140722      * CARICO LE NOTE 05/06/T5/I5 (RESPONSABILE TRASPORTI)
037990140722      *------------------------------------------------------------------------*
038000140722     c     Sr_CarNoteRT  BegSr
038010140722      *
038020140722      * Nota 05
038030140722     c                   movel     '05'          kNtcTnt
038040140722     c     kTfntc        chain(n)  TFNTC
038050140722     c                   if        %found(TFNTC01L)
038060140722     c                   eval      *in26 = *On
038070140722     c                   endif
038080140722      *
038090140722      * Nota 06
038100140722     c                   movel     '06'          kNtcTnt
038110140722     c     kTfntc        chain(n)  TFNTC
038120140722     c                   if        %found(TFNTC01L)
038130140722     c                   eval      *in26 = *On
038140140722     c                   endif
038150140722      *
038160140722      * Nota T5
038170140722     c                   movel     'T5'          kNtcTnt
038180140722     c     kTfntc        chain(n)  TFNTC
038190140722     c                   if        %found(TFNTC01L)
038200140722     c                   eval      *in26 = *On
038210140722     c                   endif
038220140722      *
038230140722      * Nota I5
038240140722     c                   movel     'I5'          kNtcTnt
038250140722     c     kTfntc        chain(n)  TFNTC
038260140722     c                   if        %found(TFNTC01L)
038270140722     c                   eval      *in26 = *On
038280140722     c                   endif
038290140722      *
038300140722     c                   EndSr
038310140722
038320140722      *------------------------------------------------------------------------*
038330140722      * CARICO LE NOTE 07/08/T7/I7 (RESPONSABILE LOGISTICA)
038340140722      *------------------------------------------------------------------------*
038350140722     c     Sr_CarNoteRL  BegSr
038360140722      *
038370140722      * Nota 07
038380140722     c                   movel     '07'          kNtcTnt
038390140722     c     kTfntc        chain(n)  TFNTC
038400140722     c                   if        %found(TFNTC01L)
038410140722     c                   eval      *in31 = *On
038420140722     c                   endif
038430140722      *
038440140722      * Nota 08
038450140722     c                   movel     '08'          kNtcTnt
038460140722     c     kTfntc        chain(n)  TFNTC
038470140722     c                   if        %found(TFNTC01L)
038480140722     c                   eval      *in31 = *On
038490140722     c                   endif
038500140722      *
038510140722      * Nota T7
038520140722     c                   movel     'T7'          kNtcTnt
038530140722     c     kTfntc        chain(n)  TFNTC
038540140722     c                   if        %found(TFNTC01L)
038550140722     c                   eval      *in31 = *On
038560140722     c                   endif
038570140722      *
038580140722      * Nota I5
038590140722     c                   movel     'I7'          kNtcTnt
038600140722     c     kTfntc        chain(n)  TFNTC
038610140722     c                   if        %found(TFNTC01L)
038620140722     c                   eval      *in31 = *On
038630140722     c                   endif
038640140722      *
038650140722     c                   EndSr
038660051216
038670051216      *------------------------------------------------------------------------*
038680051216      * DECODIFICO LE NOTE CARICATE
038690051216      *------------------------------------------------------------------------*
038700051216     c     Sr_Decnote    BegSr
038710051216
038720051216     c                   Clear                   v4dnt84
038730051216     c                   Clear                   v4cnt84
038740051216     c                   Clear                   v5dnt85
038750051216     c                   Clear                   v5cnt85
038760090612     c                   Clear                   v5dnt89
038770090612     c                   Clear                   v5cnt89
038780100729     c                   Clear                   v5cnt82
038790051216     c                   Clear                   v6dnt88
038800051216     c                   Clear                   v6cnt88
038810051216     c                   Clear                   v7dnt87
038820051216     c                   Clear                   v7cnt87
038830060208     c                   Clear                   v7dnt83
038840060208     c                   Clear                   v7cnt83
038850060208     c                   Clear                   v7dnt86
038860060208     c                   Clear                   v7cnt86
038870140722
038880140722     c                   Eval      kcod = '1T'
038890051216
038900051216      * Decodifico nota 84
038910051216     c                   Eval      kkey = '84'
038920051216     c     kTabel        Chain     Tabel00f
038930051216     c                   If        %Found(Tabel00f) and
038940051216     c                             tblflg = *Blanks
038950051216     c                   Eval      ds1t = tbluni
038960051216     c                   EndIf
038970051216      * Imposto il campo di descrizione
038980060203     c                   Eval      v4dnt84 = ' 84 - ' + §1tdes
038990051216
039000051216      * Decodifico nota 85
039010051216     c                   Eval      kkey = '85'
039020051216     c     kTabel        Chain     Tabel00f
039030051216     c                   If        %Found(Tabel00f) and
039040051216     c                             tblflg = *Blanks
039050051216     c                   Eval      ds1t = tbluni
039060051216     c                   EndIf
039070051216      * Imposto il campo di descrizione
039080060203     c                   Eval      v5dnt85 = ' 85 - ' + §1tdes
039090090612
039100090612      * Decodifico nota 89
039110090612     c                   Eval      kkey = '89'
039120090612     c     kTabel        Chain     Tabel00f
039130090612     c                   If        %Found(Tabel00f) and
039140090612     c                             tblflg = *Blanks
039150090612     c                   Eval      ds1t = tbluni
039160090612     c                   EndIf
039170090612      * Imposto il campo di descrizione
039180090612     c                   Eval      v5dnt89 = ' 89 - ' + §1tdes
039190100729
039200100729      * Decodifico nota 82
039210100729     c                   Eval      kkey = '82'
039220100729     c     kTabel        Chain     Tabel00f
039230100729     c                   If        %Found(Tabel00f) and
039240100729     c                             tblflg = *Blanks
039250100729     c                   Eval      ds1t = tbluni
039260100729     c                   EndIf
039270100729      * Imposto il campo di descrizione
039280100729     c                   Eval      v5dnt82 = ' 82 - ' + §1tdes
039290051216
039300051216      * Decodifico nota 88
039310051216     c                   Eval      kkey = '88'
039320051216     c     kTabel        Chain     Tabel00f
039330051216     c                   If        %Found(Tabel00f) and
039340051216     c                             tblflg = *Blanks
039350051216     c                   Eval      ds1t = tbluni
039360051216     c                   EndIf
039370051216      * Imposto il campo di descrizione
039380060203     c                   Eval      v6dnt88 = ' 88 - ' + §1tdes
039390051216
039400051216      * Decodifico nota 87
039410051216     c                   Eval      kkey = '87'
039420051216     c     kTabel        Chain     Tabel00f
039430051216     c                   If        %Found(Tabel00f) and
039440051216     c                             tblflg = *Blanks
039450051216     c                   Eval      ds1t = tbluni
039460051216     c                   EndIf
039470051216      * Imposto il campo di descrizione
039480060203     c                   Eval      v7dnt87 = ' 87 - ' + §1tdes
039490060208
039500060208      * Decodifico nota 83
039510060208     c                   Eval      kkey = '83'
039520060208     c     kTabel        Chain     Tabel00f
039530060208     c                   If        %Found(Tabel00f) and
039540060208     c                             tblflg = *Blanks
039550060208     c                   Eval      ds1t = tbluni
039560060208     c                   EndIf
039570060208      * Imposto il campo di descrizione
039580060208     c                   Eval      v7dnt83 = ' 83 - ' + §1tdes
039590060208
039600060208      * Decodifico nota 86
039610060208     c                   Eval      kkey = '86'
039620060208     c     kTabel        Chain     Tabel00f
039630060208     c                   If        %Found(Tabel00f) and
039640060208     c                             tblflg = *Blanks
039650060208     c                   Eval      ds1t = tbluni
039660060208     c                   EndIf
039670060208      * Imposto il campo di descrizione
039680060208     c                   Eval      v7dnt86 = ' 86 - ' + §1tdes
039690051216
039700051216     c                   EndSr
039710051216
039720051216      *------------------------------------------------------------------------*
039730051216      *  Imposto descrizione tasti funzione
039740051216      *------------------------------------------------------------------------*
039750051216     c     Sr_Tastifun   BegSr
039760051216
039770051216      * Conta i caratteri riempiti dalle RigaTf1
039780051216     c                   Clear                   $z
039790051216      * Conta i caratteri riempiti dalle RigaTf2
039800051216     c                   Clear                   $k
039810051216      * Conta i caratteri riempiti dalle RigaTf3
039820051216     c                   Clear                   $j
039830051216      * Conta la posizione del campone da cui partire per impostare
039840051216      * la descrizione del tasto funzione
039850051216     c                   Clear                   $y
039860051216
039870051216      * Stringhe che contengono le descrizioni dei tasti funzione
039880051216     c                   Reset                   rigatf1
039890051216     c                   Reset                   rigatf2
039900051216     c                   Reset                   rigatf3
039910060203
039920060731      * F4 - Abilitazioni
039930120117       //?Se no angrafica provvisoria e no immissione
039940111007     c                   eval      *in78 = *off
039950110907     c                   if        not *in01 and not *in06
039960111117     c                   eval      *in78 = *on
039970060731     c                   reset                   $tf
039980060731     c                   movea     tf04          $tf
039990060731     c                   exsr      rie_$tf
040000060731     c                   endif
040010100407
040020100524       //?F22-Richiesta Contatto
040030100806       //?Se anagrafica clienti abilito F22=Richiesta Contatto
040040100407       //?e se utente di filiale
040050100419       //?e se pgm da menù
040060100507       //?e se NON immissione
040070100806     c                   IF        not *in06 and
040080100407     c                             not *in07 and
040090100419     c                             not *in09 and
040100100507     c                             not *in01 and
040110100419     c                             not $pgmrich
040120100407     c                   reset                   $tf
040130100524     c                   Movea     tf22          $tf
040140100407     c                   ExSr      Rie_$tf
040150100407     c                   ENDIF
040160100507
040170100507       //?F5 - Attività
040180120124       //?se utente di sede solo in interrogazione
040190120124       //?se non sono in anagrafica provvisoria,
040200100507       //?se non è convalida offerta e se ci sono attività sul cliente
040210100507       //?e non sono in immissione
040220100507     c                   eval      *in67 = *off
040230100806     c                   IF        not *in06 and
040240100507     c                             not *in07 and
040250120124     c                             not *in01
040260100507      * cerco se ci sono attività sul cliente
040270110516     c     V1Cksc        setll     TIATC07L
040280110516     c                   IF        %equal(TIATC07L)
040290100507     c                   eval      *in67 = *on
040300120124      * se utente di sede e NON è interrogazione spengo F5
040310120124     c                   IF        *in09 and not *in05
040320120124     c                   eval      *in67 = *off
040330120124     c                   ENDIF
040340120124     c                   IF        *in67
040350100507     c                   reset                   $tf
040360100507     c                   movea     tf05          $tf
040370100507     c                   exsr      rie_$tf
040380120124     c                   ENDIF
040390100507     c                   ENDIF
040400100507     c                   ENDIF
040410100507
040420100507      * F15 - Codici collegati
040430100507     c                   If        Not *In01 and Not *In06
040440100507     c                   Reset                   $tf
040450100507     c                   Movea     tf15          $tf
040460100507     c                   ExSr      Rie_$tf
040470100507     c                   EndIf
040480100507
040490060731      * F14 - Info Commerciali
040500100806     c                   If        Not *In06
040510060731     c                   Reset                   $tf
040520060731     c                   Movea     tf14          $tf
040530060731     c                   ExSr      Rie_$tf
040540090914     c                   endif
040550060526      * F19 - Variazioni
040560060526     c                   If        Not *In01 and Not *In06
040570060526     c                   Reset                   $tf
040580060526     c                   Movea     tf19          $tf
040590060526     c                   ExSr      Rie_$tf
040600060526     c                   endif
040610100407      * F20 - Interrogazione clienti potenziali
040620100407     c                   If        *In29 = *On and not *in65 and
040630100806     c                             not *in06
040640100407     c                   Reset                   $tf
040650100407     c                   Movea     tf20          $tf
040660100407     c                   ExSr      Rie_$tf
040670100407     c                   EndIf
040680101213
040690101213       //?F17-Dati Fatturazione
040700110322     c                   IF        not *in06
040710101213     c                   reset                   $tf
040720101213     c                   Movea     tf17          $tf
040730101213     c                   ExSr      Rie_$tf
040740101213     c                   ENDIF
040750110223
040760110223       //?F21-Blocco cliente
040770110223       //?Abilito se anagrafica clienti
040780110223       //?se utente di filiale
040790110223       //?se pgm da menù
040800110223       //?se NON immissione
040810110223       //?se codice non bloccato
040820110223     c                   IF        not *in06 and
040830110223     c                             not *in07 and
040840110223     c                             not *in09 and
040850110223     c                             not *in01 and
040860110223     c                             v2cabl = *blanks and
040870110223     c                             not $pgmrich
040880110223     c                   reset                   $tf
040890110223     c                   Movea     tf21          $tf
040900110223     c                   ExSr      Rie_$tf
040910110223     c                   ENDIF
040920110223
040930060203      * F12 - Ritorno
040940060206     c                   If        (Not *In29 and *In06) or
040950060206     c                             (Not *In29 and *In07) or
040960060206     c                             (Not *In06 and Not *In07)
040970060203     c                   Reset                   $tf
040980060203     c                   Movea     tf12          $tf
040990060203     c                   ExSr      Rie_$tf
041000060206     c                   EndIf
041010051216
041020051216      * Pulisco la stringa
041030051216     c                   Eval      $h = 1
041040051216     c                   For       $h by 1 to 62
041050051216     c                   If        rigatf1($h) = '#'
041060051216     c                   Clear                   rigatf1($h)
041070051216     c                   EndIf
041080051216     c                   EndFor
041090051216     c                   Eval      $h = 1
041100051216     c                   For       $h by 1 to 62
041110051216     c                   If        rigatf2($h) = '#'
041120051216     c                   Clear                   rigatf2($h)
041130051216     c                   EndIf
041140051216     c                   EndFor
041150051216     c                   Eval      $h = 1
041160051216     c                   For       $h by 1 to 62
041170051216     c                   If        rigatf3($h) = '#'
041180051216     c                   Clear                   rigatf3($h)
041190051216     c                   EndIf
041200051216     c                   EndFor
041210051216
041220051216      * Imposto la descrizione del tasto funzione F24 e segnalo
041230051216      * quante righe ho riempito e quale devo emettere
041240051216     c                   Select
041250051216
041260051216     c                   When      $k <> *Zeros and $y <> *Zeros
041270051216     c                   Eval      $loop = 3
041280051216     c                   Eval      vzfd02 = cf2413
041290051216
041300051216     c                   When      $k <> *Zeros and $y = *Zeros
041310051216     c                   Eval      $loop = 2
041320051216     c                   Eval      vzfd02 = cf2412
041330051216
041340051216     c                   Other
041350051216
041360051216     c                   Eval      $loop = 1
041370051216     c                   Clear                   vzfd02
041380051216     c                   EndSl
041390051216
041400051216      * Prima riga di tasti funzione
041410051216     c                   Movea     rigatf1       vzfd01
041420051216     c                   Eval      $rig = 1
041430051216
041440051216     c                   EndSr
041450051216
041460051216      *------------------------------------------------------------------------*
041470051216      *  IMPOSTO LE STRINGHE CON I CAMPI FUNZIONE COMPATTATI
041480051216      *------------------------------------------------------------------------*
041490051216     c     Rie_$tf       BegSr
041500051216      *
041510051216     c                   Eval      $x = 1
041520051216     c     '#'           Lookup    $tf($x)                                30
041530051216     c                   Add       $x            $z
041540051216     c                   If        $Z <= 63
041550051216     c                   Eval      $j = $Z - $x + 1
041560051216     c                   Movea     $tf           rigatf1($j)
041570051216     c                   Else
041580051216     c                   Add       $x            $k
041590051216     c                   If        $k <= 63
041600051216     c                   Eval      $j = $K - $x + 1
041610051216     c                   Movea     $tf           rigatf2($j)
041620051216     c                   Else
041630051216     c                   Add       $x            $y
041640051216     c                   If        $y <= 63
041650051216     c                   Eval      $j = $y - $x + 1
041660051216     c                   Movea     $tf           rigatf3($j)
041670051216     c                   EndIf
041680051216     c                   EndIf
041690051216     c                   EndIf
041700051216
041710051216     c                   EndSr
041720051216
041730051108      *------------------------------------------------------------------------*
041740051130      * CONTROLLO SECONDA VIDEATA - DATI ANAGRAFICI/BLOCCO
041750051108      *------------------------------------------------------------------------*
041760051108     c     Sr_Ctrd02     BegSr
041770051117
041780051117      * Pulisco i campi del posizionamento cursore
041790051117     c                   Clear                   h2riga
041800051117     c                   Clear                   h2colo
041810051125
041820051118      * Imposto gli ulti 4 byte del codice cliente
041830051118     c                   Move      v1cksc        wksc04
041840051117
041850051117      * RAGIONE SOCIALE
041860051117      * --> Obbligatoria
041870051117     c                   If        v2crag = *Blanks
041880051117     c                   Eval      *In28 = *On
041890051130     c                   Eval      h2riga = 07
041900051117     c                   Eval      h2colo = 28
041910051117     c                   Eval      v2cmsg = msg(11)
041920051117     c                   Leavesr
041930051117     c                   EndIf
041940051117
041950051117      * CODICE POTENZIALE
041960051125     c                   ExSr      Sr_Ctrcpo
041970051125     c   28              Leavesr
041980051118
041990051118      * --> Caratteri non validi
042000051212     c                   Eval      xx = 1
042010051212     c                   For       xx by 1 to 10
042020051212     c                   If        car(xx) <> *Blanks
042030051212     c                   Eval      wpos = %scan(car(xx):v2crag)
042040051212     c                   If        wpos > 0
042050051212     c                   Eval      *In28 = *On
042060051212     c                   Eval      h2riga = 07
042070051212     c                   Eval      h2colo = 28
042080051212     c                   Eval      v2cmsg = msg(32)
042090051212     c                   Leavesr
042100051212     c                   EndIf
042110051212     c                   EndIf
042120051212     c                   EndFor
042130051118
042140051117      * INDIRIZZO
042150051118      * --> Controllo se cliente
042160051118     c                   If        v1ckcc = 151
042170051118     c                   Clear                   fnlv13ds
042180051118     c                   Eval      i14rsc = v2crag
042190051118     c                   Eval      i14ind = v2cvia
042200051118     c                   Eval      e14cap = v2ccae
042210051118     c                   Eval      e14loc = v2ccit
042220051118     c                   Eval      e14prv = v2cprv
042230051118     c                   Eval      e14nar = v2csta
042240051118     c                   ExSr      Sr_Ctrind
042250051118     c                   Eval      v2ccae = e14cap
042260051118     c                   Eval      v2ccit = e14loc
042270051118     c                   Eval      v2cprv = e14prv
042280051118     c                   Eval      v2csta = e14nar
042290051118     c   28              Leavesr
042300051118     c                   EndIf
042310051117
042320051117      * TELEFONO
042330051117      * --> Controllo (forzabile)
042340051124      *     Solo se variato il campo a video quindi al primo errore se non
042350051124      *     vario il campo non controllo più
042360051118     c                   If        v2ctel <> wtel and v2ctel <> *Blanks
042370051118     c                   Clear                   trul42ds
042380051118     c                   Eval      d42fax = v2ctel
042390051118     c                   Call      'TRUL42R'
042400051118     c                   Parm                    trul42ds
042410051118     c                   If        d42err = '1'
042420051118     c                   Eval      *In28 = *On
042430051202     c                   Eval      h2riga = 11
042440051118     c                   Eval      h2colo = 28
042450051118     c                   Eval      v2cmsg = d42msg
042460051118     c                   Leavesr
042470051118     c                   Eval      wtel = v2ctel
042480051117     c                   EndIf
042490051118     c                   EndIf
042500051118
042510051117      * FAX
042520051125      * --> Controllo (forzabile)
042530051125      *     Solo se variato il campo a video quindi al primo errore se non
042540051125      *     vario il campo non controllo più
042550051118     c                   If        v2ctlf <> wtlf and v2ctlf <> *Blanks
042560051118     c                   Clear                   trul42ds
042570051118     c                   Eval      d42fax = v2ctlf
042580051118     c                   Call      'TRUL42R'
042590051118     c                   Parm                    trul42ds
042600051118     c                   If        d42err = '1'
042610051118     c                   Eval      *In28 = *On
042620051202     c                   Eval      h2riga = 11
042630051118     c                   Eval      h2colo = 52
042640051118     c                   Eval      v2cmsg = d42msg
042650051118     c                   Leavesr
042660051118     c                   Eval      wtlf = v2ctlf
042670051118     c                   EndIf
042680051118     c                   EndIf
042690051118
042700051118      * CODICE FISCALE
042710051118     c                   ExSr      Sr_Ctrcdf
042720051118     c   28              Leavesr
042730051118
042740051118      * PARTITA IVA + STATO
042750051118     c                   ExSr      Sr_Ctriva
042760051118     c   28              Leavesr
042770051122
042780051122      * BLOCCO CLIENTE
042790051122     c                   ExSr      Sr_Ctrblc
042800051122     c   28              Leavesr
042810090616      * sollecito
042820090616     c                   ExSr      Sr_Ctrsol
042830090617     c   28              Leavesr
042840051108
042850051108     c                   EndSr
042860051130
042870051130      *------------------------------------------------------------------------*
042880051130      * CONTROLLO TERZA VIDEATA - DATI COMMERCIALI
042890051130      *------------------------------------------------------------------------*
042900051130     c     Sr_Ctrd03     BegSr
042910051130
042920051130      * Pulisco i campi del posizionamento cursore
042930051130     c                   Clear                   h3riga
042940051130     c                   Clear                   h3colo
042950051130
042960051130      * CODICE COMMERCIALE
042970051130     c                   ExSr      Sr_Ctrage
042980051130     c   28              Leavesr
042990051130
043000051130      * CODICE IMPORTANZA CLIENTE
043010051130     c                   ExSr      Sr_Ctrclv
043020051130     c   28              Leavesr
043030051130
043040051130      * CODICE ISTAT
043050051130     c                   ExSr      Sr_Ctritc
043060051130     c   28              Leavesr
043070051207
043080140722      * NOTE   02/03 - NEWS NOMINATIVO
043090140722     c                   eval      wRGR = 'RN'
043100140722     c                   exsr      sr_CtrNote
043110140722     c   28
043120140722     cor 90              leavesr
043130051207
043140140722      * NOTE   05/06/T5/I5 - RESPONSABILE TRASPORTI
043150140722     c                   eval      wRGR = 'RT'
043160140722     c                   exsr      sr_CtrNote
043170140722     c   28
043180140722     cor 90              leavesr
043190051207
043200140722      * NOTE   07/08/T7/I7 - RESPONSABILE LOGISTICA
043210140722     c                   eval      wRGR = 'RL'
043220140722     c                   exsr      sr_CtrNote
043230140722     c   28
043240140722     cor 90              leavesr
043250051130
043260051130     c                   EndSr
043270051108
043280051108      *------------------------------------------------------------------------*
043290051130      * CONTROLLO QUARTA VIDEATA - DATI FATTURAZIONE
043300051108      *------------------------------------------------------------------------*
043310051130     c     Sr_Ctrd04     BegSr
043320051125
043330051125      * Pulisco i campi del posizionamento cursore
043340051130     c                   Clear                   h4riga
043350051130     c                   Clear                   h4colo
043360051125
043370051125      * CODICE SOTTOCONTO FATTURAZIONE
043380051125     c                   ExSr      Sr_Ctrscf
043390100127     c   90
043400100127     cor 28              Leavesr
043410051128
043420051128      * FLAG FATTURA UNIFICATA
043430051128     c                   ExSr      Sr_Ctruni
043440051128     c   28              Leavesr
043450051125
043460051125      * TIPO FATTURA
043470051125     c                   ExSr      Sr_Ctrtft
043480051125     c   28              Leavesr
043490051125
043500051125      * FREQUENZA FATTURA
043510051125     c                   ExSr      Sr_Ctrfft
043520051125     c   28              Leavesr
043530051125
043540051125      * TIPO DATA FATTURA
043550051125     c                   ExSr      Sr_Ctrtdf
043560051125     c   28              Leavesr
043570051128
043580051128      * SPESE INVIO FATTURA
043590051128     c                   ExSr      Sr_Ctrsin
043600051128     c   28              Leavesr
043610100728
043620100728      * CODICE PAGAMENTO FATTURA
043630100728     c                   ExSr      Sr_Ctrcdp
043640100728     c   28              Leavesr
043650100728
043660100728      * BANCA APPOGGIO
043670100728     c                   ExSr      Sr_Ctrbna
043680100728     c   28              Leavesr
043690100728
043700100728      * ABI/CAB
043710100728     c                   ExSr      Sr_Ctrabicabf
043720100728     c   28              Leavesr
043730051128
043740051206      * LUOGO 001 - INVIO FATTURA
043750051212      * Se non sono in visite/offerte
043760051128     c                   If        Not *In06
043770091112      * o se trattativa da cliente
043780091112     c                             or *in70
043790051128     c                   ExSr      Sr_Ctrl001
043800051128     c   28              Leavesr
043810051128     c                   EndIf
043820051129
043830051206      * NOTA   84 - INVIO FATTURA
043840051129     c                   ExSr      Sr_Ctrnt84
043850051129     c   28              Leavesr
043860051108
043870051108     c                   EndSr
043880051216
043890051108      *------------------------------------------------------------------------*
043900051130      * CONTROLLO QUINTA VIDEATA - CONDIZIONI PAGAMENTO/PAG. CONTRASSEGNI
043910051108      *------------------------------------------------------------------------*
043920051130     c     Sr_Ctrd05     BegSr
043930051129
043940051129      * Pulisco i campi del posizionamento cursore
043950051130     c                   Clear                   h5riga
043960051130     c                   Clear                   h5colo
043970100728     c                   clear                   wbanca
043980100728     c                   clear                   wagenzia
043990100728
044000100728      * BLOCCO PAGAMENTI
044010130322      * Se impostato SI dall'utente
044020130322     c                   IF        not *in81 and V5Cblp = 'SI'
044030130325     c                   eval      wbloc = 'B'
044040130322     c                   ENDIF
044050130322      * Se impostato NO dall'utente
044060130322     c                   IF        not *in81 and V5Cblp = 'NO'
044070130325     c                   eval      wbloc = '0'
044080130322     c                   ENDIF
044090150424      /free
044100150424       //?Come prima cosa devo avvisare se anche solo 1 dei 3 pagamenti
044110150424       //?non è un pagamento tramite Bonifico
044120150424         exsr CtrPagamenti;
044130150424         IF  *in28;
044140150424           leavesr;
044150150424         ENDIF;
044160150424      /end-free
044170051108
044180051130      * CODICE PAGAMENTO CONTRASSEGNI
044190051130     c                   ExSr      Sr_Ctrfpc
044200060215     c                   If        *In28
044210060215     c                   Eval      *In22 = savin22
044220060215     c                   Eval      *In23 = savin23
044230060215     c                   Leavesr
044240060215     c                   EndIf
044250100729     c                   IF        *in90 and $Win
044260100729     c                   leavesr
044270100729     c                   ENDIF
044280150424
044290150424      /free
044300150427       //?Memorizzo se pagamento tramite Bonifico
044310150424         IF  §4Ucod = 'B';
044320150424           BonificoCA = *on;
044330150424         ELSE;
044340150424           BonificoCA = *off;
044350150424         ENDIF;
044360150427       //?Memorizzo se pagamento Estero
044370150427         IF  §4Utip = 'E';
044380150427           PagEsteroCA = *on;
044390150427         ELSE;
044400150427           PagEsteroCA = *off;
044410150427         ENDIF;
044420150427       //?Memorizzo il codice pagamento
044430150427         CodPagCA = V5Cfpc;
044440150424      /end-free
044450150424
044460080115      * CODICE IBAN
044470080115      * solo se richieste obbligatorie le coordinate
044480080115     c                   if        not *in22 and not *in23
044490100729     c                   eval      wiban = v5ciban
044500080115     c                   exsr      sr_ctriban
044510100729     c                   if        *in28
044520100729     c                   eval      h5riga = 10
044530100804     c                   eval      h5colo = 29
044540100729     c                   leavesr
044550100729     c                   endif
044560100729     c                   eval      v5cbab = wdesbanca
044570080115     c                   endif
044580100729
044590100729      /free
044600100729       //?PAGAMENTO DANNI
044610100729         exsr CtrPagDN;
044620100729         IF  *in28;
044630100729           *in71 = savin71;
044640100729           *in72 = savin72;
044650100729           leavesr;
044660100729         ENDIF;
044670150427       //?Memorizzo se pagamento tramite Bonifico
044680150424         IF  §4Ucod = 'B';
044690150424           BonificoDN = *on;
044700150424         ELSE;
044710150424           BonificoDN = *off;
044720150424         ENDIF;
044730150427       //?Memorizzo se pagamento Estero
044740150427         IF  §4Utip = 'E';
044750150427           PagEsteroDN = *on;
044760150427         ELSE;
044770150427           PagEsteroDN = *off;
044780150427         ENDIF;
044790150427       //?Memorizzo il codice pagamento
044800150427         CodPagDN = V5tpdn;
044810100729       //?se window per invio dati alla sede ritorno ad emettere la videata
044820100729       //?senza andare avanti con i controlli
044830100729         IF  $windn and *in90;
044840100729           leavesr;
044850100729         ENDIF;
044860100729       //?controllo le coordinate bancarie
044870100730         IF  not *in71 and not *in72 and d§PAGctr = 'SI';
044880100729           wiban = v5ibandn;
044890100729           exsr sr_ctriban;
044900100729           IF  *in28;
044910100729             h5riga = 13;
044920100804             h5colo = 29;
044930100729             leavesr;
044940100729           ENDIF;
044950100729           v5babdn = wDesBanca;
044960100729         ENDIF;
044970100730       //?se pagamento tramite bonifico deve essere inserito la nota 82-mail
044980100730       //?bonifici danni
044990100730         IF §4ucod = 'B' and not *in68 and v5cnt82 = *blanks;
045000100730           *in28 = *on;
045010100730           v5cmsg = msg(86);
045020100730           h5riga = 21;
045030100730           h5colo = 71;
045040100730           leavesr;
045050100730         ENDIF;
045060100729
045070100729       //?PAGAMENTO NOTE ACCREDITO
045080100729         exsr CtrPagNA;
045090100729         IF  *in28;
045100100729           *in73 = savin73;
045110100729           *in74 = savin74;
045120100729           leavesr;
045130100729         ENDIF;
045140150427       //?Memorizzo se pagamento tramite Bonifico
045150150424         IF  §4Ucod = 'B';
045160150424           BonificoNA = *on;
045170150424         ELSE;
045180150424           BonificoNA = *off;
045190150424         ENDIF;
045200150427       //?Memorizzo se pagamento Estero
045210150427         IF  §4Utip = 'E';
045220150427           PagEsteroNA = *on;
045230150427         ELSE;
045240150427           PagEsteroNA = *off;
045250150427         ENDIF;
045260150427       //?Memorizzo il codice pagamento
045270150427         CodPagNA = V5tpna;
045280150424      /end-free
045290100729       //?se window per invio dati alla sede ritorno ad emettere la videata
045300100729       //?senza andare avanti con i controlli
045310100729         IF  $winna and *in90;
045320100729           leavesr;
045330100729         ENDIF;
045340100729       //?controllo le coordinate bancarie
045350100804         IF  not *in73 and not *in74 and d§PAGctr = 'SI';
045360100729           wiban = v5ibanna;
045370100729           exsr sr_ctriban;
045380100729           IF  *in28;
045390100729             h5riga = 16;
045400100804             h5colo = 29;
045410100729             leavesr;
045420100729           ENDIF;
045430100729           v5babna = wDesBanca;
045440100729         ENDIF;
045450100729      /end-free
045460051206
045470051212      * Se non sono in visite/offerte
045480051212if  1c                   If        Not *In06
045490091112      * o se trattativa da cliente
045500091112     c                             or *in70
045510051206      * LUOGO 555 - INTESTAZIONE PAGAMENTO C/ASSEGNI
045520051206     c                   ExSr      Sr_Ctrl555
045530051206     c   28              Leavesr
045540051206
045550051206      * LUOGO 666 - LUOGO INVIO BONIFICI C/ASSEGNI
045560051206     c                   ExSr      Sr_Ctrl666
045570051206     c   28              Leavesr
045580051212    1c                   EndIf
045590051206
045600051206      * NOTA   85 - E-MAIL BONIFICI C/ASSEGNI
045610051206     c                   ExSr      Sr_Ctrnt85
045620051206     c   28              Leavesr
045630090616
045640090616      * NOTA   89 - E-MAIL RESPONSABILE AMMINISTRATIVO
045650090616     c                   ExSr      Sr_Ctrnt89
045660090616     c   28              Leavesr
045670100729
045680100729      * NOTA   82 - E-MAIL BONIFICO DANNI
045690150424     c                   ExSr      Sr_Ctrnt82
045700100730     c   90              goto      emi05
045710100729     c   28              Leavesr
045720051130
045730051108     c                   EndSr
045740051108
045750051108      *------------------------------------------------------------------------*
045760051130      * CONTROLLO SESTA VIDEATA - GESTIONE GIACENZE
045770051108      *------------------------------------------------------------------------*
045780051130     c     Sr_Ctrd06     BegSr
045790051206
045800051206      * Pulisco i campi del posizionamento cursore
045810051206     c                   Clear                   h6riga
045820051206     c                   Clear                   h6colo
045830060216
045840060216      * LUOGO 005 - SPEDIZIONE GIACENZA
045850060216     c                   If        Not *In06
045860091112     c                             or *in70
045870060216     c                   ExSr      Sr_Ctrl005
045880060216     c   28              Leavesr
045890060216     c                   EndIf
045900060216
045910060216      * NOTA   88 - E-MAIL GIACENZE
045920060216     c                   ExSr      Sr_Ctrnt88
045930060216     c   28              Leavesr
045940060216
045950060216      * LUOGO 300 - MERCE RESA
045960060216     c                   If        Not *In06
045970091112     c                             or *in70
045980060216     c                   ExSr      Sr_Ctrl300
045990060216     c   28              Leavesr
046000060216     c                   EndIf
046010051206
046020051206      * TIPO COMUNICAZIONE AL MITTENTE
046030051206     c                   ExSr      Sr_Ctrtcm
046040051206     c   28              Leavesr
046050051206
046060051206      * TIPO COMUNICAZIONE FINE GIACENZA
046070051206     c                   ExSr      Sr_Ctrtcf
046080051206     c   28              Leavesr
046090051206
046100051206      * APERTURA AUTOMATICA GIACENZA
046110051206     c                   ExSr      Sr_Ctrapg
046120051206     c   28              Leavesr
046130051206
046140051206      * DISPOSIZIONE MITTENTE IN ARRIVO
046150051206     c                   ExSr      Sr_Ctrdmg
046160051206     c   28              Leavesr
046170060801
046180060801      * CHIUSURA AUTOMATICA GIACENZA
046190060801     c                   ExSr      Sr_Ctrcag
046200060801     c   28              Leavesr
046210051108
046220051108     c                   EndSr
046230051108
046240051108      *------------------------------------------------------------------------*
046250051130      * CONTROLLO SETTIMA VIDEATA - GESTIONE DANNI/TASSAZIONE P.A./STOP
046260051108      *------------------------------------------------------------------------*
046270051130     c     Sr_Ctrd07     BegSr
046280051213
046290051213      * Pulisco i campi del posizionamento cursore
046300051213     c                   Clear                   h7riga
046310051213     c                   Clear                   h7colo
046320150728
046330150728      /free
046340150728       //?Come prima cosa devo avvisare se manca la mail per invio
046350150728       //?progetto di liquidazione
046360150728       //?NON lo faccio se anagrafica provvisoria
046370150728         IF  not *in06;
046380150728           exsr CtrMailDanni;
046390150728           IF  *in28;
046400150728             leavesr;
046410150728           ENDIF;
046420150728         ENDIF;
046430150728      /end-free
046440051213
046450051213      * LUOGO 006 - LUOGO PREAVVISO/AVVISO DANNO
046460051213     c                   If        Not *In06
046470091112     c                             or *in70
046480051213     c                   ExSr      Sr_Ctrl006
046490051213     c   28              Leavesr
046500051213     c                   EndIf
046510051213
046520051213      * NOTA   87 - E-MAIL DANNI
046530051213     c                   ExSr      Sr_Ctrnt87
046540051213     c   28              Leavesr
046550051213
046560051213      * LUOGO 008 - PROGETTO DI LIQUIDAZIONE
046570051213     c                   If        Not *In06
046580091112     c                             or *in70
046590051213     c                   ExSr      Sr_Ctrl008
046600051213     c   28              Leavesr
046610051213     c                   EndIf
046620051213
046630051213      * NR. STOP RITIRO
046640051213     c                   ExSr      Sr_Ctrstp
046650051213     c   28              Leavesr
046660060208
046670060208      * NOTA   83 - MAIL ORM ESTERO
046680060208     c                   ExSr      Sr_Ctrnt83
046690060208     c   28              Leavesr
046700060208
046710060208      * LUOGO 002 - INVIO ORM ESTERO
046720060208     c                   If        Not *In06
046730091112     c                             or *in70
046740060208     c                   ExSr      Sr_Ctrl002
046750060208     c   28              Leavesr
046760060208     c                   EndIf
046770060208
046780060208      * NOTA   86 - MANIFEST
046790060208     c                   ExSr      Sr_Ctrnt86
046800060208     c   28              Leavesr
046810060208
046820060208      * LUOGO 200 - DOC. DOGANA
046830060208     c                   If        Not *In06
046840091112     c                             or *in70
046850060208     c                   ExSr      Sr_Ctrl200
046860060208     c   28              Leavesr
046870060208     c                   EndIf
046880051108
046890051108     c                   EndSr
046900051216
046910051216      *------------------------------------------------------------------------*
046920051216      * TASTO FUNZIONE F02 - CONTATTI
046930051216      *------------------------------------------------------------------------*
046940051216     c     Sr_F02        BegSr
046950051216
046960051216     c                   Clear                   tnta12ds
046970051216     c                   Eval      ta12ric = wrich
046980091112     c   70
046990091112     corn06              Eval      ta12apl = 'C'
047000091112     c   70
047010091112     corn06              Eval      ta12ksc = v1cksc
047020100806     c   06
047030091112     cann70              Eval      ta12apl = 'T'
047040100806     c   06
047050100806     cann70              Eval      ta12nrv = v1cnrv
047060060217     c                   Eval      ta12rag = wrag
047070051216     c                   Eval      ta12tnt = wtnt
047080060216     c                   Eval      ta12gcli = '1'
047090060216     c   01              Eval      ta12icli = '1'
047100060307     c   01              Move      v3cage        ta12cmm
047110060227     c                   If        %Parms > 1 and
047120060227     c                             Not *In01 and Not *In05 and ta60icli = '1'
047130060221     c                   Eval      ta12icli = '1'
047140060307     c                   Move      v3cage        ta12cmm
047150060221     c                   EndIf
047160051216     c                   Call      'TNTA12R'
047170051216     c                   Parm                    kpjba
047180051216     c                   Parm                    tnta12ds
047190051216
047200051216     c                   ExSr      Sr_Carnote
047210051216
047220051216     c                   EndSr
047230060217
047240060217      *------------------------------------------------------------------------*
047250060217      * TASTO FUNZIONE F03 - FINE LAVORO
047260060217      *------------------------------------------------------------------------*
047270060217     c     Sr_F03        BegSr
047280060217
047290060217     c                   Eval      *In80 = *Off
047300060217
047310060217      * Se immissione emetto videata per chiedere conferma di uscita dal
047320060217      * pgm informando l'utente che i dati andranno persi
047330060217     c                   If        *In01
047340060217     c                   Eval      w03cok = 'NO'
047350060217     c                   Exfmt     ta60w03
047360060217      * Se ok per uscire dal pgm vado a fine
047370060217if  1c                   If        w03cok = 'SI'
047380060217     c                   Goto      fine
047390060217     c                   Else
047400060217     c                   Eval      *In80 = *On
047410060217     c                   Leavesr
047420060217    1c                   EndIf
047430060217     c                   EndIf
047440110302      * Imposto F3 fine se pgm richiamato
047450110302     c                   IF        $Pgmrich
047460110301     c                   eval      TA60f03 = '1'
047470110302     c                   ENDIF
047480110301
047490060217      * Se arrivo qua non è immissione e quindi vado a fine
047500060217     c                   Goto      fine
047510060217
047520060217     c                   EndSr
047530060731
047540060731      *------------------------------------------------------------------------*
047550060731      * TASTO FUNZIONE F04 - ABILITAZIONI
047560060731      *------------------------------------------------------------------------*
047570060731     c     sr_f04        begsr
047580060731
047590060731     c                   clear                   tnta61ds
047600060731     c                   eval      ta61tla = 'V'
047610060731     c                   eval      ta61ksc = v1cksc
047620060731     c                   call      'TNTA61R'
047630060731     c                   parm                    kpjba
047640060731     c                   parm                    tnta61ds
047650060731
047660060731     c                   endsr
047670100507
047680100507      /free
047690100507       //--------------------------------------------------------------
047700100507       //?Tasto Funzione F5 - Attività.
047710100507       //--------------------------------------------------------------
047720100507       BEGSR sr_F05;
047730100507
047740100507         clear trmk21ds;
047750100507         I21mod = 'I';
047760100507         I21ksc = V1Cksc;
047770120124         I21rsc = V2Crag;
047780100507         kpjbu = trmk21ds;
047790100507
047800100507         trmk21r (kpjba);
047810100507
047820100507       ENDSR;
047830100507      /end-free
047840060731
047850051216      *------------------------------------------------------------------------*
047860051216      * TASTO FUNZIONE F07 - CLIENTE UNIFICANTE
047870051216      *------------------------------------------------------------------------*
047880051216     c     Sr_F07        BegSr
047890051216
047900051216     c                   Clear                   tibs12ds
047910051216     c  n05              Eval      k12op0 = 'P02'
047920051216     c   05              Eval      k12op0 = 'P05'
047930051216     c                   Eval      k12op1 = 'O09'
047940051216     c                   Eval      k12f03 = '0'
047950051216     c                   Eval      k12f12 = '0'
047960051216     c                   Eval      k12err = '0'
047970051216     c                   Move      v1cksc        k12cop
047980051216     c                   Eval      kpjbu = tibs12ds
047990051216     c                   Call      'TIBS12R'
048000051216     c                   Parm                    kpjba
048010051216
048020051216     c                   EndSr
048030100407
048040100407      /free
048050100407       //--------------------------------------------------------------
048060100524       //?Tasto Funzione F22 - Richiesta Contatto.
048070100407       //--------------------------------------------------------------
048080100524       BEGSR sr_F22;
048090110223
048100110223       //?Non posso fare una nuova attività su cliente con potenziale in
048110110223       //?categoria PERSO, devo partire dal potenziale
048120110223         IF  V2Hfls = 'P';
048130110223         *in28 = *on;
048140110223         V2Cmsg = 'Nuova attività non possibile per Potenziale PERSO';
048150110223         leavesr;
048160110223         ENDIF;
048170100407
048180100407         clear trmk17ds;
048190100407         IMK17patt = 'S';
048200100407         IMK17att  = 'T';
048210100407         IMK17pcau = 'S';
048220100407         IMK17cau  = '20';
048230100407         IMK17cpo  = v2ccpo;
048240100407         IMK17ksc  = v1cksc;
048250100407         IF  v3cage > *zeros;
048260100407           IMK17cmm  = %dec(v3cage:7:0);
048270100407         ENDIF;
048280100407
048290100407         trmk17r (kpjba : TRMK17DS);
048300100507
048310100507       //?Ricarico i tasti funzione, nel caso venga inserita l'attività sul
048320100507       //?cliente così faccio vedere il tasto F5-Attività
048330100507         exsr Sr_Tastifun;
048340100407
048350100407       ENDSR;
048360101213
048370101213       //--------------------------------------------------------------
048380101213       //?Tasto Funzione F17 - Dati Fatturazione.
048390101213       //--------------------------------------------------------------
048400101213       BEGSR sr_F17;
048410101213
048420101213         clear trul57ds;
048430101213         D57ikscb = V1Cksc;
048440101213         IF  %check(digits:V4Cscf) > 0;
048450101213           D57ikscf = 0;
048460101213         ELSE;
048470101213           D57ikscf = %dec(V4Cscf:7:0);
048480101213         ENDIF;
048490101213         D57iunib = V4Cuni;
048500101213         D57itftb = V4Ctft;
048510101213         D57ifftb = V4Cfft;
048520101213         D57idftb = V4Ctdf;
048530101213         D57ipgm  = 'TNTA60R';
048540101213         D57isifb = V4Csin;
048550101213         D57icdpb = V4Ccdp;
048560101213         D57iabib = V4Cabi;
048570101213         D57icabb = V4Ccab;
048580101213
048590101213         trul57r (kpjba : TRUL57DS);
048600101213
048610101213       ENDSR;
048620110223
048630110223       //--------------------------------------------------------------
048640110223       //?Tasto Funzione F21 - Blocco Cliente.
048650110223       //--------------------------------------------------------------
048660110223       BEGSR sr_F21;
048670110223
048680110223         clear trmk17ds;
048690110223         IMK17patt = 'S';
048700110223         IMK17att  = 'T';
048710110223         IMK17pcau = 'S';
048720110223         IMK17cau  = '71';
048730110223         IMK17cpo  = v2ccpo;
048740110223         IMK17ksc  = v1cksc;
048750110223         IF  v3cage > *zeros;
048760110223           IMK17cmm  = %dec(v3cage:7:0);
048770110223         ENDIF;
048780110223
048790110223         trmk17r (kpjba : TRMK17DS);
048800110223
048810110223       //?Ricarico i tasti funzione
048820110223         exsr Sr_Tastifun;
048830110310
048840110223
048850110223       ENDSR;
048860110223
048870100407      /end-free
048880051216
048890051216      *------------------------------------------------------------------------*
048900051216      * TASTO FUNZIONE F11 - LUOGHI
048910051216      *------------------------------------------------------------------------*
048920051216     c     Sr_F11        BegSr
048930051216
048940051216     c                   Clear                   parmf11
048950051216     c  n05              Eval      paropz = '2'
048960051216     c   05              Eval      paropz = '5'
048970051216     c                   Move      v1cksc        parcli
048980060216     c                   Eval      partnta60 = '1'
048990060216     c   01              Eval      parimta60 = '1'
049000051216     c                   Eval      kpjbu = parmf11
049010051216     c                   Call      'TNTA80R'
049020051216     c                   Parm                    kpjba
049030051216
049040051216     c                   ExSr      Sr_Carluoghi
049050051216
049060051216     c                   EndSr
049070051216
049080051216      *------------------------------------------------------------------------*
049090051216      * TASTO FUNZIONE F14 - INFO COMMERCIALI
049100051216      *------------------------------------------------------------------------*
049110051216     c     Sr_F14        BegSr
049120051216
049130081020     c                   Clear                   trmk50ds
049140081020     c                   Eval      i50pgm = 'TNTA60R'
049150081020     c                   Movel     v2ccpo        i50cpo
049160081020     c                   Eval      i50rag = wrag
049170110310     c                   IF        *in02 and not *in06 and not *in07
049180110310     c                   Eval      i50mod = 'G'
049190110310     c                   ELSE
049200110310     c                   Eval      i50mod = 'I'
049210110310     c                   ENDIF
049220081020     c                   Call      'TRMK50R'
049230051216     c                   Parm                    kpjba
049240081020     c                   Parm                    trmk50ds
049250051216
049260081020     c                   Eval      wtrmk50 = *On
049270051216     c                   EndSr
049280051216
049290051216      *------------------------------------------------------------------------*
049300051216      * TASTO FUNZIONE F15 -
049310051216      *------------------------------------------------------------------------*
049320051216     c     Sr_F15        BegSr
049330051216
049340051216     c                   Clear                   parmf15
049350051216     c                   Move      w0070         parcli1
049360051216     c                   Eval      kpjbu = parmf15
049370051216     c                   Call      'TNTA83R'
049380051216     c                   Parm                    kpjba
049390051216
049400051216     c                   EndSr
049410051216
049420051216      *------------------------------------------------------------------------*
049430051216      * TASTO FUNZIONE F18 -
049440051216      *------------------------------------------------------------------------*
049450051216     c     Sr_F18        BegSr
049460100406
049470100406      /free
049480100806       //?Richiamo gestione note
049490100406          clear trmk20ds;
049500110511          IMK20tla  = 'L';
049510100406
049520100406         //?Se interrogazione richiamo in interrogazione
049530100406          IF  *in05;
049540100406            IMK20flm = 'I';
049550100406          ENDIF;
049560100406
049570101112          IMK20cpo = v2ccpo;
049580100806          IMK20ksc = v1cksc;
049590100406          IMK20rsc = v1crag;
049600100406
049610100406          trmk20r (kpjba : TRMK20DS);
049620100406
049630100406      /end-free
049640051216
049650051216     c                   EndSr
049660060526      *------------------------------------------------------------------------*
049670060526      * TASTO FUNZIONE F19 - Variazioni
049680060526      *------------------------------------------------------------------------*
049690060526     c     Sr_F19        BegSr
049700060526
049710060526     c                   Clear                   tnta73ds
049720060526     c                   Eval      i73kcc=acokcc
049730060526     c                   Eval      i73ksc=acoksc
049740060529     c                   Movel     v1crag        i73rag
049750060526     c                   movel     tnta73ds      kpjbu
049760060526     c                   Call      'TNTA73R'
049770060526     c                   Parm                    kpjba
049780060526     c                   EndSr
049790100407
049800100407      *------------------------------------------------------------------------*
049810100407      * TASTO FUNZIONE F20 - INTERROGAZIONE CLIENTI POTENZIALI
049820100407      *------------------------------------------------------------------------*
049830100407     c     Sr_F20        BegSr
049840100407
049850100407      /free
049860100806       //?Richiamo gestione potenziali
049870100407          clear trmk01ds;
049880100407
049890100407         //?Se non c'è il codice potenziale vado in ricerca
049900100407          IF  v2ccpo = 0;
049910100407            MK01k11  = '1';
049920100407            MK01rag  = w005a;
049930100407            kpjbu    = TRMK01DS;
049940100407            trmk01r (kpjba);
049950100407            TRMK01DS = kpjbu;
049960100407          ELSE;
049970100407         //?Se c'è il codice potenziale vado in gestione
049980100407            MK01k10 = 'N';
049990100407            MK01cpo = v2ccpo;
050000100407            trmk02r (kpjba : TRMK01DS);
050010100407          ENDIF;
050020100407
050030100407          IF MK01cpo > 0 and MK01cpo <> v2ccpo;
050040100407            v2ccpo = MK01cpo;
050050100407            v2dcpo = MK01rag;
050060100407            exsr Sr_Ctrcpo;
050070100407          ENDIF;
050080100407
050090100407      /end-free
050100100407
050110100407     c                   EndSr
050120051216
050130051216      *------------------------------------------------------------------------*
050140051216      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
050150051216      *------------------------------------------------------------------------*
050160051216     c     Sr_F24        begsr
050170051216
050180051216     c                   Select
050190051216
050200051216     c                   When      $loop = 2 and $rig =2  or
050210051216     c                             $loop = 3 and $rig =3
050220051216     c                   Movea     rigatf1       vzfd01
050230051216     c                   Z-add     1             $rig
050240051216
050250051216     c                   If        $loop = 2
050260051216     c                   Eval      vzfd02= cf2412
050270051216     c                   Else
050280051216     c                   Eval      vzfd02= cf2413
050290051216     c                   EndIf
050300051216
050310051216     c                   When      $loop = 2 and $rig =1 or
050320051216     c                             $loop = 3 and $rig =1
050330051216     c                   Movea     rigatf2       vzfd01
050340051216     c                   Z-add     2             $rig
050350051216
050360051216     c                   If        $loop = 2
050370051216     c                   Eval      vzfd02 = cf2422
050380051216     c                   Else
050390051216     c                   Eval      vzfd02 = cf2423
050400051216     c                   EndIf
050410051216
050420051216     c                   When      $loop = 3 and $rig =2
050430051216     c                   Movea     rigatf3       vzfd01
050440051216     c                   Z-add     3             $rig
050450051216     c                   Eval      vzfd02 = cf2433
050460051216     c                   EndSl
050470051216
050480051216     c                   EndSr
050490051216
050500051216      *------------------------------------------------------------------------*
050510051216      * CONFERMO I DATI
050520051216      *------------------------------------------------------------------------*
050530051216     c     Sr_Conferma   BegSr
050540051220
050550051220     c                   Eval      wokimm = *Off
050560051216
050570051216      * In base alla videata emessa faccio i controlli che servono per
050580051216      * poter confermare i dati del video
050590051216
050600051216     c                   Select
050610051216
050620051216      * F6 dato in seconda videata
050630051216     c                   When      wvideo = '02'
050640051216     c                   ExSr      Sr_Ctrd02
050650051216     c   28              Leavesr
050660051216     c                   ExSr      Sr_Ctrd03
050670051216     c  n28              ExSr      Sr_Ctrd04
050680051216     c  n28              ExSr      Sr_Ctrd05
050690051216     c  n28              ExSr      Sr_Ctrd06
050700051216     c  n28              ExSr      Sr_Ctrd07
050710051216     c   28              Eval      v2cmsg = msg(69)
050720150424     c   28              eval      wForzaPag = *off
050730150728     c   28              eval      wForzaMail = *off
050740051216     c   28              Leavesr
050750051216
050760051216      * F6 dato in terza videata
050770051216     c                   When      wvideo = '03'
050780051216     c                   ExSr      Sr_Ctrd03
050790051216     c   28              Leavesr
050800051216     c                   ExSr      Sr_Ctrd04
050810051216     c  n28              ExSr      Sr_Ctrd05
050820051216     c  n28              ExSr      Sr_Ctrd06
050830051216     c  n28              ExSr      Sr_Ctrd07
050840051216     c   28              Eval      v3cmsg = msg(69)
050850150424     c   28              eval      wForzaPag = *off
050860150728     c   28              eval      wForzaMail = *off
050870051216     c   28              Leavesr
050880051216
050890051216      * F6 dato in quarta videata
050900051216     c                   When      wvideo = '04'
050910051216     c                   ExSr      Sr_Ctrd04
050920051216     c   28              Leavesr
050930051216     c                   ExSr      Sr_Ctrd05
050940051216     c  n28              ExSr      Sr_Ctrd06
050950051216     c  n28              ExSr      Sr_Ctrd07
050960051216     c   28              Eval      v4cmsg = msg(69)
050970150424     c   28              eval      wForzaPag = *off
050980150728     c   28              eval      wForzaMail = *off
050990051216     c   28              Leavesr
051000051216
051010051216      * F6 dato in quinta videata
051020051216     c                   When      wvideo = '05'
051030051216     c                   ExSr      Sr_Ctrd05
051040051216     c   28              Leavesr
051050051216     c                   ExSr      Sr_Ctrd06
051060051216     c  n28              ExSr      Sr_Ctrd07
051070051216     c   28              Eval      v5cmsg = msg(69)
051080150728     c   28              eval      wForzaMail = *off
051090051216     c   28              Leavesr
051100051216
051110051216      * F6 dato in sesta videata
051120051216     c                   When      wvideo = '06'
051130051216     c                   ExSr      Sr_Ctrd06
051140051216     c   28              Leavesr
051150051216     c                   ExSr      Sr_Ctrd07
051160051216     c   28              Eval      v6cmsg = msg(69)
051170150728     c   28              eval      wForzaMail = *off
051180051216     c   28              Leavesr
051190051216
051200051216      * F6 dato in settima videata
051210051216     c                   When      wvideo = '07'
051220051216     c                   ExSr      Sr_Ctrd07
051230051216     c   28              Leavesr
051240051216
051250051216     c                   EndSl
051260051216
051270051216      * Se arrivo a questo punto vuol dire che tutte le videate sono a posto
051280051220
051290051220      * Recupero il flag di procedura ORM attiva
051300060731     c                   Movel     v1cksc        w0030
051310051220     c                   Clear                   og148
051320061012     c                   Clear                   og143
051330051220     c     w0030         Chain     Azorg01l
051340051220     c                   If        %Found(Azorg01l)
051350051220     c                   Eval      og148 = orgde8
051360061012     c                   Eval      og143 = orgde3
051370051220     c                   EndIf
051380160905
051390160905      * se cliente logistica no anagrafica provvisoria
051400160905     c                   IF        ClienteLog
051410160905     c                   goto      NoImmACR
051420160905     c                   ENDIF
051430051220
051440051220      * Se non sono in anagrafica provvisoria, la procedura ORM è attiva e
051450051220      * provengo da convalida offerte
051460120313if  1c                   If        Not *In06 and *In07 and §ogorm <> *blanks
051470051220      * Controllo se devo creare l'anagrafica clienti ritiro
051480051220     c                   Eval      wokimm = *On
051490060731     c                   Movel     v1cksc        w0100
051500051220     c     w0100         Setll     Fnacr01l
051510051220do  2c                   Do        *Hival
051520051220     c                   Read      Fnacr01l
051530051220      * Fine file esco
051540051220     c                   If        %Eof(Fnacr01l)
051550051220     c                   Leave
051560051220     c                   EndIf
051570051220     c                   Movel     acrcro        w0070
051580051220      * Codice diverso esco
051590060731     c                   If        w0070 > v1cksc
051600051220     c                   Leave
051610051220     c                   EndIf
051620051220      * Se trovo non devo inserire l'anagrafico clienti ritiro
051630060731     c                   If        w0070 = v1cksc
051640051220     c                   Eval      wokimm = *Off
051650051220     c                   Leave
051660051220     c                   EndIf
051670051220    2c                   EndDo
051680051220
051690051220      * Se il cliente è già sull'anagrafico clienti ritiro emetto la videata
051700051220      * con un msg info
051710051221     c                   Eval      *In29 = *Off
051720051220if  2c                   If        wokimm = *Off and wforza = *Off
051730051220     c                   Eval      *In28 = *On
051740051220     c                   Eval      wforza = *On
051750051220     c                   Select
051760051220     c                   When      wvideo = '02'
051770051220     c                   Eval      v2cmsg = msg(72)
051780051221     c                   Eval      *In29 = *On
051790051220     c                   Goto      emi02
051800051220     c                   When      wvideo = '03'
051810051220     c                   Eval      v3cmsg = msg(72)
051820051220     c                   Goto      emi03
051830051220     c                   When      wvideo = '04'
051840051220     c                   Eval      v4cmsg = msg(72)
051850051220     c                   Goto      emi04
051860051220     c                   When      wvideo = '05'
051870051220     c                   Eval      v5cmsg = msg(72)
051880051220     c                   Goto      emi05
051890051220     c                   When      wvideo = '06'
051900051220     c                   Eval      v6cmsg = msg(72)
051910051220     c                   Goto      emi06
051920051220     c                   When      wvideo = '07'
051930051220     c                   Eval      v7cmsg = msg(72)
051940051220     c                   Goto      emi07
051950051220     c                   EndSl
051960051220    2c                   EndIf
051970051220    1c                   EndIf
051980160905
051990160905     c     NoImmACR      tag
052000051220
052010150424      * aggiorno la nota "DC"
052020091119     c                   if        v2ntcdc <> savntcdc
052030091119     c                   ExSr      Sr_AggntcDC
052040091119     c                   endif
052050051220
052060051220      * Quindi posso passare i dati dal video ai vari file e aggiornare
052070051216
052080051216      * Immissione pulisco i rcd file e imposto la chiave
052090051219if  1c                   If        *In01
052100051216     c  n06              Clear                   Cnaco000
052110051216     c   06              Clear                   Tfaco000
052120051216     c  n06              Clear                   Cnind000
052130051216     c   06              Clear                   Tfind000
052140051216     c  n06              Clear                   Cnclp000
052150051216     c   06              Clear                   Tfclp000
052160051216     c  n06              Clear                   Fncls000
052170051216     c   06              Clear                   Tfcls000
052180051220      * Chiave ACO
052190051216     c                   Eval      acokut = codut
052200051216     c                   Eval      acokcc = dutkci
052210051222     c  n06              Eval      acoksc = v1cksc
052220051222     c   06              Eval      acoksc = v1cnrv
052230051220      * Chiave IND
052240051216     c                   Eval      indkut = acokut
052250051216     c                   Eval      indkcc = acokcc
052260051216     c                   Eval      indksc = acoksc
052270051220      * Chiave CLP
052280051216     c                   Eval      clpkut = acokut
052290051216     c                   Eval      clpkcc = acokcc
052300051216     c                   Eval      clpksc = acoksc
052310051220      * Chiave CLS
052320051216     c                   Eval      clsksc = acoksc
052330051219      * Campi non visualizzati solo di immissione (non vengono aggiornati)
052340051219     c                   Eval      acoopz = v2copz
052350051219     c                   Eval      acotic = v2ctic
052360051219     c                   Eval      indlin = v2clin
052370051219      * Codice filiale di trasmissione sempre uguale ai primi 3 byte del codice
052380051219      * cliente
052390060330     c                   Movel     acoksc        acoflt
052400051219      * Flag tipo trasmissione sempre a '3' xchè se sono in immissione vuol dire
052410051219      * che sto immettendo un cliente o un'anagrafica provvisoria
052420051219     c                   Eval      acoftt = '3'
052430051219      * Data prima spedizione fattura (solo immissione xchè la imposto io
052440051219      * nel caso di cliente non nuovo
052450051219     c                   Z-add     v4cdps        datagma
052460051219     c                   Eval      gg1 = gg2
052470051219     c                   Eval      mm1 = mm2
052480051219     c                   Eval      aa1 = aa2
052490051219     c                   Z-add     dataamg       clpdps
052500051219      * Cliente da sollecitare
052510090616     c                   Movel     v2csol        clpsol
052520051219    1c                   EndIf
052530051216
052540051219      * Campi che vengono aggiornati a video
052550051219      * ACO
052560051219     c                   Eval      atb02 = v2tb02
052570051219     c                   Eval      acorag = v2crag
052580051219     c                   If        v6cdmg = 'SI'
052590051219     c                   Clear                   acorx1
052600051219     c                   Else
052610051219     c                   Eval      acorx1 = 1
052620051219     c                   EndIf
052630060801     c                   If        v6ccag = 'SI'
052640060801     c                   Clear                   acory1
052650060801     c                   Else
052660060801     c                   Eval      acory1 = 1
052670060801     c                   EndIf
052680051219     c                   Z-add     *date         acoduv
052690130325     c   79              eval      ACOabl = V2Cabledp
052700130325     c  n79              eval      ACOabl = V2Cabl
052710051219     c                   Eval      acolib = v2ccpo
052720051219     c                   Move      v3citc        acoitc
052730051216     c                   Clear                   acodtr
052740051219     c                   Clear                   acoftr
052750051219      * IND
052760051216     c                   Eval      indvia = v2cvia
052770051219     c                   Eval      indcit = v2ccit
052780051216      * Se Cap numerico e non supera i 5 caratteri scrivo anche indcap
052790051216if  1c                   If        %subst(v2ccae:6:1) = *Blanks
052800051216     c                   Eval      wokcap = *On
052810051216     c                   Eval      xx = 1
052820051216     c                   For       xx by 1 to 5
052830051216     c                   If        %subst(v2ccae:(xx):1) < *zeros
052840051216     c                   Eval      wokcap = *Off
052850051216     c                   Leave
052860051216     c                   EndIf
052870051216     c                   EndFor
052880051216     c                   If        wokcap = *On
052890060208     c                   Movel     v2ccae        indcap
052900051216     c                   EndIf
052910051216    1c                   EndIf
052920051216     c                   Eval      indprv = v2cprv
052930051216     c                   Eval      indsta = v2csta
052940051216     c                   Eval      indtel = v2ctel
052950051219     c                   Eval      indcdf = v2ccdf
052960051219     c                   If        v2ivaeu = *Blanks
052970051219     c                   Movel     v2ivait       indiva
052980051219     c                   Else
052990051219     c                   Movel     v2civa        indiva
053000051219     c                   EndIf
053010051219     c                   Eval      indcdp = *all'0'
053020100727     c                   Move      v4ccdp        indcdp
053030051219     c                   Eval      %subst(indopz:1:1) = wbloc
053040100727     c                   Eval      indabi = v4cabi
053050100727     c                   Eval      indcab = v4ccab
053060051216     c                   Eval      indtlf = v2ctlf
053070051219     c                   Move      v4csin        indsin
053080051219     c                   Eval      indcae = v2ccae
053090051219      * CLP
053100100127     c                   Eval      clpscf = %dec(v4cscf:7:0)
053110051219     c                   Move      v4cfft        clpfft
053120051219     c                   Move      v4ctft        clptft
053130051219     c                   Clear                   clpfun
053140051219     c                   Eval      %subst(clpfun:1:1) = v4ctdf
053150051219     c                   Move      v3cage        clpage
053160051219     c                   Clear                   clpcon
053170051219     c                   Eval      %subst(clpcon:2:2) = v2ccon
053180051219     c                   Eval      clpnar = v2cblc
053190051219     c                   Eval      clpclv = v3cclv
053200080422     c                   eval      clpbab = v5ciban
053210080213      * se IBAN non impostato a video
053220080213      * pulisco i dati relativi a abi/cab e c/c
053230080213     c                   if        v5ciban = *blanks
053240090331      * ma solo se ABI NON è > 90000
053250090331     c                   if        clpabi < 90000
053260080213     c                   clear                   clpccb
053270080213     c                   clear                   clpabi
053280080213     c                   clear                   clpcab
053290090331     c                   endif
053300080213     c                   else
053310080213      * se IBAN italia memorizzo abi/cab e c/c
053320080213     c                   if        %subst(v5ciban:1:2) = 'IT' or
053330080213     c                             %subst(v5ciban:1:2) = 'SM'
053340080115     c                   eval      %subst(clpccb:1:12) =
053350080115     c                             %subst(v5ciban:16:12)
053360080115     c                   eval      %subst(clpccb:16:1) =
053370080115     c                             %subst(v5ciban:5:1)
053380080115     c                   eval      clpabi = %dec(%subst(v5ciban:6:5):5:0)
053390080115     c                   eval      clpcab = %dec(%subst(v5ciban:11:5):5:0)
053400080213     c                   endif
053410080213     c                   endif
053420080422     c                   Eval      clpfpc = v5cfpc
053430090617      * Cliente da sollecitare
053440090617     c                   Movel     v2csol        clpsol
053450051219      * CLS
053460051219     c                   Eval      clsstp = v7cstp
053470100729     c                   eval      %subst(CLStic:1:1) = v5tpdn
053480100729     c                   eval      %subst(CLStic:2:1) = v5tpna
053490051219     c                   Eval      %subst(clsflo:1:1) = v6ctcf
053500051219     c                   Eval      %subst(clsflo:2:1) = v4cuni
053510121105     c                   eval      %subst(CLSflo:3:1) = V4Ctpf
053520090616     c                   Eval      %subst(clsflo:4:1) = v2fsol
053530051219     c                   Eval      %subst(clsflo:5:1) = v7csdn
053540051219     c                   Eval      %subst(clsflo:6:1) = v4cstm
053550051219     c                   Eval      %subst(clsflo:7:1) = v7csin
053560051219     c                   Eval      %subst(clsflo:8:1) = v7ctpa
053570051219     c                   Eval      %subst(clsflo:9:1) = v6capg
053580051219     c                   Eval      %subst(clsflo:10:1) = v6ctcm
053590051219
053600051219      * Immissione
053610051219     c                   If        *In01
053620051219     c  n06              Write     Fncls000
053630051219     c   06              Write     Tfcls000
053640051219     c  n06              Write     Cnclp000
053650051219     c   06              Write     Tfclp000
053660051219     c  n06              Write     Cnind000
053670051219     c   06              Write     Tfind000
053680051219     c  n06              Write     Cnaco000
053690051219     c   06              Write     Tfaco000
053700060526     c* Scrivo file variazioni
053710060526     c  n06              exsr      ScriviACV
053720060526     c                   EndIf
053730060526     c
053740051219      * Manutenzione
053750051219     c                   If        *In02
053760051219     c  n06              Update    Fncls000
053770051219     c   06              Update    Tfcls000
053780051219     c  n06              Update    Cnclp000
053790051219     c   06              Update    Tfclp000
053800051219     c  n06              Update    Cnind000
053810051219     c   06              Update    Tfind000
053820051219     c  n06              Update    Cnaco000
053830051219     c   06              Update    Tfaco000
053840060526     c*
053850060526     c* Scrivo file variazioni
053860060526     c  n06              exsr      ScriviACV
053870051219     c                   EndIf
053880110927
053890110927      * Aggiorno il potenziale
053900160906      * solo se NON sono utente di sede
053910110927     c                   If        v2ccpo > *Zeros
053920160906     c                             and not *in09
053930110927     c                   ExSr      Sr_Aggcpo
053940110927     c                   EndIf
053950100729
053960100729      /free
053970100729       //?Memorizzo le coordinate bancarie
053980100730       //?Pagamento DANNI --> DN
053990100730         clear rqsOpCode;
054000100729         wpag = 'DN';
054010100729         wiban = v5ibandn;
054020100730         wbic  = v5bicdn;
054030100805         wcodpo = savtipdn;
054040100805         wcodpn = v5tpdn;
054050100805         wpgm  = 'TNTA60R';
054060100730         SELECT;
054070100730           WHEN  v5ibandn <> *blanks and savibandn = *blanks;
054080100805             rqsOpCode = 'INSERT';
054090100805           WHEN  v5ibandn = *blanks and savibandn <> *blanks;
054100100805             rqsOpCode = 'DELETE';
054110100805           OTHER;
054120100805             rqsOpCode = 'UPDATE';
054130100730         ENDSL;
054140100730         IF  rqsOpCode <> *blanks;
054150100730           exsr Coordinate;
054160100730         ENDIF;
054170100729       //?Non faccio controlli sul codice di ritorno
054180100729       //?ormai l'anagrafica è scritta
054190100729
054200100729       //?Pagamento NOTE ACCREDITO --> NA
054210100730         clear rqsOpCode;
054220100729         wpag = 'NA';
054230100729         wiban = v5ibanna;
054240100730         wbic  = v5bicna;
054250100805         wcodpo = savtipna;
054260100805         wcodpn = v5tpna;
054270100805         wpgm  = 'TNTA60R';
054280100730         SELECT;
054290100730           WHEN  v5ibanna <> *blanks and savibanna = *blanks;
054300100805             rqsOpCode = 'INSERT';
054310100805           WHEN  v5ibanna = *blanks and savibanna <> *blanks;
054320100805             rqsOpCode = 'DELETE';
054330100805           OTHER;
054340100805             rqsOpCode = 'UPDATE';
054350100730         ENDSL;
054360100730         IF  rqsOpCode <> *blanks;
054370100730           exsr Coordinate;
054380100729         ENDIF;
054390100729       //?Non faccio controlli sul codice di ritorno
054400100729       //?ormai l'anagrafica è scritta
054410110223
054420110223       //?Se NON è anagrafica provvisoria:
054430110223       //?il cliente è stato bloccato
054440110223       //?e il potenziale NON è in categoria PERSO
054450110310       //?oppure
054460110310       //?il cliente NON è più bloccato
054470110310       //?e il potenziale è in categoria PERSO
054480110223       //?oppure
054490110223       //?arrivo da convalida
054500110223       //?e il potenziale NON è in categoria CODIFICATO
054510110223         IF  not *in06 and
054520110223             ((ACOabl <> *blanks and
054530110223               savabl = *blanks and V2Hfls <> 'P') or
054540110310              (ACOabl = *blanks and
054550110310               savabl <> *blanks and V2Hfls = 'P') or
054560110223              (*in07 and V2Hfls <> 'C'));
054570110223       //?richiamo pgm di calcolo categoria potenziale
054580110223           clear trmk05ds;
054590110223           IMK05cpo = ACOlib;
054600110223           trmk05r (kpjba : TRMK05DS);
054610110223         //?se categoria variata aggiorno potenziale
054620110304           IF  OMK05err = *blanks and OMK05cat <> V2Hfls;
054630110223             chain(e) ACOlib TNCPO01L;
054640110223             IF  not %error and %found(TNCPO01L);
054650110223               CPOfls = OMK05cat;
054660110223               update TNCPO000;
054670110223             ENDIF;
054680110223           ENDIF;
054690110223         ENDIF;
054700170523
054710170523      * Se cliente logistica no anagrfica clienti ritiro
054720170523      * e no spalmatura coordinate IBAN
054730170523     c                   IF        ClienteLog
054740170523     c                   goto      ByPassaAcr
054750170523     c                   ENDIF
054760150424
054770150424       //?Devo spalmare le coordinate bancarie e i cod.pagamenti
054780150424         WinInfo = *off;
054790150424         EoFW06 = *off;
054800150424         SELECT;
054810150424       //?spalmo da pag. NA a CA e DN
054820150424         WHEN  BonificoNA and
054830150424               (not BonificoCA or not BonificoDN);
054840150424           exsr SpalmaNA;
054850150424         WHEN  BonificoDN and
054860150424               (not BonificoCA or not BonificoNA);
054870150424           exsr SpalmaDN;
054880150424         WHEN  BonificoCA and
054890150424               (not BonificoDN or not BonificoNA);
054900150424           exsr SpalmaCA;
054910150424         ENDSL;
054920150424         IF  WinInfo;
054930150424           DOW not EoFW06;
054940150424             exfmt TA60W06;
054950150424             IF  *inkf;
054960150424               EoFW06 = *on;
054970150424               leave;
054980150424             ENDIF;
054990150424           ENDDO;
055000150424         ENDIF;
055010150424
055020100729      /end-free
055030051220
055040051220      * Se non sono in anagrafica provvisoria
055050120313if  1c                   If        Not *In06 and §ogorm <> *blanks
055060051220
055070051220      * Controllo il blocco/sblocco dei clienti per aggiornare anche
055080051220      * l'anagrafica clienti ritiro (solo in caso di blocco)
055090121121      * lo faccio solo se modifica perchè se è immissione non c'è
055100121121      * ancora l'anagrafica clienti ritiro
055110121121     c                   IF        *in02
055120051220     c                   ExSr      Sr_Ctrabl
055130121121     c                   ENDIF
055140051220
055150051220      * Richiamo l'immissione anagrafica clienti ritiro
055160051220if  2c                   If        *In01 or wokimm = *On
055170121121
055180121121      * NON lo faccio se immissione di codici 8888 - 9999 o incassi
055190121121      * da attribuire --> 0001 1° livello
055200121121      *               --> 1000 2° livello
055210160429      * immissione fatta da profilo EDP
055220121121     c                   SELECT
055230121121     c                   WHEN      wksc04 = 8888 or wksc04 = 9999
055240121121     c                   goto      noacr
055250160429     c                   WHEN       *in20 and
055260121121     c                             ((sav_livello = '1' and wksc04 = 0001) or
055270121121     c                              (sav_livello = '2' and wksc04 = 1000) or
055280121121     c                              (sav_livello = *blanks and
055290121121     c                               wCodIncAtt  = *blanks and
055300121121     c                              (wksc04 = 1000  or  wksc04 = 0001)))
055310121121     c                   goto      noacr
055320121121     c                   ENDSL
055330070809     c                   reset                   FIOR37ds
055340070810     c                   eval      I37opz  = '3'
055350070809     c                   movel     ACOksc        I37fgs
055360070809     c                   movel     ACOksc        I37cro
055370070809     c                   movel     ACOksc        I37ksc
055380070809     c                   movel(p)  FIOR37ds      KPJBU
055390070810     c                   call      'FIOR37R'
055400070809     c                   parm                    KPJBA
055410051220    2c                   EndIf
055420121121     c     noacr         tag
055430051220      * Richiamo la modifica dell'anagrafica clienti ritiro
055440060217if  2c                   If        *In02 and Not *In07
055450070809     c                   reset                   FIOR37ds
055460070809     c                   eval      I37opz  = 'A'
055470070809     c                   movel     ACOksc        I37fgs
055480070809     c                   movel     ACOksc        I37cro
055490070809     c                   movel(p)  FIOR37ds      KPJBU
055500070809     c                   call      'FIOR37R'
055510070809     c                   parm                    KPJBA
055520051220    2c                   EndIf
055530051216
055540051220    1c                   EndIf
055550160905
055560160905     c     ByPassaAcr    tag
055570160905
055580061012     c*
055590061012     c* se cliente di network dpd richiamo pgm gestione legami depot/cod.PdC
055600061012     c* per permettere l'aggiornamento del codice cliente sul file dei depot
055610061012     c                   if        not *in06 and §ogntw='DPD'
055620061012     c* imposta la ds di procedura
055630061012     c                   CLEAR                   tisie5ds
055640061012     c                   MOVE      '02'          de5op0
055650061012     c                   MOVEL     *blanks       de5op1
055660061012     c                   MOVEL     '0'           de5f03
055670061012     c                   MOVEL     '0'           de5f12
055680061012     c                   MOVEL     '0'           de5err
055690061012     c                   MOVEL     *blanks       de5msg
055700061012     c                   movel     'N'           de5sel
055710061012     c* lancia il programma
055720061012     c                   CALL      'TISIE5R'
055730061012     c                   PARM                    kpjba
055740061012     c                   PARM                    tisie5ds
055750061012     c                   endif
055760120510
055770120510      /free
055780120510       //?Se il cliente è stato bloccato devo richiamare il pgm delle INFO
055790120510       //?per obbligo di azzeramento della % affido a BRT
055800120510       //?Il richiamo lo faccio solo se:
055810120510       //?--> è appena stato immesso il blocco
055820120510       //?--> il pgm non è stato richiamto dal CMR x impostare il blocco
055830120510       //?--> c'è il codice del potenziale
055840120510         IF  not $Blocco and
055850120510             ACOabl <> *blanks and ACOabl <> savabl and
055860120510             ACOlib > 0;
055870120510           IF  not %open(CNACO16L);
055880120510             open CNACO16L;
055890120510           ENDIF;
055900120510         //?tutto questo solo se sto bloccando l'ultimo cliente della catena
055910120510         //?se ne ho ancora dei NON bloccati non devo azzerare
055920120510           setll ACOlib CNACO16L;
055930120510           reade ACOlib CNACO16L;
055940120510           DOW not %eof(CNACO16L);
055950120510             IF  ww_ACOabl = *blanks;
055960120510               leavesr;
055970120510             ENDIF;
055980120510             reade ACOlib CNACO16L;
055990120510           ENDDO;
056000120510         //?Aggancio le INFO commerciali
056010120510         //?se la % affido a BRT è = 0 non devo azzerare
056020120510           chain (ACOlib:'10 ':'_BAR') TICPI01L;
056030120510 1         IF  not %found(ticpi01l) or CPIval = 0;
056040120510             leavesr;
056050120510           ENDIF;
056060120510
056070120510         //?pgm INFO
056080120510           clear TRMK50DS;
056090120510           I50pgm = 'TNTA60R';
056100120510           I50cpo = ACOlib;
056110120510           I50rag = ACOrag;
056120120510           I50mod = 'G';
056130120510           I50atb = '0';
056140120510           TRMK50R (kpjba : TRMK50DS);
056150120510
056160120510           wTRMK50 = *on;
056170120510
056180120510         ENDIF;
056190120510
056200120510      /end-free
056210051220
056220051216     c                   EndSr
056230051220
056240060526      *------------------------------------------------------------------------*
056250060526      * Scrittura file variazioni
056260060526      *------------------------------------------------------------------------*
056270060526     c     ScriviACV     BegSr
056280060531     c                   Clear                   Tibs73ds
056290060531     c                   eval      ibs73pru=knmus
056300060531     c                   eval      ibs73noj=knmeb
056310060601     c                   eval      ibs73pgm='TNTA60R'
056320060531     c                   eval      ibs73immag='D'
056330060526     c* Immissione
056340060526     c                   if        *in01
056350060531     c                   eval      ibs73cta='I'
056360060531     c                   Call      'TIBS73R'
056370060531     c                   Parm                    Tibs73ds
056380060531     c                   Parm                    cnaco_73
056390060531     c                   Parm                    cnind_73
056400060531     c                   Parm                    cnclp_73
056410060531     c                   Parm                    fncls_73
056420060526     c                   endif
056430060526     c* Manutenzione
056440060526     C                   IF        *IN02
056450060531     c                   eval      ibs73cta='M'
056460060531     c                   Call      'TIBS73R'
056470060531     c                   Parm                    Tibs73ds
056480060531     c                   Parm                    cnaco_73
056490060531     c                   Parm                    cnind_73
056500060531     c                   Parm                    cnclp_73
056510060531     c                   Parm                    fncls_73
056520060531     c                   endif
056530060526     c
056540060531     c                   Eval      wtibs73 = *On
056550060526     c                   EndSr
056560150424
056570150424      /free
056580150424       //--------------------------------------------------------------
056590150427       //?Spalmo coordinate bancarie e pagamenti CA.
056600150424       //--------------------------------------------------------------
056610150424       BEGSR SpalmaCA;
056620150427
056630150512       //?Se il pagamento CA è estero
056640150427       //?spalmo solo il codice senza le coordinate
056650150427         IF  PagEsteroCA;
056660150427           clear CLPbab;
056670150427         ENDIF;
056680150427
056690150427       //?Anagrafica clienti Reale
056700150427         IF  not *in06;
056710150427       //?Aggancio FNCLS
056720150427           chain V1Cksc FNCLS01L;
056730150427           IF  not %found(FNCLS01L);
056740150427             leavesr;
056750150427           ENDIF;
056760150427       //?Spalmo pagamento e coordinate per DN
056770151112           IF  CodPagDN <> '1';
056780150427           IF  not BonificoDN;
056790150427         //?coordinate bancarie
056800150427             clear rqsOpCode;
056810150427             wpag = 'DN';
056820150427             wiban = CLPbab;
056830150427             wcodpo = %subst(CLStic:1:1);
056840150427             wcodpn = CLPfpc;
056850150427             wpgm  = 'TNTA60R';
056860150427             rqsOpCode = 'INSERT';
056870150427             exsr Coordinate;
056880150427             %subst(CLStic:1:1) = CLPfpc;
056890151112             WinInfo = *on;
056900150427           ENDIF;
056910151112           ENDIF;
056920150427       //?Spalmo pagamento e coordinate per NA
056930151112           IF  CodPagNA <> '1';
056940150427           IF  not BonificoNA;
056950150427         //?coordinate bancarie
056960150427             clear rqsOpCode;
056970150427             wpag = 'NA';
056980150427             wiban = CLPbab;
056990150427             wcodpo = %subst(CLStic:2:1);
057000150427             wcodpn = CLPfpc;
057010150427             wpgm  = 'TNTA60R';
057020150427             rqsOpCode = 'INSERT';
057030150427             exsr Coordinate;
057040150427             %subst(CLStic:2:1) = CLPfpc;
057050151112             WinInfo = *on;
057060150427           ENDIF;
057070151112           ENDIF;
057080150427       //?Aggiorno anagrafica
057090150427           update FNCLS000;
057100150427         ENDIF;
057110150427
057120150427       //?Anagrafica clienti Provvisoria
057130150427         IF  *in06;
057140150427       //?Aggancio TFCLS
057150150427           chain V1Cnrv TFCLS01L;
057160150427           IF  not %found(TFCLS01L);
057170150427             leavesr;
057180150427           ENDIF;
057190150427       //?Spalmo pagamento e coordinate per DN
057200151112           IF  CodPagDN <> '1';
057210150427           IF  not BonificoDN;
057220150427             clear rqsOpCode;
057230150427             wpag = 'DN';
057240150427             wiban = CLPbab;
057250150427             wcodpo = %subst(CLStic:1:1);
057260150427             wcodpn = CLPfpc;
057270150427             wpgm  = 'TNTA60R';
057280150427             rqsOpCode = 'INSERT';
057290150427             exsr Coordinate;
057300150427             %subst(CLStic:1:1) = CLPfpc;
057310151112             WinInfo = *on;
057320150427           ENDIF;
057330151112           ENDIF;
057340150427       //?Spalmo pagamento e coordinate per NA
057350151112           IF  CodPagNA <> '1';
057360150427           IF  not BonificoNA;
057370150427             clear rqsOpCode;
057380150427             wpag = 'NA';
057390150427             wiban = CLPbab;
057400150427             wcodpo = %subst(CLStic:2:1);
057410150427             wcodpn = CLPfpc;
057420150427             wpgm  = 'TNTA60R';
057430150427             rqsOpCode = 'INSERT';
057440150427             exsr Coordinate;
057450150427             %subst(CLStic:2:1) = CLPfpc;
057460151112             WinInfo = *on;
057470150427           ENDIF;
057480151112           ENDIF;
057490150427       //?Aggiorno anagrafica
057500150427           update TFCLS000;
057510150427         ENDIF;
057520150424
057530150424       ENDSR;
057540150424
057550150424       //--------------------------------------------------------------
057560150427       //?Spalmo coordinate bancarie e pagamenti DN.
057570150424       //--------------------------------------------------------------
057580150424       BEGSR SpalmaDN;
057590150427
057600150427         SELECT;
057610150427
057620150427       //?Se il pagamento DN è estero ed è
057630150427       //?Anagrafica clienti Reale
057640150427       //?spalmo solo il codice senza le coordinate
057650150427         WHEN  PagEsteroDN and not *in06;
057660150427       //?Memorizzo immagine precedente
057670150427           exsr ImmaginePrima;
057680150427       //?Spalmo pagamento per CA
057690151112           IF  CodPagCA <> '1';
057700150427           IF  not BonificoCA;
057710150427         //?Aggancio CNCLP
057720150427             chain (codut:V1Ckcc:V1Cksc) CNCLP00F;
057730150427             IF  not %found(CNCLP00F);
057740150427               leavesr;
057750150427             ENDIF;
057760150427             CLPfpc = %subst(CLStic:1:1);
057770150427             update CNCLP000;
057780151112             WinInfo = *on;
057790150427           ENDIF;
057800151112           ENDIF;
057810150427       //?Spalmo pagamento per NA
057820151112           IF  CodPagNA <> '1';
057830150427           IF  not BonificoNA;
057840150427         //?Aggancio FNCLS
057850150427             chain V1Cksc FNCLS01L;
057860150427             IF  not %found(FNCLS01L);
057870150427               leavesr;
057880150427             ENDIF;
057890150427             %subst(CLStic:2:1) = %subst(CLStic:1:1);
057900150427             update FNCLS000;
057910151112             WinInfo = *on;
057920150427           ENDIF;
057930151112           ENDIF;
057940150427       //?Immagine Dopo
057950150427           exsr ImmagineDopo;
057960150427
057970150427       //?Se il pagamento DN è estero ed è
057980150427       //?Anagrafica clienti Provvisoria
057990150427       //?spalmo solo il codice senza le coordinate
058000150427         WHEN  PagEsteroDN and *in06;
058010150427       //?Spalmo pagamento per CA
058020151112           IF  CodPagCA <> '1';
058030150427           IF  not BonificoCA;
058040150427         //?Aggancio TFCLP
058050150427             chain (codut:V1Ckcc:V1Cnrv) TFCLP00F;
058060150427             IF  not %found(TFCLP00F);
058070150427               leavesr;
058080150427             ENDIF;
058090150427             CLPfpc = %subst(CLStic:1:1);
058100150427             update TFCLP000;
058110151112             WinInfo = *on;
058120150427           ENDIF;
058130151112           ENDIF;
058140150427       //?Spalmo pagamento per NA
058150151112           IF  CodPagNA <> '1';
058160150427           IF  not BonificoNA;
058170150427         //?Aggancio TFCLS
058180150427             chain V1Cnrv TFCLS01L;
058190150427             IF  not %found(TFCLS01L);
058200150427               leavesr;
058210150427             ENDIF;
058220150427             %subst(CLStic:2:1) = %subst(CLStic:1:1);
058230150427             update TFCLS000;
058240151112             WinInfo = *on;
058250150427           ENDIF;
058260151112           ENDIF;
058270150427
058280150427         OTHER;
058290150427       //?Se arrivo qua il pagamento non è estero quindi spalmo tutto
058300150424       //?Recupero le coordinate bancarie DN
058310150427           wpag = 'DN';
058320150427           rqsOpCode = 'SEARCH';
058330150427           exsr Coordinate;
058340150424       //?Spalmo coordinate e pagamento per CA
058350151112           IF  CodPagCA <> '1';
058360150427           IF  not BonificoCA;
058370150424       //?Memorizzo immagine precedente
058380150427             IF  not *in06;
058390150427               exsr ImmaginePrima;
058400150427         //?Aggancio CNCLP
058410150427               chain (codut:V1Ckcc:V1Cksc) CNCLP00F;
058420150427               IF  not %found(CNCLP00F);
058430150427                 leavesr;
058440150427               ENDIF;
058450150427             ELSE;
058460150427         //?Aggancio TFCLP
058470150427               chain (codut:V1Ckcc:V1Cnrv) TFCLP00F;
058480150427               IF  not %found(TFCLP00F);
058490150427                 leavesr;
058500150427               ENDIF;
058510150427             ENDIF;
058520150427             CLPfpc = %subst(CLStic:1:1);
058530150427             IF  not *in06;
058540150427               CLPbab = TrulIbanO0.IBAN;
058550150427               CLPabi = TrulIbanO0.ABI;
058560150427               CLPcab = TrulIbanO0.CAB;
058570150427               update CNCLP000;
058580150427         //?Memorizzo immagine sucessiva
058590150427               exsr ImmagineDopo;
058600150427             ELSE;
058610150427               CLPbab = TrulIbanO1.IBAN;
058620150427               CLPabi = TrulIbanO1.ABI;
058630150427               CLPcab = TrulIbanO1.CAB;
058640150427               update TFCLP000;
058650150427             ENDIF;
058660151112             WinInfo = *on;
058670150427           ENDIF;
058680151112           ENDIF;
058690150424
058700150424       //?Spalmo coordinate e pagamento per NA
058710151112           IF  CodPagNA <> '1';
058720150427           IF  not BonificoNA;
058730150427             clear rqsOpCode;
058740150427             wpag = 'NA';
058750150427         //?Aggancio FNCLS
058760150427             IF  not *in06;
058770150427               chain V1Cksc FNCLS01L;
058780150427               IF  not %found(FNCLS01L);
058790150427                 leavesr;
058800150427               ENDIF;
058810150427               wiban = TrulIbanO0.IBAN;
058820150427               wbic  = TrulIbanO0.BIC;
058830150427               wcodpo = %subst(CLStic:2:1);
058840150427               wcodpn = %subst(CLStic:1:1);
058850150427             ELSE;
058860150427         //?Aggancio TFCLS
058870150427               chain V1Cnrv TFCLS01L;
058880150427               IF  not %found(TFCLS01L);
058890150427                 leavesr;
058900150427               ENDIF;
058910150427               wiban = TrulIbanO1.IBAN;
058920150427               wbic  = TrulIbanO1.BIC;
058930150427               wcodpo = %subst(CLStic:2:1);
058940150427               wcodpn = %subst(CLStic:1:1);
058950150427             ENDIF;
058960150427             wpgm  = 'TNTA60R';
058970150427             rqsOpCode = 'INSERT';
058980150427             exsr Coordinate;
058990150427             %subst(CLStic:2:1) = %subst(CLStic:1:1);
059000150427         //?Aggiorno anagrafica
059010150427             IF  not *in06;
059020150427               update FNCLS000;
059030150427             ELSE;
059040150427               update TFCLS000;
059050150427             ENDIF;
059060151112             WinInfo = *on;
059070150427           ENDIF;
059080151112           ENDIF;
059090150427         ENDSL;
059100150424
059110150424       ENDSR;
059120150424
059130150424       //--------------------------------------------------------------
059140150427       //?Spalmo coordinate bancarie e pagamenti NA.
059150150424       //--------------------------------------------------------------
059160150424       BEGSR SpalmaNA;
059170150427
059180150427         SELECT;
059190150427
059200150427       //?Se il pagamento NA è estero ed è
059210150427       //?Anagrafica clienti Reale
059220150427       //?spalmo solo il codice senza le coordinate
059230150427         WHEN  PagEsteroNA and not *in06;
059240150427       //?Memorizzo immagine precedente
059250150427           exsr ImmaginePrima;
059260150427       //?Spalmo pagamento per CA
059270151112           IF  CodPagCA <> '1';
059280150427           IF  not BonificoCA;
059290150427         //?Aggancio CNCLP
059300150427             chain (codut:V1Ckcc:V1Cksc) CNCLP00F;
059310150427             IF  not %found(CNCLP00F);
059320150427               leavesr;
059330150427             ENDIF;
059340150427             CLPfpc = %subst(CLStic:2:1);
059350150427             update CNCLP000;
059360151112             WinInfo = *on;
059370150427           ENDIF;
059380151112           ENDIF;
059390150427       //?Spalmo pagamento per DN
059400151112           IF  CodPagDN <> '1';
059410150427           IF  not BonificoDN;
059420150427         //?Aggancio FNCLS
059430150427             chain V1Cksc FNCLS01L;
059440150427             IF  not %found(FNCLS01L);
059450150427               leavesr;
059460150427             ENDIF;
059470150427             %subst(CLStic:1:1) = %subst(CLStic:2:1);
059480150427             update FNCLS000;
059490151112             WinInfo = *on;
059500150427           ENDIF;
059510151112           ENDIF;
059520150427       //?Immagine Dopo
059530150427           exsr ImmagineDopo;
059540150427
059550150427       //?Se il pagamento NA è estero ed è
059560150427       //?Anagrafica clienti Provvisoria
059570150427       //?spalmo solo il codice senza le coordinate
059580150427         WHEN  PagEsteroNA and *in06;
059590150427       //?Spalmo pagamento per CA
059600151112           IF  CodPagCA <> '1';
059610150427           IF  not BonificoCA;
059620150427         //?Aggancio TFCLP
059630150427             chain (codut:V1Ckcc:V1Cnrv) TFCLP00F;
059640150427             IF  not %found(TFCLP00F);
059650150427               leavesr;
059660150427             ENDIF;
059670150427             CLPfpc = %subst(CLStic:2:1);
059680150427             update TFCLP000;
059690151112             WinInfo = *on;
059700150427           ENDIF;
059710151112           ENDIF;
059720150427       //?Spalmo pagamento per DN
059730151112           IF  CodPagDN <> '1';
059740150427           IF  not BonificoDN;
059750150427         //?Aggancio TFCLS
059760150427             chain V1Cnrv TFCLS01L;
059770150427             IF  not %found(TFCLS01L);
059780150427               leavesr;
059790150427             ENDIF;
059800150427             %subst(CLStic:1:1) = %subst(CLStic:2:1);
059810150427             update TFCLS000;
059820151112             WinInfo = *on;
059830150427           ENDIF;
059840151112           ENDIF;
059850150427
059860150427         OTHER;
059870150427       //?Se arrivo qua il pagamento non è estero quindi spalmo tutto
059880150424       //?Recupero le coordinate bancarie NA
059890150427           wpag = 'NA';
059900150427           rqsOpCode = 'SEARCH';
059910150427           exsr Coordinate;
059920150424       //?Spalmo coordinate e pagamento per CA
059930151112           IF  CodPagCA <> '1';
059940150427           IF  not BonificoCA;
059950150424       //?Memorizzo immagine precedente
059960150427             IF  not *in06;
059970150427               exsr ImmaginePrima;
059980150424       //?Aggancio CNCLP
059990150427               chain (codut:V1Ckcc:V1Cksc) CNCLP00F;
060000150427               IF  not %found(CNCLP00F);
060010150427                 leavesr;
060020150427               ENDIF;
060030150427             ELSE;
060040150427       //?Aggancio TFCLP
060050150427               chain (codut:V1Ckcc:V1Cnrv) TFCLP00F;
060060150427               IF  not %found(TFCLP00F);
060070150427                 leavesr;
060080150427               ENDIF;
060090150427             ENDIF;
060100150427             CLPfpc = %subst(CLStic:2:1);
060110150427             IF  not *in06;
060120150427               CLPbab = TrulIbanO0.IBAN;
060130150427               CLPabi = TrulIbanO0.ABI;
060140150427               CLPcab = TrulIbanO0.CAB;
060150150427               update CNCLP000;
060160150427         //?Memorizzo immagine sucessiva
060170150427               exsr ImmagineDopo;
060180150427             ELSE;
060190150427               CLPbab = TrulIbanO1.IBAN;
060200150427               CLPabi = TrulIbanO1.ABI;
060210150427               CLPcab = TrulIbanO1.CAB;
060220150427               update TFCLP000;
060230150427             ENDIF;
060240151112             WinInfo = *on;
060250150427           ENDIF;
060260151112           ENDIF;
060270150424
060280150424       //?Spalmo coordinate e pagamento per DN
060290151112           IF  CodPagDN <> '1';
060300150427           IF  not BonificoDN;
060310150427             clear rqsOpCode;
060320150427             wpag = 'DN';
060330150427         //?Aggancio FNCLS
060340150427             IF  not *in06;
060350150427               chain V1Cksc FNCLS01L;
060360150427               IF  not %found(FNCLS01L);
060370150427                 leavesr;
060380150427               ENDIF;
060390150427               wiban = TrulIbanO0.IBAN;
060400150427               wbic  = TrulIbanO0.BIC;
060410150427               wcodpo = %subst(CLStic:1:1);
060420150427               wcodpn = %subst(CLStic:2:1);
060430150427             ELSE;
060440150427         //?Aggancio TFCLS
060450150427               chain V1Cnrv TFCLS01L;
060460150427               IF  not %found(TFCLS01L);
060470150427                 leavesr;
060480150427               ENDIF;
060490150427               wiban = TrulIbanO1.IBAN;
060500150427               wbic  = TrulIbanO1.BIC;
060510150427               wcodpo = %subst(CLStic:1:1);
060520150427               wcodpn = %subst(CLStic:2:1);
060530150427             ENDIF;
060540150427             wpgm  = 'TNTA60R';
060550150427             rqsOpCode = 'INSERT';
060560150427             exsr Coordinate;
060570150427             %subst(CLStic:1:1) = %subst(CLStic:2:1);
060580150427         //?Aggiorno anagrafica
060590150427             IF  not *in06;
060600150427               update FNCLS000;
060610150427             ELSE;
060620150427               update TFCLS000;
060630150427             ENDIF;
060640151112             WinInfo = *on;
060650150427           ENDIF;
060660151112           ENDIF;
060670150424
060680150427         ENDSL;
060690150424
060700150424       ENDSR;
060710150424      /end-free
060720150424
060730051220      *------------------------------------------------------------------------*
060740051220      * AGGIORNAMENTO DEL CODICE POTENZIALE
060750051220      *------------------------------------------------------------------------*
060760051220     c     Sr_Aggcpo     BegSr
060770100407
060780100407      /free
060790100806       //?Se variato codice potenziale
060800110928       //?o se immissione di nuovo codice
060810110928        IF  (v2ccpo <> savcpo and savcpo > 0) or *in01;
060820100407         //?richiamo programma di comodo per sistemare potenziale
060830100407         //?sui vari archivi (trattativa, anagrafica provvisoria, attività)
060840100407         //?nel caso di trattativa aperta
060850110223         //?in ogni caso ricalcola e aggiorna sempre la categoria
060860110223         //?del vecchio e nuovo potenziale
060870100407          clear TRMK28DS;
060880100407          IMK28cpo   = v2ccpo;
060890100407          IMK28cpoex = savcpo;
060900100407          IMK28ksc   = v1cksc;
060910100407          trmk28r (TRMK28DS);
060920100407        ENDIF;
060930121105
060940121105       //?Se Immissione di nuovo cliente (no anagrafica provvisoria)
060950121105       //?devo aggiornare la data primo contatto sul Potenziale
060960121105        IF  *in01 and not *in06 and V2Ccpo > 0 and
060970121105           (§CPOdtpcon = *blanks or §CPOdtpcon = *zeros);
060980121105          chain(e) CPOcpo TNCPO01L;
060990121105          IF  not %error and %found(TNCPO01L);
061000121105            dCPO01 = CPOrst;
061010121105            §CPOdtpcon = %editc(dataoggi:'X');
061020121105            CPOrst = dCPO01;
061030121105            update TNCPO000;
061040121105          ENDIF;
061050121105        ENDIF;
061060121105
061070100407      /end-free
061080080307
061090080307      * se sono in anagrafica provvisoria aggiorno codice fiscale e p.iva
061100080307      * sul potenziale se sul potenziale mancano
061110080307     c                   if        *in06  and *in01
061120110516     c     v2ccpo        Chain(e)  Tncpo01l
061130110516     c                   if        %error
061140110516     c                   leavesr
061150110516     c                   endif
061160080307     c                   if        %found(tncpo01l)
061170160803      /free
061180160803       //?Salvo l'immagine precedente dell'anagrafica potenziale
061190160803         exsr ImmaginePrimaCpo;
061200160803      /end-free
061210080307     c                   if        (cpopiv=*blanks and v2civa<>*blanks)
061220080307     c                   eval      cpopiv=v2civa
061230080307     c                   z-add     *date         cpodtr
061240080307     c                   endif
061250080307     c                   if        (cpocdf=*blanks and v2ccdf<>*blanks)
061260080307     c                   eval      cpocdf=v2ccdf
061270080307     c                   z-add     *date         cpodtr
061280080307     c                   endif
061290080307     c                   update    tncpo000
061300160803      /free
061310160803       //?Scrivo la Variazione fatta sull'anagrafica potenziale
061320160803         exsr ScriviVarCpo;
061330160803      /end-free
061340080307     c                   endif
061350080307     c                   endif
061360051220
061370051220     c                   EndSr
061380051216
061390051216      *------------------------------------------------------------------------*
061400051125      * CONTROLLO DEL CODICE POTENZIALE
061410051125      *------------------------------------------------------------------------*
061420051125     c     Sr_Ctrcpo     BegSr
061430051125
061440111122      * Obbligatorio se cliente, se non è un 8888/9999
061450110223      * e se non è un 'incassi da attribuire'
061460111122      * se non è utente EDP e codice nuovo x 'incassi da attribuire'
061470051125     c                   If        v2ccpo = *Zeros and
061480110223     c                             v1ckcc = 151 and
061490051125     c                             wksc04 <> 8888 and wksc04 <> 9999
061500110223     c                             and %editc(v1cksc:'X') <> wCodIncAtt
061510170516      // Se cliente potenziale emetto un messaggio forzabile
061520170516       IF  (ClienteLog and not wForzaPotenziale) or
061530170516            not ClienteLog;
061540051125     c                   Eval      *In28 = *On
061550051202     c                   Eval      h2riga = 14
061560051125     c                   Eval      h2colo = 28
061570051125     c                   Eval      v2cmsg = msg(12)
061580170516       IF  ClienteLog;
061590170516         V2Cmsg = %trim(V2Cmsg) + '. Enter per forzare.';
061600170516         wForzaPotenziale = *on;
061610170516       ENDIF;
061620160429     c                   IF        *in20 and *in01
061630111122     c                             and sav_livello = *blanks
061640111122     c                             and wCodIncAtt = *blanks and
061650111122     c                             (wksc04 = 0001 or wksc04 = 1000)
061660111122     c                   clear                   v2cmsg
061670111122     c                   eval      *in28 = *off
061680111122     c                   ENDIF
061690051125     c                   Leavesr
061700170516       ENDIF;
061710051125     c                   EndIf
061720051125      * Carico i dati del potenziale se inserito
061730051125if  1c                   If        v2ccpo <> *Zeros and
061740051125      * e se variato il codice potenziale
061750051125     c                             v2ccpo <> savcpo
061760060221     c                             and wokpot = *off
061770051125      * Controllo se esite
061780060221     c     v2ccpo        Chain(n)  Tncpo01l
061790051125     c                   If        Not %Found(Tncpo01l)
061800051125     c                   Eval      *In28 = *On
061810051202     c                   Eval      h2riga = 14
061820051125     c                   Eval      h2colo = 28
061830051125     c                   Eval      v2cmsg = msg(13)
061840051125     c                   Leavesr
061850051125     c                   EndIf
061860051125      * Controllo se valido
061870051125     c                   If        cpoatb <> *Blanks
061880051125     c                   Eval      *In28 = *On
061890150427     c                   Eval      h2riga = 14
061900051125     c                   Eval      h2colo = 28
061910051125     c                   Eval      v2cmsg = msg(13)
061920051125     c                   Leavesr
061930051125     c                   EndIf
061940051125      * Controllo se l'utente può gestire il potenziale
061950051125     c                   Move      cpoflt        w003a
061960051125     c     w003a         Lookup    skpogcpo                               30
061970051125     c                   If        Not *In30
061980051125     c                   Eval      *In28 = *On
061990051202     c                   Eval      h2riga = 14
062000051125     c                   Eval      h2colo = 28
062010060110     c                   Eval      v2cmsg = msg(14)
062020051125     c                   Leavesr
062030051125     c                   EndIf
062040100604
062050100604      /free
062060100604       //?Se NON sono in ambiente di filiale non posso variare il potenziale
062070100604       IF  *in09 and v2ccpo <> savcpo and savcpo > 0;
062080100604         *in28 = *on;
062090100604         h2riga = 07;
062100100604         h2colo = 28;
062110100604         v2cmsg = 'Potenziale modificabile SOLO da utente di filiale';
062120100604         leavesr;
062130100604       ENDIF;
062140100806       //?controllo se con il vecchio potenziale c'è una
062150100604       //?trattativa aperta, in questo caso controllo che non ne esista già
062160100604       //?una con il nuovo potenziale
062170100806       IF  v2ccpo <> savcpo and savcpo > 0;
062180100604       //?con vecchio potenziale
062190100604         clear TNTA43ds;
062200100604         ITA43cpo = savcpo;
062210100604         ITA43ksc = v1cksc;
062220100604         ITA43cmm = %dec(v3cage:7:0);
062230100604         tnta43r (tnta43ds);
062240100604       //?trovo trattativa aperta
062250100604         IF  OTA43nrv > 0;
062260100604         //?rifaccio il controllo con nuovo potenziale
062270100604           clear TNTA43ds;
062280100604           ITA43cpo = v2ccpo;
062290100604           ITA43ksc = v1cksc;
062300100604           ITA43cmm = %dec(v3cage:7:0);
062310100604           tnta43r (tnta43ds);
062320100604         //?trovo trattativa aperta
062330100604           IF  OTA43nrv > 0;
062340100604             *in28 = *on;
062350100604             h2riga = 07;
062360100604             h2colo = 28;
062370100604             v2cmsg = 'Modifica non possibile, trattativa aperta sul +
062380100604                       potenziale immesso';
062390100604             leavesr;
062400100604           ENDIF;
062410100604         ENDIF ;
062420100604       ENDIF;
062430100604      /end-free
062440051212
062450051206      * Carico a video i dati del potenziale se sono in immissione
062460051206     c   01              ExSr      Sr_Carcpo
062470051125    1c                   EndIf
062480051125
062490051125     c                   EndSr
062500051116
062510051116      *------------------------------------------------------------------------*
062520051116      * CARICO I DATI DEL CODICE POTENZIALE
062530051116      *------------------------------------------------------------------------*
062540051116     c     Sr_Carcpo     BegSr
062550140714
062560140714     c     CPOcpo        Chain     Tncpo11l
062570051116
062580051116     c                   Eval      v2crag = cporag
062590051116     c                   Eval      v2cvia = cpovia
062600051116     c                   Eval      v2ccae = cpocap
062610051117     c                   Eval      v2csta = cponaz
062620051116     c                   Eval      v2ccit = cpocit
062630051116     c                   Eval      v2cprv = cpoprv
062640140714     c                   Eval      v2ctel = cpo1tel
062650140714     c                   Eval      v2ctlf = cpo1fax
062660051117     c                   Eval      v2ccpo = cpocpo
062670051117     c                   Eval      savcpo = cpocpo
062680051130     c                   Move      cposct        savitc
062690060208     c                   Eval      v2dcpo = cporag
062700080306     c                   Eval      v2ccdf = cpocdf
062710080306     c                   Eval      savcdf = v2ccdf
062720080306    5c                   if        %subst(cpopiv:1:2)>='00'
062730080306     c                   Movel     cpopiv        v2ivait
062740080306     c                   else
062750080306     c                   Movel     cpopiv        v2civa
062760080306    5c                   endif
062770080306     c                   Eval      saviva = v2civa
062780051116
062790051116     c                   EndSr
062800091119
062810091119      *------------------------------------------------------------------------*
062820091119      * AGGIORNAMENTO NOTA DC
062830091119      *------------------------------------------------------------------------*
062840091119     c     Sr_AggntcDC   BegSr
062850091119
062860091119     c   70
062870091119     corn06              Eval      kntcapl = 'C'
062880100806     c   06
062890091119     cann70              Eval      kntcapl = 'T'
062900091119     c                   Movel(p)  dutkci        kntcnk1
062910091119     c   70
062920091119     corn06              Move      v1cksc        kntcnk1
062930100806     c   06
062940100806     cann70              Move      v1cnrv        kntcnk1
062950091119     c                   Clear                   kntcnk2
062960091119     c                   Movel     'DC'          kntctnt
062970091119     c     kTfntc        Chain     Tfntc01l
062980091119     c                   If        %Found(Tfntc01l)
062990091119      * devo cancellarla perchè campo vuoto
063000091119     c                   if        v2ntcdc = *blanks
063010091119     c                   delete    tfntc
063020091119     c                   endif
063030091119      * aggiorno la nota
063040091119     c                   if        v2ntcdc <> *blanks
063050091119     c                   eval      ntcrnt = v2ntcdc
063060091119     c                   move      *date         ntcntr
063070091119     c                   update    tfntc
063080091119     c                   endif
063090091119     c                   endif
063100091119      * scrivo la nota
063110091119     c                   If        not %Found(Tfntc01l)
063120091119     c                   clear                   tfntc
063130091119     c                   eval      ntcapl = kntcapl
063140091119     c                   eval      ntcnk1 = kntcnk1
063150091119     c                   eval      ntctnt = kntctnt
063160091119     c                   eval      ntcrnt = v2ntcdc
063170091119     c                   eval      ntcsns = 'S'
063180091119     c                   move      *date         ntcntr
063190091119     c                   write     tfntc
063200091119     c                   endif
063210091119
063220091119     c                   EndSr
063230051212
063240051212      *--------------------------------------------------------------------*
063250051212      * WINDOW X CONFERMA DI VARIAZIONE POTENZIALE
063260051212      *--------------------------------------------------------------------*
063270051212     c     Sr_Winpot     begsr
063280051212
063290051212     c                   Reset                   wokpot
063300051212     c                   Eval      w01cok = 'NO'
063310051212     c                   Exfmt     ta60w01
063320051212      * Ok il nuovo potenziale è da tenere
063330051212if  1c                   If        w01cok = 'SI'
063340051212     c                   Eval      wokpot = *On
063350051212    1c                   EndIf
063360051212
063370051212     c                   EndSr
063380051118
063390051118      *------------------------------------------------------------------------*
063400051118      * CONTROLLO INDIRIZZO
063410051118      *------------------------------------------------------------------------*
063420051118     c     Sr_Ctrind     BegSr
063430051118
063440051118     c                   Clear                   fnlv13ds
063450051118     c                   Clear                   tisi95ds
063460051124     c                   Eval      *In90 = *Off
063470051118
063480051118      * Indirizzo completo
063490051118     c                   Z-add     *date         i14dri
063500051118     c                   Call      'FNLV14R'
063510051118     c                   Parm                    kpjba
063520051118     c                   Parm                    fnlv14ds
063530051118
063540051118s   1c                   Select
063550051118     c                   When      o14err = '1'
063560051124     c                   Eval      *In90 = *On
063570051130     c                   Eval      h2riga = 07
063580051118     c                   Eval      h2colo = 28
063590051124     c                   If        o14msg <> *Blanks
063600051124     c                   Eval      *In28 = *On
063610051118     c                   Eval      v2cmsg = o14msg
063620051124     c                   EndIf
063630051124     c                   Leavesr
063640051118     c                   When      o14err = '2'
063650051124     c                   Eval      *In90 = *On
063660051130     c                   Eval      h2riga = 08
063670051118     c                   Eval      h2colo = 28
063680051124     c                   If        o14msg <> *Blanks
063690051124     c                   Eval      *In28 = *On
063700051118     c                   Eval      v2cmsg = o14msg
063710051124     c                   EndIf
063720051124     c                   Leavesr
063730051118     c                   When      o14err = '5'
063740051124     c                   Eval      *In90 = *On
063750051130     c                   Eval      h2riga = 09
063760051118     c                   Eval      h2colo = 18
063770051124     c                   If        o14msg <> *Blanks
063780051124     c                   Eval      *In28 = *On
063790051118     c                   Eval      v2cmsg = o14msg
063800051124     c                   EndIf
063810051124     c                   Leavesr
063820051118     c                   When      o14err = '3'
063830051124     c                   Eval      *In90 = *On
063840051130     c                   Eval      h2riga = 09
063850051118     c                   Eval      h2colo = 28
063860051124     c                   If        o14msg <> *Blanks
063870051124     c                   Eval      *In28 = *On
063880051118     c                   Eval      v2cmsg = o14msg
063890051124     c                   EndIf
063900051124     c                   Leavesr
063910051118     c                   When      o14err = '4'
063920051124     c                   Eval      *In90 = *On
063930051130     c                   Eval      h2riga = 09
063940051118     c                   Eval      h2colo = 59
063950051124     c                   If        o14msg <> *Blanks
063960051124     c                   Eval      *In28 = *On
063970051118     c                   Eval      v2cmsg = o14msg
063980051124     c                   EndIf
063990051124     c                   Leavesr
064000051118     c                   When      o14err = '6'
064010051124     c                   Eval      *In90 = *On
064020051130     c                   Eval      h2riga = 09
064030051118     c                   Eval      h2colo = 62
064040051124     c                   If        o14msg <> *Blanks
064050051124     c                   Eval      *In28 = *On
064060051118     c                   Eval      v2cmsg = o14msg
064070051124     c                   EndIf
064080051124     c                   Leavesr
064090051118     c                   When      o14err <> *Blanks and o14msg = *Blanks
064100051118     c                   Goto      emi02
064110051118    1c                   EndSl
064120051118
064130051118      * Nazione con cappario controllo Cap
064140051118if  1c                   If        o14cpp = *Blanks
064150051118     c                   Eval      i95tcn = '7'
064160051118     c                   Eval      i95cap = e14cap
064170051118     c                   Eval      i95loc = e14loc
064180051118     c                   Eval      i95prv = e14prv
064190051118     c                   Eval      i95nar = e14nar
064200051118     c                   Z-add     *date         i95dat
064210051118     c                   Eval      i13af0 = 'S'
064220051118     c                   Eval      i13cnv = 'S'
064230051118     c                   Eval      i13af1 = 'S'
064240051118     c                   Eval      i13la3 = 'S'
064250051118      * Se dati variati controllo se congruenti
064260051118if  2c                   If        e14cap <> wcae or e14loc <> wcit or
064270051118     c                             e14prv <> wprv or e14nar <> wsta
064280051118     c                   Clear                   wfz3
064290051118     c                   Eval      wcae = e14cap
064300051118     c                   Eval      wcit = e14loc
064310051118     c                   Eval      wprv = e14prv
064320051118     c                   Eval      wsta = e14nar
064330051118    2c                   EndIf
064340051118     c                   Eval      E13fz3 = wfz3
064350051118     c                   Call      'FNLV13R'
064360051118     c                   Parm                    kpjba
064370051118     c                   Parm                    fnlv13ds
064380051118     c                   Parm                    tisi95ds
064390051118
064400051118     c                   If        o13ron = 'S'
064410051118     c                   Eval      e14nar = o95nar
064420051118     c                   EndIf
064430051118     c                   If        o13roc = 'S'
064440051118     c                   Eval      e14cap = o95cap
064450051118     c                   EndIf
064460051118     c                   If        o13rop = 'S'
064470051118     c                   Eval      e14prv = o95prv
064480051118     c                   EndIf
064490051118     c                   If        o13rol = 'S'
064500051118     c                   Eval      e14loc = o95loc
064510051118     c                   EndIf
064520051118
064530051124      * Reimposto le forzature
064540051118     c                   Eval      wfz3 = E13fz3
064550051118
064560051118s   2c                   Select
064570051118     c                   When      o13err = '5'
064580051118     c                   Eval      *In28 = *On
064590051130     c                   Eval      h2riga = 09
064600051118     c                   Eval      h2colo = 18
064610051118     c                   Eval      v2cmsg = o13msg
064620051118     c                   Leavesr
064630051118     c                   When      o13err = '3'
064640051118     c                   Eval      *In28 = *On
064650051130     c                   Eval      h2riga = 09
064660051118     c                   Eval      h2colo = 28
064670051118     c                   Eval      v2cmsg = o13msg
064680051118     c                   Leavesr
064690051118     c                   When      o13err = '4'
064700051118     c                   Eval      *In28 = *On
064710051130     c                   Eval      h2riga = 09
064720051118     c                   Eval      h2colo = 59
064730051118     c                   Eval      v2cmsg = o13msg
064740051118     c                   Leavesr
064750051118     c                   When      o13err = '6'
064760051118     c                   Eval      *In28 = *On
064770051130     c                   Eval      h2riga = 09
064780051118     c                   Eval      h2colo = 62
064790051118     c                   Eval      v2cmsg = o13msg
064800051118     c                   Leavesr
064810051118     c                   When      o13err <> *Blanks and o13msg = *Blanks
064820051118     c                   Goto      emi02
064830051118    2c                   EndSl
064840051118
064850051118      * Se nazione senza cappario
064860051118   x1c                   Else
064870051122      * Il cliente deve essere bloccato con 8 (si può usare solo x fatturare e
064880051122      *                                        non per bollettare)
064890051118if  2c                   If        v2cabl <> '8'
064900051118     c                   Eval      *In28 = *On
064910051130     c                   Eval      h2riga = 09
064920051118     c                   Eval      h2colo = 62
064930051118     c                   Eval      v2cmsg = msg(15)
064940051118     c                   Leavesr
064950051118    2c                   EndIf
064960051118    1c                   EndIf
064970051118
064980051118      * Cap obsoleto
064990051118     c                   If        o95cf1 = 'O'
065000051118     c                   Eval      *In28 = *On
065010051130     c                   Eval      h2riga = 09
065020051118     c                   Eval      h2colo = 18
065030051118     c                   Eval      v2cmsg = msg(16)
065040051118     c                   Leavesr
065050051118     c                   EndIf
065060051118
065070051118     c                   EndSr
065080051118
065090051118      *------------------------------------------------------------------------*
065100051118      * CONTROLLO CODICE FISCALE
065110051118      *------------------------------------------------------------------------*
065120051118     c     Sr_Ctrcdf     BegSr
065130151014     c                   clear                   w_scfsta
065140100729
065150061030      * devo richiamare il nuovo driver fatto per controllare il codice
065160061030      * fiscale e la partita iva
065170080421      * rihciamo anche per codice vuoto, per controllare i casi
065180080421      *  di obbligo non inserimento
065190151014     c                   clear                   xcfiva1ds
065200061030     c                   eval      xcfivapar = v2ccdf
065210061030     c                   eval      xcfivanar = v2csta
065220061030     c                   eval      xcfivaprv = v2cprv
065230151014     c                   eval      xcfivacap = v2ccae
065240151014     c                   eval      xcfivaloc = v2ccit
065250151014     c                   eval      xcfivapiva= v2civa
065260151014     c                   call      'XCFIVAR1'
065270151014     c                   parm                    xcfiva1ds
065280080421     c
065290080421    1c                   if        xcfivaflg < *zeros
065300051118     c                   Eval      *In28 = *On
065310051202     c                   Eval      h2riga = 12
065320051118     c                   Eval      h2colo = 28
065330080421     c                   Eval      v2cmsg = xcfivamsg
065340051118     c                   Leavesr
065350080421    1c                   EndIf
065360080421
065370080421      * il codice fiscale è obbligatorio se sto inserendo un codice nuovo
065380080421      * cliente italia a partire dalla data decorrenza per p.iva sarà
065390080421      * obbligatorio non forzabile anche in caso di codice non nuovo
065400080421
065410080421      * Escludo i casi previsti di NON inserimento
065420080421    1c  n06              if        xcfivaflg<>9 and
065430080421     c                             v2csta=*blanks and v2ccdf=*blanks
065440080421     c
065450080421     c* prima di dare errore verifico se cliente del sottoconto intestazione
065460080421     c* fattura ha la nazione ed in questo caso non do errore
065470080421     c                   exsr      sr_repscfsta
065480080421    2c                   if        w_scfsta=*blanks
065490080421     c                   eval      *In28 = *On
065500080421     c                   eval      h2riga = 12
065510080421     c                   eval      h2colo = 28
065520080421     c                   eval      v2cmsg = msg(51)
065530080421     c                   leavesr
065540080421    2c                   endif
065550080421    1c                   endif
065560080306     c* Se provengo dalle visite e il relativo potenziale non ha il cod.fisc
065570080306     c* errore forzabile se inserito un codice fiscale già presente su altro
065580080306     c* potenziale
065590080421    1c                   if        *in06 and v2ccpo<>*zeros and v2ccdf<>*blanks
065600080306     c     v2ccpo        Chain(n)  Tncpo01l
065610080421    2c                   if        %found(tncpo01l) and cpocdf=*blanks
065620080306     c                             and v2ccdf<>savcdf
065630080306     c                             and v2ccdf<>cod_forzcdf
065640080306     c                   clear                   savrag
065650080307     c                   eval      codice=v2ccdf
065660080306     c                   exsr      sr_CdfAltroP
065670080306     c                   exsr      sr_msgcdf
065680080306     c   28              leavesr
065690080421    2c                   endif
065700080421    1c                   endif
065710051118
065720051118     c                   EndSr
065730051118
065740051118      *------------------------------------------------------------------------*
065750051118      * CONTROLLO PARTITA IVA E STATO
065760051118      *------------------------------------------------------------------------*
065770051118     c     Sr_Ctriva     BegSr
065780051118
065790051118      * STATO
065800051118      * --> ricerca
065810051118     c                   Eval      wpos = %scan('?':v2csta)
065820051118     c                   If        wpos > 0
065830051118     c                   Eval      kcod = '15'
065840051118     c                   call      'TRUL19R'
065850051118     c                   parm                    kcod
065860051118     c                   parm                    ordina
065870051118     c                   parm                    kkey
065880051118     c                   parm                    comando
065890051118     c                   Eval      v2csta = kkey
065900051130     c                   Eval      h2riga = 09
065910051118     c                   Eval      h2colo = 62
065920051124     c                   Eval      *In90 = *On
065930051118     c                   EndIf
065940051118      * --> controllo
065950051118     c                   Clear                   ds15
065960051118     c                   Eval      kcod = '15'
065970051118     c                   Eval      kkey = v2csta
065980051118     c     kTabel        Chain     Tabel00f
065990051118     c                   If        Not %Found(Tabel00f) or
066000051125     c                             tblflg <> *Blanks
066010051118     c                   Eval      *In28 = *On
066020051130     c                   Eval      h2riga = 09
066030051118     c                   Eval      h2colo = 62
066040051118     c                   Eval      v2cmsg = msg(17)
066050051118     c                   Leavesr
066060051118     c                   EndIf
066070051125     c                   Eval      ds15 = tbluni
066080051118
066090051129      * --> controllo
066100151014     c**                 If        v2civa <> *Blanks
066110061030      * devo richiamare il nuovo driver fatto per controllare il codice
066120061030      * fiscale e la partita iva
066130151014     c                   clear                   xcfiva1ds
066140061030     c                   eval      xcfivamod = 'P'
066150061030     c                   eval      xcfivapar = v2civa
066160061030     c                   eval      xcfivanar = v2csta
066170061030     c                   eval      xcfivaprv = v2cprv
066180151014     c                   eval      xcfivacap = v2ccae
066190151014     c                   eval      xcfivaloc = v2ccit
066200151014     c                   eval      xcfivapiva= v2civa
066210151014     c                   call      'XCFIVAR1'
066220151014     c                   parm                    xcfiva1ds
066230061106      * --> errata forzabile
066240061106     c                   If        xcfivaflg = -9 and wforzaiva = *off
066250061106     c                   Eval      *In28 = *On
066260061106     c                   Eval      h2riga = 12
066270061106     c                   Eval      h2colo = 52
066280061106     c                   Eval      v2cmsg = xcfivamsg
066290061106     c                   Eval      v2cmsg = %trim(v2cmsg) + ' Enter x forzare'
066300061106     c                   eval      wforzaiva = *on
066310061106     c                   Leavesr
066320061106     c                   EndIf
066330061030      * --> errore
066340061106     c                   if        xcfivaflg < *zeros and xcfivaflg <> -9
066350051118     c                   Eval      *In28 = *On
066360051202     c                   Eval      h2riga = 12
066370051118     c                   Eval      h2colo = 52
066380061030     c                   eval      v2cmsg = xcfivamsg
066390051118     c                   Leavesr
066400051118     c                   EndIf
066410061106      * se non impostato devo riportare il codice iso della partita iva che
066420061106      * mi viene passato dal pgm di controllo
066430061114     c                   if        v2ivaeu <> ivaeu
066440061106     c                   eval      v2ivaeu = ivaeu
066450061106     c                   eval      *in39 = *on
066460061106     c                   eval      *in90 = *on
066470061114     c                   endif
066480151014      * PARTITA IVA
066490151014      * --> obbligatoria (se non è gestione visite/offerte)
066500151014      * --> non è però obbligatoria se cliente Italia e nazione del
066510151014      *     sottoconto intestazione fattura estera
066520151014      * Escludo i casi previsti di NON inserimento
066530151014     c                   If        v2civa = *Blanks and Not *In06
066540151014     c                             and xcfivaflg<>9
066550151014     c                   exsr      sr_repscfsta
066560151014     c                   if        v2csta<>*blanks or w_scfsta=*blanks
066570151014     c                   Eval      *In28 = *On
066580151014     c                   Eval      h2riga = 12
066590151014     c                   Eval      h2colo = 52
066600151014     c                   Eval      v2cmsg = msg(18)
066610151014     c                   Leavesr
066620151014     c                   EndIf
066630151014     c                   EndIf
066640151014
066650151014     c* Se provengo dalle visite e il relativo potenziale non ha p.iva
066660080306     c* errore forzabile se inserito una p.iva         già presente su altro
066670080306     c* potenziale
066680151014     c                   If        v2civa <> *Blanks
066690080306     c                   if        *in06 and v2ccpo<>*zeros
066700080306     c                             and v2ivaeu<>'$$'
066710080306     c     v2ccpo        Chain(n)  Tncpo01l
066720080306     c                   if        %found(tncpo01l) and cpopiv=*blanks
066730080306     c                             and v2civa<>saviva
066740080306     c                             and v2civa<>cod_forziva
066750080306     c                   clear                   savrag
066760080307     c                   if        v2ivaeu<>*blanks
066770080306     c                   eval      codice=v2civa
066780080306     c                   else
066790080306     c                   eval      codice=v2ivait
066800080306     c                   endif
066810080306     c                   exsr      sr_PivAltroP
066820080306     c                   exsr      sr_msgiva
066830080306     c   28              leavesr
066840080306     c                   endif
066850080307     c                   endif
066860061106
066870060105      * --> controllo se c'è già un cliente con la stessa p.IVA
066880060105      *     se immissione di un nuovo codice o se conferma offerta
066890060119if  1c                   If        Not *In06 and (*In01 or *In07)
066900060221     c                             and v2ivaeu <> '$$'
066910060109     c                   If        v2ivaeu = *Blanks
066920060109     c                   Movel     v2ivait       kindiva
066930060109     c                   Else
066940060109     c                   Movel     v2civa        kindiva
066950060109     c                   EndIf
066960060105     c     kCnind        Setll     Cnind02l
066970060105      * non trovo nessun altra partita IVA esco
066980060105     c                   If        Not %Equal
066990060105     c                   Leavesr
067000060105     c                   EndIf
067010060105do  2c                   Do        *Hival
067020060105     c     kCnind        Reade     Cnind02l
067030060105      * fine file esco
067040060105     c                   If        %Eof(Cnind02l)
067050060105     c                   Leave
067060060105     c                   EndIf
067070060105      * deve avere codice cliente diverso
067080060301     c                   If        ww_indksc = v1cksc
067090060105     c                   Iter
067100060105     c                   EndIf
067110060105      * controllo lo stato del credito
067120081006      * lo faccio con una routine per emettere una window nel caso di
067130081006      * messaggio info
067140081006     c                   exsr      sr_ctrstc
067150081006     c                   if        wforzacon = *On
067160081006     c                   leavesr
067170081006     c                   endif
067180060105    2c                   EndDo
067190060105    1c                   EndIf
067200151015     c
067210151015     c                   EndIf
067220051118
067230051118     c                   EndSr
067240081006
067250081006      *------------------------------------------------------------------------*
067260081006      * CONTROLLO STATO DEL CREDITO
067270081006      *------------------------------------------------------------------------*
067280081006     c     sr_ctrstc     begsr
067290081006
067300081006     c                   Clear                   Tibs69ds
067310081006     c                   Move      ww_indksc     I69kcp
067320081006     c                   Call      'TIBS69R'
067330081006     c                   Parm                    Tibs69ds
067340081006     c                   Parm                    ds_cnaco
067350081006     c                   Parm                    ds_cnind
067360081006     c                   Parm                    ds_cnclp
067370081006     c                   Parm                    ds_fncls
067380081006     c                   Eval      wtibs69 = *On
067390081006      * errore forzabile per stato del credito trovato
067400081006      * emesso con window e f6 di conferma
067410081006     c                   If        w_clpcon <> *Blanks and wforzacon = *Off
067420090803      * decodifico stato del credito
067430090803     c                   clear                   w_ds4w
067440090803     c                   eval      kcod = '4W'
067450090803     c                   eval      kkey = w_clpcon
067460090803     c                   eval      w5ccon = %subst(w_clpcon:2:2)
067470090803     c     ktabel        chain     tabel00f
067480090803     c                   if        %found(tabel00f) and
067490090803     c                             tblflg = *blanks
067500090803     c                   eval      w_ds4w = tbluni
067510090803     c                   endIf
067520090803     c                   Eval      w5dcon = w_§4wdes
067530081006     c                   do        *hival
067540081006     c                   exfmt     ta60w05
067550081006     c   kf              leave
067560081006     c                   enddo
067570081006     c                   Eval      wforzacon = *On
067580081006     c                   EndIf
067590081006
067600081006     c                   endsr
067610051122
067620051122      *------------------------------------------------------------------------*
067630051122      * CONTROLLO BLOCCO CLIENTE
067640051122      *------------------------------------------------------------------------*
067650051122     c     Sr_Ctrblc     BegSr
067660051118
067670051122      * STATO DEL CREDITO
067680051122      * --> Ricerca
067690051122     c                   Clear                   v2dcon
067700051122     c                   Eval      wpos = %scan('?':v2ccon)
067710051122     c                   If        wpos > 0
067720051122     c                   Eval      kcod = '4W'
067730051122     c                   call      'TRUL19R'
067740051122     c                   parm                    kcod
067750051122     c                   parm                    ordina
067760051122     c                   parm                    kkey
067770051122     c                   parm                    comando
067780051124     c                   Eval      w003a = kkey
067790051124     c                   Move      w003a         v2ccon
067800051202     c                   Eval      h2riga = 17
067810051122     c                   Eval      h2colo = 28
067820051124     c                   Eval      *In90 = *On
067830051122     c                   EndIf
067840051122
067850051122      * --> Controllo
067860051122     c                   Clear                   v2dcon
067870051122     c                   Clear                   ds4w
067880051122     c                   Eval      kcod = '4W'
067890051124     c                   Move(p)   v2ccon        w003a
067900051124     c                   Eval      kkey = w003a
067910051122     c     kTabel        Chain     Tabel00f
067920051122     c                   If        Not %Found(Tabel00f) or
067930051125     c                             tblflg <> *Blanks
067940051122     c                   Eval      *In28 = *On
067950051202     c                   Eval      h2riga = 17
067960051122     c                   Eval      h2colo = 28
067970051122     c                   Eval      v2cmsg = msg(23)
067980051122     c                   Leavesr
067990051122     c                   EndIf
068000051125     c                   Eval      ds4w = tbluni
068010051122     c                   Eval      v2dcon = §4wdes
068020051122
068030051122      * Se sono in filiale devo usare solo stati del credito autorizzati e
068040051122      * senza passaggio al contenzioso
068050130325      * da 25/03/2013 abbiamo deciso che anche in sede non è modificale, solo
068060130325      * da ProJ
068070051122     c                   If        (§4wmbl = 'N' or §4wctz = 'S') and
068080130325     c                             (v2ccon <> %subst(clpcon:2:2) or *In01)
068090051122     c                   Eval      *In28 = *On
068100051202     c                   Eval      h2riga = 17
068110051122     c                   Eval      h2colo = 28
068120051122     c                   Eval      v2cmsg = msg(24)
068130051122     c                   Leavesr
068140051122     c                   EndIf
068150051122
068160051122      * --> Imposto i blocchi
068170051122      * Se sono in filiale proteggo lo stato del credito se non è modificabile
068180120928      * da p.o.
068190130325      * PER TUTTI
068200130325     c                   If        §4wmbl = 'N'
068210060220     c                   Eval      *In19 = *On
068220051122     c                   EndIf
068230120928      * Se sono in filiale proteggo il blocco pagamenti se non è modificabile
068240120928      * da p.o.
068250120928     c                   If        Not *In09 and §4wmbp = 'N'
068260120928     c                   Eval      *In81 = *On
068270120928     c                   EndIf
068280120925      * Se sono in filiale proteggo il blocco cliente se non è modificabile
068290120925      * da p.o.
068300120928     c                   If        Not *In09 and §4wmtb = 'N'
068310120925     c                   Eval      *In79 = *On
068320120925     c                   EndIf
068330121002
068340121002      * se stato del credito variato
068350121002     c                   IF        V2Ccon <> %subst(CLPcon:2:2)
068360051122      * Se il cliente non è bloccato e lo stato del credito prevede il blocco
068370051122      * lo faccio io ora in automatico
068380051122     c                   If        v2cabl = *Blanks and §4wtbl <> *Blanks
068390051122     c                   Eval      v2cabl = §4wtbl
068400130322     c                   eval      V2Cabledp = §4wtbl
068410051122     c                   EndIf
068420051122
068430051122      * Se non c'è la causale del blocco la imposto io ora in automatico
068440051122     c                   If        (v2cblc = *Blanks or v2cblc = *Zeros) and
068450051122     c                              §4wblc <> *Blanks
068460051122     c                   Eval      v2cblc = §4wblc
068470051122     c                   EndIf
068480121002     c                   ENDIF
068490051122
068500051122      * BLOCCO CLIENTE
068510130322     c                   IF        V2Cabl <> *blanks and V2Cabl <> '8'
068520130322     c                             and not *in79
068530130322     c                   Eval      *In28 = *On
068540130322     c                   Eval      h2riga = 18
068550130322     c                   Eval      h2colo = 28
068560130322     c                   Eval      V2Cmsg = 'Blocco cliente errato'
068570130322     c                   Leavesr
068580130322     c                   ENDIF
068590051122      * --> ricerca
068600051122     c                   Clear                   v2dblc
068610051122     c                   Eval      wpos = %scan('?':v2cblc)
068620051122     c                   If        wpos > 0
068630121127     c                   goto      no19
068640051122     c                   Eval      kcod = 'BI'
068650051122     c                   call      'TRUL19R'
068660051122     c                   parm                    kcod
068670051122     c                   parm                    ordina
068680051122     c                   parm                    kkey
068690051122     c                   parm                    comando
068700051122     c                   Eval      v2cblc = kkey
068710121127     c     no19          tag
068720121127     c                   clear                   kpjbu
068730121127     c                   call      'TRTBBIR'
068740121127     c                   parm                    kpjba
068750121127     c                   eval      v2cblc = %subst(kpjbu:1:3)
068760051202     c                   Eval      h2riga = 18
068770051122     c                   Eval      h2colo = 42
068780051124     c                   Eval      *In90 = *On
068790051122     c                   EndIf
068800051122
068810051122      * --> Controllo
068820051122     c                   If        v2cblc <> *Blanks
068830110323     c                   Clear                   dsbi
068840051122     c                   Clear                   v2dblc
068850051122     c                   Eval      kcod = 'BI'
068860051122     c                   Eval      kkey = v2cblc
068870051122     c     kTabel        Chain     Tabel00f
068880051122     c                   If        Not %Found(Tabel00f) or
068890051125     c                             tblflg <> *Blanks
068900051122     c                   Eval      *In28 = *On
068910051202     c                   Eval      h2riga = 18
068920051122     c                   Eval      h2colo = 42
068930051122     c                   Eval      v2cmsg = msg(25)
068940051122     c                   Leavesr
068950051122     c                   EndIf
068960110323     c                   Eval      dsbi = tbluni
068970110323     c                   Eval      v2dblc = §bides
068980121127      * causale blocco non piuù utilizzabile
068990121127     c                   IF        §BIuso = 'N'
069000121127     c                   Eval      *In28 = *On
069010121127     c                   Eval      h2riga = 18
069020121127     c                   Eval      h2colo = 42
069030121127     c                   Eval      v2cmsg = 'Causale non più utilizzabile'
069040121127     c                   Leavesr
069050121127     c                   ENDIF
069060051124     c                   EndIf
069070051122
069080051122      * Se cliente bloccato la causale blocco è obbligatoria
069090051122     c                   If        v2cabl <> *Blanks and v2cblc = *Blanks
069100130325     c                             and v2cabl <> '*'
069110051122     c                   Eval      *In28 = *On
069120051202     c                   Eval      h2riga = 18
069130051122     c                   Eval      h2colo = 42
069140051122     c                   Eval      v2cmsg = msg(26)
069150051122     c                   Leavesr
069160051122     c                   EndIf
069170051122
069180051122      * Se immessa la causale blocco il cliente deve essere bloccato
069190051122     c                   If        v2cabl = *Blanks and v2cblc <> *Blanks
069200051122     c                   Eval      *In28 = *On
069210051202     c                   Eval      h2riga = 18
069220051122     c                   Eval      h2colo = 28
069230051122     c                   Eval      v2cmsg = msg(27)
069240051122     c                   Leavesr
069250051122     c                   EndIf
069260110407
069270110407      * Se pgm richiamato per blocco cliente il blocco deve esserci
069280110407     c                   If        $Blocco and V2cabl = *blanks
069290110407     c                   Eval      *In28 = *On
069300110407     c                   Eval      h2riga = 18
069310110407     c                   Eval      h2colo = 28
069320110407     c                   Eval      v2cmsg = 'Immettere il blocco cliente'
069330110407     c                   Leavesr
069340110407     c                   EndIf
069350051122
069360051122     c                   EndSr
069370090616
069380090616      *------------------------------------------------------------------------*
069390090616      * CONTROLLO SOLLECITO
069400090616      *------------------------------------------------------------------------*
069410090616     c     sr_ctrsol     begsr
069420090616
069430090616     c                   reset                   trulsoli1
069440090616     c                   eval      trulsoli1.kut = codut
069450090616     c                   eval      trulsoli1.kcc = v1ckcc
069460090616     c                   eval      trulsoli1.ksc = v1cksc
069470090616     c                   evalr     trulsoli1.con = v2ccon
069480090616     c                   eval      rqsdatasize = %size(trulsoli1)
069490090616     c                   eval      rpydatasize = %size(trulsolo1)
069500090616     c                   call      'TRULSOLR'
069510090616     c                   parm      *blanks       rqsopcode
069520090616     c                   parm      *zeros        rpyesito
069530090616     c                   parm      'TRULSOLI1'   rqsformatname
069540090616     c                   parm                    trulsoli1
069550090616     c                   parm                    rqsdatasize
069560090616     c                   parm      'TRULSOLO1'   rpyformatname
069570090616     c                   parm                    trulsolo1
069580090616     c                   parm                    rpydatasize
069590090617
069600090617      * Immissione
069610090617     c                   if        *in01
069620090617     c                   if        rpyesito = *zeros
069630090617     c                   eval      v2csol = trulsolo1.sol
069640090617     c                   else
069650090617     c                   eval      v2csol = 'S'
069660090617     c                   endif
069670090617     c                   endif
069680090617
069690090617      * Modifica
069700090617     c                   if        rpyesito = *zeros and not *in01 and
069710090617     c                             v2csol <> trulsolo1.sol and
069720090617     c                             trulsolo1.solmod = 'N'
069730090617     c                   Eval      *In28 = *On
069740111115     c                   Eval      h2riga = 19
069750090617     c                   Eval      h2colo = 28
069760090617     c                   if        trulsolo1.sol = 'S'
069770090617     c                   Eval      v2cmsg = msg(83)
069780090617     c                   Leavesr
069790090617     c                   else
069800090617     c                   Eval      v2cmsg = msg(84)
069810090617     c                   Leavesr
069820090617     c                   endif
069830090617     c                   endif
069840090616
069850090616     c                   endsr
069860051122
069870051118      *------------------------------------------------------------------------*
069880051118      * CONTROLLO CODICE COMMERCIALE
069890051118      *------------------------------------------------------------------------*
069900051118     c     Sr_Ctrage     BegSr
069910051118
069920051118      * Ricerca
069930051130     c                   Clear                   v3dage
069940051130     c                   Eval      wpos = %scan('?':v3cage)
069950051118     c                   If        wpos > 0
069960051130     c                   Eval      v3cage = *All'0'
069970130808     c                   clear                   TRMK43ds
069980130806     c  n06              movel     v1cksc        iMK43fil
069990130806     c   06              movel     ta60cli       iMK43fil
070000130806     c                   call      'TRMK43R'
070010130806     c                   parm                    kpjba
070020130806     c                   parm                    TRMK43ds
070030130806     c                   if        oMK43err = *off  and  oMK43fxx = *blank
070040130806     c                   movel     oMK43cmm      v3cage
070050130806     c                   eval      v3dage = oMK43des
070060130806     c                   endif
070070051130     c                   Eval      h3riga = 07
070080051130     c                   Eval      h3colo = 28
070090051124     c                   Eval      *In90 = *On
070100051118     c                   EndIf
070110051122
070120051118      * Controllo (obbligatorio)
070130051130     c                   Clear                   v3dage
070140130806     c                   If        %check(digits:V3Cage) > 0
070150130806     c                   eval      *In28 = *On
070160130806     c                   eval      h3riga = 07
070170130806     c                   eval      h3colo = 28
070180130806     c                   eval      V3Cmsg = Msg(20)
070190130806     c                   leavesr
070200130806     c                   EndIf
070210130806     c                   move      V3Cage        CMMcod
070220130806     c     CMMcod        chain     AZCMM000
070230130806     c                   If        Not %Found(AZCMM01L)
070240051122     c                   Eval      *In28 = *On
070250051130     c                   Eval      h3riga = 07
070260051130     c                   Eval      h3colo = 28
070270051130     c                   Eval      v3cmsg = msg(20)
070280051122     c                   Leavesr
070290051122     c                   EndIf
070300130806     c                   eval      V3Dage = CMMdes
070310051122
070320051122      * Codice commerciale vari valido solo per 8888 e 9999
070330051130     c                   Move      v3cage        w0040
070340130806     c                   IF        CMMpar = '1' and wksc04 <> 8888
070350130806     c                                          and wksc04 <> 9999
070360051122     c                   Eval      *In28 = *On
070370051130     c                   Eval      h3riga = 07
070380051130     c                   Eval      h3colo = 28
070390051130     c                   Eval      v3cmsg = msg(20)
070400051122     c                   Leavesr
070410051122     c                   EndIf
070420110216
070430110216      /free
070440111123       //?Commerciale "valido" data inizio e fine attività
070450130806         IF  CMMdtIni > dataoggi  or
070460130806             CMMdtFin < dataoggi;
070470111123           *in28 = *on;
070480111123           H3riga = 07;
070490111123           H3colo = 28;
070500111123           V3Cmsg = msg(20);
070510111123           leavesr;
070520111123         ENDIF;
070530111123
070540110216       //?Commerciale 'inattivi' possibile solo se
070550110217       //?immissione
070560110216       //?utente EDP
070570110216       //?cliente incassi da attribuire --> 0001 x 1°livello
070580110216       //?                              --> 1000 x 2°livello
070590110217       //?oppure se
070600110217       //?manutenzione
070610110217       //?NO anagrafica provvisoria
070620110217       //?cliente incassi da attribuire = a cod. in tab Y4O
070630130806         IF  CMMpar = '2';
070640110217           SELECT;
070650110217             WHEN  *in02 and not *in06 and %editc(V1Cksc:'X') = wCodIncAtt;
070660160429             WHEN  *in01 and *in20 and
070670121121                   ((sav_livello = '1' and wksc04 = 0001) or
070680121121                    (sav_livello = '2' and wksc04 = 1000) or
070690121121                    (sav_livello = *blanks and wCodIncAtt = *blanks and
070700121121                    (wksc04 = 1000  or wksc04 = 0001)));
070710110217             OTHER;
070720110216              *in28 = *on;
070730110216              H3riga = 07;
070740110216              H3colo = 28;
070750110216              V3Cmsg = msg(20);
070760110216              leavesr;
070770110217           ENDSL;
070780110217         ENDIF;
070790110216      /end-free
070800051122
070810051122      * P.o. del commerciale congruente con p.o. del cliente
070820051221      * solo se non è gestione anagrafica provvisoria di un nuovo cliente
070830051221     c                   If        Not *In06 or ta60cli > *Zeros
070840051122     c  n06              Movel     v1cksc        w0030
070850051122     c   06              Movel     ta60cli       w0030
070860051130     c                   Movel     v3cage        wpoage
070870051122     c                   If        w0030 <> wpoage
070880051122     c                   Eval      *In28 = *On
070890051130     c                   Eval      h3riga = 07
070900051130     c                   Eval      h3colo = 28
070910051130     c                   Eval      v3cmsg = msg(21)
070920051122     c                   Leavesr
070930051122     c                   EndIf
070940051221     c                   EndIf
070950051122
070960051122      * Se commerciale xxx0999 (cliente inattivo) il cliente va bloccato
070970110216      * salto il controllo
070980110216      * il commerciale 'inattivi' non va usato
070990110216     c                   leavesr
071000051130     c                   Move      v3cage        w0040
071010051122     c                   If        w0040 = 999 and v2cabl = *Blanks
071020051122     c                   Eval      *In28 = *On
071030051130     c                   Eval      v3cmsg = msg(22)
071040051122     c                   Leavesr
071050051122     c                   EndIf
071060051118
071070051118     c                   EndSr
071080051122
071090051122      *------------------------------------------------------------------------*
071100051122      * CONTROLLO CODICE IMPORTANZA CLIENTE
071110051122      *------------------------------------------------------------------------*
071120051122     c     Sr_Ctrclv     BegSr
071130100301
071140100301     c                   Clear                   v3dclv
071150100301
071160100301      * se anagrafica provvisoria da trattativa
071170100301      * non è obbligatorio inserire il codice, se a blank vado a fine controlli
071180100806     c                   if        *in06 and v3cclv = *blanks
071190100301     c                   leavesr
071200100301     c                   endif
071210051122
071220051122      * Ricerca
071230051130     c                   Eval      wpos = %scan('?':v3cclv)
071240051122     c                   If        wpos > 0
071250051122     c                   Eval      kcod = 'IC'
071260051122     c                   call      'TRUL19R'
071270051122     c                   parm                    kcod
071280051122     c                   parm                    ordina
071290051122     c                   parm                    kkey
071300051122     c                   parm                    comando
071310051130     c                   Eval      v3cclv = kkey
071320051130     c                   Eval      h3riga = 08
071330051130     c                   Eval      h3colo = 28
071340051124     c                   Eval      *In90 = *On
071350051122     c                   EndIf
071360051122
071370051124      * --> Controllo (obbligatorio)
071380051130     c                   If        v3cclv = *Blanks
071390051124     c                   Eval      *In28 = *On
071400051130     c                   Eval      h3riga = 08
071410051130     c                   Eval      h3colo = 28
071420051130     c                   Eval      v3cmsg = msg(28)
071430051124     c                   Leavesr
071440051124     c                   EndIf
071450051124
071460051130     c                   Clear                   v3dclv
071470051122     c                   Clear                   dsic
071480051122     c                   Eval      kcod = 'IC'
071490051130     c                   Eval      kkey = v3cclv
071500051122     c     kTabel        Chain     Tabel00f
071510051122     c                   If        Not %Found(Tabel00f) or
071520051125     c                             tblflg <> *Blanks
071530051122     c                   Eval      *In28 = *On
071540051130     c                   Eval      h3riga = 08
071550051130     c                   Eval      h3colo = 28
071560051130     c                   Eval      v3cmsg = msg(29)
071570051122     c                   Leavesr
071580051122     c                   EndIf
071590051125     c                   Eval      dsic = tbluni
071600051130     c                   Eval      v3dclv = §sicde
071610051122
071620051122     c                   EndSr
071630051122
071640051122      *------------------------------------------------------------------------*
071650051122      * CONTROLLO CODICE ISTAT
071660051122      *------------------------------------------------------------------------*
071670051122     c     Sr_Ctritc     BegSr
071680051122
071690051122      * Ricerca
071700051130     c                   Clear                   v3ditc
071710051130     c                   Eval      wpos = %scan('?':v3citc)
071720051122     c                   If        wpos > 0
071730051122     c                   Eval      kcod = '1L'
071740051122     c                   call      'TRUL19R'
071750051122     c                   parm                    kcod
071760051122     c                   parm                    ordina
071770051122     c                   parm                    kkey
071780051122     c                   parm                    comando
071790051130     c                   Eval      v3citc = kkey
071800051130     c                   Eval      h3riga = 09
071810051130     c                   Eval      h3colo = 28
071820051124     c                   Eval      *In90 = *On
071830051122     c                   EndIf
071840051122
071850051122      * --> Controllo (obbligatorio)
071860051130     c                   If        v3citc = *Blanks or v3citc = *Zeros
071870051122     c                   Eval      *In28 = *On
071880051130     c                   Eval      h3riga = 09
071890051130     c                   Eval      h3colo = 28
071900051130     c                   Eval      v3cmsg = msg(30)
071910051122     c                   Leavesr
071920051122     c                   EndIf
071930051122
071940051130     c                   Clear                   v3ditc
071950051122     c                   Clear                   ds1l
071960051124     c                   Eval      kcod = '1L'
071970051130     c                   Eval      kkey = v3citc
071980051122     c     kTabel        Chain     Tabel00f
071990051122     c                   If        Not %Found(Tabel00f) or
072000051125     c                             tblflg <> *Blanks
072010051122     c                   Eval      *In28 = *On
072020051130     c                   Eval      h3riga = 09
072030051130     c                   Eval      h3colo = 28
072040051130     c                   Eval      v3cmsg = msg(31)
072050051122     c                   Leavesr
072060051122     c                   EndIf
072070051125     c                   Eval      ds1l = tbluni
072080051130     c                   Eval      v3ditc = §1ldes
072090051122
072100051122     c                   EndSr
072110140722
072120140722      /free
072130140722
072140140722       //--------------------------------------------------------------
072150140722       //?Controllo Note 02/03       (News/Nominativo),                ?
072160140722       //?               05/06/T5/I5 (Responsabile Trasporti),         ?
072170140722       //?               07/08/T7/I7 (Responsabile Logistica).         ?
072180140722       //--------------------------------------------------------------
072190140722       BEGSR  sr_CtrNote;
072200140722
072210140722         // -?Se NON selezionate: esco?
072220140722         select;
072230140722           when  wRGR  = 'RN'  and  V3Cnt02 = *blank;
072240140722             leavesr;
072250140722           when  wRGR  = 'RT'  and  V3Cnt05 = *blank;
072260140722             leavesr;
072270140722           when  wRGR  = 'RL'  and  V3Cnt07 = *blank;
072280140722             leavesr;
072290140722         endsl;
072300140722
072310140722         // -?Se sono in interrogazione non posso interrogare?
072320140722         //  ?   se non ho dati?
072330140722         if  *in05  and  ( (wRGR = 'RN'  and  Not *in24)  or
072340140722                           (wRGR = 'RT'  and  Not *in26)  or
072350140722                           (wRGR = 'RL'  and  Not *in31) );
072360140722           *in28 = *on;
072370140722           select;
072380140722             when  wRGR = 'RN';
072390140722               H3riga = 15;
072400140722             when  wRGR = 'RT';
072410140722               H3riga = 17;
072420140722             when  wRGR = 'RL';
072430140722               H3riga = 19;
072440140722           endsl;
072450140722           H3colo = 35;
072460140722           V3Cmsg = Msg(52);
072470140722           leavesr;
072480140722         endif;
072490140722
072500140722         // -?Imposto se interrogazione o gestione?
072510140722         if  Not *in05;
072520140722           wRich = 'M';
072530140722         else;
072540140722           wRich = 'V';
072550140722         endif;
072560140722         wRag  = V3Crag;
072570140722         clear  wTnt;
072580140722         //wRGR  = 'RN'       ?(già impostato)?
072590140722
072600140722         exsr  sr_CallTNTA12;
072610140722
072620140722         if  TA12err <> *blank;
072630140722           *in28 = *on;
072640140722           select;
072650140722             when  wRGR = 'RN';
072660140722               H3riga = 15;
072670140722             when  wRGR = 'RT';
072680140722               H3riga = 17;
072690140722             when  wRGR = 'RL';
072700140722               H3riga = 19;
072710140722           endsl;
072720140722           H3colo = 35;
072730140722           V3Cmsg = TA12msg;
072740140722           leavesr;
072750140722         endif;
072760140722
072770140722         // -?Controllo se ora c'è o non c'è più alcuna nota?
072780140722         kNtcNk1 = %editc( DUTkci : 'X' );
072790140722         if  *in70  or  Not *in06;
072800140722           kNtcApl = 'C';
072810140722           kNtcNk1 = %trim(kNtcNk1) + %editc( V1Cksc : 'X' );
072820140722         else;
072830140722           kNtcApl = 'T';
072840140722           kNtcNk1 = %trim(kNtcNk1) + %editc( V1Cnrv : 'X' );
072850140722         endif;
072860140722         clear  kNtcNk2;
072870140722         select;
072880140722           // -?Note 02/03?
072890140722           when  wRGR  = 'RN';
072900140722             *in24 = *off;
072910140722             exsr  sr_CarNoteRN;
072920140722           // -?Note 05/06/T5/I5?
072930140722           when  wRGR  = 'RT';
072940140722             *in26 = *off;
072950140722             exsr  sr_CarNoteRT;
072960140722           // -?Note 07/08/T7/I7?
072970140722           when  wRGR  = 'RL';
072980140722             *in31 = *off;
072990140722             exsr  sr_CarNoteRL;
073000140722         endsl;
073010140722
073020140722         // -?Uscita?
073030140722         select;
073040140722           when  wRGR  = 'RN';
073050140722             clear  V3Cnt02;
073060140722             H3riga = 15;
073070140722           when  wRGR  = 'RT';
073080140722             clear  V3Cnt05;
073090140722             H3riga = 17;
073100140722           when  wRGR  = 'RL';
073110140722             clear  V3Cnt07;
073120140722             H3riga = 19;
073130140722         endsl;
073140140722         H3colo = 35;
073150140722
073160140722         *in90 = *on;
073170140722
073180140722       ENDSR;
073190140722
073200140722       //--------------------------------------------------------------
073210140722       //?Visualizzazione Contatti/Rubrica del cliente.                ?
073220140722       //--------------------------------------------------------------
073230140722       BEGSR  sr_CallTNTA12;
073240140722
073250140722         clear  TNTA12ds;
073260140722
073270140722         // -?Impostazione parametri?
073280140722         TA12ric  = wRich;
073290140722         if  *in70  or  Not *in06;
073300140722           TA12apl  = 'C';
073310140722           TA12ksc  = V1Cksc;
073320140722         else;
073330140722           TA12apl  = 'T';
073340140722           TA12nrv  = V1Cnrv;
073350140722         endif;
073360140722         TA12rag  = wRag;
073370140722         TA12tnt  = wTnt;
073380140722         TA12gcli = '1';
073390140722         if  *in01   or
073400140722            (Not *in01  and  Not *in05  and
073410140722             %parms() > 1    and
073420140722             TA60icli = '1');
073430140722           TA12icli = '1';
073440140722           TA12cmm  = %int(V3Cage);
073450140722         endif;
073460140722         TA12rgr  = wRGR;
073470140722
073480140722         // -?Richiamo *pgm TNTA12R?
073490140722         tnta12r ( KPJBA : TNTA12ds );
073500140722
073510140722       ENDSR;
073520140722
073530140722      /end-free
073540051125
073550051125      *------------------------------------------------------------------------*
073560051125      * CONTROLLO CODICE SOTTOCONTO FATTURAZIONE
073570051125      *------------------------------------------------------------------------*
073580051125     c     Sr_Ctrscf     BegSr
073590100127
073600100127      * gestisco il '?'
073610100729     c                   IF        %scan('?':v4cscf) > 0
073620100312      * e se c'è la partita IVA e non è $$
073630100312     c                             and v2civa <> *blanks and
073640100312     c                             v2ivaeu <> '$$'
073650100128     c                   exsr      sr_ta40
073660100127     c                   eval      h4riga = 07
073670100127     c                   eval      h4colo = 36
073680100127     c                   eval      *in90 = *on
073690100312     c                   leavesr
073700100127     c                   ENDIF
073710051125
073720051221      * Obbligatorio
073730100128     c                   If        (v4cscf = *Zeros or v4cscf = *blanks)
073740051221      * se non sono in gestione anagrafica provvisoria
073750051221     c                             and Not *In06
073760051125     c                   Eval      *In28 = *On
073770051130     c                   Eval      h4riga = 07
073780051130     c                   Eval      h4colo = 36
073790051130     c                   Eval      v4cmsg = msg(33)
073800051125     c                   Leavesr
073810051125     c                   EndIf
073820100127
073830100127      * deve essere numerico
073840100127     c                   IF        %check(digits:V4cscf) > 0
073850100127     c                   eval      *in28 = *on
073860100127     c                   eval      h4riga = 07
073870100127     c                   eval      h4colo = 36
073880100127     c                   eval      v4cmsg = msg(35)
073890100127     c                   leavesr
073900100127     c                   ENDIF
073910100316
073920100316      * se sono in immissione (non da trattativa)
073930100316      * non controllo esistenza del codice sottoconto fattura se = al
073940100316      * codice cliente che sto inserendo
073950170516      // non faccio questo controllo se cliente LOGISTICA
073960170516        IF  not ClienteLog;
073970100316     c                   if        *in01 and not *in06 and
073980100316     c                             %dec(v4cscf:7:0) = v1cksc
073990100316     c                   eval      v4dscf = v2crag
074000100316     c                   leavesr
074010100316     c                   endif
074020170516       ENDIF;
074030051125
074040051125      * Controllo se modificato il codice
074050170117      * o se convalida trattativa
074060100127     c                   If        v4cscf <> savscf
074070170117     c                             or *in07
074080051130     c                   Clear                   v4dscf
074090051125      * Controllo
074100051125     c                   Clear                   Tibs69ds
074110051130     c                   Move      v4cscf        I69kac
074120161129     c                   Move      v4cscf        I69kcp
074130051125     c                   Call      'TIBS69R'
074140051125     c                   Parm                    Tibs69ds
074150051125     c                   Parm                    ds_cnaco
074160051125     c                   Parm                    ds_cnind
074170051125     c                   Parm                    ds_cnclp
074180051125     c                   Parm                    ds_fncls
074190051125     c                   Eval      wtibs69 = *On
074200051125
074210051125      * Il sottoconto deve esistere
074220051125     c                   If        o69err <> *Blanks
074230051125     c                   Eval      *In28 = *On
074240051130     c                   Eval      h4riga = 07
074250051130     c                   Eval      h4colo = 36
074260051130     c                   Eval      v4cmsg = msg(35)
074270051125     c                   Leavesr
074280051125     c                   EndIf
074290051125
074300051125      * Non deve essere annullato
074310051125     c                   If        w_acoflg <> *Blanks
074320051125     c                   Eval      *In28 = *On
074330051130     c                   Eval      h4riga = 07
074340051130     c                   Eval      h4colo = 36
074350051130     c                   Eval      v4cmsg = msg(36)
074360051125     c                   Leavesr
074370051125     c                   EndIf
074380051125
074390051125      * Non si può inserire un sottoconto 8888 o 9999
074400070704      * E' possibile solo per clienti 8888 e 9999
074410070704     c                   Move      v4cscf        w0040
074420070704     c                   move      v1cksc        w0040ksc
074430070704     c                   if        *in06='1'
074440070704     c                             or (w0040ksc=8888 and w0040<>8888)
074450070704     c                             or (w0040ksc=9999 and w0040<>9999)
074460070704     c                             or (w0040ksc<>8888 and w0040ksc<>9999)
074470051125     c                   If        w0040 = 8888 or w0040 = 9999
074480051125     c                   Eval      *In28 = *On
074490051130     c                   Eval      h4riga = 07
074500051130     c                   Eval      h4colo = 36
074510051130     c                   Eval      v4cmsg = msg(34)
074520051125     c                   Leavesr
074530051125     c                   EndIf
074540070704     c                   EndIf
074550161130      /free
074560161130       //?Non posso immettere un codice sottoconto fattura in contenzioso
074570170117        IF  %dec(v4cscf:7:0) <> v1cksc;
074580170117          clear TIBS69DS;
074590170117          I69kcp = %dec(V4Cscf:7:0);
074600170117          tibs69r (TIBS69DS:ds_CNACO:ds_CNIND:ds_CNCLP:ds_FNCLS);
074610170117          wtibs69 = *on;
074620170117          IF  w_CLPcon <> *blanks;
074630170117            clear w_ds4W;
074640170117            kcod = '4W';
074650170117            kkey = w_CLPcon;
074660170117            chain (codut:kcod:kkey) TABEL00F;
074670170117            IF  %found(TABEL00F) and TBLflg = *blanks;
074680170117              w_ds4W = TBLuni;
074690170117            ENDIF;
074700170117            IF  w_§4Wtip <> *blanks;
074710170117              *in28 = *on;
074720170117              H4riga = 07;
074730170117              H4colo = 36;
074740170117              V4Cmsg = msg(73);
074750170117              leavesr;
074760170117            ENDIF;
074770170117          ENDIF;
074780161130        ENDIF;
074790161130      /end-free
074800051125
074810051125      * Cerco il network del codice
074820051130     c                   Movel     v4cscf        w0030
074830051125     c                   Clear                   og143
074840051125     c     w0030         Chain     Azorg01l
074850051125     c                   If        %Found(Azorg01l)
074860051125     c                   Eval      og143 = orgde3
074870051125     c                   EndIf
074880120806      * Se non trovo il network e sono in anafrafica provvisoria vado avanti
074890120806     c                   If        not %found(AZORG01L) and not *in06
074900120806     c                             and Not *In06
074910120806     c                   Eval      *In28 = *On
074920120806     c                   Eval      h4riga = 07
074930120806     c                   Eval      h4colo = 36
074940120806     c                   Eval      v4cmsg = msg(35)
074950120806     c                   Leavesr
074960120806     c                   ENDIF
074970081119     c                   eval      savntwscf = §ogntw
074980170516
074990170516       // Per i clienti LOG come sottoconto intestazione fattura accetto
075000170516       // solo codici di spedizione BRT
075010170516         IF  ClienteLog and §OGntw = 'LOG';
075020170516           *in28 = *on;
075030170516           H4riga = 07;
075040170516           H4colo = 36;
075050170516           V4Cmsg = msg(34);
075060170516           leavesr;
075070170516         ENDIF;
075080160905
075090160905      * Se non sono autorizzato non posso utilizzare lienti con ntw LOG
075100160905     c                   IF         §UTEKscLog <> 'S' and
075110160905     c                              §OGntw = 'LOG'
075120160905     c                   Eval      *In28 = *On
075130160905     c                   Eval      h4riga = 07
075140160905     c                   Eval      h4colo = 36
075150160905     c                   Eval      v4cmsg = msg(34)
075160160905     c                   Leavesr
075170160905     c                   ENDIF
075180051125
075190051125      * Se sono in filiale non posso inserire codici con ntw LOG o XXX
075200051125     c                   If        Not *In09 and
075210160905     c                             §ogntw = 'XXX'
075220051125     c                   Eval      *In28 = *On
075230051130     c                   Eval      h4riga = 07
075240051130     c                   Eval      h4colo = 36
075250051130     c                   Eval      v4cmsg = msg(34)
075260051125     c                   Leavesr
075270051125     c                   EndIf
075280051220
075290051220      * Non posso modificare un sottoconto fattura con p.o. 046
075300051220     c                   Movel     savscf        w0030
075310051220     c                   If        w0030 = 046
075320051220     c                   Eval      *In28 = *On
075330051220     c                   Eval      h4riga = 07
075340051220     c                   Eval      h4colo = 36
075350051220     c                   Eval      v4cmsg = msg(71)
075360051220     c                   Leavesr
075370051220     c                   EndIf
075380051220
075390081119      * Non posso inserire un sottoconto fattura con ntw LOG se ksc non
075400081119      * ha ntw LOG
075410081119     c                   Movel     v1cksc        w0030
075420081119     c                   Clear                   og143
075430081119     c     w0030         Chain     Azorg01l
075440081119     c                   If        %Found(Azorg01l)
075450081119     c                   Eval      og143 = orgde3
075460081119     c                   EndIf
075470081119     c                   if        §ogntw <> 'LOG' and savntwscf = 'LOG'
075480081119     c                   Eval      *In28 = *On
075490081119     c                   Eval      h4riga = 07
075500081119     c                   Eval      h4colo = 36
075510081119     c                   Eval      v4cmsg = msg(34)
075520081119     c                   Leavesr
075530081119     c                   EndIf
075540100127
075550100127      * devo ricontrollare la partita iva e il codice fiscale
075560100127      * perchè i controlli di questi 2 dati sono legati anche al codice
075570100127      * fatturazione
075580100127     c                   exsr      sr_ctrCDF
075590100127      * se torna errore avviso
075600100127     c                   IF        *in28
075610100127     c                   eval      v4cmsg = 'Messaggi in sospeso su -
075620100127     c                             altre videate.'
075630100127     c                   leavesr
075640100127     c                   ENDIF
075650100127     c                   exsr      sr_ctrIVA
075660100127      * se torna errore avviso
075670100127     c                   IF        *in28
075680100127     c                   eval      v4cmsg = 'Messaggi in sospeso su -
075690100127     c                             altre videate.'
075700100127     c                   leavesr
075710100127     c                   ENDIF
075720051125
075730051125      * Decodifico
075740051130     c                   Eval      v4dscf = w_acorag
075750051130     c                   Eval      savscf = v4cscf
075760051128
075770051125     c                   EndIf
075780051125
075790051125     c                   EndSr
075800051128
075810051128      *------------------------------------------------------------------------*
075820051128      * CONTROLLO FLAG FATTURA UNIFICATA
075830051128      *------------------------------------------------------------------------*
075840051128     c     Sr_Ctruni     BegSr
075850100127
075860100127     c                   move      v4cscf        w0070
075870051128
075880051128      * Se il codice sottoconto fatturazione è diverso dal codice cliente e
075890051128      * il flag di unificazione fattura non è stato impostato emetto un msg. inf
075900100127     c                   If        w0070  <> v1cksc and v4cuni = *Blanks and
075910100127     c                             wfscf = *Blanks and w0070  <> *Zeros
075920051128      * e non è da offerta
075930051128     c                             and not *In06
075940051128     c                   Eval      *In28 = *On
075950051130     c                   Eval      h4riga = 08
075960051130     c                   Eval      h4colo = 36
075970051130     c                   Eval      v4cmsg = msg(45)
075980051128     c                   Eval      wfscf = '1'
075990051128     c                   Leavesr
076000051128     c                   EndIf
076010051128
076020051128      * se modificato flag fattura unificata msg info x controllare anche il
076030051128      * flag nel sottoconto fattura
076040051128      * se immissione messaggio solo se codice cliente diverso da sottoconto fat
076050051128      * se manutenzione diversifico i messaggi
076060051130     c                   If        v4cuni <> savuni
076070051130     c                   Eval      savuni = v4cuni
076080051128     c                   Select
076090100127     c                   When      w0070  <> v1cksc
076100051128     c                   Eval      *In28 = *On
076110051130     c                   Eval      h4riga = 08
076120051130     c                   Eval      h4colo = 36
076130051130     c                   Eval      v4cmsg = msg(46)
076140051128     c                   Leavesr
076150100127     c                   When      w0070  = v1cksc and Not *In01
076160051128     c                             and Not *In07
076170051128     c                   Eval      *In28 = *On
076180051130     c                   Eval      h4riga = 08
076190051130     c                   Eval      h4colo = 36
076200051130     c                   Eval      v4cmsg = msg(47)
076210051128     c                   Leavesr
076220051128     c                   EndSl
076230051128     c                   EndIf
076240051128
076250051128     c                   EndSr
076260051125
076270051125      *------------------------------------------------------------------------*
076280051125      * CONTROLLO TIPO FATTURA
076290051125      *------------------------------------------------------------------------*
076300051125     c     Sr_Ctrtft     BegSr
076310051125
076320051125      * Ricerca
076330051130     c                   Clear                   v4dtft
076340051130     c                   If        v4ctft = '?'
076350051125     c                   Clear                   tibs02ds
076360051125     c                   Eval      t02mod = 'R'
076370051125     c                   Eval      t02sif = knsif
076380051125     c                   Eval      t02cod = 'TFT'
076390051125     c                   Call      'TIBS02R'
076400051125     c                   Parm                    kpjba
076410051125     c                   Parm                    tibs02ds
076420051125     c                   If        t02err = *Blanks
076430051130     c                   Eval      v4ctft = t02ke1
076440051125     c                   Eval      dtft = t02uni
076450051130     c                   Eval      v4dtft = §tftdes
076460051130     c                   Eval      h4riga = 09
076470051130     c                   Eval      h4colo = 36
076480051125     c                   Eval      *In90 = *On
076490051125     c                   Leavesr
076500051125     c                   EndIf
076510051128     c                   EndIf
076520051125
076530051125      * --> Controllo (obbligatorio in tabella non esiste il ' ')
076540051125     c                   Clear                   Tibs02ds
076550051125     c                   Eval      t02mod = 'C'
076560051125     c                   Eval      t02sif = knsif
076570051125     c                   Eval      t02cod = 'TFT'
076580051130     c                   Eval      t02ke1 = v4ctft
076590051125     c                   Call      'TIBS02R'
076600051125     c                   Parm                    kpjba
076610051125     c                   Parm                    Tibs02ds
076620051125     c                   If        t02err <> *Blanks
076630051125     c                   Eval      *In28 = *On
076640051130     c                   Eval      h4riga = 09
076650051130     c                   Eval      h4colo = 36
076660051130     c                   Eval      v4cmsg = msg(37)
076670051125     c                   Leavesr
076680051125     c                   EndIf
076690051125     c                   Eval      dtft = t02uni
076700051130     c                   Eval      v4dtft = §tftdes
076710051125
076720051125      * Controllo uso sede/p.o.
076730051125      * solo se variato
076740051130     c                   If        v4ctft <> savtft
076750051125     c                   Select
076760051125     c                   When      Not *In09 and §tftuti = 'S'
076770051125     c                   Eval      *In28 = *On
076780051130     c                   Eval      h4riga = 09
076790051130     c                   Eval      h4colo = 36
076800051130     c                   Eval      v4cmsg = msg(38)
076810051130     c                   Eval      v4cmsg = %trim(v4cmsg) + ' ' +
076820051125     c                             'filiale'
076830051125     c                   Leavesr
076840051125     c                   When      *In09 and §tftuti = 'F'
076850051125     c                   Eval      *In28 = *On
076860051130     c                   Eval      h4riga = 09
076870051130     c                   Eval      h4colo = 36
076880051130     c                   Eval      v4cmsg = msg(38)
076890051130     c                   Eval      v4cmsg = %trim(v4cmsg) + ' ' +
076900051125     c                             'sede'
076910051125     c                   Leavesr
076920051125     c                   EndSl
076930051125     c                   EndIf
076940090417
076950090417     c                   clear                   conta
076960090417
076970090417      * se non è anagrafica provvisoria
076980090417      * se tipo fattura è distinta verifico se il codice è un sottoconto
076990090417      * fattura e se i codici a lui collegati hanno la fatturazione
077000090417      * settimanale, in questo caso devo dare errore
077010090417     c                   if        not *in06 and
077020100127     c                             %editc(v1cksc:'X') = v4cscf
077030100127     c                             and v4ctft = '1'
077040090417     c/exec sql
077050090417     c+ select count(*) into :conta from cnclp00f where
077060090417     c+ clpkcc = 151 and clpscf = :v1cksc and clpfft = 1
077070090417     c/end-exec
077080090417      * se ho trovato almeno un rcd vuol dire che uno dei figli
077090090417      * legati al sottoconto fatturazione che sto modificando ha
077100090417      * la frequenza settimanale
077110090417     c                   if        conta > *zeros
077120090417     c                   eval      *in28 = *on
077130090417     c                   eval      h4riga = 09
077140090417     c                   eval      h4colo = 36
077150090417     c                   eval      v4cmsg = msg(82)
077160090417     c                   leavesr
077170090417     c                   endif
077180090417     c                   endif
077190051125
077200051125     c                   EndSr
077210051125
077220051125      *------------------------------------------------------------------------*
077230051125      * CONTROLLO FREQUENZA FATTURA
077240051125      *------------------------------------------------------------------------*
077250051125     c     Sr_Ctrfft     BegSr
077260051125
077270051125      * Ricerca
077280051130     c                   Clear                   v4dfft
077290051130     c                   If        v4cfft = '?'
077300051125     c                   Eval      kcod = 'FF'
077310051125     c                   call      'TRUL19R'
077320051125     c                   parm                    kcod
077330051125     c                   parm                    ordina
077340051125     c                   parm                    kkey
077350051125     c                   parm                    comando
077360051130     c                   Eval      v4cfft = kkey
077370051130     c                   Eval      h4riga = 10
077380051130     c                   Eval      h4colo = 36
077390051125     c                   Eval      *In90 = *On
077400051125     c                   EndIf
077410051125
077420051125      * --> Controllo (obbligatorio)
077430051130     c                   If        v4cfft = *Blanks
077440051125     c                   Eval      *In28 = *On
077450051130     c                   Eval      h4riga = 10
077460051130     c                   Eval      h4colo = 36
077470051130     c                   Eval      v4cmsg = msg(39)
077480051125     c                   Leavesr
077490051125     c                   EndIf
077500051125
077510051130     c                   Clear                   v4dfft
077520051125     c                   Eval      kcod = 'FF'
077530051130     c                   Eval      kkey = v4cfft
077540051125     c     kTabel        Chain     Tabel00f
077550051125     c                   If        Not %Found(Tabel00f) or
077560051125     c                             tblflg <> *Blanks
077570051125     c                   Eval      *In28 = *On
077580051130     c                   Eval      h4riga = 10
077590051130     c                   Eval      h4colo = 36
077600051130     c                   Eval      v4cmsg = msg(40)
077610051125     c                   Leavesr
077620051125     c                   EndIf
077630051125     c                   Eval      dsff = tbluni
077640051130     c                   Eval      v4dfft = §ffdes
077650051125
077660051125      * Se tipo fattura '1' non è ammessa la frequenza fattura <> dal mensile
077670051220     c                   If        v4ctft = '1' and v4cfft <> '4'
077680051125     c                   Eval      *In28 = *On
077690051130     c                   Eval      h4riga = 10
077700051130     c                   Eval      h4colo = 36
077710051130     c                   Eval      v4cmsg = msg(41)
077720051125     c                   Leavesr
077730051125     c                   EndIf
077740051125
077750120613      * La frequenza fattura settimanale (1) è ammessa solo se
077760120613      * utente abilitato
077770120613     c                   If        v4cfft = '1' and
077780120613     c                             §UTEclpfft <> 'S' and
077790051130     c                             v4cfft <> savfft
077800051125     c                   Eval      *In28 = *On
077810051130     c                   Eval      h4riga = 10
077820051130     c                   Eval      h4colo = 36
077830051130     c                   Eval      v4cmsg = msg(42)
077840051125     c                   Leavesr
077850051125     c                   EndIf
077860090416
077870090416      * se frequenza fattura settimanale controllo il tipo fattura del
077880090416      * sottoconto fatturazione (se diverso)
077890100127     c                   if        v4cfft = '1' and
077900100127     c                             v4cscf <> %editc(v1cksc:'X')
077910100127     c                   move      v4cscf        w0070
077920090416      * il sottoconto fatturazione non deve avere il tipo fattura distinta
077930090417     c/exec sql
077940090417     c+ select clptft into:w_clptft from cnclp00f where
077950100127     c+ clpkcc = 151 and clpksc = :w0070
077960090417     c/end-exec
077970090417     c                   if        sqlcod = 0 and w_clptft = 1
077980090416     c                   eval      *in28 = *on
077990090416     c                   eval      h4riga = 10
078000090416     c                   eval      h4colo = 36
078010090416     c                   eval      v4cmsg = msg(81)
078020090416     c                   leavesr
078030090417     c                   endif
078040090416     c                   endif
078050051125
078060051125     c                   EndSr
078070051125
078080051125      *------------------------------------------------------------------------*
078090051125      * CONTROLLO TIPO DATA FATTURA
078100051125      *------------------------------------------------------------------------*
078110051125     c     Sr_Ctrtdf     BegSr
078120051125
078130051125      * Ricerca
078140051130     c                   Clear                   v4dtdf
078150051130     c                   If        v4ctdf = '?'
078160051125     c                   Eval      kcod = '13'
078170051125     c                   call      'TRUL19R'
078180051125     c                   parm                    kcod
078190051125     c                   parm                    ordina
078200051125     c                   parm                    kkey
078210051125     c                   parm                    comando
078220051130     c                   Eval      v4ctdf = kkey
078230051130     c                   Eval      h4riga = 11
078240051130     c                   Eval      h4colo = 36
078250051125     c                   Eval      *In90 = *On
078260051125     c                   EndIf
078270051125
078280051125      * --> Controllo (obbligatorio)
078290051130     c                   If        v4ctdf = *Blanks
078300051125     c                   Eval      *In28 = *On
078310051130     c                   Eval      h4riga = 11
078320051130     c                   Eval      h4colo = 36
078330051130     c                   Eval      v4cmsg = msg(43)
078340051125     c                   Leavesr
078350051125     c                   EndIf
078360051125
078370051130     c                   Clear                   v4dtdf
078380051125     c                   Eval      kcod = '13'
078390051130     c                   Eval      kkey = v4ctdf
078400051125     c     kTabel        Chain     Tabel00f
078410051125     c                   If        Not %Found(Tabel00f) or
078420051125     c                             tblflg <> *Blanks
078430051125     c                   Eval      *In28 = *On
078440051130     c                   Eval      h4riga = 11
078450051130     c                   Eval      h4colo = 36
078460051130     c                   Eval      v4cmsg = msg(44)
078470051125     c                   Leavesr
078480051125     c                   EndIf
078490051125     c                   Eval      ds13 = tbluni
078500051130     c                   Eval      v4dtdf = §13des
078510051125
078520051125     c                   EndSr
078530051128
078540051128      *------------------------------------------------------------------------*
078550051128      * CONTROLLO SPESE INVIO FATTURA
078560051128      *------------------------------------------------------------------------*
078570051128     c     Sr_Ctrsin     BegSr
078580051128
078590051130     c                   If        v4csin > *Zeros
078600051128     c                   Reset                   xdecds
078610051130     c                   Z-add     v4csin        icdnum
078620051128     c                   Z-add     2             icdnrd
078630051128     c                   Call      'XDEC'
078640051128     c                   Parm                    xdecds
078650051128     c                   If        ocdesi <> *Off
078660051128     c                   Eval      *In28 = *On
078670051130     c                   Eval      h4riga = 12
078680051130     c                   Eval      h4colo = 36
078690051130     c                   Eval      v4cmsg = msg(48)
078700051128     c                   Leavesr
078710051128     c                   EndIf
078720051128     c                   EndIf
078730051128
078740051128     c                   EndSr
078750051128
078760051128      *------------------------------------------------------------------------*
078770051128      * CONTROLLO LUOGO 001 - INVIO FATTURA
078780051128      *------------------------------------------------------------------------*
078790051128     c     Sr_Ctrl001    BegSr
078800051128
078810051130if  1c                   If        v4cl001 <> *Blanks
078820051128
078830060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
078840060203     c                   If        *In05 and Not *In10
078850051128     c                   Eval      *In28 = *On
078860100727     c                   Eval      h4riga = 21
078870060203     c                   Eval      h4colo = 35
078880051130     c                   Eval      v4cmsg = msg(50)
078890051128     c                   Leavesr
078900051128     c                   EndIf
078910071128
078920071128      * se non è interrogazione controllo se luogo utilizzabile
078930071128     c                   if        not *in09 and not *in05 and $ufi001 <> 'S'
078940071128     c                   eval      *In28 = *on
078950100727     c                   eval      h4riga = 21
078960071128     c                   eval      h4colo = 35
078970071128     c                   eval      v4cmsg = msg(76)
078980071128     c                   leavesr
078990071128     c                   endif
079000051128
079010060203      * Imposto se interrogazione o gestione
079020051213     c                   Clear                   tnta81ds
079030060203     c   05              Eval      i81opz = '5'
079040060203     c  n05
079050060203     can 10              Eval      i81opz = '2'
079060051213     c                   Eval      i81cod = '001'
079070051213     c                   Eval      i81cli = v1cksc
079080060216     c                   Clear                   parmf11
079090060216     c                   Eval      i81gcli = '1'
079100060216     c   01              Eval      i81icli = '1'
079110060216     c                   Eval      kpjbu = parmf11
079120051213     c                   Call      'TNTA81R'
079130051213     c                   Parm                    kpjba
079140051213     c                   Parm                    tnta81ds
079150051128
079160051212      * Controllo se ora c'è o non c'è più il luogo
079170051216     c                   Eval      kspecli = v1cksc
079180051216     c                   Eval      kspecod = '001'
079190051128     c     kFnspe        Chain     Fnspe01l
079200051128     c                   If        %Found(Fnspe01l) and speflg = *Blanks
079210051128     c                   Eval      *In10 = *On
079220051212     c                   Else
079230051212     c                   Eval      *In10 = *Off
079240051128     c                   EndIf
079250051128
079260051130     c                   Clear                   v4cl001
079270100727     c                   Eval      h4riga = 21
079280060203     c                   Eval      h4colo = 35
079290051207     c                   Goto      emi04
079300051128
079310051129    1c                   EndIf
079320051128
079330051128     c                   EndSr
079340051122
079350051129      *------------------------------------------------------------------------*
079360051220      * CONTROLLO NOTA 84 - INVIO FATTURA
079370051129      *------------------------------------------------------------------------*
079380051129     c     Sr_Ctrnt84    BegSr
079390051129
079400051130if  1c                   If        v4cnt84 <> *Blanks
079410060203
079420060203      * Se sono in interrogazione non posso interrogare la nota se non c'è
079430060203     c                   If        *In05 and Not *In21
079440060203     c                   Eval      *In28 = *On
079450060203     c                   Eval      h4riga = 21
079460100727     c                   Eval      h4colo = 70
079470060203     c                   Eval      v4cmsg = msg(52)
079480060203     c                   Leavesr
079490060203     c                   EndIf
079500060203
079510060203      * Imposto se interrogazione o gestione
079520060203     c  n05              Eval      wrich = 'M'
079530060203     c   05              Eval      wrich = 'V'
079540060217     c                   Eval      wrag = v3crag
079550051212     c                   Eval      wtnt = '84'
079560051212     c                   ExSr      Sr_f02
079570060215
079580060215     c                   If        ta12err <> *Blanks
079590060215     c                   Eval      *In28 = *On
079600060215     c                   Eval      h4riga = 21
079610100727     c                   Eval      h4colo = 70
079620060215     c                   Eval      v4cmsg = ta12msg
079630060215     c                   Leavesr
079640060215     c                   EndIf
079650051129
079660060215      * Controllo se ora c'è o non c'è più la nota
079670091112     c   70
079680091112     corn06              Eval      kntcapl = 'C'
079690100806     c   06
079700091112     cann70              Eval      kntcapl = 'T'
079710051216     c                   Movel(p)  dutkci        kntcnk1
079720091112     c   70
079730091112     corn06              Move      v1cksc        kntcnk1
079740100806     c   06
079750100806     cann70              Move      v1cnrv        kntcnk1
079760051216     c                   Clear                   kntcnk2
079770051216     c                   Movel     '84'          kntctnt
079780091119     c     kTfntc        Chain(n)  Tfntc01l
079790051129     c                   If        %Found(Tfntc01l)
079800051129     c                   Eval      *In21 = *On
079810051212     c                   Else
079820051212     c                   Eval      *In21 = *Off
079830051129     c                   EndIf
079840051129
079850051130     c                   Clear                   v4cnt84
079860051212     c                   Eval      h4riga = 21
079870100727     c                   Eval      h4colo = 70
079880051212     c                   Goto      emi04
079890051129
079900051129    1c                   EndIf
079910051129
079920051129     c                   EndSr
079930051129
079940051129      *------------------------------------------------------------------------*
079950051130      * CONTROLLO CODICE PAGAMENTO FATTURE
079960051129      *------------------------------------------------------------------------*
079970051129     c     Sr_Ctrcdp     BegSr
079980051129
079990051129      * Ricerca
080000100727     c                   Eval      wpos = %scan('?':v4ccdp)
080010051129     c                   If        wpos > 0
080020100727     c                   Clear                   v4dcdp
080030100727     c                   Clear                   v4ccdp
080040051129     c                   Call      'XCDPAGR'
080050100727     c                   Parm                    v4ccdp
080060100727     c                   Parm                    v4dcdp
080070100728     c                   Eval      h4riga = 13
080080100728     c                   Eval      h4colo = 36
080090051129     c                   Eval      *In90 = *On
080100100727     c                   If        v4ccdp = '999'
080110100727     c                   Clear                   v4ccdp
080120051129     c                   EndIf
080130051129     c                   EndIf
080140051129
080150051129      * Controllo
080160100727     c                   Clear                   v4dcdp
080170051129     c                   Clear                   dsfa
080180051129     c                   Eval      kcod = 'FA'
080190100727     c                   Movel     v4ccdp        w004a
080200051129     c                   Move      '1'           w004a
080210051129     c                   Eval      kkey = w004a
080220051129     c     kTabel        Chain     Tabel00f
080230051129     c                   If        Not %Found(Tabel00f) or
080240051129     c                             tblflg <> *Blanks
080250051129     c                   Eval      *In28 = *On
080260100728     c                   Eval      h4riga = 13
080270100728     c                   Eval      h4colo = 36
080280100728     c                   Eval      v4cmsg = msg(53)
080290051129     c                   Leavesr
080300051129     c                   EndIf
080310051129     c                   Eval      dsfa = tbluni
080320100727     c                   Eval      v4dcdp = §fades
080330051129
080340051129      * Se capoconto cliente non posso utilizzare il codice 3
080350051129     c                   If        v1ckcc = 151 and §fatpg = '3'
080360051129     c                   Eval      *In28 = *On
080370100728     c                   Eval      h4riga = 13
080380100728     c                   Eval      h4colo = 36
080390100728     c                   Eval      v4cmsg = msg(53)
080400051129     c                   Leavesr
080410051129     c                   EndIf
080420080515
080430080515      * se utente di filiale non può inerire un pagamento con codice '2' RID
080440090416      * ora controllo il tipo utilizzo se S è solo SEDE
080450080515     c                   if        not *in09 and
080460100727     c                             v4ccdp <> %subst(indcdp:4:3) and
080470090421     c                             §fauti = 'S'
080480080515     c                   eval      *In28 = *On
080490100728     c                   eval      h4riga = 13
080500100728     c                   eval      h4colo = 36
080510100728     c                   eval      v4cmsg = msg(80)
080520080515     c                   leavesr
080530080515     c                   endif
080540150415      /free
080550160517       //?Se il cliente è sottoconto intestazione fattura
080560160517       //?controllo se OK RIBA
080570160518         IF  %editc(V1Cksc:'X') = V4Cscf and §FAtpg = '1';
080580160518       //?No RIBA (tipo pag. 1) se P.IVA estera
080590160518           IF  (V2ivaeu <> *blanks and V2ivaeu <> '$$' and
080600160518                V2ivaeu <> 'IT' and V2ivaeu <> 'SM') or
080610160518               (V2ivaeu = '$$' and V2Ccdf = *blanks);
080620160517             *in28 = *on;
080630160517             H4riga = 13;
080640160517             H4colo = 36;
080650160517             V4Cmsg = msg(89);
080660160517             leavesr;
080670160517           ENDIF;
080680160517       //?No RIBA (tipo pag. 1) su conto Bancoposta (ABI 07601)
080690160518           IF  V4Cabi = 07601;
080700160517             *in28 = *on;
080710160517             H4riga = 13;
080720160517             H4colo = 36;
080730160517             V4Cmsg = msg(90);
080740160517             leavesr;
080750160518           ENDIF;
080760160517         ENDIF;
080770160517
080780150415       //?Se utente di filiale e tipo pagamento utilizzabile solo da sede
080790150415       //?devo proteggere la Banca ABI/CAB del pagamento fatture
080800150415         *in82 = *off;
080810150415         IF  not *in09 and §FAuti = 'S';
080820150415           *in82 = *on;
080830150415         ENDIF;
080840150415      /end-free
080850051129
080860051129     c                   EndSr
080870051129
080880051129      *------------------------------------------------------------------------*
080890051129      * CONTROLLO BANCA APPOGGIO
080900051129      *------------------------------------------------------------------------*
080910051129     c     Sr_Ctrbna     BegSr
080920070216
080930070216     c                   clear                   wbanca
080940070216     c                   clear                   wagenzia
080950051129
080960051129      * Ricerca
080970100727     c                   Eval      wpos = %scan('?':v4cbna)
080980051129if  1c                   If        wpos > 0
080990100727     c                   Eval      pist = %subst(v4cbna:(wpos+1):(36-wpos))
081000100727     c                   Move      v4cabi        pabi
081010051129     c                   Clear                   pcab
081020051129     c                   Eval      ppro = v2cprv
081030051129     c                   Eval      pflg = 'R'
081040051129     c                   Eval      kpjbu = parmbanca
081050051129     c                   Call      'CNC592R'
081060051129     c                   Parm                    kpjba
081070051129     c                   Eval      parmbanca = kpjbu
081080051129      * Se è stato selezionato un codice
081090051129if  2c                   If        pcab <> *Zeros
081100100727     c                   Movel     pabi          v4cabi
081110100727     c                   Movel     pcab          v4ccab
081120100727     c                   Eval      kabi = v4cabi
081130100727     c                   Eval      kcab = v4ccab
081140100729     c                   exsr      DesBanca
081150100729     c                   eval      v4cbna = wDesBanca
081160051129    2c                   EndIf
081170051129     c                   Eval      *In90 = *On
081180100728     c                   Eval      h4riga = 14
081190100728     c                   Eval      h4colo = 36
081200051129     c                   Leavesr
081210051129    1c                   EndIf
081220051129
081230051129     c                   EndSr
081240051129
081250051129      *------------------------------------------------------------------------*
081260051130      * CONTROLLO ABI/CAB RELATIVO AL PAGAMENTO FATTURE
081270051129      *------------------------------------------------------------------------*
081280051130     c     Sr_Ctrabicabf BegSr
081290051129
081300051129      * Se non è da gestione visite/offerte controllo se abi e cab
081310051129      * obbligatori per pagamento con RB
081320051129     c                   If        Not *In06 and §fatpg = '1' and
081330100727     c                             v4cabi = *Zeros and v4ccab = *Zeros
081340051129     c                   Eval      *In28 = *On
081350100728     c                   Eval      h4riga = 15
081360100728     c                   Eval      h4colo = 36
081370100728     c                   Eval      v4cmsg = msg(54)
081380051129     c                   Leavesr
081390051129     c                   EndIf
081400090331
081410090331      * se pagamento con RB codice '1' non accetto ABI >= 90000
081420090331     c                   If        §fatpg = '1' and
081430100727     c                             v4cabi >= 90000
081440090331     c                   Eval      *In28 = *On
081450100728     c                   Eval      h4riga = 15
081460100728     c                   Eval      h4colo = 36
081470100728     c                   Eval      v4cmsg = msg(56)
081480090331     c                   Leavesr
081490090331     c                   EndIf
081500051130
081510051130      * Controllo
081520100727     c                   Eval      kabi = v4cabi
081530100727     c                   Eval      kcab = v4ccab
081540051130     c                   ExSr      Sr_Ctrabicab
081550051130     c                   If        *In28 = *On
081560100728     c                   Eval      h4riga = 15
081570100728     c                   Eval      h4colo = 36
081580051130     c                   Leavesr
081590051130     c                   EndIf
081600051130      * Creo descrizione della banca di appoggio
081610100727     c                   If        v4cabi <> 99999 or v4ccab <> 99999
081620100727     c                   Movel     wbanca        v4cbna
081630100727     c                   Move      wagenzia      v4cbna
081640051130     c                   EndIf
081650051130
081660051130     c                   EndSr
081670150424
081680150424      /free
081690150424       //--------------------------------------------------------------
081700150424       //?Controllo i pagamenti C/A - Danni - N.A.
081710150424       //--------------------------------------------------------------
081720150424       BEGSR CtrPagamenti;
081730150424
081740150424       //?Pagamenti CONTRASSEGNI
081750150424         clear ds4U;
081760150424         kcod = '4U';
081770150424         kkey = V5Cfpc;
081780150424         chain (codut:kcod:kkey) TABEL00F;
081790150424         IF  %found(TABEL00F);
081800150424           ds4U = TBLuni;
081810150424         ENDIF;
081820151112         IF  V5Cfpc <> '1';
081830150427         IF  not wForzaPag and §4Utip = 'I' and
081840150427            (§4Ucod <> 'B' or V5Ciban = *blanks);
081850150424           *in28 = *on;
081860150424           H5riga = 09;
081870150424           H5colo = 36;
081880150424           V5Cmsg = msg(87);
081890150424           wForzaPag = *on;
081900150424           leavesr;
081910150424         ENDIF;
081920151112         ENDIF;
081930150424
081940150424       //?Pagamenti DANNI
081950150424         clear ds4U;
081960150424         kcod = '4U';
081970150424         kkey = V5tpdn;
081980150424         chain (codut:kcod:kkey) TABEL00F;
081990150424         IF  %found(TABEL00F);
082000150424           ds4U = TBLuni;
082010150424         ENDIF;
082020151112         IF  V5tpdn <> '1';
082030150427         IF  not wForzaPag and §4Utip = 'I' and
082040150427            (§4Ucod <> 'B' or V5ibandn = *blanks);
082050150424           *in28 = *on;
082060150424           H5riga = 12;
082070150424           H5colo = 36;
082080150424           V5Cmsg = msg(87);
082090150424           wForzaPag = *on;
082100150424           leavesr;
082110150424         ENDIF;
082120151112         ENDIF;
082130150424
082140150424       //?Pagamenti NOTE ACCREDITO
082150150424         clear ds4U;
082160150424         kcod = '4U';
082170150424         kkey = V5tpna;
082180150424         chain (codut:kcod:kkey) TABEL00F;
082190150424         IF  %found(TABEL00F);
082200150424           ds4U = TBLuni;
082210150424         ENDIF;
082220151112         IF  V5tpna <> '1';
082230150427         IF  not wForzaPag and §4Utip = 'I' and
082240150427            (§4Ucod <> 'B' or V5ibanna = *blanks);
082250150424           *in28 = *on;
082260150424           H5riga = 15;
082270150427           H5colo = 37;
082280150424           V5Cmsg = msg(87);
082290150424           wForzaPag = *on;
082300150424           leavesr;
082310150424         ENDIF;
082320151112         ENDIF;
082330150424
082340150424       ENDSR;
082350150728
082360150728       //--------------------------------------------------------------
082370150728       //?Controllo i dati per la mail invio progetto di liquidazione.
082380150728       //--------------------------------------------------------------
082390150728       BEGSR CtrMailDanni;
082400150728
082410150728       //?Se esiste la nota 87 OK
082420150728         IF  *in18;
082430150728           leavesr;
082440150728         ENDIF;
082450150728
082460150728       //?Se esiste il luogo 008 e c'è la mail OK
082470150728         IF  *in17;
082480150728           kSPEcli = V1Cksc;
082490150728           kSPEcod = '008';
082500150728           chain (kSPEcli:kSPEcod:kSP2tpe) FNSP201L;
082510150728           IF  %Found(FNSP201L) and SP2flg = *blanks;
082520150728             leavesr;
082530150728           ENDIF;
082540150728         ENDIF;
082550150728
082560150728       //?Se arrivo qua vuol dire che non c'è la nota 87 oppure
082570150728       //?non c'è il luogo 008 o il luogo 008 non ha la mail
082580150728         IF  not wForzaMail;
082590150728           *in28 = *on;
082600150728           H7riga = 13;
082610150728           H7colo = 35;
082620150728           V7Cmsg = msg(88);
082630150728           wForzaMail = *on;
082640150728         ENDIF;
082650150728
082660150728       ENDSR;
082670150424      /end-free
082680051130
082690051130      *------------------------------------------------------------------------*
082700051130      * CONTROLLO CODICE PAGAMENTO CONTRASSEGNI
082710051130      *------------------------------------------------------------------------*
082720051130     c     Sr_Ctrfpc     BegSr
082730051219
082740051219     c                   Eval      *In22 = *Off
082750051219     c                   Eval      *In23 = *Off
082760051130
082770051130      * Ricerca
082780051130     c                   Clear                   v5dfpc
082790051130     c                   If        v5cfpc = '?'
082800051130     c                   Eval      kcod = '4U'
082810051130     c                   call      'TRUL19R'
082820051130     c                   parm                    kcod
082830051130     c                   parm                    ordina
082840051130     c                   parm                    kkey
082850051130     c                   parm                    comando
082860051130     c                   Eval      v5cfpc = kkey
082870100729     c                   Eval      h5riga = 09
082880100802     c                   Eval      h5colo = 36
082890051130     c                   Eval      *In90 = *On
082900051130     c                   EndIf
082910051130
082920051130      * Controllo
082930051130     c                   Clear                   v5dfpc
082940051130     c                   Clear                   ds4u
082950051130     c                   Eval      kcod = '4U'
082960051130     c                   Eval      kkey = v5cfpc
082970051130     c     kTabel        Chain     Tabel00f
082980051130     c                   If        Not %Found(Tabel00f) or
082990051130     c                             tblflg <> *Blanks
083000051130     c                   Eval      *In28 = *On
083010100729     c                   Eval      h5riga = 09
083020100802     c                   Eval      h5colo = 36
083030051130     c                   Eval      v5cmsg = msg(57)
083040051130     c                   Leavesr
083050051130     c                   EndIf
083060051130     c                   Eval      ds4u = tbluni
083070051130     c                   Eval      v5dfpc = §4udes
083080051130
083090051130      * Controllo se utilizzabile per i clienti vari
083100051130     c                   If        (wksc04 = 8888 or wksc04 = 9999) and
083110051130     c                              §4uvar = 'N'
083120051130     c                   Eval      *In28 = *On
083130100729     c                   Eval      h5riga = 09
083140100802     c                   Eval      h5colo = 36
083150051130     c                   Eval      v5cmsg = msg(58)
083160051130     c                   Leavesr
083170051130     c                   EndIf
083180151030
083190151030      /free
083200151030       //?Errore se utilizzabile solo da sede e NON sono sede
083210151112         IF  §4Usede <> *blanks and not *in09 and v5cfpc <> CLPfpc;
083220151030           *in28 = *on;
083230151030           H5riga = 09;
083240151030           H5colo = 36;
083250151030           V5Cmsg = msg(57);
083260151030           V5Cmsg = %trim(V5Cmsg) + '. Utilizzabili SOLO da SEDE';
083270151030           leavesr;
083280151030         ENDIF;
083290151030      /end-free
083300051219
083310051219      * Le coordinate bancare le visualizzo o le proteggo in base al
083320051219      * tipo pagamento
083330051219     c                   Select
083340051219      * Coordinate italiane, visualizzo
083350051219      * Immissione coordinate non richiesta, non visualizzo
083360051219     c                   When      §4uabi = 'N'
083370051219     c                   Eval      *In22 = *On
083380080116      * pulisco le coordinate bancarie
083390080116     c                   clear                   v5ciban
083400051219      * Se pagamento estero proteggo le coordinate bancarie perchè
083410051219      * gestite dalla sede.
083420080115     c                   when      §4utip = 'E'
083430051219     c                   Eval      *In23 = *On
083440051219     c                   EndSl
083450100729      * se il tipo pagamento del codice impostato a video è diverso da quello
083460100729      * memorizzato sul file ed è estero devo pulire il campo relativo all'IBAN
083470100729      * e avvisare con window per inviare mail alla sede
083480100729     c                   if        §4utip <> sav4utip and §4utip = 'E'
083490100729     c                   if        v5ciban <> *blanks
083500100729     c                   clear                   v5ciban
083510100729     c                   endif
083520100729      * window per inviare mail alla sede
083530100729      * solo se tipo pagamento prevede le coordinate bancarie
083540100729     c                   if        not $win and §4uabi = 'S'
083550101014     c                   eval      wuffsede = '"CONTRASSEGNI" di sede'
083560100729     c                   exsr      sr_winmail
083570100729     c                   eval      $win = *on
083580100729     c                   eval      *in90 = *on
083590100729     c                   leavesr
083600100729     c                   endif
083610100729     c                   endif
083620051130
083630051130     c                   EndSr
083640100729
083650100729      /free
083660100729       //--------------------------------------------------------------
083670100729       //?Controllo pagamento Danni.
083680100729       //--------------------------------------------------------------
083690100729       BEGSR CtrPagDN;
083700100729
083710100729         *in71 = *off;
083720100729         *in72 = *off;
083730100729
083740100729       //?Ricerco il codice pagamento
083750100729         clear v5dpdn;
083760100729         IF  %scan('?':v5tpdn) > 0;
083770100729           kcod = '4U';
083780100729           Trul19R (kcod:ordina:kkey:comando);
083790100729           v5tpdn = kkey;
083800100730           h5riga = 12;
083810100802           h5colo = 36;
083820100729           *in90 = *on;
083830100729         ENDIF;
083840100729
083850100729       //?Controllo
083860100729         clear v5dpdn;
083870100729         clear ds4u;
083880100729         kcod = '4U';
083890100729         kkey = v5tpdn;
083900100729         chain (codut:kcod:kkey) TABEL00F;
083910100729         IF  not %Found(TABEL00F) or TBLflg <> *blanks;
083920100729           *in28 = *on;
083930100730           h5riga = 12;
083940100802           h5colo = 36;
083950100729           v5cmsg = msg(57);
083960100729           leavesr;
083970100729         ENDIF;
083980100729         ds4u = TBLuni;
083990100730         v5dpdn = §4Udes;
084000100729
084010100729       //?Controllo se posso utilizzare per clienti vari
084020110131         IF  (wksc04 = 8888 or wksc04 = 9999) and §4Uvar = 'N';
084030100729           *in28 = *on;
084040100730           h5riga = 12;
084050100802           h5colo = 36;
084060100729           v5cmsg = msg(58);
084070100729           leavesr;
084080100729         ENDIF;
084090151030
084100151030       //?Errore se utilizzabile solo da sede e NON sono sede
084110151112         IF  §4Usede <> *blanks and not *in09 and
084120151112             v5tpdn <> %subst(CLStic:1:1);
084130151030           *in28 = *on;
084140151030           H5riga = 12;
084150151030           H5colo = 36;
084160151030           V5Cmsg = msg(57);
084170151030           V5Cmsg = %trim(V5Cmsg) + '. Utilizzabili SOLO da SEDE';
084180151030           leavesr;
084190151030         ENDIF;
084200151030
084210100729       //?Visualizzo il campo iban se coordinate bancarie obbligatorie
084220100730         *in71 = (§4Uabi = 'N');
084230100729
084240100729       //?Controllo se posso utilizzare in base alla tabella PAG
084250100729         clear TIBS02DS;
084260100729         clear dPAG;
084270100729         T02mod = 'C';
084280100729         T02cod = 'PAG';
084290100729         T02ke1 = 'DN';
084300100729         T02ke2 = v5tpdn;
084310100729         T02sif = KNSIF;
084320100729         TNTBE_RicercaControllo (kpjba : tibs02ds);
084330100729         IF  T02err <> *blanks;
084340100729           *in28 = *on;
084350100730           h5riga = 12;
084360100802           h5colo = 36;
084370100729           v5cmsg = msg(85);
084380100729           leavesr;
084390100729         ENDIF;
084400100729         IF  T02err = *blanks;
084410100729           dPAG = T02uni;
084420100729         ENDIF;
084430100729       //?controllo se la filiale può utilizzare il pagamento
084440151112         IF  d§PAGuti = 'NO' and not *in09 and
084450151112             v5tpdn <> %subst(CLStic:1:1);
084460100729           *in28 = *on;
084470100730           h5riga = 12;
084480100802           h5colo = 36;
084490100729           v5cmsg = msg(80);
084500100729           leavesr;
084510100729         ENDIF;
084520100730       //?proteggo il campo IBAN se pagamento estero
084530100730         *in72 = (§4Utip = 'E');
084540100909
084550100909       //?se pagamento con bonifico italia posso utilizzarlo solo
084560100909       //?per clienti 'T - D - A'
084570150506         //IF  not *in72 and d§PAGctr = 'SI' and
084580150506         //    v3cclv <> 'T' and  v3cclv <> 'D' and
084590150506         //    v3cclv <> 'A';
084600150506         //  *in28 = *on;
084610150506         //  h5riga = 12;
084620150506         //  h5colo = 36;
084630150506         //  v5cmsg = msg(85);
084640150506         //  v5cmsg = %trim(v5cmsg) + ' per clienti NON di tipo T-D-A';
084650150506         //  leavesr;
084660150506         //  ENDIF;
084670100730
084680100802       //?se è stato modificato il tipo pagamento
084690100909       //?e quello inserito non richiede le coordinate bancarie pulisco
084700100802       //?il campo dell'IBAN
084710100802         IF  v5tpdn <> savtipdn and d§PAGobb <> 'SI';
084720100802           clear v5ibandn;
084730100805           clear v5bicdn;
084740100802       //?se il tipo pagamento inserito è un tipo pagamento che lo richiede
084750100802       //?visualizzo window per invio mail
084760100802           IF  d§PAGobb = 'MA' and not $winDN;
084770101014             wuffsede = '"GESTIONE PAGAMENTI" di sede';
084780100802             exsr sr_winmail;
084790100802             $winDN = *on;
084800100802             *in90 = *on;
084810100802             leavesr;
084820100802           ENDIF;
084830100730         ENDIF;
084840100729
084850100729       ENDSR;
084860100729
084870100729       //--------------------------------------------------------------
084880100729       //?Controllo pagamento Note Accredito.
084890100729       //--------------------------------------------------------------
084900100729       BEGSR CtrPagNA;
084910100730
084920100730         *in73 = *off;
084930100730         *in74 = *off;
084940100730
084950100730       //?Ricerco il codice pagamento
084960100730         clear v5dpna;
084970100730         IF  %scan('?':v5tpna) > 0;
084980100730           kcod = '4U';
084990100730           Trul19R (kcod:ordina:kkey:comando);
085000100916           v5tpna = kkey;
085010100730           h5riga = 15;
085020150427           h5colo = 37;
085030100730           *in90 = *on;
085040100730         ENDIF;
085050100730
085060100730       //?Controllo
085070100730         clear v5dpna;
085080100730         clear ds4u;
085090100730         kcod = '4U';
085100100730         kkey = v5tpna;
085110100730         chain (codut:kcod:kkey) TABEL00F;
085120100730         IF  not %Found(TABEL00F) or TBLflg <> *blanks;
085130100730           *in28 = *on;
085140100730           h5riga = 15;
085150150427           h5colo = 37;
085160100730           v5cmsg = msg(57);
085170100730           leavesr;
085180100730         ENDIF;
085190100730         ds4u = TBLuni;
085200100730         v5dpna = §4Udes;
085210100730
085220100730       //?Controllo se posso utilizzare per clienti vari
085230110131         IF  (wksc04 = 8888 or wksc04 = 9999) and §4Uvar = 'N';
085240100730           *in28 = *on;
085250100730           h5riga = 15;
085260150427           h5colo = 37;
085270100730           v5cmsg = msg(58);
085280100730           leavesr;
085290100730         ENDIF;
085300151030
085310151030       //?Errore se utilizzabile solo da sede e NON sono sede
085320151112         IF  §4Usede <> *blanks and not *in09 and
085330151112             v5tpna <> %subst(CLStic:2:1);
085340151030           *in28 = *on;
085350151030           H5riga = 15;
085360151030           H5colo = 37;
085370151030           V5Cmsg = msg(57);
085380151030           V5Cmsg = %trim(V5Cmsg) + '. Utilizzabili SOLO da SEDE';
085390151030           leavesr;
085400151030         ENDIF;
085410151030
085420100730       //?Visualizzo il campo iban se coordinate bancarie obbligatorie
085430100730         *in73 = (§4Uabi = 'N');
085440100730
085450100730       //?Controllo se posso utilizzare in base alla tabella PAG
085460100730         clear TIBS02DS;
085470100730         clear dPAG;
085480100730         T02mod = 'C';
085490100730         T02cod = 'PAG';
085500100730         T02ke1 = 'NA';
085510100730         T02ke2 = v5tpna;
085520100730         T02sif = KNSIF;
085530100730         TNTBE_RicercaControllo (kpjba : tibs02ds);
085540100730         IF  T02err <> *blanks;
085550100730           *in28 = *on;
085560100730           h5riga = 15;
085570150427           h5colo = 37;
085580100730           v5cmsg = msg(85);
085590100730           leavesr;
085600100730         ENDIF;
085610100730         IF  T02err = *blanks;
085620100730           dPAG = T02uni;
085630100730         ENDIF;
085640100730       //?controllo se la filiale può utilizzare il pagamento
085650151112         IF  d§PAGuti = 'NO' and not *in09 and
085660151112             v5tpna <> %subst(CLStic:2:1);
085670100730           *in28 = *on;
085680100730           h5riga = 15;
085690150427           h5colo = 37;
085700100730           v5cmsg = msg(80);
085710100730           leavesr;
085720100730         ENDIF;
085730100730       //?proteggo il campo IBAN se pagamento estero
085740100730         *in74 = (§4Utip = 'E');
085750100730
085760100802       //?se è stato modificato il tipo pagamento
085770100909       //?e quello inserito non richiede le coordinate bancarie pulisco
085780100802       //?il campo dell'IBAN
085790100802         IF  v5tpna <> savtipna and d§PAGobb <> 'SI';
085800100802           clear v5ibanna;
085810100805           clear v5bicna;
085820100802       //?se il tipo pagamento inserito è un tipo pagamento che lo richiede
085830100802       //?visualizzo window per invio mail
085840100802           IF  d§PAGobb = 'MA' and not $winNA;
085850101220             wuffsede = '"GESTIONE PAGAMENTI" di sede';
085860100730             exsr sr_winmail;
085870100730             $winNA = *on;
085880100730             *in90 = *on;
085890100730             leavesr;
085900100730           ENDIF;
085910100730         ENDIF;
085920100729
085930100729       ENDSR;
085940100729      /end-free
085950080115
085960080115      *------------------------------------------------------------------------*
085970080115      * CONTROLLO CODICE IBAN
085980080115      *------------------------------------------------------------------------*
085990080115     c     sr_ctriban    begsr
086000080115
086010080115     c                   clear                   wbanca
086020080115     c                   clear                   wagenzia
086030080422
086040080422      * se tipo pagamento contrassegno italia posso accettare solo
086050090612      * un IBAN italia (IT)
086060080422     c                   if        §4utip = 'I' and
086070100729     c                             %subst(wiban:1:2) <> 'IT'
086080150609     c                             and %subst(wiban:1:2) <> 'SM'
086090150424     c                             and %subst(wiban:1:2) <> *blanks
086100080422     c                   eval      *in28 = *on
086110080422     c                   eval      v5cmsg = msg(79)
086120080422     c                   leavesr
086130080422     c                   endif
086140080116
086150080116      * richiamo programma di controllo codice IBAN (Proj)
086160080116     c                   clear                   parmiban
086170100730     c                   eval      parmstato = %subst(wiban:1:2)
086180100730     c                   eval      parmcheck = %subst(wiban:3:2)
086190100730     c                   eval      parmcoori = %subst(wiban:5:28)
086200080116     c                   call      'XCHKIBAN'
086210080116     c                   parm                    parmstato
086220080116     c                   parm                    parmcheck
086230080116     c                   parm                    parmcoori
086240080116     c                   parm                    erriban
086250080116      * se ritorna errore messaggio
086260080116     c                   if        erriban = *on
086270080116     c                   eval      *in28 = *on
086280080116     c                   eval      v5cmsg = msg(59)
086290080116     c                   leavesr
086300080116     c                   endif
086310080116      * decodifico la banca
086320100729     c                   clear                   wdesbanca
086330100729     c                   eval      kabi = %dec(%subst(wiban:6:5):5:0)
086340100729     c                   eval      kcab = %dec(%subst(wiban:11:5):5:0)
086350080116     c     kcnabi        chain     cnabi00f
086360080116     c                   if        %found(cnabi00f) and abiann <> '*'
086370080116      * compongo la descrizione della banca d'appoggio
086380080116     c                   exsr      sr_crebna
086390100730     c                   eval      wdesbanca = wbanca + wagenzia
086400080116     c                   endif
086410080115
086420080115     c                   endsr
086430051130
086440051130      *-------------------------------------------------------------------------
086450051130      *  Creo la descrizione della banca di appoggio in base ai codici ABI/CAB
086460051130      *-------------------------------------------------------------------------
086470051130     c     Sr_Crebna     BegSr
086480051130
086490051130if  1c                   If        abiabi <> 99999 or abicab <> 99999
086500051130     c                   Clear                   wagenzia
086510060221     c                   Clear                   wbanca
086520051130     c                   Eval      wbanca = abiist
086530051130      * Compongo il nome della banca con località o comune
086540051130if  2c                   If        abiage = *Blanks
086550051130     c                   Select
086560051130     c                   When      abiloc <> *Blanks
086570051130     c                   Eval      wagenzia = abiloc
086580051130     c                   When      abicom <> *Blanks
086590051130     c                   Eval      wagenzia = abicom
086600051130     c                   EndSl
086610051130      * Compongo il nome della banca con agenzia + località o comune
086620051130   x2c                   Else
086630051130      * Cerco il primo byte vuoto dell'agenzia
086640051130     c                   Eval      wpos = %checkr(' ':abiage)
086650051130
086660051130     c                   Eval      wagenzia = %subst(abiage:1:wpos)
086670051130     c                   Eval      wpos1 = wpos + 2
086680051130     c                   Eval      wpos2 = 16 - wpos1
086690060215      * Se wpos2 inferiore a zero lo imposto a 0
086700060215     c                   If        wpos2 < *Zeros
086710060215     c                   Clear                   wpos2
086720060215     c                   EndIf
086730051130      * Se posso aggiungere anche solo 1 carattere procedo
086740051130     c                   If        wpos > 1 and wpos < 16
086750051130      * Cerco di riempire quello che resta con la località o il comune
086760051130     c                   Select
086770051130     c                   When      abiloc <> *Blanks
086780060221     c                   Eval      wagenzia = %trim(wagenzia) + ' ' +
086790060221     c                             %subst(abiloc:1:wpos2)
086800051130     c                   When      abicom <> *Blanks
086810060221     c                   Eval      wagenzia = %trim(wagenzia) + ' ' +
086820060221     c                             %subst(abicom:1:wpos2)
086830051130     c                   EndSl
086840051130     c                   EndIf
086850051130
086860051130    2c                   EndIf
086870051130    1c                   EndIf
086880051130
086890051130     c                   EndSr
086900051207
086910051130      *------------------------------------------------------------------------*
086920051130      * CONTROLLO ABI/CAB GENERICO
086930051130      *------------------------------------------------------------------------*
086940051130     c     Sr_Ctrabicab  BegSr
086950051130
086960051130      * Controllo ABI/CAB inseriti
086970051130     c                   If        kabi = *Zeros and kcab = *Zeros
086980051130     c                   Leavesr
086990051130     c                   EndIf
087000051130
087010051130      * Se immesso il CAB immettere anche ABI
087020051130     c                   If        (kabi = *Zeros and kcab <> *Zeros) or
087030051130      * Se immesso il ABI immettere anche CAB
087040051130     c                             (kabi <> *Zeros and kcab = *Zeros)
087050051130     c                   Eval      *In28 = *On
087060101111     c                   Eval      v4cmsg = msg(56)
087070051130     c                   Leavesr
087080051130     c                   EndIf
087090051130      * Controllo se validi
087100051130     c     kCnabi        Chain     Cnabi00f
087110051130     c                   If        Not %Found(Cnabi00f) or abiann = '*'
087120051130     c                   Eval      *In28 = *On
087130101111     c                   Eval      v4cmsg = msg(56)
087140051130     c                   Leavesr
087150051130     c                   EndIf
087160051130
087170051130      * Compongo la descrizione della banca d'appoggio
087180051130     c                   ExSr      Sr_Crebna
087190051130
087200051130     c                   EndSr
087210051129
087220051206
087230051206      *------------------------------------------------------------------------*
087240051206      * CONTROLLO LUOGO 555 - INTESTAZIONE PAGAMENTO C/ASSEGNI
087250051206      *------------------------------------------------------------------------*
087260051206     c     Sr_Ctrl555    BegSr
087270051206
087280051206if  1c                   If        v5cl555 <> *Blanks
087290060203
087300060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
087310060203     c                   If        *In05 and Not *In11
087320051206     c                   Eval      *In28 = *On
087330100730     c                   Eval      h5riga = 19
087340060203     c                   Eval      h5colo = 35
087350051207     c                   Eval      v5cmsg = msg(50)
087360051206     c                   Leavesr
087370051206     c                   EndIf
087380071128
087390071128      * se non è interrogazione controllo se luogo utilizzabile
087400071128     c                   if        not *in09 and not *in05 and $ufi555 <> 'S'
087410071128     c                   eval      *In28 = *on
087420100730     c                   eval      h5riga = 19
087430071128     c                   eval      h5colo = 35
087440071128     c                   eval      v5cmsg = msg(76)
087450071128     c                   leavesr
087460071128     c                   endif
087470051206
087480060203      * Imposto se interrogazione o gestione
087490051213     c                   Clear                   tnta81ds
087500060203     c   05              Eval      i81opz = '5'
087510060203     c  n05
087520060214     can 11              Eval      i81opz = '2'
087530051213     c                   Eval      i81cod = '555'
087540051213     c                   Eval      i81cli = v1cksc
087550060216     c                   Eval      i81gcli = '1'
087560060216     c   01              Eval      i81icli = '1'
087570051213     c                   Call      'TNTA81R'
087580051213     c                   Parm                    kpjba
087590051213     c                   Parm                    tnta81ds
087600051206
087610051212      * Controllo se ora c'è o non c'è più il luogo
087620051216     c                   Eval      kspecli = v1cksc
087630051216     c                   Eval      kspecod = '555'
087640051206     c     kFnspe        Chain     Fnspe01l
087650051206     c                   If        %Found(Fnspe01l) and speflg = *Blanks
087660051212     c                   Eval      *In11 = *On
087670051212     c                   Else
087680051212     c                   Eval      *In11 = *Off
087690051206     c                   EndIf
087700051206
087710051206     c                   Clear                   v5cl555
087720100730     c                   Eval      h5riga = 19
087730060203     c                   Eval      h5colo = 35
087740051207     c                   Goto      emi05
087750051206
087760051206    1c                   EndIf
087770051206
087780051206     c                   EndSr
087790051207
087800051207      *------------------------------------------------------------------------*
087810051212      * CONTROLLO LUOGO 666 - LUOGO INVIO C/ASSEGNO MITTENTE
087820051207      *------------------------------------------------------------------------*
087830051207     c     Sr_Ctrl666    BegSr
087840051212
087850051212if  1c                   If        v5cl666 <> *Blanks
087860060203
087870060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
087880060203     c                   If        *In05 and Not *In12
087890051212     c                   Eval      *In28 = *On
087900100730     c                   Eval      h5riga = 20
087910060203     c                   Eval      h5colo = 35
087920051212     c                   Eval      v5cmsg = msg(50)
087930051212     c                   Leavesr
087940051212     c                   EndIf
087950071128
087960071128      * se non è interrogazione controllo se luogo utilizzabile
087970071128     c                   if        not *in09 and not *in05 and $ufi666 <> 'S'
087980071128     c                   eval      *In28 = *on
087990100730     c                   eval      h5riga = 20
088000071128     c                   eval      h5colo = 35
088010071128     c                   eval      v5cmsg = msg(76)
088020071128     c                   leavesr
088030071128     c                   endif
088040051212
088050060203      * Imposto se interrogazione o gestione
088060051213     c                   Clear                   tnta81ds
088070060203     c   05              Eval      i81opz = '5'
088080060203     c  n05
088090060214     can 12              Eval      i81opz = '2'
088100051213     c                   Eval      i81cod = '666'
088110051213     c                   Eval      i81cli = v1cksc
088120060216     c                   Eval      i81gcli = '1'
088130060216     c   01              Eval      i81icli = '1'
088140051213     c                   Call      'TNTA81R'
088150051213     c                   Parm                    kpjba
088160051213     c                   Parm                    tnta81ds
088170051212
088180051212      * Controllo se ora c'è o non c'è più il luogo
088190051216     c                   Eval      kspecli = v1cksc
088200051216     c                   Eval      kspecod = '666'
088210051212     c     kFnspe        Chain     Fnspe01l
088220051212     c                   If        %Found(Fnspe01l) and speflg = *Blanks
088230051212     c                   Eval      *In12 = *On
088240051212     c                   Else
088250051212     c                   Eval      *In12 = *Off
088260051212     c                   EndIf
088270051212
088280051212     c                   Clear                   v5cl666
088290100730     c                   Eval      h5riga = 20
088300060203     c                   Eval      h5colo = 35
088310051212     c                   Goto      emi05
088320051212
088330051212    1c                   EndIf
088340051207
088350051207     c                   EndSr
088360051206
088370051206      *------------------------------------------------------------------------*
088380051220      * CONTROLLO NOTA 85 - E-MAIL BONIFICI C/ASSEGNI
088390051206      *------------------------------------------------------------------------*
088400051212     c     Sr_Ctrnt85    BegSr
088410051206
088420051212if  1c                   If        v5cnt85 <> *Blanks
088430060203
088440060203      * Se sono in interrogazione non posso interrogare la nota se non c'è
088450060203     c                   If        *In05 and Not *In13
088460060203     c                   Eval      *In28 = *On
088470100730     c                   Eval      h5riga = 19
088480100730     c                   Eval      h5colo = 71
088490060203     c                   Eval      v5cmsg = msg(52)
088500060203     c                   Leavesr
088510060203     c                   EndIf
088520060203
088530060203      * Imposto se interrogazione o gestione
088540060203     c  n05              Eval      wrich = 'M'
088550060203     c   05              Eval      wrich = 'V'
088560060217     c                   Eval      wrag = v3crag
088570051212     c                   Eval      wtnt = '85'
088580051212     c                   ExSr      Sr_f02
088590060215
088600060215     c                   If        ta12err <> *Blanks
088610060215     c                   Eval      *In28 = *On
088620100730     c                   Eval      h5riga = 19
088630100730     c                   Eval      h5colo = 71
088640060215     c                   Eval      v5cmsg = ta12msg
088650060215     c                   Leavesr
088660060215     c                   EndIf
088670051206
088680051212      * Controllo se ora c'è o non c'è più la nota
088690091112     c   70
088700091112     corn06              Eval      kntcapl = 'C'
088710100806     c   06
088720091112     cann70              Eval      kntcapl = 'T'
088730051216     c                   Movel(p)  dutkci        kntcnk1
088740091112     c   70
088750091112     corn06              Move      v1cksc        kntcnk1
088760100806     c   06
088770100806     cann70              Move      v1cnrv        kntcnk1
088780051216     c                   Clear                   kntcnk2
088790051216     c                   Movel     '85'          kntctnt
088800091119     c     kTfntc        Chain(n)  Tfntc01l
088810051206     c                   If        %Found(Tfntc01l)
088820051212     c                   Eval      *In13 = *On
088830051212     c                   Else
088840051212     c                   Eval      *In13 = *Off
088850051206     c                   EndIf
088860051206
088870051212     c                   Clear                   v5cnt85
088880100730     c                   Eval      h5riga = 19
088890100730     c                   Eval      h5colo = 71
088900051212     c                   Goto      emi05
088910051206
088920051206    1c                   EndIf
088930051206
088940051206     c                   EndSr
088950090616
088960090616      *------------------------------------------------------------------------*
088970090616      * CONTROLLO NOTA 89 - E-MAIL RESPONSABILE AMMINISTRATIVO
088980090616      *------------------------------------------------------------------------*
088990090616     c     Sr_Ctrnt89    BegSr
089000090616
089010090616if  1c                   If        v5cnt89 <> *Blanks
089020090616
089030090616      * Se sono in interrogazione non posso interrogare la nota se non c'è
089040090616     c                   If        *In05 and Not *In40
089050090616     c                   Eval      *In28 = *On
089060100730     c                   Eval      h5riga = 20
089070100730     c                   Eval      h5colo = 71
089080090616     c                   Eval      v5cmsg = msg(52)
089090090616     c                   Leavesr
089100090616     c                   EndIf
089110090616
089120090616      * Imposto se interrogazione o gestione
089130090616     c  n05              Eval      wrich = 'M'
089140090616     c   05              Eval      wrich = 'V'
089150090616     c                   Eval      wrag = v3crag
089160090616     c                   Eval      wtnt = '89'
089170090616     c                   ExSr      Sr_f02
089180090616
089190090616     c                   If        ta12err <> *Blanks
089200090616     c                   Eval      *In28 = *On
089210100730     c                   Eval      h5riga = 20
089220100730     c                   Eval      h5colo = 71
089230090616     c                   Eval      v5cmsg = ta12msg
089240090616     c                   Leavesr
089250090616     c                   EndIf
089260090616
089270090616      * Controllo se ora c'è o non c'è più la nota
089280091112     c   70
089290091112     corn06              Eval      kntcapl = 'C'
089300100806     c   06
089310091112     cann70              Eval      kntcapl = 'T'
089320090616     c                   Movel(p)  dutkci        kntcnk1
089330091112     c   70
089340091112     corn06              Move      v1cksc        kntcnk1
089350100806     c   06
089360100806     cann70              Move      v1cnrv        kntcnk1
089370090616     c                   Clear                   kntcnk2
089380090616     c                   Movel     '89'          kntctnt
089390091119     c     kTfntc        Chain(n)  Tfntc01l
089400090616     c                   If        %Found(Tfntc01l)
089410090616     c                   Eval      *In40 = *On
089420090616     c                   Else
089430090616     c                   Eval      *In40 = *Off
089440090616     c                   EndIf
089450090616
089460090616     c                   Clear                   v5cnt89
089470100730     c                   Eval      h5riga = 20
089480100730     c                   Eval      h5colo = 71
089490090616     c                   Goto      emi05
089500090616
089510090616    1c                   EndIf
089520090616
089530090616     c                   EndSr
089540100729      /free
089550100729       //--------------------------------------------------------------
089560100729       //?Controllo nota 82 - E-mail Bonifico Danni
089570100729       //--------------------------------------------------------------
089580100729       BEGSR sr_CtrNt82;
089590100730
089600100730         IF  v5cnt82 <> *blanks;
089610100730           *in90 = *on;
089620100730       //?Se sono in interrogazione non posso interrogare la note
089630100730       //?se non è presente
089640100730           IF  *in05 and not *in68;
089650100730             *in28 = *on;
089660100730             h5riga = 21;
089670100730             h5colo = 71;
089680100730             v5cmsg = msg(52);
089690100730             leavesr;
089700100730           ENDIF;
089710100730       //?Imposto se interrogazione o gestione della nota
089720100730           IF  not *in05;
089730100730             wrich = 'M';
089740100730           ENDIF;
089750100730           IF  *in05;
089760100730             wrich = 'V';
089770100730           ENDIF;
089780100730           wrag = v3crag;
089790100730           wtnt = '82';
089800100730           exsr sr_f02;
089810100730           IF  ta12err <> *blanks;
089820100730             *in28 = *on;
089830100730             h5riga = 21;
089840100730             h5colo = 71;
089850100730             v5cmsg = ta12msg;
089860100730             leavesr;
089870100730           ENDIF;
089880100730       //?Controllo se ora c'è la nota oppure se è stata cancellata
089890100730           SELECT;
089900100730             WHEN  *in70 or not *in06;
089910100730               kntcapl = 'C';
089920100730               %subst(kntcnk1:5:7) = %editc(v1cksc:'X');
089930100806             WHEN  *in06 and not *in70;
089940100730               kntcapl = 'T';
089950100806               %subst(kntcnk1:5:7) = %editc(v1cnrv:'X');
089960100730           ENDSL;
089970100730           %subst(kntcnk1:1:4) = %editc(DUTkci:'X');
089980100730           clear kntcnk2;
089990100730           kntctnt = '82';
090000150515           chain(n) (kntcapl:kntcnk1:kntcnk2:kntctnt) TFNTC01L;
090010100730           IF  %found(TFNTC01L);
090020100730             *in68 = *on;
090030100730           ELSE;
090040100730             *in68 = *off;
090050100730           ENDIF;
090060100730           clear v5cnt82;
090070100730           h5riga = 21;
090080100730           h5colo = 71;
090090100730         ENDIF;
090100100729
090110100729       ENDSR;
090120100729      /end-free
090130051207
090140051207      *------------------------------------------------------------------------*
090150051212      * CONTROLLO TIPO COMUNICAZIONE MITTENTE
090160051207      *------------------------------------------------------------------------*
090170051207     c     Sr_Ctrtcm     BegSr
090180051212
090190051212      * Ricerca
090200051212     c                   Clear                   v6dtcm
090210051212     c                   If        v6ctcm = '?'
090220051212     c                   Eval      kcod = '2F'
090230051212     c                   call      'TRUL19R'
090240051212     c                   parm                    kcod
090250051212     c                   parm                    ordina
090260051212     c                   parm                    kkey
090270051212     c                   parm                    comando
090280051212     c                   Eval      v6ctcm = kkey
090290051212     c                   Eval      h6riga = 07
090300051212     c                   Eval      h6colo = 38
090310051212     c                   Eval      *In90 = *On
090320051212     c                   EndIf
090330120118
090340120118      /free
090350120118       //?Se anagrafica provvisoria accetto il campo vuoto
090360120118         IF  *in06 and V6Ctcm = *blanks;
090370120118           leavesr;
090380120118         ENDIF;
090390120118      /end-free
090400051212
090410051212      * Controllo
090420051212     c                   Clear                   v6dtcm
090430051212     c                   Clear                   ds2f
090440051212     c                   Eval      kcod = '2F'
090450051212     c                   Eval      kkey = v6ctcm
090460051212     c     kTabel        Chain     Tabel00f
090470051212     c                   If        Not %Found(Tabel00f) or
090480051212     c                             tblflg <> *Blanks
090490051212     c                   Eval      *In28 = *On
090500051212     c                   Eval      h6riga = 07
090510051212     c                   Eval      h6colo = 38
090520051212     c                   Eval      v6cmsg = msg(62)
090530051212     c                   Leavesr
090540051212     c                   EndIf
090550051212     c                   Eval      ds2f = tbluni
090560051212     c                   Eval      v6dtcm = §2fdes
090570051212
090580051212      * Se tipo comunicazione con invio tramite fax
090590051212if  1c                   If        §2fftp = 'F'
090600051212      * Non ammesso per i clienti vari
090610051212     c                   If        wksc04 = 8888 or wksc04 = 9999
090620051212     c                   Eval      *In28 = *On
090630051212     c                   Eval      h6riga = 07
090640051212     c                   Eval      h6colo = 38
090650051212     c                   Eval      v6cmsg = msg(63)
090660051212     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
090670051212     c                             'per clienti vari'
090680051212     c                   Leavesr
090690051212     c                   EndIf
090700051216      * Controllo se fax sul luogo 005 è esatto
090710051216if  2c                   If        *In14 and wfax005 <> *Blanks
090720051216     c                   Clear                   trul42ds
090730051216     c                   Eval      d42fax = wfax005
090740051216     c                   Call      'TRUL42R'
090750051216     c                   Parm                    trul42ds
090760051216if  3c                   If        d42err = '1'
090770051216     c                   Eval      *In28 = *On
090780051216     c                   Eval      h6riga = 07
090790051216     c                   Eval      h6colo = 38
090800051216     c                   Eval      v6cmsg = d42msg
090810051216     c                   Leavesr
090820051216    3c                   EndIf
090830051216    2c                   EndIf
090840051216      * Se non c'è il luogo o se il n. fax non è presente sul luogo
090850051216      * deve essere impostato sull'anagrafica
090860051216     c                   If        v2ctlf = *Blanks and wfax005 = *Blanks
090870051212     c                   Eval      *In28 = *On
090880051212     c                   Eval      h6riga = 07
090890051212     c                   Eval      h6colo = 38
090900051212     c                   Eval      v6cmsg = msg(63)
090910051212     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
090920051212     c                             'nr. fax obbligatorio'
090930051212     c                   Leavesr
090940051212     c                   EndIf
090950051212    1c                   EndIf
090960051219
090970051219      * Se tipo comunicazione con invio tramite mail
090980051219if  1c                   If        §2fftp = 'M'
090990051219      * Non ammesso per i clienti vari
091000051219     c                   If        wksc04 = 8888 or wksc04 = 9999
091010051219     c                   Eval      *In28 = *On
091020051219     c                   Eval      h6riga = 07
091030051219     c                   Eval      h6colo = 38
091040051219     c                   Eval      v6cmsg = msg(63)
091050051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
091060051219     c                             'per clienti vari'
091070051219     c                   Leavesr
091080051219     c                   EndIf
091090051219      * Controllo se c'è la mail nel luogo
091100051219     c                   If        *In14 and wmail005 = *Blanks
091110051219     c                   Eval      *In28 = *On
091120051219     c                   Eval      h6riga = 07
091130051219     c                   Eval      h6colo = 38
091140051219     c                   Eval      v6cmsg = msg(63)
091150051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
091160051219     c                             'mail non immessa nel luogo 005'
091170051219     c                   Leavesr
091180051219     c                   EndIf
091190051219      * Se non c'è il luogo deve esserci la nota 88
091200051219     c                   If        Not *In14 and Not *In15
091210051219     c                   Eval      *In28 = *On
091220051219     c                   Eval      h6riga = 07
091230051219     c                   Eval      h6colo = 38
091240051219     c                   Eval      v6cmsg = msg(63)
091250051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
091260051219     c                             'nota 88 non immessa'
091270051219     c                   Leavesr
091280051219     c                   EndIf
091290051219    1c                   EndIf
091300051207
091310051207     c                   EndSr
091320051207
091330051207      *------------------------------------------------------------------------*
091340051212      * CONTROLLO TIPO COMUNICAZIONE FINE GIACENZA
091350051207      *------------------------------------------------------------------------*
091360051207     c     Sr_Ctrtcf     BegSr
091370051212
091380051212      * Ricerca
091390051212     c                   Clear                   v6dtcf
091400051212     c                   If        v6ctcf = '?'
091410051212     c                   Eval      kcod = '2H'
091420051212     c                   call      'TRUL19R'
091430051212     c                   parm                    kcod
091440051212     c                   parm                    ordina
091450051212     c                   parm                    kkey
091460051212     c                   parm                    comando
091470051212     c                   Eval      v6ctcf = kkey
091480051212     c                   Eval      h6riga = 08
091490051212     c                   Eval      h6colo = 38
091500051212     c                   Eval      *In90 = *On
091510051212     c                   EndIf
091520051212
091530051212      * Controllo
091540051212     c                   Clear                   v6dtcf
091550051212     c                   Clear                   ds2h
091560051212     c                   Eval      kcod = '2H'
091570051212     c                   Eval      kkey = v6ctcf
091580051212     c     kTabel        Chain     Tabel00f
091590051212     c                   If        Not %Found(Tabel00f) or
091600051212     c                             tblflg <> *Blanks
091610051212     c                   Eval      *In28 = *On
091620051212     c                   Eval      h6riga = 08
091630051212     c                   Eval      h6colo = 38
091640060216     c                   Eval      v6cmsg = msg(70)
091650051212     c                   Leavesr
091660051212     c                   EndIf
091670051212     c                   Eval      ds2h = tbluni
091680051212     c                   Eval      v6dtcf = §2hdes
091690051212
091700051212      * Se tipo comunicazione con invio tramite fax
091710051212if  1c                   If        §2hfax = 'F'
091720051212      * Non ammesso per i clienti vari
091730051212     c                   If        wksc04 = 8888 or wksc04 = 9999
091740051212     c                   Eval      *In28 = *On
091750051212     c                   Eval      h6riga = 08
091760051212     c                   Eval      h6colo = 38
091770051212     c                   Eval      v6cmsg = msg(63)
091780051212     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
091790051212     c                             'per clienti vari'
091800051212     c                   Leavesr
091810051212     c                   EndIf
091820051216      * Controllo se fax sul luogo 005 è esatto
091830051216if  2c                   If        *In14 and wfax005 <> *Blanks
091840051216     c                   Clear                   trul42ds
091850051216     c                   Eval      d42fax = wfax005
091860051216     c                   Call      'TRUL42R'
091870051216     c                   Parm                    trul42ds
091880051216if  3c                   If        d42err = '1'
091890051216     c                   Eval      *In28 = *On
091900051216     c                   Eval      h6riga = 08
091910051216     c                   Eval      h6colo = 38
091920051216     c                   Eval      v6cmsg = d42msg
091930051216     c                   Leavesr
091940051216    3c                   EndIf
091950051216    2c                   EndIf
091960051216      * Se non c'è il luogo o se il n. fax non è presente sul luogo
091970051216      * deve essere impostato sull'anagrafica
091980051216     c                   If        v2ctlf = *Blanks and wfax005 = *Blanks
091990051212     c                   Eval      *In28 = *On
092000051212     c                   Eval      h6riga = 08
092010051212     c                   Eval      h6colo = 38
092020051212     c                   Eval      v6cmsg = msg(63)
092030051212     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
092040051212     c                             'nr. fax obbligatorio'
092050051212     c                   Leavesr
092060051212     c                   EndIf
092070051212    1c                   EndIf
092080051219      * Se tipo comunicazione con invio tramite mail
092090051219if  1c                   If        §2hfax = 'M'
092100051219      * Non ammesso per i clienti vari
092110051219     c                   If        wksc04 = 8888 or wksc04 = 9999
092120051219     c                   Eval      *In28 = *On
092130051219     c                   Eval      h6riga = 08
092140051219     c                   Eval      h6colo = 38
092150051219     c                   Eval      v6cmsg = msg(63)
092160051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
092170051219     c                             'per clienti vari'
092180051219     c                   Leavesr
092190051219     c                   EndIf
092200051219      * Controllo se c'è la mail nel luogo
092210051219     c                   If        *In14 and wmail005 = *Blanks
092220051219     c                   Eval      *In28 = *On
092230051219     c                   Eval      h6riga = 08
092240051219     c                   Eval      h6colo = 38
092250051219     c                   Eval      v6cmsg = msg(63)
092260051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
092270051219     c                             'mail non immessa nel luogo 005'
092280051219     c                   Leavesr
092290051219     c                   EndIf
092300051219      * Se non c'è il luogo deve esserci la nota 88
092310051219     c                   If        Not *In14 and Not *In15
092320051219     c                   Eval      *In28 = *On
092330051219     c                   Eval      h6riga = 08
092340051219     c                   Eval      h6colo = 38
092350051219     c                   Eval      v6cmsg = msg(63)
092360051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
092370051219     c                             'nota 88 non immessa'
092380051219     c                   Leavesr
092390051219     c                   EndIf
092400051219    1c                   EndIf
092410051207
092420051207     c                   EndSr
092430051207
092440051207      *------------------------------------------------------------------------*
092450051212      * CONTROLLO APERTURA AUTOMATICA GIACENZA
092460051207      *------------------------------------------------------------------------*
092470051207     c     Sr_Ctrapg     BegSr
092480051212
092490051212      * Ricerca
092500051212     c                   Clear                   v6dapg
092510051212     c                   If        v6capg = '?'
092520051212     c                   Eval      kcod = '2U'
092530051212     c                   call      'TRUL19R'
092540051212     c                   parm                    kcod
092550051212     c                   parm                    ordina
092560051212     c                   parm                    kkey
092570051212     c                   parm                    comando
092580051212     c                   Eval      v6capg = kkey
092590051212     c                   Eval      h6riga = 09
092600051212     c                   Eval      h6colo = 38
092610051212     c                   Eval      *In90 = *On
092620051212     c                   EndIf
092630051212
092640051212      * Controllo
092650051212     c                   Clear                   v6dapg
092660051212     c                   Clear                   ds2u
092670051212     c                   Eval      kcod = '2U'
092680051212     c                   Eval      kkey = v6capg
092690051212     c     kTabel        Chain     Tabel00f
092700051212     c                   If        Not %Found(Tabel00f) or
092710051212     c                             tblflg <> *Blanks
092720051212     c                   Eval      *In28 = *On
092730051212     c                   Eval      h6riga = 09
092740051212     c                   Eval      h6colo = 38
092750051212     c                   Eval      v6cmsg = msg(64)
092760051212     c                   Leavesr
092770051212     c                   EndIf
092780051212     c                   Eval      ds2u = tbluni
092790051212     c                   Eval      v6dapg = §2udes
092800051212
092810051212      * Non ammessa apertura automatica per i clienti vari
092820051212     c                   If        §2uape = 'S' and
092830051212     c                             (wksc04 = 8888 or wksc04 = 9999)
092840051212     c                   Eval      *In28 = *On
092850051212     c                   Eval      h6riga = 09
092860051212     c                   Eval      h6colo = 38
092870051212     c                   Eval      v6cmsg = msg(65)
092880051212     c                   Leavesr
092890051212     c                   EndIf
092900051207
092910051207     c                   EndSr
092920051207
092930051207      *------------------------------------------------------------------------*
092940051212      * CONTROLLO DISPOSIZIONI MITTENTE IN ARRIVO
092950051207      *------------------------------------------------------------------------*
092960051207     c     Sr_Ctrdmg     BegSr
092970051212
092980051212      * Non ammesse disposizioni mittente in arrivo per i clienti vari
092990051212     c                   If        v6cdmg = 'SI' and
093000051212     c                             (wksc04 = 8888 or wksc04 = 9999)
093010051212     c                   Eval      *In28 = *On
093020051212     c                   Eval      h6riga = 10
093030051212     c                   Eval      h6colo = 38
093040051212     c                   Eval      v6cmsg = msg(66)
093050051212     c                   Leavesr
093060051212     c                   EndIf
093070051207
093080051207     c                   EndSr
093090060801
093100060801      *------------------------------------------------------------------------*
093110060801      * CONTROLLO CHIUSURA AUTOMATICA GIACENZA
093120060801      *------------------------------------------------------------------------*
093130060801     c     Sr_Ctrcag     BegSr
093140060801
093150060801      * Non ammessa chiusura automatica giacenza per i clienti vari
093160060801     c                   If        v6ccag = 'SI' and
093170060801     c                             (wksc04 = 8888 or wksc04 = 9999)
093180060801     c                   Eval      *In28 = *On
093190060801     c                   Eval      h6riga = 11
093200060801     c                   Eval      h6colo = 38
093210060801     c                   Eval      v6cmsg = msg(74)
093220060801     c                   Leavesr
093230060801     c                   EndIf
093240060801
093250060801     c                   EndSr
093260051207
093270051207      *------------------------------------------------------------------------*
093280051220      * CONTROLLO LUOGO 005 - LUOGO SPEDIZIONE GIACENZA
093290051207      *------------------------------------------------------------------------*
093300051207     c     Sr_Ctrl005    BegSr
093310051212
093320051212if  1c                   If        v6cl005 <> *Blanks
093330060203
093340060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
093350060203     c                   If        *In05 and Not *In14
093360051212     c                   Eval      *In28 = *On
093370051212     c                   Eval      h6riga = 13
093380060203     c                   Eval      h6colo = 35
093390051212     c                   Eval      v6cmsg = msg(50)
093400051212     c                   Leavesr
093410051212     c                   EndIf
093420071128
093430071128      * se non è interrogazione controllo se luogo utilizzabile
093440071128     c                   if        not *in09 and not *in05 and $ufi005 <> 'S'
093450071128     c                   eval      *In28 = *on
093460071128     c                   eval      h6riga = 13
093470071128     c                   eval      h6colo = 35
093480071128     c                   eval      v6cmsg = msg(76)
093490071128     c                   leavesr
093500071128     c                   endif
093510051212
093520060203      * Imposto se interrogazione o gestione
093530051213     c                   Clear                   tnta81ds
093540060203     c   05              Eval      i81opz = '5'
093550060203     c  n05
093560060214     can 14              Eval      i81opz = '2'
093570051213     c                   Eval      i81cod = '005'
093580051213     c                   Eval      i81cli = v1cksc
093590060216     c                   Eval      i81gcli = '1'
093600060216     c   01              Eval      i81icli = '1'
093610051213     c                   Call      'TNTA81R'
093620051213     c                   Parm                    kpjba
093630051213     c                   Parm                    tnta81ds
093640051212
093650051212      * Controllo se ora c'è o non c'è più il luogo
093660060112     c                   Clear                   wfax005
093670060112     c                   Clear                   wmail005
093680051216     c                   Eval      kspecli = v1cksc
093690051216     c                   Eval      kspecod = '005'
093700051212     c     kFnspe        Chain     Fnspe01l
093710051212     c                   If        %Found(Fnspe01l) and speflg = *Blanks
093720051212     c                   Eval      *In14 = *On
093730101026     c                   Eval      wfax005 = spefax
093740101026     c     kFnsp2        Chain     Fnsp201l
093750101026     c                   If        %Found(Fnsp201l) and sp2flg = *Blanks
093760101026     c                   Eval      wmail005 = sp2est
093770101026     c                   EndIf
093780051212     c                   Else
093790051212     c                   Eval      *In14 = *Off
093800051212     c                   EndIf
093810051212
093820051212     c                   Clear                   v6cl005
093830051212     c                   Eval      h6riga = 13
093840060203     c                   Eval      h6colo = 35
093850051212     c                   Goto      emi06
093860051212
093870051212    1c                   EndIf
093880051207
093890051207     c                   EndSr
093900051207
093910051207      *------------------------------------------------------------------------*
093920051220      * CONTROLLO NOTA 88 - E-MAIL GIACENZE
093930051207      *------------------------------------------------------------------------*
093940051207     c     Sr_Ctrnt88    BegSr
093950051212
093960051212if  1c                   If        v6cnt88 <> *Blanks
093970060203
093980060203      * Se sono in interrogazione non posso interrogare la nota se non c'è
093990060203     c                   If        *In05 and Not *In15
094000051212     c                   Eval      *In28 = *On
094010051212     c                   Eval      h6riga = 14
094020060203     c                   Eval      h6colo = 35
094030051219     c                   Eval      v6cmsg = msg(52)
094040051212     c                   Leavesr
094050051212     c                   EndIf
094060051212
094070060203      * Imposto se interrogazione o gestione
094080060203     c  n05              Eval      wrich = 'M'
094090060203     c   05              Eval      wrich = 'V'
094100060217     c                   Eval      wrag = v3crag
094110051212     c                   Eval      wtnt = '88'
094120051212     c                   ExSr      Sr_f02
094130060215
094140060215     c                   If        ta12err <> *Blanks
094150060215     c                   Eval      *In28 = *On
094160060215     c                   Eval      h6riga = 14
094170060215     c                   Eval      h6colo = 35
094180060215     c                   Eval      v6cmsg = ta12msg
094190060215     c                   Leavesr
094200060215     c                   EndIf
094210051212
094220060215      * Controllo se ora c'è o non c'è più la nota
094230091112     c   70
094240091112     corn06              Eval      kntcapl = 'C'
094250100806     c   06
094260091112     cann70              Eval      kntcapl = 'T'
094270051216     c                   Movel(p)  dutkci        kntcnk1
094280091112     c   70
094290091112     corn06              Move      v1cksc        kntcnk1
094300100806     c   06
094310100806     cann70              Move      v1cnrv        kntcnk1
094320051216     c                   Clear                   kntcnk2
094330051216     c                   Movel     '88'          kntctnt
094340091119     c     kTfntc        Chain(n)  Tfntc01l
094350051212     c                   If        %Found(Tfntc01l)
094360051212     c                   Eval      *In15 = *On
094370051212     c                   Else
094380051212     c                   Eval      *In15 = *Off
094390051212     c                   EndIf
094400051212
094410051212     c                   Clear                   v6cnt88
094420051212     c                   Eval      h6riga = 14
094430060203     c                   Eval      h6colo = 35
094440051212     c                   Goto      emi06
094450051212
094460051212    1c                   EndIf
094470051207
094480051207     c                   EndSr
094490060208
094500060208      *------------------------------------------------------------------------*
094510060208      * CONTROLLO LUOGO 300 - MERCE RESA
094520060208      *------------------------------------------------------------------------*
094530060208     c     Sr_Ctrl300    BegSr
094540060208
094550060208if  1c                   If        v6cl300 <> *Blanks
094560060208
094570060208      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
094580060208     c                   If        *In05 and Not *In33
094590060208     c                   Eval      *In28 = *On
094600060208     c                   Eval      h6riga = 15
094610060208     c                   Eval      h6colo = 35
094620060208     c                   Eval      v6cmsg = msg(50)
094630060208     c                   Leavesr
094640060208     c                   EndIf
094650071128
094660071128      * se non è interrogazione controllo se luogo utilizzabile
094670071128     c                   if        not *in09 and not *in05 and $ufi300 <> 'S'
094680071128     c                   eval      *In28 = *on
094690071128     c                   eval      h6riga = 15
094700071128     c                   eval      h6colo = 35
094710071128     c                   eval      v6cmsg = msg(76)
094720071128     c                   leavesr
094730071128     c                   endif
094740060208
094750060208      * Imposto se interrogazione o gestione
094760060208     c                   Clear                   tnta81ds
094770060208     c   05              Eval      i81opz = '5'
094780060208     c  n05
094790060214     can 33              Eval      i81opz = '2'
094800060208     c                   Eval      i81cod = '300'
094810060208     c                   Eval      i81cli = v1cksc
094820060216     c                   Eval      i81gcli = '1'
094830060216     c   01              Eval      i81icli = '1'
094840060208     c                   Call      'TNTA81R'
094850060208     c                   Parm                    kpjba
094860060208     c                   Parm                    tnta81ds
094870060208
094880060208      * Controllo se ora c'è o non c'è più il luogo
094890060208     c                   Eval      kspecli = v1cksc
094900060208     c                   Eval      kspecod = '300'
094910060208     c     kFnspe        Chain     Fnspe01l
094920060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
094930060208     c                   Eval      *In33 = *On
094940060208     c                   Else
094950060208     c                   Eval      *In33 = *Off
094960060208     c                   EndIf
094970060208
094980060208     c                   Clear                   v6cl300
094990060208     c                   Eval      h6riga = 15
095000060208     c                   Eval      h6colo = 35
095010060208     c                   Goto      emi06
095020060208
095030060208    1c                   EndIf
095040060208
095050060208     c                   EndSr
095060051213
095070051213      *------------------------------------------------------------------------*
095080051213      * CONTROLLO LUOGO 006 - PREAVVISO/AVVISO DANNO
095090051213      *------------------------------------------------------------------------*
095100051213     c     Sr_Ctrl006    BegSr
095110051213
095120051213if  1c                   If        v7cl006 <> *Blanks
095130060203
095140060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
095150060203     c                   If        *In05 and Not *In16
095160051213     c                   Eval      *In28 = *On
095170051213     c                   Eval      h7riga = 12
095180060203     c                   Eval      h7colo = 35
095190051213     c                   Eval      v7cmsg = msg(50)
095200051213     c                   Leavesr
095210051213     c                   EndIf
095220071128
095230071128      * se non è interrogazione controllo se luogo utilizzabile
095240071128     c                   if        not *in09 and not *in05 and $ufi006 <> 'S'
095250071128     c                   eval      *In28 = *on
095260071128     c                   eval      h7riga = 12
095270071128     c                   eval      h7colo = 35
095280071128     c                   eval      v7cmsg = msg(76)
095290071128     c                   leavesr
095300071128     c                   endif
095310051213
095320060203      * Imposto se interrogazione o gestione
095330051213     c                   Clear                   tnta81ds
095340060203     c   05              Eval      i81opz = '5'
095350060203     c  n05
095360060214     can 16              Eval      i81opz = '2'
095370051213     c                   Eval      i81cod = '006'
095380051213     c                   Eval      i81cli = v1cksc
095390060216     c                   Eval      i81gcli = '1'
095400060216     c   01              Eval      i81icli = '1'
095410051213     c                   Call      'TNTA81R'
095420051213     c                   Parm                    kpjba
095430051213     c                   Parm                    tnta81ds
095440051213
095450051213      * Controllo se ora c'è o non c'è più il luogo
095460051216     c                   Eval      kspecli = v1cksc
095470051216     c                   Eval      kspecod = '006'
095480051213     c     kFnspe        Chain     Fnspe01l
095490051213     c                   If        %Found(Fnspe01l) and speflg = *Blanks
095500051213     c                   Eval      *In16 = *On
095510051213     c                   Else
095520051213     c                   Eval      *In16 = *Off
095530051213     c                   EndIf
095540051213
095550051213     c                   Clear                   v7cl006
095560051213     c                   Eval      h7riga = 12
095570060203     c                   Eval      h7colo = 35
095580051213     c                   Goto      emi07
095590051213
095600051213    1c                   EndIf
095610051213
095620051213     c                   EndSr
095630051213
095640051213      *------------------------------------------------------------------------*
095650051220      * CONTROLLO NOTA 87 - E-MAIL DANNI
095660051213      *------------------------------------------------------------------------*
095670051213     c     Sr_Ctrnt87    BegSr
095680051213
095690051213if  1c                   If        v7cnt87 <> *Blanks
095700060203
095710060203      * Se sono in interrogazione non posso interrogare la nota se non c'è
095720060203     c                   If        *In05 and Not *In18
095730051213     c                   Eval      *In28 = *On
095740051213     c                   Eval      h7riga = 13
095750060203     c                   Eval      h7colo = 35
095760051219     c                   Eval      v7cmsg = msg(52)
095770051213     c                   Leavesr
095780051213     c                   EndIf
095790051213
095800060203      * Imposto se interrogazione o gestione
095810060203     c  n05              Eval      wrich = 'M'
095820060203     c   05              Eval      wrich = 'V'
095830060217     c                   Eval      wrag = v3crag
095840051213     c                   Eval      wtnt = '87'
095850051213     c                   ExSr      Sr_f02
095860060215
095870060215     c                   If        ta12err <> *Blanks
095880060215     c                   Eval      *In28 = *On
095890060215     c                   Eval      h7riga = 13
095900060215     c                   Eval      h7colo = 35
095910060215     c                   Eval      v7cmsg = ta12msg
095920060215     c                   Leavesr
095930060215     c                   EndIf
095940051213
095950060215      * Controllo se ora c'è o non c'è più la nota
095960091112     c   70
095970091112     corn06              Eval      kntcapl = 'C'
095980100806     c   06
095990091112     cann70              Eval      kntcapl = 'T'
096000051216     c                   Movel(p)  dutkci        kntcnk1
096010091112     c   70
096020091112     corn06              Move      v1cksc        kntcnk1
096030100806     c   06
096040100806     cann70              Move      v1cnrv        kntcnk1
096050051216     c                   Clear                   kntcnk2
096060051216     c                   Movel     '87'          kntctnt
096070091119     c     kTfntc        Chain(n)  Tfntc01l
096080051213     c                   If        %Found(Tfntc01l)
096090051213     c                   Eval      *In18 = *On
096100051213     c                   Else
096110051213     c                   Eval      *In18 = *Off
096120051213     c                   EndIf
096130051213
096140051213     c                   Clear                   v7cnt87
096150051213     c                   Eval      h7riga = 13
096160060203     c                   Eval      h7colo = 35
096170051213     c                   Goto      emi07
096180051213
096190051213    1c                   EndIf
096200051213
096210051213     c                   EndSr
096220051213
096230051213      *------------------------------------------------------------------------*
096240051213      * CONTROLLO LUOGO 008 - PROGETTO DI LIQUIDAZIONE
096250051213      *------------------------------------------------------------------------*
096260051213     c     Sr_Ctrl008    BegSr
096270051213
096280051213if  1c                   If        v7cl008 <> *Blanks
096290060203
096300060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
096310060203     c                   If        *In05 and Not *In17
096320051213     c                   Eval      *In28 = *On
096330051213     c                   Eval      h7riga = 14
096340060203     c                   Eval      h7colo = 35
096350051213     c                   Eval      v7cmsg = msg(50)
096360051213     c                   Leavesr
096370051213     c                   EndIf
096380071128
096390071128      * se non è interrogazione controllo se luogo utilizzabile
096400071128     c                   if        not *in09 and not *in05 and $ufi008 <> 'S'
096410071128     c                   eval      *In28 = *on
096420071128     c                   eval      h7riga = 14
096430071128     c                   eval      h7colo = 35
096440071128     c                   eval      v7cmsg = msg(76)
096450071128     c                   leavesr
096460071128     c                   endif
096470051213
096480060203      * Imposto se interrogazione o gestione
096490051213     c                   Clear                   tnta81ds
096500060203     c   05              Eval      i81opz = '5'
096510060203     c  n05
096520060214     can 17              Eval      i81opz = '2'
096530051213     c                   Eval      i81cod = '008'
096540051213     c                   Eval      i81cli = v1cksc
096550060216     c                   Eval      i81gcli = '1'
096560060216     c   01              Eval      i81icli = '1'
096570051213     c                   Call      'TNTA81R'
096580051213     c                   Parm                    kpjba
096590051213     c                   Parm                    tnta81ds
096600051213
096610051213      * Controllo se ora c'è o non c'è più il luogo
096620051216     c                   Eval      kspecli = v1cksc
096630051216     c                   Eval      kspecod = '008'
096640051213     c     kFnspe        Chain     Fnspe01l
096650051213     c                   If        %Found(Fnspe01l) and speflg = *Blanks
096660051213     c                   Eval      *In17 = *On
096670051213     c                   Else
096680051213     c                   Eval      *In17 = *Off
096690051213     c                   EndIf
096700051213
096710051213     c                   Clear                   v7cl008
096720051213     c                   Eval      h7riga = 14
096730060203     c                   Eval      h7colo = 35
096740051213     c                   Goto      emi07
096750051213
096760051213    1c                   EndIf
096770051213
096780051213     c                   EndSr
096790051213
096800051213      *------------------------------------------------------------------------*
096810051213      * CONTROLLO NUMERO STOP PER RITIRO
096820051213      *------------------------------------------------------------------------*
096830051213     c     Sr_Ctrstp     BegSr
096840051213
096850051213     c                   If        v7cstp > 800
096860051213     c                   Eval      *In28 = *On
096870051213     c                   Eval      h7riga = 18
096880051213     c                   Eval      h7colo = 37
096890051213     c                   Eval      v7cmsg = msg(67)
096900051213     c                   Leavesr
096910051213     c                   EndIf
096920051213
096930051213     c                   EndSr
096940060208
096950060208      *------------------------------------------------------------------------*
096960060208      * CONTROLLO LUOGO 002 - INVIO ORM ESTERO
096970060208      *------------------------------------------------------------------------*
096980060208     c     Sr_Ctrl002    BegSr
096990060208
097000060208if  1c                   If        v7cl002 <> *Blanks
097010060208
097020060208      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
097030060208     c                   If        *In05 and Not *In35
097040060208     c                   Eval      *In28 = *On
097050060208     c                   Eval      h7riga = 20
097060060208     c                   Eval      h7colo = 35
097070060208     c                   Eval      v7cmsg = msg(50)
097080060208     c                   Leavesr
097090060208     c                   EndIf
097100071128
097110071128      * se non è interrogazione controllo se luogo utilizzabile
097120071128     c                   if        not *in09 and not *in05 and $ufi002 <> 'S'
097130071128     c                   eval      *In28 = *on
097140071128     c                   eval      h7riga = 20
097150071128     c                   eval      h7colo = 35
097160071128     c                   eval      v7cmsg = msg(76)
097170071128     c                   leavesr
097180071128     c                   endif
097190060208
097200060208      * Imposto se interrogazione o gestione
097210060208     c                   Clear                   tnta81ds
097220060208     c   05              Eval      i81opz = '5'
097230060208     c  n05
097240060214     can 35              Eval      i81opz = '2'
097250060208     c                   Eval      i81cod = '002'
097260060208     c                   Eval      i81cli = v1cksc
097270060216     c                   Eval      i81gcli = '1'
097280060216     c   01              Eval      i81icli = '1'
097290060208     c                   Call      'TNTA81R'
097300060208     c                   Parm                    kpjba
097310060208     c                   Parm                    tnta81ds
097320060208
097330060208      * Controllo se ora c'è o non c'è più il luogo
097340060208     c                   Eval      kspecli = v1cksc
097350060208     c                   Eval      kspecod = '002'
097360060208     c     kFnspe        Chain     Fnspe01l
097370060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
097380060208     c                   Eval      *In35 = *On
097390060208     c                   Else
097400060208     c                   Eval      *In35 = *Off
097410060208     c                   EndIf
097420060208
097430060208     c                   Clear                   v7cl002
097440060208     c                   Eval      h7riga = 20
097450060208     c                   Eval      h7colo = 35
097460060208     c                   Goto      emi07
097470060208
097480060208    1c                   EndIf
097490060208
097500060208     c                   EndSr
097510060208
097520060208      *------------------------------------------------------------------------*
097530060208      * CONTROLLO NOTA 83 - E-MAIL ORM ESTERO
097540060208      *------------------------------------------------------------------------*
097550060208     c     Sr_Ctrnt83    BegSr
097560060208
097570060208if  1c                   If        v7cnt83 <> *Blanks
097580060208
097590060208      * Se sono in interrogazione non posso interrogare la nota se non c'è
097600060208     c                   If        *In05 and Not *In34
097610060208     c                   Eval      *In28 = *On
097620060208     c                   Eval      h7riga = 21
097630060208     c                   Eval      h7colo = 35
097640060208     c                   Eval      v7cmsg = msg(52)
097650060208     c                   Leavesr
097660060208     c                   EndIf
097670060208
097680060208      * Imposto se interrogazione o gestione
097690060208     c  n05              Eval      wrich = 'M'
097700060208     c   05              Eval      wrich = 'V'
097710060217     c                   Eval      wrag = v3crag
097720060208     c                   Eval      wtnt = '83'
097730060208     c                   ExSr      Sr_f02
097740060215
097750060215     c                   If        ta12err <> *Blanks
097760060215     c                   Eval      *In28 = *On
097770060215     c                   Eval      h7riga = 21
097780060215     c                   Eval      h7colo = 35
097790060215     c                   Eval      v7cmsg = ta12msg
097800060215     c                   Leavesr
097810060215     c                   EndIf
097820060208
097830060215      * Controllo se ora c'è o non c'è più la nota
097840091112     c   70
097850091112     corn06              Eval      kntcapl = 'C'
097860100806     c   06
097870091112     cann70              Eval      kntcapl = 'T'
097880060208     c                   Movel(p)  dutkci        kntcnk1
097890091112     c   70
097900091112     corn06              Move      v1cksc        kntcnk1
097910100806     c   06
097920100806     cann70              Move      v1cnrv        kntcnk1
097930060208     c                   Clear                   kntcnk2
097940060208     c                   Movel     '83'          kntctnt
097950091119     c     kTfntc        Chain(n)  Tfntc01l
097960060208     c                   If        %Found(Tfntc01l)
097970060208     c                   Eval      *In34 = *On
097980060208     c                   Else
097990060208     c                   Eval      *In34 = *Off
098000060208     c                   EndIf
098010060208
098020060208     c                   Clear                   v7cnt83
098030060208     c                   Eval      h7riga = 21
098040060208     c                   Eval      h7colo = 35
098050060208     c                   Goto      emi07
098060060208
098070060208    1c                   EndIf
098080060208
098090060208     c                   EndSr
098100060208
098110060208      *------------------------------------------------------------------------*
098120060208      * CONTROLLO LUOGO 200 - INVIO DOC. DOGANA
098130060208      *------------------------------------------------------------------------*
098140060208     c     Sr_Ctrl200    BegSr
098150060208
098160060208if  1c                   If        v7cl200 <> *Blanks
098170060208
098180060208      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
098190060208     c                   If        *In05 and Not *In37
098200060208     c                   Eval      *In28 = *On
098210060208     c                   Eval      h7riga = 20
098220060208     c                   Eval      h7colo = 72
098230060208     c                   Eval      v7cmsg = msg(50)
098240060208     c                   Leavesr
098250060208     c                   EndIf
098260071128
098270071128      * se non è interrogazione controllo se luogo utilizzabile
098280071128     c                   if        not *in09 and not *in05 and $ufi200 <> 'S'
098290071128     c                   eval      *In28 = *on
098300071128     c                   eval      h7riga = 20
098310071128     c                   eval      h7colo = 72
098320071128     c                   eval      v7cmsg = msg(76)
098330071128     c                   leavesr
098340071128     c                   endif
098350060208
098360060208      * Imposto se interrogazione o gestione
098370060208     c                   Clear                   tnta81ds
098380060208     c   05              Eval      i81opz = '5'
098390060208     c  n05
098400060214     can 37              Eval      i81opz = '2'
098410060208     c                   Eval      i81cod = '200'
098420060208     c                   Eval      i81cli = v1cksc
098430060216     c                   Eval      i81gcli = '1'
098440060216     c   01              Eval      i81icli = '1'
098450060208     c                   Call      'TNTA81R'
098460060208     c                   Parm                    kpjba
098470060208     c                   Parm                    tnta81ds
098480060208
098490060208      * Controllo se ora c'è o non c'è più il luogo
098500060208     c                   Eval      kspecli = v1cksc
098510060208     c                   Eval      kspecod = '200'
098520060208     c     kFnspe        Chain     Fnspe01l
098530060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
098540060208     c                   Eval      *In37 = *On
098550060208     c                   Else
098560060208     c                   Eval      *In37 = *Off
098570060208     c                   EndIf
098580060208
098590060208     c                   Clear                   v7cl200
098600060208     c                   Eval      h7riga = 20
098610060208     c                   Eval      h7colo = 72
098620060208     c                   Goto      emi07
098630060208
098640060208    1c                   EndIf
098650060208
098660060208     c                   EndSr
098670060208
098680060208      *------------------------------------------------------------------------*
098690060208      * CONTROLLO NOTA 86 - E-MAIL MANIFEST
098700060208      *------------------------------------------------------------------------*
098710060208     c     Sr_Ctrnt86    BegSr
098720060208
098730060208if  1c                   If        v7cnt86 <> *Blanks
098740060208
098750060208      * Se sono in interrogazione non posso interrogare la nota se non c'è
098760060208     c                   If        *In05 and Not *In36
098770060208     c                   Eval      *In28 = *On
098780060208     c                   Eval      h7riga = 21
098790060208     c                   Eval      h7colo = 72
098800060208     c                   Eval      v7cmsg = msg(52)
098810060208     c                   Leavesr
098820060208     c                   EndIf
098830060208
098840060208      * Imposto se interrogazione o gestione
098850060208     c  n05              Eval      wrich = 'M'
098860060208     c   05              Eval      wrich = 'V'
098870060217     c                   Eval      wrag = v3crag
098880060208     c                   Eval      wtnt = '86'
098890060208     c                   ExSr      Sr_f02
098900060215
098910060215     c                   If        ta12err <> *Blanks
098920060215     c                   Eval      *In28 = *On
098930060215     c                   Eval      h7riga = 21
098940060215     c                   Eval      h7colo = 72
098950060215     c                   Eval      v7cmsg = ta12msg
098960060215     c                   Leavesr
098970060215     c                   EndIf
098980060208
098990060215      * Controllo se ora c'è o non c'è più la nota
099000091112     c   70
099010091112     corn06              Eval      kntcapl = 'C'
099020100806     c   06
099030091112     cann70              Eval      kntcapl = 'T'
099040060208     c                   Movel(p)  dutkci        kntcnk1
099050091112     c   70
099060091112     corn06              Move      v1cksc        kntcnk1
099070100806     c   06
099080100806     cann70              Move      v1cnrv        kntcnk1
099090060208     c                   Clear                   kntcnk2
099100060208     c                   Movel     '86'          kntctnt
099110091119     c     kTfntc        Chain(n)  Tfntc01l
099120060208     c                   If        %Found(Tfntc01l)
099130060208     c                   Eval      *In36 = *On
099140060208     c                   Else
099150060208     c                   Eval      *In36 = *Off
099160060208     c                   EndIf
099170060208
099180060208     c                   Clear                   v7cnt86
099190060208     c                   Eval      h7riga = 21
099200060208     c                   Eval      h7colo = 72
099210060208     c                   Goto      emi07
099220060208
099230060208    1c                   EndIf
099240060208
099250060208     c                   EndSr
099260051220
099270051220      *-------------------------------------------------------------------------
099280051220      * CONTROLLO IL BLOCCO/SBLOCCO DEL CLIENTE
099290051220      *-------------------------------------------------------------------------
099300051220     c     Sr_Ctrabl     BegSr
099310051220
099320051220     c                   Eval      wokabl = *Off
099330051220     c                   Clear                   w02tes
099340051220     c                   Clear                   w02msg
099350051220
099360051220      * se ho aggiornato blocco/sblocco i clienti di fnacr
099370051220if  1c                   If        acoabl <> *Blanks and savabl = *Blanks
099380051220     c                   Eval      w02tes = 'Il cliente è stato bloccato'
099390051220      * cliente bloccato richiamo pgm esterno per bloccare fnacr
099400051220     c                   Clear                   fior50ds
099410051220     c                   Move      acoksc        i50ksc
099420140113     c                   eval      I50abl = 'B'
099430051220     c                   Call      'FIOR50R'
099440051220     c                   Parm                    fior50ds
099450051220      * se torna errore emetto il msg di errore
099460051220if  2c                   If        o50err <> *Blanks
099470051220     c                   Eval      w02msg = o50msg
099480051220      * emetto il msg che ho bloccato i clienti su fnacr
099490051220   x2c                   Else
099500051220     c                   Eval      w02msg = 'Anagrafiche clienti ritiro collega-
099510051220     c                             te bloccate'
099520051220    2c                   EndIf
099530051220     c                   Eval      wokabl = *On
099540051220    1c                   EndIf
099550051220if  1c                   If        acoabl = *Blanks and savabl <> *Blanks
099560051220     c                   Eval      w02tes = 'Il cliente è stato sbloccato'
099570051220      * cliente sbloccato emetto solo il msg
099580051220     c                   Eval      w02msg = 'Anagrafiche clienti ritiro collega-
099590051220     c                             te non aggiornate'
099600051220     c                   Eval      wokabl = *On
099610051220    1c                   EndIf
099620051220
099630051220      * Emetto la window x avvisare l'utente
099640051220     c                   If        wokabl = *On
099650051220     c                   Exfmt     ta60w02
099660051220     c                   EndIf
099670051220
099680051220     c                   EndSr
099690100128
099700100128      *-------------------------------------------------------------------------
099710100128      * Richiamo interrogazione codici fatturazione con stessa p.IVA
099720100128      *-------------------------------------------------------------------------
099730100128     c     sr_ta40       begsr
099740100128
099750100128     c                   clear                   tnta40ds
099760100128     c                   IF        v2ivaeu = *blanks
099770100128     c                   eval      ITA40iva = v2ivait
099780100128     c                   else
099790100128     c                   eval      ITA40iva = v2civa
099800100128     c                   ENDIF
099810100128     c  n06              eval      ITA40ksc = v1cksc
099820100128     c   06              eval      ITA40ksc = ta60cli
099830100128     c                   eval      ITA40scf = clpscf
099840100128     c                   eval      ITA40rag = v2crag
099850100128     c                   call      'TNTA40R'
099860100128     c                   parm                    kpjba
099870100128     c                   parm                    tnta40ds
099880100128     c                   IF        OTA40err <> *blanks
099890100128     c                   eval      v4cmsg = OTA40msg
099900100128     c                   eval      *in28 = *on
099910100312     c                   leavesr
099920100128     c                   ENDIF
099930100312     c                   IF        OTA40scf = 0
099940100312     c                   Eval      v4cmsg = msg(33)
099950100312     c                   eval      *in28 = *on
099960100312     c                   leavesr
099970100312     c                   ENDIF
099980100128     c                   eval      v4cscf = %editc(OTA40scf:'X')
099990100128     c                   eval      wtnta40 = *on
100000100201     c                   clear                   v4dscf
100010100201     c                   Clear                   Tibs69ds
100020100201     c                   Move      v4cscf        I69kac
100030100201     c                   Call      'TIBS69R'
100040100201     c                   Parm                    Tibs69ds
100050100201     c                   Parm                    ds_cnaco
100060100201     c                   Parm                    ds_cnind
100070100201     c                   Parm                    ds_cnclp
100080100201     c                   Parm                    ds_fncls
100090100201     c                   Eval      wtibs69 = *On
100100100201     c                   Movel     w_acorag      v4dscf
100110100128
100120100128     c                   endsr
100130051216
100140080222      *-------------------------------------------------------------------------
100150080222      * Reperisco la nazione del sottoconto intestazione fattura
100160080222      *-------------------------------------------------------------------------
100170080222     c     Sr_Repscfsta  BegSr
100180080222
100190080222     c                   clear                   w_scfsta
100200080222      * Controllo
100210100127     c                   if        v4cscf <> %editc(v1cksc:'X')
100220110427     c                             and v4cscf > *zeros
100230080222     c                   Clear                   Tibs69ds
100240080222     c                   Move      v4cscf        I69kin
100250080222     c                   Call      'TIBS69R'
100260080222     c                   Parm                    Tibs69ds
100270080222     c                   Parm                    ds_cnaco
100280080222     c                   Parm                    ds_cnind
100290080222     c                   Parm                    ds_cnclp
100300080222     c                   Parm                    ds_fncls
100310080222     c                   Eval      wtibs69 = *On
100320080307     c                   if        o69err=*blanks and w_indflg=*blanks
100330080222     c                   eval      w_scfsta=w_indsta
100340080222     c                   endif
100350080227     c                   else
100360080227     c                   eval      w_scfsta=v2csta
100370080227     c                   endif
100380080222
100390080222     c                   EndSr
100400080306      *------------------------------------------------------------------------*
100410080306      * Controllo presenza codice su altro potenziale come cod.fiscale
100420080306      *------------------------------------------------------------------------*
100430080306     c     Sr_CdfAltroP  BegSr
100440080306     c     codice        setll     tncpo05l
100450080306     c     codice        reade     tncpo05l
100460080306    3c                   dow       not %eof(tncpo05l)
100470080306     c* se trovato un record "filiale memorizzo ragione sociale del potenz.
100480080306     c                   eval      savrag=c_cporag
100490080306     c     codice        reade     tncpo05l
100500080306    3c                   enddo
100510080306    3c                   if        not %eof (tncpo05l)
100520080306     c                   eval      savrag=c_cporag
100530080306    3c                   endif
100540080306     c                   endsr
100550080306      *------------------------------------------------------------------------*
100560080306      * Controllo presenza codice su altro potenziale come piva
100570080306      *------------------------------------------------------------------------*
100580080306     c     Sr_PivAltroP  BegSr
100590080306     c     codice        setll     tncpo06l
100600080306     c     codice        reade     tncpo06l
100610080306    3c                   dow       not %eof(tncpo06l)
100620080306     c* se trovato un record "filiale memorizzo ragione sociale del potenz.
100630080306     c                   eval      savrag=c_cporag
100640080306     c     codice        reade     tncpo06l
100650080306    3c                   enddo
100660080306    3c                   if        not %eof (tncpo06l)
100670080306     c                   eval      savrag=c_cporag
100680080306    3c                   endif
100690080306     c                   endsr
100700080306      *------------------------------------------------------------------------*
100710080306      * Messaggio di errore codice fiscal già presente per altro potenz
100720080306      *------------------------------------------------------------------------*
100730080306     c     Sr_msgcdf     BegSr
100740080306     c* Errore forzabile se trovato un altro potenziale con lo stesso
100750080306     c* codice fiscale
100760080306    3c                   if        savrag<>*blanks
100770080306     c                   eval      cod_forzcdf=codice
100780080306     c                   seton                                        28
100790080306     c                   eval      h2riga = 12
100800080306     c                   eval      h2colo = 28
100810080306     c                   Eval      v2cmsg = msg(77)
100820080306     c                   Leavesr
100830080306    3c                   endif
100840080306     c                   endsr
100850080306      *------------------------------------------------------------------------*
100860080306      * Messaggio di errore partita iva   già presente per altro potenz
100870080306      *------------------------------------------------------------------------*
100880080306     c     Sr_msgiva     BegSr
100890080306
100900080306    3c                   if        savrag<>*blanks
100910080306     c                   eval      cod_forziva=v2civa
100920080306     c                   seton                                        28
100930080306     c                   Eval      h2riga = 12
100940080306     c                   Eval      h2colo = 52
100950080306     c                   Eval      v2cmsg = msg(78)
100960080306     c                   Leavesr
100970080306    3c                   endif
100980080306     c                   endsr
100990080422
101000080422      *------------------------------------------------------------------------*
101010080422      * Emissione Window per inviare mail alla sede
101020080422      *------------------------------------------------------------------------*
101030080422     c     sr_winmail    begsr
101040080422
101050100729     c                   eval      w04uffsede = wuffsede
101060080422Do  2c                   do        *hival
101070080422     c                   exfmt     ta60w04
101080080422If  3c                   if        *inkl
101090080422     c                   leavesr
101100080422    3c                   endif
101110080422    2c                   enddo
101120080422
101130080422     c                   endsr
101140100729      /free
101150100729       //--------------------------------------------------------------
101160100729       //?Descrizione banca.
101170100729       //--------------------------------------------------------------
101180100729       BEGSR DesBanca;
101190100729
101200100729         clear wDesBanca;
101210100729         chain (kabi:kcab) CNABI00F;
101220100729         IF  %Found(CNABI00F) and ABIann <> '*';
101230100729           exsr sr_crebna;
101240100729           wDesbanca = wbanca + wagenzia;
101250100729         ENDIF;
101260100729
101270100729       ENDSR;
101280150427
101290150427       //--------------------------------------------------------------
101300150427       //?Memorizza immagine precedente.
101310150427       //--------------------------------------------------------------
101320150427       BEGSR ImmaginePrima;
101330150427
101340150427         clear TIBS73DS;
101350150427         IBS73immag ='P';
101360150427         tibs73r (TIBS73DS:cnaco_73:cnind_73:cnclp_73:fncls_73);
101370150427         wtibs73 = *on;
101380150427
101390150427       ENDSR;
101400150427
101410150427       //--------------------------------------------------------------
101420150427       //?Memorizza immagine sucessiva.
101430150427       //--------------------------------------------------------------
101440150427       BEGSR ImmagineDopo;
101450150427
101460150427         clear TIBS73DS;
101470150427         IBS73pru = knmus;
101480150427         IBS73noj = knmeb;
101490150427         ibs73pgm = 'TNTA60R';
101500150427         IBS73immag ='D';
101510150427         IF  *in02;
101520150427           IBS73cta = 'M';
101530150427         ENDIF;
101540150427         IF  *in01;
101550150427           IBS73cta = 'I';
101560150427         ENDIF;
101570150427         tibs73r (TIBS73DS:cnaco_73:cnind_73:cnclp_73:fncls_73);
101580150427         wtibs73 = *on;
101590150427
101600150427       ENDSR;
101610160803
101620160803       //--------------------------------------------------------------
101630160803       //?Scrivo l'immagine Precedente.
101640160803       //--------------------------------------------------------------
101650160803       BEGSR ImmaginePrimaCpo;
101660160803
101670160803         clear TRMK30DS;
101680160803         IMK30immag = 'P';
101690160803         IMK30tvp = 'B';
101700160803         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
101710160803
101720160803       ENDSR;
101730160803
101740160803       //--------------------------------------------------------------
101750160803       //?Scrivo Storico Variazioni.
101760160803       //--------------------------------------------------------------
101770160803       BEGSR ScriviVarCpo;
101780160803
101790160803         clear TRMK30DS;
101800160803         IMK30pru = knmus;
101810160803         IMK30noj = knmeb;
101820160803         IMK30pgm = 'TNTA60R';
101830160803         IMK30immag = 'D';
101840160803         IMK30cta = 'M';
101850160803         IMK30tvp = 'B';
101860160803
101870160803         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
101880160803
101890160803       ENDSR;
101900150427
101910100729      /end-free
101920080222
101930051107      *------------------------------------------------------------------------*
101940051107      * ROUTINE INIZIALE
101950051107      *------------------------------------------------------------------------*
101960051107     c     *Inzsr        BegSr
101970051107
101980051107     c     *Entry        Plist
101990051107     c                   Parm                    Kpjba
102000051115     c                   Parm                    Tnta60ds
102010110407     c                   Parm                    ParmCbl
102020091104
102030091104     c                   eval      $interroga = *off
102040091124     c                   eval      $modifica  = *off
102050091106     c                   eval      $pgmrich   = *off
102060110407     c                   eval      $Blocco    = *off
102070051115
102080091104      * PGM Richiamato
102090051115     c                   If        %Parms > 1
102100091106     c                   eval      $pgmrich = *on
102110100806      * trattativa
102120100806     c                   if        ta60flg = 'T'
102130090914     c                   Eval      *In06 = *on
102140091112     c                   eval      *in70 = (ta60cli > 0)
102150091112     c   70              eval      v1cksc = ta60cli
102160091104      * cliente per aggancio a cnaco
102170090914     c                   else
102180090914     c                   eval      *in06 = *off
102190091112     c                   eval      *in70 = *off
102200090914     c                   endif
102210091104      * convalida offerta
102220091104      * sono in manutenzione perchè il cliente viene scritto dal chiamante
102230051115     c                   Eval      *In07 = (ta60flg = 'C')
102240091104      * codice visita/trattativa da agganciare
102250051222     c   06              Eval      v1cnrv = ta60nrv
102260091104      * codice cliente nuovo da agganciare
102270051222     c   07              Eval      v1cksc = ta60nrv
102280091104
102290091104      * inibisco il richiamo all'interrogazione potenziali per errore
102300091104      * chiamata ricorsiva se provengo da gestione potenziali
102310091104     c                   eval      *in65 = (ta60pot = '1')
102320091104      * interrogazione se richiamato da interrogazione tariffe
102330060207     c                   Eval      *In05 = (ta60flg = 'I')
102340091104
102350091104      * richiamato per visualizzazione (nuovo campo della DS)
102360091104     c                   if        ta60int = 'S'
102370091104     c                   eval      $interroga = *on
102380091104      * devo avere il codice cliente/visita/trattativa
102390091104      * visto che non passo dalla prima videata dove viene richiesto il codice
102400091104      * per comodità ricevo tutti e 3 i codici nello stesso campo
102410091104      * tanto o sono da visita o sono da trattativa o sono da clienti
102420091104      * se il codice è a 0 torno l'errore al chiamante
102430091104     c                   if        ta60nrv = 0
102440091104     c                   eval      ta60msg = 'Codice non passato'
102450091104     c                   eval      ta60err = '1'
102460091124     c                   Eval      *In28 = *On
102470091104     c                   leavesr
102480091104     c                   endif
102490091111      * accendo l'indicatore di interrogazione
102500091111     c                   eval      *in05 = *on
102510091111      * se flag TA60FLG = 'I'
102520091111      * imposto il codice cliente, in quanto è l'unico caso non
102530091104      * previsto nelle specifiche precedenti, visita e trattativa
102540091104      * sono già impostate se TA60FLG = 'T' o 'V'
102550091106     c                   if        ta60flg = 'I'
102560091111     c                   eval      v1cksc = ta60nrv
102570091104     c                   endif
102580091104     c                   endif
102590091124      * in caso di pgm richiamato in manutenzione
102600091124     c                   if        ta60flg = 'M'
102610091124     c                   if        ta60cli = 0
102620091124     c                   eval      ta60msg = 'Codice non passato'
102630091124     c                   eval      ta60err = '1'
102640091124     c                   Eval      *In28 = *On
102650091124     c                   leavesr
102660091124     c                   endif
102670091124     c                   eval      $modifica = *on
102680091124     c                   eval      v1cksc = ta60cli
102690091124     c                   endif
102700110407
102710110407      * PGM Richiamato per blocco cliente
102720110407     c                   IF        %Parms > 2
102730110407     c                   IF        Parmcbl = *blanks or Parmcbl = '000'
102740110407     c                   eval      ta60msg = 'Causale blocco errata'
102750110407     c                   eval      ta60err = '1'
102760110407     c                   Eval      *In28 = *On
102770110407     c                   leavesr
102780110407     c                   ENDIF
102790110407     c                   eval      $Blocco = *on
102800110407     c                   ENDIF
102810091104
102820091104      * NON richiamato
102830051115     c                   Else
102840091104      * Interrogazione
102850051115     c                   If        %subst(kpjbu:256:1) = 'V'
102860051115     c                   Eval      *In05 = *On
102870051115     c                   EndIf
102880051222     c                   Clear                   v1cnrv
102890060308     c                   eval      *in65 = *off
102900051115     c                   EndIf
102910101130
102920101130      * Se pgm in interrogazione imposto subito il titolo 'VISUALIZZA'
102930101130     c   05              Eval      vtcmod = mod(03)
102940051107
102950051107     c     *dtaara       define    §azute        azuteds
102960051107     c     *dtaara       define    §datiute      ddatiute
102970051107     c                   in(E)     *dtaara
102980051117     c                   If        %error  or rsut = *blanks
102990051107     c                   Clear                   Tibs34ds
103000051107     c                   Call      'TIBS34R'
103010051107     c                   Parm                    Tibs34ds
103020051107     c                   In        *dtaara
103030051107     c                   EndIf
103040051107
103050051107     c                   Clear                   wabi
103060051107     c                   Clear                   dLat
103070051107
103080051107      * Verifica errori e autorità profilo
103090051107s   1c                   Select
103100051107      * se ho errori nei dati utente esco dal pgm
103110051117w   1c                   When      duterr = 'E'
103120051115     c                   Eval      *In08 = *On
103130051115     c                   Eval      *In28 = *On
103140051115     c                   Eval      v1cmsg = msg(01)
103150051115      * se richiamato passo il messaggio
103160051115     c                   If        *In06
103170051115     c                   Eval      ta60msg = v1cmsg
103180051115     c                   Eval      ta60err = '1'
103190051115     c                   EndIf
103200051115     c                   Leavesr
103210051107      * se non c'è l'abilitazione
103220051107      * --> se 1° livello, abilitazioni al terminal
103230051107      *     se 2° livello, abilitazioni al punto operativo
103240051107      *     se sede è impossibile ma se capita mando a fine il pgm
103250051117w   1c                   When      uteaut = *Blanks
103260051117if  2c                   If        dutlpo = '1'
103270051107     c                   Eval      wabi   = 'TP'
103280051107e   2c                   EndIf
103290051117if  2c                   If        dutlpo = '2'
103300051107     c                   Eval      wabi   = 'PO'
103310051107e   2c                   EndIf
103320051117if  2c                   If        dutlpo = 'S'
103330051115     c                   Eval      *In08 = *On
103340051115     c                   Eval      *In28 = *On
103350051115     c                   Eval      v1cmsg = msg(01)
103360051115      * se richiamato passo il messaggio
103370051115     c                   If        *In06
103380051115     c                   Eval      ta60msg = v1cmsg
103390051115     c                   Eval      ta60err = '1'
103400051115     c                   EndIf
103410051115     c                   Leavesr
103420051107e   2c                   EndIf
103430051107      * carica le abilitazioni del profilo
103440051107x   1c                   Other
103450051117     c                   Movel     utefaf        dute01
103460051117if  2c                   If        §utecli <> *Blanks
103470051117     c                   Eval      wabi = §utecli
103480051107   x2c                   Else
103490051117     c                   Eval      wabi = uteaut
103500051107e   2c                   EndIf
103510051107e   1c                   EndSl
103520051107
103530051107      * controllo se ok l'abilitazione dell'utente
103540051107     c                   Clear                   Tibs02ds
103550051116     c                   Eval      t02mod = 'C'
103560051116     c                   Eval      t02sif = knsif
103570051116     c                   Eval      t02cod = 'LAT'
103580051116     c                   Movel(p)  wabi          t02ke1
103590051107     c                   Call      'TIBS02R'
103600051107     c                   Parm                    kpjba
103610051107     c                   Parm                    Tibs02ds
103620051117     c                   If        t02err = *Blanks
103630051116     c                   Eval      dLat = t02uni
103640051117     c                   EndIf
103650051107      * errore o non abilitato
103660051117     c                   If        t02err <> *Blanks or §latabi = 'S'
103670051107     c                   Eval      *In08 = *On
103680051107     c                   Eval      *In28 = *On
103690051108     c                   Eval      v1cmsg = msg(01)
103700051115      * se richiamato passo il messaggio
103710051115     c                   If        *In06
103720051115     c                   Eval      ta60msg = v1cmsg
103730051115     c                   Eval      ta60err = '1'
103740051115     c                   EndIf
103750051115     c                   Leavesr
103760051117     c                   EndIf
103770051107
103780170914      * Reperimento delle Filiali da proporre a video per la Ricerca
103790170914     c                   clear                   TRUL31ds
103800170914     c*//*no:*           if        *in05  = *on   and
103810170914     c*// no:                      wAbi  <> 'DI'  and
103820170914     c*// no:                      wAbi  <> 'AZ'
103830170914if  1c                   if        ( wAbi = 'TP'  or
103840170914     c                               wAbi = 'PO' )
103850170914     c                   eval      i31abi = 'RA'
103860170915x   1c                   else
103870170915     c                   eval      i31abi = wabi
103880170915e   1c                   endif
103890170914     c                   eval      i31cdi = DUTdis
103900170914     c                   eval      i31car = DUTare
103910170914     c                   eval      i31cpo = DUTpou
103920170914     c                   call      'TRUL31R'
103930170914     c                   parm                    kpjba
103940170914     c                   parm                    TRUL31ds
103950170914if  2c                   if        o31pog > *zeros
103960170914     c                   movea     o31pog        skPog_RA
103970170914x   2c                   else
103980170914     c                   eval      *in08 = *On
103990170914     c                   eval      *in28 = *On
104000170914     c                   eval      V1Cmsg = Msg(01)
104010170914      * se richiamato passo il messaggio
104020170914if  3c                   if        *in06
104030170914     c                   eval      TA60msg = V1Cmsg
104040170914     c                   eval      TA60err = '1'
104050170914e   3c                   endif
104060170914     c                   leavesr
104070170914e   2c                   endif
104080170915e   1c*//*               endif
104090170914
104100051117      * Reperimento dei P.O. gestibili dall'utente per i codici clienti
104110051107     c                   Clear                   Trul31ds
104120051117     c                   Eval      i31abi = wabi
104130051117     c                   Eval      i31cdi = dutdis
104140051117     c                   Eval      i31car = dutare
104150051117     c                   Eval      i31cpo = dutpou
104160051107     c                   Call      'TRUL31R'
104170051117     c                   Parm                    kpjba
104180051107     c                   Parm                    Trul31ds
104190051117     c                   If        o31pog > *zeros
104200051117     c                   Movea     o31pog        skpog
104210051117     c                   Else
104220051107     c                   Eval      *In08 = *On
104230051107     c                   Eval      *In28 = *On
104240051107     c                   Eval      v1cmsg = msg(01)
104250051115      * se richiamato passo il messaggio
104260051115     c                   If        *In06
104270051115     c                   Eval      ta60msg = v1cmsg
104280051115     c                   Eval      ta60err = '1'
104290051115     c                   EndIf
104300051115     c                   Leavesr
104310051117     c                   EndIf
104320160905      /free
104330160905       //?Se utente abilitato alla gestione clienti logistica aggiungo
104340160905       //?alla sk SKPOG le filiali con NTW LOG
104350160905         IF  §UTEKscLog = 'S';
104360160905           exsr CaricaFilLog;
104370160905         ENDIF;
104380160905      /end-free
104390051115
104400051117      * Reperimento dei P.O. gestibili dall'utente per i codici potenziali
104410051117     c                   If        §utepot <> *Blanks
104420051117     c                   Eval      wabi = §utepot
104430051117     c                   EndIf
104440051117     c                   Clear                   Trul31ds
104450051117     c                   Eval      i31abi = wabi
104460051117     c                   Eval      i31cdi = dutdis
104470051117     c                   Eval      i31car = dutare
104480051117     c                   Eval      i31cpo = dutpou
104490051117     c                   Call      'TRUL31R'
104500051117     c                   Parm                    kpjba
104510051117     c                   Parm                    Trul31ds
104520051117     c                   If        O31pog > *zeros
104530051117     c                   Movea     O31pog        skpogcpo
104540051117     c                   EndIf
104550051117
104560051115      * Controllo se sono in sede
104570051115     c                   Eval      *In09 = (simfel = *zeros)
104580051128
104590051128      * Controllo se utente EDP
104600051129     c                   Eval      *In20 = (%subst(knmus:1:3) = 'EDP')
104610051116
104620060110      * Se sono in filiale
104630060110if  1c                   If        Not *In09
104640060110      * Verifico se il P.O. utente può manutenzionare l'anagrafico clienti
104650051116      * deve esistere la tabella MTC per il p.o. utente
104660051116     c                   Clear                   Tibs02ds
104670051116     c                   Clear                   dmtc
104680051116     c                   Eval      t02mod = 'C'
104690051116     c                   Eval      t02sif = knsif
104700051116     c                   Eval      t02cod = 'MTC'
104710051116     c                   Movel(p)  dutpou        t02ke1
104720051116     c                   Call      'TIBS02R'
104730051116     c                   Parm                    kpjba
104740051116     c                   Parm                    Tibs02ds
104750060110if  2c                   If        t02err <> *Blanks
104760051116     c                   Eval      *In28 = *On
104770051116     c                   Eval      v1cmsg = msg(01)
104780051116      * se richiamato passo il messaggio
104790051116     c                   If        *In06
104800051116     c                   Eval      ta60msg = v1cmsg
104810051116     c                   Eval      ta60err = '1'
104820051116     c                   EndIf
104830051116     c                   Leavesr
104840060110   x2c                   Else
104850051125     c                   Eval      dmtc = t02uni
104860060110    2c                   EndIf
104870060110
104880060110      * Se sono in sede deve esserci la tabella MTC key sede
104890060110   x1c                   Else
104900060110     c                   Clear                   Tibs02ds
104910060110     c                   Clear                   dmtc
104920060110     c                   Eval      t02mod = 'C'
104930060110     c                   Eval      t02sif = knsif
104940060110     c                   Eval      t02cod = 'MTC'
104950060110     c                   Movel(p)  'SEDE'        t02ke1
104960060110     c                   Call      'TIBS02R'
104970060110     c                   Parm                    kpjba
104980060110     c                   Parm                    Tibs02ds
104990060110if  2c                   If        t02err <> *Blanks
105000060110     c                   Eval      *In28 = *On
105010060110     c                   Eval      v1cmsg = msg(01)
105020060110      * se richiamato passo il messaggio
105030091106     c                   If        *In06
105040060110     c                   Eval      ta60msg = v1cmsg
105050060110     c                   Eval      ta60err = '1'
105060060110     c                   EndIf
105070060110     c                   Leavesr
105080060110   x2c                   Else
105090060110     c                   Eval      dmtc = t02uni
105100060110    2c                   EndIf
105110060110    1c                   EndIf
105120051115
105130051117      * Apro i file delle anagrafiche provvisorie se richiamato da gest.visite
105140051115     c                   If        *In06
105150051115     c                   Open      Tfaco00f
105160051115     c                   Open      Tfind00f
105170051115     c                   Open      Tfclp00f
105180051117     c                   Open      Tfcls01l
105190080306     c                   Open      Tncpo05l
105200080306     c                   Open      Tncpo06l
105210051115     c                   EndIf
105220051117
105230051117      * Apro i file delle anagrafiche effettive se non richiamato da gest.visite
105240051117     c                   If        Not *In06
105250051117     c                   Open      Cnind00f
105260060105     c                   Open      Cnind02l
105270051117     c                   Open      Cnclp00f
105280051220     c                   Open      Fnacr01l
105290051117     c                   Open      Fncls01l
105300051117     c                   Open      Fnspe01l
105310051219     c                   Open      Fnsp201l
105320060526     c                   EndIf
105330091112
105340091112      * Apro i file dei luoghi se trattativa su cliente
105350091112     c                   If        *In70
105360091112     c                   Open      Fnspe01l
105370091112     c                   Open      Fnsp201l
105380091112     c                   EndIf
105390051115
105400051222      * Passo i dati del capoconto se sono richiamato da visite/offerte
105410091106      * visto che salto la prima videata di richiesta codice
105420091124     c                   If        $pgmrich
105430051115     c                   Eval      v1ckcc = dutkci
105440051115     c                   EndIf
105450051107
105460051107      * Carico £4 capoconti gestiti solo in filiale e non in sede
105470051107     c                   Clear                   £4
105480051107     c                   Eval      kcod = '£4'
105490051108     c                   Eval      kkey = '       1'
105500051108     c     kTabel        Chain     Tabel00f
105510051107     c                   If        %Found(Tabel00f) and
105520051125     c                             tblflg = *Blanks
105530051125     c                   Movea     tbluni        £4
105540051107     c                   EndIf
105550051117
105560051117      * Recupero la divisa da tabella 'GED'
105570051117     c                   Clear                   Tibs02ds
105580051117     c                   Clear                   dged
105590051117     c                   Eval      t02mod = 'C'
105600051117     c                   Eval      t02sif = knsif
105610051117     c                   Eval      t02cod = 'GED'
105620051117     c                   Eval      t02ke1 = '1'
105630051117     c                   Call      'TIBS02R'
105640051117     c                   Parm                    kpjba
105650051117     c                   Parm                    Tibs02ds
105660051117     c                   If        t02err = *Blanks
105670051125     c                   Eval      dged = t02uni
105680051117     c                   EndIf
105690051117
105700051117      * Recupero il diritto di fatturazione di cartello
105710051117     c                   Clear                   Tibs02ds
105720051117     c                   Clear                   dgei
105730051117     c                   Eval      t02mod = 'C'
105740051117     c                   Eval      t02sif = knsif
105750051117     c                   Eval      t02cod = 'GEI'
105760051117     c                   Eval      t02ke1 = §gedcn
105770051117     c                   Call      'TIBS02R'
105780051117     c                   Parm                    kpjba
105790051117     c                   Parm                    Tibs02ds
105800051117     c                   If        t02err = *Blanks
105810051125     c                   Eval      dgei = t02uni
105820051117     c                   EndIf
105830051221
105840051221      * Recupero i dft tipo comunicazioni giacenze
105850051221     c                   Clear                   ds2g
105860051221     c                   Eval      kcod = '2G'
105870051221     c                   Eval      kkey = '1'
105880051221     c     kTabel        Chain     Tabel00f
105890051221     c                   If        %Found(Tabel00f) and
105900051221     c                             tblflg = *Blanks
105910051221     c                   Eval      ds2g = tbluni
105920051221     c                   EndIf
105930051219
105940051219      * Aggancio il capoconto su Cnaco per recuperare acoopz e acotic
105950051219      * uguali per tutti i clienti
105960051219     c     kCnaco1       Chain(n)  Cnaco00f
105970051219     c                   If        %Found(Cnaco00f)
105980051219     c                   Eval      wopz = acoopz
105990051219     c                   Eval      wtic = acotic
106000051219     c                   Else
106010051219     c                   Eval      wopz = '1100100001'
106020051219     c                   Eval      wtic = 'CG'
106030051219     c                   EndIf
106040051219
106050051129      * Imposto la data odierna
106060051129     c                   Z-add     *date         dataoggi
106070051129      * Imposto la data odierna -1 anno
106080051129     c                   Move      dataoggi      dataiso
106090051129     c                   Subdur    1:*y          dataiso
106100051129     c                   Move      dataiso       dataoggi1
106110100805
106120150424       //?cerco il tipo pag. di default da impostare in immissione
106130100805         exec sql
106140100805         declare PAG cursor for
106150100805         select TBEke1, TBEke2 from TNTBE00F
106160100805         where  TBEcod = 'PAG' and substr(TBEuni, 37, 2) = 'SI';
106170100805
106180100805         exec sql
106190100805           open PAG;
106200100805           IF sqlcode < 0;
106210100805             $End = *on;
106220100805           ENDIF;
106230100805
106240100805         DOW not $End;
106250100805           exec sql
106260100805             fetch next from PAG into :TBEke1, :TBEke2;
106270100805             IF sqlcod = 100 or sqlcod < 0;
106280100805               $End = *on;
106290100805               leave;
106300100805             ENDIF;
106310150424             IF  TBEke1 = 'DN';
106320100805               dfttipdn = TBEke2;
106330100805             ENDIF;
106340150424             IF  TBEke1 = 'NA';
106350100805               dfttipna = TBEke2;
106360100805             ENDIF;
106370100805         ENDDO;
106380100805
106390100805         exec sql close PAG;
106400170421
106410170421       //?carico in sk i clienti IMPORT DPD
106420170421         $End = *off;
106430170421         clear ww;
106440170421         exec sql
106450170421         declare TabLPD cursor for
106460170421         select TBEuni from TNTBE00F
106470170421         where TBEcod = 'LPD';
106480170421         exec sql open TabLPD;
106490170421         IF  sqlcode < 0;
106500170421           $End = *on;
106510170421         ENDIF;
106520170421         DOW  not $End;
106530170421           exec sql fetch next from TABLPD into :TBEuni;
106540170421           IF  sqlcod = 100 or sqlcod < 0;
106550170421             $End = *on;
106560170421             leave;
106570170421           ENDIF;
106580170421           dLPD = TBEUni;
106590170421           IF  §LPDksc = 0;
106600170421             iter;
106610170421           ENDIF;
106620170421           ww += 1;
106630170421           CliImport(ww) = §LPDksc;
106640170421         ENDDO;
106650170421         exec sql close TabLPD;
106660100805
106670100805      /end-free
106680051107
106690051107      * KLIST
106700051129     c     kCnabi        Klist
106710051129     c                   Kfld                    kabi
106720051129     c                   Kfld                    kcab
106730051129
106740051108     c     kCnaco        Klist
106750051108     c                   Kfld                    codut
106760051107     c                   Kfld                    v1ckcc
106770051222     c                   Kfld                    kksc
106780051116
106790051116     c     kCnaco1       Klist
106800051116     c                   Kfld                    codut
106810051116     c                   Kfld                    v1ckcc
106820051116     c                   Kfld                    wksc
106830060105
106840060105     c     kCnind        Klist
106850060105     c                   Kfld                    codut
106860060105     c                   Kfld                    v1ckcc
106870060109     c                   Kfld                    kindiva
106880051108
106890051108     c     kFnspe        Klist
106900051216     c                   Kfld                    kspefls
106910051216     c                   Kfld                    kspecli
106920051216     c                   Kfld                    kspecod
106930051219
106940051219     c     kFnsp2        Klist
106950051219     c                   Kfld                    kspecli
106960051219     c                   Kfld                    kspecod
106970051219     c                   Kfld                    ksp2tpe
106980051129
106990051129     c     kTabel        Klist
107000051129     c                   Kfld                    codut
107010051129     c                   Kfld                    kcod
107020051129     c                   Kfld                    kkey
107030051108
107040051129     c     kTfntc        klist
107050051216     c                   kfld                    kntcapl
107060051216     c                   kfld                    kntcnk1
107070051216     c                   kfld                    kntcnk2
107080051216     c                   kfld                    kntctnt
107090051107
107100051107     c                   EndSr
107110160905      /free
107120160905       //--------------------------------------------------------------
107130160905       //?Carico le filiali Logistica nella SKPOG.
107140160905       //--------------------------------------------------------------
107150160905       BEGSR CaricaFilLog;
107160160905
107170160905         $End = *off;
107180160905
107190160905         exec sql
107200160905         declare WLOG cursor for
107210160905         select ORGfil from AZORGLOG;
107220160905
107230160905         exec sql
107240160905         open WLOG;
107250160905         IF  sqlcode < 0;
107260160905           $End = *on;
107270160905         ENDIF;
107280160905
107290160905         DOW not $End;
107300160905           exec sql
107310160905           fetch next from WLOG into :ORGfil;
107320160905           IF  sqlcod = 100 or sqlcod < 0;
107330160905             $End = *on;
107340160905             leave;
107350160905           ENDIF;
107360160905
107370160905           IF  %lookup(%editc(ORGfil:'X'):SKpog) = 0;
107380160905             ww = %lookup(*zeros:SKpog);
107390160905             IF  ww > 0 and xx < 250;
107400160905               SKpog(ww) = %editc(ORGfil:'X');
107410160905             ENDIF;
107420160905           ENDIF;
107430170914
107440170914           If  %lookup( %editc(ORGfil:'X') : SKpog_RA ) = *zero;
107450170914             ww = %lookup( *zeros : SKpog_RA );
107460170914             if  ww > *zero  and  xx < %elem(skPog_RA);
107470170914               SKpog_RA(ww) = %editc(ORGfil:'X');
107480170914             endif;
107490170914           EndIf;
107500160905
107510160905         ENDDO;
107520160905
107530160905         exec sql close WLOG;
107540160905
107550160905       ENDSR;
107560160905      /end-free
107570051107      *------------------------------------------------------------------------*
107580051212** CAR  Lungh. 10
107590051212!:§@
107600051107** MOD  Lungh. 15                                                            *
107610051107  IMMISSIONE                                                                  01
107620051107 MANUTENZIONE                                                                 02
107630051107VISUALIZZAZIONE                                                               03
107640051107   ANNULLATO                                                                  04
107650051107** MSG  Lungh. 78                                                            *
107660051107Utente non autorizzato alla Gestione anagrafico clienti                       01
107670051107Immettere almeno un dato                                                      02
107680051107Immettere il capoconto                                                        03
107690051107Capoconto non gestibile                                                       04
107700051115Cliente inesistente                                                           05
107710051116Password errata                                                               06
107720051116Immettere la password                                                         07
107730051117Password scaduta                                                              08
107740051117Manca tabella di controllo scadenza password. TEL CED SEDE !!!!!              09
107750051117Cliente non gestibile dall'utente                                             10
107760051117Immettere la ragione sociale                                                  11
107770051118Immettere il codice potenziale                                                12
107780051118Codice potenziale errato                                                      13
107790051118Cliente potenziale non gestibile dall'utente                                  14
107800051118Stato che non prevede il cappario: utilizzabile solo per clienti bloccati     15
107810051118Impossibile utilizzare cap o localita' obsoleti                               16
107820051118Stato errato                                                                  17
107830051118Immettere la partita IVA                                                      18
107840051118Codice fiscale formalmente errato                                             19
107850051122Codice commerciale errato                                                     20
107860090617Fil. del codice commerciale incongruente con la fil. del cliente              21
107870051122Il cliente è da bloccare perchè inattivo                                      22
107880051122Stato del credito errato                                                      23
107890090617Immettere stato del credito gest. da fil. e senza passaggio al contenzioso    24
107900051122Causale blocco cliente errata                                                 25
107910051122Immettere la causale blocco perchè il cliente è bloccato                      26
107920051122Per immettere la causale blocco occorre bloccare il cliente                   27
107930051128Immettere il codice importanza cliente                                        28
107940051128Codice importanza cliente errata                                              29
107950080313Immettere la categoria Merceologica                                           30
107960080313Categoria Merceologica errata                                                 31
107970051212La ragione sociale contiene uno dei seguenti caratteri non validi -- ! : § @  32
107980051128Codice Sottoconto fattura non impostato                                       33
107990051128Codice Sottoconto fattura non utilizzabile                                    34
108000051128Codice Sottoconto fattura errato                                              35
108010051128Codice Sottoconto fattura annullato                                           36
108020051128Tipo fattura errato                                                           37
108030051128Tipo fattura non utilizzabile in                                              38
108040051128Immettere la frequenza fattura                                                39
108050051128Frequenza fattura errato                                                      40
108060051128Per tipo fattura "1" è ammessa solo la frequenza mensile                      41
108070120613Per frequenza settimanale chiedere autorizzazione alla DIR. AMMINISTRATIVA!!  42
108080051128Immettere il tipo data fattura                                                43
108090051128Tipo data fattura errato                                                      44
108100051128Sottoconto fattura diverso da codice cliente è possibile unificare            45
108110051128Verificare il flag Fattura Unificata anche nel sottoconto fattura             46
108120051128Verificare il flag Fattura Unificata anche nei codici collegati               47
108130051128Spese invio fattura errato. Troppi decimali, massimo 2                        48
108140060208Per interrogare le info.commerciali inserire il cod. potenziale               49
108150060203Il luogo non è stato immesso. Impossibile Visualizzare                        50
108160061030Immettere il codice fiscale                                                   51
108170060203La nota non è stata immessa. Impossibile Visualizzare                         52
108180051129Codice pagamento errato                                                       53
108190051129Codice ABI/CAB obbligatorio se pagamento con RB                               54
108200051129Codice ABI/CAB obbligatorio se immessa descrizione banca o nr. C/C            55
108210051130Codice ABI/CAB non valido                                                     56
108220100729Tipo pagamento errato                                                         57
108230100729Tipo pagamento non utilizzabile per clienti vari                              58
108240051130Coordinate bancarie errate. Correggere i dati immessi o contattare il cliente 59
108250051130Per l'estero l'ABI deve essere 99999 e il CAB deve essere 99999               60
108260051206Immettere i codici ABI/CAB                                                    61
108270051212Tipo Comunicazione al Mittente errato                                         62
108280051212Tipo invio comunicazione non utilizzabile                                     63
108290051212Apertura automatica giacenza errato                                           64
108300051212Apertura automatica giacenza non possibile per clienti vari                   65
108310051212Disposizione automatica giacenza non possibile per clienti vari               66
108320051213Immettere un numero stop ritiro minore di 800                                 67
108330051213Il codice richiesto è già in uso da altro utente. Riprovare più tardi         68
108340051216Impossibile confermare. Messaggi in sospeso su altre videate                  69
108350060216Tipo Comunicazione Fine Giacenza errato                                       70
108360051220Impossibile modificare cod.intestazione fattura inserito dalla SEDE           71
108370051220ATTENZIONE!!! Codice già presente in anagrafica clienti ritiro                72
108380170117Impossibile intestare fattura ad un codice in sofferenza.                     73
108390060801Chiusura automatica giacenza non possibile per clienti vari                   74
108400061113Inserire Codice Fiscale solo se diverso dalla Partita IVA. Enter per forzare. 75
108410071128Luogo non utilizzabile                                                        76
108420080306Cod.fiscale già presente su altri potenziali. Enter per forzare               77
108430080306Partita Iva già presente su altri potenziali. Enter per forzare               78
108440080422Non è possibile inserire coordinate bancarie estere se tipo pagamento italia. 79
108450080515Codice pagamento utilizzabile SOLO dalla SEDE                                 80
108460090421Non consentita fattura settimanale se cod.int.fattura con autofattura         81
108470090421No autofattura un codice collegato al cod.int.fattura ha frequenza settimanale82
108480090616Il cliente deve essere incluso nella procedura di sollecito automatico        83
108490090616Il cliente deve essere escluso dalla procedura di sollecito automatico        84
108500100729Codice pagamento NON utilizzabile                                             85
108510100730Per pagamento DANNI con Bonifico immettere la relativa e-mail                 86
108520150427Mancano i codici IBAN per i pagamenti tramite Bonifico. Enter x forzare.      87
108530150805Manca indirizzo mail per inoltro Progetto di Liquidazione. Enter x forzare.   88
108540160517Pagamento con RB non ammesso per cliente estero                               89
108550160517Pagamento con RB non ammesso su conto BancoPosta                              90
108560160914Filiale errata o non in gestione.                                             91
108570060731** TF04
108580060731F4=Abilitazioni  #
108590100524** TF22
108600100524F22=Richiesta Contatto  #
108610100507** TF05
108620100507F5=Attività  #
108630060203** TF12
108640060203F12=Ritorno  #
108650051206** TF14
108660051206F14=InfoCommerciali  #
108670051122** TF15
108680051129F15=Codici Collegati  #
108690060731** TF19
108700060526F19=Variazioni  #
108710100407** TF20
108720100407F20=Potenziali  #
108730101213** TF17
108740101213F17=Dati Fatturazione  #
108750110223** TF21
108760110223F21=Blocco Cliente  #
