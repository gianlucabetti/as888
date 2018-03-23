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
002380051128     d wfscf           s              1    inz
002390051122     d wfz3            s                   like(E13Fz3)
002400051116     d wgiorni         s              3  0
002410100729     d wiban           s                   like(v5ciban)
002420150424     d WinInfo         s               n   inz(*off)
002430051118     d wksc            s                   like(AcoKsc) inz
002440051118     d wksc04          s              4  0
002450051219     d wmail005        s                   like(Sp2Est)
002460051220     d wokabl          s              1    inz(*Off)
002470051219     d wokcap          s              1    inz(*Off)
002480051220     d wokimm          s              1    inz(*Off)
002490051219     d wokpot          s              1    inz(*Off)
002500051219     d wopz            s                   like(AcoOpz)
002510100728     d wpag            s              2a
002520100805     d wpgm            s             10a
002530051122     d wpoage          s              3  0
002540060711     d wpoageuni       s              3
002550051122     d wpocli          s              3
002560051117     d wpos            s              2  0
002570051129     d wpos1           s              2  0
002580051129     d wpos2           s              2  0
002590051118     d wprv            s                   like(v2cprv)
002600051124     d wrag            s             40
002610051207     d wrich           s                   like(ta12ric)
002620140722     d wRGR            s                   like(TA12rgr)   inz
002630051118     d wsta            s                   like(v2csta)
002640080222     d w_scfsta        s                   like(indsta)
002650051117     d wtel            s                   like(v2ctel)
002660051219     d wtic            s                   like(AcoTic)
002670051118     d wtlf            s                   like(v2ctlf)
002680051207     d wtnt            s                   like(NtcTnt)
002690051118     d wvia            s                   like(v2cvia)
002700051213     d wvideo          s              2
002710051108     d wtibs69         s              1    inz('0')
002720060531     d wtibs73         s              1    inz('0')
002730100127     d wtnta40         s              1n   inz(*off)
002740081020     d wtrmk50         s              1    inz('0')
002750101014     d wuffsede        s             35
002760051216     d w001a           s              1
002770051108     d w002a           s              2
002780051115     d w003a           s              3
002790051108     d w0030           s              3  0
002800051107     d w004a           s              4
002810051117     d w0040           s              4  0
002820070704     d w0040ksc        s              4  0
002830051124     d w005a           s              5
002840051220     d w0060           s              6  0
002850051124     d w0070           s              7  0
002860051220     d w0100           s             10  0
002870051108     d zz              s              2  0
002880110407     d $Blocco         s              1n
002890051219     d $d01            s              1    inz('0')
002900051129     d §fil            s             90
002910051129     d $h              s              3  0
002920091104     d $interroga      s              1n
002930051129     d $j              s              3  0
002940051129     d $k              s              3  0
002950051129     d $loop           s              1  0
002960091124     d $modifica       s              1n
002970091106     d $pgmrich        s              1n
002980051129     d $rig            s              1  0
002990051108     d §tip            s              1
003000051122     d $x              s              3  0
003010051122     d $y              s              3  0
003020051122     d $z              s              3  0
003030071128     d $ufi001         s                   like(§4lufi)
003040071128     d $ufi002         s                   like(§4lufi)
003050071128     d $ufi005         s                   like(§4lufi)
003060071128     d $ufi006         s                   like(§4lufi)
003070071128     d $ufi008         s                   like(§4lufi)
003080071128     d $ufi200         s                   like(§4lufi)
003090071128     d $ufi300         s                   like(§4lufi)
003100071128     d $ufi555         s                   like(§4lufi)
003110071128     d $ufi666         s                   like(§4lufi)
003120080422     d $win            s               n
003130100729     d $windn          s               n
003140100729     d $winna          s               n
003150080306     d savcdf          s                   like(v2ccdf) inz
003160080306     d cod_forzcdf     s                   like(cpocdf) inz
003170080306     d cod_forziva     s                   like(cpopiv) inz
003180080306     d savrag          s                   like(cporag)
003190080306     d saviva          s                   like(v2civa) inz
003200080306     d codice          s                   like(v2civa) inz
003210100805     d $End            s               n
003220160905     d ClienteLog      s               n   inz
003230051107
003240051107      *   S C H I E R E
003250051212     d car             s              1    dim(10) ctdata perrcd(10)
003260170421     d CliImport       s              7  0 dim(999) inz
003270051122     d mod             s             15    dim(04) ctdata perrcd(1)
003280160914     d msg             s             79    dim(91) ctdata perrcd(1)
003290051124     d rigatf1         s              1    dim(78)
003300051122     d rigatf2         s              1    dim(62)
003310051122     d rigatf3         s              1    dim(62)
003320051122     d skpo            s              3    dim(30)
003330051122     d skpog           s              3    dim(250) inz(*Zeros)
003340051122     d skpogcpo        s              3    dim(250) inz(*Zeros)
003350060731     d tf04            s              1    dim(25) ctdata perrcd(25)
003360100524     d tf22            s              1    dim(25) ctdata perrcd(25)
003370100507     d tf05            s              1    dim(25) ctdata perrcd(25)
003380060203     d tf12            s              1    dim(25) ctdata perrcd(25)
003390051206     d tf14            s              1    dim(25) ctdata perrcd(25)
003400051124     d tf15            s              1    dim(25) ctdata perrcd(25)
003410060526     d tf19            s              1    dim(25) ctdata perrcd(25)
003420100406     d tf20            s              1    dim(25) ctdata perrcd(25)
003430101213     d tf17            s              1    dim(25) ctdata perrcd(25)
003440110223     d tf21            s              1    dim(25) ctdata perrcd(25)
003450051122     d £4              s              4    dim(20)
003460051124     d $tf             s              1    dim(25)
003470051107
003480051107      *   D S   I N T E R N E / E S T E R N E
003490051109
003500051109     d                 ds
003510051109     d aa1                     1      2  0
003520051109     d mm1                     3      4  0
003530051109     d gg1                     5      6  0
003540051109     d dataamg                 1      6  0
003550051109
003560051109     d                 ds
003570051109     d gg2                     1      2  0
003580051109     d mm2                     3      4  0
003590051109     d aa2                     5      6  0
003600051109     d datagma                 1      6  0
003610051129
003620051129     d parmbanca       ds
003630051129     d  pabi                   1      5  0
003640051129     d  pcab                   6     10  0
003650051129     d  pist                  11     50
003660051129     d  ppro                  51     52
003670051129     d  pflg                  53     53
003680051124
003690051124     d parmf11         ds
003700051124     d  paropz                 1      1
003710051124     d  parcli                 2      8
003720060216     d  partnta60              9      9
003730060216     d  parimta60             10     10
003740051124
003750051124     d parmf15         ds
003760051124     d  parcli1                       7  0
003770080116
003780080116     d parmiban        ds
003790080116     d  parmstato              1      2
003800080116     d  parmcheck              3      4
003810080206     d  parmcoori              5     34
003820051107
003830051107     d                 ds
003840051107     d v1cf01                  1      3
003850051107     d v1cf02                  4      6
003860051107     d v1cf03                  7      9
003870051107     d v1cf04                 10     12
003880051107     d v1cf05                 13     15
003890051107     d v1cf06                 16     18
003900051107     d v1cf07                 19     21
003910051107     d v1cf08                 22     24
003920051107     d v1cf09                 25     27
003930051107     d v1cf10                 28     30
003940051107     d v1cf11                 31     33
003950051107     d v1cf12                 34     36
003960051107     d v1cf13                 37     39
003970051107     d v1cf14                 40     42
003980051107     d v1cf15                 43     45
003990051107     d v1cf16                 46     48
004000051107     d v1cf17                 49     51
004010051107     d v1cf18                 52     54
004020051107     d v1cf19                 55     57
004030051107     d v1cf20                 58     60
004040051107     d v1cf21                 61     63
004050051107     d v1cf22                 64     66
004060051107     d v1cf23                 67     69
004070051108     d v1cf24                 70     72
004080051108     d skfil                   1     72    dim(24)
004090051108
004100051108     d                 ds
004110051108     d v1ivaeu                 1      2
004120051108     d v1ivait                 3     16
004130051108     d v1civa                  1     16
004140051109
004150051109     d                 ds
004160051109     d v2ivaeu                 1      2
004170051109     d v2ivait                 3     16
004180051109     d v2civa                  1     16
004190080115
004200080115     d v5ciban         ds
004210080115     d  v5ciban1               1      4
004220080115     d  v5ciban2               5      8
004230080115     d  v5ciban3               9     12
004240080115     d  v5ciban4              13     16
004250080115     d  v5ciban5              17     20
004260080115     d  v5ciban6              21     24
004270080115     d  v5ciban7              25     28
004280080208     d  v5ciban8              29     32
004290100728
004300100728     d v5ibandn        ds
004310100728     d  v5ibandn1              1      4
004320100728     d  v5ibandn2              5      8
004330100728     d  v5ibandn3              9     12
004340100728     d  v5ibandn4             13     16
004350100728     d  v5ibandn5             17     20
004360100728     d  v5ibandn6             21     24
004370100728     d  v5ibandn7             25     28
004380100728     d  v5ibandn8             29     32
004390100728
004400100728     d v5ibanna        ds
004410100728     d  v5ibanna1              1      4
004420100728     d  v5ibanna2              5      8
004430100728     d  v5ibanna3              9     12
004440100728     d  v5ibanna4             13     16
004450100728     d  v5ibanna5             17     20
004460100728     d  v5ibanna6             21     24
004470100728     d  v5ibanna7             25     28
004480100728     d  v5ibanna8             29     32
004490051116
004500051116     d wlbdat          ds
004510051116     d  g02dat                 1      8  0
004520051116     d  g02inv                 9     16  0
004530051116     d  g02err                17     17
004540051116     d  g02tgi                18     22  0
004550051107
004560051107     d                sds
004570051107     d  Vtcpgm                 1     10
004580051107
004590051107     d Azuteds       e ds                  Extname(Azute00f)
004600051107     d dDatiute      e ds
004610100729     d dPag          e ds
004620051117     d dged          e ds
004630051117     d dgei          e ds
004640051116     d dlat          e ds
004650170421     d dLPD          e ds
004660051116     d dmtc          e ds
004670051108     d ds_cnaco      e ds                  inz  extname(Cnaco00f) prefix(w_)
004680051108     d ds_cnind      e ds                  inz  extname(Cnind00f) prefix(w_)
004690051108     d ds_cnclp      e ds                  inz  extname(Cnclp00f) prefix(w_)
004700051108     d ds_fncls      e ds                  inz  extname(Fncls00f) prefix(w_)
004710060531     d cnaco_73      e ds                  inz  extname(Cnaco00f)
004720060531     d cnind_73      e ds                  inz  extname(Cnind00f)
004730060531     d cnclp_73      e ds                  inz  extname(Cnclp00f)
004740060531     d fncls_73      e ds                  inz  extname(Fncls00f)
004750121105
004760121105      // - CPORST file TNCPO
004770121105     d dCPO01        e ds                  inz
004780121105
004790110323     d dsbi          e ds
004800051108     d dsfa          e ds
004810051108     d dsff          e ds
004820051108     d dsic          e ds
004830051108     d ds1l          e ds
004840051108     d ds1t          e ds
004850051125     d ds13          e ds
004860051118     d ds15          e ds
004870051108     d ds2f          e ds
004880051207     d ds2g          e ds
004890051108     d ds2h          e ds
004900051108     d ds2u          e ds
004910051108     d ds4l          e ds
004920051108     d ds4u          e ds
004930051115     d ds4w          e ds
004940090803     d w_ds4w        e ds                  extname(ds4w) prefix(w_)
004950051125     d dtft          e ds
004960051108     d dute01        e ds
004970110217     d dY4O          e ds
004980051118     d Fnlv13ds      e ds
004990051118     d Fnlv14ds      e ds
005000070809     d FIOR37ds      e ds                  inz
005010070809     d  I37cgi       e                     inz(*all'9')
005020051220     d Fior50ds      e ds
005030051108     d kpjba         e ds
005040100406     d OG141         e ds
005050051125     d og143         e ds
005060051220     d og148         e ds
005070051107     d Tibs02ds      e ds
005080051122     d Tibs12ds      e ds
005090051107     d Tibs34ds      e ds
005100051108     d Tibs69ds      e ds
005110060531     d Tibs73ds      e ds
005120051118     d Tisi95ds      e ds
005130051207     d Tnta12ds      e ds
005140100127     d TNTA40DS      e ds
005150100604     d TNTA43DS      e ds
005160051124     d Tnta60ds      e ds
005170060731     d Tnta61ds      e ds
005180051213     d Tnta81ds      e ds
005190100406     d TRMK01DS      e ds
005200110223     d Trmk05ds      e ds
005210100406     d TRMK17DS      e ds
005220100406     d TRMK20ds      e ds
005230100507     d TRMK21DS      e ds
005240130808     d TRMK43ds      e ds                  inz
005250081020     d Trmk50ds      e ds
005260100407     d TRMK28DS      e ds
005270051108     d Trul31ds      e ds
005280051118     d Trul42ds      e ds
005290101213     d Trul57ds      e ds
005300100730     d TrulIbanI0    e ds                  qualified
005310100728     d TrulIbanI1    e ds                  qualified
005320100728     d TrulIbanO0    e ds                  qualified
005330100728     d TrulIbanO1    e ds                  qualified
005340090616     d trulsoli1     e ds                  qualified
005350090616     d trulsolo1     e ds                  qualified
005360060529     d TNTA73DS      e ds
005370061012     d Tisie5ds      e ds
005380051128     d Xdecds        e ds                  inz
005390051128     d   ocdesi      e                     inz(*Off)
005400151014     d xcfiva1ds     e ds
005410061106     d  ivaeu                  3      4
005420061106     d  ivait                  5     18
005430100805     d TNTBE00F      e ds                  extname(TNTBE00F)
005440160803
005450160803       // -?Storicizzazione variazioni
005460160803     d TRMK30DS      e ds
005470160803     d TNCPO_30      e ds                  extname(TNCPO00F) inz
005480160803     d TNCPO1_30     e ds                  extname(TNCPO10F) inz
005490160803     d TICPI_30      e ds                  extname(TICPI00F) inz
005500051107
005510051107      *   C O S T A N T I
005520060203      * titolo videata (lunghezza massima 35)
005530060203     d VtcTit          c                   const('** GESTIONE ANAGRAFICHE CLIEN-
005540060203     d                                     TI ** ')
005550091027     d VtcTitv         c                   const('****  Anagrafica Provvisoria -
005560091027     d                                      **** ')
005570051122     d VtcTitc         c                   const('**      CONVALIDA OFFERTE    -
005580060203     d                                        ** ')
005590051122     d cf2413          c                   const('F24=AlTasti(1/3)')
005600051122     d cf2423          c                   const('F24=AlTasti(2/3)')
005610051122     d cf2433          c                   const('F24=AlTasti(3/3)')
005620051122     d cf2412          c                   const('F24=AlTasti(1/2)')
005630051122     d cf2422          c                   const('F24=AlTasti(2/2)')
005640100127
005650100127     d Digits          c                   const('0123456789')
005660100406
005670100406      //---------------------------------------------------------------
005680100406      //?Definizione procedure esterne.
005690100406      //---------------------------------------------------------------
005700100604      // - Controllo se trattativa aperta
005710100604     d Tnta43R         pr                  extpgm('TNTA43R')
005720100604     d  tnta43ds                           likeds(tnta43ds)
005730110223
005740110223      // - Calcolo categoria potenziale
005750110223     d trmk05r         pr                  extpgm('TRMK05R')
005760110223     d  kpjba                              likeds(KPJBA)
005770110223     d  trmk05ds                           likeds(TRMK05ds)
005780100604
005790100406      // - Inserimento attività
005800100406     d trmk17r         pr                  extpgm('TRMK17R')
005810100406     d  kpjba                              likeds(KPJBA)
005820100406     d  trmk17ds                           likeds(TRMK17ds)
005830100507
005840100507      // - Interrogazione attività
005850100507     d trmk21r         pr                  extpgm('TRMK21R')
005860100507     d  kpjba                              likeds(KPJBA)
005870100407
005880100407      // - Modifica potenziale
005890100407     d trmk28r         pr                  extpgm('TRMK28R')
005900100407     d  trmk28ds                           likeds(TRMK28DS)
005910120510
005920120510      // - Pgm gestione INFO commerciali
005930120510     d trmk50r         pr                  extpgm('TRMK50R')
005940120510     d  kpjba                              likeds(KPJBA)
005950120510     d  trmk50ds                           likeds(TRMK50ds)
005960100728
005970100728      // - Coordinate bancarie
005980100728     d TrulIbanR       pr                  extpgm('TRULIBANR')
005990100729     d  rqsOpCode                    10a   const
006000100729     d  rpyEsito                     10i 0
006010100729     d  rqsFormatName                10a   const
006020100729     d  rqsData                     256a   options(*varsize)
006030100729     d  rqsDataSize                  10i 0 const
006040100729     d  rpyFormatName                10a   const
006050100729     d  rpyData                     256a   options(*varsize)
006060100729     d  rpyDataSize                  10i 0 const
006070100729
006080100729      // - Interrogazione tabella
006090100729     d Trul19R         pr                  extpgm('TRUL19R')
006100100729     d  kcod                          2a
006110100729     d  ordina                        1a
006120100729     d  kkey                          8a
006130100729     d  comando                       1a
006140101213
006150101213      // - Dati Fatturazione
006160101213     d trul57r         pr                  extpgm('TRUL57R')
006170101213     d  kpjba                              likeds(KPJBA)
006180101213     d  trul57ds                           likeds(TRUL57ds)
006190160803
006200160803       // -?Storicizzazione Variazioni Potenziale
006210160803     d TRMK30R         pr                  extpgm('TRMK30R')
006220160803     d  trmk30ds                           likeds(trmk30ds)
006230160803     d  tncpo_30                           likeds(tncpo_30)
006240160803     d  tncpo1_30                          likeds(tncpo1_30)
006250160803     d  ticpi_30                           likeds(ticpi_30)
006260100406
006270100406      //---------------------------------------------------------------
006280100406      //?prototipi
006290100406      //---------------------------------------------------------------
006300100729      /copy gaitrasrc/srcprotopr,tibs02r
006310150427      /copy gaitrasrc/srcprotopr,tibs73r
006320161129      /copy gaitrasrc/srcprotopr,tibs69r
006330100406      /copy gaitrasrc/srcprotopr,trmk01r
006340100406      /copy gaitrasrc/srcprotopr,trmk02r
006350100406      /copy gaitrasrc/srcprotopr,trmk20r
006360140722      /copy gaitrasrc/srcProtoPR,TNTA12R
006370051107
006380051107      *------------------------------------------------------------------------*
006390090417
006400090417     c/exec sql
006410090417     c+ set option dynusrprf = *owner, closqlcsr = *endmod
006420090417     c/end-exec
006430051107
006440051117      * Richiamato da Gestione visite/offerte o da Comferma offerte
006450051117      * salto la videata di richiesta del codice
006460091104      * anche nel caso di richiamo per sola interrogazione (nuovo campo DS)
006470091124      * oppure manutenzione di un codice specifico (cliente-CNACO)
006480091124     c                   If        (*In06 or *In07
006490091124     c                              or $interroga or $modifica)
006500091124     c                              and not *in28
006510051118     c                   Goto      vid02
006520051115     c                   EndIf
006530091124
006540091124     c   28              goto      fine
006550051124
006560051124     * §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ * ?
006570051124     ?*  DATI DI RICERCA    ?
006580051124     ?*  Prima videata      ?
006590051124
006600060110     c                   ExSr      Sr_Pulvid
006610051118
006620051118     c     emi01         Tag
006630051107
006640051107     c                   Write     Ta60t01
006650051107     c                   Exfmt     Ta60d01
006660051107     c                   Eval      *In28 = *Off
006670051107     c                   Eval      *In90 = *Off
006680051107     c                   Clear                   v1cmsg
006690051219     c                   Eval      $d01 = *On
006700051107
006710051124     ?* F3=Fine  ?
006720051107     c                   If        *InKc or
006730051107      * 08=utente non abilitato
006740051107     c                             *In08
006750051107     c                   Goto      Fine
006760051107     c                   EndIf
006770051107
006780051124     ?* Controllo  ?
006790051107     c                   ExSr      Sr_Ctrd01
006800051118     c   28              Goto      emi01
006810051124
006820051130     c     vid02         Tag
006830060221     c                   Eval      wokpot = *Off
006840051130
006850051130      * Carico i dati nelle varie videate
006860051216     c                   ExSr      Sr_Cardati
006870091112     c   70
006880091112     corn06              ExSr      Sr_Carluoghi
006890051216     c                   ExSr      Sr_Carnote
006900051130
006910051130     * §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ * ?
006920051130     ?*  DATI ANAGRAFICI/BLOCCO   ?
006930051130     ?*  Seconda videata      ?
006940051107
006950051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
006960051221     c                   Eval      *In29 = *On
006970051207     c                   ExSr      Sr_Tastifun
006980051207
006990051118     c     emi02         Tag
007000051129
007010051107     c                   Write     Ta60t01
007020051122     c                   Write     Ta60z01
007030051107     c                   Exfmt     Ta60d02
007040051107     c                   Eval      *In28 = *Off
007050051124     c                   Eval      *In90 = *Off
007060051108     c                   Clear                   v2cmsg
007070051213     c                   Eval      wvideo = '02'
007080051107
007090051124     ?* F3=Fine  ?
007100051107     c                   If        *InKc
007110060217     c                   ExSr      Sr_F03
007120060217     c   80              Goto      emi02
007130051107     c                   EndIf
007140051114
007150051124     ?* F12=Ritorno  ?
007160051118     c                   If        *Inkl
007170060110     c                   ExSr      Sr_Pulvid
007180091106      * se richiamato in interrogazione deve uscire dal pgm
007190091124     c                   if        $interroga or $modifica
007200091106     c                   ExSr      Sr_F03
007210091106     c                   else
007220100208      * se non è interrogazione e non è anagrafica provvisoria
007230100208      * con F12 devo sbloccare i file
007240100208     c                   if        not *in05 and not *in06
007250100208     c                   unlock    cnaco00f
007260100208     c                   unlock    cnind00f
007270100208     c                   unlock    cnclp00f
007280100208     c                   unlock    fncls01l
007290100208     c                   endif
007300051118     c                   Goto      emi01
007310091106     c                   endif
007320051118     c                   EndIf
007330090717
007340090717     ?* F2=Rubrica  ?
007350090717     c                   If        *Inkb
007360090717     c                   Eval      wrag = v2crag
007370090717     c  n05              Eval      wrich = 'M'
007380090717     c   05              Eval      wrich = 'V'
007390090717     c                   clear                   wtnt
007400090717     c                   ExSr      Sr_F02
007410090717     c                   Goto      emi02
007420090717     c                   EndIf
007430060731
007440060731     ?* F4=Abilitazioni  ?
007450060731     c                   if        *inkd
007460060731     c                   exsr      sr_f04
007470060731     c                   goto      emi02
007480060731     c                   endif
007490100507
007500100507       //?F5=Attività
007510100507     c                   IF        *inke
007520100507     c                   exsr      sr_F05
007530100507     c                   goto      emi02
007540100507     c                   ENDIF
007550051128
007560051128     ?* F6=Conferma  ?
007570051128     c                   If        *InKf
007580051128     c                   ExSr      Sr_Conferma
007590051213     c   28              Goto      emi02
007600051222     c                   If        Not *In06 and Not *In07
007610091125     c                             and not $modifica
007620060110     c                   ExSr      Sr_Pulvid
007630051222     c                   Goto      emi01
007640051222     c                   Else
007650051222     c                   Goto      fine
007660051222     c                   EndIf
007670051128     c                   EndIf
007680051122
007690051124     ?* F7=Cliente Unificante  ?
007700051122     c                   If        *Inkg
007710051122     c                   ExSr      Sr_F07
007720051122     c                   Goto      emi02
007730051122     c                   EndIf
007740051206
007750100407     ?* F20=Int.Cli.Potenziali  ?
007760100407     c                   If        *Inku
007770051206      * Imposto i primi 5 caratteri della ragione sociale
007780051206     c                   Eval      w005a = v2crag
007790100407     c                   ExSr      Sr_F20
007800051206     c                   Goto      emi02
007810051206     c                   EndIf
007820051122
007830051124     ?* F11=Luoghi  ?
007840051122     c                   If        *Inkk
007850051122     c                   ExSr      Sr_F11
007860051122     c                   Goto      emi02
007870051122     c                   EndIf
007880051206
007890051206     ?* F14=Info Commerciali  ?
007900051207     c                   If        *Inkn
007910060208     c                   If        v2ccpo = *Zeros
007920060208     c                   Eval      *In28 = *On
007930060208     c                   Eval      v2cmsg = msg(49)
007940060208     c                   Else
007950051207     c                   Eval      wrag = v2crag
007960051206     c                   ExSr      Sr_F14
007970060208     c                   EndIf
007980051206     c                   Goto      emi02
007990051206     c                   EndIf
008000051122
008010051124     ?* F15=Codici Collegati  ?
008020051122     c                   If        *Inkp
008030051124     c                   If        clpscf = *Zeros
008040051124     c                   Eval      *In28 = *On
008050051128     c                   Eval      v2cmsg = msg(33)
008060051125     c                   Eval      v2cmsg = %trim(v2cmsg) + ' ' +
008070051125     c                             'non è possibile interrogare'
008080051124     c                   Else
008090051124     c                   Eval      w0070 = clpscf
008100051122     c                   ExSr      Sr_F15
008110051124     c                   EndIf
008120051122     c                   Goto      emi02
008130051122     c                   EndIf
008140051122
008150051124     ?* F18=Note  ?
008160051122     c                   If        *Inks
008170101111     c                   If        v2ccpo = *Zeros
008180101111     c                   Eval      *In28 = *On
008190101111     c                   Eval      h2riga = 14
008200101111     c                   Eval      h2colo = 28
008210101111     c                   Eval      v2cmsg = msg(12)
008220101111     c                   Else
008230051124     c                   Eval      wrag = v2crag
008240051122     c                   ExSr      Sr_F18
008250101111     c                   EndIf
008260051122     c                   Goto      emi02
008270051122     c                   EndIf
008280090717
008290060526     ?* F19=Variazioni  ?
008300060526     c                   If        *Inkt
008310060526     c                   ExSr      Sr_F19
008320060526     c                   Goto      emi02
008330060526     c                   EndIf
008340100406
008350100524       //?F22=Richiesta Contatto
008360100524     c                   IF        *inkw
008370101111     c                   If        v2ccpo = *Zeros
008380101111     c                   Eval      *In28 = *On
008390101111     c                   Eval      h2riga = 14
008400101111     c                   Eval      h2colo = 28
008410101111     c                   Eval      v2cmsg = msg(12)
008420101111     c                   Else
008430100524     c                   exsr      sr_F22
008440101111     c                   EndIf
008450100406     c                   goto      emi02
008460100406     c                   ENDIF
008470101213
008480101213       //?F17=Dati Fatturazione
008490101213     c                   IF        *inkr
008500101213     c                   exsr      sr_F17
008510101213     c                   goto      emi02
008520101213     c                   ENDIF
008530110223
008540110223       //?F21=Blocco clienti
008550110223     c                   IF        *inkv
008560110223     c                   If        v2ccpo = *Zeros
008570110223     c                   Eval      *In28 = *On
008580110223     c                   Eval      h2riga = 14
008590110223     c                   Eval      h2colo = 28
008600110223     c                   Eval      v2cmsg = msg(12)
008610110223     c                   else
008620110223     c                   exsr      sr_F21
008630110310      /free
008640110310       //?Se ho pianificato l'attività devo richiamare la gestione
008650110310       //?delle info commerciali
008660110310         IF  OMK17f12 = *blanks and OMK17err = *blanks;
008670110310           exsr sr_f14;
008680110310         ENDIF;
008690110310      /end-free
008700110223     c                   Endif
008710110223     c                   goto      emi02
008720110223     c                   ENDIF
008730051122
008740051124     ?* F24=Altri tasti  ?
008750051124     c                   If        *Inky and $loop > 1
008760051124     c                   ExSr      Sr_F24
008770051124     c                   Goto      emi02
008780051124     c                   EndIf
008790051107
008800051124     ?* Controllo  ?
008810051117     c  n05              ExSr      Sr_Ctrd02
008820051124     c   28
008830051124     cor 90              Goto      emi02
008840051130
008850051130     * §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ * ?
008860051130     ?*  DATI COMMERCIALI   ?
008870051130     ?*  Terza videata      ?
008880051130
008890051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
008900051221     c                   Eval      *In29 = *Off
008910051207     c                   ExSr      Sr_Tastifun
008920051219      * Ragione sociale x le videate dalla terza in poi
008930051219     c                   Eval      v3crag = v2crag
008940080313      * Se immissione imposto la categoria merceologica da potenziale
008950060110     c                   If        *In01 and savitc <> *Blanks and
008960060110     c                             v3citc = *Blanks
008970060110     c                   Eval      v3citc = savitc
008980060110     c                   EndIf
008990051207
009000051130     c     emi03         Tag
009010051130
009020051130     c                   Write     Ta60t01
009030051130     c                   Write     Ta60z01
009040051130     c                   Exfmt     Ta60d03
009050051130     c                   Eval      *In28 = *Off
009060051130     c                   Eval      *In90 = *Off
009070051130     c                   Clear                   v3cmsg
009080051213     c                   Eval      wvideo = '03'
009090051130
009100051130     ?* F3=Fine  ?
009110051130     c                   If        *InKc
009120060217     c                   ExSr      Sr_F03
009130060217     c   80              Goto      emi03
009140051130     c                   EndIf
009150051130
009160051130     ?* F12=Ritorno  ?
009170051207     c                   If        *Inkl
009180091111      * se richiamato in interrogazione deve uscire dal pgm
009190091124     c                   if        $interroga or $modifica
009200091111     c                   ExSr      Sr_F03
009210091111     c                   else
009220051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
009230051221     c                   Eval      *In29 = *On
009240051207     c                   ExSr      Sr_Tastifun
009250051207     c                   Goto      emi02
009260091111     c                   endif
009270051207     c                   EndIf
009280090717
009290090717     ?* F2=Rubrica  ?
009300090717     c                   If        *Inkb
009310090717     c                   Eval      wrag = v2crag
009320090717     c  n05              Eval      wrich = 'M'
009330090717     c   05              Eval      wrich = 'V'
009340090717     c                   clear                   wtnt
009350090717     c                   ExSr      Sr_F02
009360100607     c                   Goto      emi03
009370090717     c                   EndIf
009380060731
009390060731     ?* F4=Abilitazioni  ?
009400060731     c                   if        *inkd
009410060731     c                   exsr      sr_f04
009420060731     c                   goto      emi03
009430060731     c                   endif
009440100507
009450100507       //?F5=Attività
009460100507     c                   IF        *inke
009470100507     c                   exsr      sr_F05
009480100507     c                   goto      emi03
009490100507     c                   ENDIF
009500051130
009510051130     ?* F6=Conferma  ?
009520051130     c                   If        *InKf
009530051130     c                   ExSr      Sr_Conferma
009540051213     c   28              Goto      emi03
009550051222     c                   If        Not *In06 and Not *In07
009560091125     c                             and not $modifica
009570060110     c                   ExSr      Sr_Pulvid
009580051130     c                   Goto      emi01
009590051222     c                   Else
009600051222     c                   Goto      fine
009610051222     c                   EndIf
009620051130     c                   EndIf
009630051130
009640051130     ?* F7=Cliente Unificante  ?
009650051130     c                   If        *Inkg
009660051130     c                   ExSr      Sr_F07
009670051130     c                   Goto      emi03
009680051130     c                   EndIf
009690051130
009700051130     ?* F11=Luoghi  ?
009710051130     c                   If        *Inkk
009720051130     c                   ExSr      Sr_F11
009730051130     c                   Goto      emi03
009740051130     c                   EndIf
009750051206
009760051206     ?* F14=Info Commerciali  ?
009770051207     c                   If        *Inkn
009780060208     c                   If        v2ccpo = *Zeros
009790060208     c                   Eval      *In28 = *On
009800060208     c                   Eval      v3cmsg = msg(49)
009810060208     c                   Else
009820051207     c                   Eval      wrag = v3crag
009830051206     c                   ExSr      Sr_F14
009840060208     c                   EndIf
009850051206     c                   Goto      emi03
009860051206     c                   EndIf
009870051130
009880051130     ?* F15=Codici Collegati  ?
009890051130     c                   If        *Inkp
009900051130     c                   If        clpscf = *Zeros
009910051130     c                   Eval      *In28 = *On
009920051130     c                   Eval      v3cmsg = msg(33)
009930051130     c                   Eval      v3cmsg = %trim(v3cmsg) + ' ' +
009940051130     c                             'non è possibile interrogare'
009950051130     c                   Else
009960051130     c                   Eval      w0070 = clpscf
009970051130     c                   ExSr      Sr_F15
009980051130     c                   EndIf
009990051130     c                   Goto      emi03
010000051130     c                   EndIf
010010051130
010020051130     ?* F18=Note  ?
010030051130     c                   If        *Inks
010040051130     c                   Eval      wrag = v3crag
010050051130     c                   ExSr      Sr_F18
010060051130     c                   Goto      emi03
010070051130     c                   EndIf
010080060529
010090060529     ?* F19=Variazioni  ?
010100060529     c                   If        *Inkt
010110060529     c                   ExSr      Sr_F19
010120060529     c                   Goto      emi03
010130060529     c                   EndIf
010140100406
010150100524       //?F22=Richiesta Contatto
010160100524     c                   IF        *inkw
010170100524     c                   exsr      sr_F22
010180100406     c                   goto      emi03
010190100406     c                   ENDIF
010200101213
010210101213       //?F17=Dati Fatturazione
010220101213     c                   IF        *inkr
010230101213     c                   exsr      sr_F17
010240101213     c                   goto      emi03
010250101213     c                   ENDIF
010260110223
010270110223       //?F21=Blocco clienti
010280110223     c                   IF        *inkv
010290110223     c                   exsr      sr_F21
010300110310      /free
010310110310       //?Se ho pianificato l'attività devo richiamare la gestione
010320110310       //?delle info commerciali
010330110310         IF  OMK17f12 = *blanks and OMK17err = *blanks;
010340110310           exsr sr_f14;
010350110310         ENDIF;
010360110310      /end-free
010370110223     c                   goto      emi03
010380110223     c                   ENDIF
010390051130
010400051130     ?* F24=Altri tasti  ?
010410051130     c                   If        *Inky and $loop > 1
010420051130     c                   ExSr      Sr_F24
010430051130     c                   Goto      emi03
010440051130     c                   EndIf
010450051130
010460051130     ?* Controllo  ?
010470051130     c  n05              ExSr      Sr_Ctrd03
010480051219     c   28
010490051219     cor 90              Goto      emi03
010500051219      * se interrogazione controllo i luoghi e le note
010510051219     c                   If        *In05
010520140722     c                   eval      wRGR = 'RN'
010530140722     c                   exsr      sr_CtrNote
010540140722     c   28
010550140722     cor 90              goto      Emi03
010560140722     c                   eval      wRGR = 'RT'
010570140722     c                   exsr      sr_CtrNote
010580140722     c   28
010590140722     cor 90              goto      Emi03
010600140722     c                   eval      wRGR = 'RL'
010610140722     c                   exsr      sr_CtrNote
010620140722     c   28
010630140722     cor 90              goto      Emi03
010640051219     c                   EndIf
010650051108
010660051129
010670051124     * §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ * ?
010680100728     ?*  DATI FATTURAZIONE + PAGAMENTO FATTURA  ?
010690051130     ?*  Quarta videata      ?
010700051118
010710051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
010720051207     c                   ExSr      Sr_Tastifun
010730051219      * Se cliente non nuovo imposto la data prima spedizione fattura
010740051219      * uguale a oggi meno 1 anno
010750121119     c                   If        v3cnew = 'S' and *In01
010760051219     c                   Z-add     dataoggi1     dataamg
010770051219     c                   Z-add     aa1           aa2
010780051219     c                   Z-add     mm1           mm2
010790051219     c                   Z-add     gg1           gg2
010800051219     c                   Z-add     datagma       v4cdps
010810051219     c                   EndIf
010820051207
010830051130     c     emi04         Tag
010840100128
010850100128      * se sono da convalida offerta e non c'è il codice fatturazione
010860100129      * impostato imposto "?" nel campo del codice fatturazione
010870100129      * così all'enter si apre la window con i clienti trovati
010880100129     c                   IF        (v4cscf = *zeros or v4cscf = *blanks) and
010890100129     c                              *in07
010900100129     c                   eval      v4cscf = '000000?'
010910100128     c                   ENDIF
010920051129
010930051108     c                   Write     Ta60t01
010940051124     c                   Write     Ta60z01
010950051130     c                   Exfmt     Ta60d04
010960051108     c                   Eval      *In28 = *Off
010970051124     c                   Eval      *In90 = *Off
010980051130     c                   Clear                   v4cmsg
010990051213     c                   Eval      wvideo = '04'
011000051108
011010051124     ?* F3=Fine  ?
011020051108     c                   If        *InKc
011030060217     c                   ExSr      Sr_F03
011040060217     c   80              Goto      emi04
011050051108     c                   EndIf
011060051114
011070051124     ?* F12=Ritorno  ?
011080051207     c                   If        *InKl
011090091111      * se richiamato in interrogazione deve uscire dal pgm
011100091124     c                   if        $interroga or $modifica
011110091111     c                   ExSr      Sr_F03
011120091111     c                   else
011130051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
011140051221     c                   Eval      *In29 = *Off
011150051207     c                   ExSr      Sr_Tastifun
011160051207     c                   Goto      emi03
011170091111     c                   EndIf
011180051207     c                   EndIf
011190090717
011200090717     ?* F2=Rubrica  ?
011210090717     c                   If        *Inkb
011220090717     c                   Eval      wrag = v2crag
011230090717     c  n05              Eval      wrich = 'M'
011240090717     c   05              Eval      wrich = 'V'
011250090717     c                   clear                   wtnt
011260090717     c                   ExSr      Sr_F02
011270100607     c                   Goto      emi04
011280090717     c                   EndIf
011290060731
011300060731     ?* F4=Abilitazioni  ?
011310060731     c                   if        *inkd
011320060731     c                   exsr      sr_f04
011330060731     c                   goto      emi04
011340060731     c                   endif
011350100507
011360100507       //?F5=Attività
011370100507     c                   IF        *inke
011380100507     c                   exsr      sr_F05
011390100507     c                   goto      emi04
011400100507     c                   ENDIF
011410051128
011420051128     ?* F6=Conferma  ?
011430051128     c                   If        *InKf
011440051128     c                   ExSr      Sr_Conferma
011450051213     c   28              Goto      emi04
011460051222     c                   If        Not *In06 and Not *In07
011470091125     c                             and not $modifica
011480060110     c                   ExSr      Sr_Pulvid
011490051222     c                   Goto      emi01
011500051222     c                   Else
011510051222     c                   Goto      fine
011520051222     c                   EndIf
011530051128     c                   EndIf
011540051124
011550051124     ?* F7=Cliente Unificante  ?
011560051124     c                   If        *Inkg
011570051124     c                   ExSr      Sr_F07
011580051206     c                   Goto      emi04
011590051124     c                   EndIf
011600051124
011610051124     ?* F11=Luoghi  ?
011620051124     c                   If        *Inkk
011630051124     c                   ExSr      Sr_F11
011640051206     c                   Goto      emi04
011650051124     c                   EndIf
011660051206
011670051206     ?* F14=Info Commerciali  ?
011680051207     c                   If        *Inkn
011690060208     c                   If        v2ccpo = *Zeros
011700060208     c                   Eval      *In28 = *On
011710060208     c                   Eval      v4cmsg = msg(49)
011720060208     c                   Else
011730051207     c                   Eval      wrag = v3crag
011740051206     c                   ExSr      Sr_F14
011750060208     c                   EndIf
011760051206     c                   Goto      emi04
011770051206     c                   EndIf
011780051124
011790051124     ?* F15=Codici Collegati  ?
011800051124     c                   If        *Inkp
011810100127     c                   If        v4cscf = *Zeros or v4cscf = *blanks
011820100409     c                             or v4cscf = '000000?'
011830051124     c                   Eval      *In28 = *On
011840051130     c                   Eval      v4cmsg = msg(33)
011850051130     c                   Eval      v4cmsg = %trim(v4cmsg) + ' ' +
011860051125     c                             'non è possibile interrogare'
011870051124     c                   Else
011880100127     c                   Eval      w0070 = %dec(v4cscf:7:0)
011890051124     c                   ExSr      Sr_F15
011900051124     c                   EndIf
011910051206     c                   Goto      emi04
011920051124     c                   EndIf
011930051124
011940051124     ?* F18=Note  ?
011950051124     c                   If        *Inks
011960051130     c                   Eval      wrag = v3crag
011970051124     c                   ExSr      Sr_F18
011980051206     c                   Goto      emi04
011990051124     c                   EndIf
012000060529
012010060529     ?* F19=Variazioni  ?
012020060529     c                   If        *Inkt
012030060529     c                   ExSr      Sr_F19
012040060529     c                   Goto      emi04
012050060529     c                   EndIf
012060100406
012070100524       //?F22=Richiesta Contatto
012080100524     c                   IF        *inkw
012090100524     c                   exsr      sr_F22
012100100406     c                   goto      emi04
012110100406     c                   ENDIF
012120101213
012130101213       //?F17=Dati Fatturazione
012140101213     c                   IF        *inkr
012150101213     c                   exsr      sr_F17
012160101213     c                   goto      emi04
012170101213     c                   ENDIF
012180110223
012190110223       //?F21=Blocco clienti
012200110223     c                   IF        *inkv
012210110223     c                   exsr      sr_F21
012220110310      /free
012230110310       //?Se ho pianificato l'attività devo richiamare la gestione
012240110310       //?delle info commerciali
012250110310         IF  OMK17f12 = *blanks and OMK17err = *blanks;
012260110310           exsr sr_f14;
012270110310         ENDIF;
012280110310      /end-free
012290110223     c                   goto      emi04
012300110223     c                   ENDIF
012310051124
012320051124     ?* F24=Altri tasti  ?
012330051124     c                   If        *Inky and $loop > 1
012340051124     c                   ExSr      Sr_F24
012350051206     c                   Goto      emi04
012360051124     c                   EndIf
012370051108
012380051124     ?* Controllo  ?
012390051213     c  n05              ExSr      Sr_Ctrd04
012400051124     c   28
012410051130     cor 90              Goto      emi04
012420051219      * se interrogazione controllo i luoghi e le note
012430051219     c                   If        *In05
012440091112     c   70
012450091112     corn06              ExSr      Sr_Ctrl001
012460051219     c   28              Goto      emi04
012470051219     c                   ExSr      Sr_Ctrnt84
012480051219     c   28              Goto      emi04
012490051219     c                   EndIf
012500051107
012510051124     * §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ * ?
012520100728     ?*  PAGAMENTI VARI   ?
012530051130     ?*  Quinta videata      ?
012540051118
012550051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
012560051207     c                   ExSr      Sr_Tastifun
012570090612      * Blocco pagamenti
012580130322      * se Blocco pagamento NO e da stato del credito devo bloccare
012590130322     c                   If        wbloc = '0' and §4Wpbl = 'S'
012600130322      * se non sono in interrogazione
012610121002     c                             and not *in05 and
012620130322      * ed è stato variato lo stato del credito
012630121002     c                             V2Ccon <> %subst(CLPcon:2:2)
012640130322      * imposto il blocco dello stato del credito
012650130322     c                   Eval      wbloc = %subst(§4Wpjbkamm:1:1)
012660130322     c                   eval      V5Cblpedp = wbloc
012670060317     c                   EndIf
012680130322      * Imposto il blocco a video
012690130322     c                   If        wbloc = '0'
012700130322     c                   Eval      v5cblp = 'NO'
012710130322     c                   Else
012720100728     c                   Eval      v5cblp = 'SI'
012730060317     c                   EndIf
012740051207
012750051130     c     emi05         Tag
012760051129
012770051108     c                   Write     Ta60t01
012780051129     c                   Write     Ta60z01
012790051130     c                   Exfmt     Ta60d05
012800051108     c                   Eval      *In28 = *Off
012810051129     c                   Eval      *In90 = *Off
012820051130     c                   Clear                   v5cmsg
012830051213     c                   Eval      wvideo = '05'
012840051108
012850051129     ?* F3=Fine  ?
012860051108     c                   If        *InKc
012870060217     c                   ExSr      Sr_F03
012880060217     c   80              Goto      emi05
012890051108     c                   EndIf
012900051114
012910051129     ?* F12=Ritorno  ?
012920051207     c                   If        *InKl
012930091111      * se richiamato in interrogazione deve uscire dal pgm
012940091124     c                   if        $interroga or $modifica
012950091111     c                   ExSr      Sr_F03
012960091111     c                   else
012970051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
012980051207     c                   ExSr      Sr_Tastifun
012990051207     c                   Goto      emi04
013000091111     c                   EndIf
013010051207     c                   EndIf
013020090717
013030090717     ?* F2=Rubrica  ?
013040090717     c                   If        *Inkb
013050090717     c                   Eval      wrag = v2crag
013060090717     c  n05              Eval      wrich = 'M'
013070090717     c   05              Eval      wrich = 'V'
013080090717     c                   clear                   wtnt
013090090717     c                   ExSr      Sr_F02
013100100607     c                   Goto      emi05
013110090717     c                   EndIf
013120060731
013130060731     ?* F4=Abilitazioni  ?
013140060731     c                   if        *inkd
013150060731     c                   exsr      sr_f04
013160060731     c                   goto      emi05
013170060731     c                   endif
013180100507
013190100507       //?F5=Attività
013200100507     c                   IF        *inke
013210100507     c                   exsr      sr_F05
013220100507     c                   goto      emi05
013230100507     c                   ENDIF
013240051129
013250051129     ?* F6=Conferma  ?
013260051129     c                   If        *InKf
013270051129     c                   ExSr      Sr_Conferma
013280051213     c   28              Goto      emi05
013290051222     c                   If        Not *In06 and Not *In07
013300091125     c                             and not $modifica
013310060110     c                   ExSr      Sr_Pulvid
013320051222     c                   Goto      emi01
013330051222     c                   Else
013340051222     c                   Goto      fine
013350051222     c                   EndIf
013360051129     c                   EndIf
013370051129
013380051129     ?* F7=Cliente Unificante  ?
013390051129     c                   If        *Inkg
013400051129     c                   ExSr      Sr_F07
013410051130     c                   Goto      emi05
013420051129     c                   EndIf
013430051129
013440051129     ?* F11=Luoghi  ?
013450051129     c                   If        *Inkk
013460051129     c                   ExSr      Sr_F11
013470051130     c                   Goto      emi05
013480051129     c                   EndIf
013490051206
013500051206     ?* F14=Info Commerciali  ?
013510051207     c                   If        *Inkn
013520060208     c                   If        v2ccpo = *Zeros
013530060208     c                   Eval      *In28 = *On
013540060208     c                   Eval      v5cmsg = msg(49)
013550060208     c                   Else
013560051207     c                   Eval      wrag = v3crag
013570051206     c                   ExSr      Sr_F14
013580060208     c                   EndIf
013590051206     c                   Goto      emi05
013600051206     c                   EndIf
013610051129
013620051129     ?* F15=Codici Collegati  ?
013630051129     c                   If        *Inkp
013640100127     c                   If        v4cscf = *Zeros or v4cscf = *blanks
013650051129     c                   Eval      *In28 = *On
013660051130     c                   Eval      v5cmsg = msg(33)
013670051130     c                   Eval      v5cmsg = %trim(v5cmsg) + ' ' +
013680051129     c                             'non è possibile interrogare'
013690051129     c                   Else
013700100127     c                   Eval      w0070 = %dec(v4cscf:7:0)
013710051129     c                   ExSr      Sr_F15
013720051129     c                   EndIf
013730051130     c                   Goto      emi05
013740051129     c                   EndIf
013750051129
013760051129     ?* F18=Note  ?
013770051129     c                   If        *Inks
013780051130     c                   Eval      wrag = v3crag
013790051129     c                   ExSr      Sr_F18
013800051130     c                   Goto      emi05
013810051129     c                   EndIf
013820060529
013830060529     ?* F19=Variazioni  ?
013840060529     c                   If        *Inkt
013850060529     c                   ExSr      Sr_F19
013860060529     c                   Goto      emi05
013870060529     c                   EndIf
013880100406
013890100524       //?F22=Richiesta Contatto
013900100524     c                   IF        *inkw
013910100524     c                   exsr      sr_F22
013920100406     c                   goto      emi05
013930100406     c                   ENDIF
013940101213
013950101213       //?F17=Dati Fatturazione
013960101213     c                   IF        *inkr
013970101213     c                   exsr      sr_F17
013980101213     c                   goto      emi05
013990101213     c                   ENDIF
014000110223
014010110223       //?F21=Blocco clienti
014020110223     c                   IF        *inkv
014030110223     c                   exsr      sr_F21
014040110310      /free
014050110310       //?Se ho pianificato l'attività devo richiamare la gestione
014060110310       //?delle info commerciali
014070110310         IF  OMK17f12 = *blanks and OMK17err = *blanks;
014080110310           exsr sr_f14;
014090110310         ENDIF;
014100110310      /end-free
014110110223     c                   goto      emi04
014120110223     c                   ENDIF
014130051129
014140051129     ?* F24=Altri tasti  ?
014150051129     c                   If        *Inky and $loop > 1
014160051129     c                   ExSr      Sr_F24
014170051130     c                   Goto      emi05
014180051129     c                   EndIf
014190051129
014200051129     ?* Controllo  ?
014210051213     c  n05              ExSr      Sr_Ctrd05
014220051129     c   28
014230051130     cor 90              Goto      emi05
014240051219      * se interrogazione controllo i luoghi e le note
014250051219     c                   If        *In05
014260091112     c   70
014270091112     corn06              ExSr      Sr_Ctrl555
014280051219     c   28              Goto      emi05
014290091112     c   70
014300091112     corn06              ExSr      Sr_Ctrl666
014310051219     c   28              Goto      emi05
014320051219     c                   ExSr      Sr_Ctrnt85
014330051219     c   28              Goto      emi05
014340090616     c                   ExSr      Sr_Ctrnt89
014350090616     c   28              Goto      emi05
014360150424     c                   ExSr      Sr_Ctrnt82
014370100730     c   90
014380100730     cor 28              Goto      emi05
014390051219     c                   EndIf
014400051129
014410051129     * §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ * ?
014420051129     ?*  GESTIONE GIACENZE   ?
014430051130     ?*  Sesta videata      ?
014440051118
014450051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
014460051207     c                   ExSr      Sr_Tastifun
014470051207
014480120118     c     emi06         Tag
014490120118
014500120118      /free
014510120118       //?Se sono in convalida offerta e il campo tipo comunicazione al
014520120118       //?mittente non è impostato lo recupero dalla tabella '2G'
014530120118       //?valore di default
014540120118         IF  V6Ctcm = *blanks and *in07;
014550120118           V6Ctcm = §2Gtcm;
014560120118         ENDIF;
014570120118      /end-free
014580051206
014590051108     c                   Write     Ta60t01
014600051206     c                   Write     Ta60z01
014610051130     c                   Exfmt     Ta60d06
014620051108     c                   Eval      *In28 = *Off
014630051213     c                   Eval      *In90 = *Off
014640051130     c                   Clear                   v6cmsg
014650051213     c                   Eval      wvideo = '06'
014660051108
014670051206     ?* F3=Fine  ?
014680051108     c                   If        *InKc
014690060217     c                   ExSr      Sr_F03
014700060217     c   80              Goto      emi06
014710051108     c                   EndIf
014720051114
014730051206     ?* F12=Ritorno  ?
014740051207     c                   If        *InKl
014750091111      * se richiamato in interrogazione deve uscire dal pgm
014760091124     c                   if        $interroga or $modifica
014770091111     c                   ExSr      Sr_F03
014780091111     c                   else
014790051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
014800051207     c                   ExSr      Sr_Tastifun
014810051207     c                   Goto      emi05
014820091111     c                   EndIf
014830051207     c                   EndIf
014840090717
014850090717     ?* F2=Rubrica  ?
014860090717     c                   If        *Inkb
014870090717     c                   Eval      wrag = v2crag
014880090717     c  n05              Eval      wrich = 'M'
014890090717     c   05              Eval      wrich = 'V'
014900090717     c                   clear                   wtnt
014910090717     c                   ExSr      Sr_F02
014920100607     c                   Goto      emi06
014930090717     c                   EndIf
014940060731
014950060731     ?* F4=Abilitazioni  ?
014960060731     c                   if        *inkd
014970060731     c                   exsr      sr_f04
014980060731     c                   goto      emi06
014990060731     c                   endif
015000100507
015010100507       //?F5=Attività
015020100507     c                   IF        *inke
015030100507     c                   exsr      sr_F05
015040100507     c                   goto      emi06
015050100507     c                   ENDIF
015060051206
015070051206     ?* F6=Conferma  ?
015080051206     c                   If        *InKf
015090051206     c                   ExSr      Sr_Conferma
015100051213     c   28              Goto      emi06
015110051222     c                   If        Not *In06 and Not *In07
015120091125     c                             and not $modifica
015130060110     c                   ExSr      Sr_Pulvid
015140051222     c                   Goto      emi01
015150051222     c                   Else
015160051222     c                   Goto      fine
015170051222     c                   EndIf
015180051206     c                   EndIf
015190051206
015200051206     ?* F7=Cliente Unificante  ?
015210051206     c                   If        *Inkg
015220051206     c                   ExSr      Sr_F07
015230051206     c                   Goto      emi06
015240051206     c                   EndIf
015250051206
015260051206     ?* F11=Luoghi  ?
015270051206     c                   If        *Inkk
015280051206     c                   ExSr      Sr_F11
015290051206     c                   Goto      emi06
015300051206     c                   EndIf
015310051206
015320051206     ?* F14=Info Commerciali  ?
015330051207     c                   If        *Inkn
015340060208     c                   If        v2ccpo = *Zeros
015350060208     c                   Eval      *In28 = *On
015360060208     c                   Eval      v6cmsg = msg(49)
015370060208     c                   Else
015380051207     c                   Eval      wrag = v3crag
015390051206     c                   ExSr      Sr_F14
015400060208     c                   EndIf
015410051206     c                   Goto      emi06
015420051206     c                   EndIf
015430051206
015440051206     ?* F15=Codici Collegati  ?
015450051206     c                   If        *Inkp
015460100127     c                   If        v4cscf = *Zeros or v4cscf = *blanks
015470051206     c                   Eval      *In28 = *On
015480051206     c                   Eval      v6cmsg = msg(33)
015490051206     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
015500051206     c                             'non è possibile interrogare'
015510051206     c                   Else
015520100127     c                   Eval      w0070 = %dec(v4cscf:7:0)
015530051206     c                   ExSr      Sr_F15
015540051206     c                   EndIf
015550051206     c                   Goto      emi06
015560051206     c                   EndIf
015570051206
015580051206     ?* F18=Note  ?
015590051206     c                   If        *Inks
015600051206     c                   Eval      wrag = v3crag
015610051206     c                   ExSr      Sr_F18
015620051206     c                   Goto      emi06
015630051206     c                   EndIf
015640060529
015650060529     ?* F19=Variazioni  ?
015660060529     c                   If        *Inkt
015670060529     c                   ExSr      Sr_F19
015680060529     c                   Goto      emi06
015690060529     c                   EndIf
015700100406
015710100524       //?F22=Richiesta Contatto
015720100524     c                   IF        *inkw
015730100524     c                   exsr      sr_F22
015740100406     c                   goto      emi06
015750100406     c                   ENDIF
015760101213
015770101213       //?F17=Dati Fatturazione
015780101213     c                   IF        *inkr
015790101213     c                   exsr      sr_F17
015800101213     c                   goto      emi06
015810101213     c                   ENDIF
015820110223
015830110223       //?F21=Blocco clienti
015840110223     c                   IF        *inkv
015850110223     c                   exsr      sr_F21
015860110310      /free
015870110310       //?Se ho pianificato l'attività devo richiamare la gestione
015880110310       //?delle info commerciali
015890110310         IF  OMK17f12 = *blanks and OMK17err = *blanks;
015900110310           exsr sr_f14;
015910110310         ENDIF;
015920110310      /end-free
015930110223     c                   goto      emi06
015940110223     c                   ENDIF
015950051206
015960051206     ?* F24=Altri tasti  ?
015970051206     c                   If        *Inky and $loop > 1
015980051206     c                   ExSr      Sr_F24
015990051206     c                   Goto      emi06
016000051206     c                   EndIf
016010051206
016020051206     ?* Controllo  ?
016030051213     c  n05              ExSr      Sr_Ctrd06
016040051206     c   28
016050051206     cor 90              Goto      emi06
016060051219      * se interrogazione controllo i luoghi e le note
016070051219     c                   If        *In05
016080091112     c   70
016090091112     corn06              ExSr      Sr_Ctrl005
016100051219     c   28              Goto      emi06
016110051219     c                   ExSr      Sr_Ctrnt88
016120051219     c   28              Goto      emi06
016130091112     c   70
016140091112     corn06              ExSr      Sr_Ctrl300
016150060215     c   28              Goto      emi06
016160051219     c                   EndIf
016170051108
016180051130     * §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ * ?
016190051130     ?*  GESTIONE DANNI/TASSAZIONE PORTI ASSEGNATI/STOP AUTOTRASPORTATORI   ?
016200051130     ?*  Settima videata      ?
016210051213
016220051213      * Imposto il campo unico in fondo alla videata con i tasti funzione
016230051213     c                   ExSr      Sr_Tastifun
016240051118
016250051130     c     emi07         Tag
016260051108
016270051108     c                   Write     Ta60t01
016280051213     c                   Write     Ta60z01
016290051130     c                   Exfmt     Ta60d07
016300051108     c                   Eval      *In28 = *Off
016310051213     c                   Eval      *In90 = *Off
016320051130     c                   Clear                   v7cmsg
016330051213     c                   Eval      wvideo = '07'
016340051108
016350051213     ?* F3=Fine  ?
016360051108     c                   If        *InKc
016370060217     c                   ExSr      Sr_F03
016380060217     c   80              Goto      emi07
016390051108     c                   EndIf
016400051114
016410051213     ?* F12=Ritorno  ?
016420051213     c                   If        *InKl
016430091111      * se richiamato in interrogazione deve uscire dal pgm
016440091124     c                   if        $interroga or $modifica
016450091111     c                   ExSr      Sr_F03
016460091111     c                   else
016470051213      * Imposto il campo unico in fondo alla videata con i tasti funzione
016480051213     c                   ExSr      Sr_Tastifun
016490051213     c                   Goto      emi06
016500091111     c                   EndIf
016510051213     c                   EndIf
016520090717
016530090717     ?* F2=Rubrica  ?
016540090717     c                   If        *Inkb
016550090717     c                   Eval      wrag = v2crag
016560090717     c  n05              Eval      wrich = 'M'
016570090717     c   05              Eval      wrich = 'V'
016580090717     c                   clear                   wtnt
016590090717     c                   ExSr      Sr_F02
016600100607     c                   Goto      emi07
016610090717     c                   EndIf
016620060731
016630060731     ?* F4=Abilitazioni  ?
016640060731     c                   if        *inkd
016650060731     c                   exsr      sr_f04
016660060731     c                   goto      emi07
016670060731     c                   endif
016680100507
016690100507       //?F5=Attività
016700100507     c                   IF        *inke
016710100507     c                   exsr      sr_F05
016720100507     c                   goto      emi07
016730100507     c                   ENDIF
016740051213
016750051213     ?* F6=Conferma  ?
016760051213     c                   If        *InKf
016770051213     c                   ExSr      Sr_Conferma
016780051213     c   28              Goto      emi07
016790051222     c                   If        Not *In06 and Not *In07
016800091125     c                             and not $modifica
016810060110     c                   ExSr      Sr_Pulvid
016820051222     c                   Goto      emi01
016830051222     c                   Else
016840051222     c                   Goto      fine
016850051222     c                   EndIf
016860051213     c                   EndIf
016870051213
016880051213     ?* F7=Cliente Unificante  ?
016890051213     c                   If        *Inkg
016900051213     c                   ExSr      Sr_F07
016910051213     c                   Goto      emi07
016920051213     c                   EndIf
016930051213
016940051213     ?* F11=Luoghi  ?
016950051213     c                   If        *Inkk
016960051213     c                   ExSr      Sr_F11
016970051213     c                   Goto      emi07
016980051213     c                   EndIf
016990051213
017000051213     ?* F14=Info Commerciali  ?
017010051213     c                   If        *Inkn
017020060208     c                   If        v2ccpo = *Zeros
017030060208     c                   Eval      *In28 = *On
017040060208     c                   Eval      v7cmsg = msg(49)
017050060208     c                   Else
017060051213     c                   Eval      wrag = v3crag
017070051213     c                   ExSr      Sr_F14
017080060208     c                   EndIf
017090051213     c                   Goto      emi07
017100051213     c                   EndIf
017110051213
017120051213     ?* F15=Codici Collegati  ?
017130051213     c                   If        *Inkp
017140100127     c                   If        v4cscf = *Zeros or v4cscf = *blanks
017150051213     c                   Eval      *In28 = *On
017160051213     c                   Eval      v7cmsg = msg(33)
017170051213     c                   Eval      v7cmsg = %trim(v7cmsg) + ' ' +
017180051213     c                             'non è possibile interrogare'
017190051213     c                   Else
017200100127     c                   Eval      w0070 = %dec(v4cscf:7:0)
017210051213     c                   ExSr      Sr_F15
017220051213     c                   EndIf
017230051213     c                   Goto      emi07
017240051213     c                   EndIf
017250051213
017260051213     ?* F18=Note  ?
017270051213     c                   If        *Inks
017280051213     c                   Eval      wrag = v3crag
017290051213     c                   ExSr      Sr_F18
017300051213     c                   Goto      emi07
017310051213     c                   EndIf
017320060529
017330060529     ?* F19=Variazioni  ?
017340060529     c                   If        *Inkt
017350060529     c                   ExSr      Sr_F19
017360060529     c                   Goto      emi07
017370060529     c                   EndIf
017380100406
017390100524       //?F22=Richiesta Contatto
017400100524     c                   IF        *inkw
017410100524     c                   exsr      sr_F22
017420100406     c                   goto      emi07
017430100406     c                   ENDIF
017440101213
017450101213       //?F17=Dati Fatturazione
017460101213     c                   IF        *inkr
017470101213     c                   exsr      sr_F17
017480101213     c                   goto      emi07
017490101213     c                   ENDIF
017500110223
017510110223       //?F21=Blocco clienti
017520110223     c                   IF        *inkv
017530110223     c                   exsr      sr_F21
017540110310      /free
017550110310       //?Se ho pianificato l'attività devo richiamare la gestione
017560110310       //?delle info commerciali
017570110310         IF  OMK17f12 = *blanks and OMK17err = *blanks;
017580110310           exsr sr_f14;
017590110310         ENDIF;
017600110310      /end-free
017610110223     c                   goto      emi07
017620110223     c                   ENDIF
017630051213
017640051213     ?* F24=Altri tasti  ?
017650051213     c                   If        *Inky and $loop > 1
017660051213     c                   ExSr      Sr_F24
017670051213     c                   Goto      emi07
017680051213     c                   EndIf
017690051213
017700051213     ?* Controllo  ?
017710051213     c  n05              ExSr      Sr_Ctrd07
017720051219     c   28              Goto      emi07
017730051219      * se interrogazione controllo i luoghi e le note
017740051219     c                   If        *In05
017750091112     c   70
017760091112     corn06              ExSr      Sr_Ctrl006
017770051219     c   28              Goto      emi07
017780051219     c                   ExSr      Sr_Ctrnt87
017790051219     c   28              Goto      emi07
017800091112     c   70
017810091112     corn06              ExSr      Sr_Ctrl008
017820051219     c   28              Goto      emi07
017830091112     c   70
017840091112     corn06              ExSr      Sr_Ctrl002
017850060215     c   28              Goto      emi07
017860060215     c                   ExSr      Sr_Ctrnt83
017870060215     c   28              Goto      emi07
017880091112     c   70
017890091112     corn06              ExSr      Sr_Ctrl200
017900060215     c   28              Goto      emi07
017910060215     c                   ExSr      Sr_Ctrnt86
017920060215     c   28              Goto      emi07
017930051219     c                   EndIf
017940051213
017950051220     c  n05              Goto      emi07
017960051220
017970051220      * Se sono in interrogazione devo far vedere l'anagrafica clienti ritiro
017980051220      * visto che non è previsto il tasto F6 di conferma
017990051220      * Se però non è attiva la procedura ORM ritorno alla videata
018000160905      * se sto gestendo/interrogando cliente LOGISTICA no anagrafica clienti
018010160905      * ritiro
018020051220     c                   Movel     acoksc        w0030
018030051220     c                   Clear                   og148
018040051220     c     w0030         Chain     Azorg01l
018050051220     c                   If        %Found(Azorg01l)
018060051220     c                   Eval      og148 = orgde8
018070051220     c                   EndIf
018080120313     c                   If        §ogorm = *blanks
018090051220     c                   Goto      emi07
018100051220     c                   EndIf
018110160905     c                   IF        ClienteLog
018120160905     c                   goto      Fine
018130160905     c                   ENDIF
018140160905      /end-free
018150051220      * Richiamo l'interrogazione anagrafica clienti ritiro
018160070809     c                   reset                   FIOR37ds
018170070809     c                   eval      I37opz  = '5'
018180070809     c                   movel     ACOksc        I37fgs
018190070809     c                   movel     ACOksc        I37cro
018200070809     c                   movel(p)  FIOR37ds      KPJBU
018210070809     c                   call      'FIOR37R'
018220070809     c                   parm                    KPJBA
018230091111     c                   eval      fior37ds = kpjbu
018240051220
018250091124      * Se non richiamato neanche per interrogazione/modifica diretta
018260051220     c                   If        Not *In06 and Not *In07
018270091124     c                             and not $interroga
018280091124     c                             and not $modifica
018290060110     c                   ExSr      Sr_Pulvid
018300051220     c                   Goto      emi01
018310051220     c                   EndIf
018320051108
018330051107     c     Fine          Tag
018340060215
018350060215      * Se premuto F03 ritorno il dato al pgm chiamante
018360091113     c                   if        $pgmrich and
018370091113     c                             (*Inkc or w03cok = 'SI')
018380060215     c                   Eval      ta60f03 = '1'
018390060215     c                   EndIf
018400051221
018410051221      * Se richiamato da gestione visite passo i dati alla DS
018420051221     c                   If        *In06
018430051221     c                   Eval      ta60cpo = acolib
018440060607     c                   Eval      ta60cmm = clpage
018450051221     c                   EndIf
018460051108
018470051108     c                   If        wtibs69 = *On
018480051108     c                   Eval      i69tla = 'C'
018490051108     c                   Call      'TIBS69R'
018500051108     c                   Parm                    Tibs69ds
018510051108     c                   EndIf
018520060531     c                   If        wtibs73 = *On
018530060531     c                   Eval      ibs73tla = 'C'
018540060531     c                   Call      'TIBS73R'
018550060531     c                   Parm                    Tibs73ds
018560060531     c                   EndIf
018570081020     c                   If        wtrmk50 = *On
018580081020     c                   Eval      i50tla=    'C'
018590081020     c                   Call      'TRMK50R'
018600081020     c                   Parm                    kpjba
018610081020     c                   Parm                    TRMK50ds
018620081020     c                   EndIf
018630100127
018640100127     c                   IF        wtnta40
018650100127     c                   eval      ITA40tla = 'C'
018660100127     c                   call      'TNTA40R'
018670100127     c                   parm                    kpjba
018680100127     c                   parm                    TNTA40DS
018690100127     c                   ENDIF
018700160803
018710160803      /free
018720160803       //?Chiudo file storicizzazione variazioni potenziale
018730160803         clear TRMK30DS;
018740160803         clear tncpo_30;
018750160803         clear tncpo1_30;
018760160803         clear ticpi_30;
018770160803         IMK30tla = 'C';
018780160803         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
018790160803      /end-free
018800051107
018810051107     c                   Eval      *InLr = *On
018820051108
018830051108      *------------------------------------------------------------------------*
018840060110      * PULIZIA DELLE VIDEATE
018850051108      *------------------------------------------------------------------------*
018860060110     c     Sr_Pulvid     BegSr
018870060110
018880060110     c                   Clear                   h1riga
018890060110     c                   Clear                   h1colo
018900060110     c                   Clear                   h2riga
018910060110     c                   Clear                   h2colo
018920060110     c                   Clear                   h3riga
018930060110     c                   Clear                   h3colo
018940060110     c                   Clear                   h4riga
018950060110     c                   Clear                   h4colo
018960060110     c                   Clear                   h5riga
018970060110     c                   Clear                   h5colo
018980060110     c                   Clear                   h6riga
018990060110     c                   Clear                   h6colo
019000060110     c                   Clear                   h7riga
019010060110     c                   Clear                   h7colo
019020051108
019030060110      * Prima videata
019040051108     c                   Eval      v1ckcc = dutkci
019050051108     c                   Clear                   v1cksc
019060051108     c                   Clear                   v1crag
019070051108     c                   Clear                   skfil
019080051108     c                   Clear                   v1ccdf
019090051108     c                   Clear                   v1civa
019100051108
019110051108      * Imposto la sk dei p.o. di ricerca
019120051108      * se autorizzazzione diversa da distretto e se sono in filiale
019130051115     c                   If        wabi <> 'DI' and Not *In09
019140051108     c                   Clear                   yy
019150051108     c                   Do        24            xx
019160051108     c                   If        skpog(xx) <> *Blanks and skpog(xx) > *Zeros
019170051108     c                   Add       1             yy
019180051108     c                   Move      skpog(xx)     skfil(yy)
019190051108     c                   EndIf
019200051108     c                   EndDo
019210051108     c                   EndIf
019220060110
019230060110      * Seconda videata
019240060110     c                   Clear                   v2cduv
019250060110     c                   Clear                   v2ddtr
019260060110     c                   Clear                   v2cdtr
019270060110     c                   Clear                   v2copz
019280060110     c                   Clear                   v2ctic
019290060110     c                   Clear                   v2clin
019300060110     c                   Clear                   v2crag
019310060110     c                   Clear                   v2cvia
019320060110     c                   Clear                   v2ccit
019330060110     c                   Clear                   v2ccae
019340060110     c                   Clear                   v2cprv
019350060110     c                   Clear                   v2csta
019360060110     c                   Clear                   v2ctel
019370060110     c                   Clear                   v2ctlf
019380060110     c                   Clear                   v2ccdf
019390060110     c                   Clear                   v2civa
019400060110     c                   Clear                   v2cabl
019410060110     c                   Clear                   v2cblc
019420060110     c                   Clear                   v2ccon
019430060110     c                   Clear                   v2tb02
019440060110     c                   Clear                   v2ccpo
019450090616     c                   Clear                   v2csol
019460090616     c                   Clear                   v2fsol
019470091119     c                   clear                   v2ntcdc
019480091119     c                   clear                   savntcdc
019490060110      * Terza videata
019500060110     c                   Clear                   v3cage
019510060110     c                   Clear                   v3cclv
019520060110     c                   Clear                   v3citc
019530121119     c                   clear                   v3cnew
019540060110      * Quarta videata
019550060110     c                   Clear                   v4cscf
019560060110     c                   Clear                   savscf
019570060110     c                   Clear                   v4cuni
019580060110     c                   Clear                   savuni
019590060110     c                   Clear                   v4ctft
019600060110     c                   Clear                   savtft
019610060110     c                   Clear                   v4cfft
019620060110     c                   Clear                   savfft
019630060110     c                   Clear                   v4ctdf
019640060110     c                   Clear                   v4csin
019650060110     c                   Clear                   v4cdps
019660060110     c                   Clear                   v4cdus
019670060110     c                   Clear                   v4cnpn
019680060110     c                   Clear                   v4cnpr
019690060110     c                   Clear                   v4cdpr
019700100727     c                   Clear                   v4ccdp
019710100727     c                   Clear                   v4cbna
019720100727     c                   Clear                   v4cabi
019730100727     c                   Clear                   v4ccab
019740121105     c                   clear                   V4Ctpf
019750060110      * Quinta videata
019760060110     c                   Clear                   wbloc
019770100728     c                   Clear                   v5cblp
019780060110     c                   Clear                   v5cfpc
019790080115     c                   clear                   v5ciban
019800060110     c                   Clear                   v5cbab
019810100729     c                   clear                   v5tpdn
019820100728     c                   clear                   v5ibandn
019830100728     c                   clear                   v5babdn
019840140203     c                   clear                   v5bicdn
019850100729     c                   clear                   v5tpna
019860100728     c                   clear                   v5ibanna
019870100728     c                   clear                   v5babna
019880140203     c                   clear                   v5bicna
019890150424     c                   eval      BonificoCA = *off
019900150424     c                   eval      BonificoDN = *off
019910150424     c                   eval      BonificoNA = *off
019920150427     c                   eval      PagEsteroCA = *off
019930150427     c                   eval      PagEsteroDN = *off
019940150427     c                   eval      PagEsteroNA = *off
019950150427     c                   clear                   CodPagCA
019960150427     c                   clear                   CodPagDN
019970150427     c                   clear                   CodPagNA
019980060110      * Sesta videata
019990060110     c                   Clear                   v6ctcm
020000060110     c                   Clear                   v6ctcf
020010060110     c                   Clear                   v6capg
020020060110     c                   Clear                   v6cdmg
020030060801     c                   Clear                   v6ccag
020040060110      * Settima videata
020050060110     c                   Clear                   v7csdn
020060060110     c                   Clear                   v7csin
020070060110     c                   Clear                   v7ctpa
020080060110     c                   Clear                   v7cstp
020090060317
020100060317      * Pulisco la ds dello stato del credito per non sporcarmi i dati
020110060317      * del blocco tra un cliente e l'altro
020120060317     c                   Clear                   ds4w
020130100805
020140100805     c                   clear                   wbic
020150100805     c                   clear                   wcodpn
020160100805     c                   clear                   wcodpo
020170100805     c                   clear                   wiban
020180100805     c                   clear                   wpgm
020190060110
020200051108     c                   EndSr
020210051107
020220051107      *------------------------------------------------------------------------*
020230051107      * CONTROLLO PRIMA VIDEATA - DATI DI RICERCA
020240051107      *------------------------------------------------------------------------*
020250051107     c     Sr_Ctrd01     BegSr
020260051117
020270051117      * Pulisco i campi del posizionamento cursore
020280051117     c                   Clear                   h1riga
020290051117     c                   Clear                   h1colo
020300051117      * Spengo indicatori di posizionamento cursore
020310051117     c                   Setoff                                       414243
020320051117     c                   Setoff                                       444546
020330051117     c                   Setoff                                       474849
020340051117     c                   Setoff                                       505152
020350051117     c                   Setoff                                       535455
020360051117     c                   Setoff                                       565758
020370051117     c                   Setoff                                       596061
020380051117     c                   Setoff                                       626364
020390060110      * Spengo indicatori di comodo
020400090914     c                   Setoff                                       0102
020410060110     c                   Setoff                                       04
020420060711
020430060711     c                   eval      wforzaksc = *off
020440051107
020450051107      * Almeno un dato deve essere inserito
020460051107     c                   If        v1ckcc = *Zeros and v1cksc = *Zeros and
020470051108     c                             v1crag = *Blanks and
020480051107     c                             v1ccdf = *Blanks and v1civa = *Blanks
020490051111     c                   Eval      *In28 = *On
020500060109     c                   Eval      h1riga = 06
020510051118     c                   Eval      h1colo = 04
020520051111     c                   Eval      v1cmsg = msg(02)
020530051107     c                   Leavesr
020540051107     c                   EndIf
020550051107
020560051107      * CAPOCONTO
020570051107     c                   If        v1ckcc = *zeros
020580051107     c                   Eval      *In28 = *On
020590060109     c                   Eval      h1riga = 06
020600051118     c                   Eval      h1colo = 04
020610051107     c                   Eval      v1cmsg = msg(03)
020620051107     c                   Leavesr
020630051107     c                   EndIf
020640051107
020650051219      * Se sono in filiale posso solo gestire i capoconti presenti in £4
020660051219      * per ora c'è solo il capoconto clienti (151)
020670051219if  1c                   If        Not *In09
020680051107     c                   Move      v1ckcc        w004a
020690051107     c     w004a         Lookup    £4                                     30
020700051107     c                   If        Not *In30
020710051107     c                   Eval      *In28 = *On
020720060109     c                   Eval      h1riga = 06
020730051118     c                   Eval      h1colo = 04
020740051107     c                   Eval      v1cmsg = msg(04)
020750051107     c                   Leavesr
020760051107     c                   EndIf
020770051219    1c                   EndIf
020780051219
020790051219      * CODICE CLIENTE NON IMMESSO
020800051219if  1c                   If        v1cksc = *Zeros
020810060207      * se non c'è una ricerca per ragione sociale errore
020820060227     c                   If        v1crag = *Blanks and
020830060227     c                             v1ccdf = *Blanks and v1civa = *Blanks
020840060207     c                   Eval      *In28 = *On
020850060207     c                   Eval      h1riga = 06
020860060207     c                   Eval      h1colo = 12
020870060207     c                   Eval      v1cmsg = msg(02)
020880060207     c                   Leavesr
020890060207     c                   EndIf
020900051219
020910051219      * Controllo la sk dei p.o. di ricerca
020920051219     c                   ExSr      Sr_Ctrfil
020930051219
020940051219s   2c                   Select
020950051219      * Ricerca Alfabetica
020960051219     c                   When      v1crag <> *Blanks
020970051219     c                   Movel     rsut          pardut
020980051219     c                   Movea     skfil         parfil
020990051219     c                   Movel     v1ckcc        parkcc
021000051219     c                   Clear                   pardit
021010051219     c                   Z-add     1             paxnum
021020051219      * 2 = richiesta di visualizzazione del cliente unificante
021030051219     c                   Z-add     2             kkut
021040051219     c                   Call      'XALFA3BR'
021050051219     c                   Parm                    pardut
021060051219     c                   Parm                    kkut
021070051219     c                   Parm                    v1crag
021080051219     c                   Parm                    parkcc
021090051219     c                   Parm                    parsta
021100051219     c                   Parm                    parfil
021110051219     c                   Parm                    pardit
021120051219     c                   Parm                    paxnum
021130051219     c                   Parm                    paxkcm
021140051219     c                   Parm                    paxksm
021150051219     c                   Parm                    paxkdm
021160051219     c                   If        parsta = -1
021170051219     c                   Goto      emi01
021180051219     c                   EndIf
021190051219     c                   Movel     paxksm        v1cksc
021200051219     c                   If        parkcc = *Zeros
021210051219     c                   Movel     paxkcm        v1ckcc
021220051219     c                   EndIf
021230051219      * Ricerca per Codice Fiscale
021240051219     c                   When      v1ccdf <> *Blanks
021250051219     c                   Movel     rsut          pardut
021260051219     c                   Movea     skfil         parfil
021270051219     c                   Movel     v1ckcc        parkcc
021280051219     c                   Clear                   pardit
021290051219     c                   Z-add     1             paxnum
021300051219      * 2 = richiesta di visualizzazione del cliente unificante
021310051219     c                   Z-add     2             kkut
021320051219     c                   Clear                   pariva
021330051219     c                   Movel     v1ccdf        parcdf
021340051219     c                   Call      'XALFA3BR'
021350051219     c                   Parm                    pardut
021360051219     c                   Parm                    kkut
021370051219     c                   Parm                    v1crag
021380051219     c                   Parm                    parkcc
021390051219     c                   Parm                    parsta
021400051219     c                   Parm                    parfil
021410051219     c                   Parm                    pardit
021420051219     c                   Parm                    paxnum
021430051219     c                   Parm                    paxkcm
021440051219     c                   Parm                    paxksm
021450051219     c                   Parm                    paxkdm
021460051219     c                   Parm                    paresci
021470051219     c                   Parm                    parerr
021480051219     c                   Parm                    pariva
021490051219     c                   Parm                    parcdf
021500051219     c                   If        parsta = -1
021510051219     c                   Goto      emi01
021520051219     c                   EndIf
021530051219     c                   Movel     paxksm        v1cksc
021540051219     c                   Clear                   v1ccdf
021550051219     c                   If        parkcc = *Zeros
021560051219     c                   Movel     paxkcm        v1ckcc
021570051219     c                   EndIf
021580051219      * Ricerca per Partita IVA
021590051219     c                   When      v1civa <> *Blanks
021600051219     c                   Movel     rsut          pardut
021610051219     c                   Movea     skfil         parfil
021620051219     c                   Movel     v1ckcc        parkcc
021630051219     c                   Clear                   pardit
021640051219     c                   Z-add     1             paxnum
021650051219      * 2 = richiesta di visualizzazione del cliente unificante
021660051219     c                   Z-add     2             kkut
021670051219      * Se non ho impostato il codice nazione vado in ricerca con la sola
021680060109      * partita Iva altrimenti P.IVa + Nazione
021690051219     c                   If        v1ivaeu = *Blanks
021700051219     c                   Movel     v1ivait       pariva
021710051219     c                   Else
021720051219     c                   Movel     v1civa        pariva
021730051219     c                   EndIf
021740051219     c                   Clear                   parcdf
021750051219     c                   Call      'XALFA3BR'
021760051219     c                   Parm                    pardut
021770051219     c                   Parm                    kkut
021780051219     c                   Parm                    v1crag
021790051219     c                   Parm                    parkcc
021800051219     c                   Parm                    parsta
021810051219     c                   Parm                    parfil
021820051219     c                   Parm                    pardit
021830051219     c                   Parm                    paxnum
021840051219     c                   Parm                    paxkcm
021850051219     c                   Parm                    paxksm
021860051219     c                   Parm                    paxkdm
021870051219     c                   Parm                    paresci
021880051219     c                   Parm                    parerr
021890051219     c                   Parm                    pariva
021900051219     c                   Parm                    parcdf
021910051219     c                   If        parsta = -1
021920051219     c                   Goto      emi01
021930051219     c                   EndIf
021940051219     c                   Movel     paxksm        v1cksc
021950051219     c                   Clear                   v1civa
021960051219     c                   If        parkcc = *Zeros
021970051219     c                   Movel     paxkcm        v1ckcc
021980051219     c                   EndIf
021990051219    2c                   EndSl
022000051219    1c                   EndIf
022010051107
022020051107      * CODICE CLIENTE IMMESSO
022030051108if  1c                   If        v1cksc <> *Zeros
022040060711      * per prima cosa cerco il commerciale unificante del
022050060711      * cliente immesso
022060060711      * aggancio cnlcp effettivo xchè sono in prima videata quindi sono
022070060711      * per forza nell'anagrafica effettiva e non nella provvisoria
022080060711     c                   Eval      kksc = v1cksc
022090060711     c     kcnaco        chain(n)  cnclp00f
022100060711    2c                   if        %found(cnclp00f)
022110130806     c     CLPage        chain     AZCMM000
022120130806    3c                   if        Not  %found(AZCMM01L)
022130130806     c                   clear                   CMMuni
022140060711    3c                   endif
022150130806     c                   movel     CMMuni        wpoageuni
022160060711    2c                   endif
022170160905      * Cerco network del cliente
022180160905     c                   Clear                   og143
022190160905     c                   Movel     v1cksc        w0030
022200160905     c     w0030         Chain     Azorg01l
022210160905     c                   If        %Found(Azorg01l)
022220160905     c                   Eval      og143 = orgde3
022230160905     c                   EndIf
022240160905     c                   eval      ClienteLog = (§OGntw = 'LOG')
022250051117      * Controllo se cliente gestito dall'utente
022260051219      * se non sono in interrogazione
022270051219if  2c                   If        Not *In05
022280161005     c                   IF        §UTEksclog <> 'S' and
022290161005     c                             ClienteLog
022300161005     c                   Eval      *In28 = *On
022310161005     c                   Eval      h1riga = 06
022320161005     c                   Eval      h1colo = 12
022330161005     c                   Eval      v1cmsg = msg(10)
022340161005     c                   Leavesr
022350161005     c                   ENDIF
022360051117     c                   Movel     v1cksc        w003a
022370051117     c     w003a         Lookup    skpog                                  30
022380060711     c                   If        Not *In30
022390060711    4c                   if        wpoageuni <> *zeros
022400060711     c     wpoageuni     lookup    skpog                                  30
022410060711    5c                   if        not *In30
022420060711     c                   eval      *In28 = *On
022430060711     c                   eval      h1riga = 06
022440060711     c                   eval      h1colo = 12
022450060711     c                   eval      v1cmsg = msg(10)
022460060711     c                   leavesr
022470060711    5c                   endif
022480060711     c                   eval      wforzaksc = *on
022490060711    4c                   endif
022500060711    4c                   if        wforzaksc = *off
022510051117     c                   Eval      *In28 = *On
022520060109     c                   Eval      h1riga = 06
022530051117     c                   Eval      h1colo = 12
022540051117     c                   Eval      v1cmsg = msg(10)
022550051117     c                   Leavesr
022560060711    4c                   endif
022570051117     c                   EndIf
022580051219   x2c                   Else
022590051219      * Se sono in interrogazione e non sono utente di sede non posso
022600051219      * interrogare i clienti logistica o XXX
022610051219if  3c                   If        Not *In09
022620051219     c                   Move      v1ckcc        w004a
022630051219     c     w004a         Lookup    £4                                     30
022640051219if  4c                   If        *In30
022650161005     c                   If        §ogntw = 'LOG' or §ogntw = 'XXX'
022660051219     c                   Eval      *In28 = *On
022670060109     c                   Eval      h1riga = 06
022680051219     c                   Eval      h1colo = 12
022690051219     c                   Eval      v1cmsg = msg(10)
022700051219     c                   Leavesr
022710051219     c                   EndIf
022720051219    4c                   EndIf
022730051219    3c                   EndIf
022740051219    2c                   EndIf
022750051219      * Aggancio l'anagrafica
022760051219      * solo cnaco xchè se siamo in prima videata è x forza anagrafica
022770051219      * effettiva, la provvisoria è solo se richiamato
022780051222     c                   Eval      kksc = v1cksc
022790060111     c  n05kCnaco        Chain(e)  Cnaco00f
022800060111     c   05kCnaco        Chain(n)  Cnaco00f
022810051219      * Allocato errore
022820060220if  2c                   If        %Error and Not *In05
022830051219     c                   Eval      *In28 = *on
022840060109     c                   Eval      h1riga = 06
022850051219     c                   Eval      h1colo = 12
022860051219     c                   Eval      v1cmsg = msg(68)
022870051219     c                   Leavesr
022880051219    2c                   EndIf
022890051108if  2c                   If        %Found(Cnaco00f)
022900051213      * Cliente esistente - Manutenzione/Visualizzazione
022910051115     c                   If        acoflg <> '*'
022920051115     c  n05              Eval      *In02 = *On
022930051115     c  n05              Eval      vtcmod = mod(02)
022940051115     c   05              Eval      vtcmod = mod(03)
022950051107      * Cliente annullato
022960051107     c                   Else
022970051107     c                   Eval      *In04 = *On
022980051213     c                   Eval      *In05 = *On
022990051108     c                   Eval      vtcmod = mod(04)
023000051107     c                   EndIf
023010051107      * Cliente nuovo - immissione
023020051108   x2c                   Else
023030051116      * --> Se gestione clienti inserisco
023040051115if  3c                   If        Not *In05
023050051107     c                   Eval      *In01 = *On
023060051108     c                   Eval      vtcmod = mod(01)
023070051116      * --> Se interrogazione clienti errore
023080051115   x3c                   Else
023090051115     c                   Eval      *In28 = *On
023100060109     c                   Eval      h1riga = 06
023110051117     c                   Eval      h1colo = 12
023120051115     c                   Eval      v1cmsg = msg(05)
023130051115     c                   Leavesr
023140051115    3c                   EndIf
023150051108    2c                   EndIf
023160051219    1c                   EndIf
023170051116
023180051213      * Se non sono in interrogazione
023190051213     c                   If        Not *In05
023200051116      * PASSWORD
023210051116     c                   If        v1cpwd = *Blanks
023220051116     c                   Eval      *In28 = *On
023230051117     c                   Eval      h1riga = 21
023240051117     c                   Eval      h1colo = 29
023250060109     c                   Eval      v1cmsg = msg(07)
023260051116     c                   Leavesr
023270051116     c                   EndIf
023280051116      * Controllo se esatta
023290051116     c                   If        v1cpwd <> §mtcpwd
023300051116     c                   Eval      *In28 = *On
023310051117     c                   Eval      h1riga = 21
023320051117     c                   Eval      h1colo = 29
023330060109     c                   Eval      v1cmsg = msg(06)
023340051116     c                   Leavesr
023350051116     c                   EndIf
023360051116      * Controllo la validità se non sono in sede
023370051213if  2c                   If        Not *In09
023380051116      * Data immissione/variazione password
023390051116     c                   Clear                   wlbdat
023400051116     c                   Z-Add     §mtcdta       g02dat
023410051116     c                   Call      'XSRDA8'
023420051116     c                   Parm                    wlbdat
023430051116     c                   Movel     g02inv        dataiso
023440051116      * Scadenza password
023450051116     c                   Clear                   Tibs02ds
023460051116     c                   Eval      t02mod = 'C'
023470051116     c                   Eval      t02sif = knsif
023480051116     c                   Eval      t02cod = 'MTC'
023490051125     c                   Eval      t02ke1 = 'PSW'
023500051116     c                   Call      'TIBS02R'
023510051116     c                   Parm                    kpjba
023520051116     c                   Parm                    Tibs02ds
023530051213if  3c                   If        t02err <> *Blanks
023540051116     c                   Eval      *In28 = *On
023550051117     c                   Eval      h1riga = 21
023560051117     c                   Eval      h1colo = 29
023570051116     c                   Eval      v1cmsg = msg(09)
023580051116     c                   Leavesr
023590051213   x3c                   Else
023600051116     c                   Movel     t02uni        wgiorni
023610051116     c                   Adddur    wgiorni:*d    dataiso
023620051116     c                   Move      dataiso       datascad
023630051213if  4c                   If        dataoggi > dataScad
023640051116     c                   Eval      *In28 = *On
023650051117     c                   Eval      h1riga = 21
023660051117     c                   Eval      h1colo = 29
023670051116     c                   Eval      v1cmsg = msg(08)
023680051116     c                   Leavesr
023690051213    4c                   EndIf
023700051213    3c                   EndIf
023710051213    2c                   EndIf
023720051213    1c                   EndIf
023730051107
023740051107     c                   EndSr
023750051108
023760051108      *------------------------------------------------------------------------*
023770051108      * CONTROLLO SK DEI P.O. DI RICERCA
023780051108      *------------------------------------------------------------------------*
023790051108     c     Sr_Ctrfil     BegSr
023800051108
023810051108do  1c                   Do        24            xx
023820051108if  2c                   If        skfil(xx) <> *Blanks
023830051108     c     '?'           Scan      skfil(xx)                              30
023840051108if  3c                   If        *In30
023850051108     c                   Eval      §tip = 'E'
023860051108     c                   call      'TNSD23R'
023870051108     c                   Parm                    §tip
023880051108     c                   Parm                    §fil
023890051108
023900051108      * Imposto i p.o. selezionati
023910121130if  4c                   If        §fil <> *Blanks and §fil > *Zeros
023920051108     c                   Movea     §fil          skpo
023930051108     c                   Eval      yy = 1
023940051108if  5c                   Dow       skpo(yy) >*Zeros and xx <= 24
023950051108     c     '?'           Scan      skfil(xx)                              30
023960051108if  6c                   If        *In30 or skfil(xx) <= *Zeros
023970051108     c                   Movel     skpo(yy)      skfil(xx)
023980051108     c                   Add       1             yy
023990051108    6c                   EndIf
024000051108     c                   Add       1             xx
024010051108    5c                   EndDo
024020051108   x4c                   Else
024030051108     c                   Clear                   skfil(xx)
024040051108    4c                   EndIf
024050051108     c                   Leavesr
024060051108    3c                   EndIf
024070051108
024080051108      * Controllo i p.o. di ricerca immessi
024090121130if  3c                   If        skfil(xx) <> *Blanks and skfil(xx) > *Zeros
024100051108     c                   Move      skfil(xx)     w0030
024110160905     c                   clear                   og143
024120051108     c     w0030         Chain     Azorg01l
024130160905if  4c                   If        Not %Found(Azorg01l)
024140160905     c******                       (orgfag <> 'A' and orgfag <> 'F')
024150051108     c     xx            Add       40            zz
024160051108     c                   Eval      *In(zz) = *On
024170160906     c                   eval      *in28 = *on
024180160914     c                   eval      V1Cmsg = msg(91)
024190051118     c                   Goto      emi01
024200051108    4c                   EndIf
024210160905     c                   eval      OG143 = ORGde3
024220160905     c                   IF        §OGntw = 'XXX' or
024230160905     c                             (§UTEksclog <> 'S' and
024240160905     c                              §OGntw = 'LOG')
024250160905     c     xx            Add       40            zz
024260160905     c                   Eval      *In(zz) = *On
024270160906     c                   eval      *in28 = *on
024280160914     c                   eval      V1Cmsg = msg(91)
024290160905     c                   Goto      emi01
024300160905     c                   ENDIF
024310051108    3c                   EndIf
024320051108    2c                   EndIf
024330051108    1c                   EndDo
024340051108
024350051108     c                   EndSr
024360051216
024370051216      *------------------------------------------------------------------------*
024380051216      * CARICAMENTO DATI DAI FILE ALLE VIDEATE
024390051216      *------------------------------------------------------------------------*
024400051216     c     Sr_Cardati    BegSr
024410051216
024420051220     c                   Clear                   savabl
024430051220     c                   Clear                   savcpo
024440051220     c                   Clear                   savfft
024450051220     c                   Clear                   savitc
024460051220     c                   Clear                   savscf
024470051220     c                   Clear                   savtft
024480051220     c                   Clear                   savuni
024490080422     c                   clear                   sav4utip
024500100802     c                   clear                   savtipdn
024510100802     c                   clear                   savtipna
024520051216     c                   Clear                   wfscf
024530060105     c                   Eval      wforzacon = *Off
024540060215     c                   Clear                   savin22
024550060215     c                   Clear                   savin23
024560100728     c                   Clear                   savin71
024570100728     c                   Clear                   savin72
024580100728     c                   Clear                   savin73
024590100728     c                   Clear                   savin74
024600091119     c                   clear                   savntcdc
024610061030     c                   eval      wforzacdf = *off
024620061106     c                   eval      wforzaiva = *off
024630061106     c                   eval      *in39 = *off
024640080422     c                   eval      $win = *off
024650100729     c                   eval      $windn = *off
024660100729     c                   eval      $winna = *off
024670100729     c                   clear                   wbanca
024680100729     c                   clear                   wagenzia
024690100729     c                   clear                   savabidn
024700100729     c                   clear                   savabina
024710100729     c                   clear                   savcabdn
024720100729     c                   clear                   savcabna
024730100730     c                   clear                   savibandn
024740100730     c                   clear                   savibanna
024750100803     c                   eval      *in75 = *off
024760100803     c                   eval      *in76 = *off
024770100803     c                   eval      *in77 = *off
024780130322     c                   eval      *in79 = *off
024790130322     c                   eval      *in81 = *off
024800150415     c                   eval      *in82 = *off
024810170317     c                   eval      *in83 = *off
024820170320     c                   eval      *in84 = *off
024830170421     c                   eval      *in85 = *off
024840150424     c                   eval      wForzaPag = *off
024850150728     c                   eval      wForzaMail = *off
024860110217
024870110217      /free
024880110217       //?pulisco campi di comodo per controlli da fare sul commerciale
024890110217       //?quando NON è anagrafica provvisoria
024900110217        clear sav_livello;
024910110217        clear wCodIncAtt;
024920110217      /end-free
024930051216
024940051219      * Se non sono passata dalla prima videata
024950051219      * aggancio l'anagrafica
024960051219if  1c                   If        $d01 = *Off
024970060110      * Spengo indicatori di comodo
024980090914     c                   Setoff                                       0102
024990060110     c                   Setoff                                       04
025000051222     c                   eval      kksc = v1cksc
025010051219      * Se non è richiamato da gestione visite e non è interrogazione
025020051219      * aggancio Cnaco00f per allocare il rcd
025030051219if  2c                   If        Not *In06 and Not *In05
025040060111     c  n05kCnaco        Chain(e)  Cnaco00f
025050051216      * Allocato errore
025060051219if  3c                   If        %error
025070051219      * Torno in prima videata se non è conferma visita
025080051216     c                   If        Not *In07
025090051216     c                   Eval      *In28 = *on
025100051216     c                   Eval      v1cmsg = msg(68)
025110051216     c                   Goto      emi01
025120051216     c                   EndIf
025130051219      * Torno al chiamante se è conferma visita
025140051216     c                   If        *In07
025150051216     c                   Goto      fine
025160051216     c                   EndIf
025170051219    3c                   EndIf
025180051219    2c                   EndIf
025190051219
025200091111      * Se è Interrogazione
025210091111     c                   if        *in05
025220091111      * visita/trattativa
025230091111     c                   if        *in06
025240091111     c                   eval      kksc = v1cnrv
025250091111     c     kCnaco        Chain(n)  Tfaco00f
025260091111      * cliente
025270091111     c                   else
025280051222     c                   eval      kksc = v1cksc
025290091111     c     kCnaco        Chain(n)  Cnaco00f
025300091111     c                   endif
025310091111     c                   endif
025320051216
025330091111      * Richiamato da gestione visite non in interrogazione
025340051219      * aggancio Tfaco00f e alloco il record
025350091111if  2c                   If        *In06 and not *in05
025360051222     c                   eval      kksc = v1cnrv
025370051216     c     kCnaco        Chain(e)  Tfaco00f
025380051216      * Allocato errore
025390051216     c                   If        %error
025400051216     c                   Goto      fine
025410051216     c                   EndIf
025420051219    2c                   EndIf
025430051219
025440051216      * Record trovato
025450051219if  2c                   If        %Found
025460051216      * Cliente esistente - manutenzione/Visualizzazione
025470051219if  3c                   If        acoflg <> '*'
025480051216     c  n05              Eval      *In02 = *On
025490051216     c  n05              Eval      vtcmod = mod(02)
025500051216     c   05              Eval      vtcmod = mod(03)
025510051216      * Cliente annullato
025520051219   x3c                   Else
025530051216     c                   Eval      *In04 = *On
025540051216     c                   Eval      vtcmod = mod(04)
025550051219    3c                   EndIf
025560051219      * Cliente nuovo - immissione
025570051219      * non essendo passata dalla prima videata l'inserimento avviene
025580051219      * solo se sono richiamata da gestione visite/offerte
025590051219      * quindi non controllo se sono in interrogazione
025600051219   x2c                   Else
025610051216     c                   Eval      *In01 = *On
025620051216     c                   Eval      vtcmod = mod(01)
025630051219    2c                   EndIf
025640051219    1c                   EndIf
025650051216
025660051216      * Se non è immissione imposto i dati dei file a video
025670051216if  1c                   If        Not *In01
025680060111     c  n06
025690060111     cann05kCnaco        Chain     Cnind00f
025700060111     c  n06
025710060111     can 05kCnaco        Chain(n)  Cnind00f
025720091111     c   06
025730091111     cann05kCnaco        Chain     Tfind00f
025740091111     c   06
025750091111     can 05kCnaco        Chain(n)  Tfind00f
025760060111     c  n06
025770060111     cann05kCnaco        Chain     Cnclp00f
025780060111     c  n06
025790060111     can 05kCnaco        Chain(n)  Cnclp00f
025800091111     c   06
025810091111     cann05kCnaco        Chain     Tfclp00f
025820091111     c   06
025830091111     can 05kCnaco        Chain(n)  Tfclp00f
025840060111     c  n06
025850060111     cann05v1cksc        Chain     Fncls01l
025860060111     c  n06
025870060111     can 05v1cksc        Chain(n)  Fncls01l
025880091111     c   06
025890091111     cann05v1cnrv        Chain     Tfcls01l
025900091111     c   06
025910091111     can 05v1cnrv        Chain(n)  Tfcls01l
025920051216
025930051216      * Data ultima variazione
025940051216     c                   Z-add     acoduv        dataamg
025950051216     c                   Z-add     aa1           aa2
025960051216     c                   Z-add     mm1           mm2
025970051216     c                   Z-add     gg1           gg2
025980051216     c                   Z-add     datagma       v2cduv
025990051216      * Allineato rcd da sede o da p.o.
026000051216     c                   If        acoftr = 'R'
026010051216     c                   Eval      v2ddtr = 'Ricevuto da Sede'
026020080115     c                   endif
026030080115     c                   if        acoftr = 'T'
026040051216     c                   Eval      v2ddtr = 'Trasmesso a Sede'
026050051216     c                   EndIf
026060051216      * Data allineamento rcd
026070051216     c                   Z-add     acodtr        dataamg
026080051216     c                   Z-add     aa1           aa2
026090051216     c                   Z-add     mm1           mm2
026100051216     c                   Z-add     gg1           gg2
026110051216     c                   Z-add     datagma       v2cdtr
026120051219      * Dati non visualizzati a video
026130051219     c                   Eval      v2copz = acoopz
026140051219     c                   Eval      v2ctic = acotic
026150051219     c                   Eval      v2clin = indlin
026160051216      * Dati anagrafici
026170051216     c                   Eval      v2crag = acorag
026180051216     c                   Eval      v2cvia = indvia
026190051216     c                   Eval      v2ccit = indcit
026200051216     c                   Eval      v2ccae = indcae
026210051216     c                   Eval      v2cprv = indprv
026220051216     c                   Eval      v2csta = indsta
026230051216     c                   Eval      v2ctel = indtel
026240051216     c                   Eval      v2ctlf = indtlf
026250051216     c                   Eval      v2ccdf = indcdf
026260080306     c                   Eval      savcdf = v2ccdf
026270051216     c                   Movel     indiva        w002a
026280051216     c                   If        w002a >= '00'
026290051216     c                   Movel     indiva        v2ivait
026300051216     c                   Else
026310051216     c                   Movel     indiva        v2civa
026320051216     c                   EndIf
026330080306     c                   Eval      saviva = v2civa
026340051216      * Blocco cliente
026350051216     c                   Eval      v2cabl = acoabl
026360130322     c                   eval      V2Cabledp = ACOabl
026370130322      * se blocco del cliente è '*' (blocco contabile) oppure
026380130322      * '7' (blocco automatico)
026390130322      * proteggo il campo del blocco + causale blocco
026400130322      * sempre, filiale e sede
026410130322     c                   IF        ACOabl = '*' or ACOabl = '7'
026420130322     c                   eval      *in79 = *on
026430130322     c                   ENDIF
026440130322     c                   IF        ACOabl = '7'
026450130322     c                   eval      V2Cabl = '8'
026460130322     c                   ENDIF
026470051220     c                   Eval      savabl = acoabl
026480051216      * Causale blocco cliente
026490051216     c                   Eval      v2cblc = clpnar
026500051216      * Stato del credito
026510051216     c                   Move      clpcon        v2ccon
026520110407      * Se il pgm è stato richiamato per il blocco cliente
026530110407      * imposto il blocco e la causale
026540110407     c                   IF        $Blocco
026550110407     c                   eval      v2cabl = '8'
026560110407     c                   eval      v2cblc = Parmcbl
026570110407     c                   ENDIF
026580051216      * Cliente annullabile
026590051216     c                   Eval      v2tb02 = atb02
026600051216      * Codice Potenziale
026610051216     c                   Eval      v2ccpo = acolib
026620051216     c                   Eval      savcpo = acolib
026630090616      * Cliente da sollecitare
026640090616     c                   Eval      v2csol = clpsol
026650090616      * sollecito in inglese
026660090616     c                   Eval      v2fsol = %subst(clsflo:4:1)
026670051216      * Codice commerciale
026680051216     c                   Movel     clpage        v3cage
026690051216      * Codice importanza cliente
026700051216     c                   Eval      v3cclv = clpclv
026710080313      * categoria merceologica
026720051216     c                   Move      acoitc        v3citc
026730051216      * Codice sottoconto fatturazione
026740100127     c                   Eval      v4cscf = %editc(clpscf:'X')
026750100329      * se sono da convalida offerta e non c'è il codice fatturazione
026760100329      * e la partita IVA è $$ imposto come codice fatturazione il codice
026770100329      * cliente che sto generando
026780100329     c                   IF        (v4cscf = *zeros or v4cscf = *blanks) and
026790100329     c                              *in07 and v2ivaeu = '$$'
026800100329     c                   eval      v4cscf = %editc(v1cksc:'X')
026810100329     c                   ENDIF
026820100127     c                   Eval      savscf = v4cscf
026830051216      * Fattura unificata
026840051216     c                   Eval      v4cuni = %subst(clsflo:2:1)
026850051216     c                   Eval      savuni = v4cuni
026860051216      * Tipo fattura
026870051216     c                   Move      clptft        v4ctft
026880051216     c                   Move      clptft        savtft
026890051216      * Frequenza fattura
026900051216     c                   Move      clpfft        v4cfft
026910051216     c                   Move      clpfft        savfft
026920051216      * Tipo data fattura (Inizio mese/Fine mese)
026930051216     c                   Eval      v4ctdf = clpfun
026940051216      * Spese invio fattura (nel file il campo non ha decimali solo a video)
026950051216     c                   Move      indsin        v4csin
026960051216      * Stampa mittente originale in fattura
026970051216     c                   Eval      v4cstm = %subst(clsflo:6:1)
026980051216      * Data prima spedizione fattura (solo visualizzazione viene impostato
026990051216      *                                dalla fatturazione)
027000051216     c                   Z-add     clpdps        dataamg
027010051216     c                   Z-add     aa1           aa2
027020051216     c                   Z-add     mm1           mm2
027030051216     c                   Z-add     gg1           gg2
027040051216     c                   Z-add     datagma       v4cdps
027050051216      * Data ultima spedizione fattura (solo visualizzazione viene impostato
027060051216      *                                 dalla fatturazione)
027070051216     c                   Z-add     clpdus        dataamg
027080051216     c                   Z-add     aa1           aa2
027090051216     c                   Z-add     mm1           mm2
027100051216     c                   Z-add     gg1           gg2
027110051216     c                   Z-add     datagma       v4cdus
027120051216      * Numero protocollo interno (solo visualizzazione viene impostato da Proj)
027130051216     c                   Eval      v4cnpn = indnpn
027140051216      * Numero protocollo cliente (solo visualizzazione viene impostato da Proj)
027150051216     c                   Eval      v4cnpr = indnpr
027160051216      * Data protocollo cliente   (solo visualizzazione viene impostato da Proj)
027170051216     c                   Z-add     inddpr        dataamg
027180051216     c                   Z-add     aa1           aa2
027190051216     c                   Z-add     mm1           mm2
027200051216     c                   Z-add     gg1           gg2
027210051216     c                   Z-add     datagma       v4cdpr
027220051216      * Codice pagamento
027230100727     c                   Move      indcdp        v4ccdp
027240051216      * Abi
027250100727     c                   Eval      v4cabi = indabi
027260051216      * Cab
027270100727     c                   Eval      v4ccab = indcab
027280121105      * Tassazione Post Fatturazopme
027290121105     c                   eval      V4Ctpf = %subst(CLSflo:3:1)
027300090612      * Blocco pagamenti
027310051216     c                   Eval      wbloc = %subst(indopz:1:1)
027320130322     c                   eval      V5Cblpedp = %subst(indopz:1:1)
027330130322      * se blocco pagamenti NON è 'B' (blocco manuale)
027340130322      * proteggo il campo del blocco pagamenti
027350130322      * sempre, filiale e sede
027360130322     c                   IF        V5Cblpedp <> 'B' and
027370130322     c                             V5Cblpedp <> '0' and
027380130322     c                             V5Cblpedp <> *blanks
027390130322     c                   eval      *in81 = *on
027400130322     c                   ENDIF
027410130322     c                   If        wbloc = '0'
027420130322     c                   Eval      v5cblp = 'NO'
027430130322     c                   Else
027440130322     c                   Eval      v5cblp = 'SI'
027450130322     c                   EndIf
027460100728
027470051216      * Codice pagamento contrassegni
027480051216     c                   Eval      v5cfpc = clpfpc
027490080115      * Codice IBAN
027500080115     c                   eval      v5ciban = clpbab
027510100803      * il BIC per i contrassegni per ora non ce l'ho mai quindi non lo faccio
027520100803      * mai vedere
027530100803     c                   eval      *in75 = *on
027540100728
027550100728      /free
027560100728       //?Nuovi tipi pagamento
027570100729       //?Pagamento DANNI --> DN
027580100728         v5tpdn = %subst(clstic:1:1);
027590100728       //?Recupero le coordinate bancarie
027600100728         wpag = 'DN';
027610100728         rqsOpCode = 'SEARCH';
027620100728         exsr Coordinate;
027630100728         IF  rpyEsito = 0;
027640100728           IF not *in06;
027650100730             v5ibandn = TrulIbanO0.IBAN;
027660100730             savabidn = TrulIbanO0.ABI;
027670100730             savcabdn = TrulIbanO0.CAB;
027680100730             v5bicdn  = TrulIbanO0.BIC;
027690100728           ELSE;
027700100730             v5ibandn = TrulIbanO1.IBAN;
027710100730             savabidn = TrulIbanO1.ABI;
027720100730             savcabdn = TrulIbanO1.CAB;
027730100730             v5bicdn  = TrulIbanO1.BIC;
027740100728           ENDIF;
027750100728         ENDIF;
027760100803         IF  v5bicdn = *blanks;
027770100803           *in76 = *on;
027780100803         ENDIF;
027790100729       //?Pagamento NOTE ACCREDITO --> NA
027800100728         v5tpna = %subst(clstic:2:1);
027810100728       //?Recupero le coordinate bancarie
027820100728         wpag = 'NA';
027830100728         exsr Coordinate;
027840100728         IF  rpyEsito = 0;
027850100728           IF not *in06;
027860100730             v5ibanna = TrulIbanO0.IBAN;
027870100730             savabina = TrulIbanO0.ABI;
027880100730             savcabna = TrulIbanO0.CAB;
027890100730             v5bicna  = TrulIbanO0.BIC;
027900100728           ELSE;
027910100730             v5ibanna = TrulIbanO1.IBAN;
027920100730             savabina = TrulIbanO1.ABI;
027930100730             savcabna = TrulIbanO1.CAB;
027940100730             v5bicna  = TrulIbanO1.BIC;
027950100728           ENDIF;
027960100728         ENDIF;
027970100803         IF  v5bicna = *blanks;
027980100803           *in77 = *on;
027990100803         ENDIF;
028000100730         savibandn = v5ibandn;
028010100730         savibanna = v5ibanna;
028020100728      /end-free
028030100728
028040051216      * Tipo comunicazione al mittente
028050051216     c                   Eval      v6ctcm = %subst(clsflo:10:1)
028060051216      * Tipo comunicazione fine giacenza
028070051216     c                   Eval      v6ctcf = %subst(clsflo:1:1)
028080051216      * Apertura automatica giacenza
028090051216     c                   Eval      v6capg = %subst(clsflo:9:1)
028100051216      * Disposizione mittente in arrivo
028110051216     c                   If        acorx1 = *Zeros
028120051216     c                   Eval      v6cdmg = 'SI'
028130051216     c                   Else
028140051216     c                   Eval      v6cdmg = 'NO'
028150051216     c                   EndIf
028160060801      * Chiusura automatica giacenza
028170060801     c                   If        acory1 = *Zeros
028180060801     c                   Eval      v6ccag = 'SI'
028190060801     c                   Else
028200060801     c                   Eval      v6ccag = 'NO'
028210060801     c                   EndIf
028220051216      * Rimborso danni
028230051216      * Stampa copia denuncia al cliente
028240051216     c                   Eval      v7csdn = %subst(clsflo:5:1)
028250051216      * Comunicazioni in inglese
028260051216     c                   Eval      v7csin = %subst(clsflo:7:1)
028270051216      * Escludi da tassazione diretta porti assegnati
028280051216     c                   Eval      v7ctpa = %subst(clsflo:8:1)
028290051216      * Nr. stop autotrasportatori
028300051216     c                   Eval      v7cstp = clsstp
028310051216
028320051216      * Se immissione imposto dei campi di default
028330051216   x1c                   Else
028340051219      * Campi non visualizzati
028350051219     c                   Eval      v2copz = wopz
028360051219     c                   Eval      v2ctic = wtic
028370051216     c                   Eval      v2clin = '1'
028380051219      * Campi visualizzati
028390051216     c                   Clear                   v2ccpo
028400051216      * Se provengo da visite/offerte
028410051216if  2c                   If        *In06
028420051216      * Carico i dati del potenziale
028430051220     c     ta60cpo       Chain(n)  Tncpo01l
028440051216     c                   If        %Found(Tncpo01l)
028450051216     c                   ExSr      Sr_Carcpo
028460051216     c                   EndIf
028470051216    2c                   EndIf
028480090612      * Cliente da sollecitare
028490090616      * imposto il dft
028500090616     c                   ExSr      Sr_Ctrsol
028510051216      * Commerciale
028520051216     c                   Clear                   v3cage
028530051216      * Imposto il codice commerciale della visita
028540051216     c   06              Movel     ta60cmm       v3cage
028550080313      * categoria merceologica
028560051216     c                   Clear                   v3citc
028570080313      * Se impostata dal potenziale riporto la categoria merceologica
028580051216     c                   If        savitc <> *Blanks
028590051216     c                   Eval      v3citc = savitc
028600051216     c                   EndIf
028610051219      * Codice importanza cliente
028620100301      * lo imposto se non è immissione anagrafica provvisoria da trattativa
028630100806     c  n06              Eval      v3cclv = 'C'
028640051216      * Codice sottoconto fatturazione
028650100729      * imposto a 0 così l'utente è obbligato ad inserirlo
028660100303     c                   eval      v4cscf = '0000000'
028670051216      * Tipo fattura
028680051216     c                   Eval      v4ctft = '0'
028690051216     c                   Eval      savtft = '0'
028700051216      * Frequenza fattura
028710051216     c                   Eval      v4cfft = '4'
028720051216     c                   Eval      savfft = '4'
028730091109      * Tipo data fattura (Inizio mese/Fine mese)
028740091125      * se da trattativa
028750100806     c   06              eval      v4ctdf = 'F'
028760051216      * Spese invio fatturazione
028770051216     c                   Z-add     §gedrf        v4csin
028780060210      * se non è anagrafica provvisoria e cliente 8888 o 9999 non devo
028790060210      * imputare le spese di invio fattura
028800060210     c                   Move      v1cksc        w0040
028810060210     c                   If        Not *In06 and
028820060210     c                             (w0040 = 8888 or w0040 = 9999)
028830060210     c                   Clear                   v4csin
028840060210     c                   EndIf
028850051216      * Protocollo
028860051216     c                   Clear                   v4cnpn
028870051216     c                   Clear                   v4cnpr
028880051216     c                   Clear                   v4cdpr
028890051221      * Codice Pagamento
028900100727     c                   Move      *Zeros        v4ccdp
028910130322      * Blocco pagamento campo di work
028920130322     c                   eval      wbloc = '0'
028930080115      * Tipo Pagamento contrassegni
028940091125      * se NON è da trattativa
028950100806     c  n06              eval      v5cfpc = '2'
028960100805
028970100805      /free
028980100805       //?Tipo pagamento Danni
028990150424         v5tpdn = dfttipdn;
029000100805       //?Tipo pagamento Note Accredito
029010150424         v5tpna = dfttipna;
029020100805      /end-free
029030100728
029040051216      * Tipo comunicazione al mittente
029050120118      * lo imposto se non è immissione anagrafica provvisoria da trattativa
029060120118     c  n06              Eval      v6ctcm = §2gtcm
029070051216      * Tipo comunicazione fine giacenza
029080051216     c                   Eval      v6ctcf = §2gtfg
029090051216      * Apertura automatica giacenza
029100051216     c                   Clear                   v6capg
029110051216      * Disposizione mittente in arrivo
029120051216     c                   Eval      v6cdmg = 'SI'
029130060801      * Chiusura automatica giacenza
029140060801     c                   Eval      v6ccag = 'SI'
029150051216
029160051216    1c                   EndIf
029170110217
029180110217      /free
029190110217       //?Se non è anagrafica provvisoria
029200110217         IF  not *in06;
029210110217       //?Aggancio l'organigramma x controllo se filiale cliente
029220110217       //?è un primo o secondo livello
029230110217           clear og148;
029240110217           w0030 = %dec(%subst(%editc(v1cksc:'X'):1:3):3:0);
029250110217           chain  w0030  azorg01l;
029260110217           IF  %found(azorg01l);
029270110217             og148 = ORGde8;
029280110217           ENDIF;
029290110217           sav_Livello = §OGlpo;
029300110217       //?Recupero da tabella Y4O il codice cliente 'incassi da attribuire'
029310110217           clear TIBS02DS;
029320110217           clear dY4O;
029330110217           T02mod = 'C';
029340110217           T02cod = 'Y4O';
029350110217           T02ke1 = '201';
029360110217           T02ke2 = %editc(w0030:'X');
029370110217           TNTBE_RicercaControllo (kpjba : tibs02ds);
029380110217           IF  T02err = *blanks;
029390110217             dY4O = T02uni;
029400110217           ENDIF;
029410110217           wCodIncAtt = %subst(§4Osc1:2:7);
029420110217         ENDIF;
029430170421
029440170421       //?Se sono in manutenzione
029450170421       //?Se non sono utente EDP e il cliente che sto manutenzionando
029460170421       //?un cliente IMPORT DPD proteggo i flag relativi alla
029470170421       //?Gestione Giacenze
029480170421         IF  *in02 and not *in20 and %lookup(V1Cksc:CliImport) > 0;
029490170421           *in85 = *on;
029500170421         ENDIF;
029510170421
029520110217      /end-free
029530051216
029540051216      * Decodifico
029550051216     c                   ExSr      Sr_Decdati
029560060531     c
029570060531     c* Memorizzo l'immagine precedente per  verificare se dati variati
029580150427      /free
029590150427         exsr ImmaginePrima;
029600150427      /end-free
029610051216
029620051216     c                   EndSr
029630100728
029640100728      /free
029650100728       //--------------------------------------------------------------
029660100728       //?Operazioni relative alle coordinate bancarie.
029670100728       //--------------------------------------------------------------
029680100728       BEGSR Coordinate;
029690100802
029700100802         clear rpyEsito;
029710100728
029720100728       //?FNCBA00F
029730100728         IF  not *in06;
029740100730           clear TrulIbanI0;
029750100730           clear TrulIbanO0;
029760100728           TrulIbanI0.KSC = v1cksc;
029770100728           TrulIbanI0.PAG = wpag;
029780100729         //?Se non è ricerca passo i dati del video
029790100729           IF  rqsOpCode <> 'SEARCH';
029800100805             TrulIbanI0.IBAN  = wiban;
029810100805             TrulIbanI0.BIC   = wbic;
029820100805             TrulIbanI0.CODPO = wcodpo;
029830100805             TrulIbanI0.CODPN = wcodpn;
029840100805             TrulIbanI0.PGM   = wpgm;
029850100729           ENDIF;
029860100728           rqsFormatName = 'TRULIBANI0';
029870100728           rpyFormatName = 'TRULIBANO0';
029880100728           rqsDataSize   = %size(TrulIbanI0);
029890100728           rpyDataSize   = %size(TrulIbanO0);
029900100728           TrulIbanR (rqsOpCode:rpyEsito:
029910100729                      rqsFormatName:TrulIbanI0:rqsDataSize:
029920100729                      rpyFormatname:TrulIbanO0:rpyDataSize);
029930100728       //?TFCBA00F
029940100728         ELSE;
029950100730           clear TrulIbanI1;
029960100730           clear TrulIbanO1;
029970100729           TrulIbanI1.NRV = v1cnrv;
029980100728           TrulIbanI1.PAG = wpag;
029990100729         //?Se non è ricerca passo i dati del video
030000100729           IF  rqsOpCode <> 'SEARCH';
030010100730             TrulIbanI1.IBAN = wiban;
030020100730             TrulIbanI1.BIC  = wbic;
030030100729           ENDIF;
030040100728           rqsFormatName = 'TRULIBANI1';
030050100728           rpyFormatName = 'TRULIBANO1';
030060100728           rqsDataSize   = %size(TrulIbanI1);
030070100728           rpyDataSize   = %size(TrulIbanO1);
030080100728           TrulIbanR (rqsOpCode:rpyEsito:
030090100728                      rqsFormatName:TrulIbanI1:rqsDataSize:
030100100729                      rpyFormatname:TrulIbanO1:rpyDataSize);
030110100728         ENDIF;
030120100728
030130100728       ENDSR;
030140100728      /end-free
030150051216
030160051216      *------------------------------------------------------------------------*
030170051216      * DECODIFICO I DATI DELLE VIDEATE
030180051216      *------------------------------------------------------------------------*
030190051216     c     Sr_Decdati    BegSr
030200051216
030210060220     c                   Eval      *In19 = *Off
030220051216     c                   Eval      *In22 = *Off
030230051216     c                   Eval      *In23 = *Off
030240100728     c                   Eval      *In71 = *Off
030250100728     c                   Eval      *In72 = *Off
030260100728     c                   Eval      *In73 = *Off
030270100728     c                   Eval      *In74 = *Off
030280150415     c                   Eval      *In82 = *Off
030290051216
030300051216      * Decodifico lo stato del credito
030310051216     c                   Clear                   v2dcon
030320051216     c                   Clear                   ds4w
030330051216     c                   Eval      kcod = '4W'
030340051216     c                   Move(p)   v2ccon        w003a
030350051216     c                   Eval      kkey = w003a
030360051216     c     kTabel        Chain     Tabel00f
030370051216     c                   If        %Found(Tabel00f) and
030380051216     c                             tblflg = *Blanks
030390051216     c                   Eval      ds4w = tbluni
030400051216     c                   EndIf
030410051216     c                   Eval      v2dcon = §4wdes
030420051216
030430051216      * Se sono in filiale proteggo lo stato del credito se non è modificabile
030440120928      * da p.o.
030450130325      * da 25/03/2013 abbiamo deciso che anche in sede non è modificale, solo
030460130325      * da ProJ
030470130325     c                   If        §4wmbl = 'N'
030480060220     c                   Eval      *In19 = *On
030490051216     c                   EndIf
030500130322
030510120928      * Se sono in filiale proteggo il blocco pagamenti se non è modificabile
030520120928      * da p.o.
030530120928     c                   If        Not *In09 and §4wmbp = 'N'
030540120928     c                   Eval      *In81 = *On
030550120928     c                   EndIf
030560120925      * Se sono in filiale proteggo il blocco cliente se non è modificabile
030570120925      * da p.o.
030580120928     c                   If        Not *In09 and §4wmtb = 'N'
030590120925     c                   Eval      *In79 = *On
030600120925     c                   EndIf
030610170317      /free
030620170330       //?Se sono un utente NON abilitato alla variazione
030630170317       //?e il blocco clienti è protetto
030640170317       //?e il cliente è in contenzioso
030650170330       //?devo bloccare anche la frequenza fattura
030660170330         IF  §UTEclpfft <> 'S' and *in79 and
030670170330             §4Wtip <> *blanks;
030680170317           *in83 = *on;
030690170317         ENDIF;
030700170317      /end-free
030710051216
030720051216      * Decodifico la causale blocco cliente
030730110323     c                   clear                   dsbi
030740051216     c                   Clear                   v2dblc
030750051216     c                   Eval      kcod = 'BI'
030760051216     c                   Eval      kkey = v2cblc
030770051216     c     kTabel        Chain     Tabel00f
030780051216     c                   If        %Found(Tabel00f) and
030790051216     c                             tblflg = *Blanks
030800110323     c                   Eval      dsbi = tbluni
030810051216     c                   EndIf
030820110323     c                   Eval      v2dblc = §bides
030830051216
030840051216      * Decodifico il codice potenziale
030850051216     c                   Clear                   v2dcpo
030860110223     c                   clear                   v2hfls
030870051216     c                   If        v2ccpo <> *Zeros
030880051220     c     v2ccpo        Chain(n)  Tncpo01l
030890051216     c                   If        %Found(Tncpo01l)
030900051216     c                   Eval      v2dcpo = cporag
030910110223     c                   eval      v2hfls = CPOfls
030920121105     c                   eval      dCPO01 = CPOrst
030930051216     c                   EndIf
030940051216     c                   EndIf
030950051216
030960051216      * Decodifico il codice commerciale
030970051216     c                   Clear                   v3dage
030980130806     c                   If        %check(digits:V3Cage) = 0
030990130806     c                   move      V3Cage        CMMcod
031000130806     c     CMMcod        chain     AZCMM000
031010130806     c                   if        %Found(AZCMM01L)
031020130806     c                   movel     CMMdes        v3dage
031030130806     c                   endif
031040130806     c                   EndIf
031050051216
031060051216      * Decodifico il codice importanza cliente
031070051216     c                   Clear                   v3dclv
031080051216     c                   Clear                   dsic
031090051216     c                   Eval      kcod = 'IC'
031100051216     c                   Eval      kkey = v3cclv
031110051216     c     kTabel        Chain     Tabel00f
031120051216     c                   If        %Found(Tabel00f) and
031130051216     c                             tblflg = *Blanks
031140051216     c                   Eval      dsic = tbluni
031150051216     c                   EndIf
031160051216     c                   Eval      v3dclv = §sicde
031170051216
031180080313      * Decodifico categoria merceologica
031190051216     c                   Clear                   v3ditc
031200051216     c                   Clear                   ds1l
031210051216     c                   Eval      kcod = '1L'
031220051216     c                   Movel     v3citc        kkey
031230051216     c     kTabel        Chain     Tabel00f
031240051216     c                   If        %Found(Tabel00f) and
031250051216     c                             tblflg = *Blanks
031260051216     c                   Eval      ds1l = tbluni
031270051216     c                   EndIf
031280051216     c                   Eval      v3ditc = §1ldes
031290051216
031300051216      * Decodifico sottoconto fatturazione
031310051216     c                   Clear                   Tibs69ds
031320051216     c                   Move      v4cscf        I69kac
031330051216     c                   Call      'TIBS69R'
031340051216     c                   Parm                    Tibs69ds
031350051216     c                   Parm                    ds_cnaco
031360051216     c                   Parm                    ds_cnind
031370051216     c                   Parm                    ds_cnclp
031380051216     c                   Parm                    ds_fncls
031390051216     c                   Eval      wtibs69 = *On
031400051216     c                   Movel     w_acorag      v4dscf
031410051216
031420051216      * Decodifico il tipo fattura
031430051216     c                   Clear                   Tibs02ds
031440051216     c                   Eval      t02mod = 'C'
031450051216     c                   Eval      t02sif = knsif
031460051216     c                   Eval      t02cod = 'TFT'
031470051216     c                   Eval      t02ke1 = v4ctft
031480051216     c                   Call      'TIBS02R'
031490051216     c                   Parm                    kpjba
031500051216     c                   Parm                    Tibs02ds
031510051216     c                   If        t02err = *Blanks
031520051216     c                   Eval      dtft = t02uni
031530051216     c                   EndIf
031540051216     c                   Eval      v4dtft = §tftdes
031550051216
031560051216      * Decodifico la frequenza fattura
031570051216     c                   Clear                   v4dfft
031580051216     c                   Clear                   dsff
031590051216     c                   Eval      kcod = 'FF'
031600051216     c                   Eval      kkey = v4cfft
031610051216     c     kTabel        Chain     Tabel00f
031620051216     c                   If        %Found(Tabel00f) and
031630051216     c                             tblflg = *Blanks
031640051216     c                   Eval      dsff = tbluni
031650051216     c                   EndIf
031660051216     c                   Eval      v4dfft = §ffdes
031670051216
031680051216      * Decodifico il tipo data fattura
031690051216     c                   If        v4ctdf <> *Blanks
031700051216     c                   Clear                   v4dtdf
031710051216     c                   Eval      kcod = '13'
031720051216     c                   Eval      kkey = v4ctdf
031730051216     c     kTabel        Chain     Tabel00f
031740051216     c                   If        %Found(Tabel00f) and
031750051216     c                             tblflg = *Blanks
031760051216     c                   Eval      v4dtdf = tbluni
031770051216     c                   EndIf
031780051216     c                   EndIf
031790051216
031800051216      * Decodifico codice di pagamento
031810100727     c                   Clear                   v4dcdp
031820051216     c                   Clear                   dsfa
031830051216     c                   Eval      kcod = 'FA'
031840100727     c                   Movel     v4ccdp        w004a
031850051216     c                   Move      '1'           w004a
031860051216     c                   Eval      kkey = w004a
031870051216     c     kTabel        Chain     Tabel00f
031880051216     c                   If        %Found(Tabel00f) and
031890051216     c                             tblflg = *Blanks
031900051216     c                   Eval      dsfa = tbluni
031910051216     c                   EndIf
031920100727     c                   Eval      v4dcdp = §fades
031930150415
031940150415      /free
031950150415       //?Se utente di filiale e tipo pagamento utilizzabile solo da sede
031960150415       //?devo proteggere la Banca ABI/CAB del pagamento fatture
031970150415         IF  not *in09 and §FAuti = 'S';
031980150415           *in82 = *on;
031990150415         ENDIF;
032000170320       //?Se sono un utente di filiale
032010170320       //?e il blocco clienti è protetto
032020170320       //?e il cliente è in contenzioso
032030170330       //?devo bloccare il codice pagamento
032040170330         IF  not *in09 and *in79 and §4Wtip <> *blanks;
032050170320           *in84 = *on;
032060170320         ENDIF;
032070150415      /end-free
032080080206
032090080206      * decodifico le coordinate bancarie abi-cab
032100100729      * riscossione fattura
032110100729     c                   clear                   v4cbna
032120100727     c                   eval      kabi = v4cabi
032130100727     c                   eval      kcab = v4ccab
032140100729     c                   exsr      DesBanca
032150100729     c                   eval      v4cbna = wdesbanca
032160051216
032170051216      * Decodifico codice pagamanto contrassegni
032180051216     c                   Clear                   v5dfpc
032190051216     c                   Clear                   ds4u
032200051216     c                   Eval      kcod = '4U'
032210051216     c                   Eval      kkey = v5cfpc
032220051216     c     kTabel        Chain     Tabel00f
032230051216     c                   If        %Found(Tabel00f) and
032240051216     c                             tblflg = *Blanks
032250051216     c                   Eval      ds4u = tbluni
032260051216     c                   EndIf
032270051216     c                   Eval      v5dfpc = §4udes
032280100729      * tipo pagamento contrassegni, salvo il flag di tipo pagamento i/e
032290100729     c                   eval      sav4utip = §4utip
032300051216      * Le coordinate bancare le visualizzo o le proteggo in base al
032310051216      * tipo pagamento
032320051216     c                   Select
032330051216      * Coordinate italiane, visualizzo
032340051216      * Immissione coordinate non richiesta, non visualizzo
032350051216     c                   When      §4uabi = 'N'
032360051216     c                   Eval      *In22 = *On
032370060215     c                   Eval      savin22 = *In22
032380051216      * Se pagamento estero proteggo le coordinate bancarie perchè
032390051216      * gestite dalla sede.
032400080115     c                   when      §4utip = 'E'
032410051216     c                   Eval      *In23 = *On
032420060215     c                   Eval      savin23 = *In23
032430051216     c                   EndSl
032440080115      * se le visualizzo ed è un tipo pagamento italia decodifico la banca
032450080115     c                   if        not *in22 and not *in23
032460080206     c                             and v5ciban <> *blanks
032470080213     c                             and (%subst(v5ciban:1:2) = 'IT'
032480080213     c                              or %subst(v5ciban:1:2) = 'SM')
032490080116     c                   clear                   v5cbab
032500080213     c                   monitor
032510080115     c                   eval      kabi = %dec(%subst(v5ciban:6:5):5:0)
032520080115     c                   eval      kcab = %dec(%subst(v5ciban:11:5):5:0)
032530080213     c                   on-error
032540080213     c                   clear                   kabi
032550080213     c                   clear                   kcab
032560080213     c                   endmon
032570100729     c                   exsr      DesBanca
032580100729     c                   eval      v5cbab = wdesbanca
032590080115     c                   endif
032600090401      * se le visualizzo ed è un pagemento estero senza IBAN con
032610090401      * abi e cab no a 99999 decodifico la banca
032620090401     c                   if        not *in22 and *in23
032630090401     c                             and v5ciban = *blanks
032640090401     c                             and clpabi <> *all'9'
032650090401     c                             and clpcab <> *all'9'
032660090401     c                   monitor
032670090401     c                   eval      kabi = clpabi
032680090401     c                   eval      kcab = clpcab
032690090401     c                   on-error
032700090401     c                   clear                   kabi
032710090401     c                   clear                   kcab
032720090401     c                   endmon
032730100729     c                   exsr      DesBanca
032740100729     c                   eval      v5cbab = wdesbanca
032750090401     c                   endif
032760100728
032770100728      /free
032780100728       //?Pagamenti DANNI --> DN
032790100728       //?tipo pagamento
032800100728         clear v5dpdn;
032810100728         clear ds4u;
032820100728         kcod = '4U';
032830100730         kkey = v5tpdn;
032840100728         chain (codut:kcod:kkey) TABEL00F;
032850100728         IF  %found(TABEL00F) and TBLflg = *blanks;
032860100728           ds4u = TBLuni;
032870100728         ENDIF;
032880100730         v5dpdn = §4Udes;
032890100802       //?salvo il tipo pagamento
032900100802         savtipdn = v5tpdn;
032910100728       //?coordinate bancarie
032920100728         SELECT;
032930100728         //?se abi cab non obbligatori non visualizzo
032940100730           WHEN  §4Uabi = 'N';
032950100728             *in71 = *on;
032960100728             savin71 = *in71;
032970100805             *in76 = *off;
032980100728         //?se pagamento estero proteggo perchè solo la sede le gestisce
032990100730           WHEN  §4Utip = 'E';
033000100728             *in72 = *on;
033010100728             savin72 = *in72;
033020100728         ENDSL;
033030100729       //?se IBAN IT o SM (IBAN italia) visualizzo la descrizione banca
033040100729       //?recuperandola con ABI e CAB dell'IBAN
033050100729         IF  not *in71 and not *in72 and v5ibandn <> *blanks and
033060100729            (%subst(v5ibandn:1:2) = 'IT' or %subst(v5ibandn:1:2) = 'SM');
033070100729           clear v5babdn;
033080100729           monitor;
033090100729           kabi = %dec(%subst(v5ibandn:6:5):5:0);
033100100729           kcab = %dec(%subst(v5ibandn:11:5):5:0);
033110100729           on-error;
033120100729           clear kabi;
033130100729           clear kcab;
033140100729           endmon;
033150100729           exsr DesBanca;
033160100729           v5babdn = wDesBanca;
033170100729         ENDIF;
033180100729       //?se IBAN estero (campo iban vuoto e ABI e CAB impostati <> 99999)
033190100729       //?visualizzo la descrizione della banca recuperandola con ABI e CAB
033200100729       //?memorizzati sul file
033210100729         IF  not *in71 and not *in72 and v5ibandn = *blanks and
033220100729             savabidn <> *all'9' and savcabdn <> *all'9';
033230100729           clear v5babdn;
033240100729           monitor;
033250100729           kabi = savabidn;
033260100729           kcab = savcabdn;
033270100729           on-error;
033280100729           clear kabi;
033290100729           clear kcab;
033300100729           endmon;
033310100729           exsr DesBanca;
033320100729           v5babdn = wDesBanca;
033330100729         ENDIF;
033340100729
033350100729       //?Pagamenti NOTE ACCREDITO --> NA
033360100729       //?tipo pagamento
033370100729         clear v5dpna;
033380100729         clear ds4u;
033390100729         kcod = '4U';
033400100730         kkey = v5tpna;
033410100729         chain (codut:kcod:kkey) TABEL00F;
033420100729         IF  %found(TABEL00F) and TBLflg = *blanks;
033430100729           ds4u = TBLuni;
033440100729         ENDIF;
033450100730         v5dpna = §4Udes;
033460100802       //?salvo il tipo pagamento
033470100802         savtipna = v5tpna;
033480100729       //?coordinate bancarie
033490100729         SELECT;
033500100729         //?se abi cab non obbligatori non visualizzo
033510100730           WHEN  §4Uabi = 'N';
033520100729             *in73 = *on;
033530100729             savin73 = *in73;
033540100805             *in77 = *off;
033550100729         //?se pagamento estero proteggo perchè solo la sede le gestisce
033560100730           WHEN  §4Utip = 'E';
033570100729             *in74 = *on;
033580100729             savin74 = *in74;
033590100729         ENDSL;
033600100729       //?se IBAN IT o SM (IBAN italia) visualizzo la descrizione banca
033610100729       //?recuperandola con ABI e CAB dell'IBAN
033620100729         IF  not *in73 and not *in74 and v5ibanna <> *blanks and
033630100729            (%subst(v5ibanna:1:2) = 'IT' or %subst(v5ibanna:1:2) = 'SM');
033640100729           clear v5babna;
033650100729           monitor;
033660100729           kabi = %dec(%subst(v5ibanna:6:5):5:0);
033670100729           kcab = %dec(%subst(v5ibanna:11:5):5:0);
033680100729           on-error;
033690100729           clear kabi;
033700100729           clear kcab;
033710100729           endmon;
033720100729           exsr DesBanca;
033730100729           v5babna = wDesbanca;
033740100729         ENDIF;
033750100729       //?se IBAN estero (campo iban vuoto e ABI e CAB impostati <> 99999)
033760100729       //?visualizzo la descrizione della banca recuperandola con ABI e CAB
033770100729       //?memorizzati sul file
033780100729         IF  not *in73 and not *in74 and v5ibanna = *blanks and
033790100729             savabina <> *all'9' and savcabna <> *all'9';
033800100729           clear v5babna;
033810100729           monitor;
033820100729           kabi = savabina;
033830100729           kcab = savcabna;
033840100729           on-error;
033850100729           clear kabi;
033860100729           clear kcab;
033870100729           endmon;
033880100729           exsr DesBanca;
033890100729           v5babna = wDesbanca;
033900100729         ENDIF;
033910100729
033920100728      /end-free
033930051216
033940051216      * Decodifico tipo comunicazioni al mittente
033950051216     c                   Clear                   v6dtcm
033960051216     c                   Clear                   ds2f
033970051216     c                   Eval      kcod = '2F'
033980051216     c                   Eval      kkey = v6ctcm
033990051216     c     kTabel        Chain     Tabel00f
034000051216     c                   If        %Found(Tabel00f) and
034010051216     c                             tblflg = *Blanks
034020051216     c                   Eval      ds2f = tbluni
034030051216     c                   EndIf
034040051216     c                   Eval      v6dtcm = §2fdes
034050051216
034060051216      * Decodifico tipo comunicazioni fine giacenza
034070051216     c                   Clear                   v6dtcf
034080051216     c                   Clear                   ds2h
034090051216     c                   Eval      kcod = '2H'
034100051216     c                   Eval      kkey = v6ctcf
034110051216     c     kTabel        Chain     Tabel00f
034120051216     c                   If        %Found(Tabel00f) and
034130051216     c                             tblflg = *Blanks
034140051216     c                   Eval      ds2h = tbluni
034150051216     c                   EndIf
034160051216     c                   Eval      v6dtcf = §2hdes
034170051216
034180051216      * Decodifico apertura automatica giacenza
034190051216     c                   Clear                   v6dapg
034200051216     c                   Clear                   ds2u
034210051216     c                   Eval      kcod = '2U'
034220051216     c                   Eval      kkey = v6capg
034230051216     c     kTabel        Chain     Tabel00f
034240051216     c                   If        %Found(Tabel00f) and
034250051216     c                             tblflg = *Blanks
034260051216     c                   Eval      ds2u = tbluni
034270051216     c                   EndIf
034280051216     c                   Eval      v6dapg = §2udes
034290051216
034300051216     c                   EndSr
034310051216
034320051216      *------------------------------------------------------------------------*
034330051216      * CARICO I LUOGHI PREVISTI
034340051216      *------------------------------------------------------------------------*
034350051216     c     Sr_Carluoghi  BegSr
034360051216
034370051216     c                   Eval      *In10 = *Off
034380051216     c                   Eval      *In11 = *Off
034390051216     c                   Eval      *In12 = *Off
034400051216     c                   Eval      *In14 = *Off
034410051216     c                   Eval      *In16 = *Off
034420051216     c                   Eval      *In17 = *Off
034430060208     c                   Eval      *In33 = *Off
034440060208     c                   Eval      *In35 = *Off
034450060208     c                   Eval      *In37 = *Off
034460051216
034470051216      * Luogo invio fattura
034480051216     c                   Eval      kspecli = v1cksc
034490051216     c                   Eval      kspecod = '001'
034500051216     c     kFnspe        Chain     Fnspe01l
034510051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
034520051216     c                   Eval      *In10 = *On
034530051216     c                   EndIf
034540051216
034550051216      * Luogo intestatario pagamento contrassegno
034560051216     c                   Eval      kspecod = '555'
034570051216     c     kFnspe        Chain     Fnspe01l
034580051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
034590051216     c                   Eval      *In11 = *On
034600051216     c                   EndIf
034610051216
034620051216      * Luogo invio contrassegno mittente
034630051216     c                   Eval      kspecod = '666'
034640051216     c     kFnspe        Chain     Fnspe01l
034650051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
034660051216     c                   Eval      *In12 = *On
034670051216     c                   EndIf
034680051216
034690051216      * Luogo spedizione giacenza
034700051219     c                   Clear                   wfax005
034710051219     c                   Clear                   wmail005
034720051216     c                   Eval      kspecod = '005'
034730051216     c     kFnspe        Chain     Fnspe01l
034740051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
034750051216     c                   Eval      *In14 = *On
034760051216     c                   Eval      wfax005 = spefax
034770051219     c     kFnsp2        Chain     Fnsp201l
034780051219     c                   If        %Found(Fnsp201l) and sp2flg = *Blanks
034790051219     c                   Eval      wmail005 = sp2est
034800051219     c                   EndIf
034810051216     c                   EndIf
034820060208
034830060208      * Luogo invio merce resa
034840060208     c                   Eval      kspecod = '300'
034850060208     c     kFnspe        Chain     Fnspe01l
034860060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
034870060208     c                   Eval      *In33 = *On
034880060208     c                   EndIf
034890051216
034900051216      * Luogo preavviso/avviso danno
034910051216     c                   Eval      kspecod = '006'
034920051216     c     kFnspe        Chain     Fnspe01l
034930051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
034940051216     c                   Eval      *In16 = *On
034950051216     c                   EndIf
034960051216
034970051216      * Luogo progetto di liquidazione
034980051216     c                   Eval      kspecod = '008'
034990051216     c     kFnspe        Chain     Fnspe01l
035000051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
035010051216     c                   Eval      *In17 = *On
035020051216     c                   EndIf
035030060208
035040060208      * Luogo invio orm estero
035050060208     c                   Eval      kspecod = '002'
035060060208     c     kFnspe        Chain     Fnspe01l
035070060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
035080060208     c                   Eval      *In35 = *On
035090060208     c                   EndIf
035100060208
035110060208      * Luogo invio doc.a dogana
035120060208     c                   Eval      kspecod = '200'
035130060208     c     kFnspe        Chain     Fnspe01l
035140060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
035150060208     c                   Eval      *In37 = *On
035160060208     c                   EndIf
035170051216
035180051216      * Decodifico i luoghi caricati
035190051216     c                   ExSr      Sr_Decluoghi
035200051216
035210051216     c                   EndSr
035220051216
035230051216      *------------------------------------------------------------------------*
035240051216      * DECODIFICO I LUOGHI CARICATI
035250051216      *------------------------------------------------------------------------*
035260051216     c     Sr_Decluoghi  BegSr
035270051216
035280051216     c                   Clear                   v4dl001
035290051216     c                   Clear                   v4cl001
035300051216     c                   Clear                   v5dl555
035310051216     c                   Clear                   v5cl555
035320051216     c                   Clear                   v5dl666
035330051216     c                   Clear                   v5cl666
035340051216     c                   Clear                   v6dl005
035350051216     c                   Clear                   v6cl005
035360060208     c                   Clear                   v6dl300
035370060208     c                   Clear                   v6cl300
035380051216     c                   Clear                   v7dl006
035390051216     c                   Clear                   v7cl006
035400051216     c                   Clear                   v7dl008
035410051216     c                   Clear                   v7cl008
035420060208     c                   Clear                   v7dl002
035430060208     c                   Clear                   v7cl002
035440060208     c                   Clear                   v7dl200
035450060208     c                   Clear                   v7cl200
035460051216
035470051216      * Decodifico luogo 001
035480071128     c                   clear                   ds4l
035490051216     c                   Eval      kcod = '4L'
035500051216     c                   Eval      kkey = '001'
035510051216     c     kTabel        Chain     Tabel00f
035520051216     c                   If        %Found(Tabel00f) and
035530051216     c                             tblflg = *Blanks
035540051216     c                   Eval      ds4l = tbluni
035550051216     c                   EndIf
035560051216      * Imposto il campo di descrizione
035570060203     c                   Eval      v4dl001 = '001 - ' + §4ldes
035580071128     c                   eval      $ufi001 = §4lufi
035590051216
035600051216      * Decodifico luogo 555
035610071128     c                   clear                   ds4l
035620051216     c                   Eval      kkey = '555'
035630051216     c     kTabel        Chain     Tabel00f
035640051216     c                   If        %Found(Tabel00f) and
035650051216     c                             tblflg = *Blanks
035660051216     c                   Eval      ds4l = tbluni
035670051216     c                   EndIf
035680051216      * Imposto il campo di descrizione
035690060203     c                   Eval      v5dl555 = '555 - ' + §4ldes
035700071128     c                   eval      $ufi555 = §4lufi
035710051216
035720051216      * Decodifico luogo 666
035730071128     c                   clear                   ds4l
035740051216     c                   Eval      kkey = '666'
035750051216     c     kTabel        Chain     Tabel00f
035760051216     c                   If        %Found(Tabel00f) and
035770051216     c                             tblflg = *Blanks
035780051216     c                   Eval      ds4l = tbluni
035790051216     c                   EndIf
035800051216      * Imposto il campo di descrizione
035810060203     c                   Eval      v5dl666 = '666 - ' + §4ldes
035820071128     c                   eval      $ufi666 = §4lufi
035830051216
035840051216      * Decodifico luogo 005
035850071128     c                   clear                   ds4l
035860051216     c                   Eval      kkey = '005'
035870051216     c     kTabel        Chain     Tabel00f
035880051216     c                   If        %Found(Tabel00f) and
035890051216     c                             tblflg = *Blanks
035900051216     c                   Eval      ds4l = tbluni
035910051216     c                   EndIf
035920051216      * Imposto il campo di descrizione
035930060203     c                   Eval      v6dl005 = '005 - ' + §4ldes
035940071128     c                   eval      $ufi005 = §4lufi
035950060208
035960060208      * Decodifico luogo 300
035970071128     c                   clear                   ds4l
035980060208     c                   Eval      kkey = '300'
035990060208     c     kTabel        Chain     Tabel00f
036000060208     c                   If        %Found(Tabel00f) and
036010060208     c                             tblflg = *Blanks
036020060208     c                   Eval      ds4l = tbluni
036030060208     c                   EndIf
036040060208      * Imposto il campo di descrizione
036050060208     c                   Eval      v6dl300 = '300 - ' + §4ldes
036060071128     c                   eval      $ufi300 = §4lufi
036070051216
036080051216      * Decodifico luogo 006
036090071128     c                   clear                   ds4l
036100051216     c                   Eval      kkey = '006'
036110051216     c     kTabel        Chain     Tabel00f
036120051216     c                   If        %Found(Tabel00f) and
036130051216     c                             tblflg = *Blanks
036140051216     c                   Eval      ds4l = tbluni
036150051216     c                   EndIf
036160051216      * Imposto il campo di descrizione
036170060203     c                   Eval      v7dl006 = '006 - ' + §4ldes
036180071128     c                   eval      $ufi006 = §4lufi
036190051216
036200051216      * Decodifico luogo 008
036210071128     c                   clear                   ds4l
036220051216     c                   Eval      kkey = '008'
036230051216     c     kTabel        Chain     Tabel00f
036240051216     c                   If        %Found(Tabel00f) and
036250051216     c                             tblflg = *Blanks
036260051216     c                   Eval      ds4l = tbluni
036270051216     c                   EndIf
036280051216      * Imposto il campo di descrizione
036290060203     c                   Eval      v7dl008 = '008 - ' + §4ldes
036300071128     c                   eval      $ufi008 = §4lufi
036310060208
036320060208      * Decodifico luogo 002
036330071128     c                   clear                   ds4l
036340060208     c                   Eval      kkey = '002'
036350060208     c     kTabel        Chain     Tabel00f
036360060208     c                   If        %Found(Tabel00f) and
036370060208     c                             tblflg = *Blanks
036380060208     c                   Eval      ds4l = tbluni
036390060208     c                   EndIf
036400060208      * Imposto il campo di descrizione
036410060208     c                   Eval      v7dl002 = '002 - ' + §4ldes
036420071128     c                   eval      $ufi002 = §4lufi
036430060208
036440060208      * Decodifico luogo 200
036450071128     c                   clear                   ds4l
036460060208     c                   Eval      kkey = '200'
036470060208     c     kTabel        Chain     Tabel00f
036480060208     c                   If        %Found(Tabel00f) and
036490060208     c                             tblflg = *Blanks
036500060208     c                   Eval      ds4l = tbluni
036510060208     c                   EndIf
036520060208      * Imposto il campo di descrizione
036530060208     c                   Eval      v7dl200 = '200 - ' + §4ldes
036540071128     c                   eval      $ufi200 = §4lufi
036550051216
036560051216     c                   EndSr
036570051216
036580051216      *------------------------------------------------------------------------*
036590051216      * CARICO LE NOTE PREVISTE
036600051216      *------------------------------------------------------------------------*
036610051216     c     Sr_Carnote    BegSr
036620051216
036630051216     c                   Eval      *In24 = *Off
036640051216     c                   Eval      *In26 = *Off
036650051216     c                   Eval      *In31 = *Off
036660051216     c                   Eval      *In21 = *Off
036670051216     c                   Eval      *In13 = *Off
036680051216     c                   Eval      *In15 = *Off
036690051216     c                   Eval      *In18 = *Off
036700060208     c                   Eval      *In34 = *Off
036710060208     c                   Eval      *In36 = *Off
036720090612     c                   Eval      *In40 = *Off
036730100729     c                   Eval      *In68 = *Off
036740051216
036750091112     c   70
036760091112     corn06              Eval      kntcapl = 'C'
036770100806     c   06
036780091112     cann70              Eval      kntcapl = 'T'
036790051216     c                   Movel(p)  dutkci        kntcnk1
036800091112     c   70
036810091112     corn06              Move      v1cksc        kntcnk1
036820100806     c   06
036830100806     cann70              Move      v1cnrv        kntcnk1
036840051216     c                   Clear                   kntcnk2
036850091119
036860091119      * Nota DC
036870091119     c                   clear                   v2ntcdc
036880091119     c                   Movel     'DC'          kntctnt
036890091119     c     kTfntc        Chain(n)  Tfntc01l
036900091119     c                   If        %Found(Tfntc01l)
036910091119     c                   Eval      v2ntcdc = ntcrnt
036920091119     c                   Eval      savntcdc = ntcrnt
036930091119     c                   EndIf
036940091119
036950140722      * Note 02/03
036960140722     c                   exsr      sr_CarNoteRN
036970051216
036980140722      * Note 05/06/T5/I5
036990140722     c                   exsr      sr_CarNoteRT
037000051216
037010140722      * Note 07/08/T7/I7
037020140722     c                   exsr      sr_CarNoteRL
037030051216
037040051216      * Nota 84
037050051216     c                   Movel     '84'          kntctnt
037060091119     c     kTfntc        Chain(n)  Tfntc01l
037070051216     c                   If        %Found(Tfntc01l)
037080051216     c                   Eval      *In21 = *On
037090051216     c                   EndIf
037100051216
037110051216      * Nota 85
037120051216     c                   Movel     '85'          kntctnt
037130091119     c     kTfntc        Chain(n)  Tfntc01l
037140051216     c                   If        %Found(Tfntc01l)
037150051216     c                   Eval      *In13 = *On
037160051216     c                   EndIf
037170051216
037180051216      * Nota 88
037190051216     c                   Movel     '88'          kntctnt
037200091119     c     kTfntc        Chain(n)  Tfntc01l
037210051216     c                   If        %Found(Tfntc01l)
037220051216     c                   Eval      *In15 = *On
037230051216     c                   EndIf
037240051216
037250051216      * Nota 87
037260051216     c                   Movel     '87'          kntctnt
037270091119     c     kTfntc        Chain(n)  Tfntc01l
037280051216     c                   If        %Found(Tfntc01l)
037290051216     c                   Eval      *In18 = *On
037300051216     c                   EndIf
037310060208
037320060208      * Nota 83
037330060208     c                   Movel     '83'          kntctnt
037340091119     c     kTfntc        Chain(n)  Tfntc01l
037350060208     c                   If        %Found(Tfntc01l)
037360060208     c                   Eval      *In34 = *On
037370060208     c                   EndIf
037380060208
037390060208      * Nota 86
037400060208     c                   Movel     '86'          kntctnt
037410091119     c     kTfntc        Chain(n)  Tfntc01l
037420060208     c                   If        %Found(Tfntc01l)
037430060208     c                   Eval      *In36 = *On
037440060208     c                   EndIf
037450090612
037460090612      * Nota 89
037470090612     c                   Movel     '89'          kntctnt
037480091119     c     kTfntc        Chain(n)  Tfntc01l
037490090612     c                   If        %Found(Tfntc01l)
037500090612     c                   Eval      *In40 = *On
037510090612     c                   EndIf
037520100729
037530100729      * Nota 82
037540100729     c                   Movel     '82'          kntctnt
037550100729     c     kTfntc        Chain(n)  Tfntc01l
037560100729     c                   If        %Found(Tfntc01l)
037570100729     c                   Eval      *In68 = *On
037580100729     c                   EndIf
037590051216
037600051216      * Decodifico le note caricate
037610051216     c                   ExSr      Sr_Decnote
037620051216
037630051216     c                   EndSr
037640140722
037650140722      *------------------------------------------------------------------------*
037660140722      * CARICO LE NOTE 02/03 (NEWS NOMINATIVO)
037670140722      *------------------------------------------------------------------------*
037680140722     c     Sr_CarNoteRN  BegSr
037690140722      *
037700140722      * Nota 02
037710140722     c                   movel     '02'          kNtcTnt
037720140722     c     kTfntc        chain(n)  TFNTC
037730140722     c                   if        %found(TFNTC01L)
037740140722     c                   eval      *in24 = *on
037750140722     c                   endif
037760140722      *
037770140722      * Nota 03
037780140722     c                   Movel     '03'          kNtcTnt
037790140722     c     kTfntc        chain(n)  TFNTC
037800140722     c                   if        %found(TFNTC01L)
037810140722     c                   eval      *in24 = *on
037820140722     c                   endif
037830140722      *
037840140722     c                   EndSr
037850140722
037860140722      *------------------------------------------------------------------------*
037870140722      * CARICO LE NOTE 05/06/T5/I5 (RESPONSABILE TRASPORTI)
037880140722      *------------------------------------------------------------------------*
037890140722     c     Sr_CarNoteRT  BegSr
037900140722      *
037910140722      * Nota 05
037920140722     c                   movel     '05'          kNtcTnt
037930140722     c     kTfntc        chain(n)  TFNTC
037940140722     c                   if        %found(TFNTC01L)
037950140722     c                   eval      *in26 = *On
037960140722     c                   endif
037970140722      *
037980140722      * Nota 06
037990140722     c                   movel     '06'          kNtcTnt
038000140722     c     kTfntc        chain(n)  TFNTC
038010140722     c                   if        %found(TFNTC01L)
038020140722     c                   eval      *in26 = *On
038030140722     c                   endif
038040140722      *
038050140722      * Nota T5
038060140722     c                   movel     'T5'          kNtcTnt
038070140722     c     kTfntc        chain(n)  TFNTC
038080140722     c                   if        %found(TFNTC01L)
038090140722     c                   eval      *in26 = *On
038100140722     c                   endif
038110140722      *
038120140722      * Nota I5
038130140722     c                   movel     'I5'          kNtcTnt
038140140722     c     kTfntc        chain(n)  TFNTC
038150140722     c                   if        %found(TFNTC01L)
038160140722     c                   eval      *in26 = *On
038170140722     c                   endif
038180140722      *
038190140722     c                   EndSr
038200140722
038210140722      *------------------------------------------------------------------------*
038220140722      * CARICO LE NOTE 07/08/T7/I7 (RESPONSABILE LOGISTICA)
038230140722      *------------------------------------------------------------------------*
038240140722     c     Sr_CarNoteRL  BegSr
038250140722      *
038260140722      * Nota 07
038270140722     c                   movel     '07'          kNtcTnt
038280140722     c     kTfntc        chain(n)  TFNTC
038290140722     c                   if        %found(TFNTC01L)
038300140722     c                   eval      *in31 = *On
038310140722     c                   endif
038320140722      *
038330140722      * Nota 08
038340140722     c                   movel     '08'          kNtcTnt
038350140722     c     kTfntc        chain(n)  TFNTC
038360140722     c                   if        %found(TFNTC01L)
038370140722     c                   eval      *in31 = *On
038380140722     c                   endif
038390140722      *
038400140722      * Nota T7
038410140722     c                   movel     'T7'          kNtcTnt
038420140722     c     kTfntc        chain(n)  TFNTC
038430140722     c                   if        %found(TFNTC01L)
038440140722     c                   eval      *in31 = *On
038450140722     c                   endif
038460140722      *
038470140722      * Nota I5
038480140722     c                   movel     'I7'          kNtcTnt
038490140722     c     kTfntc        chain(n)  TFNTC
038500140722     c                   if        %found(TFNTC01L)
038510140722     c                   eval      *in31 = *On
038520140722     c                   endif
038530140722      *
038540140722     c                   EndSr
038550051216
038560051216      *------------------------------------------------------------------------*
038570051216      * DECODIFICO LE NOTE CARICATE
038580051216      *------------------------------------------------------------------------*
038590051216     c     Sr_Decnote    BegSr
038600051216
038610051216     c                   Clear                   v4dnt84
038620051216     c                   Clear                   v4cnt84
038630051216     c                   Clear                   v5dnt85
038640051216     c                   Clear                   v5cnt85
038650090612     c                   Clear                   v5dnt89
038660090612     c                   Clear                   v5cnt89
038670100729     c                   Clear                   v5cnt82
038680051216     c                   Clear                   v6dnt88
038690051216     c                   Clear                   v6cnt88
038700051216     c                   Clear                   v7dnt87
038710051216     c                   Clear                   v7cnt87
038720060208     c                   Clear                   v7dnt83
038730060208     c                   Clear                   v7cnt83
038740060208     c                   Clear                   v7dnt86
038750060208     c                   Clear                   v7cnt86
038760140722
038770140722     c                   Eval      kcod = '1T'
038780051216
038790051216      * Decodifico nota 84
038800051216     c                   Eval      kkey = '84'
038810051216     c     kTabel        Chain     Tabel00f
038820051216     c                   If        %Found(Tabel00f) and
038830051216     c                             tblflg = *Blanks
038840051216     c                   Eval      ds1t = tbluni
038850051216     c                   EndIf
038860051216      * Imposto il campo di descrizione
038870060203     c                   Eval      v4dnt84 = ' 84 - ' + §1tdes
038880051216
038890051216      * Decodifico nota 85
038900051216     c                   Eval      kkey = '85'
038910051216     c     kTabel        Chain     Tabel00f
038920051216     c                   If        %Found(Tabel00f) and
038930051216     c                             tblflg = *Blanks
038940051216     c                   Eval      ds1t = tbluni
038950051216     c                   EndIf
038960051216      * Imposto il campo di descrizione
038970060203     c                   Eval      v5dnt85 = ' 85 - ' + §1tdes
038980090612
038990090612      * Decodifico nota 89
039000090612     c                   Eval      kkey = '89'
039010090612     c     kTabel        Chain     Tabel00f
039020090612     c                   If        %Found(Tabel00f) and
039030090612     c                             tblflg = *Blanks
039040090612     c                   Eval      ds1t = tbluni
039050090612     c                   EndIf
039060090612      * Imposto il campo di descrizione
039070090612     c                   Eval      v5dnt89 = ' 89 - ' + §1tdes
039080100729
039090100729      * Decodifico nota 82
039100100729     c                   Eval      kkey = '82'
039110100729     c     kTabel        Chain     Tabel00f
039120100729     c                   If        %Found(Tabel00f) and
039130100729     c                             tblflg = *Blanks
039140100729     c                   Eval      ds1t = tbluni
039150100729     c                   EndIf
039160100729      * Imposto il campo di descrizione
039170100729     c                   Eval      v5dnt82 = ' 82 - ' + §1tdes
039180051216
039190051216      * Decodifico nota 88
039200051216     c                   Eval      kkey = '88'
039210051216     c     kTabel        Chain     Tabel00f
039220051216     c                   If        %Found(Tabel00f) and
039230051216     c                             tblflg = *Blanks
039240051216     c                   Eval      ds1t = tbluni
039250051216     c                   EndIf
039260051216      * Imposto il campo di descrizione
039270060203     c                   Eval      v6dnt88 = ' 88 - ' + §1tdes
039280051216
039290051216      * Decodifico nota 87
039300051216     c                   Eval      kkey = '87'
039310051216     c     kTabel        Chain     Tabel00f
039320051216     c                   If        %Found(Tabel00f) and
039330051216     c                             tblflg = *Blanks
039340051216     c                   Eval      ds1t = tbluni
039350051216     c                   EndIf
039360051216      * Imposto il campo di descrizione
039370060203     c                   Eval      v7dnt87 = ' 87 - ' + §1tdes
039380060208
039390060208      * Decodifico nota 83
039400060208     c                   Eval      kkey = '83'
039410060208     c     kTabel        Chain     Tabel00f
039420060208     c                   If        %Found(Tabel00f) and
039430060208     c                             tblflg = *Blanks
039440060208     c                   Eval      ds1t = tbluni
039450060208     c                   EndIf
039460060208      * Imposto il campo di descrizione
039470060208     c                   Eval      v7dnt83 = ' 83 - ' + §1tdes
039480060208
039490060208      * Decodifico nota 86
039500060208     c                   Eval      kkey = '86'
039510060208     c     kTabel        Chain     Tabel00f
039520060208     c                   If        %Found(Tabel00f) and
039530060208     c                             tblflg = *Blanks
039540060208     c                   Eval      ds1t = tbluni
039550060208     c                   EndIf
039560060208      * Imposto il campo di descrizione
039570060208     c                   Eval      v7dnt86 = ' 86 - ' + §1tdes
039580051216
039590051216     c                   EndSr
039600051216
039610051216      *------------------------------------------------------------------------*
039620051216      *  Imposto descrizione tasti funzione
039630051216      *------------------------------------------------------------------------*
039640051216     c     Sr_Tastifun   BegSr
039650051216
039660051216      * Conta i caratteri riempiti dalle RigaTf1
039670051216     c                   Clear                   $z
039680051216      * Conta i caratteri riempiti dalle RigaTf2
039690051216     c                   Clear                   $k
039700051216      * Conta i caratteri riempiti dalle RigaTf3
039710051216     c                   Clear                   $j
039720051216      * Conta la posizione del campone da cui partire per impostare
039730051216      * la descrizione del tasto funzione
039740051216     c                   Clear                   $y
039750051216
039760051216      * Stringhe che contengono le descrizioni dei tasti funzione
039770051216     c                   Reset                   rigatf1
039780051216     c                   Reset                   rigatf2
039790051216     c                   Reset                   rigatf3
039800060203
039810060731      * F4 - Abilitazioni
039820120117       //?Se no angrafica provvisoria e no immissione
039830111007     c                   eval      *in78 = *off
039840110907     c                   if        not *in01 and not *in06
039850111117     c                   eval      *in78 = *on
039860060731     c                   reset                   $tf
039870060731     c                   movea     tf04          $tf
039880060731     c                   exsr      rie_$tf
039890060731     c                   endif
039900100407
039910100524       //?F22-Richiesta Contatto
039920100806       //?Se anagrafica clienti abilito F22=Richiesta Contatto
039930100407       //?e se utente di filiale
039940100419       //?e se pgm da menù
039950100507       //?e se NON immissione
039960100806     c                   IF        not *in06 and
039970100407     c                             not *in07 and
039980100419     c                             not *in09 and
039990100507     c                             not *in01 and
040000100419     c                             not $pgmrich
040010100407     c                   reset                   $tf
040020100524     c                   Movea     tf22          $tf
040030100407     c                   ExSr      Rie_$tf
040040100407     c                   ENDIF
040050100507
040060100507       //?F5 - Attività
040070120124       //?se utente di sede solo in interrogazione
040080120124       //?se non sono in anagrafica provvisoria,
040090100507       //?se non è convalida offerta e se ci sono attività sul cliente
040100100507       //?e non sono in immissione
040110100507     c                   eval      *in67 = *off
040120100806     c                   IF        not *in06 and
040130100507     c                             not *in07 and
040140120124     c                             not *in01
040150100507      * cerco se ci sono attività sul cliente
040160110516     c     V1Cksc        setll     TIATC07L
040170110516     c                   IF        %equal(TIATC07L)
040180100507     c                   eval      *in67 = *on
040190120124      * se utente di sede e NON è interrogazione spengo F5
040200120124     c                   IF        *in09 and not *in05
040210120124     c                   eval      *in67 = *off
040220120124     c                   ENDIF
040230120124     c                   IF        *in67
040240100507     c                   reset                   $tf
040250100507     c                   movea     tf05          $tf
040260100507     c                   exsr      rie_$tf
040270120124     c                   ENDIF
040280100507     c                   ENDIF
040290100507     c                   ENDIF
040300100507
040310100507      * F15 - Codici collegati
040320100507     c                   If        Not *In01 and Not *In06
040330100507     c                   Reset                   $tf
040340100507     c                   Movea     tf15          $tf
040350100507     c                   ExSr      Rie_$tf
040360100507     c                   EndIf
040370100507
040380060731      * F14 - Info Commerciali
040390100806     c                   If        Not *In06
040400060731     c                   Reset                   $tf
040410060731     c                   Movea     tf14          $tf
040420060731     c                   ExSr      Rie_$tf
040430090914     c                   endif
040440060526      * F19 - Variazioni
040450060526     c                   If        Not *In01 and Not *In06
040460060526     c                   Reset                   $tf
040470060526     c                   Movea     tf19          $tf
040480060526     c                   ExSr      Rie_$tf
040490060526     c                   endif
040500100407      * F20 - Interrogazione clienti potenziali
040510100407     c                   If        *In29 = *On and not *in65 and
040520100806     c                             not *in06
040530100407     c                   Reset                   $tf
040540100407     c                   Movea     tf20          $tf
040550100407     c                   ExSr      Rie_$tf
040560100407     c                   EndIf
040570101213
040580101213       //?F17-Dati Fatturazione
040590110322     c                   IF        not *in06
040600101213     c                   reset                   $tf
040610101213     c                   Movea     tf17          $tf
040620101213     c                   ExSr      Rie_$tf
040630101213     c                   ENDIF
040640110223
040650110223       //?F21-Blocco cliente
040660110223       //?Abilito se anagrafica clienti
040670110223       //?se utente di filiale
040680110223       //?se pgm da menù
040690110223       //?se NON immissione
040700110223       //?se codice non bloccato
040710110223     c                   IF        not *in06 and
040720110223     c                             not *in07 and
040730110223     c                             not *in09 and
040740110223     c                             not *in01 and
040750110223     c                             v2cabl = *blanks and
040760110223     c                             not $pgmrich
040770110223     c                   reset                   $tf
040780110223     c                   Movea     tf21          $tf
040790110223     c                   ExSr      Rie_$tf
040800110223     c                   ENDIF
040810110223
040820060203      * F12 - Ritorno
040830060206     c                   If        (Not *In29 and *In06) or
040840060206     c                             (Not *In29 and *In07) or
040850060206     c                             (Not *In06 and Not *In07)
040860060203     c                   Reset                   $tf
040870060203     c                   Movea     tf12          $tf
040880060203     c                   ExSr      Rie_$tf
040890060206     c                   EndIf
040900051216
040910051216      * Pulisco la stringa
040920051216     c                   Eval      $h = 1
040930051216     c                   For       $h by 1 to 62
040940051216     c                   If        rigatf1($h) = '#'
040950051216     c                   Clear                   rigatf1($h)
040960051216     c                   EndIf
040970051216     c                   EndFor
040980051216     c                   Eval      $h = 1
040990051216     c                   For       $h by 1 to 62
041000051216     c                   If        rigatf2($h) = '#'
041010051216     c                   Clear                   rigatf2($h)
041020051216     c                   EndIf
041030051216     c                   EndFor
041040051216     c                   Eval      $h = 1
041050051216     c                   For       $h by 1 to 62
041060051216     c                   If        rigatf3($h) = '#'
041070051216     c                   Clear                   rigatf3($h)
041080051216     c                   EndIf
041090051216     c                   EndFor
041100051216
041110051216      * Imposto la descrizione del tasto funzione F24 e segnalo
041120051216      * quante righe ho riempito e quale devo emettere
041130051216     c                   Select
041140051216
041150051216     c                   When      $k <> *Zeros and $y <> *Zeros
041160051216     c                   Eval      $loop = 3
041170051216     c                   Eval      vzfd02 = cf2413
041180051216
041190051216     c                   When      $k <> *Zeros and $y = *Zeros
041200051216     c                   Eval      $loop = 2
041210051216     c                   Eval      vzfd02 = cf2412
041220051216
041230051216     c                   Other
041240051216
041250051216     c                   Eval      $loop = 1
041260051216     c                   Clear                   vzfd02
041270051216     c                   EndSl
041280051216
041290051216      * Prima riga di tasti funzione
041300051216     c                   Movea     rigatf1       vzfd01
041310051216     c                   Eval      $rig = 1
041320051216
041330051216     c                   EndSr
041340051216
041350051216      *------------------------------------------------------------------------*
041360051216      *  IMPOSTO LE STRINGHE CON I CAMPI FUNZIONE COMPATTATI
041370051216      *------------------------------------------------------------------------*
041380051216     c     Rie_$tf       BegSr
041390051216      *
041400051216     c                   Eval      $x = 1
041410051216     c     '#'           Lookup    $tf($x)                                30
041420051216     c                   Add       $x            $z
041430051216     c                   If        $Z <= 63
041440051216     c                   Eval      $j = $Z - $x + 1
041450051216     c                   Movea     $tf           rigatf1($j)
041460051216     c                   Else
041470051216     c                   Add       $x            $k
041480051216     c                   If        $k <= 63
041490051216     c                   Eval      $j = $K - $x + 1
041500051216     c                   Movea     $tf           rigatf2($j)
041510051216     c                   Else
041520051216     c                   Add       $x            $y
041530051216     c                   If        $y <= 63
041540051216     c                   Eval      $j = $y - $x + 1
041550051216     c                   Movea     $tf           rigatf3($j)
041560051216     c                   EndIf
041570051216     c                   EndIf
041580051216     c                   EndIf
041590051216
041600051216     c                   EndSr
041610051216
041620051108      *------------------------------------------------------------------------*
041630051130      * CONTROLLO SECONDA VIDEATA - DATI ANAGRAFICI/BLOCCO
041640051108      *------------------------------------------------------------------------*
041650051108     c     Sr_Ctrd02     BegSr
041660051117
041670051117      * Pulisco i campi del posizionamento cursore
041680051117     c                   Clear                   h2riga
041690051117     c                   Clear                   h2colo
041700051125
041710051118      * Imposto gli ulti 4 byte del codice cliente
041720051118     c                   Move      v1cksc        wksc04
041730051117
041740051117      * RAGIONE SOCIALE
041750051117      * --> Obbligatoria
041760051117     c                   If        v2crag = *Blanks
041770051117     c                   Eval      *In28 = *On
041780051130     c                   Eval      h2riga = 07
041790051117     c                   Eval      h2colo = 28
041800051117     c                   Eval      v2cmsg = msg(11)
041810051117     c                   Leavesr
041820051117     c                   EndIf
041830051117
041840051117      * CODICE POTENZIALE
041850051125     c                   ExSr      Sr_Ctrcpo
041860051125     c   28              Leavesr
041870051118
041880051118      * --> Caratteri non validi
041890051212     c                   Eval      xx = 1
041900051212     c                   For       xx by 1 to 10
041910051212     c                   If        car(xx) <> *Blanks
041920051212     c                   Eval      wpos = %scan(car(xx):v2crag)
041930051212     c                   If        wpos > 0
041940051212     c                   Eval      *In28 = *On
041950051212     c                   Eval      h2riga = 07
041960051212     c                   Eval      h2colo = 28
041970051212     c                   Eval      v2cmsg = msg(32)
041980051212     c                   Leavesr
041990051212     c                   EndIf
042000051212     c                   EndIf
042010051212     c                   EndFor
042020051118
042030051117      * INDIRIZZO
042040051118      * --> Controllo se cliente
042050051118     c                   If        v1ckcc = 151
042060051118     c                   Clear                   fnlv13ds
042070051118     c                   Eval      i14rsc = v2crag
042080051118     c                   Eval      i14ind = v2cvia
042090051118     c                   Eval      e14cap = v2ccae
042100051118     c                   Eval      e14loc = v2ccit
042110051118     c                   Eval      e14prv = v2cprv
042120051118     c                   Eval      e14nar = v2csta
042130051118     c                   ExSr      Sr_Ctrind
042140051118     c                   Eval      v2ccae = e14cap
042150051118     c                   Eval      v2ccit = e14loc
042160051118     c                   Eval      v2cprv = e14prv
042170051118     c                   Eval      v2csta = e14nar
042180051118     c   28              Leavesr
042190051118     c                   EndIf
042200051117
042210051117      * TELEFONO
042220051117      * --> Controllo (forzabile)
042230051124      *     Solo se variato il campo a video quindi al primo errore se non
042240051124      *     vario il campo non controllo più
042250051118     c                   If        v2ctel <> wtel and v2ctel <> *Blanks
042260051118     c                   Clear                   trul42ds
042270051118     c                   Eval      d42fax = v2ctel
042280051118     c                   Call      'TRUL42R'
042290051118     c                   Parm                    trul42ds
042300051118     c                   If        d42err = '1'
042310051118     c                   Eval      *In28 = *On
042320051202     c                   Eval      h2riga = 11
042330051118     c                   Eval      h2colo = 28
042340051118     c                   Eval      v2cmsg = d42msg
042350051118     c                   Leavesr
042360051118     c                   Eval      wtel = v2ctel
042370051117     c                   EndIf
042380051118     c                   EndIf
042390051118
042400051117      * FAX
042410051125      * --> Controllo (forzabile)
042420051125      *     Solo se variato il campo a video quindi al primo errore se non
042430051125      *     vario il campo non controllo più
042440051118     c                   If        v2ctlf <> wtlf and v2ctlf <> *Blanks
042450051118     c                   Clear                   trul42ds
042460051118     c                   Eval      d42fax = v2ctlf
042470051118     c                   Call      'TRUL42R'
042480051118     c                   Parm                    trul42ds
042490051118     c                   If        d42err = '1'
042500051118     c                   Eval      *In28 = *On
042510051202     c                   Eval      h2riga = 11
042520051118     c                   Eval      h2colo = 52
042530051118     c                   Eval      v2cmsg = d42msg
042540051118     c                   Leavesr
042550051118     c                   Eval      wtlf = v2ctlf
042560051118     c                   EndIf
042570051118     c                   EndIf
042580051118
042590051118      * CODICE FISCALE
042600051118     c                   ExSr      Sr_Ctrcdf
042610051118     c   28              Leavesr
042620051118
042630051118      * PARTITA IVA + STATO
042640051118     c                   ExSr      Sr_Ctriva
042650051118     c   28              Leavesr
042660051122
042670051122      * BLOCCO CLIENTE
042680051122     c                   ExSr      Sr_Ctrblc
042690051122     c   28              Leavesr
042700090616      * sollecito
042710090616     c                   ExSr      Sr_Ctrsol
042720090617     c   28              Leavesr
042730051108
042740051108     c                   EndSr
042750051130
042760051130      *------------------------------------------------------------------------*
042770051130      * CONTROLLO TERZA VIDEATA - DATI COMMERCIALI
042780051130      *------------------------------------------------------------------------*
042790051130     c     Sr_Ctrd03     BegSr
042800051130
042810051130      * Pulisco i campi del posizionamento cursore
042820051130     c                   Clear                   h3riga
042830051130     c                   Clear                   h3colo
042840051130
042850051130      * CODICE COMMERCIALE
042860051130     c                   ExSr      Sr_Ctrage
042870051130     c   28              Leavesr
042880051130
042890051130      * CODICE IMPORTANZA CLIENTE
042900051130     c                   ExSr      Sr_Ctrclv
042910051130     c   28              Leavesr
042920051130
042930051130      * CODICE ISTAT
042940051130     c                   ExSr      Sr_Ctritc
042950051130     c   28              Leavesr
042960051207
042970140722      * NOTE   02/03 - NEWS NOMINATIVO
042980140722     c                   eval      wRGR = 'RN'
042990140722     c                   exsr      sr_CtrNote
043000140722     c   28
043010140722     cor 90              leavesr
043020051207
043030140722      * NOTE   05/06/T5/I5 - RESPONSABILE TRASPORTI
043040140722     c                   eval      wRGR = 'RT'
043050140722     c                   exsr      sr_CtrNote
043060140722     c   28
043070140722     cor 90              leavesr
043080051207
043090140722      * NOTE   07/08/T7/I7 - RESPONSABILE LOGISTICA
043100140722     c                   eval      wRGR = 'RL'
043110140722     c                   exsr      sr_CtrNote
043120140722     c   28
043130140722     cor 90              leavesr
043140051130
043150051130     c                   EndSr
043160051108
043170051108      *------------------------------------------------------------------------*
043180051130      * CONTROLLO QUARTA VIDEATA - DATI FATTURAZIONE
043190051108      *------------------------------------------------------------------------*
043200051130     c     Sr_Ctrd04     BegSr
043210051125
043220051125      * Pulisco i campi del posizionamento cursore
043230051130     c                   Clear                   h4riga
043240051130     c                   Clear                   h4colo
043250051125
043260051125      * CODICE SOTTOCONTO FATTURAZIONE
043270051125     c                   ExSr      Sr_Ctrscf
043280100127     c   90
043290100127     cor 28              Leavesr
043300051128
043310051128      * FLAG FATTURA UNIFICATA
043320051128     c                   ExSr      Sr_Ctruni
043330051128     c   28              Leavesr
043340051125
043350051125      * TIPO FATTURA
043360051125     c                   ExSr      Sr_Ctrtft
043370051125     c   28              Leavesr
043380051125
043390051125      * FREQUENZA FATTURA
043400051125     c                   ExSr      Sr_Ctrfft
043410051125     c   28              Leavesr
043420051125
043430051125      * TIPO DATA FATTURA
043440051125     c                   ExSr      Sr_Ctrtdf
043450051125     c   28              Leavesr
043460051128
043470051128      * SPESE INVIO FATTURA
043480051128     c                   ExSr      Sr_Ctrsin
043490051128     c   28              Leavesr
043500100728
043510100728      * CODICE PAGAMENTO FATTURA
043520100728     c                   ExSr      Sr_Ctrcdp
043530100728     c   28              Leavesr
043540100728
043550100728      * BANCA APPOGGIO
043560100728     c                   ExSr      Sr_Ctrbna
043570100728     c   28              Leavesr
043580100728
043590100728      * ABI/CAB
043600100728     c                   ExSr      Sr_Ctrabicabf
043610100728     c   28              Leavesr
043620051128
043630051206      * LUOGO 001 - INVIO FATTURA
043640051212      * Se non sono in visite/offerte
043650051128     c                   If        Not *In06
043660091112      * o se trattativa da cliente
043670091112     c                             or *in70
043680051128     c                   ExSr      Sr_Ctrl001
043690051128     c   28              Leavesr
043700051128     c                   EndIf
043710051129
043720051206      * NOTA   84 - INVIO FATTURA
043730051129     c                   ExSr      Sr_Ctrnt84
043740051129     c   28              Leavesr
043750051108
043760051108     c                   EndSr
043770051216
043780051108      *------------------------------------------------------------------------*
043790051130      * CONTROLLO QUINTA VIDEATA - CONDIZIONI PAGAMENTO/PAG. CONTRASSEGNI
043800051108      *------------------------------------------------------------------------*
043810051130     c     Sr_Ctrd05     BegSr
043820051129
043830051129      * Pulisco i campi del posizionamento cursore
043840051130     c                   Clear                   h5riga
043850051130     c                   Clear                   h5colo
043860100728     c                   clear                   wbanca
043870100728     c                   clear                   wagenzia
043880100728
043890100728      * BLOCCO PAGAMENTI
043900130322      * Se impostato SI dall'utente
043910130322     c                   IF        not *in81 and V5Cblp = 'SI'
043920130325     c                   eval      wbloc = 'B'
043930130322     c                   ENDIF
043940130322      * Se impostato NO dall'utente
043950130322     c                   IF        not *in81 and V5Cblp = 'NO'
043960130325     c                   eval      wbloc = '0'
043970130322     c                   ENDIF
043980150424      /free
043990150424       //?Come prima cosa devo avvisare se anche solo 1 dei 3 pagamenti
044000150424       //?non è un pagamento tramite Bonifico
044010150424         exsr CtrPagamenti;
044020150424         IF  *in28;
044030150424           leavesr;
044040150424         ENDIF;
044050150424      /end-free
044060051108
044070051130      * CODICE PAGAMENTO CONTRASSEGNI
044080051130     c                   ExSr      Sr_Ctrfpc
044090060215     c                   If        *In28
044100060215     c                   Eval      *In22 = savin22
044110060215     c                   Eval      *In23 = savin23
044120060215     c                   Leavesr
044130060215     c                   EndIf
044140100729     c                   IF        *in90 and $Win
044150100729     c                   leavesr
044160100729     c                   ENDIF
044170150424
044180150424      /free
044190150427       //?Memorizzo se pagamento tramite Bonifico
044200150424         IF  §4Ucod = 'B';
044210150424           BonificoCA = *on;
044220150424         ELSE;
044230150424           BonificoCA = *off;
044240150424         ENDIF;
044250150427       //?Memorizzo se pagamento Estero
044260150427         IF  §4Utip = 'E';
044270150427           PagEsteroCA = *on;
044280150427         ELSE;
044290150427           PagEsteroCA = *off;
044300150427         ENDIF;
044310150427       //?Memorizzo il codice pagamento
044320150427         CodPagCA = V5Cfpc;
044330150424      /end-free
044340150424
044350080115      * CODICE IBAN
044360080115      * solo se richieste obbligatorie le coordinate
044370080115     c                   if        not *in22 and not *in23
044380100729     c                   eval      wiban = v5ciban
044390080115     c                   exsr      sr_ctriban
044400100729     c                   if        *in28
044410100729     c                   eval      h5riga = 10
044420100804     c                   eval      h5colo = 29
044430100729     c                   leavesr
044440100729     c                   endif
044450100729     c                   eval      v5cbab = wdesbanca
044460080115     c                   endif
044470100729
044480100729      /free
044490100729       //?PAGAMENTO DANNI
044500100729         exsr CtrPagDN;
044510100729         IF  *in28;
044520100729           *in71 = savin71;
044530100729           *in72 = savin72;
044540100729           leavesr;
044550100729         ENDIF;
044560150427       //?Memorizzo se pagamento tramite Bonifico
044570150424         IF  §4Ucod = 'B';
044580150424           BonificoDN = *on;
044590150424         ELSE;
044600150424           BonificoDN = *off;
044610150424         ENDIF;
044620150427       //?Memorizzo se pagamento Estero
044630150427         IF  §4Utip = 'E';
044640150427           PagEsteroDN = *on;
044650150427         ELSE;
044660150427           PagEsteroDN = *off;
044670150427         ENDIF;
044680150427       //?Memorizzo il codice pagamento
044690150427         CodPagDN = V5tpdn;
044700100729       //?se window per invio dati alla sede ritorno ad emettere la videata
044710100729       //?senza andare avanti con i controlli
044720100729         IF  $windn and *in90;
044730100729           leavesr;
044740100729         ENDIF;
044750100729       //?controllo le coordinate bancarie
044760100730         IF  not *in71 and not *in72 and d§PAGctr = 'SI';
044770100729           wiban = v5ibandn;
044780100729           exsr sr_ctriban;
044790100729           IF  *in28;
044800100729             h5riga = 13;
044810100804             h5colo = 29;
044820100729             leavesr;
044830100729           ENDIF;
044840100729           v5babdn = wDesBanca;
044850100729         ENDIF;
044860100730       //?se pagamento tramite bonifico deve essere inserito la nota 82-mail
044870100730       //?bonifici danni
044880100730         IF §4ucod = 'B' and not *in68 and v5cnt82 = *blanks;
044890100730           *in28 = *on;
044900100730           v5cmsg = msg(86);
044910100730           h5riga = 21;
044920100730           h5colo = 71;
044930100730           leavesr;
044940100730         ENDIF;
044950100729
044960100729       //?PAGAMENTO NOTE ACCREDITO
044970100729         exsr CtrPagNA;
044980100729         IF  *in28;
044990100729           *in73 = savin73;
045000100729           *in74 = savin74;
045010100729           leavesr;
045020100729         ENDIF;
045030150427       //?Memorizzo se pagamento tramite Bonifico
045040150424         IF  §4Ucod = 'B';
045050150424           BonificoNA = *on;
045060150424         ELSE;
045070150424           BonificoNA = *off;
045080150424         ENDIF;
045090150427       //?Memorizzo se pagamento Estero
045100150427         IF  §4Utip = 'E';
045110150427           PagEsteroNA = *on;
045120150427         ELSE;
045130150427           PagEsteroNA = *off;
045140150427         ENDIF;
045150150427       //?Memorizzo il codice pagamento
045160150427         CodPagNA = V5tpna;
045170150424      /end-free
045180100729       //?se window per invio dati alla sede ritorno ad emettere la videata
045190100729       //?senza andare avanti con i controlli
045200100729         IF  $winna and *in90;
045210100729           leavesr;
045220100729         ENDIF;
045230100729       //?controllo le coordinate bancarie
045240100804         IF  not *in73 and not *in74 and d§PAGctr = 'SI';
045250100729           wiban = v5ibanna;
045260100729           exsr sr_ctriban;
045270100729           IF  *in28;
045280100729             h5riga = 16;
045290100804             h5colo = 29;
045300100729             leavesr;
045310100729           ENDIF;
045320100729           v5babna = wDesBanca;
045330100729         ENDIF;
045340100729      /end-free
045350051206
045360051212      * Se non sono in visite/offerte
045370051212if  1c                   If        Not *In06
045380091112      * o se trattativa da cliente
045390091112     c                             or *in70
045400051206      * LUOGO 555 - INTESTAZIONE PAGAMENTO C/ASSEGNI
045410051206     c                   ExSr      Sr_Ctrl555
045420051206     c   28              Leavesr
045430051206
045440051206      * LUOGO 666 - LUOGO INVIO BONIFICI C/ASSEGNI
045450051206     c                   ExSr      Sr_Ctrl666
045460051206     c   28              Leavesr
045470051212    1c                   EndIf
045480051206
045490051206      * NOTA   85 - E-MAIL BONIFICI C/ASSEGNI
045500051206     c                   ExSr      Sr_Ctrnt85
045510051206     c   28              Leavesr
045520090616
045530090616      * NOTA   89 - E-MAIL RESPONSABILE AMMINISTRATIVO
045540090616     c                   ExSr      Sr_Ctrnt89
045550090616     c   28              Leavesr
045560100729
045570100729      * NOTA   82 - E-MAIL BONIFICO DANNI
045580150424     c                   ExSr      Sr_Ctrnt82
045590100730     c   90              goto      emi05
045600100729     c   28              Leavesr
045610051130
045620051108     c                   EndSr
045630051108
045640051108      *------------------------------------------------------------------------*
045650051130      * CONTROLLO SESTA VIDEATA - GESTIONE GIACENZE
045660051108      *------------------------------------------------------------------------*
045670051130     c     Sr_Ctrd06     BegSr
045680051206
045690051206      * Pulisco i campi del posizionamento cursore
045700051206     c                   Clear                   h6riga
045710051206     c                   Clear                   h6colo
045720060216
045730060216      * LUOGO 005 - SPEDIZIONE GIACENZA
045740060216     c                   If        Not *In06
045750091112     c                             or *in70
045760060216     c                   ExSr      Sr_Ctrl005
045770060216     c   28              Leavesr
045780060216     c                   EndIf
045790060216
045800060216      * NOTA   88 - E-MAIL GIACENZE
045810060216     c                   ExSr      Sr_Ctrnt88
045820060216     c   28              Leavesr
045830060216
045840060216      * LUOGO 300 - MERCE RESA
045850060216     c                   If        Not *In06
045860091112     c                             or *in70
045870060216     c                   ExSr      Sr_Ctrl300
045880060216     c   28              Leavesr
045890060216     c                   EndIf
045900051206
045910051206      * TIPO COMUNICAZIONE AL MITTENTE
045920051206     c                   ExSr      Sr_Ctrtcm
045930051206     c   28              Leavesr
045940051206
045950051206      * TIPO COMUNICAZIONE FINE GIACENZA
045960051206     c                   ExSr      Sr_Ctrtcf
045970051206     c   28              Leavesr
045980051206
045990051206      * APERTURA AUTOMATICA GIACENZA
046000051206     c                   ExSr      Sr_Ctrapg
046010051206     c   28              Leavesr
046020051206
046030051206      * DISPOSIZIONE MITTENTE IN ARRIVO
046040051206     c                   ExSr      Sr_Ctrdmg
046050051206     c   28              Leavesr
046060060801
046070060801      * CHIUSURA AUTOMATICA GIACENZA
046080060801     c                   ExSr      Sr_Ctrcag
046090060801     c   28              Leavesr
046100051108
046110051108     c                   EndSr
046120051108
046130051108      *------------------------------------------------------------------------*
046140051130      * CONTROLLO SETTIMA VIDEATA - GESTIONE DANNI/TASSAZIONE P.A./STOP
046150051108      *------------------------------------------------------------------------*
046160051130     c     Sr_Ctrd07     BegSr
046170051213
046180051213      * Pulisco i campi del posizionamento cursore
046190051213     c                   Clear                   h7riga
046200051213     c                   Clear                   h7colo
046210150728
046220150728      /free
046230150728       //?Come prima cosa devo avvisare se manca la mail per invio
046240150728       //?progetto di liquidazione
046250150728       //?NON lo faccio se anagrafica provvisoria
046260150728         IF  not *in06;
046270150728           exsr CtrMailDanni;
046280150728           IF  *in28;
046290150728             leavesr;
046300150728           ENDIF;
046310150728         ENDIF;
046320150728      /end-free
046330051213
046340051213      * LUOGO 006 - LUOGO PREAVVISO/AVVISO DANNO
046350051213     c                   If        Not *In06
046360091112     c                             or *in70
046370051213     c                   ExSr      Sr_Ctrl006
046380051213     c   28              Leavesr
046390051213     c                   EndIf
046400051213
046410051213      * NOTA   87 - E-MAIL DANNI
046420051213     c                   ExSr      Sr_Ctrnt87
046430051213     c   28              Leavesr
046440051213
046450051213      * LUOGO 008 - PROGETTO DI LIQUIDAZIONE
046460051213     c                   If        Not *In06
046470091112     c                             or *in70
046480051213     c                   ExSr      Sr_Ctrl008
046490051213     c   28              Leavesr
046500051213     c                   EndIf
046510051213
046520051213      * NR. STOP RITIRO
046530051213     c                   ExSr      Sr_Ctrstp
046540051213     c   28              Leavesr
046550060208
046560060208      * NOTA   83 - MAIL ORM ESTERO
046570060208     c                   ExSr      Sr_Ctrnt83
046580060208     c   28              Leavesr
046590060208
046600060208      * LUOGO 002 - INVIO ORM ESTERO
046610060208     c                   If        Not *In06
046620091112     c                             or *in70
046630060208     c                   ExSr      Sr_Ctrl002
046640060208     c   28              Leavesr
046650060208     c                   EndIf
046660060208
046670060208      * NOTA   86 - MANIFEST
046680060208     c                   ExSr      Sr_Ctrnt86
046690060208     c   28              Leavesr
046700060208
046710060208      * LUOGO 200 - DOC. DOGANA
046720060208     c                   If        Not *In06
046730091112     c                             or *in70
046740060208     c                   ExSr      Sr_Ctrl200
046750060208     c   28              Leavesr
046760060208     c                   EndIf
046770051108
046780051108     c                   EndSr
046790051216
046800051216      *------------------------------------------------------------------------*
046810051216      * TASTO FUNZIONE F02 - CONTATTI
046820051216      *------------------------------------------------------------------------*
046830051216     c     Sr_F02        BegSr
046840051216
046850051216     c                   Clear                   tnta12ds
046860051216     c                   Eval      ta12ric = wrich
046870091112     c   70
046880091112     corn06              Eval      ta12apl = 'C'
046890091112     c   70
046900091112     corn06              Eval      ta12ksc = v1cksc
046910100806     c   06
046920091112     cann70              Eval      ta12apl = 'T'
046930100806     c   06
046940100806     cann70              Eval      ta12nrv = v1cnrv
046950060217     c                   Eval      ta12rag = wrag
046960051216     c                   Eval      ta12tnt = wtnt
046970060216     c                   Eval      ta12gcli = '1'
046980060216     c   01              Eval      ta12icli = '1'
046990060307     c   01              Move      v3cage        ta12cmm
047000060227     c                   If        %Parms > 1 and
047010060227     c                             Not *In01 and Not *In05 and ta60icli = '1'
047020060221     c                   Eval      ta12icli = '1'
047030060307     c                   Move      v3cage        ta12cmm
047040060221     c                   EndIf
047050051216     c                   Call      'TNTA12R'
047060051216     c                   Parm                    kpjba
047070051216     c                   Parm                    tnta12ds
047080051216
047090051216     c                   ExSr      Sr_Carnote
047100051216
047110051216     c                   EndSr
047120060217
047130060217      *------------------------------------------------------------------------*
047140060217      * TASTO FUNZIONE F03 - FINE LAVORO
047150060217      *------------------------------------------------------------------------*
047160060217     c     Sr_F03        BegSr
047170060217
047180060217     c                   Eval      *In80 = *Off
047190060217
047200060217      * Se immissione emetto videata per chiedere conferma di uscita dal
047210060217      * pgm informando l'utente che i dati andranno persi
047220060217     c                   If        *In01
047230060217     c                   Eval      w03cok = 'NO'
047240060217     c                   Exfmt     ta60w03
047250060217      * Se ok per uscire dal pgm vado a fine
047260060217if  1c                   If        w03cok = 'SI'
047270060217     c                   Goto      fine
047280060217     c                   Else
047290060217     c                   Eval      *In80 = *On
047300060217     c                   Leavesr
047310060217    1c                   EndIf
047320060217     c                   EndIf
047330110302      * Imposto F3 fine se pgm richiamato
047340110302     c                   IF        $Pgmrich
047350110301     c                   eval      TA60f03 = '1'
047360110302     c                   ENDIF
047370110301
047380060217      * Se arrivo qua non è immissione e quindi vado a fine
047390060217     c                   Goto      fine
047400060217
047410060217     c                   EndSr
047420060731
047430060731      *------------------------------------------------------------------------*
047440060731      * TASTO FUNZIONE F04 - ABILITAZIONI
047450060731      *------------------------------------------------------------------------*
047460060731     c     sr_f04        begsr
047470060731
047480060731     c                   clear                   tnta61ds
047490060731     c                   eval      ta61tla = 'V'
047500060731     c                   eval      ta61ksc = v1cksc
047510060731     c                   call      'TNTA61R'
047520060731     c                   parm                    kpjba
047530060731     c                   parm                    tnta61ds
047540060731
047550060731     c                   endsr
047560100507
047570100507      /free
047580100507       //--------------------------------------------------------------
047590100507       //?Tasto Funzione F5 - Attività.
047600100507       //--------------------------------------------------------------
047610100507       BEGSR sr_F05;
047620100507
047630100507         clear trmk21ds;
047640100507         I21mod = 'I';
047650100507         I21ksc = V1Cksc;
047660120124         I21rsc = V2Crag;
047670100507         kpjbu = trmk21ds;
047680100507
047690100507         trmk21r (kpjba);
047700100507
047710100507       ENDSR;
047720100507      /end-free
047730060731
047740051216      *------------------------------------------------------------------------*
047750051216      * TASTO FUNZIONE F07 - CLIENTE UNIFICANTE
047760051216      *------------------------------------------------------------------------*
047770051216     c     Sr_F07        BegSr
047780051216
047790051216     c                   Clear                   tibs12ds
047800051216     c  n05              Eval      k12op0 = 'P02'
047810051216     c   05              Eval      k12op0 = 'P05'
047820051216     c                   Eval      k12op1 = 'O09'
047830051216     c                   Eval      k12f03 = '0'
047840051216     c                   Eval      k12f12 = '0'
047850051216     c                   Eval      k12err = '0'
047860051216     c                   Move      v1cksc        k12cop
047870051216     c                   Eval      kpjbu = tibs12ds
047880051216     c                   Call      'TIBS12R'
047890051216     c                   Parm                    kpjba
047900051216
047910051216     c                   EndSr
047920100407
047930100407      /free
047940100407       //--------------------------------------------------------------
047950100524       //?Tasto Funzione F22 - Richiesta Contatto.
047960100407       //--------------------------------------------------------------
047970100524       BEGSR sr_F22;
047980110223
047990110223       //?Non posso fare una nuova attività su cliente con potenziale in
048000110223       //?categoria PERSO, devo partire dal potenziale
048010110223         IF  V2Hfls = 'P';
048020110223         *in28 = *on;
048030110223         V2Cmsg = 'Nuova attività non possibile per Potenziale PERSO';
048040110223         leavesr;
048050110223         ENDIF;
048060100407
048070100407         clear trmk17ds;
048080100407         IMK17patt = 'S';
048090100407         IMK17att  = 'T';
048100100407         IMK17pcau = 'S';
048110100407         IMK17cau  = '20';
048120100407         IMK17cpo  = v2ccpo;
048130100407         IMK17ksc  = v1cksc;
048140100407         IF  v3cage > *zeros;
048150100407           IMK17cmm  = %dec(v3cage:7:0);
048160100407         ENDIF;
048170100407
048180100407         trmk17r (kpjba : TRMK17DS);
048190100507
048200100507       //?Ricarico i tasti funzione, nel caso venga inserita l'attività sul
048210100507       //?cliente così faccio vedere il tasto F5-Attività
048220100507         exsr Sr_Tastifun;
048230100407
048240100407       ENDSR;
048250101213
048260101213       //--------------------------------------------------------------
048270101213       //?Tasto Funzione F17 - Dati Fatturazione.
048280101213       //--------------------------------------------------------------
048290101213       BEGSR sr_F17;
048300101213
048310101213         clear trul57ds;
048320101213         D57ikscb = V1Cksc;
048330101213         IF  %check(digits:V4Cscf) > 0;
048340101213           D57ikscf = 0;
048350101213         ELSE;
048360101213           D57ikscf = %dec(V4Cscf:7:0);
048370101213         ENDIF;
048380101213         D57iunib = V4Cuni;
048390101213         D57itftb = V4Ctft;
048400101213         D57ifftb = V4Cfft;
048410101213         D57idftb = V4Ctdf;
048420101213         D57ipgm  = 'TNTA60R';
048430101213         D57isifb = V4Csin;
048440101213         D57icdpb = V4Ccdp;
048450101213         D57iabib = V4Cabi;
048460101213         D57icabb = V4Ccab;
048470101213
048480101213         trul57r (kpjba : TRUL57DS);
048490101213
048500101213       ENDSR;
048510110223
048520110223       //--------------------------------------------------------------
048530110223       //?Tasto Funzione F21 - Blocco Cliente.
048540110223       //--------------------------------------------------------------
048550110223       BEGSR sr_F21;
048560110223
048570110223         clear trmk17ds;
048580110223         IMK17patt = 'S';
048590110223         IMK17att  = 'T';
048600110223         IMK17pcau = 'S';
048610110223         IMK17cau  = '71';
048620110223         IMK17cpo  = v2ccpo;
048630110223         IMK17ksc  = v1cksc;
048640110223         IF  v3cage > *zeros;
048650110223           IMK17cmm  = %dec(v3cage:7:0);
048660110223         ENDIF;
048670110223
048680110223         trmk17r (kpjba : TRMK17DS);
048690110223
048700110223       //?Ricarico i tasti funzione
048710110223         exsr Sr_Tastifun;
048720110310
048730110223
048740110223       ENDSR;
048750110223
048760100407      /end-free
048770051216
048780051216      *------------------------------------------------------------------------*
048790051216      * TASTO FUNZIONE F11 - LUOGHI
048800051216      *------------------------------------------------------------------------*
048810051216     c     Sr_F11        BegSr
048820051216
048830051216     c                   Clear                   parmf11
048840051216     c  n05              Eval      paropz = '2'
048850051216     c   05              Eval      paropz = '5'
048860051216     c                   Move      v1cksc        parcli
048870060216     c                   Eval      partnta60 = '1'
048880060216     c   01              Eval      parimta60 = '1'
048890051216     c                   Eval      kpjbu = parmf11
048900051216     c                   Call      'TNTA80R'
048910051216     c                   Parm                    kpjba
048920051216
048930051216     c                   ExSr      Sr_Carluoghi
048940051216
048950051216     c                   EndSr
048960051216
048970051216      *------------------------------------------------------------------------*
048980051216      * TASTO FUNZIONE F14 - INFO COMMERCIALI
048990051216      *------------------------------------------------------------------------*
049000051216     c     Sr_F14        BegSr
049010051216
049020081020     c                   Clear                   trmk50ds
049030081020     c                   Eval      i50pgm = 'TNTA60R'
049040081020     c                   Movel     v2ccpo        i50cpo
049050081020     c                   Eval      i50rag = wrag
049060110310     c                   IF        *in02 and not *in06 and not *in07
049070110310     c                   Eval      i50mod = 'G'
049080110310     c                   ELSE
049090110310     c                   Eval      i50mod = 'I'
049100110310     c                   ENDIF
049110081020     c                   Call      'TRMK50R'
049120051216     c                   Parm                    kpjba
049130081020     c                   Parm                    trmk50ds
049140051216
049150081020     c                   Eval      wtrmk50 = *On
049160051216     c                   EndSr
049170051216
049180051216      *------------------------------------------------------------------------*
049190051216      * TASTO FUNZIONE F15 -
049200051216      *------------------------------------------------------------------------*
049210051216     c     Sr_F15        BegSr
049220051216
049230051216     c                   Clear                   parmf15
049240051216     c                   Move      w0070         parcli1
049250051216     c                   Eval      kpjbu = parmf15
049260051216     c                   Call      'TNTA83R'
049270051216     c                   Parm                    kpjba
049280051216
049290051216     c                   EndSr
049300051216
049310051216      *------------------------------------------------------------------------*
049320051216      * TASTO FUNZIONE F18 -
049330051216      *------------------------------------------------------------------------*
049340051216     c     Sr_F18        BegSr
049350100406
049360100406      /free
049370100806       //?Richiamo gestione note
049380100406          clear trmk20ds;
049390110511          IMK20tla  = 'L';
049400100406
049410100406         //?Se interrogazione richiamo in interrogazione
049420100406          IF  *in05;
049430100406            IMK20flm = 'I';
049440100406          ENDIF;
049450100406
049460101112          IMK20cpo = v2ccpo;
049470100806          IMK20ksc = v1cksc;
049480100406          IMK20rsc = v1crag;
049490100406
049500100406          trmk20r (kpjba : TRMK20DS);
049510100406
049520100406      /end-free
049530051216
049540051216     c                   EndSr
049550060526      *------------------------------------------------------------------------*
049560060526      * TASTO FUNZIONE F19 - Variazioni
049570060526      *------------------------------------------------------------------------*
049580060526     c     Sr_F19        BegSr
049590060526
049600060526     c                   Clear                   tnta73ds
049610060526     c                   Eval      i73kcc=acokcc
049620060526     c                   Eval      i73ksc=acoksc
049630060529     c                   Movel     v1crag        i73rag
049640060526     c                   movel     tnta73ds      kpjbu
049650060526     c                   Call      'TNTA73R'
049660060526     c                   Parm                    kpjba
049670060526     c                   EndSr
049680100407
049690100407      *------------------------------------------------------------------------*
049700100407      * TASTO FUNZIONE F20 - INTERROGAZIONE CLIENTI POTENZIALI
049710100407      *------------------------------------------------------------------------*
049720100407     c     Sr_F20        BegSr
049730100407
049740100407      /free
049750100806       //?Richiamo gestione potenziali
049760100407          clear trmk01ds;
049770100407
049780100407         //?Se non c'è il codice potenziale vado in ricerca
049790100407          IF  v2ccpo = 0;
049800100407            MK01k11  = '1';
049810100407            MK01rag  = w005a;
049820100407            kpjbu    = TRMK01DS;
049830100407            trmk01r (kpjba);
049840100407            TRMK01DS = kpjbu;
049850100407          ELSE;
049860100407         //?Se c'è il codice potenziale vado in gestione
049870100407            MK01k10 = 'N';
049880100407            MK01cpo = v2ccpo;
049890100407            trmk02r (kpjba : TRMK01DS);
049900100407          ENDIF;
049910100407
049920100407          IF MK01cpo > 0 and MK01cpo <> v2ccpo;
049930100407            v2ccpo = MK01cpo;
049940100407            v2dcpo = MK01rag;
049950100407            exsr Sr_Ctrcpo;
049960100407          ENDIF;
049970100407
049980100407      /end-free
049990100407
050000100407     c                   EndSr
050010051216
050020051216      *------------------------------------------------------------------------*
050030051216      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
050040051216      *------------------------------------------------------------------------*
050050051216     c     Sr_F24        begsr
050060051216
050070051216     c                   Select
050080051216
050090051216     c                   When      $loop = 2 and $rig =2  or
050100051216     c                             $loop = 3 and $rig =3
050110051216     c                   Movea     rigatf1       vzfd01
050120051216     c                   Z-add     1             $rig
050130051216
050140051216     c                   If        $loop = 2
050150051216     c                   Eval      vzfd02= cf2412
050160051216     c                   Else
050170051216     c                   Eval      vzfd02= cf2413
050180051216     c                   EndIf
050190051216
050200051216     c                   When      $loop = 2 and $rig =1 or
050210051216     c                             $loop = 3 and $rig =1
050220051216     c                   Movea     rigatf2       vzfd01
050230051216     c                   Z-add     2             $rig
050240051216
050250051216     c                   If        $loop = 2
050260051216     c                   Eval      vzfd02 = cf2422
050270051216     c                   Else
050280051216     c                   Eval      vzfd02 = cf2423
050290051216     c                   EndIf
050300051216
050310051216     c                   When      $loop = 3 and $rig =2
050320051216     c                   Movea     rigatf3       vzfd01
050330051216     c                   Z-add     3             $rig
050340051216     c                   Eval      vzfd02 = cf2433
050350051216     c                   EndSl
050360051216
050370051216     c                   EndSr
050380051216
050390051216      *------------------------------------------------------------------------*
050400051216      * CONFERMO I DATI
050410051216      *------------------------------------------------------------------------*
050420051216     c     Sr_Conferma   BegSr
050430051220
050440051220     c                   Eval      wokimm = *Off
050450051216
050460051216      * In base alla videata emessa faccio i controlli che servono per
050470051216      * poter confermare i dati del video
050480051216
050490051216     c                   Select
050500051216
050510051216      * F6 dato in seconda videata
050520051216     c                   When      wvideo = '02'
050530051216     c                   ExSr      Sr_Ctrd02
050540051216     c   28              Leavesr
050550051216     c                   ExSr      Sr_Ctrd03
050560051216     c  n28              ExSr      Sr_Ctrd04
050570051216     c  n28              ExSr      Sr_Ctrd05
050580051216     c  n28              ExSr      Sr_Ctrd06
050590051216     c  n28              ExSr      Sr_Ctrd07
050600051216     c   28              Eval      v2cmsg = msg(69)
050610150424     c   28              eval      wForzaPag = *off
050620150728     c   28              eval      wForzaMail = *off
050630051216     c   28              Leavesr
050640051216
050650051216      * F6 dato in terza videata
050660051216     c                   When      wvideo = '03'
050670051216     c                   ExSr      Sr_Ctrd03
050680051216     c   28              Leavesr
050690051216     c                   ExSr      Sr_Ctrd04
050700051216     c  n28              ExSr      Sr_Ctrd05
050710051216     c  n28              ExSr      Sr_Ctrd06
050720051216     c  n28              ExSr      Sr_Ctrd07
050730051216     c   28              Eval      v3cmsg = msg(69)
050740150424     c   28              eval      wForzaPag = *off
050750150728     c   28              eval      wForzaMail = *off
050760051216     c   28              Leavesr
050770051216
050780051216      * F6 dato in quarta videata
050790051216     c                   When      wvideo = '04'
050800051216     c                   ExSr      Sr_Ctrd04
050810051216     c   28              Leavesr
050820051216     c                   ExSr      Sr_Ctrd05
050830051216     c  n28              ExSr      Sr_Ctrd06
050840051216     c  n28              ExSr      Sr_Ctrd07
050850051216     c   28              Eval      v4cmsg = msg(69)
050860150424     c   28              eval      wForzaPag = *off
050870150728     c   28              eval      wForzaMail = *off
050880051216     c   28              Leavesr
050890051216
050900051216      * F6 dato in quinta videata
050910051216     c                   When      wvideo = '05'
050920051216     c                   ExSr      Sr_Ctrd05
050930051216     c   28              Leavesr
050940051216     c                   ExSr      Sr_Ctrd06
050950051216     c  n28              ExSr      Sr_Ctrd07
050960051216     c   28              Eval      v5cmsg = msg(69)
050970150728     c   28              eval      wForzaMail = *off
050980051216     c   28              Leavesr
050990051216
051000051216      * F6 dato in sesta videata
051010051216     c                   When      wvideo = '06'
051020051216     c                   ExSr      Sr_Ctrd06
051030051216     c   28              Leavesr
051040051216     c                   ExSr      Sr_Ctrd07
051050051216     c   28              Eval      v6cmsg = msg(69)
051060150728     c   28              eval      wForzaMail = *off
051070051216     c   28              Leavesr
051080051216
051090051216      * F6 dato in settima videata
051100051216     c                   When      wvideo = '07'
051110051216     c                   ExSr      Sr_Ctrd07
051120051216     c   28              Leavesr
051130051216
051140051216     c                   EndSl
051150051216
051160051216      * Se arrivo a questo punto vuol dire che tutte le videate sono a posto
051170051220
051180051220      * Recupero il flag di procedura ORM attiva
051190060731     c                   Movel     v1cksc        w0030
051200051220     c                   Clear                   og148
051210061012     c                   Clear                   og143
051220051220     c     w0030         Chain     Azorg01l
051230051220     c                   If        %Found(Azorg01l)
051240051220     c                   Eval      og148 = orgde8
051250061012     c                   Eval      og143 = orgde3
051260051220     c                   EndIf
051270160905
051280160905      * se cliente logistica no anagrafica provvisoria
051290160905     c                   IF        ClienteLog
051300160905     c                   goto      NoImmACR
051310160905     c                   ENDIF
051320051220
051330051220      * Se non sono in anagrafica provvisoria, la procedura ORM è attiva e
051340051220      * provengo da convalida offerte
051350120313if  1c                   If        Not *In06 and *In07 and §ogorm <> *blanks
051360051220      * Controllo se devo creare l'anagrafica clienti ritiro
051370051220     c                   Eval      wokimm = *On
051380060731     c                   Movel     v1cksc        w0100
051390051220     c     w0100         Setll     Fnacr01l
051400051220do  2c                   Do        *Hival
051410051220     c                   Read      Fnacr01l
051420051220      * Fine file esco
051430051220     c                   If        %Eof(Fnacr01l)
051440051220     c                   Leave
051450051220     c                   EndIf
051460051220     c                   Movel     acrcro        w0070
051470051220      * Codice diverso esco
051480060731     c                   If        w0070 > v1cksc
051490051220     c                   Leave
051500051220     c                   EndIf
051510051220      * Se trovo non devo inserire l'anagrafico clienti ritiro
051520060731     c                   If        w0070 = v1cksc
051530051220     c                   Eval      wokimm = *Off
051540051220     c                   Leave
051550051220     c                   EndIf
051560051220    2c                   EndDo
051570051220
051580051220      * Se il cliente è già sull'anagrafico clienti ritiro emetto la videata
051590051220      * con un msg info
051600051221     c                   Eval      *In29 = *Off
051610051220if  2c                   If        wokimm = *Off and wforza = *Off
051620051220     c                   Eval      *In28 = *On
051630051220     c                   Eval      wforza = *On
051640051220     c                   Select
051650051220     c                   When      wvideo = '02'
051660051220     c                   Eval      v2cmsg = msg(72)
051670051221     c                   Eval      *In29 = *On
051680051220     c                   Goto      emi02
051690051220     c                   When      wvideo = '03'
051700051220     c                   Eval      v3cmsg = msg(72)
051710051220     c                   Goto      emi03
051720051220     c                   When      wvideo = '04'
051730051220     c                   Eval      v4cmsg = msg(72)
051740051220     c                   Goto      emi04
051750051220     c                   When      wvideo = '05'
051760051220     c                   Eval      v5cmsg = msg(72)
051770051220     c                   Goto      emi05
051780051220     c                   When      wvideo = '06'
051790051220     c                   Eval      v6cmsg = msg(72)
051800051220     c                   Goto      emi06
051810051220     c                   When      wvideo = '07'
051820051220     c                   Eval      v7cmsg = msg(72)
051830051220     c                   Goto      emi07
051840051220     c                   EndSl
051850051220    2c                   EndIf
051860051220    1c                   EndIf
051870160905
051880160905     c     NoImmACR      tag
051890051220
051900150424      * aggiorno la nota "DC"
051910091119     c                   if        v2ntcdc <> savntcdc
051920091119     c                   ExSr      Sr_AggntcDC
051930091119     c                   endif
051940051220
051950051220      * Quindi posso passare i dati dal video ai vari file e aggiornare
051960051216
051970051216      * Immissione pulisco i rcd file e imposto la chiave
051980051219if  1c                   If        *In01
051990051216     c  n06              Clear                   Cnaco000
052000051216     c   06              Clear                   Tfaco000
052010051216     c  n06              Clear                   Cnind000
052020051216     c   06              Clear                   Tfind000
052030051216     c  n06              Clear                   Cnclp000
052040051216     c   06              Clear                   Tfclp000
052050051216     c  n06              Clear                   Fncls000
052060051216     c   06              Clear                   Tfcls000
052070051220      * Chiave ACO
052080051216     c                   Eval      acokut = codut
052090051216     c                   Eval      acokcc = dutkci
052100051222     c  n06              Eval      acoksc = v1cksc
052110051222     c   06              Eval      acoksc = v1cnrv
052120051220      * Chiave IND
052130051216     c                   Eval      indkut = acokut
052140051216     c                   Eval      indkcc = acokcc
052150051216     c                   Eval      indksc = acoksc
052160051220      * Chiave CLP
052170051216     c                   Eval      clpkut = acokut
052180051216     c                   Eval      clpkcc = acokcc
052190051216     c                   Eval      clpksc = acoksc
052200051220      * Chiave CLS
052210051216     c                   Eval      clsksc = acoksc
052220051219      * Campi non visualizzati solo di immissione (non vengono aggiornati)
052230051219     c                   Eval      acoopz = v2copz
052240051219     c                   Eval      acotic = v2ctic
052250051219     c                   Eval      indlin = v2clin
052260051219      * Codice filiale di trasmissione sempre uguale ai primi 3 byte del codice
052270051219      * cliente
052280060330     c                   Movel     acoksc        acoflt
052290051219      * Flag tipo trasmissione sempre a '3' xchè se sono in immissione vuol dire
052300051219      * che sto immettendo un cliente o un'anagrafica provvisoria
052310051219     c                   Eval      acoftt = '3'
052320051219      * Data prima spedizione fattura (solo immissione xchè la imposto io
052330051219      * nel caso di cliente non nuovo
052340051219     c                   Z-add     v4cdps        datagma
052350051219     c                   Eval      gg1 = gg2
052360051219     c                   Eval      mm1 = mm2
052370051219     c                   Eval      aa1 = aa2
052380051219     c                   Z-add     dataamg       clpdps
052390051219      * Cliente da sollecitare
052400090616     c                   Movel     v2csol        clpsol
052410051219    1c                   EndIf
052420051216
052430051219      * Campi che vengono aggiornati a video
052440051219      * ACO
052450051219     c                   Eval      atb02 = v2tb02
052460051219     c                   Eval      acorag = v2crag
052470051219     c                   If        v6cdmg = 'SI'
052480051219     c                   Clear                   acorx1
052490051219     c                   Else
052500051219     c                   Eval      acorx1 = 1
052510051219     c                   EndIf
052520060801     c                   If        v6ccag = 'SI'
052530060801     c                   Clear                   acory1
052540060801     c                   Else
052550060801     c                   Eval      acory1 = 1
052560060801     c                   EndIf
052570051219     c                   Z-add     *date         acoduv
052580130325     c   79              eval      ACOabl = V2Cabledp
052590130325     c  n79              eval      ACOabl = V2Cabl
052600051219     c                   Eval      acolib = v2ccpo
052610051219     c                   Move      v3citc        acoitc
052620051216     c                   Clear                   acodtr
052630051219     c                   Clear                   acoftr
052640051219      * IND
052650051216     c                   Eval      indvia = v2cvia
052660051219     c                   Eval      indcit = v2ccit
052670051216      * Se Cap numerico e non supera i 5 caratteri scrivo anche indcap
052680051216if  1c                   If        %subst(v2ccae:6:1) = *Blanks
052690051216     c                   Eval      wokcap = *On
052700051216     c                   Eval      xx = 1
052710051216     c                   For       xx by 1 to 5
052720051216     c                   If        %subst(v2ccae:(xx):1) < *zeros
052730051216     c                   Eval      wokcap = *Off
052740051216     c                   Leave
052750051216     c                   EndIf
052760051216     c                   EndFor
052770051216     c                   If        wokcap = *On
052780060208     c                   Movel     v2ccae        indcap
052790051216     c                   EndIf
052800051216    1c                   EndIf
052810051216     c                   Eval      indprv = v2cprv
052820051216     c                   Eval      indsta = v2csta
052830051216     c                   Eval      indtel = v2ctel
052840051219     c                   Eval      indcdf = v2ccdf
052850051219     c                   If        v2ivaeu = *Blanks
052860051219     c                   Movel     v2ivait       indiva
052870051219     c                   Else
052880051219     c                   Movel     v2civa        indiva
052890051219     c                   EndIf
052900051219     c                   Eval      indcdp = *all'0'
052910100727     c                   Move      v4ccdp        indcdp
052920051219     c                   Eval      %subst(indopz:1:1) = wbloc
052930100727     c                   Eval      indabi = v4cabi
052940100727     c                   Eval      indcab = v4ccab
052950051216     c                   Eval      indtlf = v2ctlf
052960051219     c                   Move      v4csin        indsin
052970051219     c                   Eval      indcae = v2ccae
052980051219      * CLP
052990100127     c                   Eval      clpscf = %dec(v4cscf:7:0)
053000051219     c                   Move      v4cfft        clpfft
053010051219     c                   Move      v4ctft        clptft
053020051219     c                   Clear                   clpfun
053030051219     c                   Eval      %subst(clpfun:1:1) = v4ctdf
053040051219     c                   Move      v3cage        clpage
053050051219     c                   Clear                   clpcon
053060051219     c                   Eval      %subst(clpcon:2:2) = v2ccon
053070051219     c                   Eval      clpnar = v2cblc
053080051219     c                   Eval      clpclv = v3cclv
053090080422     c                   eval      clpbab = v5ciban
053100080213      * se IBAN non impostato a video
053110080213      * pulisco i dati relativi a abi/cab e c/c
053120080213     c                   if        v5ciban = *blanks
053130090331      * ma solo se ABI NON è > 90000
053140090331     c                   if        clpabi < 90000
053150080213     c                   clear                   clpccb
053160080213     c                   clear                   clpabi
053170080213     c                   clear                   clpcab
053180090331     c                   endif
053190080213     c                   else
053200080213      * se IBAN italia memorizzo abi/cab e c/c
053210080213     c                   if        %subst(v5ciban:1:2) = 'IT' or
053220080213     c                             %subst(v5ciban:1:2) = 'SM'
053230080115     c                   eval      %subst(clpccb:1:12) =
053240080115     c                             %subst(v5ciban:16:12)
053250080115     c                   eval      %subst(clpccb:16:1) =
053260080115     c                             %subst(v5ciban:5:1)
053270080115     c                   eval      clpabi = %dec(%subst(v5ciban:6:5):5:0)
053280080115     c                   eval      clpcab = %dec(%subst(v5ciban:11:5):5:0)
053290080213     c                   endif
053300080213     c                   endif
053310080422     c                   Eval      clpfpc = v5cfpc
053320090617      * Cliente da sollecitare
053330090617     c                   Movel     v2csol        clpsol
053340051219      * CLS
053350051219     c                   Eval      clsstp = v7cstp
053360100729     c                   eval      %subst(CLStic:1:1) = v5tpdn
053370100729     c                   eval      %subst(CLStic:2:1) = v5tpna
053380051219     c                   Eval      %subst(clsflo:1:1) = v6ctcf
053390051219     c                   Eval      %subst(clsflo:2:1) = v4cuni
053400121105     c                   eval      %subst(CLSflo:3:1) = V4Ctpf
053410090616     c                   Eval      %subst(clsflo:4:1) = v2fsol
053420051219     c                   Eval      %subst(clsflo:5:1) = v7csdn
053430051219     c                   Eval      %subst(clsflo:6:1) = v4cstm
053440051219     c                   Eval      %subst(clsflo:7:1) = v7csin
053450051219     c                   Eval      %subst(clsflo:8:1) = v7ctpa
053460051219     c                   Eval      %subst(clsflo:9:1) = v6capg
053470051219     c                   Eval      %subst(clsflo:10:1) = v6ctcm
053480051219
053490051219      * Immissione
053500051219     c                   If        *In01
053510051219     c  n06              Write     Fncls000
053520051219     c   06              Write     Tfcls000
053530051219     c  n06              Write     Cnclp000
053540051219     c   06              Write     Tfclp000
053550051219     c  n06              Write     Cnind000
053560051219     c   06              Write     Tfind000
053570051219     c  n06              Write     Cnaco000
053580051219     c   06              Write     Tfaco000
053590060526     c* Scrivo file variazioni
053600060526     c  n06              exsr      ScriviACV
053610060526     c                   EndIf
053620060526     c
053630051219      * Manutenzione
053640051219     c                   If        *In02
053650051219     c  n06              Update    Fncls000
053660051219     c   06              Update    Tfcls000
053670051219     c  n06              Update    Cnclp000
053680051219     c   06              Update    Tfclp000
053690051219     c  n06              Update    Cnind000
053700051219     c   06              Update    Tfind000
053710051219     c  n06              Update    Cnaco000
053720051219     c   06              Update    Tfaco000
053730060526     c*
053740060526     c* Scrivo file variazioni
053750060526     c  n06              exsr      ScriviACV
053760051219     c                   EndIf
053770110927
053780110927      * Aggiorno il potenziale
053790160906      * solo se NON sono utente di sede
053800110927     c                   If        v2ccpo > *Zeros
053810160906     c                             and not *in09
053820110927     c                   ExSr      Sr_Aggcpo
053830110927     c                   EndIf
053840100729
053850100729      /free
053860100729       //?Memorizzo le coordinate bancarie
053870100730       //?Pagamento DANNI --> DN
053880100730         clear rqsOpCode;
053890100729         wpag = 'DN';
053900100729         wiban = v5ibandn;
053910100730         wbic  = v5bicdn;
053920100805         wcodpo = savtipdn;
053930100805         wcodpn = v5tpdn;
053940100805         wpgm  = 'TNTA60R';
053950100730         SELECT;
053960100730           WHEN  v5ibandn <> *blanks and savibandn = *blanks;
053970100805             rqsOpCode = 'INSERT';
053980100805           WHEN  v5ibandn = *blanks and savibandn <> *blanks;
053990100805             rqsOpCode = 'DELETE';
054000100805           OTHER;
054010100805             rqsOpCode = 'UPDATE';
054020100730         ENDSL;
054030100730         IF  rqsOpCode <> *blanks;
054040100730           exsr Coordinate;
054050100730         ENDIF;
054060100729       //?Non faccio controlli sul codice di ritorno
054070100729       //?ormai l'anagrafica è scritta
054080100729
054090100729       //?Pagamento NOTE ACCREDITO --> NA
054100100730         clear rqsOpCode;
054110100729         wpag = 'NA';
054120100729         wiban = v5ibanna;
054130100730         wbic  = v5bicna;
054140100805         wcodpo = savtipna;
054150100805         wcodpn = v5tpna;
054160100805         wpgm  = 'TNTA60R';
054170100730         SELECT;
054180100730           WHEN  v5ibanna <> *blanks and savibanna = *blanks;
054190100805             rqsOpCode = 'INSERT';
054200100805           WHEN  v5ibanna = *blanks and savibanna <> *blanks;
054210100805             rqsOpCode = 'DELETE';
054220100805           OTHER;
054230100805             rqsOpCode = 'UPDATE';
054240100730         ENDSL;
054250100730         IF  rqsOpCode <> *blanks;
054260100730           exsr Coordinate;
054270100729         ENDIF;
054280100729       //?Non faccio controlli sul codice di ritorno
054290100729       //?ormai l'anagrafica è scritta
054300110223
054310110223       //?Se NON è anagrafica provvisoria:
054320110223       //?il cliente è stato bloccato
054330110223       //?e il potenziale NON è in categoria PERSO
054340110310       //?oppure
054350110310       //?il cliente NON è più bloccato
054360110310       //?e il potenziale è in categoria PERSO
054370110223       //?oppure
054380110223       //?arrivo da convalida
054390110223       //?e il potenziale NON è in categoria CODIFICATO
054400110223         IF  not *in06 and
054410110223             ((ACOabl <> *blanks and
054420110223               savabl = *blanks and V2Hfls <> 'P') or
054430110310              (ACOabl = *blanks and
054440110310               savabl <> *blanks and V2Hfls = 'P') or
054450110223              (*in07 and V2Hfls <> 'C'));
054460110223       //?richiamo pgm di calcolo categoria potenziale
054470110223           clear trmk05ds;
054480110223           IMK05cpo = ACOlib;
054490110223           trmk05r (kpjba : TRMK05DS);
054500110223         //?se categoria variata aggiorno potenziale
054510110304           IF  OMK05err = *blanks and OMK05cat <> V2Hfls;
054520110223             chain(e) ACOlib TNCPO01L;
054530110223             IF  not %error and %found(TNCPO01L);
054540110223               CPOfls = OMK05cat;
054550110223               update TNCPO000;
054560110223             ENDIF;
054570110223           ENDIF;
054580110223         ENDIF;
054590150424
054600150424       //?Devo spalmare le coordinate bancarie e i cod.pagamenti
054610150424         WinInfo = *off;
054620150424         EoFW06 = *off;
054630150424         SELECT;
054640150424       //?spalmo da pag. NA a CA e DN
054650150424         WHEN  BonificoNA and
054660150424               (not BonificoCA or not BonificoDN);
054670150424           exsr SpalmaNA;
054680150424         WHEN  BonificoDN and
054690150424               (not BonificoCA or not BonificoNA);
054700150424           exsr SpalmaDN;
054710150424         WHEN  BonificoCA and
054720150424               (not BonificoDN or not BonificoNA);
054730150424           exsr SpalmaCA;
054740150424         ENDSL;
054750150424         IF  WinInfo;
054760150424           DOW not EoFW06;
054770150424             exfmt TA60W06;
054780150424             IF  *inkf;
054790150424               EoFW06 = *on;
054800150424               leave;
054810150424             ENDIF;
054820150424           ENDDO;
054830150424         ENDIF;
054840150424
054850100729      /end-free
054860160905
054870160905      * Se cliente logistica no anagrfica clienti ritiro
054880160905     c                   IF        ClienteLog
054890160905     c                   goto      ByPassaAcr
054900160905     c                   ENDIF
054910051220
054920051220      * Se non sono in anagrafica provvisoria
054930120313if  1c                   If        Not *In06 and §ogorm <> *blanks
054940051220
054950051220      * Controllo il blocco/sblocco dei clienti per aggiornare anche
054960051220      * l'anagrafica clienti ritiro (solo in caso di blocco)
054970121121      * lo faccio solo se modifica perchè se è immissione non c'è
054980121121      * ancora l'anagrafica clienti ritiro
054990121121     c                   IF        *in02
055000051220     c                   ExSr      Sr_Ctrabl
055010121121     c                   ENDIF
055020051220
055030051220      * Richiamo l'immissione anagrafica clienti ritiro
055040051220if  2c                   If        *In01 or wokimm = *On
055050121121
055060121121      * NON lo faccio se immissione di codici 8888 - 9999 o incassi
055070121121      * da attribuire --> 0001 1° livello
055080121121      *               --> 1000 2° livello
055090160429      * immissione fatta da profilo EDP
055100121121     c                   SELECT
055110121121     c                   WHEN      wksc04 = 8888 or wksc04 = 9999
055120121121     c                   goto      noacr
055130160429     c                   WHEN       *in20 and
055140121121     c                             ((sav_livello = '1' and wksc04 = 0001) or
055150121121     c                              (sav_livello = '2' and wksc04 = 1000) or
055160121121     c                              (sav_livello = *blanks and
055170121121     c                               wCodIncAtt  = *blanks and
055180121121     c                              (wksc04 = 1000  or  wksc04 = 0001)))
055190121121     c                   goto      noacr
055200121121     c                   ENDSL
055210070809     c                   reset                   FIOR37ds
055220070810     c                   eval      I37opz  = '3'
055230070809     c                   movel     ACOksc        I37fgs
055240070809     c                   movel     ACOksc        I37cro
055250070809     c                   movel     ACOksc        I37ksc
055260070809     c                   movel(p)  FIOR37ds      KPJBU
055270070810     c                   call      'FIOR37R'
055280070809     c                   parm                    KPJBA
055290051220    2c                   EndIf
055300121121     c     noacr         tag
055310051220      * Richiamo la modifica dell'anagrafica clienti ritiro
055320060217if  2c                   If        *In02 and Not *In07
055330070809     c                   reset                   FIOR37ds
055340070809     c                   eval      I37opz  = 'A'
055350070809     c                   movel     ACOksc        I37fgs
055360070809     c                   movel     ACOksc        I37cro
055370070809     c                   movel(p)  FIOR37ds      KPJBU
055380070809     c                   call      'FIOR37R'
055390070809     c                   parm                    KPJBA
055400051220    2c                   EndIf
055410051216
055420051220    1c                   EndIf
055430160905
055440160905     c     ByPassaAcr    tag
055450160905
055460061012     c*
055470061012     c* se cliente di network dpd richiamo pgm gestione legami depot/cod.PdC
055480061012     c* per permettere l'aggiornamento del codice cliente sul file dei depot
055490061012     c                   if        not *in06 and §ogntw='DPD'
055500061012     c* imposta la ds di procedura
055510061012     c                   CLEAR                   tisie5ds
055520061012     c                   MOVE      '02'          de5op0
055530061012     c                   MOVEL     *blanks       de5op1
055540061012     c                   MOVEL     '0'           de5f03
055550061012     c                   MOVEL     '0'           de5f12
055560061012     c                   MOVEL     '0'           de5err
055570061012     c                   MOVEL     *blanks       de5msg
055580061012     c                   movel     'N'           de5sel
055590061012     c* lancia il programma
055600061012     c                   CALL      'TISIE5R'
055610061012     c                   PARM                    kpjba
055620061012     c                   PARM                    tisie5ds
055630061012     c                   endif
055640120510
055650120510      /free
055660120510       //?Se il cliente è stato bloccato devo richiamare il pgm delle INFO
055670120510       //?per obbligo di azzeramento della % affido a BRT
055680120510       //?Il richiamo lo faccio solo se:
055690120510       //?--> è appena stato immesso il blocco
055700120510       //?--> il pgm non è stato richiamto dal CMR x impostare il blocco
055710120510       //?--> c'è il codice del potenziale
055720120510         IF  not $Blocco and
055730120510             ACOabl <> *blanks and ACOabl <> savabl and
055740120510             ACOlib > 0;
055750120510           IF  not %open(CNACO16L);
055760120510             open CNACO16L;
055770120510           ENDIF;
055780120510         //?tutto questo solo se sto bloccando l'ultimo cliente della catena
055790120510         //?se ne ho ancora dei NON bloccati non devo azzerare
055800120510           setll ACOlib CNACO16L;
055810120510           reade ACOlib CNACO16L;
055820120510           DOW not %eof(CNACO16L);
055830120510             IF  ww_ACOabl = *blanks;
055840120510               leavesr;
055850120510             ENDIF;
055860120510             reade ACOlib CNACO16L;
055870120510           ENDDO;
055880120510         //?Aggancio le INFO commerciali
055890120510         //?se la % affido a BRT è = 0 non devo azzerare
055900120510           chain (ACOlib:'10 ':'_BAR') TICPI01L;
055910120510 1         IF  not %found(ticpi01l) or CPIval = 0;
055920120510             leavesr;
055930120510           ENDIF;
055940120510
055950120510         //?pgm INFO
055960120510           clear TRMK50DS;
055970120510           I50pgm = 'TNTA60R';
055980120510           I50cpo = ACOlib;
055990120510           I50rag = ACOrag;
056000120510           I50mod = 'G';
056010120510           I50atb = '0';
056020120510           TRMK50R (kpjba : TRMK50DS);
056030120510
056040120510           wTRMK50 = *on;
056050120510
056060120510         ENDIF;
056070120510
056080120510      /end-free
056090051220
056100051216     c                   EndSr
056110051220
056120060526      *------------------------------------------------------------------------*
056130060526      * Scrittura file variazioni
056140060526      *------------------------------------------------------------------------*
056150060526     c     ScriviACV     BegSr
056160060531     c                   Clear                   Tibs73ds
056170060531     c                   eval      ibs73pru=knmus
056180060531     c                   eval      ibs73noj=knmeb
056190060601     c                   eval      ibs73pgm='TNTA60R'
056200060531     c                   eval      ibs73immag='D'
056210060526     c* Immissione
056220060526     c                   if        *in01
056230060531     c                   eval      ibs73cta='I'
056240060531     c                   Call      'TIBS73R'
056250060531     c                   Parm                    Tibs73ds
056260060531     c                   Parm                    cnaco_73
056270060531     c                   Parm                    cnind_73
056280060531     c                   Parm                    cnclp_73
056290060531     c                   Parm                    fncls_73
056300060526     c                   endif
056310060526     c* Manutenzione
056320060526     C                   IF        *IN02
056330060531     c                   eval      ibs73cta='M'
056340060531     c                   Call      'TIBS73R'
056350060531     c                   Parm                    Tibs73ds
056360060531     c                   Parm                    cnaco_73
056370060531     c                   Parm                    cnind_73
056380060531     c                   Parm                    cnclp_73
056390060531     c                   Parm                    fncls_73
056400060531     c                   endif
056410060526     c
056420060531     c                   Eval      wtibs73 = *On
056430060526     c                   EndSr
056440150424
056450150424      /free
056460150424       //--------------------------------------------------------------
056470150427       //?Spalmo coordinate bancarie e pagamenti CA.
056480150424       //--------------------------------------------------------------
056490150424       BEGSR SpalmaCA;
056500150427
056510150512       //?Se il pagamento CA è estero
056520150427       //?spalmo solo il codice senza le coordinate
056530150427         IF  PagEsteroCA;
056540150427           clear CLPbab;
056550150427         ENDIF;
056560150427
056570150427       //?Anagrafica clienti Reale
056580150427         IF  not *in06;
056590150427       //?Aggancio FNCLS
056600150427           chain V1Cksc FNCLS01L;
056610150427           IF  not %found(FNCLS01L);
056620150427             leavesr;
056630150427           ENDIF;
056640150427       //?Spalmo pagamento e coordinate per DN
056650151112           IF  CodPagDN <> '1';
056660150427           IF  not BonificoDN;
056670150427         //?coordinate bancarie
056680150427             clear rqsOpCode;
056690150427             wpag = 'DN';
056700150427             wiban = CLPbab;
056710150427             wcodpo = %subst(CLStic:1:1);
056720150427             wcodpn = CLPfpc;
056730150427             wpgm  = 'TNTA60R';
056740150427             rqsOpCode = 'INSERT';
056750150427             exsr Coordinate;
056760150427             %subst(CLStic:1:1) = CLPfpc;
056770151112             WinInfo = *on;
056780150427           ENDIF;
056790151112           ENDIF;
056800150427       //?Spalmo pagamento e coordinate per NA
056810151112           IF  CodPagNA <> '1';
056820150427           IF  not BonificoNA;
056830150427         //?coordinate bancarie
056840150427             clear rqsOpCode;
056850150427             wpag = 'NA';
056860150427             wiban = CLPbab;
056870150427             wcodpo = %subst(CLStic:2:1);
056880150427             wcodpn = CLPfpc;
056890150427             wpgm  = 'TNTA60R';
056900150427             rqsOpCode = 'INSERT';
056910150427             exsr Coordinate;
056920150427             %subst(CLStic:2:1) = CLPfpc;
056930151112             WinInfo = *on;
056940150427           ENDIF;
056950151112           ENDIF;
056960150427       //?Aggiorno anagrafica
056970150427           update FNCLS000;
056980150427         ENDIF;
056990150427
057000150427       //?Anagrafica clienti Provvisoria
057010150427         IF  *in06;
057020150427       //?Aggancio TFCLS
057030150427           chain V1Cnrv TFCLS01L;
057040150427           IF  not %found(TFCLS01L);
057050150427             leavesr;
057060150427           ENDIF;
057070150427       //?Spalmo pagamento e coordinate per DN
057080151112           IF  CodPagDN <> '1';
057090150427           IF  not BonificoDN;
057100150427             clear rqsOpCode;
057110150427             wpag = 'DN';
057120150427             wiban = CLPbab;
057130150427             wcodpo = %subst(CLStic:1:1);
057140150427             wcodpn = CLPfpc;
057150150427             wpgm  = 'TNTA60R';
057160150427             rqsOpCode = 'INSERT';
057170150427             exsr Coordinate;
057180150427             %subst(CLStic:1:1) = CLPfpc;
057190151112             WinInfo = *on;
057200150427           ENDIF;
057210151112           ENDIF;
057220150427       //?Spalmo pagamento e coordinate per NA
057230151112           IF  CodPagNA <> '1';
057240150427           IF  not BonificoNA;
057250150427             clear rqsOpCode;
057260150427             wpag = 'NA';
057270150427             wiban = CLPbab;
057280150427             wcodpo = %subst(CLStic:2:1);
057290150427             wcodpn = CLPfpc;
057300150427             wpgm  = 'TNTA60R';
057310150427             rqsOpCode = 'INSERT';
057320150427             exsr Coordinate;
057330150427             %subst(CLStic:2:1) = CLPfpc;
057340151112             WinInfo = *on;
057350150427           ENDIF;
057360151112           ENDIF;
057370150427       //?Aggiorno anagrafica
057380150427           update TFCLS000;
057390150427         ENDIF;
057400150424
057410150424       ENDSR;
057420150424
057430150424       //--------------------------------------------------------------
057440150427       //?Spalmo coordinate bancarie e pagamenti DN.
057450150424       //--------------------------------------------------------------
057460150424       BEGSR SpalmaDN;
057470150427
057480150427         SELECT;
057490150427
057500150427       //?Se il pagamento DN è estero ed è
057510150427       //?Anagrafica clienti Reale
057520150427       //?spalmo solo il codice senza le coordinate
057530150427         WHEN  PagEsteroDN and not *in06;
057540150427       //?Memorizzo immagine precedente
057550150427           exsr ImmaginePrima;
057560150427       //?Spalmo pagamento per CA
057570151112           IF  CodPagCA <> '1';
057580150427           IF  not BonificoCA;
057590150427         //?Aggancio CNCLP
057600150427             chain (codut:V1Ckcc:V1Cksc) CNCLP00F;
057610150427             IF  not %found(CNCLP00F);
057620150427               leavesr;
057630150427             ENDIF;
057640150427             CLPfpc = %subst(CLStic:1:1);
057650150427             update CNCLP000;
057660151112             WinInfo = *on;
057670150427           ENDIF;
057680151112           ENDIF;
057690150427       //?Spalmo pagamento per NA
057700151112           IF  CodPagNA <> '1';
057710150427           IF  not BonificoNA;
057720150427         //?Aggancio FNCLS
057730150427             chain V1Cksc FNCLS01L;
057740150427             IF  not %found(FNCLS01L);
057750150427               leavesr;
057760150427             ENDIF;
057770150427             %subst(CLStic:2:1) = %subst(CLStic:1:1);
057780150427             update FNCLS000;
057790151112             WinInfo = *on;
057800150427           ENDIF;
057810151112           ENDIF;
057820150427       //?Immagine Dopo
057830150427           exsr ImmagineDopo;
057840150427
057850150427       //?Se il pagamento DN è estero ed è
057860150427       //?Anagrafica clienti Provvisoria
057870150427       //?spalmo solo il codice senza le coordinate
057880150427         WHEN  PagEsteroDN and *in06;
057890150427       //?Spalmo pagamento per CA
057900151112           IF  CodPagCA <> '1';
057910150427           IF  not BonificoCA;
057920150427         //?Aggancio TFCLP
057930150427             chain (codut:V1Ckcc:V1Cnrv) TFCLP00F;
057940150427             IF  not %found(TFCLP00F);
057950150427               leavesr;
057960150427             ENDIF;
057970150427             CLPfpc = %subst(CLStic:1:1);
057980150427             update TFCLP000;
057990151112             WinInfo = *on;
058000150427           ENDIF;
058010151112           ENDIF;
058020150427       //?Spalmo pagamento per NA
058030151112           IF  CodPagNA <> '1';
058040150427           IF  not BonificoNA;
058050150427         //?Aggancio TFCLS
058060150427             chain V1Cnrv TFCLS01L;
058070150427             IF  not %found(TFCLS01L);
058080150427               leavesr;
058090150427             ENDIF;
058100150427             %subst(CLStic:2:1) = %subst(CLStic:1:1);
058110150427             update TFCLS000;
058120151112             WinInfo = *on;
058130150427           ENDIF;
058140151112           ENDIF;
058150150427
058160150427         OTHER;
058170150427       //?Se arrivo qua il pagamento non è estero quindi spalmo tutto
058180150424       //?Recupero le coordinate bancarie DN
058190150427           wpag = 'DN';
058200150427           rqsOpCode = 'SEARCH';
058210150427           exsr Coordinate;
058220150424       //?Spalmo coordinate e pagamento per CA
058230151112           IF  CodPagCA <> '1';
058240150427           IF  not BonificoCA;
058250150424       //?Memorizzo immagine precedente
058260150427             IF  not *in06;
058270150427               exsr ImmaginePrima;
058280150427         //?Aggancio CNCLP
058290150427               chain (codut:V1Ckcc:V1Cksc) CNCLP00F;
058300150427               IF  not %found(CNCLP00F);
058310150427                 leavesr;
058320150427               ENDIF;
058330150427             ELSE;
058340150427         //?Aggancio TFCLP
058350150427               chain (codut:V1Ckcc:V1Cnrv) TFCLP00F;
058360150427               IF  not %found(TFCLP00F);
058370150427                 leavesr;
058380150427               ENDIF;
058390150427             ENDIF;
058400150427             CLPfpc = %subst(CLStic:1:1);
058410150427             IF  not *in06;
058420150427               CLPbab = TrulIbanO0.IBAN;
058430150427               CLPabi = TrulIbanO0.ABI;
058440150427               CLPcab = TrulIbanO0.CAB;
058450150427               update CNCLP000;
058460150427         //?Memorizzo immagine sucessiva
058470150427               exsr ImmagineDopo;
058480150427             ELSE;
058490150427               CLPbab = TrulIbanO1.IBAN;
058500150427               CLPabi = TrulIbanO1.ABI;
058510150427               CLPcab = TrulIbanO1.CAB;
058520150427               update TFCLP000;
058530150427             ENDIF;
058540151112             WinInfo = *on;
058550150427           ENDIF;
058560151112           ENDIF;
058570150424
058580150424       //?Spalmo coordinate e pagamento per NA
058590151112           IF  CodPagNA <> '1';
058600150427           IF  not BonificoNA;
058610150427             clear rqsOpCode;
058620150427             wpag = 'NA';
058630150427         //?Aggancio FNCLS
058640150427             IF  not *in06;
058650150427               chain V1Cksc FNCLS01L;
058660150427               IF  not %found(FNCLS01L);
058670150427                 leavesr;
058680150427               ENDIF;
058690150427               wiban = TrulIbanO0.IBAN;
058700150427               wbic  = TrulIbanO0.BIC;
058710150427               wcodpo = %subst(CLStic:2:1);
058720150427               wcodpn = %subst(CLStic:1:1);
058730150427             ELSE;
058740150427         //?Aggancio TFCLS
058750150427               chain V1Cnrv TFCLS01L;
058760150427               IF  not %found(TFCLS01L);
058770150427                 leavesr;
058780150427               ENDIF;
058790150427               wiban = TrulIbanO1.IBAN;
058800150427               wbic  = TrulIbanO1.BIC;
058810150427               wcodpo = %subst(CLStic:2:1);
058820150427               wcodpn = %subst(CLStic:1:1);
058830150427             ENDIF;
058840150427             wpgm  = 'TNTA60R';
058850150427             rqsOpCode = 'INSERT';
058860150427             exsr Coordinate;
058870150427             %subst(CLStic:2:1) = %subst(CLStic:1:1);
058880150427         //?Aggiorno anagrafica
058890150427             IF  not *in06;
058900150427               update FNCLS000;
058910150427             ELSE;
058920150427               update TFCLS000;
058930150427             ENDIF;
058940151112             WinInfo = *on;
058950150427           ENDIF;
058960151112           ENDIF;
058970150427         ENDSL;
058980150424
058990150424       ENDSR;
059000150424
059010150424       //--------------------------------------------------------------
059020150427       //?Spalmo coordinate bancarie e pagamenti NA.
059030150424       //--------------------------------------------------------------
059040150424       BEGSR SpalmaNA;
059050150427
059060150427         SELECT;
059070150427
059080150427       //?Se il pagamento NA è estero ed è
059090150427       //?Anagrafica clienti Reale
059100150427       //?spalmo solo il codice senza le coordinate
059110150427         WHEN  PagEsteroNA and not *in06;
059120150427       //?Memorizzo immagine precedente
059130150427           exsr ImmaginePrima;
059140150427       //?Spalmo pagamento per CA
059150151112           IF  CodPagCA <> '1';
059160150427           IF  not BonificoCA;
059170150427         //?Aggancio CNCLP
059180150427             chain (codut:V1Ckcc:V1Cksc) CNCLP00F;
059190150427             IF  not %found(CNCLP00F);
059200150427               leavesr;
059210150427             ENDIF;
059220150427             CLPfpc = %subst(CLStic:2:1);
059230150427             update CNCLP000;
059240151112             WinInfo = *on;
059250150427           ENDIF;
059260151112           ENDIF;
059270150427       //?Spalmo pagamento per DN
059280151112           IF  CodPagDN <> '1';
059290150427           IF  not BonificoDN;
059300150427         //?Aggancio FNCLS
059310150427             chain V1Cksc FNCLS01L;
059320150427             IF  not %found(FNCLS01L);
059330150427               leavesr;
059340150427             ENDIF;
059350150427             %subst(CLStic:1:1) = %subst(CLStic:2:1);
059360150427             update FNCLS000;
059370151112             WinInfo = *on;
059380150427           ENDIF;
059390151112           ENDIF;
059400150427       //?Immagine Dopo
059410150427           exsr ImmagineDopo;
059420150427
059430150427       //?Se il pagamento NA è estero ed è
059440150427       //?Anagrafica clienti Provvisoria
059450150427       //?spalmo solo il codice senza le coordinate
059460150427         WHEN  PagEsteroNA and *in06;
059470150427       //?Spalmo pagamento per CA
059480151112           IF  CodPagCA <> '1';
059490150427           IF  not BonificoCA;
059500150427         //?Aggancio TFCLP
059510150427             chain (codut:V1Ckcc:V1Cnrv) TFCLP00F;
059520150427             IF  not %found(TFCLP00F);
059530150427               leavesr;
059540150427             ENDIF;
059550150427             CLPfpc = %subst(CLStic:2:1);
059560150427             update TFCLP000;
059570151112             WinInfo = *on;
059580150427           ENDIF;
059590151112           ENDIF;
059600150427       //?Spalmo pagamento per DN
059610151112           IF  CodPagDN <> '1';
059620150427           IF  not BonificoDN;
059630150427         //?Aggancio TFCLS
059640150427             chain V1Cnrv TFCLS01L;
059650150427             IF  not %found(TFCLS01L);
059660150427               leavesr;
059670150427             ENDIF;
059680150427             %subst(CLStic:1:1) = %subst(CLStic:2:1);
059690150427             update TFCLS000;
059700151112             WinInfo = *on;
059710150427           ENDIF;
059720151112           ENDIF;
059730150427
059740150427         OTHER;
059750150427       //?Se arrivo qua il pagamento non è estero quindi spalmo tutto
059760150424       //?Recupero le coordinate bancarie NA
059770150427           wpag = 'NA';
059780150427           rqsOpCode = 'SEARCH';
059790150427           exsr Coordinate;
059800150424       //?Spalmo coordinate e pagamento per CA
059810151112           IF  CodPagCA <> '1';
059820150427           IF  not BonificoCA;
059830150424       //?Memorizzo immagine precedente
059840150427             IF  not *in06;
059850150427               exsr ImmaginePrima;
059860150424       //?Aggancio CNCLP
059870150427               chain (codut:V1Ckcc:V1Cksc) CNCLP00F;
059880150427               IF  not %found(CNCLP00F);
059890150427                 leavesr;
059900150427               ENDIF;
059910150427             ELSE;
059920150427       //?Aggancio TFCLP
059930150427               chain (codut:V1Ckcc:V1Cnrv) TFCLP00F;
059940150427               IF  not %found(TFCLP00F);
059950150427                 leavesr;
059960150427               ENDIF;
059970150427             ENDIF;
059980150427             CLPfpc = %subst(CLStic:2:1);
059990150427             IF  not *in06;
060000150427               CLPbab = TrulIbanO0.IBAN;
060010150427               CLPabi = TrulIbanO0.ABI;
060020150427               CLPcab = TrulIbanO0.CAB;
060030150427               update CNCLP000;
060040150427         //?Memorizzo immagine sucessiva
060050150427               exsr ImmagineDopo;
060060150427             ELSE;
060070150427               CLPbab = TrulIbanO1.IBAN;
060080150427               CLPabi = TrulIbanO1.ABI;
060090150427               CLPcab = TrulIbanO1.CAB;
060100150427               update TFCLP000;
060110150427             ENDIF;
060120151112             WinInfo = *on;
060130150427           ENDIF;
060140151112           ENDIF;
060150150424
060160150424       //?Spalmo coordinate e pagamento per DN
060170151112           IF  CodPagDN <> '1';
060180150427           IF  not BonificoDN;
060190150427             clear rqsOpCode;
060200150427             wpag = 'DN';
060210150427         //?Aggancio FNCLS
060220150427             IF  not *in06;
060230150427               chain V1Cksc FNCLS01L;
060240150427               IF  not %found(FNCLS01L);
060250150427                 leavesr;
060260150427               ENDIF;
060270150427               wiban = TrulIbanO0.IBAN;
060280150427               wbic  = TrulIbanO0.BIC;
060290150427               wcodpo = %subst(CLStic:1:1);
060300150427               wcodpn = %subst(CLStic:2:1);
060310150427             ELSE;
060320150427         //?Aggancio TFCLS
060330150427               chain V1Cnrv TFCLS01L;
060340150427               IF  not %found(TFCLS01L);
060350150427                 leavesr;
060360150427               ENDIF;
060370150427               wiban = TrulIbanO1.IBAN;
060380150427               wbic  = TrulIbanO1.BIC;
060390150427               wcodpo = %subst(CLStic:1:1);
060400150427               wcodpn = %subst(CLStic:2:1);
060410150427             ENDIF;
060420150427             wpgm  = 'TNTA60R';
060430150427             rqsOpCode = 'INSERT';
060440150427             exsr Coordinate;
060450150427             %subst(CLStic:1:1) = %subst(CLStic:2:1);
060460150427         //?Aggiorno anagrafica
060470150427             IF  not *in06;
060480150427               update FNCLS000;
060490150427             ELSE;
060500150427               update TFCLS000;
060510150427             ENDIF;
060520151112             WinInfo = *on;
060530150427           ENDIF;
060540151112           ENDIF;
060550150424
060560150427         ENDSL;
060570150424
060580150424       ENDSR;
060590150424      /end-free
060600150424
060610051220      *------------------------------------------------------------------------*
060620051220      * AGGIORNAMENTO DEL CODICE POTENZIALE
060630051220      *------------------------------------------------------------------------*
060640051220     c     Sr_Aggcpo     BegSr
060650100407
060660100407      /free
060670100806       //?Se variato codice potenziale
060680110928       //?o se immissione di nuovo codice
060690110928        IF  (v2ccpo <> savcpo and savcpo > 0) or *in01;
060700100407         //?richiamo programma di comodo per sistemare potenziale
060710100407         //?sui vari archivi (trattativa, anagrafica provvisoria, attività)
060720100407         //?nel caso di trattativa aperta
060730110223         //?in ogni caso ricalcola e aggiorna sempre la categoria
060740110223         //?del vecchio e nuovo potenziale
060750100407          clear TRMK28DS;
060760100407          IMK28cpo   = v2ccpo;
060770100407          IMK28cpoex = savcpo;
060780100407          IMK28ksc   = v1cksc;
060790100407          trmk28r (TRMK28DS);
060800100407        ENDIF;
060810121105
060820121105       //?Se Immissione di nuovo cliente (no anagrafica provvisoria)
060830121105       //?devo aggiornare la data primo contatto sul Potenziale
060840121105        IF  *in01 and not *in06 and V2Ccpo > 0 and
060850121105           (§CPOdtpcon = *blanks or §CPOdtpcon = *zeros);
060860121105          chain(e) CPOcpo TNCPO01L;
060870121105          IF  not %error and %found(TNCPO01L);
060880121105            dCPO01 = CPOrst;
060890121105            §CPOdtpcon = %editc(dataoggi:'X');
060900121105            CPOrst = dCPO01;
060910121105            update TNCPO000;
060920121105          ENDIF;
060930121105        ENDIF;
060940121105
060950100407      /end-free
060960080307
060970080307      * se sono in anagrafica provvisoria aggiorno codice fiscale e p.iva
060980080307      * sul potenziale se sul potenziale mancano
060990080307     c                   if        *in06  and *in01
061000110516     c     v2ccpo        Chain(e)  Tncpo01l
061010110516     c                   if        %error
061020110516     c                   leavesr
061030110516     c                   endif
061040080307     c                   if        %found(tncpo01l)
061050160803      /free
061060160803       //?Salvo l'immagine precedente dell'anagrafica potenziale
061070160803         exsr ImmaginePrimaCpo;
061080160803      /end-free
061090080307     c                   if        (cpopiv=*blanks and v2civa<>*blanks)
061100080307     c                   eval      cpopiv=v2civa
061110080307     c                   z-add     *date         cpodtr
061120080307     c                   endif
061130080307     c                   if        (cpocdf=*blanks and v2ccdf<>*blanks)
061140080307     c                   eval      cpocdf=v2ccdf
061150080307     c                   z-add     *date         cpodtr
061160080307     c                   endif
061170080307     c                   update    tncpo000
061180160803      /free
061190160803       //?Scrivo la Variazione fatta sull'anagrafica potenziale
061200160803         exsr ScriviVarCpo;
061210160803      /end-free
061220080307     c                   endif
061230080307     c                   endif
061240051220
061250051220     c                   EndSr
061260051216
061270051216      *------------------------------------------------------------------------*
061280051125      * CONTROLLO DEL CODICE POTENZIALE
061290051125      *------------------------------------------------------------------------*
061300051125     c     Sr_Ctrcpo     BegSr
061310051125
061320111122      * Obbligatorio se cliente, se non è un 8888/9999
061330110223      * e se non è un 'incassi da attribuire'
061340111122      * se non è utente EDP e codice nuovo x 'incassi da attribuire'
061350051125     c                   If        v2ccpo = *Zeros and
061360110223     c                             v1ckcc = 151 and
061370051125     c                             wksc04 <> 8888 and wksc04 <> 9999
061380110223     c                             and %editc(v1cksc:'X') <> wCodIncAtt
061390051125     c                   Eval      *In28 = *On
061400051202     c                   Eval      h2riga = 14
061410051125     c                   Eval      h2colo = 28
061420051125     c                   Eval      v2cmsg = msg(12)
061430160429     c                   IF        *in20 and *in01
061440111122     c                             and sav_livello = *blanks
061450111122     c                             and wCodIncAtt = *blanks and
061460111122     c                             (wksc04 = 0001 or wksc04 = 1000)
061470111122     c                   clear                   v2cmsg
061480111122     c                   eval      *in28 = *off
061490111122     c                   ENDIF
061500051125     c                   Leavesr
061510051125     c                   EndIf
061520051125      * Carico i dati del potenziale se inserito
061530051125if  1c                   If        v2ccpo <> *Zeros and
061540051125      * e se variato il codice potenziale
061550051125     c                             v2ccpo <> savcpo
061560060221     c                             and wokpot = *off
061570051125      * Controllo se esite
061580060221     c     v2ccpo        Chain(n)  Tncpo01l
061590051125     c                   If        Not %Found(Tncpo01l)
061600051125     c                   Eval      *In28 = *On
061610051202     c                   Eval      h2riga = 14
061620051125     c                   Eval      h2colo = 28
061630051125     c                   Eval      v2cmsg = msg(13)
061640051125     c                   Leavesr
061650051125     c                   EndIf
061660051125      * Controllo se valido
061670051125     c                   If        cpoatb <> *Blanks
061680051125     c                   Eval      *In28 = *On
061690150427     c                   Eval      h2riga = 14
061700051125     c                   Eval      h2colo = 28
061710051125     c                   Eval      v2cmsg = msg(13)
061720051125     c                   Leavesr
061730051125     c                   EndIf
061740051125      * Controllo se l'utente può gestire il potenziale
061750051125     c                   Move      cpoflt        w003a
061760051125     c     w003a         Lookup    skpogcpo                               30
061770051125     c                   If        Not *In30
061780051125     c                   Eval      *In28 = *On
061790051202     c                   Eval      h2riga = 14
061800051125     c                   Eval      h2colo = 28
061810060110     c                   Eval      v2cmsg = msg(14)
061820051125     c                   Leavesr
061830051125     c                   EndIf
061840100604
061850100604      /free
061860100604       //?Se NON sono in ambiente di filiale non posso variare il potenziale
061870100604       IF  *in09 and v2ccpo <> savcpo and savcpo > 0;
061880100604         *in28 = *on;
061890100604         h2riga = 07;
061900100604         h2colo = 28;
061910100604         v2cmsg = 'Potenziale modificabile SOLO da utente di filiale';
061920100604         leavesr;
061930100604       ENDIF;
061940100806       //?controllo se con il vecchio potenziale c'è una
061950100604       //?trattativa aperta, in questo caso controllo che non ne esista già
061960100604       //?una con il nuovo potenziale
061970100806       IF  v2ccpo <> savcpo and savcpo > 0;
061980100604       //?con vecchio potenziale
061990100604         clear TNTA43ds;
062000100604         ITA43cpo = savcpo;
062010100604         ITA43ksc = v1cksc;
062020100604         ITA43cmm = %dec(v3cage:7:0);
062030100604         tnta43r (tnta43ds);
062040100604       //?trovo trattativa aperta
062050100604         IF  OTA43nrv > 0;
062060100604         //?rifaccio il controllo con nuovo potenziale
062070100604           clear TNTA43ds;
062080100604           ITA43cpo = v2ccpo;
062090100604           ITA43ksc = v1cksc;
062100100604           ITA43cmm = %dec(v3cage:7:0);
062110100604           tnta43r (tnta43ds);
062120100604         //?trovo trattativa aperta
062130100604           IF  OTA43nrv > 0;
062140100604             *in28 = *on;
062150100604             h2riga = 07;
062160100604             h2colo = 28;
062170100604             v2cmsg = 'Modifica non possibile, trattativa aperta sul +
062180100604                       potenziale immesso';
062190100604             leavesr;
062200100604           ENDIF;
062210100604         ENDIF ;
062220100604       ENDIF;
062230100604      /end-free
062240051212
062250051206      * Carico a video i dati del potenziale se sono in immissione
062260051206     c   01              ExSr      Sr_Carcpo
062270051125    1c                   EndIf
062280051125
062290051125     c                   EndSr
062300051116
062310051116      *------------------------------------------------------------------------*
062320051116      * CARICO I DATI DEL CODICE POTENZIALE
062330051116      *------------------------------------------------------------------------*
062340051116     c     Sr_Carcpo     BegSr
062350140714
062360140714     c     CPOcpo        Chain     Tncpo11l
062370051116
062380051116     c                   Eval      v2crag = cporag
062390051116     c                   Eval      v2cvia = cpovia
062400051116     c                   Eval      v2ccae = cpocap
062410051117     c                   Eval      v2csta = cponaz
062420051116     c                   Eval      v2ccit = cpocit
062430051116     c                   Eval      v2cprv = cpoprv
062440140714     c                   Eval      v2ctel = cpo1tel
062450140714     c                   Eval      v2ctlf = cpo1fax
062460051117     c                   Eval      v2ccpo = cpocpo
062470051117     c                   Eval      savcpo = cpocpo
062480051130     c                   Move      cposct        savitc
062490060208     c                   Eval      v2dcpo = cporag
062500080306     c                   Eval      v2ccdf = cpocdf
062510080306     c                   Eval      savcdf = v2ccdf
062520080306    5c                   if        %subst(cpopiv:1:2)>='00'
062530080306     c                   Movel     cpopiv        v2ivait
062540080306     c                   else
062550080306     c                   Movel     cpopiv        v2civa
062560080306    5c                   endif
062570080306     c                   Eval      saviva = v2civa
062580051116
062590051116     c                   EndSr
062600091119
062610091119      *------------------------------------------------------------------------*
062620091119      * AGGIORNAMENTO NOTA DC
062630091119      *------------------------------------------------------------------------*
062640091119     c     Sr_AggntcDC   BegSr
062650091119
062660091119     c   70
062670091119     corn06              Eval      kntcapl = 'C'
062680100806     c   06
062690091119     cann70              Eval      kntcapl = 'T'
062700091119     c                   Movel(p)  dutkci        kntcnk1
062710091119     c   70
062720091119     corn06              Move      v1cksc        kntcnk1
062730100806     c   06
062740100806     cann70              Move      v1cnrv        kntcnk1
062750091119     c                   Clear                   kntcnk2
062760091119     c                   Movel     'DC'          kntctnt
062770091119     c     kTfntc        Chain     Tfntc01l
062780091119     c                   If        %Found(Tfntc01l)
062790091119      * devo cancellarla perchè campo vuoto
062800091119     c                   if        v2ntcdc = *blanks
062810091119     c                   delete    tfntc
062820091119     c                   endif
062830091119      * aggiorno la nota
062840091119     c                   if        v2ntcdc <> *blanks
062850091119     c                   eval      ntcrnt = v2ntcdc
062860091119     c                   move      *date         ntcntr
062870091119     c                   update    tfntc
062880091119     c                   endif
062890091119     c                   endif
062900091119      * scrivo la nota
062910091119     c                   If        not %Found(Tfntc01l)
062920091119     c                   clear                   tfntc
062930091119     c                   eval      ntcapl = kntcapl
062940091119     c                   eval      ntcnk1 = kntcnk1
062950091119     c                   eval      ntctnt = kntctnt
062960091119     c                   eval      ntcrnt = v2ntcdc
062970091119     c                   eval      ntcsns = 'S'
062980091119     c                   move      *date         ntcntr
062990091119     c                   write     tfntc
063000091119     c                   endif
063010091119
063020091119     c                   EndSr
063030051212
063040051212      *--------------------------------------------------------------------*
063050051212      * WINDOW X CONFERMA DI VARIAZIONE POTENZIALE
063060051212      *--------------------------------------------------------------------*
063070051212     c     Sr_Winpot     begsr
063080051212
063090051212     c                   Reset                   wokpot
063100051212     c                   Eval      w01cok = 'NO'
063110051212     c                   Exfmt     ta60w01
063120051212      * Ok il nuovo potenziale è da tenere
063130051212if  1c                   If        w01cok = 'SI'
063140051212     c                   Eval      wokpot = *On
063150051212    1c                   EndIf
063160051212
063170051212     c                   EndSr
063180051118
063190051118      *------------------------------------------------------------------------*
063200051118      * CONTROLLO INDIRIZZO
063210051118      *------------------------------------------------------------------------*
063220051118     c     Sr_Ctrind     BegSr
063230051118
063240051118     c                   Clear                   fnlv13ds
063250051118     c                   Clear                   tisi95ds
063260051124     c                   Eval      *In90 = *Off
063270051118
063280051118      * Indirizzo completo
063290051118     c                   Z-add     *date         i14dri
063300051118     c                   Call      'FNLV14R'
063310051118     c                   Parm                    kpjba
063320051118     c                   Parm                    fnlv14ds
063330051118
063340051118s   1c                   Select
063350051118     c                   When      o14err = '1'
063360051124     c                   Eval      *In90 = *On
063370051130     c                   Eval      h2riga = 07
063380051118     c                   Eval      h2colo = 28
063390051124     c                   If        o14msg <> *Blanks
063400051124     c                   Eval      *In28 = *On
063410051118     c                   Eval      v2cmsg = o14msg
063420051124     c                   EndIf
063430051124     c                   Leavesr
063440051118     c                   When      o14err = '2'
063450051124     c                   Eval      *In90 = *On
063460051130     c                   Eval      h2riga = 08
063470051118     c                   Eval      h2colo = 28
063480051124     c                   If        o14msg <> *Blanks
063490051124     c                   Eval      *In28 = *On
063500051118     c                   Eval      v2cmsg = o14msg
063510051124     c                   EndIf
063520051124     c                   Leavesr
063530051118     c                   When      o14err = '5'
063540051124     c                   Eval      *In90 = *On
063550051130     c                   Eval      h2riga = 09
063560051118     c                   Eval      h2colo = 18
063570051124     c                   If        o14msg <> *Blanks
063580051124     c                   Eval      *In28 = *On
063590051118     c                   Eval      v2cmsg = o14msg
063600051124     c                   EndIf
063610051124     c                   Leavesr
063620051118     c                   When      o14err = '3'
063630051124     c                   Eval      *In90 = *On
063640051130     c                   Eval      h2riga = 09
063650051118     c                   Eval      h2colo = 28
063660051124     c                   If        o14msg <> *Blanks
063670051124     c                   Eval      *In28 = *On
063680051118     c                   Eval      v2cmsg = o14msg
063690051124     c                   EndIf
063700051124     c                   Leavesr
063710051118     c                   When      o14err = '4'
063720051124     c                   Eval      *In90 = *On
063730051130     c                   Eval      h2riga = 09
063740051118     c                   Eval      h2colo = 59
063750051124     c                   If        o14msg <> *Blanks
063760051124     c                   Eval      *In28 = *On
063770051118     c                   Eval      v2cmsg = o14msg
063780051124     c                   EndIf
063790051124     c                   Leavesr
063800051118     c                   When      o14err = '6'
063810051124     c                   Eval      *In90 = *On
063820051130     c                   Eval      h2riga = 09
063830051118     c                   Eval      h2colo = 62
063840051124     c                   If        o14msg <> *Blanks
063850051124     c                   Eval      *In28 = *On
063860051118     c                   Eval      v2cmsg = o14msg
063870051124     c                   EndIf
063880051124     c                   Leavesr
063890051118     c                   When      o14err <> *Blanks and o14msg = *Blanks
063900051118     c                   Goto      emi02
063910051118    1c                   EndSl
063920051118
063930051118      * Nazione con cappario controllo Cap
063940051118if  1c                   If        o14cpp = *Blanks
063950051118     c                   Eval      i95tcn = '7'
063960051118     c                   Eval      i95cap = e14cap
063970051118     c                   Eval      i95loc = e14loc
063980051118     c                   Eval      i95prv = e14prv
063990051118     c                   Eval      i95nar = e14nar
064000051118     c                   Z-add     *date         i95dat
064010051118     c                   Eval      i13af0 = 'S'
064020051118     c                   Eval      i13cnv = 'S'
064030051118     c                   Eval      i13af1 = 'S'
064040051118     c                   Eval      i13la3 = 'S'
064050051118      * Se dati variati controllo se congruenti
064060051118if  2c                   If        e14cap <> wcae or e14loc <> wcit or
064070051118     c                             e14prv <> wprv or e14nar <> wsta
064080051118     c                   Clear                   wfz3
064090051118     c                   Eval      wcae = e14cap
064100051118     c                   Eval      wcit = e14loc
064110051118     c                   Eval      wprv = e14prv
064120051118     c                   Eval      wsta = e14nar
064130051118    2c                   EndIf
064140051118     c                   Eval      E13fz3 = wfz3
064150051118     c                   Call      'FNLV13R'
064160051118     c                   Parm                    kpjba
064170051118     c                   Parm                    fnlv13ds
064180051118     c                   Parm                    tisi95ds
064190051118
064200051118     c                   If        o13ron = 'S'
064210051118     c                   Eval      e14nar = o95nar
064220051118     c                   EndIf
064230051118     c                   If        o13roc = 'S'
064240051118     c                   Eval      e14cap = o95cap
064250051118     c                   EndIf
064260051118     c                   If        o13rop = 'S'
064270051118     c                   Eval      e14prv = o95prv
064280051118     c                   EndIf
064290051118     c                   If        o13rol = 'S'
064300051118     c                   Eval      e14loc = o95loc
064310051118     c                   EndIf
064320051118
064330051124      * Reimposto le forzature
064340051118     c                   Eval      wfz3 = E13fz3
064350051118
064360051118s   2c                   Select
064370051118     c                   When      o13err = '5'
064380051118     c                   Eval      *In28 = *On
064390051130     c                   Eval      h2riga = 09
064400051118     c                   Eval      h2colo = 18
064410051118     c                   Eval      v2cmsg = o13msg
064420051118     c                   Leavesr
064430051118     c                   When      o13err = '3'
064440051118     c                   Eval      *In28 = *On
064450051130     c                   Eval      h2riga = 09
064460051118     c                   Eval      h2colo = 28
064470051118     c                   Eval      v2cmsg = o13msg
064480051118     c                   Leavesr
064490051118     c                   When      o13err = '4'
064500051118     c                   Eval      *In28 = *On
064510051130     c                   Eval      h2riga = 09
064520051118     c                   Eval      h2colo = 59
064530051118     c                   Eval      v2cmsg = o13msg
064540051118     c                   Leavesr
064550051118     c                   When      o13err = '6'
064560051118     c                   Eval      *In28 = *On
064570051130     c                   Eval      h2riga = 09
064580051118     c                   Eval      h2colo = 62
064590051118     c                   Eval      v2cmsg = o13msg
064600051118     c                   Leavesr
064610051118     c                   When      o13err <> *Blanks and o13msg = *Blanks
064620051118     c                   Goto      emi02
064630051118    2c                   EndSl
064640051118
064650051118      * Se nazione senza cappario
064660051118   x1c                   Else
064670051122      * Il cliente deve essere bloccato con 8 (si può usare solo x fatturare e
064680051122      *                                        non per bollettare)
064690051118if  2c                   If        v2cabl <> '8'
064700051118     c                   Eval      *In28 = *On
064710051130     c                   Eval      h2riga = 09
064720051118     c                   Eval      h2colo = 62
064730051118     c                   Eval      v2cmsg = msg(15)
064740051118     c                   Leavesr
064750051118    2c                   EndIf
064760051118    1c                   EndIf
064770051118
064780051118      * Cap obsoleto
064790051118     c                   If        o95cf1 = 'O'
064800051118     c                   Eval      *In28 = *On
064810051130     c                   Eval      h2riga = 09
064820051118     c                   Eval      h2colo = 18
064830051118     c                   Eval      v2cmsg = msg(16)
064840051118     c                   Leavesr
064850051118     c                   EndIf
064860051118
064870051118     c                   EndSr
064880051118
064890051118      *------------------------------------------------------------------------*
064900051118      * CONTROLLO CODICE FISCALE
064910051118      *------------------------------------------------------------------------*
064920051118     c     Sr_Ctrcdf     BegSr
064930151014     c                   clear                   w_scfsta
064940100729
064950061030      * devo richiamare il nuovo driver fatto per controllare il codice
064960061030      * fiscale e la partita iva
064970080421      * rihciamo anche per codice vuoto, per controllare i casi
064980080421      *  di obbligo non inserimento
064990151014     c                   clear                   xcfiva1ds
065000061030     c                   eval      xcfivapar = v2ccdf
065010061030     c                   eval      xcfivanar = v2csta
065020061030     c                   eval      xcfivaprv = v2cprv
065030151014     c                   eval      xcfivacap = v2ccae
065040151014     c                   eval      xcfivaloc = v2ccit
065050151014     c                   eval      xcfivapiva= v2civa
065060151014     c                   call      'XCFIVAR1'
065070151014     c                   parm                    xcfiva1ds
065080080421     c
065090080421    1c                   if        xcfivaflg < *zeros
065100051118     c                   Eval      *In28 = *On
065110051202     c                   Eval      h2riga = 12
065120051118     c                   Eval      h2colo = 28
065130080421     c                   Eval      v2cmsg = xcfivamsg
065140051118     c                   Leavesr
065150080421    1c                   EndIf
065160080421
065170080421      * il codice fiscale è obbligatorio se sto inserendo un codice nuovo
065180080421      * cliente italia a partire dalla data decorrenza per p.iva sarà
065190080421      * obbligatorio non forzabile anche in caso di codice non nuovo
065200080421
065210080421      * Escludo i casi previsti di NON inserimento
065220080421    1c  n06              if        xcfivaflg<>9 and
065230080421     c                             v2csta=*blanks and v2ccdf=*blanks
065240080421     c
065250080421     c* prima di dare errore verifico se cliente del sottoconto intestazione
065260080421     c* fattura ha la nazione ed in questo caso non do errore
065270080421     c                   exsr      sr_repscfsta
065280080421    2c                   if        w_scfsta=*blanks
065290080421     c                   eval      *In28 = *On
065300080421     c                   eval      h2riga = 12
065310080421     c                   eval      h2colo = 28
065320080421     c                   eval      v2cmsg = msg(51)
065330080421     c                   leavesr
065340080421    2c                   endif
065350080421    1c                   endif
065360080306     c* Se provengo dalle visite e il relativo potenziale non ha il cod.fisc
065370080306     c* errore forzabile se inserito un codice fiscale già presente su altro
065380080306     c* potenziale
065390080421    1c                   if        *in06 and v2ccpo<>*zeros and v2ccdf<>*blanks
065400080306     c     v2ccpo        Chain(n)  Tncpo01l
065410080421    2c                   if        %found(tncpo01l) and cpocdf=*blanks
065420080306     c                             and v2ccdf<>savcdf
065430080306     c                             and v2ccdf<>cod_forzcdf
065440080306     c                   clear                   savrag
065450080307     c                   eval      codice=v2ccdf
065460080306     c                   exsr      sr_CdfAltroP
065470080306     c                   exsr      sr_msgcdf
065480080306     c   28              leavesr
065490080421    2c                   endif
065500080421    1c                   endif
065510051118
065520051118     c                   EndSr
065530051118
065540051118      *------------------------------------------------------------------------*
065550051118      * CONTROLLO PARTITA IVA E STATO
065560051118      *------------------------------------------------------------------------*
065570051118     c     Sr_Ctriva     BegSr
065580051118
065590051118      * STATO
065600051118      * --> ricerca
065610051118     c                   Eval      wpos = %scan('?':v2csta)
065620051118     c                   If        wpos > 0
065630051118     c                   Eval      kcod = '15'
065640051118     c                   call      'TRUL19R'
065650051118     c                   parm                    kcod
065660051118     c                   parm                    ordina
065670051118     c                   parm                    kkey
065680051118     c                   parm                    comando
065690051118     c                   Eval      v2csta = kkey
065700051130     c                   Eval      h2riga = 09
065710051118     c                   Eval      h2colo = 62
065720051124     c                   Eval      *In90 = *On
065730051118     c                   EndIf
065740051118      * --> controllo
065750051118     c                   Clear                   ds15
065760051118     c                   Eval      kcod = '15'
065770051118     c                   Eval      kkey = v2csta
065780051118     c     kTabel        Chain     Tabel00f
065790051118     c                   If        Not %Found(Tabel00f) or
065800051125     c                             tblflg <> *Blanks
065810051118     c                   Eval      *In28 = *On
065820051130     c                   Eval      h2riga = 09
065830051118     c                   Eval      h2colo = 62
065840051118     c                   Eval      v2cmsg = msg(17)
065850051118     c                   Leavesr
065860051118     c                   EndIf
065870051125     c                   Eval      ds15 = tbluni
065880051118
065890051129      * --> controllo
065900151014     c**                 If        v2civa <> *Blanks
065910061030      * devo richiamare il nuovo driver fatto per controllare il codice
065920061030      * fiscale e la partita iva
065930151014     c                   clear                   xcfiva1ds
065940061030     c                   eval      xcfivamod = 'P'
065950061030     c                   eval      xcfivapar = v2civa
065960061030     c                   eval      xcfivanar = v2csta
065970061030     c                   eval      xcfivaprv = v2cprv
065980151014     c                   eval      xcfivacap = v2ccae
065990151014     c                   eval      xcfivaloc = v2ccit
066000151014     c                   eval      xcfivapiva= v2civa
066010151014     c                   call      'XCFIVAR1'
066020151014     c                   parm                    xcfiva1ds
066030061106      * --> errata forzabile
066040061106     c                   If        xcfivaflg = -9 and wforzaiva = *off
066050061106     c                   Eval      *In28 = *On
066060061106     c                   Eval      h2riga = 12
066070061106     c                   Eval      h2colo = 52
066080061106     c                   Eval      v2cmsg = xcfivamsg
066090061106     c                   Eval      v2cmsg = %trim(v2cmsg) + ' Enter x forzare'
066100061106     c                   eval      wforzaiva = *on
066110061106     c                   Leavesr
066120061106     c                   EndIf
066130061030      * --> errore
066140061106     c                   if        xcfivaflg < *zeros and xcfivaflg <> -9
066150051118     c                   Eval      *In28 = *On
066160051202     c                   Eval      h2riga = 12
066170051118     c                   Eval      h2colo = 52
066180061030     c                   eval      v2cmsg = xcfivamsg
066190051118     c                   Leavesr
066200051118     c                   EndIf
066210061106      * se non impostato devo riportare il codice iso della partita iva che
066220061106      * mi viene passato dal pgm di controllo
066230061114     c                   if        v2ivaeu <> ivaeu
066240061106     c                   eval      v2ivaeu = ivaeu
066250061106     c                   eval      *in39 = *on
066260061106     c                   eval      *in90 = *on
066270061114     c                   endif
066280151014      * PARTITA IVA
066290151014      * --> obbligatoria (se non è gestione visite/offerte)
066300151014      * --> non è però obbligatoria se cliente Italia e nazione del
066310151014      *     sottoconto intestazione fattura estera
066320151014      * Escludo i casi previsti di NON inserimento
066330151014     c                   If        v2civa = *Blanks and Not *In06
066340151014     c                             and xcfivaflg<>9
066350151014     c                   exsr      sr_repscfsta
066360151014     c                   if        v2csta<>*blanks or w_scfsta=*blanks
066370151014     c                   Eval      *In28 = *On
066380151014     c                   Eval      h2riga = 12
066390151014     c                   Eval      h2colo = 52
066400151014     c                   Eval      v2cmsg = msg(18)
066410151014     c                   Leavesr
066420151014     c                   EndIf
066430151014     c                   EndIf
066440151014
066450151014     c* Se provengo dalle visite e il relativo potenziale non ha p.iva
066460080306     c* errore forzabile se inserito una p.iva         già presente su altro
066470080306     c* potenziale
066480151014     c                   If        v2civa <> *Blanks
066490080306     c                   if        *in06 and v2ccpo<>*zeros
066500080306     c                             and v2ivaeu<>'$$'
066510080306     c     v2ccpo        Chain(n)  Tncpo01l
066520080306     c                   if        %found(tncpo01l) and cpopiv=*blanks
066530080306     c                             and v2civa<>saviva
066540080306     c                             and v2civa<>cod_forziva
066550080306     c                   clear                   savrag
066560080307     c                   if        v2ivaeu<>*blanks
066570080306     c                   eval      codice=v2civa
066580080306     c                   else
066590080306     c                   eval      codice=v2ivait
066600080306     c                   endif
066610080306     c                   exsr      sr_PivAltroP
066620080306     c                   exsr      sr_msgiva
066630080306     c   28              leavesr
066640080306     c                   endif
066650080307     c                   endif
066660061106
066670060105      * --> controllo se c'è già un cliente con la stessa p.IVA
066680060105      *     se immissione di un nuovo codice o se conferma offerta
066690060119if  1c                   If        Not *In06 and (*In01 or *In07)
066700060221     c                             and v2ivaeu <> '$$'
066710060109     c                   If        v2ivaeu = *Blanks
066720060109     c                   Movel     v2ivait       kindiva
066730060109     c                   Else
066740060109     c                   Movel     v2civa        kindiva
066750060109     c                   EndIf
066760060105     c     kCnind        Setll     Cnind02l
066770060105      * non trovo nessun altra partita IVA esco
066780060105     c                   If        Not %Equal
066790060105     c                   Leavesr
066800060105     c                   EndIf
066810060105do  2c                   Do        *Hival
066820060105     c     kCnind        Reade     Cnind02l
066830060105      * fine file esco
066840060105     c                   If        %Eof(Cnind02l)
066850060105     c                   Leave
066860060105     c                   EndIf
066870060105      * deve avere codice cliente diverso
066880060301     c                   If        ww_indksc = v1cksc
066890060105     c                   Iter
066900060105     c                   EndIf
066910060105      * controllo lo stato del credito
066920081006      * lo faccio con una routine per emettere una window nel caso di
066930081006      * messaggio info
066940081006     c                   exsr      sr_ctrstc
066950081006     c                   if        wforzacon = *On
066960081006     c                   leavesr
066970081006     c                   endif
066980060105    2c                   EndDo
066990060105    1c                   EndIf
067000151015     c
067010151015     c                   EndIf
067020051118
067030051118     c                   EndSr
067040081006
067050081006      *------------------------------------------------------------------------*
067060081006      * CONTROLLO STATO DEL CREDITO
067070081006      *------------------------------------------------------------------------*
067080081006     c     sr_ctrstc     begsr
067090081006
067100081006     c                   Clear                   Tibs69ds
067110081006     c                   Move      ww_indksc     I69kcp
067120081006     c                   Call      'TIBS69R'
067130081006     c                   Parm                    Tibs69ds
067140081006     c                   Parm                    ds_cnaco
067150081006     c                   Parm                    ds_cnind
067160081006     c                   Parm                    ds_cnclp
067170081006     c                   Parm                    ds_fncls
067180081006     c                   Eval      wtibs69 = *On
067190081006      * errore forzabile per stato del credito trovato
067200081006      * emesso con window e f6 di conferma
067210081006     c                   If        w_clpcon <> *Blanks and wforzacon = *Off
067220090803      * decodifico stato del credito
067230090803     c                   clear                   w_ds4w
067240090803     c                   eval      kcod = '4W'
067250090803     c                   eval      kkey = w_clpcon
067260090803     c                   eval      w5ccon = %subst(w_clpcon:2:2)
067270090803     c     ktabel        chain     tabel00f
067280090803     c                   if        %found(tabel00f) and
067290090803     c                             tblflg = *blanks
067300090803     c                   eval      w_ds4w = tbluni
067310090803     c                   endIf
067320090803     c                   Eval      w5dcon = w_§4wdes
067330081006     c                   do        *hival
067340081006     c                   exfmt     ta60w05
067350081006     c   kf              leave
067360081006     c                   enddo
067370081006     c                   Eval      wforzacon = *On
067380081006     c                   EndIf
067390081006
067400081006     c                   endsr
067410051122
067420051122      *------------------------------------------------------------------------*
067430051122      * CONTROLLO BLOCCO CLIENTE
067440051122      *------------------------------------------------------------------------*
067450051122     c     Sr_Ctrblc     BegSr
067460051118
067470051122      * STATO DEL CREDITO
067480051122      * --> Ricerca
067490051122     c                   Clear                   v2dcon
067500051122     c                   Eval      wpos = %scan('?':v2ccon)
067510051122     c                   If        wpos > 0
067520051122     c                   Eval      kcod = '4W'
067530051122     c                   call      'TRUL19R'
067540051122     c                   parm                    kcod
067550051122     c                   parm                    ordina
067560051122     c                   parm                    kkey
067570051122     c                   parm                    comando
067580051124     c                   Eval      w003a = kkey
067590051124     c                   Move      w003a         v2ccon
067600051202     c                   Eval      h2riga = 17
067610051122     c                   Eval      h2colo = 28
067620051124     c                   Eval      *In90 = *On
067630051122     c                   EndIf
067640051122
067650051122      * --> Controllo
067660051122     c                   Clear                   v2dcon
067670051122     c                   Clear                   ds4w
067680051122     c                   Eval      kcod = '4W'
067690051124     c                   Move(p)   v2ccon        w003a
067700051124     c                   Eval      kkey = w003a
067710051122     c     kTabel        Chain     Tabel00f
067720051122     c                   If        Not %Found(Tabel00f) or
067730051125     c                             tblflg <> *Blanks
067740051122     c                   Eval      *In28 = *On
067750051202     c                   Eval      h2riga = 17
067760051122     c                   Eval      h2colo = 28
067770051122     c                   Eval      v2cmsg = msg(23)
067780051122     c                   Leavesr
067790051122     c                   EndIf
067800051125     c                   Eval      ds4w = tbluni
067810051122     c                   Eval      v2dcon = §4wdes
067820051122
067830051122      * Se sono in filiale devo usare solo stati del credito autorizzati e
067840051122      * senza passaggio al contenzioso
067850130325      * da 25/03/2013 abbiamo deciso che anche in sede non è modificale, solo
067860130325      * da ProJ
067870051122     c                   If        (§4wmbl = 'N' or §4wctz = 'S') and
067880130325     c                             (v2ccon <> %subst(clpcon:2:2) or *In01)
067890051122     c                   Eval      *In28 = *On
067900051202     c                   Eval      h2riga = 17
067910051122     c                   Eval      h2colo = 28
067920051122     c                   Eval      v2cmsg = msg(24)
067930051122     c                   Leavesr
067940051122     c                   EndIf
067950051122
067960051122      * --> Imposto i blocchi
067970051122      * Se sono in filiale proteggo lo stato del credito se non è modificabile
067980120928      * da p.o.
067990130325      * PER TUTTI
068000130325     c                   If        §4wmbl = 'N'
068010060220     c                   Eval      *In19 = *On
068020051122     c                   EndIf
068030120928      * Se sono in filiale proteggo il blocco pagamenti se non è modificabile
068040120928      * da p.o.
068050120928     c                   If        Not *In09 and §4wmbp = 'N'
068060120928     c                   Eval      *In81 = *On
068070120928     c                   EndIf
068080120925      * Se sono in filiale proteggo il blocco cliente se non è modificabile
068090120925      * da p.o.
068100120928     c                   If        Not *In09 and §4wmtb = 'N'
068110120925     c                   Eval      *In79 = *On
068120120925     c                   EndIf
068130121002
068140121002      * se stato del credito variato
068150121002     c                   IF        V2Ccon <> %subst(CLPcon:2:2)
068160051122      * Se il cliente non è bloccato e lo stato del credito prevede il blocco
068170051122      * lo faccio io ora in automatico
068180051122     c                   If        v2cabl = *Blanks and §4wtbl <> *Blanks
068190051122     c                   Eval      v2cabl = §4wtbl
068200130322     c                   eval      V2Cabledp = §4wtbl
068210051122     c                   EndIf
068220051122
068230051122      * Se non c'è la causale del blocco la imposto io ora in automatico
068240051122     c                   If        (v2cblc = *Blanks or v2cblc = *Zeros) and
068250051122     c                              §4wblc <> *Blanks
068260051122     c                   Eval      v2cblc = §4wblc
068270051122     c                   EndIf
068280121002     c                   ENDIF
068290051122
068300051122      * BLOCCO CLIENTE
068310130322     c                   IF        V2Cabl <> *blanks and V2Cabl <> '8'
068320130322     c                             and not *in79
068330130322     c                   Eval      *In28 = *On
068340130322     c                   Eval      h2riga = 18
068350130322     c                   Eval      h2colo = 28
068360130322     c                   Eval      V2Cmsg = 'Blocco cliente errato'
068370130322     c                   Leavesr
068380130322     c                   ENDIF
068390051122      * --> ricerca
068400051122     c                   Clear                   v2dblc
068410051122     c                   Eval      wpos = %scan('?':v2cblc)
068420051122     c                   If        wpos > 0
068430121127     c                   goto      no19
068440051122     c                   Eval      kcod = 'BI'
068450051122     c                   call      'TRUL19R'
068460051122     c                   parm                    kcod
068470051122     c                   parm                    ordina
068480051122     c                   parm                    kkey
068490051122     c                   parm                    comando
068500051122     c                   Eval      v2cblc = kkey
068510121127     c     no19          tag
068520121127     c                   clear                   kpjbu
068530121127     c                   call      'TRTBBIR'
068540121127     c                   parm                    kpjba
068550121127     c                   eval      v2cblc = %subst(kpjbu:1:3)
068560051202     c                   Eval      h2riga = 18
068570051122     c                   Eval      h2colo = 42
068580051124     c                   Eval      *In90 = *On
068590051122     c                   EndIf
068600051122
068610051122      * --> Controllo
068620051122     c                   If        v2cblc <> *Blanks
068630110323     c                   Clear                   dsbi
068640051122     c                   Clear                   v2dblc
068650051122     c                   Eval      kcod = 'BI'
068660051122     c                   Eval      kkey = v2cblc
068670051122     c     kTabel        Chain     Tabel00f
068680051122     c                   If        Not %Found(Tabel00f) or
068690051125     c                             tblflg <> *Blanks
068700051122     c                   Eval      *In28 = *On
068710051202     c                   Eval      h2riga = 18
068720051122     c                   Eval      h2colo = 42
068730051122     c                   Eval      v2cmsg = msg(25)
068740051122     c                   Leavesr
068750051122     c                   EndIf
068760110323     c                   Eval      dsbi = tbluni
068770110323     c                   Eval      v2dblc = §bides
068780121127      * causale blocco non piuù utilizzabile
068790121127     c                   IF        §BIuso = 'N'
068800121127     c                   Eval      *In28 = *On
068810121127     c                   Eval      h2riga = 18
068820121127     c                   Eval      h2colo = 42
068830121127     c                   Eval      v2cmsg = 'Causale non più utilizzabile'
068840121127     c                   Leavesr
068850121127     c                   ENDIF
068860051124     c                   EndIf
068870051122
068880051122      * Se cliente bloccato la causale blocco è obbligatoria
068890051122     c                   If        v2cabl <> *Blanks and v2cblc = *Blanks
068900130325     c                             and v2cabl <> '*'
068910051122     c                   Eval      *In28 = *On
068920051202     c                   Eval      h2riga = 18
068930051122     c                   Eval      h2colo = 42
068940051122     c                   Eval      v2cmsg = msg(26)
068950051122     c                   Leavesr
068960051122     c                   EndIf
068970051122
068980051122      * Se immessa la causale blocco il cliente deve essere bloccato
068990051122     c                   If        v2cabl = *Blanks and v2cblc <> *Blanks
069000051122     c                   Eval      *In28 = *On
069010051202     c                   Eval      h2riga = 18
069020051122     c                   Eval      h2colo = 28
069030051122     c                   Eval      v2cmsg = msg(27)
069040051122     c                   Leavesr
069050051122     c                   EndIf
069060110407
069070110407      * Se pgm richiamato per blocco cliente il blocco deve esserci
069080110407     c                   If        $Blocco and V2cabl = *blanks
069090110407     c                   Eval      *In28 = *On
069100110407     c                   Eval      h2riga = 18
069110110407     c                   Eval      h2colo = 28
069120110407     c                   Eval      v2cmsg = 'Immettere il blocco cliente'
069130110407     c                   Leavesr
069140110407     c                   EndIf
069150051122
069160051122     c                   EndSr
069170090616
069180090616      *------------------------------------------------------------------------*
069190090616      * CONTROLLO SOLLECITO
069200090616      *------------------------------------------------------------------------*
069210090616     c     sr_ctrsol     begsr
069220090616
069230090616     c                   reset                   trulsoli1
069240090616     c                   eval      trulsoli1.kut = codut
069250090616     c                   eval      trulsoli1.kcc = v1ckcc
069260090616     c                   eval      trulsoli1.ksc = v1cksc
069270090616     c                   evalr     trulsoli1.con = v2ccon
069280090616     c                   eval      rqsdatasize = %size(trulsoli1)
069290090616     c                   eval      rpydatasize = %size(trulsolo1)
069300090616     c                   call      'TRULSOLR'
069310090616     c                   parm      *blanks       rqsopcode
069320090616     c                   parm      *zeros        rpyesito
069330090616     c                   parm      'TRULSOLI1'   rqsformatname
069340090616     c                   parm                    trulsoli1
069350090616     c                   parm                    rqsdatasize
069360090616     c                   parm      'TRULSOLO1'   rpyformatname
069370090616     c                   parm                    trulsolo1
069380090616     c                   parm                    rpydatasize
069390090617
069400090617      * Immissione
069410090617     c                   if        *in01
069420090617     c                   if        rpyesito = *zeros
069430090617     c                   eval      v2csol = trulsolo1.sol
069440090617     c                   else
069450090617     c                   eval      v2csol = 'S'
069460090617     c                   endif
069470090617     c                   endif
069480090617
069490090617      * Modifica
069500090617     c                   if        rpyesito = *zeros and not *in01 and
069510090617     c                             v2csol <> trulsolo1.sol and
069520090617     c                             trulsolo1.solmod = 'N'
069530090617     c                   Eval      *In28 = *On
069540111115     c                   Eval      h2riga = 19
069550090617     c                   Eval      h2colo = 28
069560090617     c                   if        trulsolo1.sol = 'S'
069570090617     c                   Eval      v2cmsg = msg(83)
069580090617     c                   Leavesr
069590090617     c                   else
069600090617     c                   Eval      v2cmsg = msg(84)
069610090617     c                   Leavesr
069620090617     c                   endif
069630090617     c                   endif
069640090616
069650090616     c                   endsr
069660051122
069670051118      *------------------------------------------------------------------------*
069680051118      * CONTROLLO CODICE COMMERCIALE
069690051118      *------------------------------------------------------------------------*
069700051118     c     Sr_Ctrage     BegSr
069710051118
069720051118      * Ricerca
069730051130     c                   Clear                   v3dage
069740051130     c                   Eval      wpos = %scan('?':v3cage)
069750051118     c                   If        wpos > 0
069760051130     c                   Eval      v3cage = *All'0'
069770130808     c                   clear                   TRMK43ds
069780130806     c  n06              movel     v1cksc        iMK43fil
069790130806     c   06              movel     ta60cli       iMK43fil
069800130806     c                   call      'TRMK43R'
069810130806     c                   parm                    kpjba
069820130806     c                   parm                    TRMK43ds
069830130806     c                   if        oMK43err = *off  and  oMK43fxx = *blank
069840130806     c                   movel     oMK43cmm      v3cage
069850130806     c                   eval      v3dage = oMK43des
069860130806     c                   endif
069870051130     c                   Eval      h3riga = 07
069880051130     c                   Eval      h3colo = 28
069890051124     c                   Eval      *In90 = *On
069900051118     c                   EndIf
069910051122
069920051118      * Controllo (obbligatorio)
069930051130     c                   Clear                   v3dage
069940130806     c                   If        %check(digits:V3Cage) > 0
069950130806     c                   eval      *In28 = *On
069960130806     c                   eval      h3riga = 07
069970130806     c                   eval      h3colo = 28
069980130806     c                   eval      V3Cmsg = Msg(20)
069990130806     c                   leavesr
070000130806     c                   EndIf
070010130806     c                   move      V3Cage        CMMcod
070020130806     c     CMMcod        chain     AZCMM000
070030130806     c                   If        Not %Found(AZCMM01L)
070040051122     c                   Eval      *In28 = *On
070050051130     c                   Eval      h3riga = 07
070060051130     c                   Eval      h3colo = 28
070070051130     c                   Eval      v3cmsg = msg(20)
070080051122     c                   Leavesr
070090051122     c                   EndIf
070100130806     c                   eval      V3Dage = CMMdes
070110051122
070120051122      * Codice commerciale vari valido solo per 8888 e 9999
070130051130     c                   Move      v3cage        w0040
070140130806     c                   IF        CMMpar = '1' and wksc04 <> 8888
070150130806     c                                          and wksc04 <> 9999
070160051122     c                   Eval      *In28 = *On
070170051130     c                   Eval      h3riga = 07
070180051130     c                   Eval      h3colo = 28
070190051130     c                   Eval      v3cmsg = msg(20)
070200051122     c                   Leavesr
070210051122     c                   EndIf
070220110216
070230110216      /free
070240111123       //?Commerciale "valido" data inizio e fine attività
070250130806         IF  CMMdtIni > dataoggi  or
070260130806             CMMdtFin < dataoggi;
070270111123           *in28 = *on;
070280111123           H3riga = 07;
070290111123           H3colo = 28;
070300111123           V3Cmsg = msg(20);
070310111123           leavesr;
070320111123         ENDIF;
070330111123
070340110216       //?Commerciale 'inattivi' possibile solo se
070350110217       //?immissione
070360110216       //?utente EDP
070370110216       //?cliente incassi da attribuire --> 0001 x 1°livello
070380110216       //?                              --> 1000 x 2°livello
070390110217       //?oppure se
070400110217       //?manutenzione
070410110217       //?NO anagrafica provvisoria
070420110217       //?cliente incassi da attribuire = a cod. in tab Y4O
070430130806         IF  CMMpar = '2';
070440110217           SELECT;
070450110217             WHEN  *in02 and not *in06 and %editc(V1Cksc:'X') = wCodIncAtt;
070460160429             WHEN  *in01 and *in20 and
070470121121                   ((sav_livello = '1' and wksc04 = 0001) or
070480121121                    (sav_livello = '2' and wksc04 = 1000) or
070490121121                    (sav_livello = *blanks and wCodIncAtt = *blanks and
070500121121                    (wksc04 = 1000  or wksc04 = 0001)));
070510110217             OTHER;
070520110216              *in28 = *on;
070530110216              H3riga = 07;
070540110216              H3colo = 28;
070550110216              V3Cmsg = msg(20);
070560110216              leavesr;
070570110217           ENDSL;
070580110217         ENDIF;
070590110216      /end-free
070600051122
070610051122      * P.o. del commerciale congruente con p.o. del cliente
070620051221      * solo se non è gestione anagrafica provvisoria di un nuovo cliente
070630051221     c                   If        Not *In06 or ta60cli > *Zeros
070640051122     c  n06              Movel     v1cksc        w0030
070650051122     c   06              Movel     ta60cli       w0030
070660051130     c                   Movel     v3cage        wpoage
070670051122     c                   If        w0030 <> wpoage
070680051122     c                   Eval      *In28 = *On
070690051130     c                   Eval      h3riga = 07
070700051130     c                   Eval      h3colo = 28
070710051130     c                   Eval      v3cmsg = msg(21)
070720051122     c                   Leavesr
070730051122     c                   EndIf
070740051221     c                   EndIf
070750051122
070760051122      * Se commerciale xxx0999 (cliente inattivo) il cliente va bloccato
070770110216      * salto il controllo
070780110216      * il commerciale 'inattivi' non va usato
070790110216     c                   leavesr
070800051130     c                   Move      v3cage        w0040
070810051122     c                   If        w0040 = 999 and v2cabl = *Blanks
070820051122     c                   Eval      *In28 = *On
070830051130     c                   Eval      v3cmsg = msg(22)
070840051122     c                   Leavesr
070850051122     c                   EndIf
070860051118
070870051118     c                   EndSr
070880051122
070890051122      *------------------------------------------------------------------------*
070900051122      * CONTROLLO CODICE IMPORTANZA CLIENTE
070910051122      *------------------------------------------------------------------------*
070920051122     c     Sr_Ctrclv     BegSr
070930100301
070940100301     c                   Clear                   v3dclv
070950100301
070960100301      * se anagrafica provvisoria da trattativa
070970100301      * non è obbligatorio inserire il codice, se a blank vado a fine controlli
070980100806     c                   if        *in06 and v3cclv = *blanks
070990100301     c                   leavesr
071000100301     c                   endif
071010051122
071020051122      * Ricerca
071030051130     c                   Eval      wpos = %scan('?':v3cclv)
071040051122     c                   If        wpos > 0
071050051122     c                   Eval      kcod = 'IC'
071060051122     c                   call      'TRUL19R'
071070051122     c                   parm                    kcod
071080051122     c                   parm                    ordina
071090051122     c                   parm                    kkey
071100051122     c                   parm                    comando
071110051130     c                   Eval      v3cclv = kkey
071120051130     c                   Eval      h3riga = 08
071130051130     c                   Eval      h3colo = 28
071140051124     c                   Eval      *In90 = *On
071150051122     c                   EndIf
071160051122
071170051124      * --> Controllo (obbligatorio)
071180051130     c                   If        v3cclv = *Blanks
071190051124     c                   Eval      *In28 = *On
071200051130     c                   Eval      h3riga = 08
071210051130     c                   Eval      h3colo = 28
071220051130     c                   Eval      v3cmsg = msg(28)
071230051124     c                   Leavesr
071240051124     c                   EndIf
071250051124
071260051130     c                   Clear                   v3dclv
071270051122     c                   Clear                   dsic
071280051122     c                   Eval      kcod = 'IC'
071290051130     c                   Eval      kkey = v3cclv
071300051122     c     kTabel        Chain     Tabel00f
071310051122     c                   If        Not %Found(Tabel00f) or
071320051125     c                             tblflg <> *Blanks
071330051122     c                   Eval      *In28 = *On
071340051130     c                   Eval      h3riga = 08
071350051130     c                   Eval      h3colo = 28
071360051130     c                   Eval      v3cmsg = msg(29)
071370051122     c                   Leavesr
071380051122     c                   EndIf
071390051125     c                   Eval      dsic = tbluni
071400051130     c                   Eval      v3dclv = §sicde
071410051122
071420051122     c                   EndSr
071430051122
071440051122      *------------------------------------------------------------------------*
071450051122      * CONTROLLO CODICE ISTAT
071460051122      *------------------------------------------------------------------------*
071470051122     c     Sr_Ctritc     BegSr
071480051122
071490051122      * Ricerca
071500051130     c                   Clear                   v3ditc
071510051130     c                   Eval      wpos = %scan('?':v3citc)
071520051122     c                   If        wpos > 0
071530051122     c                   Eval      kcod = '1L'
071540051122     c                   call      'TRUL19R'
071550051122     c                   parm                    kcod
071560051122     c                   parm                    ordina
071570051122     c                   parm                    kkey
071580051122     c                   parm                    comando
071590051130     c                   Eval      v3citc = kkey
071600051130     c                   Eval      h3riga = 09
071610051130     c                   Eval      h3colo = 28
071620051124     c                   Eval      *In90 = *On
071630051122     c                   EndIf
071640051122
071650051122      * --> Controllo (obbligatorio)
071660051130     c                   If        v3citc = *Blanks or v3citc = *Zeros
071670051122     c                   Eval      *In28 = *On
071680051130     c                   Eval      h3riga = 09
071690051130     c                   Eval      h3colo = 28
071700051130     c                   Eval      v3cmsg = msg(30)
071710051122     c                   Leavesr
071720051122     c                   EndIf
071730051122
071740051130     c                   Clear                   v3ditc
071750051122     c                   Clear                   ds1l
071760051124     c                   Eval      kcod = '1L'
071770051130     c                   Eval      kkey = v3citc
071780051122     c     kTabel        Chain     Tabel00f
071790051122     c                   If        Not %Found(Tabel00f) or
071800051125     c                             tblflg <> *Blanks
071810051122     c                   Eval      *In28 = *On
071820051130     c                   Eval      h3riga = 09
071830051130     c                   Eval      h3colo = 28
071840051130     c                   Eval      v3cmsg = msg(31)
071850051122     c                   Leavesr
071860051122     c                   EndIf
071870051125     c                   Eval      ds1l = tbluni
071880051130     c                   Eval      v3ditc = §1ldes
071890051122
071900051122     c                   EndSr
071910140722
071920140722      /free
071930140722
071940140722       //--------------------------------------------------------------
071950140722       //?Controllo Note 02/03       (News/Nominativo),                ?
071960140722       //?               05/06/T5/I5 (Responsabile Trasporti),         ?
071970140722       //?               07/08/T7/I7 (Responsabile Logistica).         ?
071980140722       //--------------------------------------------------------------
071990140722       BEGSR  sr_CtrNote;
072000140722
072010140722         // -?Se NON selezionate: esco?
072020140722         select;
072030140722           when  wRGR  = 'RN'  and  V3Cnt02 = *blank;
072040140722             leavesr;
072050140722           when  wRGR  = 'RT'  and  V3Cnt05 = *blank;
072060140722             leavesr;
072070140722           when  wRGR  = 'RL'  and  V3Cnt07 = *blank;
072080140722             leavesr;
072090140722         endsl;
072100140722
072110140722         // -?Se sono in interrogazione non posso interrogare?
072120140722         //  ?   se non ho dati?
072130140722         if  *in05  and  ( (wRGR = 'RN'  and  Not *in24)  or
072140140722                           (wRGR = 'RT'  and  Not *in26)  or
072150140722                           (wRGR = 'RL'  and  Not *in31) );
072160140722           *in28 = *on;
072170140722           select;
072180140722             when  wRGR = 'RN';
072190140722               H3riga = 15;
072200140722             when  wRGR = 'RT';
072210140722               H3riga = 17;
072220140722             when  wRGR = 'RL';
072230140722               H3riga = 19;
072240140722           endsl;
072250140722           H3colo = 35;
072260140722           V3Cmsg = Msg(52);
072270140722           leavesr;
072280140722         endif;
072290140722
072300140722         // -?Imposto se interrogazione o gestione?
072310140722         if  Not *in05;
072320140722           wRich = 'M';
072330140722         else;
072340140722           wRich = 'V';
072350140722         endif;
072360140722         wRag  = V3Crag;
072370140722         clear  wTnt;
072380140722         //wRGR  = 'RN'       ?(già impostato)?
072390140722
072400140722         exsr  sr_CallTNTA12;
072410140722
072420140722         if  TA12err <> *blank;
072430140722           *in28 = *on;
072440140722           select;
072450140722             when  wRGR = 'RN';
072460140722               H3riga = 15;
072470140722             when  wRGR = 'RT';
072480140722               H3riga = 17;
072490140722             when  wRGR = 'RL';
072500140722               H3riga = 19;
072510140722           endsl;
072520140722           H3colo = 35;
072530140722           V3Cmsg = TA12msg;
072540140722           leavesr;
072550140722         endif;
072560140722
072570140722         // -?Controllo se ora c'è o non c'è più alcuna nota?
072580140722         kNtcNk1 = %editc( DUTkci : 'X' );
072590140722         if  *in70  or  Not *in06;
072600140722           kNtcApl = 'C';
072610140722           kNtcNk1 = %trim(kNtcNk1) + %editc( V1Cksc : 'X' );
072620140722         else;
072630140722           kNtcApl = 'T';
072640140722           kNtcNk1 = %trim(kNtcNk1) + %editc( V1Cnrv : 'X' );
072650140722         endif;
072660140722         clear  kNtcNk2;
072670140722         select;
072680140722           // -?Note 02/03?
072690140722           when  wRGR  = 'RN';
072700140722             *in24 = *off;
072710140722             exsr  sr_CarNoteRN;
072720140722           // -?Note 05/06/T5/I5?
072730140722           when  wRGR  = 'RT';
072740140722             *in26 = *off;
072750140722             exsr  sr_CarNoteRT;
072760140722           // -?Note 07/08/T7/I7?
072770140722           when  wRGR  = 'RL';
072780140722             *in31 = *off;
072790140722             exsr  sr_CarNoteRL;
072800140722         endsl;
072810140722
072820140722         // -?Uscita?
072830140722         select;
072840140722           when  wRGR  = 'RN';
072850140722             clear  V3Cnt02;
072860140722             H3riga = 15;
072870140722           when  wRGR  = 'RT';
072880140722             clear  V3Cnt05;
072890140722             H3riga = 17;
072900140722           when  wRGR  = 'RL';
072910140722             clear  V3Cnt07;
072920140722             H3riga = 19;
072930140722         endsl;
072940140722         H3colo = 35;
072950140722
072960140722         *in90 = *on;
072970140722
072980140722       ENDSR;
072990140722
073000140722       //--------------------------------------------------------------
073010140722       //?Visualizzazione Contatti/Rubrica del cliente.                ?
073020140722       //--------------------------------------------------------------
073030140722       BEGSR  sr_CallTNTA12;
073040140722
073050140722         clear  TNTA12ds;
073060140722
073070140722         // -?Impostazione parametri?
073080140722         TA12ric  = wRich;
073090140722         if  *in70  or  Not *in06;
073100140722           TA12apl  = 'C';
073110140722           TA12ksc  = V1Cksc;
073120140722         else;
073130140722           TA12apl  = 'T';
073140140722           TA12nrv  = V1Cnrv;
073150140722         endif;
073160140722         TA12rag  = wRag;
073170140722         TA12tnt  = wTnt;
073180140722         TA12gcli = '1';
073190140722         if  *in01   or
073200140722            (Not *in01  and  Not *in05  and
073210140722             %parms() > 1    and
073220140722             TA60icli = '1');
073230140722           TA12icli = '1';
073240140722           TA12cmm  = %int(V3Cage);
073250140722         endif;
073260140722         TA12rgr  = wRGR;
073270140722
073280140722         // -?Richiamo *pgm TNTA12R?
073290140722         tnta12r ( KPJBA : TNTA12ds );
073300140722
073310140722       ENDSR;
073320140722
073330140722      /end-free
073340051125
073350051125      *------------------------------------------------------------------------*
073360051125      * CONTROLLO CODICE SOTTOCONTO FATTURAZIONE
073370051125      *------------------------------------------------------------------------*
073380051125     c     Sr_Ctrscf     BegSr
073390100127
073400100127      * gestisco il '?'
073410100729     c                   IF        %scan('?':v4cscf) > 0
073420100312      * e se c'è la partita IVA e non è $$
073430100312     c                             and v2civa <> *blanks and
073440100312     c                             v2ivaeu <> '$$'
073450100128     c                   exsr      sr_ta40
073460100127     c                   eval      h4riga = 07
073470100127     c                   eval      h4colo = 36
073480100127     c                   eval      *in90 = *on
073490100312     c                   leavesr
073500100127     c                   ENDIF
073510051125
073520051221      * Obbligatorio
073530100128     c                   If        (v4cscf = *Zeros or v4cscf = *blanks)
073540051221      * se non sono in gestione anagrafica provvisoria
073550051221     c                             and Not *In06
073560051125     c                   Eval      *In28 = *On
073570051130     c                   Eval      h4riga = 07
073580051130     c                   Eval      h4colo = 36
073590051130     c                   Eval      v4cmsg = msg(33)
073600051125     c                   Leavesr
073610051125     c                   EndIf
073620100127
073630100127      * deve essere numerico
073640100127     c                   IF        %check(digits:V4cscf) > 0
073650100127     c                   eval      *in28 = *on
073660100127     c                   eval      h4riga = 07
073670100127     c                   eval      h4colo = 36
073680100127     c                   eval      v4cmsg = msg(35)
073690100127     c                   leavesr
073700100127     c                   ENDIF
073710100316
073720100316      * se sono in immissione (non da trattativa)
073730100316      * non controllo esistenza del codice sottoconto fattura se = al
073740100316      * codice cliente che sto inserendo
073750100316     c                   if        *in01 and not *in06 and
073760100316     c                             %dec(v4cscf:7:0) = v1cksc
073770100316     c                   eval      v4dscf = v2crag
073780100316     c                   leavesr
073790100316     c                   endif
073800051125
073810051125      * Controllo se modificato il codice
073820170117      * o se convalida trattativa
073830100127     c                   If        v4cscf <> savscf
073840170117     c                             or *in07
073850051130     c                   Clear                   v4dscf
073860051125      * Controllo
073870051125     c                   Clear                   Tibs69ds
073880051130     c                   Move      v4cscf        I69kac
073890161129     c                   Move      v4cscf        I69kcp
073900051125     c                   Call      'TIBS69R'
073910051125     c                   Parm                    Tibs69ds
073920051125     c                   Parm                    ds_cnaco
073930051125     c                   Parm                    ds_cnind
073940051125     c                   Parm                    ds_cnclp
073950051125     c                   Parm                    ds_fncls
073960051125     c                   Eval      wtibs69 = *On
073970051125
073980051125      * Il sottoconto deve esistere
073990051125     c                   If        o69err <> *Blanks
074000051125     c                   Eval      *In28 = *On
074010051130     c                   Eval      h4riga = 07
074020051130     c                   Eval      h4colo = 36
074030051130     c                   Eval      v4cmsg = msg(35)
074040051125     c                   Leavesr
074050051125     c                   EndIf
074060051125
074070051125      * Non deve essere annullato
074080051125     c                   If        w_acoflg <> *Blanks
074090051125     c                   Eval      *In28 = *On
074100051130     c                   Eval      h4riga = 07
074110051130     c                   Eval      h4colo = 36
074120051130     c                   Eval      v4cmsg = msg(36)
074130051125     c                   Leavesr
074140051125     c                   EndIf
074150051125
074160051125      * Non si può inserire un sottoconto 8888 o 9999
074170070704      * E' possibile solo per clienti 8888 e 9999
074180070704     c                   Move      v4cscf        w0040
074190070704     c                   move      v1cksc        w0040ksc
074200070704     c                   if        *in06='1'
074210070704     c                             or (w0040ksc=8888 and w0040<>8888)
074220070704     c                             or (w0040ksc=9999 and w0040<>9999)
074230070704     c                             or (w0040ksc<>8888 and w0040ksc<>9999)
074240051125     c                   If        w0040 = 8888 or w0040 = 9999
074250051125     c                   Eval      *In28 = *On
074260051130     c                   Eval      h4riga = 07
074270051130     c                   Eval      h4colo = 36
074280051130     c                   Eval      v4cmsg = msg(34)
074290051125     c                   Leavesr
074300051125     c                   EndIf
074310070704     c                   EndIf
074320161130      /free
074330161130       //?Non posso immettere un codice sottoconto fattura in contenzioso
074340170117        IF  %dec(v4cscf:7:0) <> v1cksc;
074350170117          clear TIBS69DS;
074360170117          I69kcp = %dec(V4Cscf:7:0);
074370170117          tibs69r (TIBS69DS:ds_CNACO:ds_CNIND:ds_CNCLP:ds_FNCLS);
074380170117          wtibs69 = *on;
074390170117          IF  w_CLPcon <> *blanks;
074400170117            clear w_ds4W;
074410170117            kcod = '4W';
074420170117            kkey = w_CLPcon;
074430170117            chain (codut:kcod:kkey) TABEL00F;
074440170117            IF  %found(TABEL00F) and TBLflg = *blanks;
074450170117              w_ds4W = TBLuni;
074460170117            ENDIF;
074470170117            IF  w_§4Wtip <> *blanks;
074480170117              *in28 = *on;
074490170117              H4riga = 07;
074500170117              H4colo = 36;
074510170117              V4Cmsg = msg(73);
074520170117              leavesr;
074530170117            ENDIF;
074540170117          ENDIF;
074550161130        ENDIF;
074560161130      /end-free
074570051125
074580051125      * Cerco il network del codice
074590051130     c                   Movel     v4cscf        w0030
074600051125     c                   Clear                   og143
074610051125     c     w0030         Chain     Azorg01l
074620051125     c                   If        %Found(Azorg01l)
074630051125     c                   Eval      og143 = orgde3
074640051125     c                   EndIf
074650120806      * Se non trovo il network e sono in anafrafica provvisoria vado avanti
074660120806     c                   If        not %found(AZORG01L) and not *in06
074670120806     c                             and Not *In06
074680120806     c                   Eval      *In28 = *On
074690120806     c                   Eval      h4riga = 07
074700120806     c                   Eval      h4colo = 36
074710120806     c                   Eval      v4cmsg = msg(35)
074720120806     c                   Leavesr
074730120806     c                   ENDIF
074740081119     c                   eval      savntwscf = §ogntw
074750160905
074760160905      * Se non sono autorizzato non posso utilizzare lienti con ntw LOG
074770160905     c                   IF         §UTEKscLog <> 'S' and
074780160905     c                              §OGntw = 'LOG'
074790160905     c                   Eval      *In28 = *On
074800160905     c                   Eval      h4riga = 07
074810160905     c                   Eval      h4colo = 36
074820160905     c                   Eval      v4cmsg = msg(34)
074830160905     c                   Leavesr
074840160905     c                   ENDIF
074850051125
074860051125      * Se sono in filiale non posso inserire codici con ntw LOG o XXX
074870051125     c                   If        Not *In09 and
074880160905     c                             §ogntw = 'XXX'
074890051125     c                   Eval      *In28 = *On
074900051130     c                   Eval      h4riga = 07
074910051130     c                   Eval      h4colo = 36
074920051130     c                   Eval      v4cmsg = msg(34)
074930051125     c                   Leavesr
074940051125     c                   EndIf
074950051220
074960051220      * Non posso modificare un sottoconto fattura con p.o. 046
074970051220     c                   Movel     savscf        w0030
074980051220     c                   If        w0030 = 046
074990051220     c                   Eval      *In28 = *On
075000051220     c                   Eval      h4riga = 07
075010051220     c                   Eval      h4colo = 36
075020051220     c                   Eval      v4cmsg = msg(71)
075030051220     c                   Leavesr
075040051220     c                   EndIf
075050051220
075060081119      * Non posso inserire un sottoconto fattura con ntw LOG se ksc non
075070081119      * ha ntw LOG
075080081119     c                   Movel     v1cksc        w0030
075090081119     c                   Clear                   og143
075100081119     c     w0030         Chain     Azorg01l
075110081119     c                   If        %Found(Azorg01l)
075120081119     c                   Eval      og143 = orgde3
075130081119     c                   EndIf
075140081119     c                   if        §ogntw <> 'LOG' and savntwscf = 'LOG'
075150081119     c                   Eval      *In28 = *On
075160081119     c                   Eval      h4riga = 07
075170081119     c                   Eval      h4colo = 36
075180081119     c                   Eval      v4cmsg = msg(34)
075190081119     c                   Leavesr
075200081119     c                   EndIf
075210100127
075220100127      * devo ricontrollare la partita iva e il codice fiscale
075230100127      * perchè i controlli di questi 2 dati sono legati anche al codice
075240100127      * fatturazione
075250100127     c                   exsr      sr_ctrCDF
075260100127      * se torna errore avviso
075270100127     c                   IF        *in28
075280100127     c                   eval      v4cmsg = 'Messaggi in sospeso su -
075290100127     c                             altre videate.'
075300100127     c                   leavesr
075310100127     c                   ENDIF
075320100127     c                   exsr      sr_ctrIVA
075330100127      * se torna errore avviso
075340100127     c                   IF        *in28
075350100127     c                   eval      v4cmsg = 'Messaggi in sospeso su -
075360100127     c                             altre videate.'
075370100127     c                   leavesr
075380100127     c                   ENDIF
075390051125
075400051125      * Decodifico
075410051130     c                   Eval      v4dscf = w_acorag
075420051130     c                   Eval      savscf = v4cscf
075430051128
075440051125     c                   EndIf
075450051125
075460051125     c                   EndSr
075470051128
075480051128      *------------------------------------------------------------------------*
075490051128      * CONTROLLO FLAG FATTURA UNIFICATA
075500051128      *------------------------------------------------------------------------*
075510051128     c     Sr_Ctruni     BegSr
075520100127
075530100127     c                   move      v4cscf        w0070
075540051128
075550051128      * Se il codice sottoconto fatturazione è diverso dal codice cliente e
075560051128      * il flag di unificazione fattura non è stato impostato emetto un msg. inf
075570100127     c                   If        w0070  <> v1cksc and v4cuni = *Blanks and
075580100127     c                             wfscf = *Blanks and w0070  <> *Zeros
075590051128      * e non è da offerta
075600051128     c                             and not *In06
075610051128     c                   Eval      *In28 = *On
075620051130     c                   Eval      h4riga = 08
075630051130     c                   Eval      h4colo = 36
075640051130     c                   Eval      v4cmsg = msg(45)
075650051128     c                   Eval      wfscf = '1'
075660051128     c                   Leavesr
075670051128     c                   EndIf
075680051128
075690051128      * se modificato flag fattura unificata msg info x controllare anche il
075700051128      * flag nel sottoconto fattura
075710051128      * se immissione messaggio solo se codice cliente diverso da sottoconto fat
075720051128      * se manutenzione diversifico i messaggi
075730051130     c                   If        v4cuni <> savuni
075740051130     c                   Eval      savuni = v4cuni
075750051128     c                   Select
075760100127     c                   When      w0070  <> v1cksc
075770051128     c                   Eval      *In28 = *On
075780051130     c                   Eval      h4riga = 08
075790051130     c                   Eval      h4colo = 36
075800051130     c                   Eval      v4cmsg = msg(46)
075810051128     c                   Leavesr
075820100127     c                   When      w0070  = v1cksc and Not *In01
075830051128     c                             and Not *In07
075840051128     c                   Eval      *In28 = *On
075850051130     c                   Eval      h4riga = 08
075860051130     c                   Eval      h4colo = 36
075870051130     c                   Eval      v4cmsg = msg(47)
075880051128     c                   Leavesr
075890051128     c                   EndSl
075900051128     c                   EndIf
075910051128
075920051128     c                   EndSr
075930051125
075940051125      *------------------------------------------------------------------------*
075950051125      * CONTROLLO TIPO FATTURA
075960051125      *------------------------------------------------------------------------*
075970051125     c     Sr_Ctrtft     BegSr
075980051125
075990051125      * Ricerca
076000051130     c                   Clear                   v4dtft
076010051130     c                   If        v4ctft = '?'
076020051125     c                   Clear                   tibs02ds
076030051125     c                   Eval      t02mod = 'R'
076040051125     c                   Eval      t02sif = knsif
076050051125     c                   Eval      t02cod = 'TFT'
076060051125     c                   Call      'TIBS02R'
076070051125     c                   Parm                    kpjba
076080051125     c                   Parm                    tibs02ds
076090051125     c                   If        t02err = *Blanks
076100051130     c                   Eval      v4ctft = t02ke1
076110051125     c                   Eval      dtft = t02uni
076120051130     c                   Eval      v4dtft = §tftdes
076130051130     c                   Eval      h4riga = 09
076140051130     c                   Eval      h4colo = 36
076150051125     c                   Eval      *In90 = *On
076160051125     c                   Leavesr
076170051125     c                   EndIf
076180051128     c                   EndIf
076190051125
076200051125      * --> Controllo (obbligatorio in tabella non esiste il ' ')
076210051125     c                   Clear                   Tibs02ds
076220051125     c                   Eval      t02mod = 'C'
076230051125     c                   Eval      t02sif = knsif
076240051125     c                   Eval      t02cod = 'TFT'
076250051130     c                   Eval      t02ke1 = v4ctft
076260051125     c                   Call      'TIBS02R'
076270051125     c                   Parm                    kpjba
076280051125     c                   Parm                    Tibs02ds
076290051125     c                   If        t02err <> *Blanks
076300051125     c                   Eval      *In28 = *On
076310051130     c                   Eval      h4riga = 09
076320051130     c                   Eval      h4colo = 36
076330051130     c                   Eval      v4cmsg = msg(37)
076340051125     c                   Leavesr
076350051125     c                   EndIf
076360051125     c                   Eval      dtft = t02uni
076370051130     c                   Eval      v4dtft = §tftdes
076380051125
076390051125      * Controllo uso sede/p.o.
076400051125      * solo se variato
076410051130     c                   If        v4ctft <> savtft
076420051125     c                   Select
076430051125     c                   When      Not *In09 and §tftuti = 'S'
076440051125     c                   Eval      *In28 = *On
076450051130     c                   Eval      h4riga = 09
076460051130     c                   Eval      h4colo = 36
076470051130     c                   Eval      v4cmsg = msg(38)
076480051130     c                   Eval      v4cmsg = %trim(v4cmsg) + ' ' +
076490051125     c                             'filiale'
076500051125     c                   Leavesr
076510051125     c                   When      *In09 and §tftuti = 'F'
076520051125     c                   Eval      *In28 = *On
076530051130     c                   Eval      h4riga = 09
076540051130     c                   Eval      h4colo = 36
076550051130     c                   Eval      v4cmsg = msg(38)
076560051130     c                   Eval      v4cmsg = %trim(v4cmsg) + ' ' +
076570051125     c                             'sede'
076580051125     c                   Leavesr
076590051125     c                   EndSl
076600051125     c                   EndIf
076610090417
076620090417     c                   clear                   conta
076630090417
076640090417      * se non è anagrafica provvisoria
076650090417      * se tipo fattura è distinta verifico se il codice è un sottoconto
076660090417      * fattura e se i codici a lui collegati hanno la fatturazione
076670090417      * settimanale, in questo caso devo dare errore
076680090417     c                   if        not *in06 and
076690100127     c                             %editc(v1cksc:'X') = v4cscf
076700100127     c                             and v4ctft = '1'
076710090417     c/exec sql
076720090417     c+ select count(*) into :conta from cnclp00f where
076730090417     c+ clpkcc = 151 and clpscf = :v1cksc and clpfft = 1
076740090417     c/end-exec
076750090417      * se ho trovato almeno un rcd vuol dire che uno dei figli
076760090417      * legati al sottoconto fatturazione che sto modificando ha
076770090417      * la frequenza settimanale
076780090417     c                   if        conta > *zeros
076790090417     c                   eval      *in28 = *on
076800090417     c                   eval      h4riga = 09
076810090417     c                   eval      h4colo = 36
076820090417     c                   eval      v4cmsg = msg(82)
076830090417     c                   leavesr
076840090417     c                   endif
076850090417     c                   endif
076860051125
076870051125     c                   EndSr
076880051125
076890051125      *------------------------------------------------------------------------*
076900051125      * CONTROLLO FREQUENZA FATTURA
076910051125      *------------------------------------------------------------------------*
076920051125     c     Sr_Ctrfft     BegSr
076930051125
076940051125      * Ricerca
076950051130     c                   Clear                   v4dfft
076960051130     c                   If        v4cfft = '?'
076970051125     c                   Eval      kcod = 'FF'
076980051125     c                   call      'TRUL19R'
076990051125     c                   parm                    kcod
077000051125     c                   parm                    ordina
077010051125     c                   parm                    kkey
077020051125     c                   parm                    comando
077030051130     c                   Eval      v4cfft = kkey
077040051130     c                   Eval      h4riga = 10
077050051130     c                   Eval      h4colo = 36
077060051125     c                   Eval      *In90 = *On
077070051125     c                   EndIf
077080051125
077090051125      * --> Controllo (obbligatorio)
077100051130     c                   If        v4cfft = *Blanks
077110051125     c                   Eval      *In28 = *On
077120051130     c                   Eval      h4riga = 10
077130051130     c                   Eval      h4colo = 36
077140051130     c                   Eval      v4cmsg = msg(39)
077150051125     c                   Leavesr
077160051125     c                   EndIf
077170051125
077180051130     c                   Clear                   v4dfft
077190051125     c                   Eval      kcod = 'FF'
077200051130     c                   Eval      kkey = v4cfft
077210051125     c     kTabel        Chain     Tabel00f
077220051125     c                   If        Not %Found(Tabel00f) or
077230051125     c                             tblflg <> *Blanks
077240051125     c                   Eval      *In28 = *On
077250051130     c                   Eval      h4riga = 10
077260051130     c                   Eval      h4colo = 36
077270051130     c                   Eval      v4cmsg = msg(40)
077280051125     c                   Leavesr
077290051125     c                   EndIf
077300051125     c                   Eval      dsff = tbluni
077310051130     c                   Eval      v4dfft = §ffdes
077320051125
077330051125      * Se tipo fattura '1' non è ammessa la frequenza fattura <> dal mensile
077340051220     c                   If        v4ctft = '1' and v4cfft <> '4'
077350051125     c                   Eval      *In28 = *On
077360051130     c                   Eval      h4riga = 10
077370051130     c                   Eval      h4colo = 36
077380051130     c                   Eval      v4cmsg = msg(41)
077390051125     c                   Leavesr
077400051125     c                   EndIf
077410051125
077420120613      * La frequenza fattura settimanale (1) è ammessa solo se
077430120613      * utente abilitato
077440120613     c                   If        v4cfft = '1' and
077450120613     c                             §UTEclpfft <> 'S' and
077460051130     c                             v4cfft <> savfft
077470051125     c                   Eval      *In28 = *On
077480051130     c                   Eval      h4riga = 10
077490051130     c                   Eval      h4colo = 36
077500051130     c                   Eval      v4cmsg = msg(42)
077510051125     c                   Leavesr
077520051125     c                   EndIf
077530090416
077540090416      * se frequenza fattura settimanale controllo il tipo fattura del
077550090416      * sottoconto fatturazione (se diverso)
077560100127     c                   if        v4cfft = '1' and
077570100127     c                             v4cscf <> %editc(v1cksc:'X')
077580100127     c                   move      v4cscf        w0070
077590090416      * il sottoconto fatturazione non deve avere il tipo fattura distinta
077600090417     c/exec sql
077610090417     c+ select clptft into:w_clptft from cnclp00f where
077620100127     c+ clpkcc = 151 and clpksc = :w0070
077630090417     c/end-exec
077640090417     c                   if        sqlcod = 0 and w_clptft = 1
077650090416     c                   eval      *in28 = *on
077660090416     c                   eval      h4riga = 10
077670090416     c                   eval      h4colo = 36
077680090416     c                   eval      v4cmsg = msg(81)
077690090416     c                   leavesr
077700090417     c                   endif
077710090416     c                   endif
077720051125
077730051125     c                   EndSr
077740051125
077750051125      *------------------------------------------------------------------------*
077760051125      * CONTROLLO TIPO DATA FATTURA
077770051125      *------------------------------------------------------------------------*
077780051125     c     Sr_Ctrtdf     BegSr
077790051125
077800051125      * Ricerca
077810051130     c                   Clear                   v4dtdf
077820051130     c                   If        v4ctdf = '?'
077830051125     c                   Eval      kcod = '13'
077840051125     c                   call      'TRUL19R'
077850051125     c                   parm                    kcod
077860051125     c                   parm                    ordina
077870051125     c                   parm                    kkey
077880051125     c                   parm                    comando
077890051130     c                   Eval      v4ctdf = kkey
077900051130     c                   Eval      h4riga = 11
077910051130     c                   Eval      h4colo = 36
077920051125     c                   Eval      *In90 = *On
077930051125     c                   EndIf
077940051125
077950051125      * --> Controllo (obbligatorio)
077960051130     c                   If        v4ctdf = *Blanks
077970051125     c                   Eval      *In28 = *On
077980051130     c                   Eval      h4riga = 11
077990051130     c                   Eval      h4colo = 36
078000051130     c                   Eval      v4cmsg = msg(43)
078010051125     c                   Leavesr
078020051125     c                   EndIf
078030051125
078040051130     c                   Clear                   v4dtdf
078050051125     c                   Eval      kcod = '13'
078060051130     c                   Eval      kkey = v4ctdf
078070051125     c     kTabel        Chain     Tabel00f
078080051125     c                   If        Not %Found(Tabel00f) or
078090051125     c                             tblflg <> *Blanks
078100051125     c                   Eval      *In28 = *On
078110051130     c                   Eval      h4riga = 11
078120051130     c                   Eval      h4colo = 36
078130051130     c                   Eval      v4cmsg = msg(44)
078140051125     c                   Leavesr
078150051125     c                   EndIf
078160051125     c                   Eval      ds13 = tbluni
078170051130     c                   Eval      v4dtdf = §13des
078180051125
078190051125     c                   EndSr
078200051128
078210051128      *------------------------------------------------------------------------*
078220051128      * CONTROLLO SPESE INVIO FATTURA
078230051128      *------------------------------------------------------------------------*
078240051128     c     Sr_Ctrsin     BegSr
078250051128
078260051130     c                   If        v4csin > *Zeros
078270051128     c                   Reset                   xdecds
078280051130     c                   Z-add     v4csin        icdnum
078290051128     c                   Z-add     2             icdnrd
078300051128     c                   Call      'XDEC'
078310051128     c                   Parm                    xdecds
078320051128     c                   If        ocdesi <> *Off
078330051128     c                   Eval      *In28 = *On
078340051130     c                   Eval      h4riga = 12
078350051130     c                   Eval      h4colo = 36
078360051130     c                   Eval      v4cmsg = msg(48)
078370051128     c                   Leavesr
078380051128     c                   EndIf
078390051128     c                   EndIf
078400051128
078410051128     c                   EndSr
078420051128
078430051128      *------------------------------------------------------------------------*
078440051128      * CONTROLLO LUOGO 001 - INVIO FATTURA
078450051128      *------------------------------------------------------------------------*
078460051128     c     Sr_Ctrl001    BegSr
078470051128
078480051130if  1c                   If        v4cl001 <> *Blanks
078490051128
078500060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
078510060203     c                   If        *In05 and Not *In10
078520051128     c                   Eval      *In28 = *On
078530100727     c                   Eval      h4riga = 21
078540060203     c                   Eval      h4colo = 35
078550051130     c                   Eval      v4cmsg = msg(50)
078560051128     c                   Leavesr
078570051128     c                   EndIf
078580071128
078590071128      * se non è interrogazione controllo se luogo utilizzabile
078600071128     c                   if        not *in09 and not *in05 and $ufi001 <> 'S'
078610071128     c                   eval      *In28 = *on
078620100727     c                   eval      h4riga = 21
078630071128     c                   eval      h4colo = 35
078640071128     c                   eval      v4cmsg = msg(76)
078650071128     c                   leavesr
078660071128     c                   endif
078670051128
078680060203      * Imposto se interrogazione o gestione
078690051213     c                   Clear                   tnta81ds
078700060203     c   05              Eval      i81opz = '5'
078710060203     c  n05
078720060203     can 10              Eval      i81opz = '2'
078730051213     c                   Eval      i81cod = '001'
078740051213     c                   Eval      i81cli = v1cksc
078750060216     c                   Clear                   parmf11
078760060216     c                   Eval      i81gcli = '1'
078770060216     c   01              Eval      i81icli = '1'
078780060216     c                   Eval      kpjbu = parmf11
078790051213     c                   Call      'TNTA81R'
078800051213     c                   Parm                    kpjba
078810051213     c                   Parm                    tnta81ds
078820051128
078830051212      * Controllo se ora c'è o non c'è più il luogo
078840051216     c                   Eval      kspecli = v1cksc
078850051216     c                   Eval      kspecod = '001'
078860051128     c     kFnspe        Chain     Fnspe01l
078870051128     c                   If        %Found(Fnspe01l) and speflg = *Blanks
078880051128     c                   Eval      *In10 = *On
078890051212     c                   Else
078900051212     c                   Eval      *In10 = *Off
078910051128     c                   EndIf
078920051128
078930051130     c                   Clear                   v4cl001
078940100727     c                   Eval      h4riga = 21
078950060203     c                   Eval      h4colo = 35
078960051207     c                   Goto      emi04
078970051128
078980051129    1c                   EndIf
078990051128
079000051128     c                   EndSr
079010051122
079020051129      *------------------------------------------------------------------------*
079030051220      * CONTROLLO NOTA 84 - INVIO FATTURA
079040051129      *------------------------------------------------------------------------*
079050051129     c     Sr_Ctrnt84    BegSr
079060051129
079070051130if  1c                   If        v4cnt84 <> *Blanks
079080060203
079090060203      * Se sono in interrogazione non posso interrogare la nota se non c'è
079100060203     c                   If        *In05 and Not *In21
079110060203     c                   Eval      *In28 = *On
079120060203     c                   Eval      h4riga = 21
079130100727     c                   Eval      h4colo = 70
079140060203     c                   Eval      v4cmsg = msg(52)
079150060203     c                   Leavesr
079160060203     c                   EndIf
079170060203
079180060203      * Imposto se interrogazione o gestione
079190060203     c  n05              Eval      wrich = 'M'
079200060203     c   05              Eval      wrich = 'V'
079210060217     c                   Eval      wrag = v3crag
079220051212     c                   Eval      wtnt = '84'
079230051212     c                   ExSr      Sr_f02
079240060215
079250060215     c                   If        ta12err <> *Blanks
079260060215     c                   Eval      *In28 = *On
079270060215     c                   Eval      h4riga = 21
079280100727     c                   Eval      h4colo = 70
079290060215     c                   Eval      v4cmsg = ta12msg
079300060215     c                   Leavesr
079310060215     c                   EndIf
079320051129
079330060215      * Controllo se ora c'è o non c'è più la nota
079340091112     c   70
079350091112     corn06              Eval      kntcapl = 'C'
079360100806     c   06
079370091112     cann70              Eval      kntcapl = 'T'
079380051216     c                   Movel(p)  dutkci        kntcnk1
079390091112     c   70
079400091112     corn06              Move      v1cksc        kntcnk1
079410100806     c   06
079420100806     cann70              Move      v1cnrv        kntcnk1
079430051216     c                   Clear                   kntcnk2
079440051216     c                   Movel     '84'          kntctnt
079450091119     c     kTfntc        Chain(n)  Tfntc01l
079460051129     c                   If        %Found(Tfntc01l)
079470051129     c                   Eval      *In21 = *On
079480051212     c                   Else
079490051212     c                   Eval      *In21 = *Off
079500051129     c                   EndIf
079510051129
079520051130     c                   Clear                   v4cnt84
079530051212     c                   Eval      h4riga = 21
079540100727     c                   Eval      h4colo = 70
079550051212     c                   Goto      emi04
079560051129
079570051129    1c                   EndIf
079580051129
079590051129     c                   EndSr
079600051129
079610051129      *------------------------------------------------------------------------*
079620051130      * CONTROLLO CODICE PAGAMENTO FATTURE
079630051129      *------------------------------------------------------------------------*
079640051129     c     Sr_Ctrcdp     BegSr
079650051129
079660051129      * Ricerca
079670100727     c                   Eval      wpos = %scan('?':v4ccdp)
079680051129     c                   If        wpos > 0
079690100727     c                   Clear                   v4dcdp
079700100727     c                   Clear                   v4ccdp
079710051129     c                   Call      'XCDPAGR'
079720100727     c                   Parm                    v4ccdp
079730100727     c                   Parm                    v4dcdp
079740100728     c                   Eval      h4riga = 13
079750100728     c                   Eval      h4colo = 36
079760051129     c                   Eval      *In90 = *On
079770100727     c                   If        v4ccdp = '999'
079780100727     c                   Clear                   v4ccdp
079790051129     c                   EndIf
079800051129     c                   EndIf
079810051129
079820051129      * Controllo
079830100727     c                   Clear                   v4dcdp
079840051129     c                   Clear                   dsfa
079850051129     c                   Eval      kcod = 'FA'
079860100727     c                   Movel     v4ccdp        w004a
079870051129     c                   Move      '1'           w004a
079880051129     c                   Eval      kkey = w004a
079890051129     c     kTabel        Chain     Tabel00f
079900051129     c                   If        Not %Found(Tabel00f) or
079910051129     c                             tblflg <> *Blanks
079920051129     c                   Eval      *In28 = *On
079930100728     c                   Eval      h4riga = 13
079940100728     c                   Eval      h4colo = 36
079950100728     c                   Eval      v4cmsg = msg(53)
079960051129     c                   Leavesr
079970051129     c                   EndIf
079980051129     c                   Eval      dsfa = tbluni
079990100727     c                   Eval      v4dcdp = §fades
080000051129
080010051129      * Se capoconto cliente non posso utilizzare il codice 3
080020051129     c                   If        v1ckcc = 151 and §fatpg = '3'
080030051129     c                   Eval      *In28 = *On
080040100728     c                   Eval      h4riga = 13
080050100728     c                   Eval      h4colo = 36
080060100728     c                   Eval      v4cmsg = msg(53)
080070051129     c                   Leavesr
080080051129     c                   EndIf
080090080515
080100080515      * se utente di filiale non può inerire un pagamento con codice '2' RID
080110090416      * ora controllo il tipo utilizzo se S è solo SEDE
080120080515     c                   if        not *in09 and
080130100727     c                             v4ccdp <> %subst(indcdp:4:3) and
080140090421     c                             §fauti = 'S'
080150080515     c                   eval      *In28 = *On
080160100728     c                   eval      h4riga = 13
080170100728     c                   eval      h4colo = 36
080180100728     c                   eval      v4cmsg = msg(80)
080190080515     c                   leavesr
080200080515     c                   endif
080210150415      /free
080220160517       //?Se il cliente è sottoconto intestazione fattura
080230160517       //?controllo se OK RIBA
080240160518         IF  %editc(V1Cksc:'X') = V4Cscf and §FAtpg = '1';
080250160518       //?No RIBA (tipo pag. 1) se P.IVA estera
080260160518           IF  (V2ivaeu <> *blanks and V2ivaeu <> '$$' and
080270160518                V2ivaeu <> 'IT' and V2ivaeu <> 'SM') or
080280160518               (V2ivaeu = '$$' and V2Ccdf = *blanks);
080290160517             *in28 = *on;
080300160517             H4riga = 13;
080310160517             H4colo = 36;
080320160517             V4Cmsg = msg(89);
080330160517             leavesr;
080340160517           ENDIF;
080350160517       //?No RIBA (tipo pag. 1) su conto Bancoposta (ABI 07601)
080360160518           IF  V4Cabi = 07601;
080370160517             *in28 = *on;
080380160517             H4riga = 13;
080390160517             H4colo = 36;
080400160517             V4Cmsg = msg(90);
080410160517             leavesr;
080420160518           ENDIF;
080430160517         ENDIF;
080440160517
080450150415       //?Se utente di filiale e tipo pagamento utilizzabile solo da sede
080460150415       //?devo proteggere la Banca ABI/CAB del pagamento fatture
080470150415         *in82 = *off;
080480150415         IF  not *in09 and §FAuti = 'S';
080490150415           *in82 = *on;
080500150415         ENDIF;
080510150415      /end-free
080520051129
080530051129     c                   EndSr
080540051129
080550051129      *------------------------------------------------------------------------*
080560051129      * CONTROLLO BANCA APPOGGIO
080570051129      *------------------------------------------------------------------------*
080580051129     c     Sr_Ctrbna     BegSr
080590070216
080600070216     c                   clear                   wbanca
080610070216     c                   clear                   wagenzia
080620051129
080630051129      * Ricerca
080640100727     c                   Eval      wpos = %scan('?':v4cbna)
080650051129if  1c                   If        wpos > 0
080660100727     c                   Eval      pist = %subst(v4cbna:(wpos+1):(36-wpos))
080670100727     c                   Move      v4cabi        pabi
080680051129     c                   Clear                   pcab
080690051129     c                   Eval      ppro = v2cprv
080700051129     c                   Eval      pflg = 'R'
080710051129     c                   Eval      kpjbu = parmbanca
080720051129     c                   Call      'CNC592R'
080730051129     c                   Parm                    kpjba
080740051129     c                   Eval      parmbanca = kpjbu
080750051129      * Se è stato selezionato un codice
080760051129if  2c                   If        pcab <> *Zeros
080770100727     c                   Movel     pabi          v4cabi
080780100727     c                   Movel     pcab          v4ccab
080790100727     c                   Eval      kabi = v4cabi
080800100727     c                   Eval      kcab = v4ccab
080810100729     c                   exsr      DesBanca
080820100729     c                   eval      v4cbna = wDesBanca
080830051129    2c                   EndIf
080840051129     c                   Eval      *In90 = *On
080850100728     c                   Eval      h4riga = 14
080860100728     c                   Eval      h4colo = 36
080870051129     c                   Leavesr
080880051129    1c                   EndIf
080890051129
080900051129     c                   EndSr
080910051129
080920051129      *------------------------------------------------------------------------*
080930051130      * CONTROLLO ABI/CAB RELATIVO AL PAGAMENTO FATTURE
080940051129      *------------------------------------------------------------------------*
080950051130     c     Sr_Ctrabicabf BegSr
080960051129
080970051129      * Se non è da gestione visite/offerte controllo se abi e cab
080980051129      * obbligatori per pagamento con RB
080990051129     c                   If        Not *In06 and §fatpg = '1' and
081000100727     c                             v4cabi = *Zeros and v4ccab = *Zeros
081010051129     c                   Eval      *In28 = *On
081020100728     c                   Eval      h4riga = 15
081030100728     c                   Eval      h4colo = 36
081040100728     c                   Eval      v4cmsg = msg(54)
081050051129     c                   Leavesr
081060051129     c                   EndIf
081070090331
081080090331      * se pagamento con RB codice '1' non accetto ABI >= 90000
081090090331     c                   If        §fatpg = '1' and
081100100727     c                             v4cabi >= 90000
081110090331     c                   Eval      *In28 = *On
081120100728     c                   Eval      h4riga = 15
081130100728     c                   Eval      h4colo = 36
081140100728     c                   Eval      v4cmsg = msg(56)
081150090331     c                   Leavesr
081160090331     c                   EndIf
081170051130
081180051130      * Controllo
081190100727     c                   Eval      kabi = v4cabi
081200100727     c                   Eval      kcab = v4ccab
081210051130     c                   ExSr      Sr_Ctrabicab
081220051130     c                   If        *In28 = *On
081230100728     c                   Eval      h4riga = 15
081240100728     c                   Eval      h4colo = 36
081250051130     c                   Leavesr
081260051130     c                   EndIf
081270051130      * Creo descrizione della banca di appoggio
081280100727     c                   If        v4cabi <> 99999 or v4ccab <> 99999
081290100727     c                   Movel     wbanca        v4cbna
081300100727     c                   Move      wagenzia      v4cbna
081310051130     c                   EndIf
081320051130
081330051130     c                   EndSr
081340150424
081350150424      /free
081360150424       //--------------------------------------------------------------
081370150424       //?Controllo i pagamenti C/A - Danni - N.A.
081380150424       //--------------------------------------------------------------
081390150424       BEGSR CtrPagamenti;
081400150424
081410150424       //?Pagamenti CONTRASSEGNI
081420150424         clear ds4U;
081430150424         kcod = '4U';
081440150424         kkey = V5Cfpc;
081450150424         chain (codut:kcod:kkey) TABEL00F;
081460150424         IF  %found(TABEL00F);
081470150424           ds4U = TBLuni;
081480150424         ENDIF;
081490151112         IF  V5Cfpc <> '1';
081500150427         IF  not wForzaPag and §4Utip = 'I' and
081510150427            (§4Ucod <> 'B' or V5Ciban = *blanks);
081520150424           *in28 = *on;
081530150424           H5riga = 09;
081540150424           H5colo = 36;
081550150424           V5Cmsg = msg(87);
081560150424           wForzaPag = *on;
081570150424           leavesr;
081580150424         ENDIF;
081590151112         ENDIF;
081600150424
081610150424       //?Pagamenti DANNI
081620150424         clear ds4U;
081630150424         kcod = '4U';
081640150424         kkey = V5tpdn;
081650150424         chain (codut:kcod:kkey) TABEL00F;
081660150424         IF  %found(TABEL00F);
081670150424           ds4U = TBLuni;
081680150424         ENDIF;
081690151112         IF  V5tpdn <> '1';
081700150427         IF  not wForzaPag and §4Utip = 'I' and
081710150427            (§4Ucod <> 'B' or V5ibandn = *blanks);
081720150424           *in28 = *on;
081730150424           H5riga = 12;
081740150424           H5colo = 36;
081750150424           V5Cmsg = msg(87);
081760150424           wForzaPag = *on;
081770150424           leavesr;
081780150424         ENDIF;
081790151112         ENDIF;
081800150424
081810150424       //?Pagamenti NOTE ACCREDITO
081820150424         clear ds4U;
081830150424         kcod = '4U';
081840150424         kkey = V5tpna;
081850150424         chain (codut:kcod:kkey) TABEL00F;
081860150424         IF  %found(TABEL00F);
081870150424           ds4U = TBLuni;
081880150424         ENDIF;
081890151112         IF  V5tpna <> '1';
081900150427         IF  not wForzaPag and §4Utip = 'I' and
081910150427            (§4Ucod <> 'B' or V5ibanna = *blanks);
081920150424           *in28 = *on;
081930150424           H5riga = 15;
081940150427           H5colo = 37;
081950150424           V5Cmsg = msg(87);
081960150424           wForzaPag = *on;
081970150424           leavesr;
081980150424         ENDIF;
081990151112         ENDIF;
082000150424
082010150424       ENDSR;
082020150728
082030150728       //--------------------------------------------------------------
082040150728       //?Controllo i dati per la mail invio progetto di liquidazione.
082050150728       //--------------------------------------------------------------
082060150728       BEGSR CtrMailDanni;
082070150728
082080150728       //?Se esiste la nota 87 OK
082090150728         IF  *in18;
082100150728           leavesr;
082110150728         ENDIF;
082120150728
082130150728       //?Se esiste il luogo 008 e c'è la mail OK
082140150728         IF  *in17;
082150150728           kSPEcli = V1Cksc;
082160150728           kSPEcod = '008';
082170150728           chain (kSPEcli:kSPEcod:kSP2tpe) FNSP201L;
082180150728           IF  %Found(FNSP201L) and SP2flg = *blanks;
082190150728             leavesr;
082200150728           ENDIF;
082210150728         ENDIF;
082220150728
082230150728       //?Se arrivo qua vuol dire che non c'è la nota 87 oppure
082240150728       //?non c'è il luogo 008 o il luogo 008 non ha la mail
082250150728         IF  not wForzaMail;
082260150728           *in28 = *on;
082270150728           H7riga = 13;
082280150728           H7colo = 35;
082290150728           V7Cmsg = msg(88);
082300150728           wForzaMail = *on;
082310150728         ENDIF;
082320150728
082330150728       ENDSR;
082340150424      /end-free
082350051130
082360051130      *------------------------------------------------------------------------*
082370051130      * CONTROLLO CODICE PAGAMENTO CONTRASSEGNI
082380051130      *------------------------------------------------------------------------*
082390051130     c     Sr_Ctrfpc     BegSr
082400051219
082410051219     c                   Eval      *In22 = *Off
082420051219     c                   Eval      *In23 = *Off
082430051130
082440051130      * Ricerca
082450051130     c                   Clear                   v5dfpc
082460051130     c                   If        v5cfpc = '?'
082470051130     c                   Eval      kcod = '4U'
082480051130     c                   call      'TRUL19R'
082490051130     c                   parm                    kcod
082500051130     c                   parm                    ordina
082510051130     c                   parm                    kkey
082520051130     c                   parm                    comando
082530051130     c                   Eval      v5cfpc = kkey
082540100729     c                   Eval      h5riga = 09
082550100802     c                   Eval      h5colo = 36
082560051130     c                   Eval      *In90 = *On
082570051130     c                   EndIf
082580051130
082590051130      * Controllo
082600051130     c                   Clear                   v5dfpc
082610051130     c                   Clear                   ds4u
082620051130     c                   Eval      kcod = '4U'
082630051130     c                   Eval      kkey = v5cfpc
082640051130     c     kTabel        Chain     Tabel00f
082650051130     c                   If        Not %Found(Tabel00f) or
082660051130     c                             tblflg <> *Blanks
082670051130     c                   Eval      *In28 = *On
082680100729     c                   Eval      h5riga = 09
082690100802     c                   Eval      h5colo = 36
082700051130     c                   Eval      v5cmsg = msg(57)
082710051130     c                   Leavesr
082720051130     c                   EndIf
082730051130     c                   Eval      ds4u = tbluni
082740051130     c                   Eval      v5dfpc = §4udes
082750051130
082760051130      * Controllo se utilizzabile per i clienti vari
082770051130     c                   If        (wksc04 = 8888 or wksc04 = 9999) and
082780051130     c                              §4uvar = 'N'
082790051130     c                   Eval      *In28 = *On
082800100729     c                   Eval      h5riga = 09
082810100802     c                   Eval      h5colo = 36
082820051130     c                   Eval      v5cmsg = msg(58)
082830051130     c                   Leavesr
082840051130     c                   EndIf
082850151030
082860151030      /free
082870151030       //?Errore se utilizzabile solo da sede e NON sono sede
082880151112         IF  §4Usede <> *blanks and not *in09 and v5cfpc <> CLPfpc;
082890151030           *in28 = *on;
082900151030           H5riga = 09;
082910151030           H5colo = 36;
082920151030           V5Cmsg = msg(57);
082930151030           V5Cmsg = %trim(V5Cmsg) + '. Utilizzabili SOLO da SEDE';
082940151030           leavesr;
082950151030         ENDIF;
082960151030      /end-free
082970051219
082980051219      * Le coordinate bancare le visualizzo o le proteggo in base al
082990051219      * tipo pagamento
083000051219     c                   Select
083010051219      * Coordinate italiane, visualizzo
083020051219      * Immissione coordinate non richiesta, non visualizzo
083030051219     c                   When      §4uabi = 'N'
083040051219     c                   Eval      *In22 = *On
083050080116      * pulisco le coordinate bancarie
083060080116     c                   clear                   v5ciban
083070051219      * Se pagamento estero proteggo le coordinate bancarie perchè
083080051219      * gestite dalla sede.
083090080115     c                   when      §4utip = 'E'
083100051219     c                   Eval      *In23 = *On
083110051219     c                   EndSl
083120100729      * se il tipo pagamento del codice impostato a video è diverso da quello
083130100729      * memorizzato sul file ed è estero devo pulire il campo relativo all'IBAN
083140100729      * e avvisare con window per inviare mail alla sede
083150100729     c                   if        §4utip <> sav4utip and §4utip = 'E'
083160100729     c                   if        v5ciban <> *blanks
083170100729     c                   clear                   v5ciban
083180100729     c                   endif
083190100729      * window per inviare mail alla sede
083200100729      * solo se tipo pagamento prevede le coordinate bancarie
083210100729     c                   if        not $win and §4uabi = 'S'
083220101014     c                   eval      wuffsede = '"CONTRASSEGNI" di sede'
083230100729     c                   exsr      sr_winmail
083240100729     c                   eval      $win = *on
083250100729     c                   eval      *in90 = *on
083260100729     c                   leavesr
083270100729     c                   endif
083280100729     c                   endif
083290051130
083300051130     c                   EndSr
083310100729
083320100729      /free
083330100729       //--------------------------------------------------------------
083340100729       //?Controllo pagamento Danni.
083350100729       //--------------------------------------------------------------
083360100729       BEGSR CtrPagDN;
083370100729
083380100729         *in71 = *off;
083390100729         *in72 = *off;
083400100729
083410100729       //?Ricerco il codice pagamento
083420100729         clear v5dpdn;
083430100729         IF  %scan('?':v5tpdn) > 0;
083440100729           kcod = '4U';
083450100729           Trul19R (kcod:ordina:kkey:comando);
083460100729           v5tpdn = kkey;
083470100730           h5riga = 12;
083480100802           h5colo = 36;
083490100729           *in90 = *on;
083500100729         ENDIF;
083510100729
083520100729       //?Controllo
083530100729         clear v5dpdn;
083540100729         clear ds4u;
083550100729         kcod = '4U';
083560100729         kkey = v5tpdn;
083570100729         chain (codut:kcod:kkey) TABEL00F;
083580100729         IF  not %Found(TABEL00F) or TBLflg <> *blanks;
083590100729           *in28 = *on;
083600100730           h5riga = 12;
083610100802           h5colo = 36;
083620100729           v5cmsg = msg(57);
083630100729           leavesr;
083640100729         ENDIF;
083650100729         ds4u = TBLuni;
083660100730         v5dpdn = §4Udes;
083670100729
083680100729       //?Controllo se posso utilizzare per clienti vari
083690110131         IF  (wksc04 = 8888 or wksc04 = 9999) and §4Uvar = 'N';
083700100729           *in28 = *on;
083710100730           h5riga = 12;
083720100802           h5colo = 36;
083730100729           v5cmsg = msg(58);
083740100729           leavesr;
083750100729         ENDIF;
083760151030
083770151030       //?Errore se utilizzabile solo da sede e NON sono sede
083780151112         IF  §4Usede <> *blanks and not *in09 and
083790151112             v5tpdn <> %subst(CLStic:1:1);
083800151030           *in28 = *on;
083810151030           H5riga = 12;
083820151030           H5colo = 36;
083830151030           V5Cmsg = msg(57);
083840151030           V5Cmsg = %trim(V5Cmsg) + '. Utilizzabili SOLO da SEDE';
083850151030           leavesr;
083860151030         ENDIF;
083870151030
083880100729       //?Visualizzo il campo iban se coordinate bancarie obbligatorie
083890100730         *in71 = (§4Uabi = 'N');
083900100729
083910100729       //?Controllo se posso utilizzare in base alla tabella PAG
083920100729         clear TIBS02DS;
083930100729         clear dPAG;
083940100729         T02mod = 'C';
083950100729         T02cod = 'PAG';
083960100729         T02ke1 = 'DN';
083970100729         T02ke2 = v5tpdn;
083980100729         T02sif = KNSIF;
083990100729         TNTBE_RicercaControllo (kpjba : tibs02ds);
084000100729         IF  T02err <> *blanks;
084010100729           *in28 = *on;
084020100730           h5riga = 12;
084030100802           h5colo = 36;
084040100729           v5cmsg = msg(85);
084050100729           leavesr;
084060100729         ENDIF;
084070100729         IF  T02err = *blanks;
084080100729           dPAG = T02uni;
084090100729         ENDIF;
084100100729       //?controllo se la filiale può utilizzare il pagamento
084110151112         IF  d§PAGuti = 'NO' and not *in09 and
084120151112             v5tpdn <> %subst(CLStic:1:1);
084130100729           *in28 = *on;
084140100730           h5riga = 12;
084150100802           h5colo = 36;
084160100729           v5cmsg = msg(80);
084170100729           leavesr;
084180100729         ENDIF;
084190100730       //?proteggo il campo IBAN se pagamento estero
084200100730         *in72 = (§4Utip = 'E');
084210100909
084220100909       //?se pagamento con bonifico italia posso utilizzarlo solo
084230100909       //?per clienti 'T - D - A'
084240150506         //IF  not *in72 and d§PAGctr = 'SI' and
084250150506         //    v3cclv <> 'T' and  v3cclv <> 'D' and
084260150506         //    v3cclv <> 'A';
084270150506         //  *in28 = *on;
084280150506         //  h5riga = 12;
084290150506         //  h5colo = 36;
084300150506         //  v5cmsg = msg(85);
084310150506         //  v5cmsg = %trim(v5cmsg) + ' per clienti NON di tipo T-D-A';
084320150506         //  leavesr;
084330150506         //  ENDIF;
084340100730
084350100802       //?se è stato modificato il tipo pagamento
084360100909       //?e quello inserito non richiede le coordinate bancarie pulisco
084370100802       //?il campo dell'IBAN
084380100802         IF  v5tpdn <> savtipdn and d§PAGobb <> 'SI';
084390100802           clear v5ibandn;
084400100805           clear v5bicdn;
084410100802       //?se il tipo pagamento inserito è un tipo pagamento che lo richiede
084420100802       //?visualizzo window per invio mail
084430100802           IF  d§PAGobb = 'MA' and not $winDN;
084440101014             wuffsede = '"GESTIONE PAGAMENTI" di sede';
084450100802             exsr sr_winmail;
084460100802             $winDN = *on;
084470100802             *in90 = *on;
084480100802             leavesr;
084490100802           ENDIF;
084500100730         ENDIF;
084510100729
084520100729       ENDSR;
084530100729
084540100729       //--------------------------------------------------------------
084550100729       //?Controllo pagamento Note Accredito.
084560100729       //--------------------------------------------------------------
084570100729       BEGSR CtrPagNA;
084580100730
084590100730         *in73 = *off;
084600100730         *in74 = *off;
084610100730
084620100730       //?Ricerco il codice pagamento
084630100730         clear v5dpna;
084640100730         IF  %scan('?':v5tpna) > 0;
084650100730           kcod = '4U';
084660100730           Trul19R (kcod:ordina:kkey:comando);
084670100916           v5tpna = kkey;
084680100730           h5riga = 15;
084690150427           h5colo = 37;
084700100730           *in90 = *on;
084710100730         ENDIF;
084720100730
084730100730       //?Controllo
084740100730         clear v5dpna;
084750100730         clear ds4u;
084760100730         kcod = '4U';
084770100730         kkey = v5tpna;
084780100730         chain (codut:kcod:kkey) TABEL00F;
084790100730         IF  not %Found(TABEL00F) or TBLflg <> *blanks;
084800100730           *in28 = *on;
084810100730           h5riga = 15;
084820150427           h5colo = 37;
084830100730           v5cmsg = msg(57);
084840100730           leavesr;
084850100730         ENDIF;
084860100730         ds4u = TBLuni;
084870100730         v5dpna = §4Udes;
084880100730
084890100730       //?Controllo se posso utilizzare per clienti vari
084900110131         IF  (wksc04 = 8888 or wksc04 = 9999) and §4Uvar = 'N';
084910100730           *in28 = *on;
084920100730           h5riga = 15;
084930150427           h5colo = 37;
084940100730           v5cmsg = msg(58);
084950100730           leavesr;
084960100730         ENDIF;
084970151030
084980151030       //?Errore se utilizzabile solo da sede e NON sono sede
084990151112         IF  §4Usede <> *blanks and not *in09 and
085000151112             v5tpna <> %subst(CLStic:2:1);
085010151030           *in28 = *on;
085020151030           H5riga = 15;
085030151030           H5colo = 37;
085040151030           V5Cmsg = msg(57);
085050151030           V5Cmsg = %trim(V5Cmsg) + '. Utilizzabili SOLO da SEDE';
085060151030           leavesr;
085070151030         ENDIF;
085080151030
085090100730       //?Visualizzo il campo iban se coordinate bancarie obbligatorie
085100100730         *in73 = (§4Uabi = 'N');
085110100730
085120100730       //?Controllo se posso utilizzare in base alla tabella PAG
085130100730         clear TIBS02DS;
085140100730         clear dPAG;
085150100730         T02mod = 'C';
085160100730         T02cod = 'PAG';
085170100730         T02ke1 = 'NA';
085180100730         T02ke2 = v5tpna;
085190100730         T02sif = KNSIF;
085200100730         TNTBE_RicercaControllo (kpjba : tibs02ds);
085210100730         IF  T02err <> *blanks;
085220100730           *in28 = *on;
085230100730           h5riga = 15;
085240150427           h5colo = 37;
085250100730           v5cmsg = msg(85);
085260100730           leavesr;
085270100730         ENDIF;
085280100730         IF  T02err = *blanks;
085290100730           dPAG = T02uni;
085300100730         ENDIF;
085310100730       //?controllo se la filiale può utilizzare il pagamento
085320151112         IF  d§PAGuti = 'NO' and not *in09 and
085330151112             v5tpna <> %subst(CLStic:2:1);
085340100730           *in28 = *on;
085350100730           h5riga = 15;
085360150427           h5colo = 37;
085370100730           v5cmsg = msg(80);
085380100730           leavesr;
085390100730         ENDIF;
085400100730       //?proteggo il campo IBAN se pagamento estero
085410100730         *in74 = (§4Utip = 'E');
085420100730
085430100802       //?se è stato modificato il tipo pagamento
085440100909       //?e quello inserito non richiede le coordinate bancarie pulisco
085450100802       //?il campo dell'IBAN
085460100802         IF  v5tpna <> savtipna and d§PAGobb <> 'SI';
085470100802           clear v5ibanna;
085480100805           clear v5bicna;
085490100802       //?se il tipo pagamento inserito è un tipo pagamento che lo richiede
085500100802       //?visualizzo window per invio mail
085510100802           IF  d§PAGobb = 'MA' and not $winNA;
085520101220             wuffsede = '"GESTIONE PAGAMENTI" di sede';
085530100730             exsr sr_winmail;
085540100730             $winNA = *on;
085550100730             *in90 = *on;
085560100730             leavesr;
085570100730           ENDIF;
085580100730         ENDIF;
085590100729
085600100729       ENDSR;
085610100729      /end-free
085620080115
085630080115      *------------------------------------------------------------------------*
085640080115      * CONTROLLO CODICE IBAN
085650080115      *------------------------------------------------------------------------*
085660080115     c     sr_ctriban    begsr
085670080115
085680080115     c                   clear                   wbanca
085690080115     c                   clear                   wagenzia
085700080422
085710080422      * se tipo pagamento contrassegno italia posso accettare solo
085720090612      * un IBAN italia (IT)
085730080422     c                   if        §4utip = 'I' and
085740100729     c                             %subst(wiban:1:2) <> 'IT'
085750150609     c                             and %subst(wiban:1:2) <> 'SM'
085760150424     c                             and %subst(wiban:1:2) <> *blanks
085770080422     c                   eval      *in28 = *on
085780080422     c                   eval      v5cmsg = msg(79)
085790080422     c                   leavesr
085800080422     c                   endif
085810080116
085820080116      * richiamo programma di controllo codice IBAN (Proj)
085830080116     c                   clear                   parmiban
085840100730     c                   eval      parmstato = %subst(wiban:1:2)
085850100730     c                   eval      parmcheck = %subst(wiban:3:2)
085860100730     c                   eval      parmcoori = %subst(wiban:5:28)
085870080116     c                   call      'XCHKIBAN'
085880080116     c                   parm                    parmstato
085890080116     c                   parm                    parmcheck
085900080116     c                   parm                    parmcoori
085910080116     c                   parm                    erriban
085920080116      * se ritorna errore messaggio
085930080116     c                   if        erriban = *on
085940080116     c                   eval      *in28 = *on
085950080116     c                   eval      v5cmsg = msg(59)
085960080116     c                   leavesr
085970080116     c                   endif
085980080116      * decodifico la banca
085990100729     c                   clear                   wdesbanca
086000100729     c                   eval      kabi = %dec(%subst(wiban:6:5):5:0)
086010100729     c                   eval      kcab = %dec(%subst(wiban:11:5):5:0)
086020080116     c     kcnabi        chain     cnabi00f
086030080116     c                   if        %found(cnabi00f) and abiann <> '*'
086040080116      * compongo la descrizione della banca d'appoggio
086050080116     c                   exsr      sr_crebna
086060100730     c                   eval      wdesbanca = wbanca + wagenzia
086070080116     c                   endif
086080080115
086090080115     c                   endsr
086100051130
086110051130      *-------------------------------------------------------------------------
086120051130      *  Creo la descrizione della banca di appoggio in base ai codici ABI/CAB
086130051130      *-------------------------------------------------------------------------
086140051130     c     Sr_Crebna     BegSr
086150051130
086160051130if  1c                   If        abiabi <> 99999 or abicab <> 99999
086170051130     c                   Clear                   wagenzia
086180060221     c                   Clear                   wbanca
086190051130     c                   Eval      wbanca = abiist
086200051130      * Compongo il nome della banca con località o comune
086210051130if  2c                   If        abiage = *Blanks
086220051130     c                   Select
086230051130     c                   When      abiloc <> *Blanks
086240051130     c                   Eval      wagenzia = abiloc
086250051130     c                   When      abicom <> *Blanks
086260051130     c                   Eval      wagenzia = abicom
086270051130     c                   EndSl
086280051130      * Compongo il nome della banca con agenzia + località o comune
086290051130   x2c                   Else
086300051130      * Cerco il primo byte vuoto dell'agenzia
086310051130     c                   Eval      wpos = %checkr(' ':abiage)
086320051130
086330051130     c                   Eval      wagenzia = %subst(abiage:1:wpos)
086340051130     c                   Eval      wpos1 = wpos + 2
086350051130     c                   Eval      wpos2 = 16 - wpos1
086360060215      * Se wpos2 inferiore a zero lo imposto a 0
086370060215     c                   If        wpos2 < *Zeros
086380060215     c                   Clear                   wpos2
086390060215     c                   EndIf
086400051130      * Se posso aggiungere anche solo 1 carattere procedo
086410051130     c                   If        wpos > 1 and wpos < 16
086420051130      * Cerco di riempire quello che resta con la località o il comune
086430051130     c                   Select
086440051130     c                   When      abiloc <> *Blanks
086450060221     c                   Eval      wagenzia = %trim(wagenzia) + ' ' +
086460060221     c                             %subst(abiloc:1:wpos2)
086470051130     c                   When      abicom <> *Blanks
086480060221     c                   Eval      wagenzia = %trim(wagenzia) + ' ' +
086490060221     c                             %subst(abicom:1:wpos2)
086500051130     c                   EndSl
086510051130     c                   EndIf
086520051130
086530051130    2c                   EndIf
086540051130    1c                   EndIf
086550051130
086560051130     c                   EndSr
086570051207
086580051130      *------------------------------------------------------------------------*
086590051130      * CONTROLLO ABI/CAB GENERICO
086600051130      *------------------------------------------------------------------------*
086610051130     c     Sr_Ctrabicab  BegSr
086620051130
086630051130      * Controllo ABI/CAB inseriti
086640051130     c                   If        kabi = *Zeros and kcab = *Zeros
086650051130     c                   Leavesr
086660051130     c                   EndIf
086670051130
086680051130      * Se immesso il CAB immettere anche ABI
086690051130     c                   If        (kabi = *Zeros and kcab <> *Zeros) or
086700051130      * Se immesso il ABI immettere anche CAB
086710051130     c                             (kabi <> *Zeros and kcab = *Zeros)
086720051130     c                   Eval      *In28 = *On
086730101111     c                   Eval      v4cmsg = msg(56)
086740051130     c                   Leavesr
086750051130     c                   EndIf
086760051130      * Controllo se validi
086770051130     c     kCnabi        Chain     Cnabi00f
086780051130     c                   If        Not %Found(Cnabi00f) or abiann = '*'
086790051130     c                   Eval      *In28 = *On
086800101111     c                   Eval      v4cmsg = msg(56)
086810051130     c                   Leavesr
086820051130     c                   EndIf
086830051130
086840051130      * Compongo la descrizione della banca d'appoggio
086850051130     c                   ExSr      Sr_Crebna
086860051130
086870051130     c                   EndSr
086880051129
086890051206
086900051206      *------------------------------------------------------------------------*
086910051206      * CONTROLLO LUOGO 555 - INTESTAZIONE PAGAMENTO C/ASSEGNI
086920051206      *------------------------------------------------------------------------*
086930051206     c     Sr_Ctrl555    BegSr
086940051206
086950051206if  1c                   If        v5cl555 <> *Blanks
086960060203
086970060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
086980060203     c                   If        *In05 and Not *In11
086990051206     c                   Eval      *In28 = *On
087000100730     c                   Eval      h5riga = 19
087010060203     c                   Eval      h5colo = 35
087020051207     c                   Eval      v5cmsg = msg(50)
087030051206     c                   Leavesr
087040051206     c                   EndIf
087050071128
087060071128      * se non è interrogazione controllo se luogo utilizzabile
087070071128     c                   if        not *in09 and not *in05 and $ufi555 <> 'S'
087080071128     c                   eval      *In28 = *on
087090100730     c                   eval      h5riga = 19
087100071128     c                   eval      h5colo = 35
087110071128     c                   eval      v5cmsg = msg(76)
087120071128     c                   leavesr
087130071128     c                   endif
087140051206
087150060203      * Imposto se interrogazione o gestione
087160051213     c                   Clear                   tnta81ds
087170060203     c   05              Eval      i81opz = '5'
087180060203     c  n05
087190060214     can 11              Eval      i81opz = '2'
087200051213     c                   Eval      i81cod = '555'
087210051213     c                   Eval      i81cli = v1cksc
087220060216     c                   Eval      i81gcli = '1'
087230060216     c   01              Eval      i81icli = '1'
087240051213     c                   Call      'TNTA81R'
087250051213     c                   Parm                    kpjba
087260051213     c                   Parm                    tnta81ds
087270051206
087280051212      * Controllo se ora c'è o non c'è più il luogo
087290051216     c                   Eval      kspecli = v1cksc
087300051216     c                   Eval      kspecod = '555'
087310051206     c     kFnspe        Chain     Fnspe01l
087320051206     c                   If        %Found(Fnspe01l) and speflg = *Blanks
087330051212     c                   Eval      *In11 = *On
087340051212     c                   Else
087350051212     c                   Eval      *In11 = *Off
087360051206     c                   EndIf
087370051206
087380051206     c                   Clear                   v5cl555
087390100730     c                   Eval      h5riga = 19
087400060203     c                   Eval      h5colo = 35
087410051207     c                   Goto      emi05
087420051206
087430051206    1c                   EndIf
087440051206
087450051206     c                   EndSr
087460051207
087470051207      *------------------------------------------------------------------------*
087480051212      * CONTROLLO LUOGO 666 - LUOGO INVIO C/ASSEGNO MITTENTE
087490051207      *------------------------------------------------------------------------*
087500051207     c     Sr_Ctrl666    BegSr
087510051212
087520051212if  1c                   If        v5cl666 <> *Blanks
087530060203
087540060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
087550060203     c                   If        *In05 and Not *In12
087560051212     c                   Eval      *In28 = *On
087570100730     c                   Eval      h5riga = 20
087580060203     c                   Eval      h5colo = 35
087590051212     c                   Eval      v5cmsg = msg(50)
087600051212     c                   Leavesr
087610051212     c                   EndIf
087620071128
087630071128      * se non è interrogazione controllo se luogo utilizzabile
087640071128     c                   if        not *in09 and not *in05 and $ufi666 <> 'S'
087650071128     c                   eval      *In28 = *on
087660100730     c                   eval      h5riga = 20
087670071128     c                   eval      h5colo = 35
087680071128     c                   eval      v5cmsg = msg(76)
087690071128     c                   leavesr
087700071128     c                   endif
087710051212
087720060203      * Imposto se interrogazione o gestione
087730051213     c                   Clear                   tnta81ds
087740060203     c   05              Eval      i81opz = '5'
087750060203     c  n05
087760060214     can 12              Eval      i81opz = '2'
087770051213     c                   Eval      i81cod = '666'
087780051213     c                   Eval      i81cli = v1cksc
087790060216     c                   Eval      i81gcli = '1'
087800060216     c   01              Eval      i81icli = '1'
087810051213     c                   Call      'TNTA81R'
087820051213     c                   Parm                    kpjba
087830051213     c                   Parm                    tnta81ds
087840051212
087850051212      * Controllo se ora c'è o non c'è più il luogo
087860051216     c                   Eval      kspecli = v1cksc
087870051216     c                   Eval      kspecod = '666'
087880051212     c     kFnspe        Chain     Fnspe01l
087890051212     c                   If        %Found(Fnspe01l) and speflg = *Blanks
087900051212     c                   Eval      *In12 = *On
087910051212     c                   Else
087920051212     c                   Eval      *In12 = *Off
087930051212     c                   EndIf
087940051212
087950051212     c                   Clear                   v5cl666
087960100730     c                   Eval      h5riga = 20
087970060203     c                   Eval      h5colo = 35
087980051212     c                   Goto      emi05
087990051212
088000051212    1c                   EndIf
088010051207
088020051207     c                   EndSr
088030051206
088040051206      *------------------------------------------------------------------------*
088050051220      * CONTROLLO NOTA 85 - E-MAIL BONIFICI C/ASSEGNI
088060051206      *------------------------------------------------------------------------*
088070051212     c     Sr_Ctrnt85    BegSr
088080051206
088090051212if  1c                   If        v5cnt85 <> *Blanks
088100060203
088110060203      * Se sono in interrogazione non posso interrogare la nota se non c'è
088120060203     c                   If        *In05 and Not *In13
088130060203     c                   Eval      *In28 = *On
088140100730     c                   Eval      h5riga = 19
088150100730     c                   Eval      h5colo = 71
088160060203     c                   Eval      v5cmsg = msg(52)
088170060203     c                   Leavesr
088180060203     c                   EndIf
088190060203
088200060203      * Imposto se interrogazione o gestione
088210060203     c  n05              Eval      wrich = 'M'
088220060203     c   05              Eval      wrich = 'V'
088230060217     c                   Eval      wrag = v3crag
088240051212     c                   Eval      wtnt = '85'
088250051212     c                   ExSr      Sr_f02
088260060215
088270060215     c                   If        ta12err <> *Blanks
088280060215     c                   Eval      *In28 = *On
088290100730     c                   Eval      h5riga = 19
088300100730     c                   Eval      h5colo = 71
088310060215     c                   Eval      v5cmsg = ta12msg
088320060215     c                   Leavesr
088330060215     c                   EndIf
088340051206
088350051212      * Controllo se ora c'è o non c'è più la nota
088360091112     c   70
088370091112     corn06              Eval      kntcapl = 'C'
088380100806     c   06
088390091112     cann70              Eval      kntcapl = 'T'
088400051216     c                   Movel(p)  dutkci        kntcnk1
088410091112     c   70
088420091112     corn06              Move      v1cksc        kntcnk1
088430100806     c   06
088440100806     cann70              Move      v1cnrv        kntcnk1
088450051216     c                   Clear                   kntcnk2
088460051216     c                   Movel     '85'          kntctnt
088470091119     c     kTfntc        Chain(n)  Tfntc01l
088480051206     c                   If        %Found(Tfntc01l)
088490051212     c                   Eval      *In13 = *On
088500051212     c                   Else
088510051212     c                   Eval      *In13 = *Off
088520051206     c                   EndIf
088530051206
088540051212     c                   Clear                   v5cnt85
088550100730     c                   Eval      h5riga = 19
088560100730     c                   Eval      h5colo = 71
088570051212     c                   Goto      emi05
088580051206
088590051206    1c                   EndIf
088600051206
088610051206     c                   EndSr
088620090616
088630090616      *------------------------------------------------------------------------*
088640090616      * CONTROLLO NOTA 89 - E-MAIL RESPONSABILE AMMINISTRATIVO
088650090616      *------------------------------------------------------------------------*
088660090616     c     Sr_Ctrnt89    BegSr
088670090616
088680090616if  1c                   If        v5cnt89 <> *Blanks
088690090616
088700090616      * Se sono in interrogazione non posso interrogare la nota se non c'è
088710090616     c                   If        *In05 and Not *In40
088720090616     c                   Eval      *In28 = *On
088730100730     c                   Eval      h5riga = 20
088740100730     c                   Eval      h5colo = 71
088750090616     c                   Eval      v5cmsg = msg(52)
088760090616     c                   Leavesr
088770090616     c                   EndIf
088780090616
088790090616      * Imposto se interrogazione o gestione
088800090616     c  n05              Eval      wrich = 'M'
088810090616     c   05              Eval      wrich = 'V'
088820090616     c                   Eval      wrag = v3crag
088830090616     c                   Eval      wtnt = '89'
088840090616     c                   ExSr      Sr_f02
088850090616
088860090616     c                   If        ta12err <> *Blanks
088870090616     c                   Eval      *In28 = *On
088880100730     c                   Eval      h5riga = 20
088890100730     c                   Eval      h5colo = 71
088900090616     c                   Eval      v5cmsg = ta12msg
088910090616     c                   Leavesr
088920090616     c                   EndIf
088930090616
088940090616      * Controllo se ora c'è o non c'è più la nota
088950091112     c   70
088960091112     corn06              Eval      kntcapl = 'C'
088970100806     c   06
088980091112     cann70              Eval      kntcapl = 'T'
088990090616     c                   Movel(p)  dutkci        kntcnk1
089000091112     c   70
089010091112     corn06              Move      v1cksc        kntcnk1
089020100806     c   06
089030100806     cann70              Move      v1cnrv        kntcnk1
089040090616     c                   Clear                   kntcnk2
089050090616     c                   Movel     '89'          kntctnt
089060091119     c     kTfntc        Chain(n)  Tfntc01l
089070090616     c                   If        %Found(Tfntc01l)
089080090616     c                   Eval      *In40 = *On
089090090616     c                   Else
089100090616     c                   Eval      *In40 = *Off
089110090616     c                   EndIf
089120090616
089130090616     c                   Clear                   v5cnt89
089140100730     c                   Eval      h5riga = 20
089150100730     c                   Eval      h5colo = 71
089160090616     c                   Goto      emi05
089170090616
089180090616    1c                   EndIf
089190090616
089200090616     c                   EndSr
089210100729      /free
089220100729       //--------------------------------------------------------------
089230100729       //?Controllo nota 82 - E-mail Bonifico Danni
089240100729       //--------------------------------------------------------------
089250100729       BEGSR sr_CtrNt82;
089260100730
089270100730         IF  v5cnt82 <> *blanks;
089280100730           *in90 = *on;
089290100730       //?Se sono in interrogazione non posso interrogare la note
089300100730       //?se non è presente
089310100730           IF  *in05 and not *in68;
089320100730             *in28 = *on;
089330100730             h5riga = 21;
089340100730             h5colo = 71;
089350100730             v5cmsg = msg(52);
089360100730             leavesr;
089370100730           ENDIF;
089380100730       //?Imposto se interrogazione o gestione della nota
089390100730           IF  not *in05;
089400100730             wrich = 'M';
089410100730           ENDIF;
089420100730           IF  *in05;
089430100730             wrich = 'V';
089440100730           ENDIF;
089450100730           wrag = v3crag;
089460100730           wtnt = '82';
089470100730           exsr sr_f02;
089480100730           IF  ta12err <> *blanks;
089490100730             *in28 = *on;
089500100730             h5riga = 21;
089510100730             h5colo = 71;
089520100730             v5cmsg = ta12msg;
089530100730             leavesr;
089540100730           ENDIF;
089550100730       //?Controllo se ora c'è la nota oppure se è stata cancellata
089560100730           SELECT;
089570100730             WHEN  *in70 or not *in06;
089580100730               kntcapl = 'C';
089590100730               %subst(kntcnk1:5:7) = %editc(v1cksc:'X');
089600100806             WHEN  *in06 and not *in70;
089610100730               kntcapl = 'T';
089620100806               %subst(kntcnk1:5:7) = %editc(v1cnrv:'X');
089630100730           ENDSL;
089640100730           %subst(kntcnk1:1:4) = %editc(DUTkci:'X');
089650100730           clear kntcnk2;
089660100730           kntctnt = '82';
089670150515           chain(n) (kntcapl:kntcnk1:kntcnk2:kntctnt) TFNTC01L;
089680100730           IF  %found(TFNTC01L);
089690100730             *in68 = *on;
089700100730           ELSE;
089710100730             *in68 = *off;
089720100730           ENDIF;
089730100730           clear v5cnt82;
089740100730           h5riga = 21;
089750100730           h5colo = 71;
089760100730         ENDIF;
089770100729
089780100729       ENDSR;
089790100729      /end-free
089800051207
089810051207      *------------------------------------------------------------------------*
089820051212      * CONTROLLO TIPO COMUNICAZIONE MITTENTE
089830051207      *------------------------------------------------------------------------*
089840051207     c     Sr_Ctrtcm     BegSr
089850051212
089860051212      * Ricerca
089870051212     c                   Clear                   v6dtcm
089880051212     c                   If        v6ctcm = '?'
089890051212     c                   Eval      kcod = '2F'
089900051212     c                   call      'TRUL19R'
089910051212     c                   parm                    kcod
089920051212     c                   parm                    ordina
089930051212     c                   parm                    kkey
089940051212     c                   parm                    comando
089950051212     c                   Eval      v6ctcm = kkey
089960051212     c                   Eval      h6riga = 07
089970051212     c                   Eval      h6colo = 38
089980051212     c                   Eval      *In90 = *On
089990051212     c                   EndIf
090000120118
090010120118      /free
090020120118       //?Se anagrafica provvisoria accetto il campo vuoto
090030120118         IF  *in06 and V6Ctcm = *blanks;
090040120118           leavesr;
090050120118         ENDIF;
090060120118      /end-free
090070051212
090080051212      * Controllo
090090051212     c                   Clear                   v6dtcm
090100051212     c                   Clear                   ds2f
090110051212     c                   Eval      kcod = '2F'
090120051212     c                   Eval      kkey = v6ctcm
090130051212     c     kTabel        Chain     Tabel00f
090140051212     c                   If        Not %Found(Tabel00f) or
090150051212     c                             tblflg <> *Blanks
090160051212     c                   Eval      *In28 = *On
090170051212     c                   Eval      h6riga = 07
090180051212     c                   Eval      h6colo = 38
090190051212     c                   Eval      v6cmsg = msg(62)
090200051212     c                   Leavesr
090210051212     c                   EndIf
090220051212     c                   Eval      ds2f = tbluni
090230051212     c                   Eval      v6dtcm = §2fdes
090240051212
090250051212      * Se tipo comunicazione con invio tramite fax
090260051212if  1c                   If        §2fftp = 'F'
090270051212      * Non ammesso per i clienti vari
090280051212     c                   If        wksc04 = 8888 or wksc04 = 9999
090290051212     c                   Eval      *In28 = *On
090300051212     c                   Eval      h6riga = 07
090310051212     c                   Eval      h6colo = 38
090320051212     c                   Eval      v6cmsg = msg(63)
090330051212     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
090340051212     c                             'per clienti vari'
090350051212     c                   Leavesr
090360051212     c                   EndIf
090370051216      * Controllo se fax sul luogo 005 è esatto
090380051216if  2c                   If        *In14 and wfax005 <> *Blanks
090390051216     c                   Clear                   trul42ds
090400051216     c                   Eval      d42fax = wfax005
090410051216     c                   Call      'TRUL42R'
090420051216     c                   Parm                    trul42ds
090430051216if  3c                   If        d42err = '1'
090440051216     c                   Eval      *In28 = *On
090450051216     c                   Eval      h6riga = 07
090460051216     c                   Eval      h6colo = 38
090470051216     c                   Eval      v6cmsg = d42msg
090480051216     c                   Leavesr
090490051216    3c                   EndIf
090500051216    2c                   EndIf
090510051216      * Se non c'è il luogo o se il n. fax non è presente sul luogo
090520051216      * deve essere impostato sull'anagrafica
090530051216     c                   If        v2ctlf = *Blanks and wfax005 = *Blanks
090540051212     c                   Eval      *In28 = *On
090550051212     c                   Eval      h6riga = 07
090560051212     c                   Eval      h6colo = 38
090570051212     c                   Eval      v6cmsg = msg(63)
090580051212     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
090590051212     c                             'nr. fax obbligatorio'
090600051212     c                   Leavesr
090610051212     c                   EndIf
090620051212    1c                   EndIf
090630051219
090640051219      * Se tipo comunicazione con invio tramite mail
090650051219if  1c                   If        §2fftp = 'M'
090660051219      * Non ammesso per i clienti vari
090670051219     c                   If        wksc04 = 8888 or wksc04 = 9999
090680051219     c                   Eval      *In28 = *On
090690051219     c                   Eval      h6riga = 07
090700051219     c                   Eval      h6colo = 38
090710051219     c                   Eval      v6cmsg = msg(63)
090720051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
090730051219     c                             'per clienti vari'
090740051219     c                   Leavesr
090750051219     c                   EndIf
090760051219      * Controllo se c'è la mail nel luogo
090770051219     c                   If        *In14 and wmail005 = *Blanks
090780051219     c                   Eval      *In28 = *On
090790051219     c                   Eval      h6riga = 07
090800051219     c                   Eval      h6colo = 38
090810051219     c                   Eval      v6cmsg = msg(63)
090820051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
090830051219     c                             'mail non immessa nel luogo 005'
090840051219     c                   Leavesr
090850051219     c                   EndIf
090860051219      * Se non c'è il luogo deve esserci la nota 88
090870051219     c                   If        Not *In14 and Not *In15
090880051219     c                   Eval      *In28 = *On
090890051219     c                   Eval      h6riga = 07
090900051219     c                   Eval      h6colo = 38
090910051219     c                   Eval      v6cmsg = msg(63)
090920051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
090930051219     c                             'nota 88 non immessa'
090940051219     c                   Leavesr
090950051219     c                   EndIf
090960051219    1c                   EndIf
090970051207
090980051207     c                   EndSr
090990051207
091000051207      *------------------------------------------------------------------------*
091010051212      * CONTROLLO TIPO COMUNICAZIONE FINE GIACENZA
091020051207      *------------------------------------------------------------------------*
091030051207     c     Sr_Ctrtcf     BegSr
091040051212
091050051212      * Ricerca
091060051212     c                   Clear                   v6dtcf
091070051212     c                   If        v6ctcf = '?'
091080051212     c                   Eval      kcod = '2H'
091090051212     c                   call      'TRUL19R'
091100051212     c                   parm                    kcod
091110051212     c                   parm                    ordina
091120051212     c                   parm                    kkey
091130051212     c                   parm                    comando
091140051212     c                   Eval      v6ctcf = kkey
091150051212     c                   Eval      h6riga = 08
091160051212     c                   Eval      h6colo = 38
091170051212     c                   Eval      *In90 = *On
091180051212     c                   EndIf
091190051212
091200051212      * Controllo
091210051212     c                   Clear                   v6dtcf
091220051212     c                   Clear                   ds2h
091230051212     c                   Eval      kcod = '2H'
091240051212     c                   Eval      kkey = v6ctcf
091250051212     c     kTabel        Chain     Tabel00f
091260051212     c                   If        Not %Found(Tabel00f) or
091270051212     c                             tblflg <> *Blanks
091280051212     c                   Eval      *In28 = *On
091290051212     c                   Eval      h6riga = 08
091300051212     c                   Eval      h6colo = 38
091310060216     c                   Eval      v6cmsg = msg(70)
091320051212     c                   Leavesr
091330051212     c                   EndIf
091340051212     c                   Eval      ds2h = tbluni
091350051212     c                   Eval      v6dtcf = §2hdes
091360051212
091370051212      * Se tipo comunicazione con invio tramite fax
091380051212if  1c                   If        §2hfax = 'F'
091390051212      * Non ammesso per i clienti vari
091400051212     c                   If        wksc04 = 8888 or wksc04 = 9999
091410051212     c                   Eval      *In28 = *On
091420051212     c                   Eval      h6riga = 08
091430051212     c                   Eval      h6colo = 38
091440051212     c                   Eval      v6cmsg = msg(63)
091450051212     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
091460051212     c                             'per clienti vari'
091470051212     c                   Leavesr
091480051212     c                   EndIf
091490051216      * Controllo se fax sul luogo 005 è esatto
091500051216if  2c                   If        *In14 and wfax005 <> *Blanks
091510051216     c                   Clear                   trul42ds
091520051216     c                   Eval      d42fax = wfax005
091530051216     c                   Call      'TRUL42R'
091540051216     c                   Parm                    trul42ds
091550051216if  3c                   If        d42err = '1'
091560051216     c                   Eval      *In28 = *On
091570051216     c                   Eval      h6riga = 08
091580051216     c                   Eval      h6colo = 38
091590051216     c                   Eval      v6cmsg = d42msg
091600051216     c                   Leavesr
091610051216    3c                   EndIf
091620051216    2c                   EndIf
091630051216      * Se non c'è il luogo o se il n. fax non è presente sul luogo
091640051216      * deve essere impostato sull'anagrafica
091650051216     c                   If        v2ctlf = *Blanks and wfax005 = *Blanks
091660051212     c                   Eval      *In28 = *On
091670051212     c                   Eval      h6riga = 08
091680051212     c                   Eval      h6colo = 38
091690051212     c                   Eval      v6cmsg = msg(63)
091700051212     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
091710051212     c                             'nr. fax obbligatorio'
091720051212     c                   Leavesr
091730051212     c                   EndIf
091740051212    1c                   EndIf
091750051219      * Se tipo comunicazione con invio tramite mail
091760051219if  1c                   If        §2hfax = 'M'
091770051219      * Non ammesso per i clienti vari
091780051219     c                   If        wksc04 = 8888 or wksc04 = 9999
091790051219     c                   Eval      *In28 = *On
091800051219     c                   Eval      h6riga = 08
091810051219     c                   Eval      h6colo = 38
091820051219     c                   Eval      v6cmsg = msg(63)
091830051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
091840051219     c                             'per clienti vari'
091850051219     c                   Leavesr
091860051219     c                   EndIf
091870051219      * Controllo se c'è la mail nel luogo
091880051219     c                   If        *In14 and wmail005 = *Blanks
091890051219     c                   Eval      *In28 = *On
091900051219     c                   Eval      h6riga = 08
091910051219     c                   Eval      h6colo = 38
091920051219     c                   Eval      v6cmsg = msg(63)
091930051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
091940051219     c                             'mail non immessa nel luogo 005'
091950051219     c                   Leavesr
091960051219     c                   EndIf
091970051219      * Se non c'è il luogo deve esserci la nota 88
091980051219     c                   If        Not *In14 and Not *In15
091990051219     c                   Eval      *In28 = *On
092000051219     c                   Eval      h6riga = 08
092010051219     c                   Eval      h6colo = 38
092020051219     c                   Eval      v6cmsg = msg(63)
092030051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
092040051219     c                             'nota 88 non immessa'
092050051219     c                   Leavesr
092060051219     c                   EndIf
092070051219    1c                   EndIf
092080051207
092090051207     c                   EndSr
092100051207
092110051207      *------------------------------------------------------------------------*
092120051212      * CONTROLLO APERTURA AUTOMATICA GIACENZA
092130051207      *------------------------------------------------------------------------*
092140051207     c     Sr_Ctrapg     BegSr
092150051212
092160051212      * Ricerca
092170051212     c                   Clear                   v6dapg
092180051212     c                   If        v6capg = '?'
092190051212     c                   Eval      kcod = '2U'
092200051212     c                   call      'TRUL19R'
092210051212     c                   parm                    kcod
092220051212     c                   parm                    ordina
092230051212     c                   parm                    kkey
092240051212     c                   parm                    comando
092250051212     c                   Eval      v6capg = kkey
092260051212     c                   Eval      h6riga = 09
092270051212     c                   Eval      h6colo = 38
092280051212     c                   Eval      *In90 = *On
092290051212     c                   EndIf
092300051212
092310051212      * Controllo
092320051212     c                   Clear                   v6dapg
092330051212     c                   Clear                   ds2u
092340051212     c                   Eval      kcod = '2U'
092350051212     c                   Eval      kkey = v6capg
092360051212     c     kTabel        Chain     Tabel00f
092370051212     c                   If        Not %Found(Tabel00f) or
092380051212     c                             tblflg <> *Blanks
092390051212     c                   Eval      *In28 = *On
092400051212     c                   Eval      h6riga = 09
092410051212     c                   Eval      h6colo = 38
092420051212     c                   Eval      v6cmsg = msg(64)
092430051212     c                   Leavesr
092440051212     c                   EndIf
092450051212     c                   Eval      ds2u = tbluni
092460051212     c                   Eval      v6dapg = §2udes
092470051212
092480051212      * Non ammessa apertura automatica per i clienti vari
092490051212     c                   If        §2uape = 'S' and
092500051212     c                             (wksc04 = 8888 or wksc04 = 9999)
092510051212     c                   Eval      *In28 = *On
092520051212     c                   Eval      h6riga = 09
092530051212     c                   Eval      h6colo = 38
092540051212     c                   Eval      v6cmsg = msg(65)
092550051212     c                   Leavesr
092560051212     c                   EndIf
092570051207
092580051207     c                   EndSr
092590051207
092600051207      *------------------------------------------------------------------------*
092610051212      * CONTROLLO DISPOSIZIONI MITTENTE IN ARRIVO
092620051207      *------------------------------------------------------------------------*
092630051207     c     Sr_Ctrdmg     BegSr
092640051212
092650051212      * Non ammesse disposizioni mittente in arrivo per i clienti vari
092660051212     c                   If        v6cdmg = 'SI' and
092670051212     c                             (wksc04 = 8888 or wksc04 = 9999)
092680051212     c                   Eval      *In28 = *On
092690051212     c                   Eval      h6riga = 10
092700051212     c                   Eval      h6colo = 38
092710051212     c                   Eval      v6cmsg = msg(66)
092720051212     c                   Leavesr
092730051212     c                   EndIf
092740051207
092750051207     c                   EndSr
092760060801
092770060801      *------------------------------------------------------------------------*
092780060801      * CONTROLLO CHIUSURA AUTOMATICA GIACENZA
092790060801      *------------------------------------------------------------------------*
092800060801     c     Sr_Ctrcag     BegSr
092810060801
092820060801      * Non ammessa chiusura automatica giacenza per i clienti vari
092830060801     c                   If        v6ccag = 'SI' and
092840060801     c                             (wksc04 = 8888 or wksc04 = 9999)
092850060801     c                   Eval      *In28 = *On
092860060801     c                   Eval      h6riga = 11
092870060801     c                   Eval      h6colo = 38
092880060801     c                   Eval      v6cmsg = msg(74)
092890060801     c                   Leavesr
092900060801     c                   EndIf
092910060801
092920060801     c                   EndSr
092930051207
092940051207      *------------------------------------------------------------------------*
092950051220      * CONTROLLO LUOGO 005 - LUOGO SPEDIZIONE GIACENZA
092960051207      *------------------------------------------------------------------------*
092970051207     c     Sr_Ctrl005    BegSr
092980051212
092990051212if  1c                   If        v6cl005 <> *Blanks
093000060203
093010060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
093020060203     c                   If        *In05 and Not *In14
093030051212     c                   Eval      *In28 = *On
093040051212     c                   Eval      h6riga = 13
093050060203     c                   Eval      h6colo = 35
093060051212     c                   Eval      v6cmsg = msg(50)
093070051212     c                   Leavesr
093080051212     c                   EndIf
093090071128
093100071128      * se non è interrogazione controllo se luogo utilizzabile
093110071128     c                   if        not *in09 and not *in05 and $ufi005 <> 'S'
093120071128     c                   eval      *In28 = *on
093130071128     c                   eval      h6riga = 13
093140071128     c                   eval      h6colo = 35
093150071128     c                   eval      v6cmsg = msg(76)
093160071128     c                   leavesr
093170071128     c                   endif
093180051212
093190060203      * Imposto se interrogazione o gestione
093200051213     c                   Clear                   tnta81ds
093210060203     c   05              Eval      i81opz = '5'
093220060203     c  n05
093230060214     can 14              Eval      i81opz = '2'
093240051213     c                   Eval      i81cod = '005'
093250051213     c                   Eval      i81cli = v1cksc
093260060216     c                   Eval      i81gcli = '1'
093270060216     c   01              Eval      i81icli = '1'
093280051213     c                   Call      'TNTA81R'
093290051213     c                   Parm                    kpjba
093300051213     c                   Parm                    tnta81ds
093310051212
093320051212      * Controllo se ora c'è o non c'è più il luogo
093330060112     c                   Clear                   wfax005
093340060112     c                   Clear                   wmail005
093350051216     c                   Eval      kspecli = v1cksc
093360051216     c                   Eval      kspecod = '005'
093370051212     c     kFnspe        Chain     Fnspe01l
093380051212     c                   If        %Found(Fnspe01l) and speflg = *Blanks
093390051212     c                   Eval      *In14 = *On
093400101026     c                   Eval      wfax005 = spefax
093410101026     c     kFnsp2        Chain     Fnsp201l
093420101026     c                   If        %Found(Fnsp201l) and sp2flg = *Blanks
093430101026     c                   Eval      wmail005 = sp2est
093440101026     c                   EndIf
093450051212     c                   Else
093460051212     c                   Eval      *In14 = *Off
093470051212     c                   EndIf
093480051212
093490051212     c                   Clear                   v6cl005
093500051212     c                   Eval      h6riga = 13
093510060203     c                   Eval      h6colo = 35
093520051212     c                   Goto      emi06
093530051212
093540051212    1c                   EndIf
093550051207
093560051207     c                   EndSr
093570051207
093580051207      *------------------------------------------------------------------------*
093590051220      * CONTROLLO NOTA 88 - E-MAIL GIACENZE
093600051207      *------------------------------------------------------------------------*
093610051207     c     Sr_Ctrnt88    BegSr
093620051212
093630051212if  1c                   If        v6cnt88 <> *Blanks
093640060203
093650060203      * Se sono in interrogazione non posso interrogare la nota se non c'è
093660060203     c                   If        *In05 and Not *In15
093670051212     c                   Eval      *In28 = *On
093680051212     c                   Eval      h6riga = 14
093690060203     c                   Eval      h6colo = 35
093700051219     c                   Eval      v6cmsg = msg(52)
093710051212     c                   Leavesr
093720051212     c                   EndIf
093730051212
093740060203      * Imposto se interrogazione o gestione
093750060203     c  n05              Eval      wrich = 'M'
093760060203     c   05              Eval      wrich = 'V'
093770060217     c                   Eval      wrag = v3crag
093780051212     c                   Eval      wtnt = '88'
093790051212     c                   ExSr      Sr_f02
093800060215
093810060215     c                   If        ta12err <> *Blanks
093820060215     c                   Eval      *In28 = *On
093830060215     c                   Eval      h6riga = 14
093840060215     c                   Eval      h6colo = 35
093850060215     c                   Eval      v6cmsg = ta12msg
093860060215     c                   Leavesr
093870060215     c                   EndIf
093880051212
093890060215      * Controllo se ora c'è o non c'è più la nota
093900091112     c   70
093910091112     corn06              Eval      kntcapl = 'C'
093920100806     c   06
093930091112     cann70              Eval      kntcapl = 'T'
093940051216     c                   Movel(p)  dutkci        kntcnk1
093950091112     c   70
093960091112     corn06              Move      v1cksc        kntcnk1
093970100806     c   06
093980100806     cann70              Move      v1cnrv        kntcnk1
093990051216     c                   Clear                   kntcnk2
094000051216     c                   Movel     '88'          kntctnt
094010091119     c     kTfntc        Chain(n)  Tfntc01l
094020051212     c                   If        %Found(Tfntc01l)
094030051212     c                   Eval      *In15 = *On
094040051212     c                   Else
094050051212     c                   Eval      *In15 = *Off
094060051212     c                   EndIf
094070051212
094080051212     c                   Clear                   v6cnt88
094090051212     c                   Eval      h6riga = 14
094100060203     c                   Eval      h6colo = 35
094110051212     c                   Goto      emi06
094120051212
094130051212    1c                   EndIf
094140051207
094150051207     c                   EndSr
094160060208
094170060208      *------------------------------------------------------------------------*
094180060208      * CONTROLLO LUOGO 300 - MERCE RESA
094190060208      *------------------------------------------------------------------------*
094200060208     c     Sr_Ctrl300    BegSr
094210060208
094220060208if  1c                   If        v6cl300 <> *Blanks
094230060208
094240060208      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
094250060208     c                   If        *In05 and Not *In33
094260060208     c                   Eval      *In28 = *On
094270060208     c                   Eval      h6riga = 15
094280060208     c                   Eval      h6colo = 35
094290060208     c                   Eval      v6cmsg = msg(50)
094300060208     c                   Leavesr
094310060208     c                   EndIf
094320071128
094330071128      * se non è interrogazione controllo se luogo utilizzabile
094340071128     c                   if        not *in09 and not *in05 and $ufi300 <> 'S'
094350071128     c                   eval      *In28 = *on
094360071128     c                   eval      h6riga = 15
094370071128     c                   eval      h6colo = 35
094380071128     c                   eval      v6cmsg = msg(76)
094390071128     c                   leavesr
094400071128     c                   endif
094410060208
094420060208      * Imposto se interrogazione o gestione
094430060208     c                   Clear                   tnta81ds
094440060208     c   05              Eval      i81opz = '5'
094450060208     c  n05
094460060214     can 33              Eval      i81opz = '2'
094470060208     c                   Eval      i81cod = '300'
094480060208     c                   Eval      i81cli = v1cksc
094490060216     c                   Eval      i81gcli = '1'
094500060216     c   01              Eval      i81icli = '1'
094510060208     c                   Call      'TNTA81R'
094520060208     c                   Parm                    kpjba
094530060208     c                   Parm                    tnta81ds
094540060208
094550060208      * Controllo se ora c'è o non c'è più il luogo
094560060208     c                   Eval      kspecli = v1cksc
094570060208     c                   Eval      kspecod = '300'
094580060208     c     kFnspe        Chain     Fnspe01l
094590060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
094600060208     c                   Eval      *In33 = *On
094610060208     c                   Else
094620060208     c                   Eval      *In33 = *Off
094630060208     c                   EndIf
094640060208
094650060208     c                   Clear                   v6cl300
094660060208     c                   Eval      h6riga = 15
094670060208     c                   Eval      h6colo = 35
094680060208     c                   Goto      emi06
094690060208
094700060208    1c                   EndIf
094710060208
094720060208     c                   EndSr
094730051213
094740051213      *------------------------------------------------------------------------*
094750051213      * CONTROLLO LUOGO 006 - PREAVVISO/AVVISO DANNO
094760051213      *------------------------------------------------------------------------*
094770051213     c     Sr_Ctrl006    BegSr
094780051213
094790051213if  1c                   If        v7cl006 <> *Blanks
094800060203
094810060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
094820060203     c                   If        *In05 and Not *In16
094830051213     c                   Eval      *In28 = *On
094840051213     c                   Eval      h7riga = 12
094850060203     c                   Eval      h7colo = 35
094860051213     c                   Eval      v7cmsg = msg(50)
094870051213     c                   Leavesr
094880051213     c                   EndIf
094890071128
094900071128      * se non è interrogazione controllo se luogo utilizzabile
094910071128     c                   if        not *in09 and not *in05 and $ufi006 <> 'S'
094920071128     c                   eval      *In28 = *on
094930071128     c                   eval      h7riga = 12
094940071128     c                   eval      h7colo = 35
094950071128     c                   eval      v7cmsg = msg(76)
094960071128     c                   leavesr
094970071128     c                   endif
094980051213
094990060203      * Imposto se interrogazione o gestione
095000051213     c                   Clear                   tnta81ds
095010060203     c   05              Eval      i81opz = '5'
095020060203     c  n05
095030060214     can 16              Eval      i81opz = '2'
095040051213     c                   Eval      i81cod = '006'
095050051213     c                   Eval      i81cli = v1cksc
095060060216     c                   Eval      i81gcli = '1'
095070060216     c   01              Eval      i81icli = '1'
095080051213     c                   Call      'TNTA81R'
095090051213     c                   Parm                    kpjba
095100051213     c                   Parm                    tnta81ds
095110051213
095120051213      * Controllo se ora c'è o non c'è più il luogo
095130051216     c                   Eval      kspecli = v1cksc
095140051216     c                   Eval      kspecod = '006'
095150051213     c     kFnspe        Chain     Fnspe01l
095160051213     c                   If        %Found(Fnspe01l) and speflg = *Blanks
095170051213     c                   Eval      *In16 = *On
095180051213     c                   Else
095190051213     c                   Eval      *In16 = *Off
095200051213     c                   EndIf
095210051213
095220051213     c                   Clear                   v7cl006
095230051213     c                   Eval      h7riga = 12
095240060203     c                   Eval      h7colo = 35
095250051213     c                   Goto      emi07
095260051213
095270051213    1c                   EndIf
095280051213
095290051213     c                   EndSr
095300051213
095310051213      *------------------------------------------------------------------------*
095320051220      * CONTROLLO NOTA 87 - E-MAIL DANNI
095330051213      *------------------------------------------------------------------------*
095340051213     c     Sr_Ctrnt87    BegSr
095350051213
095360051213if  1c                   If        v7cnt87 <> *Blanks
095370060203
095380060203      * Se sono in interrogazione non posso interrogare la nota se non c'è
095390060203     c                   If        *In05 and Not *In18
095400051213     c                   Eval      *In28 = *On
095410051213     c                   Eval      h7riga = 13
095420060203     c                   Eval      h7colo = 35
095430051219     c                   Eval      v7cmsg = msg(52)
095440051213     c                   Leavesr
095450051213     c                   EndIf
095460051213
095470060203      * Imposto se interrogazione o gestione
095480060203     c  n05              Eval      wrich = 'M'
095490060203     c   05              Eval      wrich = 'V'
095500060217     c                   Eval      wrag = v3crag
095510051213     c                   Eval      wtnt = '87'
095520051213     c                   ExSr      Sr_f02
095530060215
095540060215     c                   If        ta12err <> *Blanks
095550060215     c                   Eval      *In28 = *On
095560060215     c                   Eval      h7riga = 13
095570060215     c                   Eval      h7colo = 35
095580060215     c                   Eval      v7cmsg = ta12msg
095590060215     c                   Leavesr
095600060215     c                   EndIf
095610051213
095620060215      * Controllo se ora c'è o non c'è più la nota
095630091112     c   70
095640091112     corn06              Eval      kntcapl = 'C'
095650100806     c   06
095660091112     cann70              Eval      kntcapl = 'T'
095670051216     c                   Movel(p)  dutkci        kntcnk1
095680091112     c   70
095690091112     corn06              Move      v1cksc        kntcnk1
095700100806     c   06
095710100806     cann70              Move      v1cnrv        kntcnk1
095720051216     c                   Clear                   kntcnk2
095730051216     c                   Movel     '87'          kntctnt
095740091119     c     kTfntc        Chain(n)  Tfntc01l
095750051213     c                   If        %Found(Tfntc01l)
095760051213     c                   Eval      *In18 = *On
095770051213     c                   Else
095780051213     c                   Eval      *In18 = *Off
095790051213     c                   EndIf
095800051213
095810051213     c                   Clear                   v7cnt87
095820051213     c                   Eval      h7riga = 13
095830060203     c                   Eval      h7colo = 35
095840051213     c                   Goto      emi07
095850051213
095860051213    1c                   EndIf
095870051213
095880051213     c                   EndSr
095890051213
095900051213      *------------------------------------------------------------------------*
095910051213      * CONTROLLO LUOGO 008 - PROGETTO DI LIQUIDAZIONE
095920051213      *------------------------------------------------------------------------*
095930051213     c     Sr_Ctrl008    BegSr
095940051213
095950051213if  1c                   If        v7cl008 <> *Blanks
095960060203
095970060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
095980060203     c                   If        *In05 and Not *In17
095990051213     c                   Eval      *In28 = *On
096000051213     c                   Eval      h7riga = 14
096010060203     c                   Eval      h7colo = 35
096020051213     c                   Eval      v7cmsg = msg(50)
096030051213     c                   Leavesr
096040051213     c                   EndIf
096050071128
096060071128      * se non è interrogazione controllo se luogo utilizzabile
096070071128     c                   if        not *in09 and not *in05 and $ufi008 <> 'S'
096080071128     c                   eval      *In28 = *on
096090071128     c                   eval      h7riga = 14
096100071128     c                   eval      h7colo = 35
096110071128     c                   eval      v7cmsg = msg(76)
096120071128     c                   leavesr
096130071128     c                   endif
096140051213
096150060203      * Imposto se interrogazione o gestione
096160051213     c                   Clear                   tnta81ds
096170060203     c   05              Eval      i81opz = '5'
096180060203     c  n05
096190060214     can 17              Eval      i81opz = '2'
096200051213     c                   Eval      i81cod = '008'
096210051213     c                   Eval      i81cli = v1cksc
096220060216     c                   Eval      i81gcli = '1'
096230060216     c   01              Eval      i81icli = '1'
096240051213     c                   Call      'TNTA81R'
096250051213     c                   Parm                    kpjba
096260051213     c                   Parm                    tnta81ds
096270051213
096280051213      * Controllo se ora c'è o non c'è più il luogo
096290051216     c                   Eval      kspecli = v1cksc
096300051216     c                   Eval      kspecod = '008'
096310051213     c     kFnspe        Chain     Fnspe01l
096320051213     c                   If        %Found(Fnspe01l) and speflg = *Blanks
096330051213     c                   Eval      *In17 = *On
096340051213     c                   Else
096350051213     c                   Eval      *In17 = *Off
096360051213     c                   EndIf
096370051213
096380051213     c                   Clear                   v7cl008
096390051213     c                   Eval      h7riga = 14
096400060203     c                   Eval      h7colo = 35
096410051213     c                   Goto      emi07
096420051213
096430051213    1c                   EndIf
096440051213
096450051213     c                   EndSr
096460051213
096470051213      *------------------------------------------------------------------------*
096480051213      * CONTROLLO NUMERO STOP PER RITIRO
096490051213      *------------------------------------------------------------------------*
096500051213     c     Sr_Ctrstp     BegSr
096510051213
096520051213     c                   If        v7cstp > 800
096530051213     c                   Eval      *In28 = *On
096540051213     c                   Eval      h7riga = 18
096550051213     c                   Eval      h7colo = 37
096560051213     c                   Eval      v7cmsg = msg(67)
096570051213     c                   Leavesr
096580051213     c                   EndIf
096590051213
096600051213     c                   EndSr
096610060208
096620060208      *------------------------------------------------------------------------*
096630060208      * CONTROLLO LUOGO 002 - INVIO ORM ESTERO
096640060208      *------------------------------------------------------------------------*
096650060208     c     Sr_Ctrl002    BegSr
096660060208
096670060208if  1c                   If        v7cl002 <> *Blanks
096680060208
096690060208      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
096700060208     c                   If        *In05 and Not *In35
096710060208     c                   Eval      *In28 = *On
096720060208     c                   Eval      h7riga = 20
096730060208     c                   Eval      h7colo = 35
096740060208     c                   Eval      v7cmsg = msg(50)
096750060208     c                   Leavesr
096760060208     c                   EndIf
096770071128
096780071128      * se non è interrogazione controllo se luogo utilizzabile
096790071128     c                   if        not *in09 and not *in05 and $ufi002 <> 'S'
096800071128     c                   eval      *In28 = *on
096810071128     c                   eval      h7riga = 20
096820071128     c                   eval      h7colo = 35
096830071128     c                   eval      v7cmsg = msg(76)
096840071128     c                   leavesr
096850071128     c                   endif
096860060208
096870060208      * Imposto se interrogazione o gestione
096880060208     c                   Clear                   tnta81ds
096890060208     c   05              Eval      i81opz = '5'
096900060208     c  n05
096910060214     can 35              Eval      i81opz = '2'
096920060208     c                   Eval      i81cod = '002'
096930060208     c                   Eval      i81cli = v1cksc
096940060216     c                   Eval      i81gcli = '1'
096950060216     c   01              Eval      i81icli = '1'
096960060208     c                   Call      'TNTA81R'
096970060208     c                   Parm                    kpjba
096980060208     c                   Parm                    tnta81ds
096990060208
097000060208      * Controllo se ora c'è o non c'è più il luogo
097010060208     c                   Eval      kspecli = v1cksc
097020060208     c                   Eval      kspecod = '002'
097030060208     c     kFnspe        Chain     Fnspe01l
097040060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
097050060208     c                   Eval      *In35 = *On
097060060208     c                   Else
097070060208     c                   Eval      *In35 = *Off
097080060208     c                   EndIf
097090060208
097100060208     c                   Clear                   v7cl002
097110060208     c                   Eval      h7riga = 20
097120060208     c                   Eval      h7colo = 35
097130060208     c                   Goto      emi07
097140060208
097150060208    1c                   EndIf
097160060208
097170060208     c                   EndSr
097180060208
097190060208      *------------------------------------------------------------------------*
097200060208      * CONTROLLO NOTA 83 - E-MAIL ORM ESTERO
097210060208      *------------------------------------------------------------------------*
097220060208     c     Sr_Ctrnt83    BegSr
097230060208
097240060208if  1c                   If        v7cnt83 <> *Blanks
097250060208
097260060208      * Se sono in interrogazione non posso interrogare la nota se non c'è
097270060208     c                   If        *In05 and Not *In34
097280060208     c                   Eval      *In28 = *On
097290060208     c                   Eval      h7riga = 21
097300060208     c                   Eval      h7colo = 35
097310060208     c                   Eval      v7cmsg = msg(52)
097320060208     c                   Leavesr
097330060208     c                   EndIf
097340060208
097350060208      * Imposto se interrogazione o gestione
097360060208     c  n05              Eval      wrich = 'M'
097370060208     c   05              Eval      wrich = 'V'
097380060217     c                   Eval      wrag = v3crag
097390060208     c                   Eval      wtnt = '83'
097400060208     c                   ExSr      Sr_f02
097410060215
097420060215     c                   If        ta12err <> *Blanks
097430060215     c                   Eval      *In28 = *On
097440060215     c                   Eval      h7riga = 21
097450060215     c                   Eval      h7colo = 35
097460060215     c                   Eval      v7cmsg = ta12msg
097470060215     c                   Leavesr
097480060215     c                   EndIf
097490060208
097500060215      * Controllo se ora c'è o non c'è più la nota
097510091112     c   70
097520091112     corn06              Eval      kntcapl = 'C'
097530100806     c   06
097540091112     cann70              Eval      kntcapl = 'T'
097550060208     c                   Movel(p)  dutkci        kntcnk1
097560091112     c   70
097570091112     corn06              Move      v1cksc        kntcnk1
097580100806     c   06
097590100806     cann70              Move      v1cnrv        kntcnk1
097600060208     c                   Clear                   kntcnk2
097610060208     c                   Movel     '83'          kntctnt
097620091119     c     kTfntc        Chain(n)  Tfntc01l
097630060208     c                   If        %Found(Tfntc01l)
097640060208     c                   Eval      *In34 = *On
097650060208     c                   Else
097660060208     c                   Eval      *In34 = *Off
097670060208     c                   EndIf
097680060208
097690060208     c                   Clear                   v7cnt83
097700060208     c                   Eval      h7riga = 21
097710060208     c                   Eval      h7colo = 35
097720060208     c                   Goto      emi07
097730060208
097740060208    1c                   EndIf
097750060208
097760060208     c                   EndSr
097770060208
097780060208      *------------------------------------------------------------------------*
097790060208      * CONTROLLO LUOGO 200 - INVIO DOC. DOGANA
097800060208      *------------------------------------------------------------------------*
097810060208     c     Sr_Ctrl200    BegSr
097820060208
097830060208if  1c                   If        v7cl200 <> *Blanks
097840060208
097850060208      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
097860060208     c                   If        *In05 and Not *In37
097870060208     c                   Eval      *In28 = *On
097880060208     c                   Eval      h7riga = 20
097890060208     c                   Eval      h7colo = 72
097900060208     c                   Eval      v7cmsg = msg(50)
097910060208     c                   Leavesr
097920060208     c                   EndIf
097930071128
097940071128      * se non è interrogazione controllo se luogo utilizzabile
097950071128     c                   if        not *in09 and not *in05 and $ufi200 <> 'S'
097960071128     c                   eval      *In28 = *on
097970071128     c                   eval      h7riga = 20
097980071128     c                   eval      h7colo = 72
097990071128     c                   eval      v7cmsg = msg(76)
098000071128     c                   leavesr
098010071128     c                   endif
098020060208
098030060208      * Imposto se interrogazione o gestione
098040060208     c                   Clear                   tnta81ds
098050060208     c   05              Eval      i81opz = '5'
098060060208     c  n05
098070060214     can 37              Eval      i81opz = '2'
098080060208     c                   Eval      i81cod = '200'
098090060208     c                   Eval      i81cli = v1cksc
098100060216     c                   Eval      i81gcli = '1'
098110060216     c   01              Eval      i81icli = '1'
098120060208     c                   Call      'TNTA81R'
098130060208     c                   Parm                    kpjba
098140060208     c                   Parm                    tnta81ds
098150060208
098160060208      * Controllo se ora c'è o non c'è più il luogo
098170060208     c                   Eval      kspecli = v1cksc
098180060208     c                   Eval      kspecod = '200'
098190060208     c     kFnspe        Chain     Fnspe01l
098200060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
098210060208     c                   Eval      *In37 = *On
098220060208     c                   Else
098230060208     c                   Eval      *In37 = *Off
098240060208     c                   EndIf
098250060208
098260060208     c                   Clear                   v7cl200
098270060208     c                   Eval      h7riga = 20
098280060208     c                   Eval      h7colo = 72
098290060208     c                   Goto      emi07
098300060208
098310060208    1c                   EndIf
098320060208
098330060208     c                   EndSr
098340060208
098350060208      *------------------------------------------------------------------------*
098360060208      * CONTROLLO NOTA 86 - E-MAIL MANIFEST
098370060208      *------------------------------------------------------------------------*
098380060208     c     Sr_Ctrnt86    BegSr
098390060208
098400060208if  1c                   If        v7cnt86 <> *Blanks
098410060208
098420060208      * Se sono in interrogazione non posso interrogare la nota se non c'è
098430060208     c                   If        *In05 and Not *In36
098440060208     c                   Eval      *In28 = *On
098450060208     c                   Eval      h7riga = 21
098460060208     c                   Eval      h7colo = 72
098470060208     c                   Eval      v7cmsg = msg(52)
098480060208     c                   Leavesr
098490060208     c                   EndIf
098500060208
098510060208      * Imposto se interrogazione o gestione
098520060208     c  n05              Eval      wrich = 'M'
098530060208     c   05              Eval      wrich = 'V'
098540060217     c                   Eval      wrag = v3crag
098550060208     c                   Eval      wtnt = '86'
098560060208     c                   ExSr      Sr_f02
098570060215
098580060215     c                   If        ta12err <> *Blanks
098590060215     c                   Eval      *In28 = *On
098600060215     c                   Eval      h7riga = 21
098610060215     c                   Eval      h7colo = 72
098620060215     c                   Eval      v7cmsg = ta12msg
098630060215     c                   Leavesr
098640060215     c                   EndIf
098650060208
098660060215      * Controllo se ora c'è o non c'è più la nota
098670091112     c   70
098680091112     corn06              Eval      kntcapl = 'C'
098690100806     c   06
098700091112     cann70              Eval      kntcapl = 'T'
098710060208     c                   Movel(p)  dutkci        kntcnk1
098720091112     c   70
098730091112     corn06              Move      v1cksc        kntcnk1
098740100806     c   06
098750100806     cann70              Move      v1cnrv        kntcnk1
098760060208     c                   Clear                   kntcnk2
098770060208     c                   Movel     '86'          kntctnt
098780091119     c     kTfntc        Chain(n)  Tfntc01l
098790060208     c                   If        %Found(Tfntc01l)
098800060208     c                   Eval      *In36 = *On
098810060208     c                   Else
098820060208     c                   Eval      *In36 = *Off
098830060208     c                   EndIf
098840060208
098850060208     c                   Clear                   v7cnt86
098860060208     c                   Eval      h7riga = 21
098870060208     c                   Eval      h7colo = 72
098880060208     c                   Goto      emi07
098890060208
098900060208    1c                   EndIf
098910060208
098920060208     c                   EndSr
098930051220
098940051220      *-------------------------------------------------------------------------
098950051220      * CONTROLLO IL BLOCCO/SBLOCCO DEL CLIENTE
098960051220      *-------------------------------------------------------------------------
098970051220     c     Sr_Ctrabl     BegSr
098980051220
098990051220     c                   Eval      wokabl = *Off
099000051220     c                   Clear                   w02tes
099010051220     c                   Clear                   w02msg
099020051220
099030051220      * se ho aggiornato blocco/sblocco i clienti di fnacr
099040051220if  1c                   If        acoabl <> *Blanks and savabl = *Blanks
099050051220     c                   Eval      w02tes = 'Il cliente è stato bloccato'
099060051220      * cliente bloccato richiamo pgm esterno per bloccare fnacr
099070051220     c                   Clear                   fior50ds
099080051220     c                   Move      acoksc        i50ksc
099090140113     c                   eval      I50abl = 'B'
099100051220     c                   Call      'FIOR50R'
099110051220     c                   Parm                    fior50ds
099120051220      * se torna errore emetto il msg di errore
099130051220if  2c                   If        o50err <> *Blanks
099140051220     c                   Eval      w02msg = o50msg
099150051220      * emetto il msg che ho bloccato i clienti su fnacr
099160051220   x2c                   Else
099170051220     c                   Eval      w02msg = 'Anagrafiche clienti ritiro collega-
099180051220     c                             te bloccate'
099190051220    2c                   EndIf
099200051220     c                   Eval      wokabl = *On
099210051220    1c                   EndIf
099220051220if  1c                   If        acoabl = *Blanks and savabl <> *Blanks
099230051220     c                   Eval      w02tes = 'Il cliente è stato sbloccato'
099240051220      * cliente sbloccato emetto solo il msg
099250051220     c                   Eval      w02msg = 'Anagrafiche clienti ritiro collega-
099260051220     c                             te non aggiornate'
099270051220     c                   Eval      wokabl = *On
099280051220    1c                   EndIf
099290051220
099300051220      * Emetto la window x avvisare l'utente
099310051220     c                   If        wokabl = *On
099320051220     c                   Exfmt     ta60w02
099330051220     c                   EndIf
099340051220
099350051220     c                   EndSr
099360100128
099370100128      *-------------------------------------------------------------------------
099380100128      * Richiamo interrogazione codici fatturazione con stessa p.IVA
099390100128      *-------------------------------------------------------------------------
099400100128     c     sr_ta40       begsr
099410100128
099420100128     c                   clear                   tnta40ds
099430100128     c                   IF        v2ivaeu = *blanks
099440100128     c                   eval      ITA40iva = v2ivait
099450100128     c                   else
099460100128     c                   eval      ITA40iva = v2civa
099470100128     c                   ENDIF
099480100128     c  n06              eval      ITA40ksc = v1cksc
099490100128     c   06              eval      ITA40ksc = ta60cli
099500100128     c                   eval      ITA40scf = clpscf
099510100128     c                   eval      ITA40rag = v2crag
099520100128     c                   call      'TNTA40R'
099530100128     c                   parm                    kpjba
099540100128     c                   parm                    tnta40ds
099550100128     c                   IF        OTA40err <> *blanks
099560100128     c                   eval      v4cmsg = OTA40msg
099570100128     c                   eval      *in28 = *on
099580100312     c                   leavesr
099590100128     c                   ENDIF
099600100312     c                   IF        OTA40scf = 0
099610100312     c                   Eval      v4cmsg = msg(33)
099620100312     c                   eval      *in28 = *on
099630100312     c                   leavesr
099640100312     c                   ENDIF
099650100128     c                   eval      v4cscf = %editc(OTA40scf:'X')
099660100128     c                   eval      wtnta40 = *on
099670100201     c                   clear                   v4dscf
099680100201     c                   Clear                   Tibs69ds
099690100201     c                   Move      v4cscf        I69kac
099700100201     c                   Call      'TIBS69R'
099710100201     c                   Parm                    Tibs69ds
099720100201     c                   Parm                    ds_cnaco
099730100201     c                   Parm                    ds_cnind
099740100201     c                   Parm                    ds_cnclp
099750100201     c                   Parm                    ds_fncls
099760100201     c                   Eval      wtibs69 = *On
099770100201     c                   Movel     w_acorag      v4dscf
099780100128
099790100128     c                   endsr
099800051216
099810080222      *-------------------------------------------------------------------------
099820080222      * Reperisco la nazione del sottoconto intestazione fattura
099830080222      *-------------------------------------------------------------------------
099840080222     c     Sr_Repscfsta  BegSr
099850080222
099860080222     c                   clear                   w_scfsta
099870080222      * Controllo
099880100127     c                   if        v4cscf <> %editc(v1cksc:'X')
099890110427     c                             and v4cscf > *zeros
099900080222     c                   Clear                   Tibs69ds
099910080222     c                   Move      v4cscf        I69kin
099920080222     c                   Call      'TIBS69R'
099930080222     c                   Parm                    Tibs69ds
099940080222     c                   Parm                    ds_cnaco
099950080222     c                   Parm                    ds_cnind
099960080222     c                   Parm                    ds_cnclp
099970080222     c                   Parm                    ds_fncls
099980080222     c                   Eval      wtibs69 = *On
099990080307     c                   if        o69err=*blanks and w_indflg=*blanks
100000080222     c                   eval      w_scfsta=w_indsta
100010080222     c                   endif
100020080227     c                   else
100030080227     c                   eval      w_scfsta=v2csta
100040080227     c                   endif
100050080222
100060080222     c                   EndSr
100070080306      *------------------------------------------------------------------------*
100080080306      * Controllo presenza codice su altro potenziale come cod.fiscale
100090080306      *------------------------------------------------------------------------*
100100080306     c     Sr_CdfAltroP  BegSr
100110080306     c     codice        setll     tncpo05l
100120080306     c     codice        reade     tncpo05l
100130080306    3c                   dow       not %eof(tncpo05l)
100140080306     c* se trovato un record "filiale memorizzo ragione sociale del potenz.
100150080306     c                   eval      savrag=c_cporag
100160080306     c     codice        reade     tncpo05l
100170080306    3c                   enddo
100180080306    3c                   if        not %eof (tncpo05l)
100190080306     c                   eval      savrag=c_cporag
100200080306    3c                   endif
100210080306     c                   endsr
100220080306      *------------------------------------------------------------------------*
100230080306      * Controllo presenza codice su altro potenziale come piva
100240080306      *------------------------------------------------------------------------*
100250080306     c     Sr_PivAltroP  BegSr
100260080306     c     codice        setll     tncpo06l
100270080306     c     codice        reade     tncpo06l
100280080306    3c                   dow       not %eof(tncpo06l)
100290080306     c* se trovato un record "filiale memorizzo ragione sociale del potenz.
100300080306     c                   eval      savrag=c_cporag
100310080306     c     codice        reade     tncpo06l
100320080306    3c                   enddo
100330080306    3c                   if        not %eof (tncpo06l)
100340080306     c                   eval      savrag=c_cporag
100350080306    3c                   endif
100360080306     c                   endsr
100370080306      *------------------------------------------------------------------------*
100380080306      * Messaggio di errore codice fiscal già presente per altro potenz
100390080306      *------------------------------------------------------------------------*
100400080306     c     Sr_msgcdf     BegSr
100410080306     c* Errore forzabile se trovato un altro potenziale con lo stesso
100420080306     c* codice fiscale
100430080306    3c                   if        savrag<>*blanks
100440080306     c                   eval      cod_forzcdf=codice
100450080306     c                   seton                                        28
100460080306     c                   eval      h2riga = 12
100470080306     c                   eval      h2colo = 28
100480080306     c                   Eval      v2cmsg = msg(77)
100490080306     c                   Leavesr
100500080306    3c                   endif
100510080306     c                   endsr
100520080306      *------------------------------------------------------------------------*
100530080306      * Messaggio di errore partita iva   già presente per altro potenz
100540080306      *------------------------------------------------------------------------*
100550080306     c     Sr_msgiva     BegSr
100560080306
100570080306    3c                   if        savrag<>*blanks
100580080306     c                   eval      cod_forziva=v2civa
100590080306     c                   seton                                        28
100600080306     c                   Eval      h2riga = 12
100610080306     c                   Eval      h2colo = 52
100620080306     c                   Eval      v2cmsg = msg(78)
100630080306     c                   Leavesr
100640080306    3c                   endif
100650080306     c                   endsr
100660080422
100670080422      *------------------------------------------------------------------------*
100680080422      * Emissione Window per inviare mail alla sede
100690080422      *------------------------------------------------------------------------*
100700080422     c     sr_winmail    begsr
100710080422
100720100729     c                   eval      w04uffsede = wuffsede
100730080422Do  2c                   do        *hival
100740080422     c                   exfmt     ta60w04
100750080422If  3c                   if        *inkl
100760080422     c                   leavesr
100770080422    3c                   endif
100780080422    2c                   enddo
100790080422
100800080422     c                   endsr
100810100729      /free
100820100729       //--------------------------------------------------------------
100830100729       //?Descrizione banca.
100840100729       //--------------------------------------------------------------
100850100729       BEGSR DesBanca;
100860100729
100870100729         clear wDesBanca;
100880100729         chain (kabi:kcab) CNABI00F;
100890100729         IF  %Found(CNABI00F) and ABIann <> '*';
100900100729           exsr sr_crebna;
100910100729           wDesbanca = wbanca + wagenzia;
100920100729         ENDIF;
100930100729
100940100729       ENDSR;
100950150427
100960150427       //--------------------------------------------------------------
100970150427       //?Memorizza immagine precedente.
100980150427       //--------------------------------------------------------------
100990150427       BEGSR ImmaginePrima;
101000150427
101010150427         clear TIBS73DS;
101020150427         IBS73immag ='P';
101030150427         tibs73r (TIBS73DS:cnaco_73:cnind_73:cnclp_73:fncls_73);
101040150427         wtibs73 = *on;
101050150427
101060150427       ENDSR;
101070150427
101080150427       //--------------------------------------------------------------
101090150427       //?Memorizza immagine sucessiva.
101100150427       //--------------------------------------------------------------
101110150427       BEGSR ImmagineDopo;
101120150427
101130150427         clear TIBS73DS;
101140150427         IBS73pru = knmus;
101150150427         IBS73noj = knmeb;
101160150427         ibs73pgm = 'TNTA60R';
101170150427         IBS73immag ='D';
101180150427         IF  *in02;
101190150427           IBS73cta = 'M';
101200150427         ENDIF;
101210150427         IF  *in01;
101220150427           IBS73cta = 'I';
101230150427         ENDIF;
101240150427         tibs73r (TIBS73DS:cnaco_73:cnind_73:cnclp_73:fncls_73);
101250150427         wtibs73 = *on;
101260150427
101270150427       ENDSR;
101280160803
101290160803       //--------------------------------------------------------------
101300160803       //?Scrivo l'immagine Precedente.
101310160803       //--------------------------------------------------------------
101320160803       BEGSR ImmaginePrimaCpo;
101330160803
101340160803         clear TRMK30DS;
101350160803         IMK30immag = 'P';
101360160803         IMK30tvp = 'B';
101370160803         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
101380160803
101390160803       ENDSR;
101400160803
101410160803       //--------------------------------------------------------------
101420160803       //?Scrivo Storico Variazioni.
101430160803       //--------------------------------------------------------------
101440160803       BEGSR ScriviVarCpo;
101450160803
101460160803         clear TRMK30DS;
101470160803         IMK30pru = knmus;
101480160803         IMK30noj = knmeb;
101490160803         IMK30pgm = 'TNTA60R';
101500160803         IMK30immag = 'D';
101510160803         IMK30cta = 'M';
101520160803         IMK30tvp = 'B';
101530160803
101540160803         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
101550160803
101560160803       ENDSR;
101570150427
101580100729      /end-free
101590080222
101600051107      *------------------------------------------------------------------------*
101610051107      * ROUTINE INIZIALE
101620051107      *------------------------------------------------------------------------*
101630051107     c     *Inzsr        BegSr
101640051107
101650051107     c     *Entry        Plist
101660051107     c                   Parm                    Kpjba
101670051115     c                   Parm                    Tnta60ds
101680110407     c                   Parm                    ParmCbl
101690091104
101700091104     c                   eval      $interroga = *off
101710091124     c                   eval      $modifica  = *off
101720091106     c                   eval      $pgmrich   = *off
101730110407     c                   eval      $Blocco    = *off
101740051115
101750091104      * PGM Richiamato
101760051115     c                   If        %Parms > 1
101770091106     c                   eval      $pgmrich = *on
101780100806      * trattativa
101790100806     c                   if        ta60flg = 'T'
101800090914     c                   Eval      *In06 = *on
101810091112     c                   eval      *in70 = (ta60cli > 0)
101820091112     c   70              eval      v1cksc = ta60cli
101830091104      * cliente per aggancio a cnaco
101840090914     c                   else
101850090914     c                   eval      *in06 = *off
101860091112     c                   eval      *in70 = *off
101870090914     c                   endif
101880091104      * convalida offerta
101890091104      * sono in manutenzione perchè il cliente viene scritto dal chiamante
101900051115     c                   Eval      *In07 = (ta60flg = 'C')
101910091104      * codice visita/trattativa da agganciare
101920051222     c   06              Eval      v1cnrv = ta60nrv
101930091104      * codice cliente nuovo da agganciare
101940051222     c   07              Eval      v1cksc = ta60nrv
101950091104
101960091104      * inibisco il richiamo all'interrogazione potenziali per errore
101970091104      * chiamata ricorsiva se provengo da gestione potenziali
101980091104     c                   eval      *in65 = (ta60pot = '1')
101990091104      * interrogazione se richiamato da interrogazione tariffe
102000060207     c                   Eval      *In05 = (ta60flg = 'I')
102010091104
102020091104      * richiamato per visualizzazione (nuovo campo della DS)
102030091104     c                   if        ta60int = 'S'
102040091104     c                   eval      $interroga = *on
102050091104      * devo avere il codice cliente/visita/trattativa
102060091104      * visto che non passo dalla prima videata dove viene richiesto il codice
102070091104      * per comodità ricevo tutti e 3 i codici nello stesso campo
102080091104      * tanto o sono da visita o sono da trattativa o sono da clienti
102090091104      * se il codice è a 0 torno l'errore al chiamante
102100091104     c                   if        ta60nrv = 0
102110091104     c                   eval      ta60msg = 'Codice non passato'
102120091104     c                   eval      ta60err = '1'
102130091124     c                   Eval      *In28 = *On
102140091104     c                   leavesr
102150091104     c                   endif
102160091111      * accendo l'indicatore di interrogazione
102170091111     c                   eval      *in05 = *on
102180091111      * se flag TA60FLG = 'I'
102190091111      * imposto il codice cliente, in quanto è l'unico caso non
102200091104      * previsto nelle specifiche precedenti, visita e trattativa
102210091104      * sono già impostate se TA60FLG = 'T' o 'V'
102220091106     c                   if        ta60flg = 'I'
102230091111     c                   eval      v1cksc = ta60nrv
102240091104     c                   endif
102250091104     c                   endif
102260091124      * in caso di pgm richiamato in manutenzione
102270091124     c                   if        ta60flg = 'M'
102280091124     c                   if        ta60cli = 0
102290091124     c                   eval      ta60msg = 'Codice non passato'
102300091124     c                   eval      ta60err = '1'
102310091124     c                   Eval      *In28 = *On
102320091124     c                   leavesr
102330091124     c                   endif
102340091124     c                   eval      $modifica = *on
102350091124     c                   eval      v1cksc = ta60cli
102360091124     c                   endif
102370110407
102380110407      * PGM Richiamato per blocco cliente
102390110407     c                   IF        %Parms > 2
102400110407     c                   IF        Parmcbl = *blanks or Parmcbl = '000'
102410110407     c                   eval      ta60msg = 'Causale blocco errata'
102420110407     c                   eval      ta60err = '1'
102430110407     c                   Eval      *In28 = *On
102440110407     c                   leavesr
102450110407     c                   ENDIF
102460110407     c                   eval      $Blocco = *on
102470110407     c                   ENDIF
102480091104
102490091104      * NON richiamato
102500051115     c                   Else
102510091104      * Interrogazione
102520051115     c                   If        %subst(kpjbu:256:1) = 'V'
102530051115     c                   Eval      *In05 = *On
102540051115     c                   EndIf
102550051222     c                   Clear                   v1cnrv
102560060308     c                   eval      *in65 = *off
102570051115     c                   EndIf
102580101130
102590101130      * Se pgm in interrogazione imposto subito il titolo 'VISUALIZZA'
102600101130     c   05              Eval      vtcmod = mod(03)
102610051107
102620051107     c     *dtaara       define    §azute        azuteds
102630051107     c     *dtaara       define    §datiute      ddatiute
102640051107     c                   in(E)     *dtaara
102650051117     c                   If        %error  or rsut = *blanks
102660051107     c                   Clear                   Tibs34ds
102670051107     c                   Call      'TIBS34R'
102680051107     c                   Parm                    Tibs34ds
102690051107     c                   In        *dtaara
102700051107     c                   EndIf
102710051107
102720051107     c                   Clear                   wabi
102730051107     c                   Clear                   dLat
102740051107
102750051107      * Verifica errori e autorità profilo
102760051107s   1c                   Select
102770051107      * se ho errori nei dati utente esco dal pgm
102780051117w   1c                   When      duterr = 'E'
102790051115     c                   Eval      *In08 = *On
102800051115     c                   Eval      *In28 = *On
102810051115     c                   Eval      v1cmsg = msg(01)
102820051115      * se richiamato passo il messaggio
102830051115     c                   If        *In06
102840051115     c                   Eval      ta60msg = v1cmsg
102850051115     c                   Eval      ta60err = '1'
102860051115     c                   EndIf
102870051115     c                   Leavesr
102880051107      * se non c'è l'abilitazione
102890051107      * --> se 1° livello, abilitazioni al terminal
102900051107      *     se 2° livello, abilitazioni al punto operativo
102910051107      *     se sede è impossibile ma se capita mando a fine il pgm
102920051117w   1c                   When      uteaut = *Blanks
102930051117if  2c                   If        dutlpo = '1'
102940051107     c                   Eval      wabi   = 'TP'
102950051107e   2c                   EndIf
102960051117if  2c                   If        dutlpo = '2'
102970051107     c                   Eval      wabi   = 'PO'
102980051107e   2c                   EndIf
102990051117if  2c                   If        dutlpo = 'S'
103000051115     c                   Eval      *In08 = *On
103010051115     c                   Eval      *In28 = *On
103020051115     c                   Eval      v1cmsg = msg(01)
103030051115      * se richiamato passo il messaggio
103040051115     c                   If        *In06
103050051115     c                   Eval      ta60msg = v1cmsg
103060051115     c                   Eval      ta60err = '1'
103070051115     c                   EndIf
103080051115     c                   Leavesr
103090051107e   2c                   EndIf
103100051107      * carica le abilitazioni del profilo
103110051107x   1c                   Other
103120051117     c                   Movel     utefaf        dute01
103130051117if  2c                   If        §utecli <> *Blanks
103140051117     c                   Eval      wabi = §utecli
103150051107   x2c                   Else
103160051117     c                   Eval      wabi = uteaut
103170051107e   2c                   EndIf
103180051107e   1c                   EndSl
103190051107
103200051107      * controllo se ok l'abilitazione dell'utente
103210051107     c                   Clear                   Tibs02ds
103220051116     c                   Eval      t02mod = 'C'
103230051116     c                   Eval      t02sif = knsif
103240051116     c                   Eval      t02cod = 'LAT'
103250051116     c                   Movel(p)  wabi          t02ke1
103260051107     c                   Call      'TIBS02R'
103270051107     c                   Parm                    kpjba
103280051107     c                   Parm                    Tibs02ds
103290051117     c                   If        t02err = *Blanks
103300051116     c                   Eval      dLat = t02uni
103310051117     c                   EndIf
103320051107      * errore o non abilitato
103330051117     c                   If        t02err <> *Blanks or §latabi = 'S'
103340051107     c                   Eval      *In08 = *On
103350051107     c                   Eval      *In28 = *On
103360051108     c                   Eval      v1cmsg = msg(01)
103370051115      * se richiamato passo il messaggio
103380051115     c                   If        *In06
103390051115     c                   Eval      ta60msg = v1cmsg
103400051115     c                   Eval      ta60err = '1'
103410051115     c                   EndIf
103420051115     c                   Leavesr
103430051117     c                   EndIf
103440051107
103450051117      * Reperimento dei P.O. gestibili dall'utente per i codici clienti
103460051107     c                   Clear                   Trul31ds
103470051117     c                   Eval      i31abi = wabi
103480051117     c                   Eval      i31cdi = dutdis
103490051117     c                   Eval      i31car = dutare
103500051117     c                   Eval      i31cpo = dutpou
103510051107     c                   Call      'TRUL31R'
103520051117     c                   Parm                    kpjba
103530051107     c                   Parm                    Trul31ds
103540051117     c                   If        o31pog > *zeros
103550051117     c                   Movea     o31pog        skpog
103560051117     c                   Else
103570051107     c                   Eval      *In08 = *On
103580051107     c                   Eval      *In28 = *On
103590051107     c                   Eval      v1cmsg = msg(01)
103600051115      * se richiamato passo il messaggio
103610051115     c                   If        *In06
103620051115     c                   Eval      ta60msg = v1cmsg
103630051115     c                   Eval      ta60err = '1'
103640051115     c                   EndIf
103650051115     c                   Leavesr
103660051117     c                   EndIf
103670160905      /free
103680160905       //?Se utente abilitato alla gestione clienti logistica aggiungo
103690160905       //?alla sk SKPOG le filiali con NTW LOG
103700160905         IF  §UTEKscLog = 'S';
103710160905           exsr CaricaFilLog;
103720160905         ENDIF;
103730160905      /end-free
103740051115
103750051117      * Reperimento dei P.O. gestibili dall'utente per i codici potenziali
103760051117     c                   If        §utepot <> *Blanks
103770051117     c                   Eval      wabi = §utepot
103780051117     c                   EndIf
103790051117     c                   Clear                   Trul31ds
103800051117     c                   Eval      i31abi = wabi
103810051117     c                   Eval      i31cdi = dutdis
103820051117     c                   Eval      i31car = dutare
103830051117     c                   Eval      i31cpo = dutpou
103840051117     c                   Call      'TRUL31R'
103850051117     c                   Parm                    kpjba
103860051117     c                   Parm                    Trul31ds
103870051117     c                   If        O31pog > *zeros
103880051117     c                   Movea     O31pog        skpogcpo
103890051117     c                   EndIf
103900051117
103910051115      * Controllo se sono in sede
103920051115     c                   Eval      *In09 = (simfel = *zeros)
103930051128
103940051128      * Controllo se utente EDP
103950051129     c                   Eval      *In20 = (%subst(knmus:1:3) = 'EDP')
103960051116
103970060110      * Se sono in filiale
103980060110if  1c                   If        Not *In09
103990060110      * Verifico se il P.O. utente può manutenzionare l'anagrafico clienti
104000051116      * deve esistere la tabella MTC per il p.o. utente
104010051116     c                   Clear                   Tibs02ds
104020051116     c                   Clear                   dmtc
104030051116     c                   Eval      t02mod = 'C'
104040051116     c                   Eval      t02sif = knsif
104050051116     c                   Eval      t02cod = 'MTC'
104060051116     c                   Movel(p)  dutpou        t02ke1
104070051116     c                   Call      'TIBS02R'
104080051116     c                   Parm                    kpjba
104090051116     c                   Parm                    Tibs02ds
104100060110if  2c                   If        t02err <> *Blanks
104110051116     c                   Eval      *In28 = *On
104120051116     c                   Eval      v1cmsg = msg(01)
104130051116      * se richiamato passo il messaggio
104140051116     c                   If        *In06
104150051116     c                   Eval      ta60msg = v1cmsg
104160051116     c                   Eval      ta60err = '1'
104170051116     c                   EndIf
104180051116     c                   Leavesr
104190060110   x2c                   Else
104200051125     c                   Eval      dmtc = t02uni
104210060110    2c                   EndIf
104220060110
104230060110      * Se sono in sede deve esserci la tabella MTC key sede
104240060110   x1c                   Else
104250060110     c                   Clear                   Tibs02ds
104260060110     c                   Clear                   dmtc
104270060110     c                   Eval      t02mod = 'C'
104280060110     c                   Eval      t02sif = knsif
104290060110     c                   Eval      t02cod = 'MTC'
104300060110     c                   Movel(p)  'SEDE'        t02ke1
104310060110     c                   Call      'TIBS02R'
104320060110     c                   Parm                    kpjba
104330060110     c                   Parm                    Tibs02ds
104340060110if  2c                   If        t02err <> *Blanks
104350060110     c                   Eval      *In28 = *On
104360060110     c                   Eval      v1cmsg = msg(01)
104370060110      * se richiamato passo il messaggio
104380091106     c                   If        *In06
104390060110     c                   Eval      ta60msg = v1cmsg
104400060110     c                   Eval      ta60err = '1'
104410060110     c                   EndIf
104420060110     c                   Leavesr
104430060110   x2c                   Else
104440060110     c                   Eval      dmtc = t02uni
104450060110    2c                   EndIf
104460060110    1c                   EndIf
104470051115
104480051117      * Apro i file delle anagrafiche provvisorie se richiamato da gest.visite
104490051115     c                   If        *In06
104500051115     c                   Open      Tfaco00f
104510051115     c                   Open      Tfind00f
104520051115     c                   Open      Tfclp00f
104530051117     c                   Open      Tfcls01l
104540080306     c                   Open      Tncpo05l
104550080306     c                   Open      Tncpo06l
104560051115     c                   EndIf
104570051117
104580051117      * Apro i file delle anagrafiche effettive se non richiamato da gest.visite
104590051117     c                   If        Not *In06
104600051117     c                   Open      Cnind00f
104610060105     c                   Open      Cnind02l
104620051117     c                   Open      Cnclp00f
104630051220     c                   Open      Fnacr01l
104640051117     c                   Open      Fncls01l
104650051117     c                   Open      Fnspe01l
104660051219     c                   Open      Fnsp201l
104670060526     c                   EndIf
104680091112
104690091112      * Apro i file dei luoghi se trattativa su cliente
104700091112     c                   If        *In70
104710091112     c                   Open      Fnspe01l
104720091112     c                   Open      Fnsp201l
104730091112     c                   EndIf
104740051115
104750051222      * Passo i dati del capoconto se sono richiamato da visite/offerte
104760091106      * visto che salto la prima videata di richiesta codice
104770091124     c                   If        $pgmrich
104780051115     c                   Eval      v1ckcc = dutkci
104790051115     c                   EndIf
104800051107
104810051107      * Carico £4 capoconti gestiti solo in filiale e non in sede
104820051107     c                   Clear                   £4
104830051107     c                   Eval      kcod = '£4'
104840051108     c                   Eval      kkey = '       1'
104850051108     c     kTabel        Chain     Tabel00f
104860051107     c                   If        %Found(Tabel00f) and
104870051125     c                             tblflg = *Blanks
104880051125     c                   Movea     tbluni        £4
104890051107     c                   EndIf
104900051117
104910051117      * Recupero la divisa da tabella 'GED'
104920051117     c                   Clear                   Tibs02ds
104930051117     c                   Clear                   dged
104940051117     c                   Eval      t02mod = 'C'
104950051117     c                   Eval      t02sif = knsif
104960051117     c                   Eval      t02cod = 'GED'
104970051117     c                   Eval      t02ke1 = '1'
104980051117     c                   Call      'TIBS02R'
104990051117     c                   Parm                    kpjba
105000051117     c                   Parm                    Tibs02ds
105010051117     c                   If        t02err = *Blanks
105020051125     c                   Eval      dged = t02uni
105030051117     c                   EndIf
105040051117
105050051117      * Recupero il diritto di fatturazione di cartello
105060051117     c                   Clear                   Tibs02ds
105070051117     c                   Clear                   dgei
105080051117     c                   Eval      t02mod = 'C'
105090051117     c                   Eval      t02sif = knsif
105100051117     c                   Eval      t02cod = 'GEI'
105110051117     c                   Eval      t02ke1 = §gedcn
105120051117     c                   Call      'TIBS02R'
105130051117     c                   Parm                    kpjba
105140051117     c                   Parm                    Tibs02ds
105150051117     c                   If        t02err = *Blanks
105160051125     c                   Eval      dgei = t02uni
105170051117     c                   EndIf
105180051221
105190051221      * Recupero i dft tipo comunicazioni giacenze
105200051221     c                   Clear                   ds2g
105210051221     c                   Eval      kcod = '2G'
105220051221     c                   Eval      kkey = '1'
105230051221     c     kTabel        Chain     Tabel00f
105240051221     c                   If        %Found(Tabel00f) and
105250051221     c                             tblflg = *Blanks
105260051221     c                   Eval      ds2g = tbluni
105270051221     c                   EndIf
105280051219
105290051219      * Aggancio il capoconto su Cnaco per recuperare acoopz e acotic
105300051219      * uguali per tutti i clienti
105310051219     c     kCnaco1       Chain(n)  Cnaco00f
105320051219     c                   If        %Found(Cnaco00f)
105330051219     c                   Eval      wopz = acoopz
105340051219     c                   Eval      wtic = acotic
105350051219     c                   Else
105360051219     c                   Eval      wopz = '1100100001'
105370051219     c                   Eval      wtic = 'CG'
105380051219     c                   EndIf
105390051219
105400051129      * Imposto la data odierna
105410051129     c                   Z-add     *date         dataoggi
105420051129      * Imposto la data odierna -1 anno
105430051129     c                   Move      dataoggi      dataiso
105440051129     c                   Subdur    1:*y          dataiso
105450051129     c                   Move      dataiso       dataoggi1
105460100805
105470150424       //?cerco il tipo pag. di default da impostare in immissione
105480100805         exec sql
105490100805         declare PAG cursor for
105500100805         select TBEke1, TBEke2 from TNTBE00F
105510100805         where  TBEcod = 'PAG' and substr(TBEuni, 37, 2) = 'SI';
105520100805
105530100805         exec sql
105540100805           open PAG;
105550100805           IF sqlcode < 0;
105560100805             $End = *on;
105570100805           ENDIF;
105580100805
105590100805         DOW not $End;
105600100805           exec sql
105610100805             fetch next from PAG into :TBEke1, :TBEke2;
105620100805             IF sqlcod = 100 or sqlcod < 0;
105630100805               $End = *on;
105640100805               leave;
105650100805             ENDIF;
105660150424             IF  TBEke1 = 'DN';
105670100805               dfttipdn = TBEke2;
105680100805             ENDIF;
105690150424             IF  TBEke1 = 'NA';
105700100805               dfttipna = TBEke2;
105710100805             ENDIF;
105720100805         ENDDO;
105730100805
105740100805         exec sql close PAG;
105750170421
105760170421       //?carico in sk i clienti IMPORT DPD
105770170421         $End = *off;
105780170421         clear ww;
105790170421         exec sql
105800170421         declare TabLPD cursor for
105810170421         select TBEuni from TNTBE00F
105820170421         where TBEcod = 'LPD';
105830170421         exec sql open TabLPD;
105840170421         IF  sqlcode < 0;
105850170421           $End = *on;
105860170421         ENDIF;
105870170421         DOW  not $End;
105880170421           exec sql fetch next from TABLPD into :TBEuni;
105890170421           IF  sqlcod = 100 or sqlcod < 0;
105900170421             $End = *on;
105910170421             leave;
105920170421           ENDIF;
105930170421           dLPD = TBEUni;
105940170421           IF  §LPDksc = 0;
105950170421             iter;
105960170421           ENDIF;
105970170421           ww += 1;
105980170421           CliImport(ww) = §LPDksc;
105990170421         ENDDO;
106000170421         exec sql close TabLPD;
106010100805
106020100805      /end-free
106030051107
106040051107      * KLIST
106050051129     c     kCnabi        Klist
106060051129     c                   Kfld                    kabi
106070051129     c                   Kfld                    kcab
106080051129
106090051108     c     kCnaco        Klist
106100051108     c                   Kfld                    codut
106110051107     c                   Kfld                    v1ckcc
106120051222     c                   Kfld                    kksc
106130051116
106140051116     c     kCnaco1       Klist
106150051116     c                   Kfld                    codut
106160051116     c                   Kfld                    v1ckcc
106170051116     c                   Kfld                    wksc
106180060105
106190060105     c     kCnind        Klist
106200060105     c                   Kfld                    codut
106210060105     c                   Kfld                    v1ckcc
106220060109     c                   Kfld                    kindiva
106230051108
106240051108     c     kFnspe        Klist
106250051216     c                   Kfld                    kspefls
106260051216     c                   Kfld                    kspecli
106270051216     c                   Kfld                    kspecod
106280051219
106290051219     c     kFnsp2        Klist
106300051219     c                   Kfld                    kspecli
106310051219     c                   Kfld                    kspecod
106320051219     c                   Kfld                    ksp2tpe
106330051129
106340051129     c     kTabel        Klist
106350051129     c                   Kfld                    codut
106360051129     c                   Kfld                    kcod
106370051129     c                   Kfld                    kkey
106380051108
106390051129     c     kTfntc        klist
106400051216     c                   kfld                    kntcapl
106410051216     c                   kfld                    kntcnk1
106420051216     c                   kfld                    kntcnk2
106430051216     c                   kfld                    kntctnt
106440051107
106450051107     c                   EndSr
106460160905      /free
106470160905       //--------------------------------------------------------------
106480160905       //?Carico le filiali Logistica nella SKPOG.
106490160905       //--------------------------------------------------------------
106500160905       BEGSR CaricaFilLog;
106510160905
106520160905         $End = *off;
106530160905
106540160905         exec sql
106550160905         declare WLOG cursor for
106560160905         select ORGfil from AZORGLOG;
106570160905
106580160905         exec sql
106590160905         open WLOG;
106600160905         IF  sqlcode < 0;
106610160905           $End = *on;
106620160905         ENDIF;
106630160905
106640160905         DOW not $End;
106650160905           exec sql
106660160905           fetch next from WLOG into :ORGfil;
106670160905           IF  sqlcod = 100 or sqlcod < 0;
106680160905             $End = *on;
106690160905             leave;
106700160905           ENDIF;
106710160905
106720160905           IF  %lookup(%editc(ORGfil:'X'):SKpog) = 0;
106730160905             ww = %lookup(*zeros:SKpog);
106740160905             IF  ww > 0 and xx < 250;
106750160905               SKpog(ww) = %editc(ORGfil:'X');
106760160905             ENDIF;
106770160905           ENDIF;
106780160905
106790160905         ENDDO;
106800160905
106810160905         exec sql close WLOG;
106820160905
106830160905       ENDSR;
106840160905      /end-free
106850051107      *------------------------------------------------------------------------*
106860051212** CAR  Lungh. 10
106870051212!:§@
106880051107** MOD  Lungh. 15                                                            *
106890051107  IMMISSIONE                                                                  01
106900051107 MANUTENZIONE                                                                 02
106910051107VISUALIZZAZIONE                                                               03
106920051107   ANNULLATO                                                                  04
106930051107** MSG  Lungh. 78                                                            *
106940051107Utente non autorizzato alla Gestione anagrafico clienti                       01
106950051107Immettere almeno un dato                                                      02
106960051107Immettere il capoconto                                                        03
106970051107Capoconto non gestibile                                                       04
106980051115Cliente inesistente                                                           05
106990051116Password errata                                                               06
107000051116Immettere la password                                                         07
107010051117Password scaduta                                                              08
107020051117Manca tabella di controllo scadenza password. TEL CED SEDE !!!!!              09
107030051117Cliente non gestibile dall'utente                                             10
107040051117Immettere la ragione sociale                                                  11
107050051118Immettere il codice potenziale                                                12
107060051118Codice potenziale errato                                                      13
107070051118Cliente potenziale non gestibile dall'utente                                  14
107080051118Stato che non prevede il cappario: utilizzabile solo per clienti bloccati     15
107090051118Impossibile utilizzare cap o localita' obsoleti                               16
107100051118Stato errato                                                                  17
107110051118Immettere la partita IVA                                                      18
107120051118Codice fiscale formalmente errato                                             19
107130051122Codice commerciale errato                                                     20
107140090617Fil. del codice commerciale incongruente con la fil. del cliente              21
107150051122Il cliente è da bloccare perchè inattivo                                      22
107160051122Stato del credito errato                                                      23
107170090617Immettere stato del credito gest. da fil. e senza passaggio al contenzioso    24
107180051122Causale blocco cliente errata                                                 25
107190051122Immettere la causale blocco perchè il cliente è bloccato                      26
107200051122Per immettere la causale blocco occorre bloccare il cliente                   27
107210051128Immettere il codice importanza cliente                                        28
107220051128Codice importanza cliente errata                                              29
107230080313Immettere la categoria Merceologica                                           30
107240080313Categoria Merceologica errata                                                 31
107250051212La ragione sociale contiene uno dei seguenti caratteri non validi -- ! : § @  32
107260051128Codice Sottoconto fattura non impostato                                       33
107270051128Codice Sottoconto fattura non utilizzabile                                    34
107280051128Codice Sottoconto fattura errato                                              35
107290051128Codice Sottoconto fattura annullato                                           36
107300051128Tipo fattura errato                                                           37
107310051128Tipo fattura non utilizzabile in                                              38
107320051128Immettere la frequenza fattura                                                39
107330051128Frequenza fattura errato                                                      40
107340051128Per tipo fattura "1" è ammessa solo la frequenza mensile                      41
107350120613Per frequenza settimanale chiedere autorizzazione alla DIR. AMMINISTRATIVA!!  42
107360051128Immettere il tipo data fattura                                                43
107370051128Tipo data fattura errato                                                      44
107380051128Sottoconto fattura diverso da codice cliente è possibile unificare            45
107390051128Verificare il flag Fattura Unificata anche nel sottoconto fattura             46
107400051128Verificare il flag Fattura Unificata anche nei codici collegati               47
107410051128Spese invio fattura errato. Troppi decimali, massimo 2                        48
107420060208Per interrogare le info.commerciali inserire il cod. potenziale               49
107430060203Il luogo non è stato immesso. Impossibile Visualizzare                        50
107440061030Immettere il codice fiscale                                                   51
107450060203La nota non è stata immessa. Impossibile Visualizzare                         52
107460051129Codice pagamento errato                                                       53
107470051129Codice ABI/CAB obbligatorio se pagamento con RB                               54
107480051129Codice ABI/CAB obbligatorio se immessa descrizione banca o nr. C/C            55
107490051130Codice ABI/CAB non valido                                                     56
107500100729Tipo pagamento errato                                                         57
107510100729Tipo pagamento non utilizzabile per clienti vari                              58
107520051130Coordinate bancarie errate. Correggere i dati immessi o contattare il cliente 59
107530051130Per l'estero l'ABI deve essere 99999 e il CAB deve essere 99999               60
107540051206Immettere i codici ABI/CAB                                                    61
107550051212Tipo Comunicazione al Mittente errato                                         62
107560051212Tipo invio comunicazione non utilizzabile                                     63
107570051212Apertura automatica giacenza errato                                           64
107580051212Apertura automatica giacenza non possibile per clienti vari                   65
107590051212Disposizione automatica giacenza non possibile per clienti vari               66
107600051213Immettere un numero stop ritiro minore di 800                                 67
107610051213Il codice richiesto è già in uso da altro utente. Riprovare più tardi         68
107620051216Impossibile confermare. Messaggi in sospeso su altre videate                  69
107630060216Tipo Comunicazione Fine Giacenza errato                                       70
107640051220Impossibile modificare cod.intestazione fattura inserito dalla SEDE           71
107650051220ATTENZIONE!!! Codice già presente in anagrafica clienti ritiro                72
107660170117Impossibile intestare fattura ad un codice in sofferenza.                     73
107670060801Chiusura automatica giacenza non possibile per clienti vari                   74
107680061113Inserire Codice Fiscale solo se diverso dalla Partita IVA. Enter per forzare. 75
107690071128Luogo non utilizzabile                                                        76
107700080306Cod.fiscale già presente su altri potenziali. Enter per forzare               77
107710080306Partita Iva già presente su altri potenziali. Enter per forzare               78
107720080422Non è possibile inserire coordinate bancarie estere se tipo pagamento italia. 79
107730080515Codice pagamento utilizzabile SOLO dalla SEDE                                 80
107740090421Non consentita fattura settimanale se cod.int.fattura con autofattura         81
107750090421No autofattura un codice collegato al cod.int.fattura ha frequenza settimanale82
107760090616Il cliente deve essere incluso nella procedura di sollecito automatico        83
107770090616Il cliente deve essere escluso dalla procedura di sollecito automatico        84
107780100729Codice pagamento NON utilizzabile                                             85
107790100730Per pagamento DANNI con Bonifico immettere la relativa e-mail                 86
107800150427Mancano i codici IBAN per i pagamenti tramite Bonifico. Enter x forzare.      87
107810150805Manca indirizzo mail per inoltro Progetto di Liquidazione. Enter x forzare.   88
107820160517Pagamento con RB non ammesso per cliente estero                               89
107830160517Pagamento con RB non ammesso su conto BancoPosta                              90
107840160914Filiale errata o non in gestione.                                             91
107850060731** TF04
107860060731F4=Abilitazioni  #
107870100524** TF22
107880100524F22=Richiesta Contatto  #
107890100507** TF05
107900100507F5=Attività  #
107910060203** TF12
107920060203F12=Ritorno  #
107930051206** TF14
107940051206F14=InfoCommerciali  #
107950051122** TF15
107960051129F15=Codici Collegati  #
107970060731** TF19
107980060526F19=Variazioni  #
107990100407** TF20
108000100407F20=Potenziali  #
108010101213** TF17
108020101213F17=Dati Fatturazione  #
108030110223** TF21
108040110223F21=Blocco Cliente  #
