000010051102     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000020051102
000030051102      *------------------------------------------------------------------------*
000040051117      *
000050051117      *                 GESTIONE ANAGRAFICO CLIENTI        ?
000060051117      *
000070051102      *------------------------------------------------------------------------*
000080060406      * ? ATTENZIONE!!!!! ?
000090060406      *? ?  Il posizionamento sul file video dei campi errati è gestito con le
000100060406      *? ?  posizioni di riga e colonna del campo a video e non con gli
000110060406      *? ?  indicatori, ne servivano troppi.
000120060406      *? ?  Se viene spostato un campo nel video aggiornare anche il relativo
000130060406      *? ?  posizionamento del cursore, campi HxRIGA e HxCOLO
000140051117      *
000150051117      *------------------------------------------------------------------------*
000160051102
000170051221     fCnaco00f  uf a e           k disk
000180120510     fCnaco16l  if   e           k disk    rename(cnaco000:cnaco16)  usropn
000190120510     f                                     prefix (ww_)
000200051115     fTfaco00f  uf a e           k disk    rename(cnaco000:tfaco000) usropn
000210060105     fCnind00f  uf a e           k disk    usropn
000220060105     fCnind02l  if   e           k disk    rename(cnind000:cnind002) usropn
000230060301     f                                     prefix(ww_)
000240051115     fTfind00f  uf a e           k disk    rename(cnind000:tfind000) usropn
000250051117     fCnclp00f  uf a e           k disk    usropn
000260051115     fTfclp00f  uf a e           k disk    rename(cnclp000:tfclp000) usropn
000270051117     fFncls01l  uf a e           k disk    usropn
000280051124     fTfcls01l  uf a e           k disk    rename(fncls000:tfcls000) usropn
000290051108     fAzorg01l  if   e           k disk
000300130806     fAZCMM01L  if   e           k disk
000310051129     fCnabi00f  if   e           k disk
000320051220     fFnacr01l  if   e           k disk    usropn
000330051117     fFnspe01l  if   e           k disk    usropn
000340051219     fFnsp201l  if   e           k disk    usropn
000350051107     fTabel00f  if   e           k disk
000360091119     fTfntc01l  uf a e           k disk
000370051220     fTncpo01l  uf   e           k disk
000380080306     FTNCPO05L  IF   E           K DISK    rename(tncpo000:tncpo005)
000390080306     F                                     prefix(c_)
000400080306     f                                     usropn
000410080306     FTNCPO06L  IF   E           K DISK    rename(tncpo000:tncpo006)
000420080306     F                                     prefix(c_)
000430080306     f                                     usropn
000440140714     fTncpo11l  if   e           k disk
000450110516     FTIATC07L  IF   E           K DISK
000460120510     fTICPI01L  if   e           k disk
000470051102     fTnta60d   cf   e             workstn
000480051107
000490051107      *------------------------------------------------------------------------*
000500051107      *  RIEPILOGO INDICATORI
000510051107      *------------------------------------------------------------------------*
000520051107      * 01 - Immissione
000530051107      * 02 - Modifica
000540051107      * 04 - Annullato
000550051107      * 05 - Visualizza
000560100806      * 06 - Anagrafica provvisoria da trattativa
000570051115      * 07 - Convalida offerte
000580051107      * 08 - Utente non abilitato - esce dal pgm
000590051115      * 09 - Sono in sede
000600051115      * 10 - Luogo 001 presente
000610051115      * 11 - Luogo 555 presente
000620051115      * 12 - Luogo 666 presente
000630051115      * 13 - Nota   85 presente
000640051115      * 14 - Luogo 005 presente
000650051115      * 15 - Nota   88 presente
000660051115      * 16 - Luogo 006 presente
000670051115      * 17 - Luogo 008 presente
000680051115      * 18 - Nota   87 presente
000690120928      * 19 - Protegge stato del credito
000700051128      * 20 - Utente EDP
000710051129      * 21 - Nota   84 presente
000720051129      * 22 - Non visualizzo le coordinate bancarie per pag. c/a
000730051129      * 23 - Proteggo le coordinate bancarie per pag. c/a
000740140722      * 24 - Note 02 o 03 presenti
000750140722      * 26 - Note 05 o 06 o T5 o I5 presenti
000760051110      * 28 - Errore generico
000770051221      * 29 - Sono in seconda videata
000780051107      * 30 - Comodo
000790140722      * 31 - Note 07 o 08 o T7 o I7 presenti
000800060208      * 33 - Luogo 300 presente
000810060208      * 34 - Nota   83 presente
000820060208      * 35 - Luogo 002 presente
000830060208      * 36 - Nota   86 presente
000840060208      * 37 - Luogo 200 presente
000850061106      * 39 - Codice ISO impostato in automatico
000860090612      * 40 - Nota   89 presente
000870051117      * 41/64 - P.o. di ricerca
000880100407      * 65 - Inibisco il tasto F20 potenziale xchè se no chiamata ricorsiva
000890100507      * 67 - Abilito F5-Attività
000900100728      * 68 - Nota   82 presente
000910091112      * 70 - Trattativa su cliente
000920100728      * 71 - Non visualizzo le coordinate bancarie per pag. Danni
000930100728      * 72 - Proteggo le coordinate bancarie per pag. Danni
000940100728      * 73 - Non visualizzo le coordinate bancarie per pag. Note Accr.
000950100728      * 74 - Proteggo le coordinate bancarie per pag. Note Accr.
000960100803      * 75 - Non visualizzo BIC contrassegni
000970100803      * 76 - Non visualizzo BIC danni
000980100803      * 77 - Non visualizzo BIC note accredito
000990111007      * 78 - F4 abilitato
001000120925      * 79 - Protegge blocco cliente
001010060217      * 80 - Non devo uscire dal pgm se F3
001020120928      * 81 - Protegge blocco pagamenti
001030150415      * 82 - Protegge Banca ABI/CAB pagamento fatture
001040170320      * 83 - Protegge Frequenza Fattura x Cl.in contenzioso
001050170320      * 84 - Protegge Codice Pagamento x Cl.in contenzioso
001060170421      * 85 - Protegge Flag Gestione Giacenze
001070051124      * 90 - Riemetto la videata
001080051107      *------------------------------------------------------------------------*
001090051107
001100051107      *   V A R I A B I L I
001110150424     d BonificoCA      s               n   inz(*off)
001120150424     d BonificoDN      s               n   inz(*off)
001130150424     d BonificoNA      s               n   inz(*off)
001140150427     d CodPagCA        s              1a   inz
001150150427     d CodPagDN        s              1a   inz
001160150427     d CodPagNA        s              1a   inz
001170051219     d codut           s                   like(TblKut) inz(1)
001180051118     d comando         s              1
001190090417     d conta           s              5i 0
001200051116     d dataiso         s               d   datfmt(*iso)
001210060526     d dataeur         s               d   datfmt(*eur)
001220051116     d dataoggi        s              8  0
001230051129     d dataoggi1       s              8  0
001240051116     d datascad        s              8  0
001250100406     d dataCRM         s              8  0
001260161129     d EndW06          s               n   inz(*off)
001270150424     d EoFW06          s               n   inz(*off)
001280080116     d erriban         s               n
001290051129     d kabi            s                   like(IndAbi)
001300051129     d kcab            s                   like(IndCab)
001310051129     d kcod            s                   like(TblCod)
001320060109     d kindiva         s                   like(IndIva)
001330051129     d kkey            s                   like(TblKey)
001340051222     d kksc            s                   like(AcoKsc)
001350051108     d kkut            s              1  0
001360051216     d kntcapl         s                   like(NtcApl)
001370051216     d kntcnk1         s                   like(NtcNk1)
001380051216     d kntcnk2         s                   like(NtcNk2)
001390051216     d kntctnt         s                   like(NtcTnt)
001400051216     d kspefls         s                   like(SpeFls) inz('L')
001410051216     d kspecli         s                   like(SpeCli)
001420051216     d kspecod         s                   like(SpeCod)
001430051219     d ksp2tpe         s                   like(Sp2Tpe) inz('EM')
001440051118     d ordina          s              1
001450150427     d PagEsteroCA     s               n   inz(*off)
001460150427     d PagEsteroDN     s               n   inz(*off)
001470150427     d PagEsteroNA     s               n   inz(*off)
001480051108     d parcdf          s             16
001490051108     d pardit          s              3
001500051108     d pardut          s             30
001510051129     d parkcc          s                   like(AcoKcc)
001520051108     d parerr          s              2
001530051108     d paresci         s              1
001540051108     d parfil          s             90
001550051108     d pariva          s             16
001560110407     d ParmCbl         s              3
001570051108     d parsta          s              1  0
001580051108     d paxkcm          s             80
001590051108     d paxkdm          s             60
001600051108     d paxksm          s            140
001610051108     d paxnum          s              2  0
001620090616     d rqsformatname...
001630090616     d                 S             10A
001640100729     d rqsData         s            256a
001650090616     d rqsdatasize...
001660090616     d                 S             10I 0
001670090616     d rqsopcode...
001680090616     d                 S             10A
001690090616     d rpyformatname...
001700090616     d                 S             10A
001710100729     d rpyData         s            256a
001720090616     d rpydatasize...
001730090616     d                 S             10I 0
001740090616     d rpyesito...
001750090616     d                 S             10I 0
001760100729     d savABIdn        s                   like(CLPabi)
001770100729     d savABIna        s                   like(CLPabi)
001780051220     d savabl          s                   like(AcoAbl)
001790100729     d savCABdn        s                   like(CLPcab)
001800100729     d savCABna        s                   like(CLPcab)
001810150427     d savCLStic       s                   like(CLStic)
001820051117     d savcpo          s                   like(AcoLib)
001830051130     d savfft          s                   like(v4cfft)
001840100730     d savIBANdn       s                   like(v5ibandn)
001850100730     d savIBANna       s                   like(v5ibanna)
001860171006     d savin05         s               n   inz
001870060215     d savin22         s              1
001880060215     d savin23         s              1
001890100728     d savin71         s              1
001900100728     d savin72         s              1
001910100728     d savin73         s              1
001920100728     d savin74         s              1
001930051130     d savitc          s                   like(v3citc)
001940091119     d savntcdc        s                   like(v2ntcdc)
001950081119     d savntwscf       s                   like(§ogntw)
001960100127     d savscf          s                   like(v4cscf)
001970051130     d savtft          s                   like(v4ctft)
001980100802     d savtipdn        s                   like(v5tpdn)
001990100802     d savtipna        s                   like(v5tpna)
002000100805     d dfttipdn        s                   like(v5tpdn)
002010100805     d dfttipna        s                   like(v5tpna)
002020051130     d savuni          s                   like(v4cuni)
002030080422     d sav4utip        s                   like(§4utip)
002040110217     d sav_Livello     s                   like(§oglpo) inz
002050051118     d xivaprv         s              2
002060051118     d xstato          s              1  0
002070051107     d xx              s              2  0
002080051108     d yy              s              2  0
002090160905     d ww              s              3s 0 inz
002100051107     d wabi            s                   like(UteAut)
002110051129     d wagenzia        s             16
002120051129     d wbanca          s             20
002130100730     d wbic            s                   like(v5bicdn)
002140051202     d wbloc           s              1
002150051118     d wcae            s                   like(v2ccae)
002160051118     d wcit            s                   like(v2ccit)
002170110217     d wCodIncAtt      s              7    inz
002180100805     d wcodpn          s                   like(v5tpdn)
002190100805     d wcodpo          s                   like(v5tpdn)
002200100729     d wdesbanca       s                   like(CLPbab)
002210051122     d wdestab         s             25
002220051212     d wfax005         s                   like(SpeFax)
002230051220     d wforza          s              1    inz(*Off)
002240061030     d wforzacdf       s              1    inz(*Off)
002250060105     d wforzacon       s              1    inz(*Off)
002260060711     d wforzaksc       s              1    inz(*Off)
002270061106     d wforzaiva       s              1    inz(*Off)
002280150728     d wForzaMail      s               n   inz(*off)
002290150424     d wForzaPag       s               n   inz(*off)
002300170516     d wForzaPotenziale...
002310170516     d                 s               n   inz(*off)
002320051128     d wfscf           s              1    inz
002330051122     d wfz3            s                   like(E13Fz3)
002340051116     d wgiorni         s              3  0
002350100729     d wiban           s                   like(v5ciban)
002360150424     d WinInfo         s               n   inz(*off)
002370051118     d wksc            s                   like(AcoKsc) inz
002380051118     d wksc04          s              4  0
002390051219     d wmail005        s                   like(Sp2Est)
002400051220     d wokabl          s              1    inz(*Off)
002410051219     d wokcap          s              1    inz(*Off)
002420051220     d wokimm          s              1    inz(*Off)
002430051219     d wokpot          s              1    inz(*Off)
002440051219     d wopz            s                   like(AcoOpz)
002450100728     d wpag            s              2a
002460100805     d wpgm            s             10a
002470051122     d wpoage          s              3  0
002480060711     d wpoageuni       s              3
002490051122     d wpocli          s              3
002500051117     d wpos            s              2  0
002510051129     d wpos1           s              2  0
002520051129     d wpos2           s              2  0
002530051118     d wprv            s                   like(v2cprv)
002540051124     d wrag            s             40
002550051207     d wrich           s                   like(ta12ric)
002560140722     d wRGR            s                   like(TA12rgr)   inz
002570051118     d wsta            s                   like(v2csta)
002580080222     d w_scfsta        s                   like(indsta)
002590051117     d wtel            s                   like(v2ctel)
002600051219     d wtic            s                   like(AcoTic)
002610051118     d wtlf            s                   like(v2ctlf)
002620051207     d wtnt            s                   like(NtcTnt)
002630051118     d wvia            s                   like(v2cvia)
002640051213     d wvideo          s              2
002650051108     d wtibs69         s              1    inz('0')
002660060531     d wtibs73         s              1    inz('0')
002670100127     d wtnta40         s              1n   inz(*off)
002680081020     d wtrmk50         s              1    inz('0')
002690101014     d wuffsede        s             35
002700051216     d w001a           s              1
002710051108     d w002a           s              2
002720051115     d w003a           s              3
002730051108     d w0030           s              3  0
002740051107     d w004a           s              4
002750051117     d w0040           s              4  0
002760070704     d w0040ksc        s              4  0
002770051124     d w005a           s              5
002780051220     d w0060           s              6  0
002790051124     d w0070           s              7  0
002800051220     d w0100           s             10  0
002810051108     d zz              s              2  0
002820110407     d $Blocco         s              1n
002830051219     d $d01            s              1    inz('0')
002840051129     d §fil            s             90
002850051129     d $h              s              3  0
002860091104     d $interroga      s              1n
002870051129     d $j              s              3  0
002880051129     d $k              s              3  0
002890051129     d $loop           s              1  0
002900091124     d $modifica       s              1n
002910091106     d $pgmrich        s              1n
002920051129     d $rig            s              1  0
002930051108     d §tip            s              1
002940051122     d $x              s              3  0
002950051122     d $y              s              3  0
002960051122     d $z              s              3  0
002970071128     d $ufi001         s                   like(§4lufi)
002980071128     d $ufi002         s                   like(§4lufi)
002990071128     d $ufi005         s                   like(§4lufi)
003000071128     d $ufi006         s                   like(§4lufi)
003010071128     d $ufi008         s                   like(§4lufi)
003020071128     d $ufi200         s                   like(§4lufi)
003030071128     d $ufi300         s                   like(§4lufi)
003040071128     d $ufi555         s                   like(§4lufi)
003050071128     d $ufi666         s                   like(§4lufi)
003060080422     d $win            s               n
003070100729     d $windn          s               n
003080100729     d $winna          s               n
003090080306     d savcdf          s                   like(v2ccdf) inz
003100080306     d cod_forzcdf     s                   like(cpocdf) inz
003110080306     d cod_forziva     s                   like(cpopiv) inz
003120080306     d savrag          s                   like(cporag)
003130080306     d saviva          s                   like(v2civa) inz
003140080306     d codice          s                   like(v2civa) inz
003150100805     d $End            s               n
003160160905     d ClienteLog      s               n   inz
003170171213     d ClienteSofferenza...
003180171213     d                 s               n   inz
003190051107
003200051107      *   S C H I E R E
003210051212     d car             s              1    dim(10) ctdata perrcd(10)
003220170421     d CliImport       s              7  0 dim(999) inz
003230051122     d mod             s             15    dim(04) ctdata perrcd(1)
003240160914     d msg             s             79    dim(91) ctdata perrcd(1)
003250051124     d rigatf1         s              1    dim(78)
003260051122     d rigatf2         s              1    dim(62)
003270051122     d rigatf3         s              1    dim(62)
003280051122     d skpo            s              3    dim(30)
003290051122     d skpog           s              3    dim(250) inz(*Zeros)
003300170914     d skpog_RA        s              3    dim(250) inz(*Zeros)
003310051122     d skpogcpo        s              3    dim(250) inz(*Zeros)
003320060731     d tf04            s              1    dim(25) ctdata perrcd(25)
003330100524     d tf22            s              1    dim(25) ctdata perrcd(25)
003340100507     d tf05            s              1    dim(25) ctdata perrcd(25)
003350060203     d tf12            s              1    dim(25) ctdata perrcd(25)
003360051206     d tf14            s              1    dim(25) ctdata perrcd(25)
003370051124     d tf15            s              1    dim(25) ctdata perrcd(25)
003380060526     d tf19            s              1    dim(25) ctdata perrcd(25)
003390100406     d tf20            s              1    dim(25) ctdata perrcd(25)
003400101213     d tf17            s              1    dim(25) ctdata perrcd(25)
003410110223     d tf21            s              1    dim(25) ctdata perrcd(25)
003420051122     d £4              s              4    dim(20)
003430051124     d $tf             s              1    dim(25)
003440051107
003450051107      *   D S   I N T E R N E / E S T E R N E
003460051109
003470051109     d                 ds
003480051109     d aa1                     1      2  0
003490051109     d mm1                     3      4  0
003500051109     d gg1                     5      6  0
003510051109     d dataamg                 1      6  0
003520051109
003530051109     d                 ds
003540051109     d gg2                     1      2  0
003550051109     d mm2                     3      4  0
003560051109     d aa2                     5      6  0
003570051109     d datagma                 1      6  0
003580051129
003590051129     d parmbanca       ds
003600051129     d  pabi                   1      5  0
003610051129     d  pcab                   6     10  0
003620051129     d  pist                  11     50
003630051129     d  ppro                  51     52
003640051129     d  pflg                  53     53
003650051124
003660051124     d parmf11         ds
003670051124     d  paropz                 1      1
003680051124     d  parcli                 2      8
003690060216     d  partnta60              9      9
003700060216     d  parimta60             10     10
003710051124
003720051124     d parmf15         ds
003730051124     d  parcli1                       7  0
003740080116
003750080116     d parmiban        ds
003760080116     d  parmstato              1      2
003770080116     d  parmcheck              3      4
003780080206     d  parmcoori              5     34
003790051107
003800051107     d                 ds
003810051107     d v1cf01                  1      3
003820051107     d v1cf02                  4      6
003830051107     d v1cf03                  7      9
003840051107     d v1cf04                 10     12
003850051107     d v1cf05                 13     15
003860051107     d v1cf06                 16     18
003870051107     d v1cf07                 19     21
003880051107     d v1cf08                 22     24
003890051107     d v1cf09                 25     27
003900051107     d v1cf10                 28     30
003910051107     d v1cf11                 31     33
003920051107     d v1cf12                 34     36
003930051107     d v1cf13                 37     39
003940051107     d v1cf14                 40     42
003950051107     d v1cf15                 43     45
003960051107     d v1cf16                 46     48
003970051107     d v1cf17                 49     51
003980051107     d v1cf18                 52     54
003990051107     d v1cf19                 55     57
004000051107     d v1cf20                 58     60
004010051107     d v1cf21                 61     63
004020051107     d v1cf22                 64     66
004030051107     d v1cf23                 67     69
004040051108     d v1cf24                 70     72
004050051108     d skfil                   1     72    dim(24)
004060051108
004070051108     d                 ds
004080051108     d v1ivaeu                 1      2
004090051108     d v1ivait                 3     16
004100051108     d v1civa                  1     16
004110051109
004120051109     d                 ds
004130051109     d v2ivaeu                 1      2
004140051109     d v2ivait                 3     16
004150051109     d v2civa                  1     16
004160080115
004170080115     d v5ciban         ds
004180080115     d  v5ciban1               1      4
004190080115     d  v5ciban2               5      8
004200080115     d  v5ciban3               9     12
004210080115     d  v5ciban4              13     16
004220080115     d  v5ciban5              17     20
004230080115     d  v5ciban6              21     24
004240080115     d  v5ciban7              25     28
004250080208     d  v5ciban8              29     32
004260100728
004270100728     d v5ibandn        ds
004280100728     d  v5ibandn1              1      4
004290100728     d  v5ibandn2              5      8
004300100728     d  v5ibandn3              9     12
004310100728     d  v5ibandn4             13     16
004320100728     d  v5ibandn5             17     20
004330100728     d  v5ibandn6             21     24
004340100728     d  v5ibandn7             25     28
004350100728     d  v5ibandn8             29     32
004360100728
004370100728     d v5ibanna        ds
004380100728     d  v5ibanna1              1      4
004390100728     d  v5ibanna2              5      8
004400100728     d  v5ibanna3              9     12
004410100728     d  v5ibanna4             13     16
004420100728     d  v5ibanna5             17     20
004430100728     d  v5ibanna6             21     24
004440100728     d  v5ibanna7             25     28
004450100728     d  v5ibanna8             29     32
004460051116
004470051116     d wlbdat          ds
004480051116     d  g02dat                 1      8  0
004490051116     d  g02inv                 9     16  0
004500051116     d  g02err                17     17
004510051116     d  g02tgi                18     22  0
004520051107
004530051107     d                sds
004540051107     d  Vtcpgm                 1     10
004550051107
004560051107     d Azuteds       e ds                  Extname(Azute00f)
004570051107     d dDatiute      e ds
004580100729     d dPag          e ds
004590051117     d dged          e ds
004600051117     d dgei          e ds
004610051116     d dlat          e ds
004620170421     d dLPD          e ds
004630051116     d dmtc          e ds
004640051108     d ds_cnaco      e ds                  inz  extname(Cnaco00f) prefix(w_)
004650051108     d ds_cnind      e ds                  inz  extname(Cnind00f) prefix(w_)
004660051108     d ds_cnclp      e ds                  inz  extname(Cnclp00f) prefix(w_)
004670051108     d ds_fncls      e ds                  inz  extname(Fncls00f) prefix(w_)
004680060531     d cnaco_73      e ds                  inz  extname(Cnaco00f)
004690060531     d cnind_73      e ds                  inz  extname(Cnind00f)
004700060531     d cnclp_73      e ds                  inz  extname(Cnclp00f)
004710060531     d fncls_73      e ds                  inz  extname(Fncls00f)
004720121105
004730121105      // - CPORST file TNCPO
004740121105     d dCPO01        e ds                  inz
004750121105
004760110323     d dsbi          e ds
004770051108     d dsfa          e ds
004780051108     d dsff          e ds
004790051108     d dsic          e ds
004800051108     d ds1l          e ds
004810051108     d ds1t          e ds
004820051125     d ds13          e ds
004830051118     d ds15          e ds
004840051108     d ds2f          e ds
004850051207     d ds2g          e ds
004860051108     d ds2h          e ds
004870051108     d ds2u          e ds
004880051108     d ds4l          e ds
004890051108     d ds4u          e ds
004900051115     d ds4w          e ds
004910090803     d w_ds4w        e ds                  extname(ds4w) prefix(w_)
004920051125     d dtft          e ds
004930051108     d dute01        e ds
004940110217     d dY4O          e ds
004950051118     d Fnlv13ds      e ds
004960051118     d Fnlv14ds      e ds
004970070809     d FIOR37ds      e ds                  inz
004980070809     d  I37cgi       e                     inz(*all'9')
004990051220     d Fior50ds      e ds
005000051108     d kpjba         e ds
005010100406     d OG141         e ds
005020051125     d og143         e ds
005030051220     d og148         e ds
005040051107     d Tibs02ds      e ds
005050051122     d Tibs12ds      e ds
005060051107     d Tibs34ds      e ds
005070051108     d Tibs69ds      e ds
005080060531     d Tibs73ds      e ds
005090051118     d Tisi95ds      e ds
005100051207     d Tnta12ds      e ds
005110100127     d TNTA40DS      e ds
005120100604     d TNTA43DS      e ds
005130051124     d Tnta60ds      e ds
005140060731     d Tnta61ds      e ds
005150051213     d Tnta81ds      e ds
005160100406     d TRMK01DS      e ds
005170110223     d Trmk05ds      e ds
005180100406     d TRMK17DS      e ds
005190100406     d TRMK20ds      e ds
005200100507     d TRMK21DS      e ds
005210130808     d TRMK43ds      e ds                  inz
005220081020     d Trmk50ds      e ds
005230100407     d TRMK28DS      e ds
005240051108     d Trul31ds      e ds
005250051118     d Trul42ds      e ds
005260101213     d Trul57ds      e ds
005270100730     d TrulIbanI0    e ds                  qualified
005280100728     d TrulIbanI1    e ds                  qualified
005290100728     d TrulIbanO0    e ds                  qualified
005300100728     d TrulIbanO1    e ds                  qualified
005310090616     d trulsoli1     e ds                  qualified
005320090616     d trulsolo1     e ds                  qualified
005330060529     d TNTA73DS      e ds
005340061012     d Tisie5ds      e ds
005350051128     d Xdecds        e ds                  inz
005360051128     d   ocdesi      e                     inz(*Off)
005370151014     d xcfiva1ds     e ds
005380061106     d  ivaeu                  3      4
005390061106     d  ivait                  5     18
005400100805     d TNTBE00F      e ds                  extname(TNTBE00F)
005410160803
005420160803       // -?Storicizzazione variazioni
005430160803     d TRMK30DS      e ds
005440160803     d TNCPO_30      e ds                  extname(TNCPO00F) inz
005450160803     d TNCPO1_30     e ds                  extname(TNCPO10F) inz
005460160803     d TICPI_30      e ds                  extname(TICPI00F) inz
005470051107
005480051107      *   C O S T A N T I
005490060203      * titolo videata (lunghezza massima 35)
005500060203     d VtcTit          c                   const('** GESTIONE ANAGRAFICHE CLIEN-
005510060203     d                                     TI ** ')
005520091027     d VtcTitv         c                   const('****  Anagrafica Provvisoria -
005530091027     d                                      **** ')
005540051122     d VtcTitc         c                   const('**      CONVALIDA OFFERTE    -
005550060203     d                                        ** ')
005560051122     d cf2413          c                   const('F24=AlTasti(1/3)')
005570051122     d cf2423          c                   const('F24=AlTasti(2/3)')
005580051122     d cf2433          c                   const('F24=AlTasti(3/3)')
005590051122     d cf2412          c                   const('F24=AlTasti(1/2)')
005600051122     d cf2422          c                   const('F24=AlTasti(2/2)')
005610100127
005620100127     d Digits          c                   const('0123456789')
005630100406
005640100406      //---------------------------------------------------------------
005650100406      //?Definizione procedure esterne.
005660100406      //---------------------------------------------------------------
005670100604      // - Controllo se trattativa aperta
005680100604     d Tnta43R         pr                  extpgm('TNTA43R')
005690100604     d  tnta43ds                           likeds(tnta43ds)
005700110223
005710110223      // - Calcolo categoria potenziale
005720110223     d trmk05r         pr                  extpgm('TRMK05R')
005730110223     d  kpjba                              likeds(KPJBA)
005740110223     d  trmk05ds                           likeds(TRMK05ds)
005750100604
005760100406      // - Inserimento attività
005770100406     d trmk17r         pr                  extpgm('TRMK17R')
005780100406     d  kpjba                              likeds(KPJBA)
005790100406     d  trmk17ds                           likeds(TRMK17ds)
005800100507
005810100507      // - Interrogazione attività
005820100507     d trmk21r         pr                  extpgm('TRMK21R')
005830100507     d  kpjba                              likeds(KPJBA)
005840100407
005850100407      // - Modifica potenziale
005860100407     d trmk28r         pr                  extpgm('TRMK28R')
005870100407     d  trmk28ds                           likeds(TRMK28DS)
005880120510
005890120510      // - Pgm gestione INFO commerciali
005900120510     d trmk50r         pr                  extpgm('TRMK50R')
005910120510     d  kpjba                              likeds(KPJBA)
005920120510     d  trmk50ds                           likeds(TRMK50ds)
005930100728
005940100728      // - Coordinate bancarie
005950100728     d TrulIbanR       pr                  extpgm('TRULIBANR')
005960100729     d  rqsOpCode                    10a   const
005970100729     d  rpyEsito                     10i 0
005980100729     d  rqsFormatName                10a   const
005990100729     d  rqsData                     256a   options(*varsize)
006000100729     d  rqsDataSize                  10i 0 const
006010100729     d  rpyFormatName                10a   const
006020100729     d  rpyData                     256a   options(*varsize)
006030100729     d  rpyDataSize                  10i 0 const
006040100729
006050100729      // - Interrogazione tabella
006060100729     d Trul19R         pr                  extpgm('TRUL19R')
006070100729     d  kcod                          2a
006080100729     d  ordina                        1a
006090100729     d  kkey                          8a
006100100729     d  comando                       1a
006110101213
006120101213      // - Dati Fatturazione
006130101213     d trul57r         pr                  extpgm('TRUL57R')
006140101213     d  kpjba                              likeds(KPJBA)
006150101213     d  trul57ds                           likeds(TRUL57ds)
006160160803
006170160803       // -?Storicizzazione Variazioni Potenziale
006180160803     d TRMK30R         pr                  extpgm('TRMK30R')
006190160803     d  trmk30ds                           likeds(trmk30ds)
006200160803     d  tncpo_30                           likeds(tncpo_30)
006210160803     d  tncpo1_30                          likeds(tncpo1_30)
006220160803     d  ticpi_30                           likeds(ticpi_30)
006230100406
006240100406      //---------------------------------------------------------------
006250100406      //?prototipi
006260100406      //---------------------------------------------------------------
006270100729      /copy gaitrasrc/srcprotopr,tibs02r
006280150427      /copy gaitrasrc/srcprotopr,tibs73r
006290161129      /copy gaitrasrc/srcprotopr,tibs69r
006300100406      /copy gaitrasrc/srcprotopr,trmk01r
006310100406      /copy gaitrasrc/srcprotopr,trmk02r
006320100406      /copy gaitrasrc/srcprotopr,trmk20r
006330140722      /copy gaitrasrc/srcProtoPR,TNTA12R
006340051107
006350051107      *------------------------------------------------------------------------*
006360090417
006370090417     c/exec sql
006380090417     c+ set option dynusrprf = *owner, closqlcsr = *endmod
006390090417     c/end-exec
006400051107
006410051117      * Richiamato da Gestione visite/offerte o da Comferma offerte
006420051117      * salto la videata di richiesta del codice
006430091104      * anche nel caso di richiamo per sola interrogazione (nuovo campo DS)
006440091124      * oppure manutenzione di un codice specifico (cliente-CNACO)
006450091124     c                   If        (*In06 or *In07
006460091124     c                              or $interroga or $modifica)
006470091124     c                              and not *in28
006480051118     c                   Goto      vid02
006490051115     c                   EndIf
006500091124
006510091124     c   28              goto      fine
006520051124
006530051124     * §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ * ?
006540051124     ?*  DATI DI RICERCA    ?
006550051124     ?*  Prima videata      ?
006560051124
006570060110     c                   ExSr      Sr_Pulvid
006580051118
006590051118     c     emi01         Tag
006600051107
006610051107     c                   Write     Ta60t01
006620051107     c                   Exfmt     Ta60d01
006630051107     c                   Eval      *In28 = *Off
006640051107     c                   Eval      *In90 = *Off
006650051107     c                   Clear                   v1cmsg
006660051219     c                   Eval      $d01 = *On
006670051107
006680051124     ?* F3=Fine  ?
006690051107     c                   If        *InKc or
006700051107      * 08=utente non abilitato
006710051107     c                             *In08
006720051107     c                   Goto      Fine
006730051107     c                   EndIf
006740051107
006750051124     ?* Controllo  ?
006760051107     c                   ExSr      Sr_Ctrd01
006770051118     c   28              Goto      emi01
006780051124
006790051130     c     vid02         Tag
006800060221     c                   Eval      wokpot = *Off
006810051130
006820051130      * Carico i dati nelle varie videate
006830051216     c                   ExSr      Sr_Cardati
006840091112     c   70
006850091112     corn06              ExSr      Sr_Carluoghi
006860051216     c                   ExSr      Sr_Carnote
006870051130
006880051130     * §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ * ?
006890051130     ?*  DATI ANAGRAFICI/BLOCCO   ?
006900051130     ?*  Seconda videata      ?
006910051107
006920051207      * Imposto il campo unico in fondo alla videata con i tasti funzione
006930051221     c                   Eval      *In29 = *On
006940051207     c                   ExSr      Sr_Tastifun
006950051207
006960051118     c     emi02         Tag
006970051129
006980051107     c                   Write     Ta60t01
006990051122     c                   Write     Ta60z01
007000051107     c                   Exfmt     Ta60d02
007010051107     c                   Eval      *In28 = *Off
007020051124     c                   Eval      *In90 = *Off
007030051108     c                   Clear                   v2cmsg
007040051213     c                   Eval      wvideo = '02'
007050051107
007060051124     ?* F3=Fine  ?
007070051107     c                   If        *InKc
007080060217     c                   ExSr      Sr_F03
007090060217     c   80              Goto      emi02
007100051107     c                   EndIf
007110051114
007120051124     ?* F12=Ritorno  ?
007130051118     c                   If        *Inkl
007140060110     c                   ExSr      Sr_Pulvid
007150091106      * se richiamato in interrogazione deve uscire dal pgm
007160091124     c                   if        $interroga or $modifica
007170091106     c                   ExSr      Sr_F03
007180091106     c                   else
007190100208      * se non è interrogazione e non è anagrafica provvisoria
007200100208      * con F12 devo sbloccare i file
007210100208     c                   if        not *in05 and not *in06
007220100208     c                   unlock    cnaco00f
007230100208     c                   unlock    cnind00f
007240100208     c                   unlock    cnclp00f
007250100208     c                   unlock    fncls01l
007260100208     c                   endif
007270051118     c                   Goto      emi01
007280091106     c                   endif
007290051118     c                   EndIf
007300090717
007310090717     ?* F2=Rubrica  ?
007320090717     c                   If        *Inkb
007330090717     c                   Eval      wrag = v2crag
007340090717     c  n05              Eval      wrich = 'M'
007350090717     c   05              Eval      wrich = 'V'
007360090717     c                   clear                   wtnt
007370090717     c                   ExSr      Sr_F02
007380090717     c                   Goto      emi02
007390090717     c                   EndIf
007400060731
007410060731     ?* F4=Abilitazioni  ?
007420060731     c                   if        *inkd
007430060731     c                   exsr      sr_f04
007440060731     c                   goto      emi02
007450060731     c                   endif
007460100507
007470100507       //?F5=Attività
007480100507     c                   IF        *inke
007490100507     c                   exsr      sr_F05
007500100507     c                   goto      emi02
007510100507     c                   ENDIF
007520051128
007530051128     ?* F6=Conferma  ?
007540051128     c                   If        *InKf
007550051128     c                   ExSr      Sr_Conferma
007560180119     c                   IF        ClienteSofferenza
007570180119     c                   goto      emi02
007580180119     c                   ENDIF
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
008840171213     c                   IF        ClienteSofferenza
008850171213     c                   Goto      emi02
008860171213     c                   ENDIF
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
019050171005
019060171006       IF  savin05 = *off;
019070171006         *in05 = *off;
019080171006         *in04 = *off;
019090171006         clear VTCmod;
019100171006       ENDIF;
019110051108
019120060110      * Prima videata
019130051108     c                   Eval      v1ckcc = dutkci
019140051108     c                   Clear                   v1cksc
019150051108     c                   Clear                   v1crag
019160051108     c                   Clear                   skfil
019170051108     c                   Clear                   v1ccdf
019180051108     c                   Clear                   v1civa
019190051108
019200051108      * Imposto la sk dei p.o. di ricerca
019210051108      * se autorizzazzione diversa da distretto e se sono in filiale
019220051115     c                   If        wabi <> 'DI' and Not *In09
019230051108     c                   Clear                   yy
019240051108     c                   Do        24            xx
019250170914     c*//                If        skpog(xx) <> *Blanks and skpog(xx) > *Zeros
019260170914     c                   If        skPog_RA(xx) <> *Blanks and
019270170914     c                             skPog_RA(xx)  > *Zeros
019280051108     c                   Add       1             yy
019290170914     c*//                Move      skpog(xx)     skfil(yy)
019300170914     c                   Move      skPog_RA(xx)  skFil(yy)
019310051108     c                   EndIf
019320051108     c                   EndDo
019330051108     c                   EndIf
019340060110
019350060110      * Seconda videata
019360060110     c                   Clear                   v2cduv
019370060110     c                   Clear                   v2ddtr
019380060110     c                   Clear                   v2cdtr
019390060110     c                   Clear                   v2copz
019400060110     c                   Clear                   v2ctic
019410060110     c                   Clear                   v2clin
019420060110     c                   Clear                   v2crag
019430060110     c                   Clear                   v2cvia
019440060110     c                   Clear                   v2ccit
019450060110     c                   Clear                   v2ccae
019460060110     c                   Clear                   v2cprv
019470060110     c                   Clear                   v2csta
019480060110     c                   Clear                   v2ctel
019490060110     c                   Clear                   v2ctlf
019500060110     c                   Clear                   v2ccdf
019510060110     c                   Clear                   v2civa
019520060110     c                   Clear                   v2cabl
019530060110     c                   Clear                   v2cblc
019540060110     c                   Clear                   v2ccon
019550060110     c                   Clear                   v2tb02
019560060110     c                   Clear                   v2ccpo
019570090616     c                   Clear                   v2csol
019580090616     c                   Clear                   v2fsol
019590091119     c                   clear                   v2ntcdc
019600091119     c                   clear                   savntcdc
019610171213     c                   eval      ClienteSofferenza = *off
019620060110      * Terza videata
019630060110     c                   Clear                   v3cage
019640060110     c                   Clear                   v3cclv
019650060110     c                   Clear                   v3citc
019660121119     c                   clear                   v3cnew
019670060110      * Quarta videata
019680060110     c                   Clear                   v4cscf
019690060110     c                   Clear                   savscf
019700060110     c                   Clear                   v4cuni
019710060110     c                   Clear                   savuni
019720060110     c                   Clear                   v4ctft
019730060110     c                   Clear                   savtft
019740060110     c                   Clear                   v4cfft
019750060110     c                   Clear                   savfft
019760060110     c                   Clear                   v4ctdf
019770060110     c                   Clear                   v4csin
019780060110     c                   Clear                   v4cdps
019790060110     c                   Clear                   v4cdus
019800060110     c                   Clear                   v4cnpn
019810060110     c                   Clear                   v4cnpr
019820060110     c                   Clear                   v4cdpr
019830100727     c                   Clear                   v4ccdp
019840100727     c                   Clear                   v4cbna
019850100727     c                   Clear                   v4cabi
019860100727     c                   Clear                   v4ccab
019870121105     c                   clear                   V4Ctpf
019880060110      * Quinta videata
019890060110     c                   Clear                   wbloc
019900100728     c                   Clear                   v5cblp
019910060110     c                   Clear                   v5cfpc
019920080115     c                   clear                   v5ciban
019930060110     c                   Clear                   v5cbab
019940100729     c                   clear                   v5tpdn
019950100728     c                   clear                   v5ibandn
019960100728     c                   clear                   v5babdn
019970140203     c                   clear                   v5bicdn
019980100729     c                   clear                   v5tpna
019990100728     c                   clear                   v5ibanna
020000100728     c                   clear                   v5babna
020010140203     c                   clear                   v5bicna
020020150424     c                   eval      BonificoCA = *off
020030150424     c                   eval      BonificoDN = *off
020040150424     c                   eval      BonificoNA = *off
020050150427     c                   eval      PagEsteroCA = *off
020060150427     c                   eval      PagEsteroDN = *off
020070150427     c                   eval      PagEsteroNA = *off
020080150427     c                   clear                   CodPagCA
020090150427     c                   clear                   CodPagDN
020100150427     c                   clear                   CodPagNA
020110060110      * Sesta videata
020120060110     c                   Clear                   v6ctcm
020130060110     c                   Clear                   v6ctcf
020140060110     c                   Clear                   v6capg
020150060110     c                   Clear                   v6cdmg
020160060801     c                   Clear                   v6ccag
020170060110      * Settima videata
020180060110     c                   Clear                   v7csdn
020190060110     c                   Clear                   v7csin
020200060110     c                   Clear                   v7ctpa
020210060110     c                   Clear                   v7cstp
020220060317
020230060317      * Pulisco la ds dello stato del credito per non sporcarmi i dati
020240060317      * del blocco tra un cliente e l'altro
020250060317     c                   Clear                   ds4w
020260100805
020270100805     c                   clear                   wbic
020280100805     c                   clear                   wcodpn
020290100805     c                   clear                   wcodpo
020300100805     c                   clear                   wiban
020310100805     c                   clear                   wpgm
020320060110
020330051108     c                   EndSr
020340051107
020350051107      *------------------------------------------------------------------------*
020360051107      * CONTROLLO PRIMA VIDEATA - DATI DI RICERCA
020370051107      *------------------------------------------------------------------------*
020380051107     c     Sr_Ctrd01     BegSr
020390051117
020400051117      * Pulisco i campi del posizionamento cursore
020410051117     c                   Clear                   h1riga
020420051117     c                   Clear                   h1colo
020430051117      * Spengo indicatori di posizionamento cursore
020440051117     c                   Setoff                                       414243
020450051117     c                   Setoff                                       444546
020460051117     c                   Setoff                                       474849
020470051117     c                   Setoff                                       505152
020480051117     c                   Setoff                                       535455
020490051117     c                   Setoff                                       565758
020500051117     c                   Setoff                                       596061
020510051117     c                   Setoff                                       626364
020520060110      * Spengo indicatori di comodo
020530090914     c                   Setoff                                       0102
020540060110     c                   Setoff                                       04
020550060711
020560060711     c                   eval      wforzaksc = *off
020570051107
020580051107      * Almeno un dato deve essere inserito
020590051107     c                   If        v1ckcc = *Zeros and v1cksc = *Zeros and
020600051108     c                             v1crag = *Blanks and
020610051107     c                             v1ccdf = *Blanks and v1civa = *Blanks
020620051111     c                   Eval      *In28 = *On
020630060109     c                   Eval      h1riga = 06
020640051118     c                   Eval      h1colo = 04
020650051111     c                   Eval      v1cmsg = msg(02)
020660051107     c                   Leavesr
020670051107     c                   EndIf
020680051107
020690051107      * CAPOCONTO
020700051107     c                   If        v1ckcc = *zeros
020710051107     c                   Eval      *In28 = *On
020720060109     c                   Eval      h1riga = 06
020730051118     c                   Eval      h1colo = 04
020740051107     c                   Eval      v1cmsg = msg(03)
020750051107     c                   Leavesr
020760051107     c                   EndIf
020770051107
020780051219      * Se sono in filiale posso solo gestire i capoconti presenti in £4
020790051219      * per ora c'è solo il capoconto clienti (151)
020800051219if  1c                   If        Not *In09
020810051107     c                   Move      v1ckcc        w004a
020820051107     c     w004a         Lookup    £4                                     30
020830051107     c                   If        Not *In30
020840051107     c                   Eval      *In28 = *On
020850060109     c                   Eval      h1riga = 06
020860051118     c                   Eval      h1colo = 04
020870051107     c                   Eval      v1cmsg = msg(04)
020880051107     c                   Leavesr
020890051107     c                   EndIf
020900051219    1c                   EndIf
020910051219
020920051219      * CODICE CLIENTE NON IMMESSO
020930051219if  1c                   If        v1cksc = *Zeros
020940060207      * se non c'è una ricerca per ragione sociale errore
020950060227     c                   If        v1crag = *Blanks and
020960060227     c                             v1ccdf = *Blanks and v1civa = *Blanks
020970060207     c                   Eval      *In28 = *On
020980060207     c                   Eval      h1riga = 06
020990060207     c                   Eval      h1colo = 12
021000060207     c                   Eval      v1cmsg = msg(02)
021010060207     c                   Leavesr
021020060207     c                   EndIf
021030051219
021040051219      * Controllo la sk dei p.o. di ricerca
021050051219     c                   ExSr      Sr_Ctrfil
021060051219
021070051219s   2c                   Select
021080051219      * Ricerca Alfabetica
021090051219     c                   When      v1crag <> *Blanks
021100051219     c                   Movel     rsut          pardut
021110051219     c                   Movea     skfil         parfil
021120051219     c                   Movel     v1ckcc        parkcc
021130051219     c                   Clear                   pardit
021140051219     c                   Z-add     1             paxnum
021150051219      * 2 = richiesta di visualizzazione del cliente unificante
021160051219     c                   Z-add     2             kkut
021170051219     c                   Call      'XALFA3BR'
021180051219     c                   Parm                    pardut
021190051219     c                   Parm                    kkut
021200051219     c                   Parm                    v1crag
021210051219     c                   Parm                    parkcc
021220051219     c                   Parm                    parsta
021230051219     c                   Parm                    parfil
021240051219     c                   Parm                    pardit
021250051219     c                   Parm                    paxnum
021260051219     c                   Parm                    paxkcm
021270051219     c                   Parm                    paxksm
021280051219     c                   Parm                    paxkdm
021290051219     c                   If        parsta = -1
021300051219     c                   Goto      emi01
021310051219     c                   EndIf
021320051219     c                   Movel     paxksm        v1cksc
021330051219     c                   If        parkcc = *Zeros
021340051219     c                   Movel     paxkcm        v1ckcc
021350051219     c                   EndIf
021360051219      * Ricerca per Codice Fiscale
021370051219     c                   When      v1ccdf <> *Blanks
021380051219     c                   Movel     rsut          pardut
021390051219     c                   Movea     skfil         parfil
021400051219     c                   Movel     v1ckcc        parkcc
021410051219     c                   Clear                   pardit
021420051219     c                   Z-add     1             paxnum
021430051219      * 2 = richiesta di visualizzazione del cliente unificante
021440051219     c                   Z-add     2             kkut
021450051219     c                   Clear                   pariva
021460051219     c                   Movel     v1ccdf        parcdf
021470051219     c                   Call      'XALFA3BR'
021480051219     c                   Parm                    pardut
021490051219     c                   Parm                    kkut
021500051219     c                   Parm                    v1crag
021510051219     c                   Parm                    parkcc
021520051219     c                   Parm                    parsta
021530051219     c                   Parm                    parfil
021540051219     c                   Parm                    pardit
021550051219     c                   Parm                    paxnum
021560051219     c                   Parm                    paxkcm
021570051219     c                   Parm                    paxksm
021580051219     c                   Parm                    paxkdm
021590051219     c                   Parm                    paresci
021600051219     c                   Parm                    parerr
021610051219     c                   Parm                    pariva
021620051219     c                   Parm                    parcdf
021630051219     c                   If        parsta = -1
021640051219     c                   Goto      emi01
021650051219     c                   EndIf
021660051219     c                   Movel     paxksm        v1cksc
021670051219     c                   Clear                   v1ccdf
021680051219     c                   If        parkcc = *Zeros
021690051219     c                   Movel     paxkcm        v1ckcc
021700051219     c                   EndIf
021710051219      * Ricerca per Partita IVA
021720051219     c                   When      v1civa <> *Blanks
021730051219     c                   Movel     rsut          pardut
021740051219     c                   Movea     skfil         parfil
021750051219     c                   Movel     v1ckcc        parkcc
021760051219     c                   Clear                   pardit
021770051219     c                   Z-add     1             paxnum
021780051219      * 2 = richiesta di visualizzazione del cliente unificante
021790051219     c                   Z-add     2             kkut
021800051219      * Se non ho impostato il codice nazione vado in ricerca con la sola
021810060109      * partita Iva altrimenti P.IVa + Nazione
021820051219     c                   If        v1ivaeu = *Blanks
021830051219     c                   Movel     v1ivait       pariva
021840051219     c                   Else
021850051219     c                   Movel     v1civa        pariva
021860051219     c                   EndIf
021870051219     c                   Clear                   parcdf
021880051219     c                   Call      'XALFA3BR'
021890051219     c                   Parm                    pardut
021900051219     c                   Parm                    kkut
021910051219     c                   Parm                    v1crag
021920051219     c                   Parm                    parkcc
021930051219     c                   Parm                    parsta
021940051219     c                   Parm                    parfil
021950051219     c                   Parm                    pardit
021960051219     c                   Parm                    paxnum
021970051219     c                   Parm                    paxkcm
021980051219     c                   Parm                    paxksm
021990051219     c                   Parm                    paxkdm
022000051219     c                   Parm                    paresci
022010051219     c                   Parm                    parerr
022020051219     c                   Parm                    pariva
022030051219     c                   Parm                    parcdf
022040051219     c                   If        parsta = -1
022050051219     c                   Goto      emi01
022060051219     c                   EndIf
022070051219     c                   Movel     paxksm        v1cksc
022080051219     c                   Clear                   v1civa
022090051219     c                   If        parkcc = *Zeros
022100051219     c                   Movel     paxkcm        v1ckcc
022110051219     c                   EndIf
022120051219    2c                   EndSl
022130051219    1c                   EndIf
022140051107
022150051107      * CODICE CLIENTE IMMESSO
022160051108if  1c                   If        v1cksc <> *Zeros
022170060711      * per prima cosa cerco il commerciale unificante del
022180060711      * cliente immesso
022190060711      * aggancio cnlcp effettivo xchè sono in prima videata quindi sono
022200060711      * per forza nell'anagrafica effettiva e non nella provvisoria
022210060711     c                   Eval      kksc = v1cksc
022220060711     c     kcnaco        chain(n)  cnclp00f
022230060711    2c                   if        %found(cnclp00f)
022240130806     c     CLPage        chain     AZCMM000
022250130806    3c                   if        Not  %found(AZCMM01L)
022260130806     c                   clear                   CMMuni
022270060711    3c                   endif
022280130806     c                   movel     CMMuni        wpoageuni
022290060711    2c                   endif
022300160905      * Cerco network del cliente
022310160905     c                   Clear                   og143
022320160905     c                   Movel     v1cksc        w0030
022330160905     c     w0030         Chain     Azorg01l
022340160905     c                   If        %Found(Azorg01l)
022350160905     c                   Eval      og143 = orgde3
022360160905     c                   EndIf
022370160905     c                   eval      ClienteLog = (§OGntw = 'LOG')
022380051117      * Controllo se cliente gestito dall'utente
022390051219      * se non sono in interrogazione
022400051219if  2c                   If        Not *In05
022410161005     c                   IF        §UTEksclog <> 'S' and
022420161005     c                             ClienteLog
022430161005     c                   Eval      *In28 = *On
022440161005     c                   Eval      h1riga = 06
022450161005     c                   Eval      h1colo = 12
022460161005     c                   Eval      v1cmsg = msg(10)
022470161005     c                   Leavesr
022480161005     c                   ENDIF
022490051117     c                   Movel     v1cksc        w003a
022500051117     c     w003a         Lookup    skpog                                  30
022510060711     c                   If        Not *In30
022520060711    4c                   if        wpoageuni <> *zeros
022530060711     c     wpoageuni     lookup    skpog                                  30
022540060711    5c                   if        not *In30
022550060711     c                   eval      *In28 = *On
022560060711     c                   eval      h1riga = 06
022570060711     c                   eval      h1colo = 12
022580060711     c                   eval      v1cmsg = msg(10)
022590060711     c                   leavesr
022600060711    5c                   endif
022610060711     c                   eval      wforzaksc = *on
022620060711    4c                   endif
022630060711    4c                   if        wforzaksc = *off
022640051117     c                   Eval      *In28 = *On
022650060109     c                   Eval      h1riga = 06
022660051117     c                   Eval      h1colo = 12
022670051117     c                   Eval      v1cmsg = msg(10)
022680051117     c                   Leavesr
022690060711    4c                   endif
022700051117     c                   EndIf
022710051219   x2c                   Else
022720051219      * Se sono in interrogazione e non sono utente di sede non posso
022730051219      * interrogare i clienti logistica o XXX
022740170907      *?=> Interrogare i clienti Logistica è ora possibile?
022750170907      *   ?(come lo era già per i clienti di altre filiali NON?
022760170907      *   ?logistica e NON di competenza dell'utente)?
022770051219if  3c                   If        Not *In09
022780051219     c                   Move      v1ckcc        w004a
022790051219     c     w004a         Lookup    £4                                     30
022800051219if  4c                   If        *In30
022810170907     c*//*               If        §ogntw = 'LOG' or §ogntw = 'XXX'
022820170907     c                   If        §OGntw = 'XXX'
022830051219     c                   Eval      *In28 = *On
022840060109     c                   Eval      h1riga = 06
022850051219     c                   Eval      h1colo = 12
022860051219     c                   Eval      v1cmsg = msg(10)
022870051219     c                   Leavesr
022880051219     c                   EndIf
022890051219    4c                   EndIf
022900051219    3c                   EndIf
022910051219    2c                   EndIf
022920051219      * Aggancio l'anagrafica
022930051219      * solo cnaco xchè se siamo in prima videata è x forza anagrafica
022940051219      * effettiva, la provvisoria è solo se richiamato
022950051222     c                   Eval      kksc = v1cksc
022960060111     c  n05kCnaco        Chain(e)  Cnaco00f
022970060111     c   05kCnaco        Chain(n)  Cnaco00f
022980051219      * Allocato errore
022990060220if  2c                   If        %Error and Not *In05
023000051219     c                   Eval      *In28 = *on
023010060109     c                   Eval      h1riga = 06
023020051219     c                   Eval      h1colo = 12
023030051219     c                   Eval      v1cmsg = msg(68)
023040051219     c                   Leavesr
023050051219    2c                   EndIf
023060051108if  2c                   If        %Found(Cnaco00f)
023070051213      * Cliente esistente - Manutenzione/Visualizzazione
023080051115     c                   If        acoflg <> '*'
023090051115     c  n05              Eval      *In02 = *On
023100051115     c  n05              Eval      vtcmod = mod(02)
023110051115     c   05              Eval      vtcmod = mod(03)
023120051107      * Cliente annullato
023130051107     c                   Else
023140051107     c                   Eval      *In04 = *On
023150051213     c                   Eval      *In05 = *On
023160051108     c                   Eval      vtcmod = mod(04)
023170051107     c                   EndIf
023180051107      * Cliente nuovo - immissione
023190051108   x2c                   Else
023200051116      * --> Se gestione clienti inserisco
023210051115if  3c                   If        Not *In05
023220051107     c                   Eval      *In01 = *On
023230051108     c                   Eval      vtcmod = mod(01)
023240051116      * --> Se interrogazione clienti errore
023250051115   x3c                   Else
023260051115     c                   Eval      *In28 = *On
023270060109     c                   Eval      h1riga = 06
023280051117     c                   Eval      h1colo = 12
023290051115     c                   Eval      v1cmsg = msg(05)
023300051115     c                   Leavesr
023310051115    3c                   EndIf
023320051108    2c                   EndIf
023330051219    1c                   EndIf
023340051116
023350051213      * Se non sono in interrogazione
023360051213     c                   If        Not *In05
023370051116      * PASSWORD
023380051116     c                   If        v1cpwd = *Blanks
023390051116     c                   Eval      *In28 = *On
023400051117     c                   Eval      h1riga = 21
023410051117     c                   Eval      h1colo = 29
023420060109     c                   Eval      v1cmsg = msg(07)
023430051116     c                   Leavesr
023440051116     c                   EndIf
023450051116      * Controllo se esatta
023460051116     c                   If        v1cpwd <> §mtcpwd
023470051116     c                   Eval      *In28 = *On
023480051117     c                   Eval      h1riga = 21
023490051117     c                   Eval      h1colo = 29
023500060109     c                   Eval      v1cmsg = msg(06)
023510051116     c                   Leavesr
023520051116     c                   EndIf
023530051116      * Controllo la validità se non sono in sede
023540051213if  2c                   If        Not *In09
023550051116      * Data immissione/variazione password
023560051116     c                   Clear                   wlbdat
023570051116     c                   Z-Add     §mtcdta       g02dat
023580051116     c                   Call      'XSRDA8'
023590051116     c                   Parm                    wlbdat
023600051116     c                   Movel     g02inv        dataiso
023610051116      * Scadenza password
023620051116     c                   Clear                   Tibs02ds
023630051116     c                   Eval      t02mod = 'C'
023640051116     c                   Eval      t02sif = knsif
023650051116     c                   Eval      t02cod = 'MTC'
023660051125     c                   Eval      t02ke1 = 'PSW'
023670051116     c                   Call      'TIBS02R'
023680051116     c                   Parm                    kpjba
023690051116     c                   Parm                    Tibs02ds
023700051213if  3c                   If        t02err <> *Blanks
023710051116     c                   Eval      *In28 = *On
023720051117     c                   Eval      h1riga = 21
023730051117     c                   Eval      h1colo = 29
023740051116     c                   Eval      v1cmsg = msg(09)
023750051116     c                   Leavesr
023760051213   x3c                   Else
023770051116     c                   Movel     t02uni        wgiorni
023780051116     c                   Adddur    wgiorni:*d    dataiso
023790051116     c                   Move      dataiso       datascad
023800051213if  4c                   If        dataoggi > dataScad
023810051116     c                   Eval      *In28 = *On
023820051117     c                   Eval      h1riga = 21
023830051117     c                   Eval      h1colo = 29
023840051116     c                   Eval      v1cmsg = msg(08)
023850051116     c                   Leavesr
023860051213    4c                   EndIf
023870051213    3c                   EndIf
023880051213    2c                   EndIf
023890051213    1c                   EndIf
023900051107
023910051107     c                   EndSr
023920051108
023930051108      *------------------------------------------------------------------------*
023940051108      * CONTROLLO SK DEI P.O. DI RICERCA
023950051108      *------------------------------------------------------------------------*
023960051108     c     Sr_Ctrfil     BegSr
023970051108
023980051108do  1c                   Do        24            xx
023990051108if  2c                   If        skfil(xx) <> *Blanks
024000051108     c     '?'           Scan      skfil(xx)                              30
024010051108if  3c                   If        *In30
024020051108     c                   Eval      §tip = 'E'
024030051108     c                   call      'TNSD23R'
024040051108     c                   Parm                    §tip
024050051108     c                   Parm                    §fil
024060051108
024070051108      * Imposto i p.o. selezionati
024080121130if  4c                   If        §fil <> *Blanks and §fil > *Zeros
024090051108     c                   Movea     §fil          skpo
024100051108     c                   Eval      yy = 1
024110051108if  5c                   Dow       skpo(yy) >*Zeros and xx <= 24
024120051108     c     '?'           Scan      skfil(xx)                              30
024130051108if  6c                   If        *In30 or skfil(xx) <= *Zeros
024140051108     c                   Movel     skpo(yy)      skfil(xx)
024150051108     c                   Add       1             yy
024160051108    6c                   EndIf
024170051108     c                   Add       1             xx
024180051108    5c                   EndDo
024190051108   x4c                   Else
024200051108     c                   Clear                   skfil(xx)
024210051108    4c                   EndIf
024220051108     c                   Leavesr
024230051108    3c                   EndIf
024240051108
024250051108      * Controllo i p.o. di ricerca immessi
024260121130if  3c                   If        skfil(xx) <> *Blanks and skfil(xx) > *Zeros
024270051108     c                   Move      skfil(xx)     w0030
024280160905     c                   clear                   og143
024290051108     c     w0030         Chain     Azorg01l
024300160905if  4c                   If        Not %Found(Azorg01l)
024310160905     c******                       (orgfag <> 'A' and orgfag <> 'F')
024320051108     c     xx            Add       40            zz
024330051108     c                   Eval      *In(zz) = *On
024340160906     c                   eval      *in28 = *on
024350160914     c                   eval      V1Cmsg = msg(91)
024360051118     c                   Goto      emi01
024370051108    4c                   EndIf
024380160905     c                   eval      OG143 = ORGde3
024390170922     c                   IF        §OGntw = 'XXX'
024400160905     c     xx            Add       40            zz
024410160905     c                   Eval      *In(zz) = *On
024420160905     c                   eval      *in28 = *on
024430160914     c                   eval      V1Cmsg = msg(91)
024440160905     c                   Goto      emi01
024450160905     c                   ENDIF
024460051108    3c                   EndIf
024470051108    2c                   EndIf
024480051108    1c                   EndDo
024490051108
024500051108     c                   EndSr
024510051216
024520051216      *------------------------------------------------------------------------*
024530051216      * CARICAMENTO DATI DAI FILE ALLE VIDEATE
024540051216      *------------------------------------------------------------------------*
024550051216     c     Sr_Cardati    BegSr
024560051216
024570051220     c                   Clear                   savabl
024580051220     c                   Clear                   savcpo
024590051220     c                   Clear                   savfft
024600051220     c                   Clear                   savitc
024610051220     c                   Clear                   savscf
024620051220     c                   Clear                   savtft
024630051220     c                   Clear                   savuni
024640080422     c                   clear                   sav4utip
024650100802     c                   clear                   savtipdn
024660100802     c                   clear                   savtipna
024670051216     c                   Clear                   wfscf
024680060105     c                   Eval      wforzacon = *Off
024690060215     c                   Clear                   savin22
024700060215     c                   Clear                   savin23
024710100728     c                   Clear                   savin71
024720100728     c                   Clear                   savin72
024730100728     c                   Clear                   savin73
024740100728     c                   Clear                   savin74
024750091119     c                   clear                   savntcdc
024760061030     c                   eval      wforzacdf = *off
024770061106     c                   eval      wforzaiva = *off
024780061106     c                   eval      *in39 = *off
024790080422     c                   eval      $win = *off
024800100729     c                   eval      $windn = *off
024810100729     c                   eval      $winna = *off
024820100729     c                   clear                   wbanca
024830100729     c                   clear                   wagenzia
024840100729     c                   clear                   savabidn
024850100729     c                   clear                   savabina
024860100729     c                   clear                   savcabdn
024870100729     c                   clear                   savcabna
024880100730     c                   clear                   savibandn
024890100730     c                   clear                   savibanna
024900100803     c                   eval      *in75 = *off
024910100803     c                   eval      *in76 = *off
024920100803     c                   eval      *in77 = *off
024930130322     c                   eval      *in79 = *off
024940130322     c                   eval      *in81 = *off
024950150415     c                   eval      *in82 = *off
024960170317     c                   eval      *in83 = *off
024970170320     c                   eval      *in84 = *off
024980170421     c                   eval      *in85 = *off
024990150424     c                   eval      wForzaPag = *off
025000150728     c                   eval      wForzaMail = *off
025010170516       wForzaPotenziale = *off;
025020110217
025030110217      /free
025040110217       //?pulisco campi di comodo per controlli da fare sul commerciale
025050110217       //?quando NON è anagrafica provvisoria
025060110217        clear sav_livello;
025070110217        clear wCodIncAtt;
025080110217      /end-free
025090051216
025100051219      * Se non sono passata dalla prima videata
025110051219      * aggancio l'anagrafica
025120051219if  1c                   If        $d01 = *Off
025130060110      * Spengo indicatori di comodo
025140090914     c                   Setoff                                       0102
025150060110     c                   Setoff                                       04
025160051222     c                   eval      kksc = v1cksc
025170051219      * Se non è richiamato da gestione visite e non è interrogazione
025180051219      * aggancio Cnaco00f per allocare il rcd
025190051219if  2c                   If        Not *In06 and Not *In05
025200060111     c  n05kCnaco        Chain(e)  Cnaco00f
025210051216      * Allocato errore
025220051219if  3c                   If        %error
025230051219      * Torno in prima videata se non è conferma visita
025240051216     c                   If        Not *In07
025250051216     c                   Eval      *In28 = *on
025260051216     c                   Eval      v1cmsg = msg(68)
025270051216     c                   Goto      emi01
025280051216     c                   EndIf
025290051219      * Torno al chiamante se è conferma visita
025300051216     c                   If        *In07
025310051216     c                   Goto      fine
025320051216     c                   EndIf
025330051219    3c                   EndIf
025340051219    2c                   EndIf
025350051219
025360091111      * Se è Interrogazione
025370091111     c                   if        *in05
025380091111      * visita/trattativa
025390091111     c                   if        *in06
025400091111     c                   eval      kksc = v1cnrv
025410091111     c     kCnaco        Chain(n)  Tfaco00f
025420091111      * cliente
025430091111     c                   else
025440051222     c                   eval      kksc = v1cksc
025450091111     c     kCnaco        Chain(n)  Cnaco00f
025460091111     c                   endif
025470091111     c                   endif
025480051216
025490091111      * Richiamato da gestione visite non in interrogazione
025500051219      * aggancio Tfaco00f e alloco il record
025510091111if  2c                   If        *In06 and not *in05
025520051222     c                   eval      kksc = v1cnrv
025530051216     c     kCnaco        Chain(e)  Tfaco00f
025540051216      * Allocato errore
025550051216     c                   If        %error
025560051216     c                   Goto      fine
025570051216     c                   EndIf
025580051219    2c                   EndIf
025590051219
025600051216      * Record trovato
025610051219if  2c                   If        %Found
025620051216      * Cliente esistente - manutenzione/Visualizzazione
025630051219if  3c                   If        acoflg <> '*'
025640051216     c  n05              Eval      *In02 = *On
025650051216     c  n05              Eval      vtcmod = mod(02)
025660051216     c   05              Eval      vtcmod = mod(03)
025670051216      * Cliente annullato
025680051219   x3c                   Else
025690051216     c                   Eval      *In04 = *On
025700051216     c                   Eval      vtcmod = mod(04)
025710051219    3c                   EndIf
025720051219      * Cliente nuovo - immissione
025730051219      * non essendo passata dalla prima videata l'inserimento avviene
025740051219      * solo se sono richiamata da gestione visite/offerte
025750051219      * quindi non controllo se sono in interrogazione
025760051219   x2c                   Else
025770051216     c                   Eval      *In01 = *On
025780051216     c                   Eval      vtcmod = mod(01)
025790051219    2c                   EndIf
025800051219    1c                   EndIf
025810051216
025820051216      * Se non è immissione imposto i dati dei file a video
025830051216if  1c                   If        Not *In01
025840060111     c  n06
025850060111     cann05kCnaco        Chain     Cnind00f
025860060111     c  n06
025870060111     can 05kCnaco        Chain(n)  Cnind00f
025880091111     c   06
025890091111     cann05kCnaco        Chain     Tfind00f
025900091111     c   06
025910091111     can 05kCnaco        Chain(n)  Tfind00f
025920060111     c  n06
025930060111     cann05kCnaco        Chain     Cnclp00f
025940060111     c  n06
025950060111     can 05kCnaco        Chain(n)  Cnclp00f
025960091111     c   06
025970091111     cann05kCnaco        Chain     Tfclp00f
025980091111     c   06
025990091111     can 05kCnaco        Chain(n)  Tfclp00f
026000060111     c  n06
026010060111     cann05v1cksc        Chain     Fncls01l
026020060111     c  n06
026030060111     can 05v1cksc        Chain(n)  Fncls01l
026040091111     c   06
026050091111     cann05v1cnrv        Chain     Tfcls01l
026060091111     c   06
026070091111     can 05v1cnrv        Chain(n)  Tfcls01l
026080051216
026090051216      * Data ultima variazione
026100051216     c                   Z-add     acoduv        dataamg
026110051216     c                   Z-add     aa1           aa2
026120051216     c                   Z-add     mm1           mm2
026130051216     c                   Z-add     gg1           gg2
026140051216     c                   Z-add     datagma       v2cduv
026150051216      * Allineato rcd da sede o da p.o.
026160051216     c                   If        acoftr = 'R'
026170051216     c                   Eval      v2ddtr = 'Ricevuto da Sede'
026180080115     c                   endif
026190080115     c                   if        acoftr = 'T'
026200051216     c                   Eval      v2ddtr = 'Trasmesso a Sede'
026210051216     c                   EndIf
026220051216      * Data allineamento rcd
026230051216     c                   Z-add     acodtr        dataamg
026240051216     c                   Z-add     aa1           aa2
026250051216     c                   Z-add     mm1           mm2
026260051216     c                   Z-add     gg1           gg2
026270051216     c                   Z-add     datagma       v2cdtr
026280051219      * Dati non visualizzati a video
026290051219     c                   Eval      v2copz = acoopz
026300051219     c                   Eval      v2ctic = acotic
026310051219     c                   Eval      v2clin = indlin
026320051216      * Dati anagrafici
026330051216     c                   Eval      v2crag = acorag
026340051216     c                   Eval      v2cvia = indvia
026350051216     c                   Eval      v2ccit = indcit
026360051216     c                   Eval      v2ccae = indcae
026370051216     c                   Eval      v2cprv = indprv
026380051216     c                   Eval      v2csta = indsta
026390051216     c                   Eval      v2ctel = indtel
026400051216     c                   Eval      v2ctlf = indtlf
026410051216     c                   Eval      v2ccdf = indcdf
026420080306     c                   Eval      savcdf = v2ccdf
026430051216     c                   Movel     indiva        w002a
026440051216     c                   If        w002a >= '00'
026450051216     c                   Movel     indiva        v2ivait
026460051216     c                   Else
026470051216     c                   Movel     indiva        v2civa
026480051216     c                   EndIf
026490080306     c                   Eval      saviva = v2civa
026500051216      * Blocco cliente
026510051216     c                   Eval      v2cabl = acoabl
026520130322     c                   eval      V2Cabledp = ACOabl
026530130322      * se blocco del cliente è '*' (blocco contabile) oppure
026540130322      * '7' (blocco automatico)
026550130322      * proteggo il campo del blocco + causale blocco
026560130322      * sempre, filiale e sede
026570130322     c                   IF        ACOabl = '*' or ACOabl = '7'
026580130322     c                   eval      *in79 = *on
026590130322     c                   ENDIF
026600130322     c                   IF        ACOabl = '7'
026610130322     c                   eval      V2Cabl = '8'
026620130322     c                   ENDIF
026630051220     c                   Eval      savabl = acoabl
026640051216      * Causale blocco cliente
026650051216     c                   Eval      v2cblc = clpnar
026660051216      * Stato del credito
026670051216     c                   Move      clpcon        v2ccon
026680110407      * Se il pgm è stato richiamato per il blocco cliente
026690110407      * imposto il blocco e la causale
026700110407     c                   IF        $Blocco
026710110407     c                   eval      v2cabl = '8'
026720110407     c                   eval      v2cblc = Parmcbl
026730110407     c                   ENDIF
026740051216      * Cliente annullabile
026750051216     c                   Eval      v2tb02 = atb02
026760051216      * Codice Potenziale
026770051216     c                   Eval      v2ccpo = acolib
026780051216     c                   Eval      savcpo = acolib
026790090616      * Cliente da sollecitare
026800090616     c                   Eval      v2csol = clpsol
026810090616      * sollecito in inglese
026820090616     c                   Eval      v2fsol = %subst(clsflo:4:1)
026830051216      * Codice commerciale
026840051216     c                   Movel     clpage        v3cage
026850051216      * Codice importanza cliente
026860051216     c                   Eval      v3cclv = clpclv
026870080313      * categoria merceologica
026880051216     c                   Move      acoitc        v3citc
026890051216      * Codice sottoconto fatturazione
026900100127     c                   Eval      v4cscf = %editc(clpscf:'X')
026910100329      * se sono da convalida offerta e non c'è il codice fatturazione
026920100329      * e la partita IVA è $$ imposto come codice fatturazione il codice
026930100329      * cliente che sto generando
026940100329     c                   IF        (v4cscf = *zeros or v4cscf = *blanks) and
026950100329     c                              *in07 and v2ivaeu = '$$'
026960100329     c                   eval      v4cscf = %editc(v1cksc:'X')
026970100329     c                   ENDIF
026980100127     c                   Eval      savscf = v4cscf
026990051216      * Fattura unificata
027000051216     c                   Eval      v4cuni = %subst(clsflo:2:1)
027010051216     c                   Eval      savuni = v4cuni
027020051216      * Tipo fattura
027030051216     c                   Move      clptft        v4ctft
027040051216     c                   Move      clptft        savtft
027050051216      * Frequenza fattura
027060051216     c                   Move      clpfft        v4cfft
027070051216     c                   Move      clpfft        savfft
027080051216      * Tipo data fattura (Inizio mese/Fine mese)
027090051216     c                   Eval      v4ctdf = clpfun
027100051216      * Spese invio fattura (nel file il campo non ha decimali solo a video)
027110051216     c                   Move      indsin        v4csin
027120051216      * Stampa mittente originale in fattura
027130051216     c                   Eval      v4cstm = %subst(clsflo:6:1)
027140051216      * Data prima spedizione fattura (solo visualizzazione viene impostato
027150051216      *                                dalla fatturazione)
027160051216     c                   Z-add     clpdps        dataamg
027170051216     c                   Z-add     aa1           aa2
027180051216     c                   Z-add     mm1           mm2
027190051216     c                   Z-add     gg1           gg2
027200051216     c                   Z-add     datagma       v4cdps
027210051216      * Data ultima spedizione fattura (solo visualizzazione viene impostato
027220051216      *                                 dalla fatturazione)
027230051216     c                   Z-add     clpdus        dataamg
027240051216     c                   Z-add     aa1           aa2
027250051216     c                   Z-add     mm1           mm2
027260051216     c                   Z-add     gg1           gg2
027270051216     c                   Z-add     datagma       v4cdus
027280051216      * Numero protocollo interno (solo visualizzazione viene impostato da Proj)
027290051216     c                   Eval      v4cnpn = indnpn
027300051216      * Numero protocollo cliente (solo visualizzazione viene impostato da Proj)
027310051216     c                   Eval      v4cnpr = indnpr
027320051216      * Data protocollo cliente   (solo visualizzazione viene impostato da Proj)
027330051216     c                   Z-add     inddpr        dataamg
027340051216     c                   Z-add     aa1           aa2
027350051216     c                   Z-add     mm1           mm2
027360051216     c                   Z-add     gg1           gg2
027370051216     c                   Z-add     datagma       v4cdpr
027380051216      * Codice pagamento
027390100727     c                   Move      indcdp        v4ccdp
027400051216      * Abi
027410100727     c                   Eval      v4cabi = indabi
027420051216      * Cab
027430100727     c                   Eval      v4ccab = indcab
027440121105      * Tassazione Post Fatturazopme
027450121105     c                   eval      V4Ctpf = %subst(CLSflo:3:1)
027460090612      * Blocco pagamenti
027470051216     c                   Eval      wbloc = %subst(indopz:1:1)
027480130322     c                   eval      V5Cblpedp = %subst(indopz:1:1)
027490130322      * se blocco pagamenti NON è 'B' (blocco manuale)
027500130322      * proteggo il campo del blocco pagamenti
027510130322      * sempre, filiale e sede
027520130322     c                   IF        V5Cblpedp <> 'B' and
027530130322     c                             V5Cblpedp <> '0' and
027540130322     c                             V5Cblpedp <> *blanks
027550130322     c                   eval      *in81 = *on
027560130322     c                   ENDIF
027570130322     c                   If        wbloc = '0'
027580130322     c                   Eval      v5cblp = 'NO'
027590130322     c                   Else
027600130322     c                   Eval      v5cblp = 'SI'
027610130322     c                   EndIf
027620100728
027630051216      * Codice pagamento contrassegni
027640051216     c                   Eval      v5cfpc = clpfpc
027650080115      * Codice IBAN
027660080115     c                   eval      v5ciban = clpbab
027670100803      * il BIC per i contrassegni per ora non ce l'ho mai quindi non lo faccio
027680100803      * mai vedere
027690100803     c                   eval      *in75 = *on
027700100728
027710100728      /free
027720100728       //?Nuovi tipi pagamento
027730100729       //?Pagamento DANNI --> DN
027740100728         v5tpdn = %subst(clstic:1:1);
027750100728       //?Recupero le coordinate bancarie
027760100728         wpag = 'DN';
027770100728         rqsOpCode = 'SEARCH';
027780100728         exsr Coordinate;
027790100728         IF  rpyEsito = 0;
027800100728           IF not *in06;
027810100730             v5ibandn = TrulIbanO0.IBAN;
027820100730             savabidn = TrulIbanO0.ABI;
027830100730             savcabdn = TrulIbanO0.CAB;
027840100730             v5bicdn  = TrulIbanO0.BIC;
027850100728           ELSE;
027860100730             v5ibandn = TrulIbanO1.IBAN;
027870100730             savabidn = TrulIbanO1.ABI;
027880100730             savcabdn = TrulIbanO1.CAB;
027890100730             v5bicdn  = TrulIbanO1.BIC;
027900100728           ENDIF;
027910100728         ENDIF;
027920100803         IF  v5bicdn = *blanks;
027930100803           *in76 = *on;
027940100803         ENDIF;
027950100729       //?Pagamento NOTE ACCREDITO --> NA
027960100728         v5tpna = %subst(clstic:2:1);
027970100728       //?Recupero le coordinate bancarie
027980100728         wpag = 'NA';
027990100728         exsr Coordinate;
028000100728         IF  rpyEsito = 0;
028010100728           IF not *in06;
028020100730             v5ibanna = TrulIbanO0.IBAN;
028030100730             savabina = TrulIbanO0.ABI;
028040100730             savcabna = TrulIbanO0.CAB;
028050100730             v5bicna  = TrulIbanO0.BIC;
028060100728           ELSE;
028070100730             v5ibanna = TrulIbanO1.IBAN;
028080100730             savabina = TrulIbanO1.ABI;
028090100730             savcabna = TrulIbanO1.CAB;
028100100730             v5bicna  = TrulIbanO1.BIC;
028110100728           ENDIF;
028120100728         ENDIF;
028130100803         IF  v5bicna = *blanks;
028140100803           *in77 = *on;
028150100803         ENDIF;
028160100730         savibandn = v5ibandn;
028170100730         savibanna = v5ibanna;
028180100728      /end-free
028190100728
028200051216      * Tipo comunicazione al mittente
028210051216     c                   Eval      v6ctcm = %subst(clsflo:10:1)
028220051216      * Tipo comunicazione fine giacenza
028230051216     c                   Eval      v6ctcf = %subst(clsflo:1:1)
028240051216      * Apertura automatica giacenza
028250051216     c                   Eval      v6capg = %subst(clsflo:9:1)
028260051216      * Disposizione mittente in arrivo
028270051216     c                   If        acorx1 = *Zeros
028280051216     c                   Eval      v6cdmg = 'SI'
028290051216     c                   Else
028300051216     c                   Eval      v6cdmg = 'NO'
028310051216     c                   EndIf
028320060801      * Chiusura automatica giacenza
028330060801     c                   If        acory1 = *Zeros
028340060801     c                   Eval      v6ccag = 'SI'
028350060801     c                   Else
028360060801     c                   Eval      v6ccag = 'NO'
028370060801     c                   EndIf
028380051216      * Rimborso danni
028390051216      * Stampa copia denuncia al cliente
028400051216     c                   Eval      v7csdn = %subst(clsflo:5:1)
028410051216      * Comunicazioni in inglese
028420051216     c                   Eval      v7csin = %subst(clsflo:7:1)
028430051216      * Escludi da tassazione diretta porti assegnati
028440051216     c                   Eval      v7ctpa = %subst(clsflo:8:1)
028450051216      * Nr. stop autotrasportatori
028460051216     c                   Eval      v7cstp = clsstp
028470051216
028480051216      * Se immissione imposto dei campi di default
028490051216   x1c                   Else
028500051219      * Campi non visualizzati
028510051219     c                   Eval      v2copz = wopz
028520051219     c                   Eval      v2ctic = wtic
028530051216     c                   Eval      v2clin = '1'
028540051219      * Campi visualizzati
028550051216     c                   Clear                   v2ccpo
028560051216      * Se provengo da visite/offerte
028570051216if  2c                   If        *In06
028580051216      * Carico i dati del potenziale
028590051220     c     ta60cpo       Chain(n)  Tncpo01l
028600051216     c                   If        %Found(Tncpo01l)
028610051216     c                   ExSr      Sr_Carcpo
028620051216     c                   EndIf
028630051216    2c                   EndIf
028640090612      * Cliente da sollecitare
028650090616      * imposto il dft
028660090616     c                   ExSr      Sr_Ctrsol
028670051216      * Commerciale
028680051216     c                   Clear                   v3cage
028690051216      * Imposto il codice commerciale della visita
028700051216     c   06              Movel     ta60cmm       v3cage
028710080313      * categoria merceologica
028720051216     c                   Clear                   v3citc
028730080313      * Se impostata dal potenziale riporto la categoria merceologica
028740051216     c                   If        savitc <> *Blanks
028750051216     c                   Eval      v3citc = savitc
028760051216     c                   EndIf
028770051219      * Codice importanza cliente
028780100301      * lo imposto se non è immissione anagrafica provvisoria da trattativa
028790100806     c  n06              Eval      v3cclv = 'C'
028800051216      * Codice sottoconto fatturazione
028810100729      * imposto a 0 così l'utente è obbligato ad inserirlo
028820100303     c                   eval      v4cscf = '0000000'
028830051216      * Tipo fattura
028840051216     c                   Eval      v4ctft = '0'
028850051216     c                   Eval      savtft = '0'
028860051216      * Frequenza fattura
028870051216     c                   Eval      v4cfft = '4'
028880051216     c                   Eval      savfft = '4'
028890091109      * Tipo data fattura (Inizio mese/Fine mese)
028900091125      * se da trattativa
028910100806     c   06              eval      v4ctdf = 'F'
028920051216      * Spese invio fatturazione
028930051216     c                   Z-add     §gedrf        v4csin
028940060210      * se non è anagrafica provvisoria e cliente 8888 o 9999 non devo
028950060210      * imputare le spese di invio fattura
028960060210     c                   Move      v1cksc        w0040
028970060210     c                   If        Not *In06 and
028980060210     c                             (w0040 = 8888 or w0040 = 9999)
028990060210     c                   Clear                   v4csin
029000060210     c                   EndIf
029010051216      * Protocollo
029020051216     c                   Clear                   v4cnpn
029030051216     c                   Clear                   v4cnpr
029040051216     c                   Clear                   v4cdpr
029050051221      * Codice Pagamento
029060100727     c                   Move      *Zeros        v4ccdp
029070130322      * Blocco pagamento campo di work
029080130322     c                   eval      wbloc = '0'
029090080115      * Tipo Pagamento contrassegni
029100091125      * se NON è da trattativa
029110100806     c  n06              eval      v5cfpc = '2'
029120100805
029130100805      /free
029140100805       //?Tipo pagamento Danni
029150150424         v5tpdn = dfttipdn;
029160100805       //?Tipo pagamento Note Accredito
029170150424         v5tpna = dfttipna;
029180100805      /end-free
029190100728
029200051216      * Tipo comunicazione al mittente
029210120118      * lo imposto se non è immissione anagrafica provvisoria da trattativa
029220120118     c  n06              Eval      v6ctcm = §2gtcm
029230051216      * Tipo comunicazione fine giacenza
029240051216     c                   Eval      v6ctcf = §2gtfg
029250051216      * Apertura automatica giacenza
029260051216     c                   Clear                   v6capg
029270051216      * Disposizione mittente in arrivo
029280051216     c                   Eval      v6cdmg = 'SI'
029290060801      * Chiusura automatica giacenza
029300060801     c                   Eval      v6ccag = 'SI'
029310051216
029320051216    1c                   EndIf
029330110217
029340110217      /free
029350110217       //?Se non è anagrafica provvisoria
029360110217         IF  not *in06;
029370110217       //?Aggancio l'organigramma x controllo se filiale cliente
029380110217       //?è un primo o secondo livello
029390110217           clear og148;
029400110217           w0030 = %dec(%subst(%editc(v1cksc:'X'):1:3):3:0);
029410110217           chain  w0030  azorg01l;
029420110217           IF  %found(azorg01l);
029430110217             og148 = ORGde8;
029440110217           ENDIF;
029450110217           sav_Livello = §OGlpo;
029460110217       //?Recupero da tabella Y4O il codice cliente 'incassi da attribuire'
029470110217           clear TIBS02DS;
029480110217           clear dY4O;
029490110217           T02mod = 'C';
029500110217           T02cod = 'Y4O';
029510110217           T02ke1 = '201';
029520110217           T02ke2 = %editc(w0030:'X');
029530110217           TNTBE_RicercaControllo (kpjba : tibs02ds);
029540110217           IF  T02err = *blanks;
029550110217             dY4O = T02uni;
029560110217           ENDIF;
029570110217           wCodIncAtt = %subst(§4Osc1:2:7);
029580110217         ENDIF;
029590170421
029600170421       //?Se sono in manutenzione
029610170421       //?Se non sono utente EDP e il cliente che sto manutenzionando
029620170421       //?un cliente IMPORT DPD proteggo i flag relativi alla
029630170421       //?Gestione Giacenze
029640170421         IF  *in02 and not *in20 and %lookup(V1Cksc:CliImport) > 0;
029650170421           *in85 = *on;
029660170421         ENDIF;
029670170421
029680110217      /end-free
029690051216
029700051216      * Decodifico
029710051216     c                   ExSr      Sr_Decdati
029720060531     c
029730060531     c* Memorizzo l'immagine precedente per  verificare se dati variati
029740150427      /free
029750150427         exsr ImmaginePrima;
029760150427      /end-free
029770051216
029780051216     c                   EndSr
029790100728
029800100728      /free
029810100728       //--------------------------------------------------------------
029820100728       //?Operazioni relative alle coordinate bancarie.
029830100728       //--------------------------------------------------------------
029840100728       BEGSR Coordinate;
029850100802
029860100802         clear rpyEsito;
029870100728
029880100728       //?FNCBA00F
029890100728         IF  not *in06;
029900100730           clear TrulIbanI0;
029910100730           clear TrulIbanO0;
029920100728           TrulIbanI0.KSC = v1cksc;
029930100728           TrulIbanI0.PAG = wpag;
029940100729         //?Se non è ricerca passo i dati del video
029950100729           IF  rqsOpCode <> 'SEARCH';
029960100805             TrulIbanI0.IBAN  = wiban;
029970100805             TrulIbanI0.BIC   = wbic;
029980100805             TrulIbanI0.CODPO = wcodpo;
029990100805             TrulIbanI0.CODPN = wcodpn;
030000100805             TrulIbanI0.PGM   = wpgm;
030010100729           ENDIF;
030020100728           rqsFormatName = 'TRULIBANI0';
030030100728           rpyFormatName = 'TRULIBANO0';
030040100728           rqsDataSize   = %size(TrulIbanI0);
030050100728           rpyDataSize   = %size(TrulIbanO0);
030060100728           TrulIbanR (rqsOpCode:rpyEsito:
030070100729                      rqsFormatName:TrulIbanI0:rqsDataSize:
030080100729                      rpyFormatname:TrulIbanO0:rpyDataSize);
030090100728       //?TFCBA00F
030100100728         ELSE;
030110100730           clear TrulIbanI1;
030120100730           clear TrulIbanO1;
030130100729           TrulIbanI1.NRV = v1cnrv;
030140100728           TrulIbanI1.PAG = wpag;
030150100729         //?Se non è ricerca passo i dati del video
030160100729           IF  rqsOpCode <> 'SEARCH';
030170100730             TrulIbanI1.IBAN = wiban;
030180100730             TrulIbanI1.BIC  = wbic;
030190100729           ENDIF;
030200100728           rqsFormatName = 'TRULIBANI1';
030210100728           rpyFormatName = 'TRULIBANO1';
030220100728           rqsDataSize   = %size(TrulIbanI1);
030230100728           rpyDataSize   = %size(TrulIbanO1);
030240100728           TrulIbanR (rqsOpCode:rpyEsito:
030250100728                      rqsFormatName:TrulIbanI1:rqsDataSize:
030260100729                      rpyFormatname:TrulIbanO1:rpyDataSize);
030270100728         ENDIF;
030280100728
030290100728       ENDSR;
030300100728      /end-free
030310051216
030320051216      *------------------------------------------------------------------------*
030330051216      * DECODIFICO I DATI DELLE VIDEATE
030340051216      *------------------------------------------------------------------------*
030350051216     c     Sr_Decdati    BegSr
030360051216
030370060220     c                   Eval      *In19 = *Off
030380051216     c                   Eval      *In22 = *Off
030390051216     c                   Eval      *In23 = *Off
030400100728     c                   Eval      *In71 = *Off
030410100728     c                   Eval      *In72 = *Off
030420100728     c                   Eval      *In73 = *Off
030430100728     c                   Eval      *In74 = *Off
030440150415     c                   Eval      *In82 = *Off
030450051216
030460051216      * Decodifico lo stato del credito
030470051216     c                   Clear                   v2dcon
030480051216     c                   Clear                   ds4w
030490051216     c                   Eval      kcod = '4W'
030500051216     c                   Move(p)   v2ccon        w003a
030510051216     c                   Eval      kkey = w003a
030520051216     c     kTabel        Chain     Tabel00f
030530051216     c                   If        %Found(Tabel00f) and
030540051216     c                             tblflg = *Blanks
030550051216     c                   Eval      ds4w = tbluni
030560051216     c                   EndIf
030570051216     c                   Eval      v2dcon = §4wdes
030580051216
030590051216      * Se sono in filiale proteggo lo stato del credito se non è modificabile
030600120928      * da p.o.
030610130325      * da 25/03/2013 abbiamo deciso che anche in sede non è modificale, solo
030620130325      * da ProJ
030630130325     c                   If        §4wmbl = 'N'
030640060220     c                   Eval      *In19 = *On
030650051216     c                   EndIf
030660130322
030670120928      * Se sono in filiale proteggo il blocco pagamenti se non è modificabile
030680120928      * da p.o.
030690120928     c                   If        Not *In09 and §4wmbp = 'N'
030700120928     c                   Eval      *In81 = *On
030710120928     c                   EndIf
030720120925      * Se sono in filiale proteggo il blocco cliente se non è modificabile
030730120925      * da p.o.
030740120928     c                   If        Not *In09 and §4wmtb = 'N'
030750120925     c                   Eval      *In79 = *On
030760120925     c                   EndIf
030770170317      /free
030780170330       //?Se sono un utente NON abilitato alla variazione
030790170317       //?e il blocco clienti è protetto
030800170317       //?e il cliente è in contenzioso
030810170330       //?devo bloccare anche la frequenza fattura
030820170330         IF  §UTEclpfft <> 'S' and *in79 and
030830170330             §4Wtip <> *blanks;
030840170317           *in83 = *on;
030850170317         ENDIF;
030860170317      /end-free
030870051216
030880051216      * Decodifico la causale blocco cliente
030890110323     c                   clear                   dsbi
030900051216     c                   Clear                   v2dblc
030910051216     c                   Eval      kcod = 'BI'
030920051216     c                   Eval      kkey = v2cblc
030930051216     c     kTabel        Chain     Tabel00f
030940051216     c                   If        %Found(Tabel00f) and
030950051216     c                             tblflg = *Blanks
030960110323     c                   Eval      dsbi = tbluni
030970051216     c                   EndIf
030980110323     c                   Eval      v2dblc = §bides
030990051216
031000051216      * Decodifico il codice potenziale
031010051216     c                   Clear                   v2dcpo
031020110223     c                   clear                   v2hfls
031030051216     c                   If        v2ccpo <> *Zeros
031040051220     c     v2ccpo        Chain(n)  Tncpo01l
031050051216     c                   If        %Found(Tncpo01l)
031060051216     c                   Eval      v2dcpo = cporag
031070110223     c                   eval      v2hfls = CPOfls
031080121105     c                   eval      dCPO01 = CPOrst
031090051216     c                   EndIf
031100051216     c                   EndIf
031110051216
031120051216      * Decodifico il codice commerciale
031130051216     c                   Clear                   v3dage
031140130806     c                   If        %check(digits:V3Cage) = 0
031150130806     c                   move      V3Cage        CMMcod
031160130806     c     CMMcod        chain     AZCMM000
031170130806     c                   if        %Found(AZCMM01L)
031180130806     c                   movel     CMMdes        v3dage
031190130806     c                   endif
031200130806     c                   EndIf
031210051216
031220051216      * Decodifico il codice importanza cliente
031230051216     c                   Clear                   v3dclv
031240051216     c                   Clear                   dsic
031250051216     c                   Eval      kcod = 'IC'
031260051216     c                   Eval      kkey = v3cclv
031270051216     c     kTabel        Chain     Tabel00f
031280051216     c                   If        %Found(Tabel00f) and
031290051216     c                             tblflg = *Blanks
031300051216     c                   Eval      dsic = tbluni
031310051216     c                   EndIf
031320051216     c                   Eval      v3dclv = §sicde
031330051216
031340080313      * Decodifico categoria merceologica
031350051216     c                   Clear                   v3ditc
031360051216     c                   Clear                   ds1l
031370051216     c                   Eval      kcod = '1L'
031380051216     c                   Movel     v3citc        kkey
031390051216     c     kTabel        Chain     Tabel00f
031400051216     c                   If        %Found(Tabel00f) and
031410051216     c                             tblflg = *Blanks
031420051216     c                   Eval      ds1l = tbluni
031430051216     c                   EndIf
031440051216     c                   Eval      v3ditc = §1ldes
031450051216
031460051216      * Decodifico sottoconto fatturazione
031470051216     c                   Clear                   Tibs69ds
031480051216     c                   Move      v4cscf        I69kac
031490051216     c                   Call      'TIBS69R'
031500051216     c                   Parm                    Tibs69ds
031510051216     c                   Parm                    ds_cnaco
031520051216     c                   Parm                    ds_cnind
031530051216     c                   Parm                    ds_cnclp
031540051216     c                   Parm                    ds_fncls
031550051216     c                   Eval      wtibs69 = *On
031560051216     c                   Movel     w_acorag      v4dscf
031570051216
031580051216      * Decodifico il tipo fattura
031590051216     c                   Clear                   Tibs02ds
031600051216     c                   Eval      t02mod = 'C'
031610051216     c                   Eval      t02sif = knsif
031620051216     c                   Eval      t02cod = 'TFT'
031630051216     c                   Eval      t02ke1 = v4ctft
031640051216     c                   Call      'TIBS02R'
031650051216     c                   Parm                    kpjba
031660051216     c                   Parm                    Tibs02ds
031670051216     c                   If        t02err = *Blanks
031680051216     c                   Eval      dtft = t02uni
031690051216     c                   EndIf
031700051216     c                   Eval      v4dtft = §tftdes
031710051216
031720051216      * Decodifico la frequenza fattura
031730051216     c                   Clear                   v4dfft
031740051216     c                   Clear                   dsff
031750051216     c                   Eval      kcod = 'FF'
031760051216     c                   Eval      kkey = v4cfft
031770051216     c     kTabel        Chain     Tabel00f
031780051216     c                   If        %Found(Tabel00f) and
031790051216     c                             tblflg = *Blanks
031800051216     c                   Eval      dsff = tbluni
031810051216     c                   EndIf
031820051216     c                   Eval      v4dfft = §ffdes
031830051216
031840051216      * Decodifico il tipo data fattura
031850051216     c                   If        v4ctdf <> *Blanks
031860051216     c                   Clear                   v4dtdf
031870051216     c                   Eval      kcod = '13'
031880051216     c                   Eval      kkey = v4ctdf
031890051216     c     kTabel        Chain     Tabel00f
031900051216     c                   If        %Found(Tabel00f) and
031910051216     c                             tblflg = *Blanks
031920051216     c                   Eval      v4dtdf = tbluni
031930051216     c                   EndIf
031940051216     c                   EndIf
031950051216
031960051216      * Decodifico codice di pagamento
031970100727     c                   Clear                   v4dcdp
031980051216     c                   Clear                   dsfa
031990051216     c                   Eval      kcod = 'FA'
032000100727     c                   Movel     v4ccdp        w004a
032010051216     c                   Move      '1'           w004a
032020051216     c                   Eval      kkey = w004a
032030051216     c     kTabel        Chain     Tabel00f
032040051216     c                   If        %Found(Tabel00f) and
032050051216     c                             tblflg = *Blanks
032060051216     c                   Eval      dsfa = tbluni
032070051216     c                   EndIf
032080100727     c                   Eval      v4dcdp = §fades
032090150415
032100150415      /free
032110150415       //?Se utente di filiale e tipo pagamento utilizzabile solo da sede
032120150415       //?devo proteggere la Banca ABI/CAB del pagamento fatture
032130150415         IF  not *in09 and §FAuti = 'S';
032140150415           *in82 = *on;
032150150415         ENDIF;
032160170320       //?Se sono un utente di filiale
032170170320       //?e il blocco clienti è protetto
032180170320       //?e il cliente è in contenzioso
032190170330       //?devo bloccare il codice pagamento
032200170330         IF  not *in09 and *in79 and §4Wtip <> *blanks;
032210170320           *in84 = *on;
032220170320         ENDIF;
032230150415      /end-free
032240080206
032250080206      * decodifico le coordinate bancarie abi-cab
032260100729      * riscossione fattura
032270100729     c                   clear                   v4cbna
032280100727     c                   eval      kabi = v4cabi
032290100727     c                   eval      kcab = v4ccab
032300100729     c                   exsr      DesBanca
032310100729     c                   eval      v4cbna = wdesbanca
032320051216
032330051216      * Decodifico codice pagamanto contrassegni
032340051216     c                   Clear                   v5dfpc
032350051216     c                   Clear                   ds4u
032360051216     c                   Eval      kcod = '4U'
032370051216     c                   Eval      kkey = v5cfpc
032380051216     c     kTabel        Chain     Tabel00f
032390051216     c                   If        %Found(Tabel00f) and
032400051216     c                             tblflg = *Blanks
032410051216     c                   Eval      ds4u = tbluni
032420051216     c                   EndIf
032430051216     c                   Eval      v5dfpc = §4udes
032440100729      * tipo pagamento contrassegni, salvo il flag di tipo pagamento i/e
032450100729     c                   eval      sav4utip = §4utip
032460051216      * Le coordinate bancare le visualizzo o le proteggo in base al
032470051216      * tipo pagamento
032480051216     c                   Select
032490051216      * Coordinate italiane, visualizzo
032500051216      * Immissione coordinate non richiesta, non visualizzo
032510051216     c                   When      §4uabi = 'N'
032520051216     c                   Eval      *In22 = *On
032530060215     c                   Eval      savin22 = *In22
032540051216      * Se pagamento estero proteggo le coordinate bancarie perchè
032550051216      * gestite dalla sede.
032560080115     c                   when      §4utip = 'E'
032570051216     c                   Eval      *In23 = *On
032580060215     c                   Eval      savin23 = *In23
032590051216     c                   EndSl
032600080115      * se le visualizzo ed è un tipo pagamento italia decodifico la banca
032610080115     c                   if        not *in22 and not *in23
032620080206     c                             and v5ciban <> *blanks
032630080213     c                             and (%subst(v5ciban:1:2) = 'IT'
032640080213     c                              or %subst(v5ciban:1:2) = 'SM')
032650080116     c                   clear                   v5cbab
032660080213     c                   monitor
032670080115     c                   eval      kabi = %dec(%subst(v5ciban:6:5):5:0)
032680080115     c                   eval      kcab = %dec(%subst(v5ciban:11:5):5:0)
032690080213     c                   on-error
032700080213     c                   clear                   kabi
032710080213     c                   clear                   kcab
032720080213     c                   endmon
032730100729     c                   exsr      DesBanca
032740100729     c                   eval      v5cbab = wdesbanca
032750080115     c                   endif
032760090401      * se le visualizzo ed è un pagemento estero senza IBAN con
032770090401      * abi e cab no a 99999 decodifico la banca
032780090401     c                   if        not *in22 and *in23
032790090401     c                             and v5ciban = *blanks
032800090401     c                             and clpabi <> *all'9'
032810090401     c                             and clpcab <> *all'9'
032820090401     c                   monitor
032830090401     c                   eval      kabi = clpabi
032840090401     c                   eval      kcab = clpcab
032850090401     c                   on-error
032860090401     c                   clear                   kabi
032870090401     c                   clear                   kcab
032880090401     c                   endmon
032890100729     c                   exsr      DesBanca
032900100729     c                   eval      v5cbab = wdesbanca
032910090401     c                   endif
032920100728
032930100728      /free
032940100728       //?Pagamenti DANNI --> DN
032950100728       //?tipo pagamento
032960100728         clear v5dpdn;
032970100728         clear ds4u;
032980100728         kcod = '4U';
032990100730         kkey = v5tpdn;
033000100728         chain (codut:kcod:kkey) TABEL00F;
033010100728         IF  %found(TABEL00F) and TBLflg = *blanks;
033020100728           ds4u = TBLuni;
033030100728         ENDIF;
033040100730         v5dpdn = §4Udes;
033050100802       //?salvo il tipo pagamento
033060100802         savtipdn = v5tpdn;
033070100728       //?coordinate bancarie
033080100728         SELECT;
033090100728         //?se abi cab non obbligatori non visualizzo
033100100730           WHEN  §4Uabi = 'N';
033110100728             *in71 = *on;
033120100728             savin71 = *in71;
033130100805             *in76 = *off;
033140100728         //?se pagamento estero proteggo perchè solo la sede le gestisce
033150100730           WHEN  §4Utip = 'E';
033160100728             *in72 = *on;
033170100728             savin72 = *in72;
033180100728         ENDSL;
033190100729       //?se IBAN IT o SM (IBAN italia) visualizzo la descrizione banca
033200100729       //?recuperandola con ABI e CAB dell'IBAN
033210100729         IF  not *in71 and not *in72 and v5ibandn <> *blanks and
033220100729            (%subst(v5ibandn:1:2) = 'IT' or %subst(v5ibandn:1:2) = 'SM');
033230100729           clear v5babdn;
033240100729           monitor;
033250100729           kabi = %dec(%subst(v5ibandn:6:5):5:0);
033260100729           kcab = %dec(%subst(v5ibandn:11:5):5:0);
033270100729           on-error;
033280100729           clear kabi;
033290100729           clear kcab;
033300100729           endmon;
033310100729           exsr DesBanca;
033320100729           v5babdn = wDesBanca;
033330100729         ENDIF;
033340100729       //?se IBAN estero (campo iban vuoto e ABI e CAB impostati <> 99999)
033350100729       //?visualizzo la descrizione della banca recuperandola con ABI e CAB
033360100729       //?memorizzati sul file
033370100729         IF  not *in71 and not *in72 and v5ibandn = *blanks and
033380100729             savabidn <> *all'9' and savcabdn <> *all'9';
033390100729           clear v5babdn;
033400100729           monitor;
033410100729           kabi = savabidn;
033420100729           kcab = savcabdn;
033430100729           on-error;
033440100729           clear kabi;
033450100729           clear kcab;
033460100729           endmon;
033470100729           exsr DesBanca;
033480100729           v5babdn = wDesBanca;
033490100729         ENDIF;
033500100729
033510100729       //?Pagamenti NOTE ACCREDITO --> NA
033520100729       //?tipo pagamento
033530100729         clear v5dpna;
033540100729         clear ds4u;
033550100729         kcod = '4U';
033560100730         kkey = v5tpna;
033570100729         chain (codut:kcod:kkey) TABEL00F;
033580100729         IF  %found(TABEL00F) and TBLflg = *blanks;
033590100729           ds4u = TBLuni;
033600100729         ENDIF;
033610100730         v5dpna = §4Udes;
033620100802       //?salvo il tipo pagamento
033630100802         savtipna = v5tpna;
033640100729       //?coordinate bancarie
033650100729         SELECT;
033660100729         //?se abi cab non obbligatori non visualizzo
033670100730           WHEN  §4Uabi = 'N';
033680100729             *in73 = *on;
033690100729             savin73 = *in73;
033700100805             *in77 = *off;
033710100729         //?se pagamento estero proteggo perchè solo la sede le gestisce
033720100730           WHEN  §4Utip = 'E';
033730100729             *in74 = *on;
033740100729             savin74 = *in74;
033750100729         ENDSL;
033760100729       //?se IBAN IT o SM (IBAN italia) visualizzo la descrizione banca
033770100729       //?recuperandola con ABI e CAB dell'IBAN
033780100729         IF  not *in73 and not *in74 and v5ibanna <> *blanks and
033790100729            (%subst(v5ibanna:1:2) = 'IT' or %subst(v5ibanna:1:2) = 'SM');
033800100729           clear v5babna;
033810100729           monitor;
033820100729           kabi = %dec(%subst(v5ibanna:6:5):5:0);
033830100729           kcab = %dec(%subst(v5ibanna:11:5):5:0);
033840100729           on-error;
033850100729           clear kabi;
033860100729           clear kcab;
033870100729           endmon;
033880100729           exsr DesBanca;
033890100729           v5babna = wDesbanca;
033900100729         ENDIF;
033910100729       //?se IBAN estero (campo iban vuoto e ABI e CAB impostati <> 99999)
033920100729       //?visualizzo la descrizione della banca recuperandola con ABI e CAB
033930100729       //?memorizzati sul file
033940100729         IF  not *in73 and not *in74 and v5ibanna = *blanks and
033950100729             savabina <> *all'9' and savcabna <> *all'9';
033960100729           clear v5babna;
033970100729           monitor;
033980100729           kabi = savabina;
033990100729           kcab = savcabna;
034000100729           on-error;
034010100729           clear kabi;
034020100729           clear kcab;
034030100729           endmon;
034040100729           exsr DesBanca;
034050100729           v5babna = wDesbanca;
034060100729         ENDIF;
034070100729
034080100728      /end-free
034090051216
034100051216      * Decodifico tipo comunicazioni al mittente
034110051216     c                   Clear                   v6dtcm
034120051216     c                   Clear                   ds2f
034130051216     c                   Eval      kcod = '2F'
034140051216     c                   Eval      kkey = v6ctcm
034150051216     c     kTabel        Chain     Tabel00f
034160051216     c                   If        %Found(Tabel00f) and
034170051216     c                             tblflg = *Blanks
034180051216     c                   Eval      ds2f = tbluni
034190051216     c                   EndIf
034200051216     c                   Eval      v6dtcm = §2fdes
034210051216
034220051216      * Decodifico tipo comunicazioni fine giacenza
034230051216     c                   Clear                   v6dtcf
034240051216     c                   Clear                   ds2h
034250051216     c                   Eval      kcod = '2H'
034260051216     c                   Eval      kkey = v6ctcf
034270051216     c     kTabel        Chain     Tabel00f
034280051216     c                   If        %Found(Tabel00f) and
034290051216     c                             tblflg = *Blanks
034300051216     c                   Eval      ds2h = tbluni
034310051216     c                   EndIf
034320051216     c                   Eval      v6dtcf = §2hdes
034330051216
034340051216      * Decodifico apertura automatica giacenza
034350051216     c                   Clear                   v6dapg
034360051216     c                   Clear                   ds2u
034370051216     c                   Eval      kcod = '2U'
034380051216     c                   Eval      kkey = v6capg
034390051216     c     kTabel        Chain     Tabel00f
034400051216     c                   If        %Found(Tabel00f) and
034410051216     c                             tblflg = *Blanks
034420051216     c                   Eval      ds2u = tbluni
034430051216     c                   EndIf
034440051216     c                   Eval      v6dapg = §2udes
034450051216
034460051216     c                   EndSr
034470051216
034480051216      *------------------------------------------------------------------------*
034490051216      * CARICO I LUOGHI PREVISTI
034500051216      *------------------------------------------------------------------------*
034510051216     c     Sr_Carluoghi  BegSr
034520051216
034530051216     c                   Eval      *In10 = *Off
034540051216     c                   Eval      *In11 = *Off
034550051216     c                   Eval      *In12 = *Off
034560051216     c                   Eval      *In14 = *Off
034570051216     c                   Eval      *In16 = *Off
034580051216     c                   Eval      *In17 = *Off
034590060208     c                   Eval      *In33 = *Off
034600060208     c                   Eval      *In35 = *Off
034610060208     c                   Eval      *In37 = *Off
034620051216
034630051216      * Luogo invio fattura
034640051216     c                   Eval      kspecli = v1cksc
034650051216     c                   Eval      kspecod = '001'
034660051216     c     kFnspe        Chain     Fnspe01l
034670051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
034680051216     c                   Eval      *In10 = *On
034690051216     c                   EndIf
034700051216
034710051216      * Luogo intestatario pagamento contrassegno
034720051216     c                   Eval      kspecod = '555'
034730051216     c     kFnspe        Chain     Fnspe01l
034740051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
034750051216     c                   Eval      *In11 = *On
034760051216     c                   EndIf
034770051216
034780051216      * Luogo invio contrassegno mittente
034790051216     c                   Eval      kspecod = '666'
034800051216     c     kFnspe        Chain     Fnspe01l
034810051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
034820051216     c                   Eval      *In12 = *On
034830051216     c                   EndIf
034840051216
034850051216      * Luogo spedizione giacenza
034860051219     c                   Clear                   wfax005
034870051219     c                   Clear                   wmail005
034880051216     c                   Eval      kspecod = '005'
034890051216     c     kFnspe        Chain     Fnspe01l
034900051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
034910051216     c                   Eval      *In14 = *On
034920051216     c                   Eval      wfax005 = spefax
034930051219     c     kFnsp2        Chain     Fnsp201l
034940051219     c                   If        %Found(Fnsp201l) and sp2flg = *Blanks
034950051219     c                   Eval      wmail005 = sp2est
034960051219     c                   EndIf
034970051216     c                   EndIf
034980060208
034990060208      * Luogo invio merce resa
035000060208     c                   Eval      kspecod = '300'
035010060208     c     kFnspe        Chain     Fnspe01l
035020060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
035030060208     c                   Eval      *In33 = *On
035040060208     c                   EndIf
035050051216
035060051216      * Luogo preavviso/avviso danno
035070051216     c                   Eval      kspecod = '006'
035080051216     c     kFnspe        Chain     Fnspe01l
035090051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
035100051216     c                   Eval      *In16 = *On
035110051216     c                   EndIf
035120051216
035130051216      * Luogo progetto di liquidazione
035140051216     c                   Eval      kspecod = '008'
035150051216     c     kFnspe        Chain     Fnspe01l
035160051216     c                   If        %Found(Fnspe01l) and speflg = *Blanks
035170051216     c                   Eval      *In17 = *On
035180051216     c                   EndIf
035190060208
035200060208      * Luogo invio orm estero
035210060208     c                   Eval      kspecod = '002'
035220060208     c     kFnspe        Chain     Fnspe01l
035230060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
035240060208     c                   Eval      *In35 = *On
035250060208     c                   EndIf
035260060208
035270060208      * Luogo invio doc.a dogana
035280060208     c                   Eval      kspecod = '200'
035290060208     c     kFnspe        Chain     Fnspe01l
035300060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
035310060208     c                   Eval      *In37 = *On
035320060208     c                   EndIf
035330051216
035340051216      * Decodifico i luoghi caricati
035350051216     c                   ExSr      Sr_Decluoghi
035360051216
035370051216     c                   EndSr
035380051216
035390051216      *------------------------------------------------------------------------*
035400051216      * DECODIFICO I LUOGHI CARICATI
035410051216      *------------------------------------------------------------------------*
035420051216     c     Sr_Decluoghi  BegSr
035430051216
035440051216     c                   Clear                   v4dl001
035450051216     c                   Clear                   v4cl001
035460051216     c                   Clear                   v5dl555
035470051216     c                   Clear                   v5cl555
035480051216     c                   Clear                   v5dl666
035490051216     c                   Clear                   v5cl666
035500051216     c                   Clear                   v6dl005
035510051216     c                   Clear                   v6cl005
035520060208     c                   Clear                   v6dl300
035530060208     c                   Clear                   v6cl300
035540051216     c                   Clear                   v7dl006
035550051216     c                   Clear                   v7cl006
035560051216     c                   Clear                   v7dl008
035570051216     c                   Clear                   v7cl008
035580060208     c                   Clear                   v7dl002
035590060208     c                   Clear                   v7cl002
035600060208     c                   Clear                   v7dl200
035610060208     c                   Clear                   v7cl200
035620051216
035630051216      * Decodifico luogo 001
035640071128     c                   clear                   ds4l
035650051216     c                   Eval      kcod = '4L'
035660051216     c                   Eval      kkey = '001'
035670051216     c     kTabel        Chain     Tabel00f
035680051216     c                   If        %Found(Tabel00f) and
035690051216     c                             tblflg = *Blanks
035700051216     c                   Eval      ds4l = tbluni
035710051216     c                   EndIf
035720051216      * Imposto il campo di descrizione
035730060203     c                   Eval      v4dl001 = '001 - ' + §4ldes
035740071128     c                   eval      $ufi001 = §4lufi
035750051216
035760051216      * Decodifico luogo 555
035770071128     c                   clear                   ds4l
035780051216     c                   Eval      kkey = '555'
035790051216     c     kTabel        Chain     Tabel00f
035800051216     c                   If        %Found(Tabel00f) and
035810051216     c                             tblflg = *Blanks
035820051216     c                   Eval      ds4l = tbluni
035830051216     c                   EndIf
035840051216      * Imposto il campo di descrizione
035850060203     c                   Eval      v5dl555 = '555 - ' + §4ldes
035860071128     c                   eval      $ufi555 = §4lufi
035870051216
035880051216      * Decodifico luogo 666
035890071128     c                   clear                   ds4l
035900051216     c                   Eval      kkey = '666'
035910051216     c     kTabel        Chain     Tabel00f
035920051216     c                   If        %Found(Tabel00f) and
035930051216     c                             tblflg = *Blanks
035940051216     c                   Eval      ds4l = tbluni
035950051216     c                   EndIf
035960051216      * Imposto il campo di descrizione
035970060203     c                   Eval      v5dl666 = '666 - ' + §4ldes
035980071128     c                   eval      $ufi666 = §4lufi
035990051216
036000051216      * Decodifico luogo 005
036010071128     c                   clear                   ds4l
036020051216     c                   Eval      kkey = '005'
036030051216     c     kTabel        Chain     Tabel00f
036040051216     c                   If        %Found(Tabel00f) and
036050051216     c                             tblflg = *Blanks
036060051216     c                   Eval      ds4l = tbluni
036070051216     c                   EndIf
036080051216      * Imposto il campo di descrizione
036090060203     c                   Eval      v6dl005 = '005 - ' + §4ldes
036100071128     c                   eval      $ufi005 = §4lufi
036110060208
036120060208      * Decodifico luogo 300
036130071128     c                   clear                   ds4l
036140060208     c                   Eval      kkey = '300'
036150060208     c     kTabel        Chain     Tabel00f
036160060208     c                   If        %Found(Tabel00f) and
036170060208     c                             tblflg = *Blanks
036180060208     c                   Eval      ds4l = tbluni
036190060208     c                   EndIf
036200060208      * Imposto il campo di descrizione
036210060208     c                   Eval      v6dl300 = '300 - ' + §4ldes
036220071128     c                   eval      $ufi300 = §4lufi
036230051216
036240051216      * Decodifico luogo 006
036250071128     c                   clear                   ds4l
036260051216     c                   Eval      kkey = '006'
036270051216     c     kTabel        Chain     Tabel00f
036280051216     c                   If        %Found(Tabel00f) and
036290051216     c                             tblflg = *Blanks
036300051216     c                   Eval      ds4l = tbluni
036310051216     c                   EndIf
036320051216      * Imposto il campo di descrizione
036330060203     c                   Eval      v7dl006 = '006 - ' + §4ldes
036340071128     c                   eval      $ufi006 = §4lufi
036350051216
036360051216      * Decodifico luogo 008
036370071128     c                   clear                   ds4l
036380051216     c                   Eval      kkey = '008'
036390051216     c     kTabel        Chain     Tabel00f
036400051216     c                   If        %Found(Tabel00f) and
036410051216     c                             tblflg = *Blanks
036420051216     c                   Eval      ds4l = tbluni
036430051216     c                   EndIf
036440051216      * Imposto il campo di descrizione
036450060203     c                   Eval      v7dl008 = '008 - ' + §4ldes
036460071128     c                   eval      $ufi008 = §4lufi
036470060208
036480060208      * Decodifico luogo 002
036490071128     c                   clear                   ds4l
036500060208     c                   Eval      kkey = '002'
036510060208     c     kTabel        Chain     Tabel00f
036520060208     c                   If        %Found(Tabel00f) and
036530060208     c                             tblflg = *Blanks
036540060208     c                   Eval      ds4l = tbluni
036550060208     c                   EndIf
036560060208      * Imposto il campo di descrizione
036570060208     c                   Eval      v7dl002 = '002 - ' + §4ldes
036580071128     c                   eval      $ufi002 = §4lufi
036590060208
036600060208      * Decodifico luogo 200
036610071128     c                   clear                   ds4l
036620060208     c                   Eval      kkey = '200'
036630060208     c     kTabel        Chain     Tabel00f
036640060208     c                   If        %Found(Tabel00f) and
036650060208     c                             tblflg = *Blanks
036660060208     c                   Eval      ds4l = tbluni
036670060208     c                   EndIf
036680060208      * Imposto il campo di descrizione
036690060208     c                   Eval      v7dl200 = '200 - ' + §4ldes
036700071128     c                   eval      $ufi200 = §4lufi
036710051216
036720051216     c                   EndSr
036730051216
036740051216      *------------------------------------------------------------------------*
036750051216      * CARICO LE NOTE PREVISTE
036760051216      *------------------------------------------------------------------------*
036770051216     c     Sr_Carnote    BegSr
036780051216
036790051216     c                   Eval      *In24 = *Off
036800051216     c                   Eval      *In26 = *Off
036810051216     c                   Eval      *In31 = *Off
036820051216     c                   Eval      *In21 = *Off
036830051216     c                   Eval      *In13 = *Off
036840051216     c                   Eval      *In15 = *Off
036850051216     c                   Eval      *In18 = *Off
036860060208     c                   Eval      *In34 = *Off
036870060208     c                   Eval      *In36 = *Off
036880090612     c                   Eval      *In40 = *Off
036890100729     c                   Eval      *In68 = *Off
036900051216
036910091112     c   70
036920091112     corn06              Eval      kntcapl = 'C'
036930100806     c   06
036940091112     cann70              Eval      kntcapl = 'T'
036950051216     c                   Movel(p)  dutkci        kntcnk1
036960091112     c   70
036970091112     corn06              Move      v1cksc        kntcnk1
036980100806     c   06
036990100806     cann70              Move      v1cnrv        kntcnk1
037000051216     c                   Clear                   kntcnk2
037010091119
037020091119      * Nota DC
037030091119     c                   clear                   v2ntcdc
037040091119     c                   Movel     'DC'          kntctnt
037050091119     c     kTfntc        Chain(n)  Tfntc01l
037060091119     c                   If        %Found(Tfntc01l)
037070091119     c                   Eval      v2ntcdc = ntcrnt
037080091119     c                   Eval      savntcdc = ntcrnt
037090091119     c                   EndIf
037100091119
037110140722      * Note 02/03
037120140722     c                   exsr      sr_CarNoteRN
037130051216
037140140722      * Note 05/06/T5/I5
037150140722     c                   exsr      sr_CarNoteRT
037160051216
037170140722      * Note 07/08/T7/I7
037180140722     c                   exsr      sr_CarNoteRL
037190051216
037200051216      * Nota 84
037210051216     c                   Movel     '84'          kntctnt
037220091119     c     kTfntc        Chain(n)  Tfntc01l
037230051216     c                   If        %Found(Tfntc01l)
037240051216     c                   Eval      *In21 = *On
037250051216     c                   EndIf
037260051216
037270051216      * Nota 85
037280051216     c                   Movel     '85'          kntctnt
037290091119     c     kTfntc        Chain(n)  Tfntc01l
037300051216     c                   If        %Found(Tfntc01l)
037310051216     c                   Eval      *In13 = *On
037320051216     c                   EndIf
037330051216
037340051216      * Nota 88
037350051216     c                   Movel     '88'          kntctnt
037360091119     c     kTfntc        Chain(n)  Tfntc01l
037370051216     c                   If        %Found(Tfntc01l)
037380051216     c                   Eval      *In15 = *On
037390051216     c                   EndIf
037400051216
037410051216      * Nota 87
037420051216     c                   Movel     '87'          kntctnt
037430091119     c     kTfntc        Chain(n)  Tfntc01l
037440051216     c                   If        %Found(Tfntc01l)
037450051216     c                   Eval      *In18 = *On
037460051216     c                   EndIf
037470060208
037480060208      * Nota 83
037490060208     c                   Movel     '83'          kntctnt
037500091119     c     kTfntc        Chain(n)  Tfntc01l
037510060208     c                   If        %Found(Tfntc01l)
037520060208     c                   Eval      *In34 = *On
037530060208     c                   EndIf
037540060208
037550060208      * Nota 86
037560060208     c                   Movel     '86'          kntctnt
037570091119     c     kTfntc        Chain(n)  Tfntc01l
037580060208     c                   If        %Found(Tfntc01l)
037590060208     c                   Eval      *In36 = *On
037600060208     c                   EndIf
037610090612
037620090612      * Nota 89
037630090612     c                   Movel     '89'          kntctnt
037640091119     c     kTfntc        Chain(n)  Tfntc01l
037650090612     c                   If        %Found(Tfntc01l)
037660090612     c                   Eval      *In40 = *On
037670090612     c                   EndIf
037680100729
037690100729      * Nota 82
037700100729     c                   Movel     '82'          kntctnt
037710100729     c     kTfntc        Chain(n)  Tfntc01l
037720100729     c                   If        %Found(Tfntc01l)
037730100729     c                   Eval      *In68 = *On
037740100729     c                   EndIf
037750051216
037760051216      * Decodifico le note caricate
037770051216     c                   ExSr      Sr_Decnote
037780051216
037790051216     c                   EndSr
037800140722
037810140722      *------------------------------------------------------------------------*
037820140722      * CARICO LE NOTE 02/03 (NEWS NOMINATIVO)
037830140722      *------------------------------------------------------------------------*
037840140722     c     Sr_CarNoteRN  BegSr
037850140722      *
037860140722      * Nota 02
037870140722     c                   movel     '02'          kNtcTnt
037880140722     c     kTfntc        chain(n)  TFNTC
037890140722     c                   if        %found(TFNTC01L)
037900140722     c                   eval      *in24 = *on
037910140722     c                   endif
037920140722      *
037930140722      * Nota 03
037940140722     c                   Movel     '03'          kNtcTnt
037950140722     c     kTfntc        chain(n)  TFNTC
037960140722     c                   if        %found(TFNTC01L)
037970140722     c                   eval      *in24 = *on
037980140722     c                   endif
037990140722      *
038000140722     c                   EndSr
038010140722
038020140722      *------------------------------------------------------------------------*
038030140722      * CARICO LE NOTE 05/06/T5/I5 (RESPONSABILE TRASPORTI)
038040140722      *------------------------------------------------------------------------*
038050140722     c     Sr_CarNoteRT  BegSr
038060140722      *
038070140722      * Nota 05
038080140722     c                   movel     '05'          kNtcTnt
038090140722     c     kTfntc        chain(n)  TFNTC
038100140722     c                   if        %found(TFNTC01L)
038110140722     c                   eval      *in26 = *On
038120140722     c                   endif
038130140722      *
038140140722      * Nota 06
038150140722     c                   movel     '06'          kNtcTnt
038160140722     c     kTfntc        chain(n)  TFNTC
038170140722     c                   if        %found(TFNTC01L)
038180140722     c                   eval      *in26 = *On
038190140722     c                   endif
038200140722      *
038210140722      * Nota T5
038220140722     c                   movel     'T5'          kNtcTnt
038230140722     c     kTfntc        chain(n)  TFNTC
038240140722     c                   if        %found(TFNTC01L)
038250140722     c                   eval      *in26 = *On
038260140722     c                   endif
038270140722      *
038280140722      * Nota I5
038290140722     c                   movel     'I5'          kNtcTnt
038300140722     c     kTfntc        chain(n)  TFNTC
038310140722     c                   if        %found(TFNTC01L)
038320140722     c                   eval      *in26 = *On
038330140722     c                   endif
038340140722      *
038350140722     c                   EndSr
038360140722
038370140722      *------------------------------------------------------------------------*
038380140722      * CARICO LE NOTE 07/08/T7/I7 (RESPONSABILE LOGISTICA)
038390140722      *------------------------------------------------------------------------*
038400140722     c     Sr_CarNoteRL  BegSr
038410140722      *
038420140722      * Nota 07
038430140722     c                   movel     '07'          kNtcTnt
038440140722     c     kTfntc        chain(n)  TFNTC
038450140722     c                   if        %found(TFNTC01L)
038460140722     c                   eval      *in31 = *On
038470140722     c                   endif
038480140722      *
038490140722      * Nota 08
038500140722     c                   movel     '08'          kNtcTnt
038510140722     c     kTfntc        chain(n)  TFNTC
038520140722     c                   if        %found(TFNTC01L)
038530140722     c                   eval      *in31 = *On
038540140722     c                   endif
038550140722      *
038560140722      * Nota T7
038570140722     c                   movel     'T7'          kNtcTnt
038580140722     c     kTfntc        chain(n)  TFNTC
038590140722     c                   if        %found(TFNTC01L)
038600140722     c                   eval      *in31 = *On
038610140722     c                   endif
038620140722      *
038630140722      * Nota I5
038640140722     c                   movel     'I7'          kNtcTnt
038650140722     c     kTfntc        chain(n)  TFNTC
038660140722     c                   if        %found(TFNTC01L)
038670140722     c                   eval      *in31 = *On
038680140722     c                   endif
038690140722      *
038700140722     c                   EndSr
038710051216
038720051216      *------------------------------------------------------------------------*
038730051216      * DECODIFICO LE NOTE CARICATE
038740051216      *------------------------------------------------------------------------*
038750051216     c     Sr_Decnote    BegSr
038760051216
038770051216     c                   Clear                   v4dnt84
038780051216     c                   Clear                   v4cnt84
038790051216     c                   Clear                   v5dnt85
038800051216     c                   Clear                   v5cnt85
038810090612     c                   Clear                   v5dnt89
038820090612     c                   Clear                   v5cnt89
038830100729     c                   Clear                   v5cnt82
038840051216     c                   Clear                   v6dnt88
038850051216     c                   Clear                   v6cnt88
038860051216     c                   Clear                   v7dnt87
038870051216     c                   Clear                   v7cnt87
038880060208     c                   Clear                   v7dnt83
038890060208     c                   Clear                   v7cnt83
038900060208     c                   Clear                   v7dnt86
038910060208     c                   Clear                   v7cnt86
038920140722
038930140722     c                   Eval      kcod = '1T'
038940051216
038950051216      * Decodifico nota 84
038960051216     c                   Eval      kkey = '84'
038970051216     c     kTabel        Chain     Tabel00f
038980051216     c                   If        %Found(Tabel00f) and
038990051216     c                             tblflg = *Blanks
039000051216     c                   Eval      ds1t = tbluni
039010051216     c                   EndIf
039020051216      * Imposto il campo di descrizione
039030060203     c                   Eval      v4dnt84 = ' 84 - ' + §1tdes
039040051216
039050051216      * Decodifico nota 85
039060051216     c                   Eval      kkey = '85'
039070051216     c     kTabel        Chain     Tabel00f
039080051216     c                   If        %Found(Tabel00f) and
039090051216     c                             tblflg = *Blanks
039100051216     c                   Eval      ds1t = tbluni
039110051216     c                   EndIf
039120051216      * Imposto il campo di descrizione
039130060203     c                   Eval      v5dnt85 = ' 85 - ' + §1tdes
039140090612
039150090612      * Decodifico nota 89
039160090612     c                   Eval      kkey = '89'
039170090612     c     kTabel        Chain     Tabel00f
039180090612     c                   If        %Found(Tabel00f) and
039190090612     c                             tblflg = *Blanks
039200090612     c                   Eval      ds1t = tbluni
039210090612     c                   EndIf
039220090612      * Imposto il campo di descrizione
039230090612     c                   Eval      v5dnt89 = ' 89 - ' + §1tdes
039240100729
039250100729      * Decodifico nota 82
039260100729     c                   Eval      kkey = '82'
039270100729     c     kTabel        Chain     Tabel00f
039280100729     c                   If        %Found(Tabel00f) and
039290100729     c                             tblflg = *Blanks
039300100729     c                   Eval      ds1t = tbluni
039310100729     c                   EndIf
039320100729      * Imposto il campo di descrizione
039330100729     c                   Eval      v5dnt82 = ' 82 - ' + §1tdes
039340051216
039350051216      * Decodifico nota 88
039360051216     c                   Eval      kkey = '88'
039370051216     c     kTabel        Chain     Tabel00f
039380051216     c                   If        %Found(Tabel00f) and
039390051216     c                             tblflg = *Blanks
039400051216     c                   Eval      ds1t = tbluni
039410051216     c                   EndIf
039420051216      * Imposto il campo di descrizione
039430060203     c                   Eval      v6dnt88 = ' 88 - ' + §1tdes
039440051216
039450051216      * Decodifico nota 87
039460051216     c                   Eval      kkey = '87'
039470051216     c     kTabel        Chain     Tabel00f
039480051216     c                   If        %Found(Tabel00f) and
039490051216     c                             tblflg = *Blanks
039500051216     c                   Eval      ds1t = tbluni
039510051216     c                   EndIf
039520051216      * Imposto il campo di descrizione
039530060203     c                   Eval      v7dnt87 = ' 87 - ' + §1tdes
039540060208
039550060208      * Decodifico nota 83
039560060208     c                   Eval      kkey = '83'
039570060208     c     kTabel        Chain     Tabel00f
039580060208     c                   If        %Found(Tabel00f) and
039590060208     c                             tblflg = *Blanks
039600060208     c                   Eval      ds1t = tbluni
039610060208     c                   EndIf
039620060208      * Imposto il campo di descrizione
039630060208     c                   Eval      v7dnt83 = ' 83 - ' + §1tdes
039640060208
039650060208      * Decodifico nota 86
039660060208     c                   Eval      kkey = '86'
039670060208     c     kTabel        Chain     Tabel00f
039680060208     c                   If        %Found(Tabel00f) and
039690060208     c                             tblflg = *Blanks
039700060208     c                   Eval      ds1t = tbluni
039710060208     c                   EndIf
039720060208      * Imposto il campo di descrizione
039730060208     c                   Eval      v7dnt86 = ' 86 - ' + §1tdes
039740051216
039750051216     c                   EndSr
039760051216
039770051216      *------------------------------------------------------------------------*
039780051216      *  Imposto descrizione tasti funzione
039790051216      *------------------------------------------------------------------------*
039800051216     c     Sr_Tastifun   BegSr
039810051216
039820051216      * Conta i caratteri riempiti dalle RigaTf1
039830051216     c                   Clear                   $z
039840051216      * Conta i caratteri riempiti dalle RigaTf2
039850051216     c                   Clear                   $k
039860051216      * Conta i caratteri riempiti dalle RigaTf3
039870051216     c                   Clear                   $j
039880051216      * Conta la posizione del campone da cui partire per impostare
039890051216      * la descrizione del tasto funzione
039900051216     c                   Clear                   $y
039910051216
039920051216      * Stringhe che contengono le descrizioni dei tasti funzione
039930051216     c                   Reset                   rigatf1
039940051216     c                   Reset                   rigatf2
039950051216     c                   Reset                   rigatf3
039960060203
039970060731      * F4 - Abilitazioni
039980120117       //?Se no angrafica provvisoria e no immissione
039990111007     c                   eval      *in78 = *off
040000110907     c                   if        not *in01 and not *in06
040010111117     c                   eval      *in78 = *on
040020060731     c                   reset                   $tf
040030060731     c                   movea     tf04          $tf
040040060731     c                   exsr      rie_$tf
040050060731     c                   endif
040060100407
040070100524       //?F22-Richiesta Contatto
040080100806       //?Se anagrafica clienti abilito F22=Richiesta Contatto
040090100407       //?e se utente di filiale
040100100419       //?e se pgm da menù
040110100507       //?e se NON immissione
040120100806     c                   IF        not *in06 and
040130100407     c                             not *in07 and
040140100419     c                             not *in09 and
040150100507     c                             not *in01 and
040160100419     c                             not $pgmrich
040170100407     c                   reset                   $tf
040180100524     c                   Movea     tf22          $tf
040190100407     c                   ExSr      Rie_$tf
040200100407     c                   ENDIF
040210100507
040220100507       //?F5 - Attività
040230120124       //?se utente di sede solo in interrogazione
040240120124       //?se non sono in anagrafica provvisoria,
040250100507       //?se non è convalida offerta e se ci sono attività sul cliente
040260100507       //?e non sono in immissione
040270100507     c                   eval      *in67 = *off
040280100806     c                   IF        not *in06 and
040290100507     c                             not *in07 and
040300120124     c                             not *in01
040310100507      * cerco se ci sono attività sul cliente
040320110516     c     V1Cksc        setll     TIATC07L
040330110516     c                   IF        %equal(TIATC07L)
040340100507     c                   eval      *in67 = *on
040350120124      * se utente di sede e NON è interrogazione spengo F5
040360120124     c                   IF        *in09 and not *in05
040370120124     c                   eval      *in67 = *off
040380120124     c                   ENDIF
040390120124     c                   IF        *in67
040400100507     c                   reset                   $tf
040410100507     c                   movea     tf05          $tf
040420100507     c                   exsr      rie_$tf
040430120124     c                   ENDIF
040440100507     c                   ENDIF
040450100507     c                   ENDIF
040460100507
040470100507      * F15 - Codici collegati
040480100507     c                   If        Not *In01 and Not *In06
040490100507     c                   Reset                   $tf
040500100507     c                   Movea     tf15          $tf
040510100507     c                   ExSr      Rie_$tf
040520100507     c                   EndIf
040530100507
040540060731      * F14 - Info Commerciali
040550100806     c                   If        Not *In06
040560060731     c                   Reset                   $tf
040570060731     c                   Movea     tf14          $tf
040580060731     c                   ExSr      Rie_$tf
040590090914     c                   endif
040600060526      * F19 - Variazioni
040610060526     c                   If        Not *In01 and Not *In06
040620060526     c                   Reset                   $tf
040630060526     c                   Movea     tf19          $tf
040640060526     c                   ExSr      Rie_$tf
040650060526     c                   endif
040660100407      * F20 - Interrogazione clienti potenziali
040670100407     c                   If        *In29 = *On and not *in65 and
040680100806     c                             not *in06
040690100407     c                   Reset                   $tf
040700100407     c                   Movea     tf20          $tf
040710100407     c                   ExSr      Rie_$tf
040720100407     c                   EndIf
040730101213
040740101213       //?F17-Dati Fatturazione
040750110322     c                   IF        not *in06
040760101213     c                   reset                   $tf
040770101213     c                   Movea     tf17          $tf
040780101213     c                   ExSr      Rie_$tf
040790101213     c                   ENDIF
040800110223
040810110223       //?F21-Blocco cliente
040820110223       //?Abilito se anagrafica clienti
040830110223       //?se utente di filiale
040840110223       //?se pgm da menù
040850110223       //?se NON immissione
040860110223       //?se codice non bloccato
040870110223     c                   IF        not *in06 and
040880110223     c                             not *in07 and
040890110223     c                             not *in09 and
040900110223     c                             not *in01 and
040910110223     c                             v2cabl = *blanks and
040920110223     c                             not $pgmrich
040930110223     c                   reset                   $tf
040940110223     c                   Movea     tf21          $tf
040950110223     c                   ExSr      Rie_$tf
040960110223     c                   ENDIF
040970110223
040980060203      * F12 - Ritorno
040990060206     c                   If        (Not *In29 and *In06) or
041000060206     c                             (Not *In29 and *In07) or
041010060206     c                             (Not *In06 and Not *In07)
041020060203     c                   Reset                   $tf
041030060203     c                   Movea     tf12          $tf
041040060203     c                   ExSr      Rie_$tf
041050060206     c                   EndIf
041060051216
041070051216      * Pulisco la stringa
041080051216     c                   Eval      $h = 1
041090051216     c                   For       $h by 1 to 62
041100051216     c                   If        rigatf1($h) = '#'
041110051216     c                   Clear                   rigatf1($h)
041120051216     c                   EndIf
041130051216     c                   EndFor
041140051216     c                   Eval      $h = 1
041150051216     c                   For       $h by 1 to 62
041160051216     c                   If        rigatf2($h) = '#'
041170051216     c                   Clear                   rigatf2($h)
041180051216     c                   EndIf
041190051216     c                   EndFor
041200051216     c                   Eval      $h = 1
041210051216     c                   For       $h by 1 to 62
041220051216     c                   If        rigatf3($h) = '#'
041230051216     c                   Clear                   rigatf3($h)
041240051216     c                   EndIf
041250051216     c                   EndFor
041260051216
041270051216      * Imposto la descrizione del tasto funzione F24 e segnalo
041280051216      * quante righe ho riempito e quale devo emettere
041290051216     c                   Select
041300051216
041310051216     c                   When      $k <> *Zeros and $y <> *Zeros
041320051216     c                   Eval      $loop = 3
041330051216     c                   Eval      vzfd02 = cf2413
041340051216
041350051216     c                   When      $k <> *Zeros and $y = *Zeros
041360051216     c                   Eval      $loop = 2
041370051216     c                   Eval      vzfd02 = cf2412
041380051216
041390051216     c                   Other
041400051216
041410051216     c                   Eval      $loop = 1
041420051216     c                   Clear                   vzfd02
041430051216     c                   EndSl
041440051216
041450051216      * Prima riga di tasti funzione
041460051216     c                   Movea     rigatf1       vzfd01
041470051216     c                   Eval      $rig = 1
041480051216
041490051216     c                   EndSr
041500051216
041510051216      *------------------------------------------------------------------------*
041520051216      *  IMPOSTO LE STRINGHE CON I CAMPI FUNZIONE COMPATTATI
041530051216      *------------------------------------------------------------------------*
041540051216     c     Rie_$tf       BegSr
041550051216      *
041560051216     c                   Eval      $x = 1
041570051216     c     '#'           Lookup    $tf($x)                                30
041580051216     c                   Add       $x            $z
041590051216     c                   If        $Z <= 63
041600051216     c                   Eval      $j = $Z - $x + 1
041610051216     c                   Movea     $tf           rigatf1($j)
041620051216     c                   Else
041630051216     c                   Add       $x            $k
041640051216     c                   If        $k <= 63
041650051216     c                   Eval      $j = $K - $x + 1
041660051216     c                   Movea     $tf           rigatf2($j)
041670051216     c                   Else
041680051216     c                   Add       $x            $y
041690051216     c                   If        $y <= 63
041700051216     c                   Eval      $j = $y - $x + 1
041710051216     c                   Movea     $tf           rigatf3($j)
041720051216     c                   EndIf
041730051216     c                   EndIf
041740051216     c                   EndIf
041750051216
041760051216     c                   EndSr
041770051216
041780051108      *------------------------------------------------------------------------*
041790051130      * CONTROLLO SECONDA VIDEATA - DATI ANAGRAFICI/BLOCCO
041800051108      *------------------------------------------------------------------------*
041810051108     c     Sr_Ctrd02     BegSr
041820051117
041830051117      * Pulisco i campi del posizionamento cursore
041840051117     c                   Clear                   h2riga
041850051117     c                   Clear                   h2colo
041860051125
041870051118      * Imposto gli ulti 4 byte del codice cliente
041880051118     c                   Move      v1cksc        wksc04
041890051117
041900051117      * RAGIONE SOCIALE
041910051117      * --> Obbligatoria
041920051117     c                   If        v2crag = *Blanks
041930051117     c                   Eval      *In28 = *On
041940051130     c                   Eval      h2riga = 07
041950051117     c                   Eval      h2colo = 28
041960051117     c                   Eval      v2cmsg = msg(11)
041970051117     c                   Leavesr
041980051117     c                   EndIf
041990051117
042000051117      * CODICE POTENZIALE
042010051125     c                   ExSr      Sr_Ctrcpo
042020051125     c   28              Leavesr
042030051118
042040051118      * --> Caratteri non validi
042050051212     c                   Eval      xx = 1
042060051212     c                   For       xx by 1 to 10
042070051212     c                   If        car(xx) <> *Blanks
042080051212     c                   Eval      wpos = %scan(car(xx):v2crag)
042090051212     c                   If        wpos > 0
042100051212     c                   Eval      *In28 = *On
042110051212     c                   Eval      h2riga = 07
042120051212     c                   Eval      h2colo = 28
042130051212     c                   Eval      v2cmsg = msg(32)
042140051212     c                   Leavesr
042150051212     c                   EndIf
042160051212     c                   EndIf
042170051212     c                   EndFor
042180051118
042190051117      * INDIRIZZO
042200051118      * --> Controllo se cliente
042210051118     c                   If        v1ckcc = 151
042220051118     c                   Clear                   fnlv13ds
042230051118     c                   Eval      i14rsc = v2crag
042240051118     c                   Eval      i14ind = v2cvia
042250051118     c                   Eval      e14cap = v2ccae
042260051118     c                   Eval      e14loc = v2ccit
042270051118     c                   Eval      e14prv = v2cprv
042280051118     c                   Eval      e14nar = v2csta
042290051118     c                   ExSr      Sr_Ctrind
042300051118     c                   Eval      v2ccae = e14cap
042310051118     c                   Eval      v2ccit = e14loc
042320051118     c                   Eval      v2cprv = e14prv
042330051118     c                   Eval      v2csta = e14nar
042340051118     c   28              Leavesr
042350051118     c                   EndIf
042360051117
042370051117      * TELEFONO
042380051117      * --> Controllo (forzabile)
042390051124      *     Solo se variato il campo a video quindi al primo errore se non
042400051124      *     vario il campo non controllo più
042410051118     c                   If        v2ctel <> wtel and v2ctel <> *Blanks
042420051118     c                   Clear                   trul42ds
042430051118     c                   Eval      d42fax = v2ctel
042440051118     c                   Call      'TRUL42R'
042450051118     c                   Parm                    trul42ds
042460051118     c                   If        d42err = '1'
042470051118     c                   Eval      *In28 = *On
042480051202     c                   Eval      h2riga = 11
042490051118     c                   Eval      h2colo = 28
042500051118     c                   Eval      v2cmsg = d42msg
042510051118     c                   Leavesr
042520051118     c                   Eval      wtel = v2ctel
042530051117     c                   EndIf
042540051118     c                   EndIf
042550051118
042560051117      * FAX
042570051125      * --> Controllo (forzabile)
042580051125      *     Solo se variato il campo a video quindi al primo errore se non
042590051125      *     vario il campo non controllo più
042600051118     c                   If        v2ctlf <> wtlf and v2ctlf <> *Blanks
042610051118     c                   Clear                   trul42ds
042620051118     c                   Eval      d42fax = v2ctlf
042630051118     c                   Call      'TRUL42R'
042640051118     c                   Parm                    trul42ds
042650051118     c                   If        d42err = '1'
042660051118     c                   Eval      *In28 = *On
042670051202     c                   Eval      h2riga = 11
042680051118     c                   Eval      h2colo = 52
042690051118     c                   Eval      v2cmsg = d42msg
042700051118     c                   Leavesr
042710051118     c                   Eval      wtlf = v2ctlf
042720051118     c                   EndIf
042730051118     c                   EndIf
042740051118
042750051118      * CODICE FISCALE
042760051118     c                   ExSr      Sr_Ctrcdf
042770051118     c   28              Leavesr
042780051118
042790051118      * PARTITA IVA + STATO
042800051118     c                   ExSr      Sr_Ctriva
042810051118     c   28              Leavesr
042820171213      //se dal controllo torna che cliente già presente con stessa p.IVA
042830171213      //ma in sofferenza esco
042840171213       IF  ClienteSofferenza;
042850171213         leavesr;
042860171213       ENDIF;
042870051122
042880051122      * BLOCCO CLIENTE
042890051122     c                   ExSr      Sr_Ctrblc
042900051122     c   28              Leavesr
042910090616      * sollecito
042920090616     c                   ExSr      Sr_Ctrsol
042930090617     c   28              Leavesr
042940051108
042950051108     c                   EndSr
042960051130
042970051130      *------------------------------------------------------------------------*
042980051130      * CONTROLLO TERZA VIDEATA - DATI COMMERCIALI
042990051130      *------------------------------------------------------------------------*
043000051130     c     Sr_Ctrd03     BegSr
043010051130
043020051130      * Pulisco i campi del posizionamento cursore
043030051130     c                   Clear                   h3riga
043040051130     c                   Clear                   h3colo
043050051130
043060051130      * CODICE COMMERCIALE
043070051130     c                   ExSr      Sr_Ctrage
043080051130     c   28              Leavesr
043090051130
043100051130      * CODICE IMPORTANZA CLIENTE
043110051130     c                   ExSr      Sr_Ctrclv
043120051130     c   28              Leavesr
043130051130
043140051130      * CODICE ISTAT
043150051130     c                   ExSr      Sr_Ctritc
043160051130     c   28              Leavesr
043170051207
043180140722      * NOTE   02/03 - NEWS NOMINATIVO
043190140722     c                   eval      wRGR = 'RN'
043200140722     c                   exsr      sr_CtrNote
043210140722     c   28
043220140722     cor 90              leavesr
043230051207
043240140722      * NOTE   05/06/T5/I5 - RESPONSABILE TRASPORTI
043250140722     c                   eval      wRGR = 'RT'
043260140722     c                   exsr      sr_CtrNote
043270140722     c   28
043280140722     cor 90              leavesr
043290051207
043300140722      * NOTE   07/08/T7/I7 - RESPONSABILE LOGISTICA
043310140722     c                   eval      wRGR = 'RL'
043320140722     c                   exsr      sr_CtrNote
043330140722     c   28
043340140722     cor 90              leavesr
043350051130
043360051130     c                   EndSr
043370051108
043380051108      *------------------------------------------------------------------------*
043390051130      * CONTROLLO QUARTA VIDEATA - DATI FATTURAZIONE
043400051108      *------------------------------------------------------------------------*
043410051130     c     Sr_Ctrd04     BegSr
043420051125
043430051125      * Pulisco i campi del posizionamento cursore
043440051130     c                   Clear                   h4riga
043450051130     c                   Clear                   h4colo
043460051125
043470051125      * CODICE SOTTOCONTO FATTURAZIONE
043480051125     c                   ExSr      Sr_Ctrscf
043490100127     c   90
043500100127     cor 28              Leavesr
043510051128
043520051128      * FLAG FATTURA UNIFICATA
043530051128     c                   ExSr      Sr_Ctruni
043540051128     c   28              Leavesr
043550051125
043560051125      * TIPO FATTURA
043570051125     c                   ExSr      Sr_Ctrtft
043580051125     c   28              Leavesr
043590051125
043600051125      * FREQUENZA FATTURA
043610051125     c                   ExSr      Sr_Ctrfft
043620051125     c   28              Leavesr
043630051125
043640051125      * TIPO DATA FATTURA
043650051125     c                   ExSr      Sr_Ctrtdf
043660051125     c   28              Leavesr
043670051128
043680051128      * SPESE INVIO FATTURA
043690051128     c                   ExSr      Sr_Ctrsin
043700051128     c   28              Leavesr
043710100728
043720100728      * CODICE PAGAMENTO FATTURA
043730100728     c                   ExSr      Sr_Ctrcdp
043740100728     c   28              Leavesr
043750100728
043760100728      * BANCA APPOGGIO
043770100728     c                   ExSr      Sr_Ctrbna
043780100728     c   28              Leavesr
043790100728
043800100728      * ABI/CAB
043810100728     c                   ExSr      Sr_Ctrabicabf
043820100728     c   28              Leavesr
043830051128
043840051206      * LUOGO 001 - INVIO FATTURA
043850051212      * Se non sono in visite/offerte
043860051128     c                   If        Not *In06
043870091112      * o se trattativa da cliente
043880091112     c                             or *in70
043890051128     c                   ExSr      Sr_Ctrl001
043900051128     c   28              Leavesr
043910051128     c                   EndIf
043920051129
043930051206      * NOTA   84 - INVIO FATTURA
043940051129     c                   ExSr      Sr_Ctrnt84
043950051129     c   28              Leavesr
043960051108
043970051108     c                   EndSr
043980051216
043990051108      *------------------------------------------------------------------------*
044000051130      * CONTROLLO QUINTA VIDEATA - CONDIZIONI PAGAMENTO/PAG. CONTRASSEGNI
044010051108      *------------------------------------------------------------------------*
044020051130     c     Sr_Ctrd05     BegSr
044030051129
044040051129      * Pulisco i campi del posizionamento cursore
044050051130     c                   Clear                   h5riga
044060051130     c                   Clear                   h5colo
044070100728     c                   clear                   wbanca
044080100728     c                   clear                   wagenzia
044090100728
044100100728      * BLOCCO PAGAMENTI
044110130322      * Se impostato SI dall'utente
044120130322     c                   IF        not *in81 and V5Cblp = 'SI'
044130130325     c                   eval      wbloc = 'B'
044140130322     c                   ENDIF
044150130322      * Se impostato NO dall'utente
044160130322     c                   IF        not *in81 and V5Cblp = 'NO'
044170130325     c                   eval      wbloc = '0'
044180130322     c                   ENDIF
044190150424      /free
044200150424       //?Come prima cosa devo avvisare se anche solo 1 dei 3 pagamenti
044210150424       //?non è un pagamento tramite Bonifico
044220150424         exsr CtrPagamenti;
044230150424         IF  *in28;
044240150424           leavesr;
044250150424         ENDIF;
044260150424      /end-free
044270051108
044280051130      * CODICE PAGAMENTO CONTRASSEGNI
044290051130     c                   ExSr      Sr_Ctrfpc
044300060215     c                   If        *In28
044310060215     c                   Eval      *In22 = savin22
044320060215     c                   Eval      *In23 = savin23
044330060215     c                   Leavesr
044340060215     c                   EndIf
044350100729     c                   IF        *in90 and $Win
044360100729     c                   leavesr
044370100729     c                   ENDIF
044380150424
044390150424      /free
044400150427       //?Memorizzo se pagamento tramite Bonifico
044410150424         IF  §4Ucod = 'B';
044420150424           BonificoCA = *on;
044430150424         ELSE;
044440150424           BonificoCA = *off;
044450150424         ENDIF;
044460150427       //?Memorizzo se pagamento Estero
044470150427         IF  §4Utip = 'E';
044480150427           PagEsteroCA = *on;
044490150427         ELSE;
044500150427           PagEsteroCA = *off;
044510150427         ENDIF;
044520150427       //?Memorizzo il codice pagamento
044530150427         CodPagCA = V5Cfpc;
044540150424      /end-free
044550150424
044560080115      * CODICE IBAN
044570080115      * solo se richieste obbligatorie le coordinate
044580080115     c                   if        not *in22 and not *in23
044590100729     c                   eval      wiban = v5ciban
044600080115     c                   exsr      sr_ctriban
044610100729     c                   if        *in28
044620100729     c                   eval      h5riga = 10
044630100804     c                   eval      h5colo = 29
044640100729     c                   leavesr
044650100729     c                   endif
044660100729     c                   eval      v5cbab = wdesbanca
044670080115     c                   endif
044680100729
044690100729      /free
044700100729       //?PAGAMENTO DANNI
044710100729         exsr CtrPagDN;
044720100729         IF  *in28;
044730100729           *in71 = savin71;
044740100729           *in72 = savin72;
044750100729           leavesr;
044760100729         ENDIF;
044770150427       //?Memorizzo se pagamento tramite Bonifico
044780150424         IF  §4Ucod = 'B';
044790150424           BonificoDN = *on;
044800150424         ELSE;
044810150424           BonificoDN = *off;
044820150424         ENDIF;
044830150427       //?Memorizzo se pagamento Estero
044840150427         IF  §4Utip = 'E';
044850150427           PagEsteroDN = *on;
044860150427         ELSE;
044870150427           PagEsteroDN = *off;
044880150427         ENDIF;
044890150427       //?Memorizzo il codice pagamento
044900150427         CodPagDN = V5tpdn;
044910100729       //?se window per invio dati alla sede ritorno ad emettere la videata
044920100729       //?senza andare avanti con i controlli
044930100729         IF  $windn and *in90;
044940100729           leavesr;
044950100729         ENDIF;
044960100729       //?controllo le coordinate bancarie
044970100730         IF  not *in71 and not *in72 and d§PAGctr = 'SI';
044980100729           wiban = v5ibandn;
044990100729           exsr sr_ctriban;
045000100729           IF  *in28;
045010100729             h5riga = 13;
045020100804             h5colo = 29;
045030100729             leavesr;
045040100729           ENDIF;
045050100729           v5babdn = wDesBanca;
045060100729         ENDIF;
045070100730       //?se pagamento tramite bonifico deve essere inserito la nota 82-mail
045080100730       //?bonifici danni
045090100730         IF §4ucod = 'B' and not *in68 and v5cnt82 = *blanks;
045100100730           *in28 = *on;
045110100730           v5cmsg = msg(86);
045120100730           h5riga = 21;
045130100730           h5colo = 71;
045140100730           leavesr;
045150100730         ENDIF;
045160100729
045170100729       //?PAGAMENTO NOTE ACCREDITO
045180100729         exsr CtrPagNA;
045190100729         IF  *in28;
045200100729           *in73 = savin73;
045210100729           *in74 = savin74;
045220100729           leavesr;
045230100729         ENDIF;
045240150427       //?Memorizzo se pagamento tramite Bonifico
045250150424         IF  §4Ucod = 'B';
045260150424           BonificoNA = *on;
045270150424         ELSE;
045280150424           BonificoNA = *off;
045290150424         ENDIF;
045300150427       //?Memorizzo se pagamento Estero
045310150427         IF  §4Utip = 'E';
045320150427           PagEsteroNA = *on;
045330150427         ELSE;
045340150427           PagEsteroNA = *off;
045350150427         ENDIF;
045360150427       //?Memorizzo il codice pagamento
045370150427         CodPagNA = V5tpna;
045380150424      /end-free
045390100729       //?se window per invio dati alla sede ritorno ad emettere la videata
045400100729       //?senza andare avanti con i controlli
045410100729         IF  $winna and *in90;
045420100729           leavesr;
045430100729         ENDIF;
045440100729       //?controllo le coordinate bancarie
045450100804         IF  not *in73 and not *in74 and d§PAGctr = 'SI';
045460100729           wiban = v5ibanna;
045470100729           exsr sr_ctriban;
045480100729           IF  *in28;
045490100729             h5riga = 16;
045500100804             h5colo = 29;
045510100729             leavesr;
045520100729           ENDIF;
045530100729           v5babna = wDesBanca;
045540100729         ENDIF;
045550100729      /end-free
045560051206
045570051212      * Se non sono in visite/offerte
045580051212if  1c                   If        Not *In06
045590091112      * o se trattativa da cliente
045600091112     c                             or *in70
045610051206      * LUOGO 555 - INTESTAZIONE PAGAMENTO C/ASSEGNI
045620051206     c                   ExSr      Sr_Ctrl555
045630051206     c   28              Leavesr
045640051206
045650051206      * LUOGO 666 - LUOGO INVIO BONIFICI C/ASSEGNI
045660051206     c                   ExSr      Sr_Ctrl666
045670051206     c   28              Leavesr
045680051212    1c                   EndIf
045690051206
045700051206      * NOTA   85 - E-MAIL BONIFICI C/ASSEGNI
045710051206     c                   ExSr      Sr_Ctrnt85
045720051206     c   28              Leavesr
045730090616
045740090616      * NOTA   89 - E-MAIL RESPONSABILE AMMINISTRATIVO
045750090616     c                   ExSr      Sr_Ctrnt89
045760090616     c   28              Leavesr
045770100729
045780100729      * NOTA   82 - E-MAIL BONIFICO DANNI
045790150424     c                   ExSr      Sr_Ctrnt82
045800100730     c   90              goto      emi05
045810100729     c   28              Leavesr
045820051130
045830051108     c                   EndSr
045840051108
045850051108      *------------------------------------------------------------------------*
045860051130      * CONTROLLO SESTA VIDEATA - GESTIONE GIACENZE
045870051108      *------------------------------------------------------------------------*
045880051130     c     Sr_Ctrd06     BegSr
045890051206
045900051206      * Pulisco i campi del posizionamento cursore
045910051206     c                   Clear                   h6riga
045920051206     c                   Clear                   h6colo
045930060216
045940060216      * LUOGO 005 - SPEDIZIONE GIACENZA
045950060216     c                   If        Not *In06
045960091112     c                             or *in70
045970060216     c                   ExSr      Sr_Ctrl005
045980060216     c   28              Leavesr
045990060216     c                   EndIf
046000060216
046010060216      * NOTA   88 - E-MAIL GIACENZE
046020060216     c                   ExSr      Sr_Ctrnt88
046030060216     c   28              Leavesr
046040060216
046050060216      * LUOGO 300 - MERCE RESA
046060060216     c                   If        Not *In06
046070091112     c                             or *in70
046080060216     c                   ExSr      Sr_Ctrl300
046090060216     c   28              Leavesr
046100060216     c                   EndIf
046110051206
046120051206      * TIPO COMUNICAZIONE AL MITTENTE
046130051206     c                   ExSr      Sr_Ctrtcm
046140051206     c   28              Leavesr
046150051206
046160051206      * TIPO COMUNICAZIONE FINE GIACENZA
046170051206     c                   ExSr      Sr_Ctrtcf
046180051206     c   28              Leavesr
046190051206
046200051206      * APERTURA AUTOMATICA GIACENZA
046210051206     c                   ExSr      Sr_Ctrapg
046220051206     c   28              Leavesr
046230051206
046240051206      * DISPOSIZIONE MITTENTE IN ARRIVO
046250051206     c                   ExSr      Sr_Ctrdmg
046260051206     c   28              Leavesr
046270060801
046280060801      * CHIUSURA AUTOMATICA GIACENZA
046290060801     c                   ExSr      Sr_Ctrcag
046300060801     c   28              Leavesr
046310051108
046320051108     c                   EndSr
046330051108
046340051108      *------------------------------------------------------------------------*
046350051130      * CONTROLLO SETTIMA VIDEATA - GESTIONE DANNI/TASSAZIONE P.A./STOP
046360051108      *------------------------------------------------------------------------*
046370051130     c     Sr_Ctrd07     BegSr
046380051213
046390051213      * Pulisco i campi del posizionamento cursore
046400051213     c                   Clear                   h7riga
046410051213     c                   Clear                   h7colo
046420150728
046430150728      /free
046440150728       //?Come prima cosa devo avvisare se manca la mail per invio
046450150728       //?progetto di liquidazione
046460150728       //?NON lo faccio se anagrafica provvisoria
046470150728         IF  not *in06;
046480150728           exsr CtrMailDanni;
046490150728           IF  *in28;
046500150728             leavesr;
046510150728           ENDIF;
046520150728         ENDIF;
046530150728      /end-free
046540051213
046550051213      * LUOGO 006 - LUOGO PREAVVISO/AVVISO DANNO
046560051213     c                   If        Not *In06
046570091112     c                             or *in70
046580051213     c                   ExSr      Sr_Ctrl006
046590051213     c   28              Leavesr
046600051213     c                   EndIf
046610051213
046620051213      * NOTA   87 - E-MAIL DANNI
046630051213     c                   ExSr      Sr_Ctrnt87
046640051213     c   28              Leavesr
046650051213
046660051213      * LUOGO 008 - PROGETTO DI LIQUIDAZIONE
046670051213     c                   If        Not *In06
046680091112     c                             or *in70
046690051213     c                   ExSr      Sr_Ctrl008
046700051213     c   28              Leavesr
046710051213     c                   EndIf
046720051213
046730051213      * NR. STOP RITIRO
046740051213     c                   ExSr      Sr_Ctrstp
046750051213     c   28              Leavesr
046760060208
046770060208      * NOTA   83 - MAIL ORM ESTERO
046780060208     c                   ExSr      Sr_Ctrnt83
046790060208     c   28              Leavesr
046800060208
046810060208      * LUOGO 002 - INVIO ORM ESTERO
046820060208     c                   If        Not *In06
046830091112     c                             or *in70
046840060208     c                   ExSr      Sr_Ctrl002
046850060208     c   28              Leavesr
046860060208     c                   EndIf
046870060208
046880060208      * NOTA   86 - MANIFEST
046890060208     c                   ExSr      Sr_Ctrnt86
046900060208     c   28              Leavesr
046910060208
046920060208      * LUOGO 200 - DOC. DOGANA
046930060208     c                   If        Not *In06
046940091112     c                             or *in70
046950060208     c                   ExSr      Sr_Ctrl200
046960060208     c   28              Leavesr
046970060208     c                   EndIf
046980051108
046990051108     c                   EndSr
047000051216
047010051216      *------------------------------------------------------------------------*
047020051216      * TASTO FUNZIONE F02 - CONTATTI
047030051216      *------------------------------------------------------------------------*
047040051216     c     Sr_F02        BegSr
047050051216
047060051216     c                   Clear                   tnta12ds
047070051216     c                   Eval      ta12ric = wrich
047080091112     c   70
047090091112     corn06              Eval      ta12apl = 'C'
047100091112     c   70
047110091112     corn06              Eval      ta12ksc = v1cksc
047120100806     c   06
047130091112     cann70              Eval      ta12apl = 'T'
047140100806     c   06
047150100806     cann70              Eval      ta12nrv = v1cnrv
047160060217     c                   Eval      ta12rag = wrag
047170051216     c                   Eval      ta12tnt = wtnt
047180060216     c                   Eval      ta12gcli = '1'
047190060216     c   01              Eval      ta12icli = '1'
047200060307     c   01              Move      v3cage        ta12cmm
047210060227     c                   If        %Parms > 1 and
047220060227     c                             Not *In01 and Not *In05 and ta60icli = '1'
047230060221     c                   Eval      ta12icli = '1'
047240060307     c                   Move      v3cage        ta12cmm
047250060221     c                   EndIf
047260051216     c                   Call      'TNTA12R'
047270051216     c                   Parm                    kpjba
047280051216     c                   Parm                    tnta12ds
047290051216
047300051216     c                   ExSr      Sr_Carnote
047310051216
047320051216     c                   EndSr
047330060217
047340060217      *------------------------------------------------------------------------*
047350060217      * TASTO FUNZIONE F03 - FINE LAVORO
047360060217      *------------------------------------------------------------------------*
047370060217     c     Sr_F03        BegSr
047380060217
047390060217     c                   Eval      *In80 = *Off
047400060217
047410060217      * Se immissione emetto videata per chiedere conferma di uscita dal
047420060217      * pgm informando l'utente che i dati andranno persi
047430060217     c                   If        *In01
047440060217     c                   Eval      w03cok = 'NO'
047450060217     c                   Exfmt     ta60w03
047460060217      * Se ok per uscire dal pgm vado a fine
047470060217if  1c                   If        w03cok = 'SI'
047480060217     c                   Goto      fine
047490060217     c                   Else
047500060217     c                   Eval      *In80 = *On
047510060217     c                   Leavesr
047520060217    1c                   EndIf
047530060217     c                   EndIf
047540110302      * Imposto F3 fine se pgm richiamato
047550110302     c                   IF        $Pgmrich
047560110301     c                   eval      TA60f03 = '1'
047570110302     c                   ENDIF
047580110301
047590060217      * Se arrivo qua non è immissione e quindi vado a fine
047600060217     c                   Goto      fine
047610060217
047620060217     c                   EndSr
047630060731
047640060731      *------------------------------------------------------------------------*
047650060731      * TASTO FUNZIONE F04 - ABILITAZIONI
047660060731      *------------------------------------------------------------------------*
047670060731     c     sr_f04        begsr
047680060731
047690060731     c                   clear                   tnta61ds
047700060731     c                   eval      ta61tla = 'V'
047710060731     c                   eval      ta61ksc = v1cksc
047720060731     c                   call      'TNTA61R'
047730060731     c                   parm                    kpjba
047740060731     c                   parm                    tnta61ds
047750060731
047760060731     c                   endsr
047770100507
047780100507      /free
047790100507       //--------------------------------------------------------------
047800100507       //?Tasto Funzione F5 - Attività.
047810100507       //--------------------------------------------------------------
047820100507       BEGSR sr_F05;
047830100507
047840100507         clear trmk21ds;
047850100507         I21mod = 'I';
047860100507         I21ksc = V1Cksc;
047870120124         I21rsc = V2Crag;
047880100507         kpjbu = trmk21ds;
047890100507
047900100507         trmk21r (kpjba);
047910100507
047920100507       ENDSR;
047930100507      /end-free
047940060731
047950051216      *------------------------------------------------------------------------*
047960051216      * TASTO FUNZIONE F07 - CLIENTE UNIFICANTE
047970051216      *------------------------------------------------------------------------*
047980051216     c     Sr_F07        BegSr
047990051216
048000051216     c                   Clear                   tibs12ds
048010051216     c  n05              Eval      k12op0 = 'P02'
048020051216     c   05              Eval      k12op0 = 'P05'
048030051216     c                   Eval      k12op1 = 'O09'
048040051216     c                   Eval      k12f03 = '0'
048050051216     c                   Eval      k12f12 = '0'
048060051216     c                   Eval      k12err = '0'
048070051216     c                   Move      v1cksc        k12cop
048080051216     c                   Eval      kpjbu = tibs12ds
048090051216     c                   Call      'TIBS12R'
048100051216     c                   Parm                    kpjba
048110051216
048120051216     c                   EndSr
048130100407
048140100407      /free
048150100407       //--------------------------------------------------------------
048160100524       //?Tasto Funzione F22 - Richiesta Contatto.
048170100407       //--------------------------------------------------------------
048180100524       BEGSR sr_F22;
048190110223
048200110223       //?Non posso fare una nuova attività su cliente con potenziale in
048210110223       //?categoria PERSO, devo partire dal potenziale
048220110223         IF  V2Hfls = 'P';
048230110223         *in28 = *on;
048240110223         V2Cmsg = 'Nuova attività non possibile per Potenziale PERSO';
048250110223         leavesr;
048260110223         ENDIF;
048270100407
048280100407         clear trmk17ds;
048290100407         IMK17patt = 'S';
048300100407         IMK17att  = 'T';
048310100407         IMK17pcau = 'S';
048320100407         IMK17cau  = '20';
048330100407         IMK17cpo  = v2ccpo;
048340100407         IMK17ksc  = v1cksc;
048350100407         IF  v3cage > *zeros;
048360100407           IMK17cmm  = %dec(v3cage:7:0);
048370100407         ENDIF;
048380100407
048390100407         trmk17r (kpjba : TRMK17DS);
048400100507
048410100507       //?Ricarico i tasti funzione, nel caso venga inserita l'attività sul
048420100507       //?cliente così faccio vedere il tasto F5-Attività
048430100507         exsr Sr_Tastifun;
048440100407
048450100407       ENDSR;
048460101213
048470101213       //--------------------------------------------------------------
048480101213       //?Tasto Funzione F17 - Dati Fatturazione.
048490101213       //--------------------------------------------------------------
048500101213       BEGSR sr_F17;
048510101213
048520101213         clear trul57ds;
048530101213         D57ikscb = V1Cksc;
048540101213         IF  %check(digits:V4Cscf) > 0;
048550101213           D57ikscf = 0;
048560101213         ELSE;
048570101213           D57ikscf = %dec(V4Cscf:7:0);
048580101213         ENDIF;
048590101213         D57iunib = V4Cuni;
048600101213         D57itftb = V4Ctft;
048610101213         D57ifftb = V4Cfft;
048620101213         D57idftb = V4Ctdf;
048630101213         D57ipgm  = 'TNTA60R';
048640101213         D57isifb = V4Csin;
048650101213         D57icdpb = V4Ccdp;
048660101213         D57iabib = V4Cabi;
048670101213         D57icabb = V4Ccab;
048680101213
048690101213         trul57r (kpjba : TRUL57DS);
048700101213
048710101213       ENDSR;
048720110223
048730110223       //--------------------------------------------------------------
048740110223       //?Tasto Funzione F21 - Blocco Cliente.
048750110223       //--------------------------------------------------------------
048760110223       BEGSR sr_F21;
048770110223
048780110223         clear trmk17ds;
048790110223         IMK17patt = 'S';
048800110223         IMK17att  = 'T';
048810110223         IMK17pcau = 'S';
048820110223         IMK17cau  = '71';
048830110223         IMK17cpo  = v2ccpo;
048840110223         IMK17ksc  = v1cksc;
048850110223         IF  v3cage > *zeros;
048860110223           IMK17cmm  = %dec(v3cage:7:0);
048870110223         ENDIF;
048880110223
048890110223         trmk17r (kpjba : TRMK17DS);
048900110223
048910110223       //?Ricarico i tasti funzione
048920110223         exsr Sr_Tastifun;
048930110310
048940110223
048950110223       ENDSR;
048960110223
048970100407      /end-free
048980051216
048990051216      *------------------------------------------------------------------------*
049000051216      * TASTO FUNZIONE F11 - LUOGHI
049010051216      *------------------------------------------------------------------------*
049020051216     c     Sr_F11        BegSr
049030051216
049040051216     c                   Clear                   parmf11
049050051216     c  n05              Eval      paropz = '2'
049060051216     c   05              Eval      paropz = '5'
049070051216     c                   Move      v1cksc        parcli
049080060216     c                   Eval      partnta60 = '1'
049090060216     c   01              Eval      parimta60 = '1'
049100051216     c                   Eval      kpjbu = parmf11
049110051216     c                   Call      'TNTA80R'
049120051216     c                   Parm                    kpjba
049130051216
049140051216     c                   ExSr      Sr_Carluoghi
049150051216
049160051216     c                   EndSr
049170051216
049180051216      *------------------------------------------------------------------------*
049190051216      * TASTO FUNZIONE F14 - INFO COMMERCIALI
049200051216      *------------------------------------------------------------------------*
049210051216     c     Sr_F14        BegSr
049220051216
049230081020     c                   Clear                   trmk50ds
049240081020     c                   Eval      i50pgm = 'TNTA60R'
049250081020     c                   Movel     v2ccpo        i50cpo
049260081020     c                   Eval      i50rag = wrag
049270110310     c                   IF        *in02 and not *in06 and not *in07
049280110310     c                   Eval      i50mod = 'G'
049290110310     c                   ELSE
049300110310     c                   Eval      i50mod = 'I'
049310110310     c                   ENDIF
049320081020     c                   Call      'TRMK50R'
049330051216     c                   Parm                    kpjba
049340081020     c                   Parm                    trmk50ds
049350051216
049360081020     c                   Eval      wtrmk50 = *On
049370051216     c                   EndSr
049380051216
049390051216      *------------------------------------------------------------------------*
049400051216      * TASTO FUNZIONE F15 -
049410051216      *------------------------------------------------------------------------*
049420051216     c     Sr_F15        BegSr
049430051216
049440051216     c                   Clear                   parmf15
049450051216     c                   Move      w0070         parcli1
049460051216     c                   Eval      kpjbu = parmf15
049470051216     c                   Call      'TNTA83R'
049480051216     c                   Parm                    kpjba
049490051216
049500051216     c                   EndSr
049510051216
049520051216      *------------------------------------------------------------------------*
049530051216      * TASTO FUNZIONE F18 -
049540051216      *------------------------------------------------------------------------*
049550051216     c     Sr_F18        BegSr
049560100406
049570100406      /free
049580100806       //?Richiamo gestione note
049590100406          clear trmk20ds;
049600110511          IMK20tla  = 'L';
049610100406
049620100406         //?Se interrogazione richiamo in interrogazione
049630100406          IF  *in05;
049640100406            IMK20flm = 'I';
049650100406          ENDIF;
049660100406
049670101112          IMK20cpo = v2ccpo;
049680100806          IMK20ksc = v1cksc;
049690100406          IMK20rsc = v1crag;
049700100406
049710100406          trmk20r (kpjba : TRMK20DS);
049720100406
049730100406      /end-free
049740051216
049750051216     c                   EndSr
049760060526      *------------------------------------------------------------------------*
049770060526      * TASTO FUNZIONE F19 - Variazioni
049780060526      *------------------------------------------------------------------------*
049790060526     c     Sr_F19        BegSr
049800060526
049810060526     c                   Clear                   tnta73ds
049820060526     c                   Eval      i73kcc=acokcc
049830060526     c                   Eval      i73ksc=acoksc
049840060529     c                   Movel     v1crag        i73rag
049850060526     c                   movel     tnta73ds      kpjbu
049860060526     c                   Call      'TNTA73R'
049870060526     c                   Parm                    kpjba
049880060526     c                   EndSr
049890100407
049900100407      *------------------------------------------------------------------------*
049910100407      * TASTO FUNZIONE F20 - INTERROGAZIONE CLIENTI POTENZIALI
049920100407      *------------------------------------------------------------------------*
049930100407     c     Sr_F20        BegSr
049940100407
049950100407      /free
049960100806       //?Richiamo gestione potenziali
049970100407          clear trmk01ds;
049980100407
049990100407         //?Se non c'è il codice potenziale vado in ricerca
050000100407          IF  v2ccpo = 0;
050010100407            MK01k11  = '1';
050020100407            MK01rag  = w005a;
050030100407            kpjbu    = TRMK01DS;
050040100407            trmk01r (kpjba);
050050100407            TRMK01DS = kpjbu;
050060100407          ELSE;
050070100407         //?Se c'è il codice potenziale vado in gestione
050080100407            MK01k10 = 'N';
050090100407            MK01cpo = v2ccpo;
050100100407            trmk02r (kpjba : TRMK01DS);
050110100407          ENDIF;
050120100407
050130100407          IF MK01cpo > 0 and MK01cpo <> v2ccpo;
050140100407            v2ccpo = MK01cpo;
050150100407            v2dcpo = MK01rag;
050160100407            exsr Sr_Ctrcpo;
050170100407          ENDIF;
050180100407
050190100407      /end-free
050200100407
050210100407     c                   EndSr
050220051216
050230051216      *------------------------------------------------------------------------*
050240051216      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
050250051216      *------------------------------------------------------------------------*
050260051216     c     Sr_F24        begsr
050270051216
050280051216     c                   Select
050290051216
050300051216     c                   When      $loop = 2 and $rig =2  or
050310051216     c                             $loop = 3 and $rig =3
050320051216     c                   Movea     rigatf1       vzfd01
050330051216     c                   Z-add     1             $rig
050340051216
050350051216     c                   If        $loop = 2
050360051216     c                   Eval      vzfd02= cf2412
050370051216     c                   Else
050380051216     c                   Eval      vzfd02= cf2413
050390051216     c                   EndIf
050400051216
050410051216     c                   When      $loop = 2 and $rig =1 or
050420051216     c                             $loop = 3 and $rig =1
050430051216     c                   Movea     rigatf2       vzfd01
050440051216     c                   Z-add     2             $rig
050450051216
050460051216     c                   If        $loop = 2
050470051216     c                   Eval      vzfd02 = cf2422
050480051216     c                   Else
050490051216     c                   Eval      vzfd02 = cf2423
050500051216     c                   EndIf
050510051216
050520051216     c                   When      $loop = 3 and $rig =2
050530051216     c                   Movea     rigatf3       vzfd01
050540051216     c                   Z-add     3             $rig
050550051216     c                   Eval      vzfd02 = cf2433
050560051216     c                   EndSl
050570051216
050580051216     c                   EndSr
050590051216
050600051216      *------------------------------------------------------------------------*
050610051216      * CONFERMO I DATI
050620051216      *------------------------------------------------------------------------*
050630051216     c     Sr_Conferma   BegSr
050640051220
050650051220     c                   Eval      wokimm = *Off
050660051216
050670051216      * In base alla videata emessa faccio i controlli che servono per
050680051216      * poter confermare i dati del video
050690051216
050700051216     c                   Select
050710051216
050720051216      * F6 dato in seconda videata
050730051216     c                   When      wvideo = '02'
050740051216     c                   ExSr      Sr_Ctrd02
050750051216     c   28              Leavesr
050760180119     c                   IF        ClienteSofferenza
050770180119     c                   leavesr
050780180119     c                   ENDIF
050790051216     c                   ExSr      Sr_Ctrd03
050800051216     c  n28              ExSr      Sr_Ctrd04
050810051216     c  n28              ExSr      Sr_Ctrd05
050820051216     c  n28              ExSr      Sr_Ctrd06
050830051216     c  n28              ExSr      Sr_Ctrd07
050840051216     c   28              Eval      v2cmsg = msg(69)
050850150424     c   28              eval      wForzaPag = *off
050860150728     c   28              eval      wForzaMail = *off
050870051216     c   28              Leavesr
050880051216
050890051216      * F6 dato in terza videata
050900051216     c                   When      wvideo = '03'
050910051216     c                   ExSr      Sr_Ctrd03
050920051216     c   28              Leavesr
050930051216     c                   ExSr      Sr_Ctrd04
050940051216     c  n28              ExSr      Sr_Ctrd05
050950051216     c  n28              ExSr      Sr_Ctrd06
050960051216     c  n28              ExSr      Sr_Ctrd07
050970051216     c   28              Eval      v3cmsg = msg(69)
050980150424     c   28              eval      wForzaPag = *off
050990150728     c   28              eval      wForzaMail = *off
051000051216     c   28              Leavesr
051010051216
051020051216      * F6 dato in quarta videata
051030051216     c                   When      wvideo = '04'
051040051216     c                   ExSr      Sr_Ctrd04
051050051216     c   28              Leavesr
051060051216     c                   ExSr      Sr_Ctrd05
051070051216     c  n28              ExSr      Sr_Ctrd06
051080051216     c  n28              ExSr      Sr_Ctrd07
051090051216     c   28              Eval      v4cmsg = msg(69)
051100150424     c   28              eval      wForzaPag = *off
051110150728     c   28              eval      wForzaMail = *off
051120051216     c   28              Leavesr
051130051216
051140051216      * F6 dato in quinta videata
051150051216     c                   When      wvideo = '05'
051160051216     c                   ExSr      Sr_Ctrd05
051170051216     c   28              Leavesr
051180051216     c                   ExSr      Sr_Ctrd06
051190051216     c  n28              ExSr      Sr_Ctrd07
051200051216     c   28              Eval      v5cmsg = msg(69)
051210150728     c   28              eval      wForzaMail = *off
051220051216     c   28              Leavesr
051230051216
051240051216      * F6 dato in sesta videata
051250051216     c                   When      wvideo = '06'
051260051216     c                   ExSr      Sr_Ctrd06
051270051216     c   28              Leavesr
051280051216     c                   ExSr      Sr_Ctrd07
051290051216     c   28              Eval      v6cmsg = msg(69)
051300150728     c   28              eval      wForzaMail = *off
051310051216     c   28              Leavesr
051320051216
051330051216      * F6 dato in settima videata
051340051216     c                   When      wvideo = '07'
051350051216     c                   ExSr      Sr_Ctrd07
051360051216     c   28              Leavesr
051370051216
051380051216     c                   EndSl
051390051216
051400051216      * Se arrivo a questo punto vuol dire che tutte le videate sono a posto
051410051220
051420051220      * Recupero il flag di procedura ORM attiva
051430060731     c                   Movel     v1cksc        w0030
051440051220     c                   Clear                   og148
051450061012     c                   Clear                   og143
051460051220     c     w0030         Chain     Azorg01l
051470051220     c                   If        %Found(Azorg01l)
051480051220     c                   Eval      og148 = orgde8
051490061012     c                   Eval      og143 = orgde3
051500051220     c                   EndIf
051510160905
051520160905      * se cliente logistica no anagrafica provvisoria
051530160905     c                   IF        ClienteLog
051540160905     c                   goto      NoImmACR
051550160905     c                   ENDIF
051560051220
051570051220      * Se non sono in anagrafica provvisoria, la procedura ORM è attiva e
051580051220      * provengo da convalida offerte
051590120313if  1c                   If        Not *In06 and *In07 and §ogorm <> *blanks
051600051220      * Controllo se devo creare l'anagrafica clienti ritiro
051610051220     c                   Eval      wokimm = *On
051620060731     c                   Movel     v1cksc        w0100
051630051220     c     w0100         Setll     Fnacr01l
051640051220do  2c                   Do        *Hival
051650051220     c                   Read      Fnacr01l
051660051220      * Fine file esco
051670051220     c                   If        %Eof(Fnacr01l)
051680051220     c                   Leave
051690051220     c                   EndIf
051700051220     c                   Movel     acrcro        w0070
051710051220      * Codice diverso esco
051720060731     c                   If        w0070 > v1cksc
051730051220     c                   Leave
051740051220     c                   EndIf
051750051220      * Se trovo non devo inserire l'anagrafico clienti ritiro
051760060731     c                   If        w0070 = v1cksc
051770051220     c                   Eval      wokimm = *Off
051780051220     c                   Leave
051790051220     c                   EndIf
051800051220    2c                   EndDo
051810051220
051820051220      * Se il cliente è già sull'anagrafico clienti ritiro emetto la videata
051830051220      * con un msg info
051840051221     c                   Eval      *In29 = *Off
051850051220if  2c                   If        wokimm = *Off and wforza = *Off
051860051220     c                   Eval      *In28 = *On
051870051220     c                   Eval      wforza = *On
051880051220     c                   Select
051890051220     c                   When      wvideo = '02'
051900051220     c                   Eval      v2cmsg = msg(72)
051910051221     c                   Eval      *In29 = *On
051920051220     c                   Goto      emi02
051930051220     c                   When      wvideo = '03'
051940051220     c                   Eval      v3cmsg = msg(72)
051950051220     c                   Goto      emi03
051960051220     c                   When      wvideo = '04'
051970051220     c                   Eval      v4cmsg = msg(72)
051980051220     c                   Goto      emi04
051990051220     c                   When      wvideo = '05'
052000051220     c                   Eval      v5cmsg = msg(72)
052010051220     c                   Goto      emi05
052020051220     c                   When      wvideo = '06'
052030051220     c                   Eval      v6cmsg = msg(72)
052040051220     c                   Goto      emi06
052050051220     c                   When      wvideo = '07'
052060051220     c                   Eval      v7cmsg = msg(72)
052070051220     c                   Goto      emi07
052080051220     c                   EndSl
052090051220    2c                   EndIf
052100051220    1c                   EndIf
052110160905
052120160905     c     NoImmACR      tag
052130051220
052140150424      * aggiorno la nota "DC"
052150091119     c                   if        v2ntcdc <> savntcdc
052160091119     c                   ExSr      Sr_AggntcDC
052170091119     c                   endif
052180051220
052190051220      * Quindi posso passare i dati dal video ai vari file e aggiornare
052200051216
052210051216      * Immissione pulisco i rcd file e imposto la chiave
052220051219if  1c                   If        *In01
052230051216     c  n06              Clear                   Cnaco000
052240051216     c   06              Clear                   Tfaco000
052250051216     c  n06              Clear                   Cnind000
052260051216     c   06              Clear                   Tfind000
052270051216     c  n06              Clear                   Cnclp000
052280051216     c   06              Clear                   Tfclp000
052290051216     c  n06              Clear                   Fncls000
052300051216     c   06              Clear                   Tfcls000
052310051220      * Chiave ACO
052320051216     c                   Eval      acokut = codut
052330051216     c                   Eval      acokcc = dutkci
052340051222     c  n06              Eval      acoksc = v1cksc
052350051222     c   06              Eval      acoksc = v1cnrv
052360051220      * Chiave IND
052370051216     c                   Eval      indkut = acokut
052380051216     c                   Eval      indkcc = acokcc
052390051216     c                   Eval      indksc = acoksc
052400051220      * Chiave CLP
052410051216     c                   Eval      clpkut = acokut
052420051216     c                   Eval      clpkcc = acokcc
052430051216     c                   Eval      clpksc = acoksc
052440051220      * Chiave CLS
052450051216     c                   Eval      clsksc = acoksc
052460051219      * Campi non visualizzati solo di immissione (non vengono aggiornati)
052470051219     c                   Eval      acoopz = v2copz
052480051219     c                   Eval      acotic = v2ctic
052490051219     c                   Eval      indlin = v2clin
052500051219      * Codice filiale di trasmissione sempre uguale ai primi 3 byte del codice
052510051219      * cliente
052520060330     c                   Movel     acoksc        acoflt
052530051219      * Flag tipo trasmissione sempre a '3' xchè se sono in immissione vuol dire
052540051219      * che sto immettendo un cliente o un'anagrafica provvisoria
052550051219     c                   Eval      acoftt = '3'
052560051219      * Data prima spedizione fattura (solo immissione xchè la imposto io
052570051219      * nel caso di cliente non nuovo
052580051219     c                   Z-add     v4cdps        datagma
052590051219     c                   Eval      gg1 = gg2
052600051219     c                   Eval      mm1 = mm2
052610051219     c                   Eval      aa1 = aa2
052620051219     c                   Z-add     dataamg       clpdps
052630051219      * Cliente da sollecitare
052640090616     c                   Movel     v2csol        clpsol
052650051219    1c                   EndIf
052660051216
052670051219      * Campi che vengono aggiornati a video
052680051219      * ACO
052690051219     c                   Eval      atb02 = v2tb02
052700051219     c                   Eval      acorag = v2crag
052710051219     c                   If        v6cdmg = 'SI'
052720051219     c                   Clear                   acorx1
052730051219     c                   Else
052740051219     c                   Eval      acorx1 = 1
052750051219     c                   EndIf
052760060801     c                   If        v6ccag = 'SI'
052770060801     c                   Clear                   acory1
052780060801     c                   Else
052790060801     c                   Eval      acory1 = 1
052800060801     c                   EndIf
052810051219     c                   Z-add     *date         acoduv
052820130325     c   79              eval      ACOabl = V2Cabledp
052830130325     c  n79              eval      ACOabl = V2Cabl
052840051219     c                   Eval      acolib = v2ccpo
052850051219     c                   Move      v3citc        acoitc
052860051216     c                   Clear                   acodtr
052870051219     c                   Clear                   acoftr
052880051219      * IND
052890051216     c                   Eval      indvia = v2cvia
052900051219     c                   Eval      indcit = v2ccit
052910051216      * Se Cap numerico e non supera i 5 caratteri scrivo anche indcap
052920051216if  1c                   If        %subst(v2ccae:6:1) = *Blanks
052930051216     c                   Eval      wokcap = *On
052940051216     c                   Eval      xx = 1
052950051216     c                   For       xx by 1 to 5
052960051216     c                   If        %subst(v2ccae:(xx):1) < *zeros
052970051216     c                   Eval      wokcap = *Off
052980051216     c                   Leave
052990051216     c                   EndIf
053000051216     c                   EndFor
053010051216     c                   If        wokcap = *On
053020060208     c                   Movel     v2ccae        indcap
053030051216     c                   EndIf
053040051216    1c                   EndIf
053050051216     c                   Eval      indprv = v2cprv
053060051216     c                   Eval      indsta = v2csta
053070051216     c                   Eval      indtel = v2ctel
053080051219     c                   Eval      indcdf = v2ccdf
053090051219     c                   If        v2ivaeu = *Blanks
053100051219     c                   Movel     v2ivait       indiva
053110051219     c                   Else
053120051219     c                   Movel     v2civa        indiva
053130051219     c                   EndIf
053140051219     c                   Eval      indcdp = *all'0'
053150100727     c                   Move      v4ccdp        indcdp
053160051219     c                   Eval      %subst(indopz:1:1) = wbloc
053170100727     c                   Eval      indabi = v4cabi
053180100727     c                   Eval      indcab = v4ccab
053190051216     c                   Eval      indtlf = v2ctlf
053200051219     c                   Move      v4csin        indsin
053210051219     c                   Eval      indcae = v2ccae
053220051219      * CLP
053230100127     c                   Eval      clpscf = %dec(v4cscf:7:0)
053240051219     c                   Move      v4cfft        clpfft
053250051219     c                   Move      v4ctft        clptft
053260051219     c                   Clear                   clpfun
053270051219     c                   Eval      %subst(clpfun:1:1) = v4ctdf
053280051219     c                   Move      v3cage        clpage
053290051219     c                   Clear                   clpcon
053300051219     c                   Eval      %subst(clpcon:2:2) = v2ccon
053310051219     c                   Eval      clpnar = v2cblc
053320051219     c                   Eval      clpclv = v3cclv
053330080422     c                   eval      clpbab = v5ciban
053340080213      * se IBAN non impostato a video
053350080213      * pulisco i dati relativi a abi/cab e c/c
053360080213     c                   if        v5ciban = *blanks
053370090331      * ma solo se ABI NON è > 90000
053380090331     c                   if        clpabi < 90000
053390080213     c                   clear                   clpccb
053400080213     c                   clear                   clpabi
053410080213     c                   clear                   clpcab
053420090331     c                   endif
053430080213     c                   else
053440080213      * se IBAN italia memorizzo abi/cab e c/c
053450080213     c                   if        %subst(v5ciban:1:2) = 'IT' or
053460080213     c                             %subst(v5ciban:1:2) = 'SM'
053470080115     c                   eval      %subst(clpccb:1:12) =
053480080115     c                             %subst(v5ciban:16:12)
053490080115     c                   eval      %subst(clpccb:16:1) =
053500080115     c                             %subst(v5ciban:5:1)
053510080115     c                   eval      clpabi = %dec(%subst(v5ciban:6:5):5:0)
053520080115     c                   eval      clpcab = %dec(%subst(v5ciban:11:5):5:0)
053530080213     c                   endif
053540080213     c                   endif
053550080422     c                   Eval      clpfpc = v5cfpc
053560090617      * Cliente da sollecitare
053570090617     c                   Movel     v2csol        clpsol
053580051219      * CLS
053590051219     c                   Eval      clsstp = v7cstp
053600100729     c                   eval      %subst(CLStic:1:1) = v5tpdn
053610100729     c                   eval      %subst(CLStic:2:1) = v5tpna
053620051219     c                   Eval      %subst(clsflo:1:1) = v6ctcf
053630051219     c                   Eval      %subst(clsflo:2:1) = v4cuni
053640121105     c                   eval      %subst(CLSflo:3:1) = V4Ctpf
053650090616     c                   Eval      %subst(clsflo:4:1) = v2fsol
053660051219     c                   Eval      %subst(clsflo:5:1) = v7csdn
053670051219     c                   Eval      %subst(clsflo:6:1) = v4cstm
053680051219     c                   Eval      %subst(clsflo:7:1) = v7csin
053690051219     c                   Eval      %subst(clsflo:8:1) = v7ctpa
053700051219     c                   Eval      %subst(clsflo:9:1) = v6capg
053710051219     c                   Eval      %subst(clsflo:10:1) = v6ctcm
053720051219
053730051219      * Immissione
053740051219     c                   If        *In01
053750051219     c  n06              Write     Fncls000
053760051219     c   06              Write     Tfcls000
053770051219     c  n06              Write     Cnclp000
053780051219     c   06              Write     Tfclp000
053790051219     c  n06              Write     Cnind000
053800051219     c   06              Write     Tfind000
053810051219     c  n06              Write     Cnaco000
053820051219     c   06              Write     Tfaco000
053830060526     c* Scrivo file variazioni
053840060526     c  n06              exsr      ScriviACV
053850060526     c                   EndIf
053860060526     c
053870051219      * Manutenzione
053880051219     c                   If        *In02
053890051219     c  n06              Update    Fncls000
053900051219     c   06              Update    Tfcls000
053910051219     c  n06              Update    Cnclp000
053920051219     c   06              Update    Tfclp000
053930051219     c  n06              Update    Cnind000
053940051219     c   06              Update    Tfind000
053950051219     c  n06              Update    Cnaco000
053960051219     c   06              Update    Tfaco000
053970060526     c*
053980060526     c* Scrivo file variazioni
053990060526     c  n06              exsr      ScriviACV
054000051219     c                   EndIf
054010110927
054020110927      * Aggiorno il potenziale
054030160906      * solo se NON sono utente di sede
054040110927     c                   If        v2ccpo > *Zeros
054050160906     c                             and not *in09
054060110927     c                   ExSr      Sr_Aggcpo
054070110927     c                   EndIf
054080100729
054090100729      /free
054100100729       //?Memorizzo le coordinate bancarie
054110100730       //?Pagamento DANNI --> DN
054120100730         clear rqsOpCode;
054130100729         wpag = 'DN';
054140100729         wiban = v5ibandn;
054150100730         wbic  = v5bicdn;
054160100805         wcodpo = savtipdn;
054170100805         wcodpn = v5tpdn;
054180100805         wpgm  = 'TNTA60R';
054190100730         SELECT;
054200100730           WHEN  v5ibandn <> *blanks and savibandn = *blanks;
054210100805             rqsOpCode = 'INSERT';
054220100805           WHEN  v5ibandn = *blanks and savibandn <> *blanks;
054230100805             rqsOpCode = 'DELETE';
054240100805           OTHER;
054250100805             rqsOpCode = 'UPDATE';
054260100730         ENDSL;
054270100730         IF  rqsOpCode <> *blanks;
054280100730           exsr Coordinate;
054290100730         ENDIF;
054300100729       //?Non faccio controlli sul codice di ritorno
054310100729       //?ormai l'anagrafica è scritta
054320100729
054330100729       //?Pagamento NOTE ACCREDITO --> NA
054340100730         clear rqsOpCode;
054350100729         wpag = 'NA';
054360100729         wiban = v5ibanna;
054370100730         wbic  = v5bicna;
054380100805         wcodpo = savtipna;
054390100805         wcodpn = v5tpna;
054400100805         wpgm  = 'TNTA60R';
054410100730         SELECT;
054420100730           WHEN  v5ibanna <> *blanks and savibanna = *blanks;
054430100805             rqsOpCode = 'INSERT';
054440100805           WHEN  v5ibanna = *blanks and savibanna <> *blanks;
054450100805             rqsOpCode = 'DELETE';
054460100805           OTHER;
054470100805             rqsOpCode = 'UPDATE';
054480100730         ENDSL;
054490100730         IF  rqsOpCode <> *blanks;
054500100730           exsr Coordinate;
054510100729         ENDIF;
054520100729       //?Non faccio controlli sul codice di ritorno
054530100729       //?ormai l'anagrafica è scritta
054540110223
054550110223       //?Se NON è anagrafica provvisoria:
054560110223       //?il cliente è stato bloccato
054570110223       //?e il potenziale NON è in categoria PERSO
054580110310       //?oppure
054590110310       //?il cliente NON è più bloccato
054600110310       //?e il potenziale è in categoria PERSO
054610110223       //?oppure
054620110223       //?arrivo da convalida
054630110223       //?e il potenziale NON è in categoria CODIFICATO
054640110223         IF  not *in06 and
054650110223             ((ACOabl <> *blanks and
054660110223               savabl = *blanks and V2Hfls <> 'P') or
054670110310              (ACOabl = *blanks and
054680110310               savabl <> *blanks and V2Hfls = 'P') or
054690110223              (*in07 and V2Hfls <> 'C'));
054700110223       //?richiamo pgm di calcolo categoria potenziale
054710110223           clear trmk05ds;
054720110223           IMK05cpo = ACOlib;
054730110223           trmk05r (kpjba : TRMK05DS);
054740110223         //?se categoria variata aggiorno potenziale
054750110304           IF  OMK05err = *blanks and OMK05cat <> V2Hfls;
054760110223             chain(e) ACOlib TNCPO01L;
054770110223             IF  not %error and %found(TNCPO01L);
054780110223               CPOfls = OMK05cat;
054790110223               update TNCPO000;
054800110223             ENDIF;
054810110223           ENDIF;
054820110223         ENDIF;
054830170523
054840170523      * Se cliente logistica no anagrfica clienti ritiro
054850170523      * e no spalmatura coordinate IBAN
054860170523     c                   IF        ClienteLog
054870170523     c                   goto      ByPassaAcr
054880170523     c                   ENDIF
054890150424
054900150424       //?Devo spalmare le coordinate bancarie e i cod.pagamenti
054910150424         WinInfo = *off;
054920150424         EoFW06 = *off;
054930150424         SELECT;
054940150424       //?spalmo da pag. NA a CA e DN
054950150424         WHEN  BonificoNA and
054960150424               (not BonificoCA or not BonificoDN);
054970150424           exsr SpalmaNA;
054980150424         WHEN  BonificoDN and
054990150424               (not BonificoCA or not BonificoNA);
055000150424           exsr SpalmaDN;
055010150424         WHEN  BonificoCA and
055020150424               (not BonificoDN or not BonificoNA);
055030150424           exsr SpalmaCA;
055040150424         ENDSL;
055050150424         IF  WinInfo;
055060150424           DOW not EoFW06;
055070150424             exfmt TA60W06;
055080150424             IF  *inkf;
055090150424               EoFW06 = *on;
055100150424               leave;
055110150424             ENDIF;
055120150424           ENDDO;
055130150424         ENDIF;
055140150424
055150100729      /end-free
055160051220
055170051220      * Se non sono in anagrafica provvisoria
055180120313if  1c                   If        Not *In06 and §ogorm <> *blanks
055190051220
055200051220      * Controllo il blocco/sblocco dei clienti per aggiornare anche
055210051220      * l'anagrafica clienti ritiro (solo in caso di blocco)
055220121121      * lo faccio solo se modifica perchè se è immissione non c'è
055230121121      * ancora l'anagrafica clienti ritiro
055240121121     c                   IF        *in02
055250051220     c                   ExSr      Sr_Ctrabl
055260121121     c                   ENDIF
055270051220
055280051220      * Richiamo l'immissione anagrafica clienti ritiro
055290051220if  2c                   If        *In01 or wokimm = *On
055300121121
055310121121      * NON lo faccio se immissione di codici 8888 - 9999 o incassi
055320121121      * da attribuire --> 0001 1° livello
055330121121      *               --> 1000 2° livello
055340160429      * immissione fatta da profilo EDP
055350121121     c                   SELECT
055360121121     c                   WHEN      wksc04 = 8888 or wksc04 = 9999
055370121121     c                   goto      noacr
055380160429     c                   WHEN       *in20 and
055390121121     c                             ((sav_livello = '1' and wksc04 = 0001) or
055400121121     c                              (sav_livello = '2' and wksc04 = 1000) or
055410121121     c                              (sav_livello = *blanks and
055420121121     c                               wCodIncAtt  = *blanks and
055430121121     c                              (wksc04 = 1000  or  wksc04 = 0001)))
055440121121     c                   goto      noacr
055450121121     c                   ENDSL
055460070809     c                   reset                   FIOR37ds
055470070810     c                   eval      I37opz  = '3'
055480070809     c                   movel     ACOksc        I37fgs
055490070809     c                   movel     ACOksc        I37cro
055500070809     c                   movel     ACOksc        I37ksc
055510070809     c                   movel(p)  FIOR37ds      KPJBU
055520070810     c                   call      'FIOR37R'
055530070809     c                   parm                    KPJBA
055540051220    2c                   EndIf
055550121121     c     noacr         tag
055560051220      * Richiamo la modifica dell'anagrafica clienti ritiro
055570060217if  2c                   If        *In02 and Not *In07
055580070809     c                   reset                   FIOR37ds
055590070809     c                   eval      I37opz  = 'A'
055600070809     c                   movel     ACOksc        I37fgs
055610070809     c                   movel     ACOksc        I37cro
055620070809     c                   movel(p)  FIOR37ds      KPJBU
055630070809     c                   call      'FIOR37R'
055640070809     c                   parm                    KPJBA
055650051220    2c                   EndIf
055660051216
055670051220    1c                   EndIf
055680160905
055690160905     c     ByPassaAcr    tag
055700160905
055710061012     c*
055720061012     c* se cliente di network dpd richiamo pgm gestione legami depot/cod.PdC
055730061012     c* per permettere l'aggiornamento del codice cliente sul file dei depot
055740061012     c                   if        not *in06 and §ogntw='DPD'
055750061012     c* imposta la ds di procedura
055760061012     c                   CLEAR                   tisie5ds
055770061012     c                   MOVE      '02'          de5op0
055780061012     c                   MOVEL     *blanks       de5op1
055790061012     c                   MOVEL     '0'           de5f03
055800061012     c                   MOVEL     '0'           de5f12
055810061012     c                   MOVEL     '0'           de5err
055820061012     c                   MOVEL     *blanks       de5msg
055830061012     c                   movel     'N'           de5sel
055840061012     c* lancia il programma
055850061012     c                   CALL      'TISIE5R'
055860061012     c                   PARM                    kpjba
055870061012     c                   PARM                    tisie5ds
055880061012     c                   endif
055890120510
055900120510      /free
055910120510       //?Se il cliente è stato bloccato devo richiamare il pgm delle INFO
055920120510       //?per obbligo di azzeramento della % affido a BRT
055930120510       //?Il richiamo lo faccio solo se:
055940120510       //?--> è appena stato immesso il blocco
055950120510       //?--> il pgm non è stato richiamto dal CMR x impostare il blocco
055960120510       //?--> c'è il codice del potenziale
055970120510         IF  not $Blocco and
055980120510             ACOabl <> *blanks and ACOabl <> savabl and
055990120510             ACOlib > 0;
056000120510           IF  not %open(CNACO16L);
056010120510             open CNACO16L;
056020120510           ENDIF;
056030120510         //?tutto questo solo se sto bloccando l'ultimo cliente della catena
056040120510         //?se ne ho ancora dei NON bloccati non devo azzerare
056050120510           setll ACOlib CNACO16L;
056060120510           reade ACOlib CNACO16L;
056070120510           DOW not %eof(CNACO16L);
056080120510             IF  ww_ACOabl = *blanks;
056090120510               leavesr;
056100120510             ENDIF;
056110120510             reade ACOlib CNACO16L;
056120120510           ENDDO;
056130120510         //?Aggancio le INFO commerciali
056140120510         //?se la % affido a BRT è = 0 non devo azzerare
056150120510           chain (ACOlib:'10 ':'_BAR') TICPI01L;
056160120510 1         IF  not %found(ticpi01l) or CPIval = 0;
056170120510             leavesr;
056180120510           ENDIF;
056190120510
056200120510         //?pgm INFO
056210120510           clear TRMK50DS;
056220120510           I50pgm = 'TNTA60R';
056230120510           I50cpo = ACOlib;
056240120510           I50rag = ACOrag;
056250120510           I50mod = 'G';
056260120510           I50atb = '0';
056270120510           TRMK50R (kpjba : TRMK50DS);
056280120510
056290120510           wTRMK50 = *on;
056300120510
056310120510         ENDIF;
056320120510
056330120510      /end-free
056340051220
056350051216     c                   EndSr
056360051220
056370060526      *------------------------------------------------------------------------*
056380060526      * Scrittura file variazioni
056390060526      *------------------------------------------------------------------------*
056400060526     c     ScriviACV     BegSr
056410060531     c                   Clear                   Tibs73ds
056420060531     c                   eval      ibs73pru=knmus
056430060531     c                   eval      ibs73noj=knmeb
056440060601     c                   eval      ibs73pgm='TNTA60R'
056450060531     c                   eval      ibs73immag='D'
056460060526     c* Immissione
056470060526     c                   if        *in01
056480060531     c                   eval      ibs73cta='I'
056490060531     c                   Call      'TIBS73R'
056500060531     c                   Parm                    Tibs73ds
056510060531     c                   Parm                    cnaco_73
056520060531     c                   Parm                    cnind_73
056530060531     c                   Parm                    cnclp_73
056540060531     c                   Parm                    fncls_73
056550060526     c                   endif
056560060526     c* Manutenzione
056570060526     C                   IF        *IN02
056580060531     c                   eval      ibs73cta='M'
056590060531     c                   Call      'TIBS73R'
056600060531     c                   Parm                    Tibs73ds
056610060531     c                   Parm                    cnaco_73
056620060531     c                   Parm                    cnind_73
056630060531     c                   Parm                    cnclp_73
056640060531     c                   Parm                    fncls_73
056650060531     c                   endif
056660060526     c
056670060531     c                   Eval      wtibs73 = *On
056680060526     c                   EndSr
056690150424
056700150424      /free
056710150424       //--------------------------------------------------------------
056720150427       //?Spalmo coordinate bancarie e pagamenti CA.
056730150424       //--------------------------------------------------------------
056740150424       BEGSR SpalmaCA;
056750150427
056760150512       //?Se il pagamento CA è estero
056770150427       //?spalmo solo il codice senza le coordinate
056780150427         IF  PagEsteroCA;
056790150427           clear CLPbab;
056800150427         ENDIF;
056810150427
056820150427       //?Anagrafica clienti Reale
056830150427         IF  not *in06;
056840150427       //?Aggancio FNCLS
056850150427           chain V1Cksc FNCLS01L;
056860150427           IF  not %found(FNCLS01L);
056870150427             leavesr;
056880150427           ENDIF;
056890150427       //?Spalmo pagamento e coordinate per DN
056900151112           IF  CodPagDN <> '1';
056910150427           IF  not BonificoDN;
056920150427         //?coordinate bancarie
056930150427             clear rqsOpCode;
056940150427             wpag = 'DN';
056950150427             wiban = CLPbab;
056960150427             wcodpo = %subst(CLStic:1:1);
056970150427             wcodpn = CLPfpc;
056980150427             wpgm  = 'TNTA60R';
056990150427             rqsOpCode = 'INSERT';
057000150427             exsr Coordinate;
057010150427             %subst(CLStic:1:1) = CLPfpc;
057020151112             WinInfo = *on;
057030150427           ENDIF;
057040151112           ENDIF;
057050150427       //?Spalmo pagamento e coordinate per NA
057060151112           IF  CodPagNA <> '1';
057070150427           IF  not BonificoNA;
057080150427         //?coordinate bancarie
057090150427             clear rqsOpCode;
057100150427             wpag = 'NA';
057110150427             wiban = CLPbab;
057120150427             wcodpo = %subst(CLStic:2:1);
057130150427             wcodpn = CLPfpc;
057140150427             wpgm  = 'TNTA60R';
057150150427             rqsOpCode = 'INSERT';
057160150427             exsr Coordinate;
057170150427             %subst(CLStic:2:1) = CLPfpc;
057180151112             WinInfo = *on;
057190150427           ENDIF;
057200151112           ENDIF;
057210150427       //?Aggiorno anagrafica
057220150427           update FNCLS000;
057230150427         ENDIF;
057240150427
057250150427       //?Anagrafica clienti Provvisoria
057260150427         IF  *in06;
057270150427       //?Aggancio TFCLS
057280150427           chain V1Cnrv TFCLS01L;
057290150427           IF  not %found(TFCLS01L);
057300150427             leavesr;
057310150427           ENDIF;
057320150427       //?Spalmo pagamento e coordinate per DN
057330151112           IF  CodPagDN <> '1';
057340150427           IF  not BonificoDN;
057350150427             clear rqsOpCode;
057360150427             wpag = 'DN';
057370150427             wiban = CLPbab;
057380150427             wcodpo = %subst(CLStic:1:1);
057390150427             wcodpn = CLPfpc;
057400150427             wpgm  = 'TNTA60R';
057410150427             rqsOpCode = 'INSERT';
057420150427             exsr Coordinate;
057430150427             %subst(CLStic:1:1) = CLPfpc;
057440151112             WinInfo = *on;
057450150427           ENDIF;
057460151112           ENDIF;
057470150427       //?Spalmo pagamento e coordinate per NA
057480151112           IF  CodPagNA <> '1';
057490150427           IF  not BonificoNA;
057500150427             clear rqsOpCode;
057510150427             wpag = 'NA';
057520150427             wiban = CLPbab;
057530150427             wcodpo = %subst(CLStic:2:1);
057540150427             wcodpn = CLPfpc;
057550150427             wpgm  = 'TNTA60R';
057560150427             rqsOpCode = 'INSERT';
057570150427             exsr Coordinate;
057580150427             %subst(CLStic:2:1) = CLPfpc;
057590151112             WinInfo = *on;
057600150427           ENDIF;
057610151112           ENDIF;
057620150427       //?Aggiorno anagrafica
057630150427           update TFCLS000;
057640150427         ENDIF;
057650150424
057660150424       ENDSR;
057670150424
057680150424       //--------------------------------------------------------------
057690150427       //?Spalmo coordinate bancarie e pagamenti DN.
057700150424       //--------------------------------------------------------------
057710150424       BEGSR SpalmaDN;
057720150427
057730150427         SELECT;
057740150427
057750150427       //?Se il pagamento DN è estero ed è
057760150427       //?Anagrafica clienti Reale
057770150427       //?spalmo solo il codice senza le coordinate
057780150427         WHEN  PagEsteroDN and not *in06;
057790150427       //?Memorizzo immagine precedente
057800150427           exsr ImmaginePrima;
057810150427       //?Spalmo pagamento per CA
057820151112           IF  CodPagCA <> '1';
057830150427           IF  not BonificoCA;
057840150427         //?Aggancio CNCLP
057850150427             chain (codut:V1Ckcc:V1Cksc) CNCLP00F;
057860150427             IF  not %found(CNCLP00F);
057870150427               leavesr;
057880150427             ENDIF;
057890150427             CLPfpc = %subst(CLStic:1:1);
057900150427             update CNCLP000;
057910151112             WinInfo = *on;
057920150427           ENDIF;
057930151112           ENDIF;
057940150427       //?Spalmo pagamento per NA
057950151112           IF  CodPagNA <> '1';
057960150427           IF  not BonificoNA;
057970150427         //?Aggancio FNCLS
057980150427             chain V1Cksc FNCLS01L;
057990150427             IF  not %found(FNCLS01L);
058000150427               leavesr;
058010150427             ENDIF;
058020150427             %subst(CLStic:2:1) = %subst(CLStic:1:1);
058030150427             update FNCLS000;
058040151112             WinInfo = *on;
058050150427           ENDIF;
058060151112           ENDIF;
058070150427       //?Immagine Dopo
058080150427           exsr ImmagineDopo;
058090150427
058100150427       //?Se il pagamento DN è estero ed è
058110150427       //?Anagrafica clienti Provvisoria
058120150427       //?spalmo solo il codice senza le coordinate
058130150427         WHEN  PagEsteroDN and *in06;
058140150427       //?Spalmo pagamento per CA
058150151112           IF  CodPagCA <> '1';
058160150427           IF  not BonificoCA;
058170150427         //?Aggancio TFCLP
058180150427             chain (codut:V1Ckcc:V1Cnrv) TFCLP00F;
058190150427             IF  not %found(TFCLP00F);
058200150427               leavesr;
058210150427             ENDIF;
058220150427             CLPfpc = %subst(CLStic:1:1);
058230150427             update TFCLP000;
058240151112             WinInfo = *on;
058250150427           ENDIF;
058260151112           ENDIF;
058270150427       //?Spalmo pagamento per NA
058280151112           IF  CodPagNA <> '1';
058290150427           IF  not BonificoNA;
058300150427         //?Aggancio TFCLS
058310150427             chain V1Cnrv TFCLS01L;
058320150427             IF  not %found(TFCLS01L);
058330150427               leavesr;
058340150427             ENDIF;
058350150427             %subst(CLStic:2:1) = %subst(CLStic:1:1);
058360150427             update TFCLS000;
058370151112             WinInfo = *on;
058380150427           ENDIF;
058390151112           ENDIF;
058400150427
058410150427         OTHER;
058420150427       //?Se arrivo qua il pagamento non è estero quindi spalmo tutto
058430150424       //?Recupero le coordinate bancarie DN
058440150427           wpag = 'DN';
058450150427           rqsOpCode = 'SEARCH';
058460150427           exsr Coordinate;
058470150424       //?Spalmo coordinate e pagamento per CA
058480151112           IF  CodPagCA <> '1';
058490150427           IF  not BonificoCA;
058500150424       //?Memorizzo immagine precedente
058510150427             IF  not *in06;
058520150427               exsr ImmaginePrima;
058530150427         //?Aggancio CNCLP
058540150427               chain (codut:V1Ckcc:V1Cksc) CNCLP00F;
058550150427               IF  not %found(CNCLP00F);
058560150427                 leavesr;
058570150427               ENDIF;
058580150427             ELSE;
058590150427         //?Aggancio TFCLP
058600150427               chain (codut:V1Ckcc:V1Cnrv) TFCLP00F;
058610150427               IF  not %found(TFCLP00F);
058620150427                 leavesr;
058630150427               ENDIF;
058640150427             ENDIF;
058650150427             CLPfpc = %subst(CLStic:1:1);
058660150427             IF  not *in06;
058670150427               CLPbab = TrulIbanO0.IBAN;
058680150427               CLPabi = TrulIbanO0.ABI;
058690150427               CLPcab = TrulIbanO0.CAB;
058700150427               update CNCLP000;
058710150427         //?Memorizzo immagine sucessiva
058720150427               exsr ImmagineDopo;
058730150427             ELSE;
058740150427               CLPbab = TrulIbanO1.IBAN;
058750150427               CLPabi = TrulIbanO1.ABI;
058760150427               CLPcab = TrulIbanO1.CAB;
058770150427               update TFCLP000;
058780150427             ENDIF;
058790151112             WinInfo = *on;
058800150427           ENDIF;
058810151112           ENDIF;
058820150424
058830150424       //?Spalmo coordinate e pagamento per NA
058840151112           IF  CodPagNA <> '1';
058850150427           IF  not BonificoNA;
058860150427             clear rqsOpCode;
058870150427             wpag = 'NA';
058880150427         //?Aggancio FNCLS
058890150427             IF  not *in06;
058900150427               chain V1Cksc FNCLS01L;
058910150427               IF  not %found(FNCLS01L);
058920150427                 leavesr;
058930150427               ENDIF;
058940150427               wiban = TrulIbanO0.IBAN;
058950150427               wbic  = TrulIbanO0.BIC;
058960150427               wcodpo = %subst(CLStic:2:1);
058970150427               wcodpn = %subst(CLStic:1:1);
058980150427             ELSE;
058990150427         //?Aggancio TFCLS
059000150427               chain V1Cnrv TFCLS01L;
059010150427               IF  not %found(TFCLS01L);
059020150427                 leavesr;
059030150427               ENDIF;
059040150427               wiban = TrulIbanO1.IBAN;
059050150427               wbic  = TrulIbanO1.BIC;
059060150427               wcodpo = %subst(CLStic:2:1);
059070150427               wcodpn = %subst(CLStic:1:1);
059080150427             ENDIF;
059090150427             wpgm  = 'TNTA60R';
059100150427             rqsOpCode = 'INSERT';
059110150427             exsr Coordinate;
059120150427             %subst(CLStic:2:1) = %subst(CLStic:1:1);
059130150427         //?Aggiorno anagrafica
059140150427             IF  not *in06;
059150150427               update FNCLS000;
059160150427             ELSE;
059170150427               update TFCLS000;
059180150427             ENDIF;
059190151112             WinInfo = *on;
059200150427           ENDIF;
059210151112           ENDIF;
059220150427         ENDSL;
059230150424
059240150424       ENDSR;
059250150424
059260150424       //--------------------------------------------------------------
059270150427       //?Spalmo coordinate bancarie e pagamenti NA.
059280150424       //--------------------------------------------------------------
059290150424       BEGSR SpalmaNA;
059300150427
059310150427         SELECT;
059320150427
059330150427       //?Se il pagamento NA è estero ed è
059340150427       //?Anagrafica clienti Reale
059350150427       //?spalmo solo il codice senza le coordinate
059360150427         WHEN  PagEsteroNA and not *in06;
059370150427       //?Memorizzo immagine precedente
059380150427           exsr ImmaginePrima;
059390150427       //?Spalmo pagamento per CA
059400151112           IF  CodPagCA <> '1';
059410150427           IF  not BonificoCA;
059420150427         //?Aggancio CNCLP
059430150427             chain (codut:V1Ckcc:V1Cksc) CNCLP00F;
059440150427             IF  not %found(CNCLP00F);
059450150427               leavesr;
059460150427             ENDIF;
059470150427             CLPfpc = %subst(CLStic:2:1);
059480150427             update CNCLP000;
059490151112             WinInfo = *on;
059500150427           ENDIF;
059510151112           ENDIF;
059520150427       //?Spalmo pagamento per DN
059530151112           IF  CodPagDN <> '1';
059540150427           IF  not BonificoDN;
059550150427         //?Aggancio FNCLS
059560150427             chain V1Cksc FNCLS01L;
059570150427             IF  not %found(FNCLS01L);
059580150427               leavesr;
059590150427             ENDIF;
059600150427             %subst(CLStic:1:1) = %subst(CLStic:2:1);
059610150427             update FNCLS000;
059620151112             WinInfo = *on;
059630150427           ENDIF;
059640151112           ENDIF;
059650150427       //?Immagine Dopo
059660150427           exsr ImmagineDopo;
059670150427
059680150427       //?Se il pagamento NA è estero ed è
059690150427       //?Anagrafica clienti Provvisoria
059700150427       //?spalmo solo il codice senza le coordinate
059710150427         WHEN  PagEsteroNA and *in06;
059720150427       //?Spalmo pagamento per CA
059730151112           IF  CodPagCA <> '1';
059740150427           IF  not BonificoCA;
059750150427         //?Aggancio TFCLP
059760150427             chain (codut:V1Ckcc:V1Cnrv) TFCLP00F;
059770150427             IF  not %found(TFCLP00F);
059780150427               leavesr;
059790150427             ENDIF;
059800150427             CLPfpc = %subst(CLStic:2:1);
059810150427             update TFCLP000;
059820151112             WinInfo = *on;
059830150427           ENDIF;
059840151112           ENDIF;
059850150427       //?Spalmo pagamento per DN
059860151112           IF  CodPagDN <> '1';
059870150427           IF  not BonificoDN;
059880150427         //?Aggancio TFCLS
059890150427             chain V1Cnrv TFCLS01L;
059900150427             IF  not %found(TFCLS01L);
059910150427               leavesr;
059920150427             ENDIF;
059930150427             %subst(CLStic:1:1) = %subst(CLStic:2:1);
059940150427             update TFCLS000;
059950151112             WinInfo = *on;
059960150427           ENDIF;
059970151112           ENDIF;
059980150427
059990150427         OTHER;
060000150427       //?Se arrivo qua il pagamento non è estero quindi spalmo tutto
060010150424       //?Recupero le coordinate bancarie NA
060020150427           wpag = 'NA';
060030150427           rqsOpCode = 'SEARCH';
060040150427           exsr Coordinate;
060050150424       //?Spalmo coordinate e pagamento per CA
060060151112           IF  CodPagCA <> '1';
060070150427           IF  not BonificoCA;
060080150424       //?Memorizzo immagine precedente
060090150427             IF  not *in06;
060100150427               exsr ImmaginePrima;
060110150424       //?Aggancio CNCLP
060120150427               chain (codut:V1Ckcc:V1Cksc) CNCLP00F;
060130150427               IF  not %found(CNCLP00F);
060140150427                 leavesr;
060150150427               ENDIF;
060160150427             ELSE;
060170150427       //?Aggancio TFCLP
060180150427               chain (codut:V1Ckcc:V1Cnrv) TFCLP00F;
060190150427               IF  not %found(TFCLP00F);
060200150427                 leavesr;
060210150427               ENDIF;
060220150427             ENDIF;
060230150427             CLPfpc = %subst(CLStic:2:1);
060240150427             IF  not *in06;
060250150427               CLPbab = TrulIbanO0.IBAN;
060260150427               CLPabi = TrulIbanO0.ABI;
060270150427               CLPcab = TrulIbanO0.CAB;
060280150427               update CNCLP000;
060290150427         //?Memorizzo immagine sucessiva
060300150427               exsr ImmagineDopo;
060310150427             ELSE;
060320150427               CLPbab = TrulIbanO1.IBAN;
060330150427               CLPabi = TrulIbanO1.ABI;
060340150427               CLPcab = TrulIbanO1.CAB;
060350150427               update TFCLP000;
060360150427             ENDIF;
060370151112             WinInfo = *on;
060380150427           ENDIF;
060390151112           ENDIF;
060400150424
060410150424       //?Spalmo coordinate e pagamento per DN
060420151112           IF  CodPagDN <> '1';
060430150427           IF  not BonificoDN;
060440150427             clear rqsOpCode;
060450150427             wpag = 'DN';
060460150427         //?Aggancio FNCLS
060470150427             IF  not *in06;
060480150427               chain V1Cksc FNCLS01L;
060490150427               IF  not %found(FNCLS01L);
060500150427                 leavesr;
060510150427               ENDIF;
060520150427               wiban = TrulIbanO0.IBAN;
060530150427               wbic  = TrulIbanO0.BIC;
060540150427               wcodpo = %subst(CLStic:1:1);
060550150427               wcodpn = %subst(CLStic:2:1);
060560150427             ELSE;
060570150427         //?Aggancio TFCLS
060580150427               chain V1Cnrv TFCLS01L;
060590150427               IF  not %found(TFCLS01L);
060600150427                 leavesr;
060610150427               ENDIF;
060620150427               wiban = TrulIbanO1.IBAN;
060630150427               wbic  = TrulIbanO1.BIC;
060640150427               wcodpo = %subst(CLStic:1:1);
060650150427               wcodpn = %subst(CLStic:2:1);
060660150427             ENDIF;
060670150427             wpgm  = 'TNTA60R';
060680150427             rqsOpCode = 'INSERT';
060690150427             exsr Coordinate;
060700150427             %subst(CLStic:1:1) = %subst(CLStic:2:1);
060710150427         //?Aggiorno anagrafica
060720150427             IF  not *in06;
060730150427               update FNCLS000;
060740150427             ELSE;
060750150427               update TFCLS000;
060760150427             ENDIF;
060770151112             WinInfo = *on;
060780150427           ENDIF;
060790151112           ENDIF;
060800150424
060810150427         ENDSL;
060820150424
060830150424       ENDSR;
060840150424      /end-free
060850150424
060860051220      *------------------------------------------------------------------------*
060870051220      * AGGIORNAMENTO DEL CODICE POTENZIALE
060880051220      *------------------------------------------------------------------------*
060890051220     c     Sr_Aggcpo     BegSr
060900100407
060910100407      /free
060920100806       //?Se variato codice potenziale
060930110928       //?o se immissione di nuovo codice
060940110928        IF  (v2ccpo <> savcpo and savcpo > 0) or *in01;
060950100407         //?richiamo programma di comodo per sistemare potenziale
060960100407         //?sui vari archivi (trattativa, anagrafica provvisoria, attività)
060970100407         //?nel caso di trattativa aperta
060980110223         //?in ogni caso ricalcola e aggiorna sempre la categoria
060990110223         //?del vecchio e nuovo potenziale
061000100407          clear TRMK28DS;
061010100407          IMK28cpo   = v2ccpo;
061020100407          IMK28cpoex = savcpo;
061030100407          IMK28ksc   = v1cksc;
061040100407          trmk28r (TRMK28DS);
061050100407        ENDIF;
061060121105
061070121105       //?Se Immissione di nuovo cliente (no anagrafica provvisoria)
061080121105       //?devo aggiornare la data primo contatto sul Potenziale
061090121105        IF  *in01 and not *in06 and V2Ccpo > 0 and
061100121105           (§CPOdtpcon = *blanks or §CPOdtpcon = *zeros);
061110121105          chain(e) CPOcpo TNCPO01L;
061120121105          IF  not %error and %found(TNCPO01L);
061130121105            dCPO01 = CPOrst;
061140121105            §CPOdtpcon = %editc(dataoggi:'X');
061150121105            CPOrst = dCPO01;
061160121105            update TNCPO000;
061170121105          ENDIF;
061180121105        ENDIF;
061190121105
061200100407      /end-free
061210080307
061220080307      * se sono in anagrafica provvisoria aggiorno codice fiscale e p.iva
061230080307      * sul potenziale se sul potenziale mancano
061240080307     c                   if        *in06  and *in01
061250110516     c     v2ccpo        Chain(e)  Tncpo01l
061260110516     c                   if        %error
061270110516     c                   leavesr
061280110516     c                   endif
061290080307     c                   if        %found(tncpo01l)
061300160803      /free
061310160803       //?Salvo l'immagine precedente dell'anagrafica potenziale
061320160803         exsr ImmaginePrimaCpo;
061330160803      /end-free
061340080307     c                   if        (cpopiv=*blanks and v2civa<>*blanks)
061350080307     c                   eval      cpopiv=v2civa
061360080307     c                   z-add     *date         cpodtr
061370080307     c                   endif
061380080307     c                   if        (cpocdf=*blanks and v2ccdf<>*blanks)
061390080307     c                   eval      cpocdf=v2ccdf
061400080307     c                   z-add     *date         cpodtr
061410080307     c                   endif
061420080307     c                   update    tncpo000
061430160803      /free
061440160803       //?Scrivo la Variazione fatta sull'anagrafica potenziale
061450160803         exsr ScriviVarCpo;
061460160803      /end-free
061470080307     c                   endif
061480080307     c                   endif
061490051220
061500051220     c                   EndSr
061510051216
061520051216      *------------------------------------------------------------------------*
061530051125      * CONTROLLO DEL CODICE POTENZIALE
061540051125      *------------------------------------------------------------------------*
061550051125     c     Sr_Ctrcpo     BegSr
061560051125
061570111122      * Obbligatorio se cliente, se non è un 8888/9999
061580110223      * e se non è un 'incassi da attribuire'
061590111122      * se non è utente EDP e codice nuovo x 'incassi da attribuire'
061600051125     c                   If        v2ccpo = *Zeros and
061610110223     c                             v1ckcc = 151 and
061620051125     c                             wksc04 <> 8888 and wksc04 <> 9999
061630110223     c                             and %editc(v1cksc:'X') <> wCodIncAtt
061640170516      // Se cliente potenziale emetto un messaggio forzabile
061650170516       IF  (ClienteLog and not wForzaPotenziale) or
061660170516            not ClienteLog;
061670051125     c                   Eval      *In28 = *On
061680051202     c                   Eval      h2riga = 14
061690051125     c                   Eval      h2colo = 28
061700051125     c                   Eval      v2cmsg = msg(12)
061710170516       IF  ClienteLog;
061720170516         V2Cmsg = %trim(V2Cmsg) + '. Enter per forzare.';
061730170516         wForzaPotenziale = *on;
061740170516       ENDIF;
061750160429     c                   IF        *in20 and *in01
061760111122     c                             and sav_livello = *blanks
061770111122     c                             and wCodIncAtt = *blanks and
061780111122     c                             (wksc04 = 0001 or wksc04 = 1000)
061790111122     c                   clear                   v2cmsg
061800111122     c                   eval      *in28 = *off
061810111122     c                   ENDIF
061820051125     c                   Leavesr
061830170516       ENDIF;
061840051125     c                   EndIf
061850051125      * Carico i dati del potenziale se inserito
061860051125if  1c                   If        v2ccpo <> *Zeros and
061870051125      * e se variato il codice potenziale
061880051125     c                             v2ccpo <> savcpo
061890060221     c                             and wokpot = *off
061900051125      * Controllo se esite
061910060221     c     v2ccpo        Chain(n)  Tncpo01l
061920051125     c                   If        Not %Found(Tncpo01l)
061930051125     c                   Eval      *In28 = *On
061940051202     c                   Eval      h2riga = 14
061950051125     c                   Eval      h2colo = 28
061960051125     c                   Eval      v2cmsg = msg(13)
061970051125     c                   Leavesr
061980051125     c                   EndIf
061990051125      * Controllo se valido
062000051125     c                   If        cpoatb <> *Blanks
062010051125     c                   Eval      *In28 = *On
062020150427     c                   Eval      h2riga = 14
062030051125     c                   Eval      h2colo = 28
062040051125     c                   Eval      v2cmsg = msg(13)
062050051125     c                   Leavesr
062060051125     c                   EndIf
062070051125      * Controllo se l'utente può gestire il potenziale
062080051125     c                   Move      cpoflt        w003a
062090051125     c     w003a         Lookup    skpogcpo                               30
062100051125     c                   If        Not *In30
062110051125     c                   Eval      *In28 = *On
062120051202     c                   Eval      h2riga = 14
062130051125     c                   Eval      h2colo = 28
062140060110     c                   Eval      v2cmsg = msg(14)
062150051125     c                   Leavesr
062160051125     c                   EndIf
062170100604
062180100604      /free
062190100604       //?Se NON sono in ambiente di filiale non posso variare il potenziale
062200100604       IF  *in09 and v2ccpo <> savcpo and savcpo > 0;
062210100604         *in28 = *on;
062220100604         h2riga = 07;
062230100604         h2colo = 28;
062240100604         v2cmsg = 'Potenziale modificabile SOLO da utente di filiale';
062250100604         leavesr;
062260100604       ENDIF;
062270100806       //?controllo se con il vecchio potenziale c'è una
062280100604       //?trattativa aperta, in questo caso controllo che non ne esista già
062290100604       //?una con il nuovo potenziale
062300100806       IF  v2ccpo <> savcpo and savcpo > 0;
062310100604       //?con vecchio potenziale
062320100604         clear TNTA43ds;
062330100604         ITA43cpo = savcpo;
062340100604         ITA43ksc = v1cksc;
062350100604         ITA43cmm = %dec(v3cage:7:0);
062360100604         tnta43r (tnta43ds);
062370100604       //?trovo trattativa aperta
062380100604         IF  OTA43nrv > 0;
062390100604         //?rifaccio il controllo con nuovo potenziale
062400100604           clear TNTA43ds;
062410100604           ITA43cpo = v2ccpo;
062420100604           ITA43ksc = v1cksc;
062430100604           ITA43cmm = %dec(v3cage:7:0);
062440100604           tnta43r (tnta43ds);
062450100604         //?trovo trattativa aperta
062460100604           IF  OTA43nrv > 0;
062470100604             *in28 = *on;
062480100604             h2riga = 07;
062490100604             h2colo = 28;
062500100604             v2cmsg = 'Modifica non possibile, trattativa aperta sul +
062510100604                       potenziale immesso';
062520100604             leavesr;
062530100604           ENDIF;
062540100604         ENDIF ;
062550100604       ENDIF;
062560100604      /end-free
062570051212
062580051206      * Carico a video i dati del potenziale se sono in immissione
062590051206     c   01              ExSr      Sr_Carcpo
062600051125    1c                   EndIf
062610051125
062620051125     c                   EndSr
062630051116
062640051116      *------------------------------------------------------------------------*
062650051116      * CARICO I DATI DEL CODICE POTENZIALE
062660051116      *------------------------------------------------------------------------*
062670051116     c     Sr_Carcpo     BegSr
062680140714
062690140714     c     CPOcpo        Chain     Tncpo11l
062700051116
062710051116     c                   Eval      v2crag = cporag
062720051116     c                   Eval      v2cvia = cpovia
062730051116     c                   Eval      v2ccae = cpocap
062740051117     c                   Eval      v2csta = cponaz
062750051116     c                   Eval      v2ccit = cpocit
062760051116     c                   Eval      v2cprv = cpoprv
062770140714     c                   Eval      v2ctel = cpo1tel
062780140714     c                   Eval      v2ctlf = cpo1fax
062790051117     c                   Eval      v2ccpo = cpocpo
062800051117     c                   Eval      savcpo = cpocpo
062810051130     c                   Move      cposct        savitc
062820060208     c                   Eval      v2dcpo = cporag
062830080306     c                   Eval      v2ccdf = cpocdf
062840080306     c                   Eval      savcdf = v2ccdf
062850080306    5c                   if        %subst(cpopiv:1:2)>='00'
062860080306     c                   Movel     cpopiv        v2ivait
062870080306     c                   else
062880080306     c                   Movel     cpopiv        v2civa
062890080306    5c                   endif
062900080306     c                   Eval      saviva = v2civa
062910051116
062920051116     c                   EndSr
062930091119
062940091119      *------------------------------------------------------------------------*
062950091119      * AGGIORNAMENTO NOTA DC
062960091119      *------------------------------------------------------------------------*
062970091119     c     Sr_AggntcDC   BegSr
062980091119
062990091119     c   70
063000091119     corn06              Eval      kntcapl = 'C'
063010100806     c   06
063020091119     cann70              Eval      kntcapl = 'T'
063030091119     c                   Movel(p)  dutkci        kntcnk1
063040091119     c   70
063050091119     corn06              Move      v1cksc        kntcnk1
063060100806     c   06
063070100806     cann70              Move      v1cnrv        kntcnk1
063080091119     c                   Clear                   kntcnk2
063090091119     c                   Movel     'DC'          kntctnt
063100091119     c     kTfntc        Chain     Tfntc01l
063110091119     c                   If        %Found(Tfntc01l)
063120091119      * devo cancellarla perchè campo vuoto
063130091119     c                   if        v2ntcdc = *blanks
063140091119     c                   delete    tfntc
063150091119     c                   endif
063160091119      * aggiorno la nota
063170091119     c                   if        v2ntcdc <> *blanks
063180091119     c                   eval      ntcrnt = v2ntcdc
063190091119     c                   move      *date         ntcntr
063200091119     c                   update    tfntc
063210091119     c                   endif
063220091119     c                   endif
063230091119      * scrivo la nota
063240091119     c                   If        not %Found(Tfntc01l)
063250091119     c                   clear                   tfntc
063260091119     c                   eval      ntcapl = kntcapl
063270091119     c                   eval      ntcnk1 = kntcnk1
063280091119     c                   eval      ntctnt = kntctnt
063290091119     c                   eval      ntcrnt = v2ntcdc
063300091119     c                   eval      ntcsns = 'S'
063310091119     c                   move      *date         ntcntr
063320091119     c                   write     tfntc
063330091119     c                   endif
063340091119
063350091119     c                   EndSr
063360051212
063370051212      *--------------------------------------------------------------------*
063380051212      * WINDOW X CONFERMA DI VARIAZIONE POTENZIALE
063390051212      *--------------------------------------------------------------------*
063400051212     c     Sr_Winpot     begsr
063410051212
063420051212     c                   Reset                   wokpot
063430051212     c                   Eval      w01cok = 'NO'
063440051212     c                   Exfmt     ta60w01
063450051212      * Ok il nuovo potenziale è da tenere
063460051212if  1c                   If        w01cok = 'SI'
063470051212     c                   Eval      wokpot = *On
063480051212    1c                   EndIf
063490051212
063500051212     c                   EndSr
063510051118
063520051118      *------------------------------------------------------------------------*
063530051118      * CONTROLLO INDIRIZZO
063540051118      *------------------------------------------------------------------------*
063550051118     c     Sr_Ctrind     BegSr
063560051118
063570051118     c                   Clear                   fnlv13ds
063580051118     c                   Clear                   tisi95ds
063590051124     c                   Eval      *In90 = *Off
063600051118
063610051118      * Indirizzo completo
063620051118     c                   Z-add     *date         i14dri
063630051118     c                   Call      'FNLV14R'
063640051118     c                   Parm                    kpjba
063650051118     c                   Parm                    fnlv14ds
063660051118
063670051118s   1c                   Select
063680051118     c                   When      o14err = '1'
063690051124     c                   Eval      *In90 = *On
063700051130     c                   Eval      h2riga = 07
063710051118     c                   Eval      h2colo = 28
063720051124     c                   If        o14msg <> *Blanks
063730051124     c                   Eval      *In28 = *On
063740051118     c                   Eval      v2cmsg = o14msg
063750051124     c                   EndIf
063760051124     c                   Leavesr
063770051118     c                   When      o14err = '2'
063780051124     c                   Eval      *In90 = *On
063790051130     c                   Eval      h2riga = 08
063800051118     c                   Eval      h2colo = 28
063810051124     c                   If        o14msg <> *Blanks
063820051124     c                   Eval      *In28 = *On
063830051118     c                   Eval      v2cmsg = o14msg
063840051124     c                   EndIf
063850051124     c                   Leavesr
063860051118     c                   When      o14err = '5'
063870051124     c                   Eval      *In90 = *On
063880051130     c                   Eval      h2riga = 09
063890051118     c                   Eval      h2colo = 18
063900051124     c                   If        o14msg <> *Blanks
063910051124     c                   Eval      *In28 = *On
063920051118     c                   Eval      v2cmsg = o14msg
063930051124     c                   EndIf
063940051124     c                   Leavesr
063950051118     c                   When      o14err = '3'
063960051124     c                   Eval      *In90 = *On
063970051130     c                   Eval      h2riga = 09
063980051118     c                   Eval      h2colo = 28
063990051124     c                   If        o14msg <> *Blanks
064000051124     c                   Eval      *In28 = *On
064010051118     c                   Eval      v2cmsg = o14msg
064020051124     c                   EndIf
064030051124     c                   Leavesr
064040051118     c                   When      o14err = '4'
064050051124     c                   Eval      *In90 = *On
064060051130     c                   Eval      h2riga = 09
064070051118     c                   Eval      h2colo = 59
064080051124     c                   If        o14msg <> *Blanks
064090051124     c                   Eval      *In28 = *On
064100051118     c                   Eval      v2cmsg = o14msg
064110051124     c                   EndIf
064120051124     c                   Leavesr
064130051118     c                   When      o14err = '6'
064140051124     c                   Eval      *In90 = *On
064150051130     c                   Eval      h2riga = 09
064160051118     c                   Eval      h2colo = 62
064170051124     c                   If        o14msg <> *Blanks
064180051124     c                   Eval      *In28 = *On
064190051118     c                   Eval      v2cmsg = o14msg
064200051124     c                   EndIf
064210051124     c                   Leavesr
064220051118     c                   When      o14err <> *Blanks and o14msg = *Blanks
064230051118     c                   Goto      emi02
064240051118    1c                   EndSl
064250051118
064260051118      * Nazione con cappario controllo Cap
064270051118if  1c                   If        o14cpp = *Blanks
064280051118     c                   Eval      i95tcn = '7'
064290051118     c                   Eval      i95cap = e14cap
064300051118     c                   Eval      i95loc = e14loc
064310051118     c                   Eval      i95prv = e14prv
064320051118     c                   Eval      i95nar = e14nar
064330051118     c                   Z-add     *date         i95dat
064340051118     c                   Eval      i13af0 = 'S'
064350051118     c                   Eval      i13cnv = 'S'
064360051118     c                   Eval      i13af1 = 'S'
064370051118     c                   Eval      i13la3 = 'S'
064380051118      * Se dati variati controllo se congruenti
064390051118if  2c                   If        e14cap <> wcae or e14loc <> wcit or
064400051118     c                             e14prv <> wprv or e14nar <> wsta
064410051118     c                   Clear                   wfz3
064420051118     c                   Eval      wcae = e14cap
064430051118     c                   Eval      wcit = e14loc
064440051118     c                   Eval      wprv = e14prv
064450051118     c                   Eval      wsta = e14nar
064460051118    2c                   EndIf
064470051118     c                   Eval      E13fz3 = wfz3
064480051118     c                   Call      'FNLV13R'
064490051118     c                   Parm                    kpjba
064500051118     c                   Parm                    fnlv13ds
064510051118     c                   Parm                    tisi95ds
064520051118
064530051118     c                   If        o13ron = 'S'
064540051118     c                   Eval      e14nar = o95nar
064550051118     c                   EndIf
064560051118     c                   If        o13roc = 'S'
064570051118     c                   Eval      e14cap = o95cap
064580051118     c                   EndIf
064590051118     c                   If        o13rop = 'S'
064600051118     c                   Eval      e14prv = o95prv
064610051118     c                   EndIf
064620051118     c                   If        o13rol = 'S'
064630051118     c                   Eval      e14loc = o95loc
064640051118     c                   EndIf
064650051118
064660051124      * Reimposto le forzature
064670051118     c                   Eval      wfz3 = E13fz3
064680051118
064690051118s   2c                   Select
064700051118     c                   When      o13err = '5'
064710051118     c                   Eval      *In28 = *On
064720051130     c                   Eval      h2riga = 09
064730051118     c                   Eval      h2colo = 18
064740051118     c                   Eval      v2cmsg = o13msg
064750051118     c                   Leavesr
064760051118     c                   When      o13err = '3'
064770051118     c                   Eval      *In28 = *On
064780051130     c                   Eval      h2riga = 09
064790051118     c                   Eval      h2colo = 28
064800051118     c                   Eval      v2cmsg = o13msg
064810051118     c                   Leavesr
064820051118     c                   When      o13err = '4'
064830051118     c                   Eval      *In28 = *On
064840051130     c                   Eval      h2riga = 09
064850051118     c                   Eval      h2colo = 59
064860051118     c                   Eval      v2cmsg = o13msg
064870051118     c                   Leavesr
064880051118     c                   When      o13err = '6'
064890051118     c                   Eval      *In28 = *On
064900051130     c                   Eval      h2riga = 09
064910051118     c                   Eval      h2colo = 62
064920051118     c                   Eval      v2cmsg = o13msg
064930051118     c                   Leavesr
064940051118     c                   When      o13err <> *Blanks and o13msg = *Blanks
064950051118     c                   Goto      emi02
064960051118    2c                   EndSl
064970051118
064980051118      * Se nazione senza cappario
064990051118   x1c                   Else
065000051122      * Il cliente deve essere bloccato con 8 (si può usare solo x fatturare e
065010051122      *                                        non per bollettare)
065020051118if  2c                   If        v2cabl <> '8'
065030051118     c                   Eval      *In28 = *On
065040051130     c                   Eval      h2riga = 09
065050051118     c                   Eval      h2colo = 62
065060051118     c                   Eval      v2cmsg = msg(15)
065070051118     c                   Leavesr
065080051118    2c                   EndIf
065090051118    1c                   EndIf
065100051118
065110051118      * Cap obsoleto
065120051118     c                   If        o95cf1 = 'O'
065130051118     c                   Eval      *In28 = *On
065140051130     c                   Eval      h2riga = 09
065150051118     c                   Eval      h2colo = 18
065160051118     c                   Eval      v2cmsg = msg(16)
065170051118     c                   Leavesr
065180051118     c                   EndIf
065190051118
065200051118     c                   EndSr
065210051118
065220051118      *------------------------------------------------------------------------*
065230051118      * CONTROLLO CODICE FISCALE
065240051118      *------------------------------------------------------------------------*
065250051118     c     Sr_Ctrcdf     BegSr
065260151014     c                   clear                   w_scfsta
065270100729
065280061030      * devo richiamare il nuovo driver fatto per controllare il codice
065290061030      * fiscale e la partita iva
065300080421      * rihciamo anche per codice vuoto, per controllare i casi
065310080421      *  di obbligo non inserimento
065320151014     c                   clear                   xcfiva1ds
065330061030     c                   eval      xcfivapar = v2ccdf
065340061030     c                   eval      xcfivanar = v2csta
065350061030     c                   eval      xcfivaprv = v2cprv
065360151014     c                   eval      xcfivacap = v2ccae
065370151014     c                   eval      xcfivaloc = v2ccit
065380151014     c                   eval      xcfivapiva= v2civa
065390151014     c                   call      'XCFIVAR1'
065400151014     c                   parm                    xcfiva1ds
065410080421     c
065420080421    1c                   if        xcfivaflg < *zeros
065430051118     c                   Eval      *In28 = *On
065440051202     c                   Eval      h2riga = 12
065450051118     c                   Eval      h2colo = 28
065460080421     c                   Eval      v2cmsg = xcfivamsg
065470051118     c                   Leavesr
065480080421    1c                   EndIf
065490080421
065500080421      * il codice fiscale è obbligatorio se sto inserendo un codice nuovo
065510080421      * cliente italia a partire dalla data decorrenza per p.iva sarà
065520080421      * obbligatorio non forzabile anche in caso di codice non nuovo
065530080421
065540080421      * Escludo i casi previsti di NON inserimento
065550080421    1c  n06              if        xcfivaflg<>9 and
065560080421     c                             v2csta=*blanks and v2ccdf=*blanks
065570080421     c
065580080421     c* prima di dare errore verifico se cliente del sottoconto intestazione
065590080421     c* fattura ha la nazione ed in questo caso non do errore
065600080421     c                   exsr      sr_repscfsta
065610080421    2c                   if        w_scfsta=*blanks
065620080421     c                   eval      *In28 = *On
065630080421     c                   eval      h2riga = 12
065640080421     c                   eval      h2colo = 28
065650080421     c                   eval      v2cmsg = msg(51)
065660080421     c                   leavesr
065670080421    2c                   endif
065680080421    1c                   endif
065690080306     c* Se provengo dalle visite e il relativo potenziale non ha il cod.fisc
065700080306     c* errore forzabile se inserito un codice fiscale già presente su altro
065710080306     c* potenziale
065720080421    1c                   if        *in06 and v2ccpo<>*zeros and v2ccdf<>*blanks
065730080306     c     v2ccpo        Chain(n)  Tncpo01l
065740080421    2c                   if        %found(tncpo01l) and cpocdf=*blanks
065750080306     c                             and v2ccdf<>savcdf
065760080306     c                             and v2ccdf<>cod_forzcdf
065770080306     c                   clear                   savrag
065780080307     c                   eval      codice=v2ccdf
065790080306     c                   exsr      sr_CdfAltroP
065800080306     c                   exsr      sr_msgcdf
065810080306     c   28              leavesr
065820080421    2c                   endif
065830080421    1c                   endif
065840051118
065850051118     c                   EndSr
065860051118
065870051118      *------------------------------------------------------------------------*
065880051118      * CONTROLLO PARTITA IVA E STATO
065890051118      *------------------------------------------------------------------------*
065900051118     c     Sr_Ctriva     BegSr
065910171213
065920171213        ClienteSofferenza = *off;
065930051118
065940051118      * STATO
065950051118      * --> ricerca
065960051118     c                   Eval      wpos = %scan('?':v2csta)
065970051118     c                   If        wpos > 0
065980051118     c                   Eval      kcod = '15'
065990051118     c                   call      'TRUL19R'
066000051118     c                   parm                    kcod
066010051118     c                   parm                    ordina
066020051118     c                   parm                    kkey
066030051118     c                   parm                    comando
066040051118     c                   Eval      v2csta = kkey
066050051130     c                   Eval      h2riga = 09
066060051118     c                   Eval      h2colo = 62
066070051124     c                   Eval      *In90 = *On
066080051118     c                   EndIf
066090051118      * --> controllo
066100051118     c                   Clear                   ds15
066110051118     c                   Eval      kcod = '15'
066120051118     c                   Eval      kkey = v2csta
066130051118     c     kTabel        Chain     Tabel00f
066140051118     c                   If        Not %Found(Tabel00f) or
066150051125     c                             tblflg <> *Blanks
066160051118     c                   Eval      *In28 = *On
066170051130     c                   Eval      h2riga = 09
066180051118     c                   Eval      h2colo = 62
066190051118     c                   Eval      v2cmsg = msg(17)
066200051118     c                   Leavesr
066210051118     c                   EndIf
066220051125     c                   Eval      ds15 = tbluni
066230051118
066240051129      * --> controllo
066250151014     c**                 If        v2civa <> *Blanks
066260061030      * devo richiamare il nuovo driver fatto per controllare il codice
066270061030      * fiscale e la partita iva
066280151014     c                   clear                   xcfiva1ds
066290061030     c                   eval      xcfivamod = 'P'
066300061030     c                   eval      xcfivapar = v2civa
066310061030     c                   eval      xcfivanar = v2csta
066320061030     c                   eval      xcfivaprv = v2cprv
066330151014     c                   eval      xcfivacap = v2ccae
066340151014     c                   eval      xcfivaloc = v2ccit
066350151014     c                   eval      xcfivapiva= v2civa
066360151014     c                   call      'XCFIVAR1'
066370151014     c                   parm                    xcfiva1ds
066380061106      * --> errata forzabile
066390061106     c                   If        xcfivaflg = -9 and wforzaiva = *off
066400061106     c                   Eval      *In28 = *On
066410061106     c                   Eval      h2riga = 12
066420061106     c                   Eval      h2colo = 52
066430061106     c                   Eval      v2cmsg = xcfivamsg
066440061106     c                   Eval      v2cmsg = %trim(v2cmsg) + ' Enter x forzare'
066450061106     c                   eval      wforzaiva = *on
066460061106     c                   Leavesr
066470061106     c                   EndIf
066480061030      * --> errore
066490061106     c                   if        xcfivaflg < *zeros and xcfivaflg <> -9
066500051118     c                   Eval      *In28 = *On
066510051202     c                   Eval      h2riga = 12
066520051118     c                   Eval      h2colo = 52
066530061030     c                   eval      v2cmsg = xcfivamsg
066540051118     c                   Leavesr
066550051118     c                   EndIf
066560061106      * se non impostato devo riportare il codice iso della partita iva che
066570061106      * mi viene passato dal pgm di controllo
066580061114     c                   if        v2ivaeu <> ivaeu
066590061106     c                   eval      v2ivaeu = ivaeu
066600061106     c                   eval      *in39 = *on
066610061106     c                   eval      *in90 = *on
066620061114     c                   endif
066630151014      * PARTITA IVA
066640151014      * --> obbligatoria (se non è gestione visite/offerte)
066650151014      * --> non è però obbligatoria se cliente Italia e nazione del
066660151014      *     sottoconto intestazione fattura estera
066670151014      * Escludo i casi previsti di NON inserimento
066680151014     c                   If        v2civa = *Blanks and Not *In06
066690151014     c                             and xcfivaflg<>9
066700151014     c                   exsr      sr_repscfsta
066710151014     c                   if        v2csta<>*blanks or w_scfsta=*blanks
066720151014     c                   Eval      *In28 = *On
066730151014     c                   Eval      h2riga = 12
066740151014     c                   Eval      h2colo = 52
066750151014     c                   Eval      v2cmsg = msg(18)
066760151014     c                   Leavesr
066770151014     c                   EndIf
066780151014     c                   EndIf
066790151014
066800151014     c* Se provengo dalle visite e il relativo potenziale non ha p.iva
066810080306     c* errore forzabile se inserito una p.iva         già presente su altro
066820080306     c* potenziale
066830151014     c                   If        v2civa <> *Blanks
066840080306     c                   if        *in06 and v2ccpo<>*zeros
066850080306     c                             and v2ivaeu<>'$$'
066860080306     c     v2ccpo        Chain(n)  Tncpo01l
066870080306     c                   if        %found(tncpo01l) and cpopiv=*blanks
066880080306     c                             and v2civa<>saviva
066890080306     c                             and v2civa<>cod_forziva
066900080306     c                   clear                   savrag
066910080307     c                   if        v2ivaeu<>*blanks
066920080306     c                   eval      codice=v2civa
066930080306     c                   else
066940080306     c                   eval      codice=v2ivait
066950080306     c                   endif
066960080306     c                   exsr      sr_PivAltroP
066970080306     c                   exsr      sr_msgiva
066980080306     c   28              leavesr
066990080306     c                   endif
067000080307     c                   endif
067010061106
067020060105      * --> controllo se c'è già un cliente con la stessa p.IVA
067030171221      *     faccio il controllo sempre se NON è anagrafica provvisoria
067040180119      *     in immissione e convalida offerte
067050180119     c                   IF        *in06
067060180119     c                   leavesr
067070180119     c                   ENDIF
067080180119     c                   IF        V2ivaeu = '$$'
067090180119     c                   leavesr
067100180119     c                   ENDIF
067110180119if  1c*****              If        Not *In06 and (*In01 or *In07)
067120180119     c*****                        and v2ivaeu <> '$$'
067130180119     c                   IF        *in01 or *in07 or
067140180119     c                             (saviva <> V2civa and *in02)
067150060109     c                   If        v2ivaeu = *Blanks
067160060109     c                   Movel     v2ivait       kindiva
067170060109     c                   Else
067180060109     c                   Movel     v2civa        kindiva
067190060109     c                   EndIf
067200060105     c     kCnind        Setll     Cnind02l
067210060105      * non trovo nessun altra partita IVA esco
067220060105     c                   If        Not %Equal
067230060105     c                   Leavesr
067240060105     c                   EndIf
067250060105do  2c                   Do        *Hival
067260060105     c     kCnind        Reade     Cnind02l
067270060105      * fine file esco
067280060105     c                   If        %Eof(Cnind02l)
067290060105     c                   Leave
067300060105     c                   EndIf
067310060105      * deve avere codice cliente diverso
067320060301     c                   If        ww_indksc = v1cksc
067330060105     c                   Iter
067340060105     c                   EndIf
067350060105      * controllo lo stato del credito
067360081006      * lo faccio con una routine per emettere una window nel caso di
067370081006      * messaggio info
067380081006     c                   exsr      sr_ctrstc
067390171213     c                   if        wforzacon = *On or
067400171213     c                             ClienteSofferenza
067410081006     c                   leavesr
067420081006     c                   endif
067430060105    2c                   EndDo
067440060105    1c                   EndIf
067450151015     c
067460151015     c                   EndIf
067470051118
067480051118     c                   EndSr
067490081006
067500081006      *------------------------------------------------------------------------*
067510081006      * CONTROLLO STATO DEL CREDITO
067520081006      *------------------------------------------------------------------------*
067530081006     c     sr_ctrstc     begsr
067540081006
067550081006     c                   Clear                   Tibs69ds
067560081006     c                   Move      ww_indksc     I69kcp
067570081006     c                   Call      'TIBS69R'
067580081006     c                   Parm                    Tibs69ds
067590081006     c                   Parm                    ds_cnaco
067600081006     c                   Parm                    ds_cnind
067610081006     c                   Parm                    ds_cnclp
067620081006     c                   Parm                    ds_fncls
067630081006     c                   Eval      wtibs69 = *On
067640081006      * errore forzabile per stato del credito trovato
067650081006      * emesso con window e f6 di conferma
067660171213      * se stato del credito è SOFFERENZA >= "41" devo BLOCCARE
067670171213      * uso il campo §4Wtip <> *blanks
067680081006     c                   If        w_clpcon <> *Blanks and wforzacon = *Off
067690090803      * decodifico stato del credito
067700090803     c                   clear                   w_ds4w
067710090803     c                   eval      kcod = '4W'
067720090803     c                   eval      kkey = w_clpcon
067730090803     c                   eval      w5ccon = %subst(w_clpcon:2:2)
067740090803     c     ktabel        chain     tabel00f
067750090803     c                   if        %found(tabel00f) and
067760090803     c                             tblflg = *blanks
067770090803     c                   eval      w_ds4w = tbluni
067780090803     c                   endIf
067790090803     c                   Eval      w5dcon = w_§4wdes
067800081006     c                   do        *hival
067810081006     c                   exfmt     ta60w05
067820081006     c   kf              leave
067830081006     c                   enddo
067840171213     c                   IF        w_§4Wtip = *blanks
067850081006     c                   Eval      wforzacon = *On
067860171213     c                   ELSE
067870171213     c                   eval      ClienteSofferenza = *on
067880171213     c                   ENDIF
067890081006     c                   EndIf
067900081006
067910081006     c                   endsr
067920051122
067930051122      *------------------------------------------------------------------------*
067940051122      * CONTROLLO BLOCCO CLIENTE
067950051122      *------------------------------------------------------------------------*
067960051122     c     Sr_Ctrblc     BegSr
067970051118
067980051122      * STATO DEL CREDITO
067990051122      * --> Ricerca
068000051122     c                   Clear                   v2dcon
068010051122     c                   Eval      wpos = %scan('?':v2ccon)
068020051122     c                   If        wpos > 0
068030051122     c                   Eval      kcod = '4W'
068040051122     c                   call      'TRUL19R'
068050051122     c                   parm                    kcod
068060051122     c                   parm                    ordina
068070051122     c                   parm                    kkey
068080051122     c                   parm                    comando
068090051124     c                   Eval      w003a = kkey
068100051124     c                   Move      w003a         v2ccon
068110051202     c                   Eval      h2riga = 17
068120051122     c                   Eval      h2colo = 28
068130051124     c                   Eval      *In90 = *On
068140051122     c                   EndIf
068150051122
068160051122      * --> Controllo
068170051122     c                   Clear                   v2dcon
068180051122     c                   Clear                   ds4w
068190051122     c                   Eval      kcod = '4W'
068200051124     c                   Move(p)   v2ccon        w003a
068210051124     c                   Eval      kkey = w003a
068220051122     c     kTabel        Chain     Tabel00f
068230051122     c                   If        Not %Found(Tabel00f) or
068240051125     c                             tblflg <> *Blanks
068250051122     c                   Eval      *In28 = *On
068260051202     c                   Eval      h2riga = 17
068270051122     c                   Eval      h2colo = 28
068280051122     c                   Eval      v2cmsg = msg(23)
068290051122     c                   Leavesr
068300051122     c                   EndIf
068310051125     c                   Eval      ds4w = tbluni
068320051122     c                   Eval      v2dcon = §4wdes
068330051122
068340051122      * Se sono in filiale devo usare solo stati del credito autorizzati e
068350051122      * senza passaggio al contenzioso
068360130325      * da 25/03/2013 abbiamo deciso che anche in sede non è modificale, solo
068370130325      * da ProJ
068380051122     c                   If        (§4wmbl = 'N' or §4wctz = 'S') and
068390130325     c                             (v2ccon <> %subst(clpcon:2:2) or *In01)
068400051122     c                   Eval      *In28 = *On
068410051202     c                   Eval      h2riga = 17
068420051122     c                   Eval      h2colo = 28
068430051122     c                   Eval      v2cmsg = msg(24)
068440051122     c                   Leavesr
068450051122     c                   EndIf
068460051122
068470051122      * --> Imposto i blocchi
068480051122      * Se sono in filiale proteggo lo stato del credito se non è modificabile
068490120928      * da p.o.
068500130325      * PER TUTTI
068510130325     c                   If        §4wmbl = 'N'
068520060220     c                   Eval      *In19 = *On
068530051122     c                   EndIf
068540120928      * Se sono in filiale proteggo il blocco pagamenti se non è modificabile
068550120928      * da p.o.
068560120928     c                   If        Not *In09 and §4wmbp = 'N'
068570120928     c                   Eval      *In81 = *On
068580120928     c                   EndIf
068590120925      * Se sono in filiale proteggo il blocco cliente se non è modificabile
068600120925      * da p.o.
068610120928     c                   If        Not *In09 and §4wmtb = 'N'
068620120925     c                   Eval      *In79 = *On
068630120925     c                   EndIf
068640121002
068650121002      * se stato del credito variato
068660121002     c                   IF        V2Ccon <> %subst(CLPcon:2:2)
068670051122      * Se il cliente non è bloccato e lo stato del credito prevede il blocco
068680051122      * lo faccio io ora in automatico
068690051122     c                   If        v2cabl = *Blanks and §4wtbl <> *Blanks
068700051122     c                   Eval      v2cabl = §4wtbl
068710130322     c                   eval      V2Cabledp = §4wtbl
068720051122     c                   EndIf
068730051122
068740051122      * Se non c'è la causale del blocco la imposto io ora in automatico
068750051122     c                   If        (v2cblc = *Blanks or v2cblc = *Zeros) and
068760051122     c                              §4wblc <> *Blanks
068770051122     c                   Eval      v2cblc = §4wblc
068780051122     c                   EndIf
068790121002     c                   ENDIF
068800051122
068810051122      * BLOCCO CLIENTE
068820130322     c                   IF        V2Cabl <> *blanks and V2Cabl <> '8'
068830130322     c                             and not *in79
068840130322     c                   Eval      *In28 = *On
068850130322     c                   Eval      h2riga = 18
068860130322     c                   Eval      h2colo = 28
068870130322     c                   Eval      V2Cmsg = 'Blocco cliente errato'
068880130322     c                   Leavesr
068890130322     c                   ENDIF
068900051122      * --> ricerca
068910051122     c                   Clear                   v2dblc
068920051122     c                   Eval      wpos = %scan('?':v2cblc)
068930051122     c                   If        wpos > 0
068940121127     c                   goto      no19
068950051122     c                   Eval      kcod = 'BI'
068960051122     c                   call      'TRUL19R'
068970051122     c                   parm                    kcod
068980051122     c                   parm                    ordina
068990051122     c                   parm                    kkey
069000051122     c                   parm                    comando
069010051122     c                   Eval      v2cblc = kkey
069020121127     c     no19          tag
069030121127     c                   clear                   kpjbu
069040121127     c                   call      'TRTBBIR'
069050121127     c                   parm                    kpjba
069060121127     c                   eval      v2cblc = %subst(kpjbu:1:3)
069070051202     c                   Eval      h2riga = 18
069080051122     c                   Eval      h2colo = 42
069090051124     c                   Eval      *In90 = *On
069100051122     c                   EndIf
069110051122
069120051122      * --> Controllo
069130051122     c                   If        v2cblc <> *Blanks
069140110323     c                   Clear                   dsbi
069150051122     c                   Clear                   v2dblc
069160051122     c                   Eval      kcod = 'BI'
069170051122     c                   Eval      kkey = v2cblc
069180051122     c     kTabel        Chain     Tabel00f
069190051122     c                   If        Not %Found(Tabel00f) or
069200051125     c                             tblflg <> *Blanks
069210051122     c                   Eval      *In28 = *On
069220051202     c                   Eval      h2riga = 18
069230051122     c                   Eval      h2colo = 42
069240051122     c                   Eval      v2cmsg = msg(25)
069250051122     c                   Leavesr
069260051122     c                   EndIf
069270110323     c                   Eval      dsbi = tbluni
069280110323     c                   Eval      v2dblc = §bides
069290121127      * causale blocco non piuù utilizzabile
069300121127     c                   IF        §BIuso = 'N'
069310121127     c                   Eval      *In28 = *On
069320121127     c                   Eval      h2riga = 18
069330121127     c                   Eval      h2colo = 42
069340121127     c                   Eval      v2cmsg = 'Causale non più utilizzabile'
069350121127     c                   Leavesr
069360121127     c                   ENDIF
069370180129      * causale blocco NON ammesa per lo stato del credito
069380180129     c                   If          §BIbcb =  'S'    and
069390180129     c                             ( V2Ccon =  *blank  or
069400180129     c                               V2Cblc <> §4Wblc )
069410180129     c                   eval      *in28 = *on
069420180129     c                   eval      H2riga = 18
069430180129     c                   eval      H2colo = 42
069440180129     c                   eval      V2Cmsg = 'Causale NON ammessa per lo +
069450180129     c                                       stato del credito "' +
069460180129     c                                       V2Ccon + '"'
069470180129     c                   EndIf
069480051124     c                   EndIf
069490051122
069500051122      * Se cliente bloccato la causale blocco è obbligatoria
069510051122     c                   If        v2cabl <> *Blanks and v2cblc = *Blanks
069520130325     c                             and v2cabl <> '*'
069530051122     c                   Eval      *In28 = *On
069540051202     c                   Eval      h2riga = 18
069550051122     c                   Eval      h2colo = 42
069560051122     c                   Eval      v2cmsg = msg(26)
069570051122     c                   Leavesr
069580051122     c                   EndIf
069590051122
069600051122      * Se immessa la causale blocco il cliente deve essere bloccato
069610051122     c                   If        v2cabl = *Blanks and v2cblc <> *Blanks
069620051122     c                   Eval      *In28 = *On
069630051202     c                   Eval      h2riga = 18
069640051122     c                   Eval      h2colo = 28
069650051122     c                   Eval      v2cmsg = msg(27)
069660051122     c                   Leavesr
069670051122     c                   EndIf
069680110407
069690110407      * Se pgm richiamato per blocco cliente il blocco deve esserci
069700110407     c                   If        $Blocco and V2cabl = *blanks
069710110407     c                   Eval      *In28 = *On
069720110407     c                   Eval      h2riga = 18
069730110407     c                   Eval      h2colo = 28
069740110407     c                   Eval      v2cmsg = 'Immettere il blocco cliente'
069750110407     c                   Leavesr
069760110407     c                   EndIf
069770051122
069780051122     c                   EndSr
069790090616
069800090616      *------------------------------------------------------------------------*
069810090616      * CONTROLLO SOLLECITO
069820090616      *------------------------------------------------------------------------*
069830090616     c     sr_ctrsol     begsr
069840090616
069850090616     c                   reset                   trulsoli1
069860090616     c                   eval      trulsoli1.kut = codut
069870090616     c                   eval      trulsoli1.kcc = v1ckcc
069880090616     c                   eval      trulsoli1.ksc = v1cksc
069890090616     c                   evalr     trulsoli1.con = v2ccon
069900090616     c                   eval      rqsdatasize = %size(trulsoli1)
069910090616     c                   eval      rpydatasize = %size(trulsolo1)
069920090616     c                   call      'TRULSOLR'
069930090616     c                   parm      *blanks       rqsopcode
069940090616     c                   parm      *zeros        rpyesito
069950090616     c                   parm      'TRULSOLI1'   rqsformatname
069960090616     c                   parm                    trulsoli1
069970090616     c                   parm                    rqsdatasize
069980090616     c                   parm      'TRULSOLO1'   rpyformatname
069990090616     c                   parm                    trulsolo1
070000090616     c                   parm                    rpydatasize
070010090617
070020090617      * Immissione
070030090617     c                   if        *in01
070040090617     c                   if        rpyesito = *zeros
070050090617     c                   eval      v2csol = trulsolo1.sol
070060090617     c                   else
070070090617     c                   eval      v2csol = 'S'
070080090617     c                   endif
070090090617     c                   endif
070100090617
070110090617      * Modifica
070120090617     c                   if        rpyesito = *zeros and not *in01 and
070130090617     c                             v2csol <> trulsolo1.sol and
070140090617     c                             trulsolo1.solmod = 'N'
070150090617     c                   Eval      *In28 = *On
070160111115     c                   Eval      h2riga = 19
070170090617     c                   Eval      h2colo = 28
070180090617     c                   if        trulsolo1.sol = 'S'
070190090617     c                   Eval      v2cmsg = msg(83)
070200090617     c                   Leavesr
070210090617     c                   else
070220090617     c                   Eval      v2cmsg = msg(84)
070230090617     c                   Leavesr
070240090617     c                   endif
070250090617     c                   endif
070260090616
070270090616     c                   endsr
070280051122
070290051118      *------------------------------------------------------------------------*
070300051118      * CONTROLLO CODICE COMMERCIALE
070310051118      *------------------------------------------------------------------------*
070320051118     c     Sr_Ctrage     BegSr
070330051118
070340051118      * Ricerca
070350051130     c                   Clear                   v3dage
070360051130     c                   Eval      wpos = %scan('?':v3cage)
070370051118     c                   If        wpos > 0
070380051130     c                   Eval      v3cage = *All'0'
070390130808     c                   clear                   TRMK43ds
070400130806     c  n06              movel     v1cksc        iMK43fil
070410130806     c   06              movel     ta60cli       iMK43fil
070420130806     c                   call      'TRMK43R'
070430130806     c                   parm                    kpjba
070440130806     c                   parm                    TRMK43ds
070450130806     c                   if        oMK43err = *off  and  oMK43fxx = *blank
070460130806     c                   movel     oMK43cmm      v3cage
070470130806     c                   eval      v3dage = oMK43des
070480130806     c                   endif
070490051130     c                   Eval      h3riga = 07
070500051130     c                   Eval      h3colo = 28
070510051124     c                   Eval      *In90 = *On
070520051118     c                   EndIf
070530051122
070540051118      * Controllo (obbligatorio)
070550051130     c                   Clear                   v3dage
070560130806     c                   If        %check(digits:V3Cage) > 0
070570130806     c                   eval      *In28 = *On
070580130806     c                   eval      h3riga = 07
070590130806     c                   eval      h3colo = 28
070600130806     c                   eval      V3Cmsg = Msg(20)
070610130806     c                   leavesr
070620130806     c                   EndIf
070630130806     c                   move      V3Cage        CMMcod
070640130806     c     CMMcod        chain     AZCMM000
070650130806     c                   If        Not %Found(AZCMM01L)
070660051122     c                   Eval      *In28 = *On
070670051130     c                   Eval      h3riga = 07
070680051130     c                   Eval      h3colo = 28
070690051130     c                   Eval      v3cmsg = msg(20)
070700051122     c                   Leavesr
070710051122     c                   EndIf
070720130806     c                   eval      V3Dage = CMMdes
070730051122
070740051122      * Codice commerciale vari valido solo per 8888 e 9999
070750051130     c                   Move      v3cage        w0040
070760130806     c                   IF        CMMpar = '1' and wksc04 <> 8888
070770130806     c                                          and wksc04 <> 9999
070780051122     c                   Eval      *In28 = *On
070790051130     c                   Eval      h3riga = 07
070800051130     c                   Eval      h3colo = 28
070810051130     c                   Eval      v3cmsg = msg(20)
070820051122     c                   Leavesr
070830051122     c                   EndIf
070840110216
070850110216      /free
070860111123       //?Commerciale "valido" data inizio e fine attività
070870130806         IF  CMMdtIni > dataoggi  or
070880130806             CMMdtFin < dataoggi;
070890111123           *in28 = *on;
070900111123           H3riga = 07;
070910111123           H3colo = 28;
070920111123           V3Cmsg = msg(20);
070930111123           leavesr;
070940111123         ENDIF;
070950111123
070960110216       //?Commerciale 'inattivi' possibile solo se
070970110217       //?immissione
070980110216       //?utente EDP
070990110216       //?cliente incassi da attribuire --> 0001 x 1°livello
071000110216       //?                              --> 1000 x 2°livello
071010110217       //?oppure se
071020110217       //?manutenzione
071030110217       //?NO anagrafica provvisoria
071040110217       //?cliente incassi da attribuire = a cod. in tab Y4O
071050130806         IF  CMMpar = '2';
071060110217           SELECT;
071070110217             WHEN  *in02 and not *in06 and %editc(V1Cksc:'X') = wCodIncAtt;
071080160429             WHEN  *in01 and *in20 and
071090121121                   ((sav_livello = '1' and wksc04 = 0001) or
071100121121                    (sav_livello = '2' and wksc04 = 1000) or
071110121121                    (sav_livello = *blanks and wCodIncAtt = *blanks and
071120121121                    (wksc04 = 1000  or wksc04 = 0001)));
071130110217             OTHER;
071140110216              *in28 = *on;
071150110216              H3riga = 07;
071160110216              H3colo = 28;
071170110216              V3Cmsg = msg(20);
071180110216              leavesr;
071190110217           ENDSL;
071200110217         ENDIF;
071210110216      /end-free
071220051122
071230051122      * P.o. del commerciale congruente con p.o. del cliente
071240051221      * solo se non è gestione anagrafica provvisoria di un nuovo cliente
071250051221     c                   If        Not *In06 or ta60cli > *Zeros
071260051122     c  n06              Movel     v1cksc        w0030
071270051122     c   06              Movel     ta60cli       w0030
071280051130     c                   Movel     v3cage        wpoage
071290051122     c                   If        w0030 <> wpoage
071300051122     c                   Eval      *In28 = *On
071310051130     c                   Eval      h3riga = 07
071320051130     c                   Eval      h3colo = 28
071330051130     c                   Eval      v3cmsg = msg(21)
071340051122     c                   Leavesr
071350051122     c                   EndIf
071360051221     c                   EndIf
071370051122
071380051122      * Se commerciale xxx0999 (cliente inattivo) il cliente va bloccato
071390110216      * salto il controllo
071400110216      * il commerciale 'inattivi' non va usato
071410110216     c                   leavesr
071420051130     c                   Move      v3cage        w0040
071430051122     c                   If        w0040 = 999 and v2cabl = *Blanks
071440051122     c                   Eval      *In28 = *On
071450051130     c                   Eval      v3cmsg = msg(22)
071460051122     c                   Leavesr
071470051122     c                   EndIf
071480051118
071490051118     c                   EndSr
071500051122
071510051122      *------------------------------------------------------------------------*
071520051122      * CONTROLLO CODICE IMPORTANZA CLIENTE
071530051122      *------------------------------------------------------------------------*
071540051122     c     Sr_Ctrclv     BegSr
071550100301
071560100301     c                   Clear                   v3dclv
071570100301
071580100301      * se anagrafica provvisoria da trattativa
071590100301      * non è obbligatorio inserire il codice, se a blank vado a fine controlli
071600100806     c                   if        *in06 and v3cclv = *blanks
071610100301     c                   leavesr
071620100301     c                   endif
071630051122
071640051122      * Ricerca
071650051130     c                   Eval      wpos = %scan('?':v3cclv)
071660051122     c                   If        wpos > 0
071670051122     c                   Eval      kcod = 'IC'
071680051122     c                   call      'TRUL19R'
071690051122     c                   parm                    kcod
071700051122     c                   parm                    ordina
071710051122     c                   parm                    kkey
071720051122     c                   parm                    comando
071730051130     c                   Eval      v3cclv = kkey
071740051130     c                   Eval      h3riga = 08
071750051130     c                   Eval      h3colo = 28
071760051124     c                   Eval      *In90 = *On
071770051122     c                   EndIf
071780051122
071790051124      * --> Controllo (obbligatorio)
071800051130     c                   If        v3cclv = *Blanks
071810051124     c                   Eval      *In28 = *On
071820051130     c                   Eval      h3riga = 08
071830051130     c                   Eval      h3colo = 28
071840051130     c                   Eval      v3cmsg = msg(28)
071850051124     c                   Leavesr
071860051124     c                   EndIf
071870051124
071880051130     c                   Clear                   v3dclv
071890051122     c                   Clear                   dsic
071900051122     c                   Eval      kcod = 'IC'
071910051130     c                   Eval      kkey = v3cclv
071920051122     c     kTabel        Chain     Tabel00f
071930051122     c                   If        Not %Found(Tabel00f) or
071940051125     c                             tblflg <> *Blanks
071950051122     c                   Eval      *In28 = *On
071960051130     c                   Eval      h3riga = 08
071970051130     c                   Eval      h3colo = 28
071980051130     c                   Eval      v3cmsg = msg(29)
071990051122     c                   Leavesr
072000051122     c                   EndIf
072010051125     c                   Eval      dsic = tbluni
072020051130     c                   Eval      v3dclv = §sicde
072030051122
072040051122     c                   EndSr
072050051122
072060051122      *------------------------------------------------------------------------*
072070051122      * CONTROLLO CODICE ISTAT
072080051122      *------------------------------------------------------------------------*
072090051122     c     Sr_Ctritc     BegSr
072100051122
072110051122      * Ricerca
072120051130     c                   Clear                   v3ditc
072130051130     c                   Eval      wpos = %scan('?':v3citc)
072140051122     c                   If        wpos > 0
072150051122     c                   Eval      kcod = '1L'
072160051122     c                   call      'TRUL19R'
072170051122     c                   parm                    kcod
072180051122     c                   parm                    ordina
072190051122     c                   parm                    kkey
072200051122     c                   parm                    comando
072210051130     c                   Eval      v3citc = kkey
072220051130     c                   Eval      h3riga = 09
072230051130     c                   Eval      h3colo = 28
072240051124     c                   Eval      *In90 = *On
072250051122     c                   EndIf
072260051122
072270051122      * --> Controllo (obbligatorio)
072280051130     c                   If        v3citc = *Blanks or v3citc = *Zeros
072290051122     c                   Eval      *In28 = *On
072300051130     c                   Eval      h3riga = 09
072310051130     c                   Eval      h3colo = 28
072320051130     c                   Eval      v3cmsg = msg(30)
072330051122     c                   Leavesr
072340051122     c                   EndIf
072350051122
072360051130     c                   Clear                   v3ditc
072370051122     c                   Clear                   ds1l
072380051124     c                   Eval      kcod = '1L'
072390051130     c                   Eval      kkey = v3citc
072400051122     c     kTabel        Chain     Tabel00f
072410051122     c                   If        Not %Found(Tabel00f) or
072420051125     c                             tblflg <> *Blanks
072430051122     c                   Eval      *In28 = *On
072440051130     c                   Eval      h3riga = 09
072450051130     c                   Eval      h3colo = 28
072460051130     c                   Eval      v3cmsg = msg(31)
072470051122     c                   Leavesr
072480051122     c                   EndIf
072490051125     c                   Eval      ds1l = tbluni
072500051130     c                   Eval      v3ditc = §1ldes
072510051122
072520051122     c                   EndSr
072530140722
072540140722      /free
072550140722
072560140722       //--------------------------------------------------------------
072570140722       //?Controllo Note 02/03       (News/Nominativo),                ?
072580140722       //?               05/06/T5/I5 (Responsabile Trasporti),         ?
072590140722       //?               07/08/T7/I7 (Responsabile Logistica).         ?
072600140722       //--------------------------------------------------------------
072610140722       BEGSR  sr_CtrNote;
072620140722
072630140722         // -?Se NON selezionate: esco?
072640140722         select;
072650140722           when  wRGR  = 'RN'  and  V3Cnt02 = *blank;
072660140722             leavesr;
072670140722           when  wRGR  = 'RT'  and  V3Cnt05 = *blank;
072680140722             leavesr;
072690140722           when  wRGR  = 'RL'  and  V3Cnt07 = *blank;
072700140722             leavesr;
072710140722         endsl;
072720140722
072730140722         // -?Se sono in interrogazione non posso interrogare?
072740140722         //  ?   se non ho dati?
072750140722         if  *in05  and  ( (wRGR = 'RN'  and  Not *in24)  or
072760140722                           (wRGR = 'RT'  and  Not *in26)  or
072770140722                           (wRGR = 'RL'  and  Not *in31) );
072780140722           *in28 = *on;
072790140722           select;
072800140722             when  wRGR = 'RN';
072810140722               H3riga = 15;
072820140722             when  wRGR = 'RT';
072830140722               H3riga = 17;
072840140722             when  wRGR = 'RL';
072850140722               H3riga = 19;
072860140722           endsl;
072870140722           H3colo = 35;
072880140722           V3Cmsg = Msg(52);
072890140722           leavesr;
072900140722         endif;
072910140722
072920140722         // -?Imposto se interrogazione o gestione?
072930140722         if  Not *in05;
072940140722           wRich = 'M';
072950140722         else;
072960140722           wRich = 'V';
072970140722         endif;
072980140722         wRag  = V3Crag;
072990140722         clear  wTnt;
073000140722         //wRGR  = 'RN'       ?(già impostato)?
073010140722
073020140722         exsr  sr_CallTNTA12;
073030140722
073040140722         if  TA12err <> *blank;
073050140722           *in28 = *on;
073060140722           select;
073070140722             when  wRGR = 'RN';
073080140722               H3riga = 15;
073090140722             when  wRGR = 'RT';
073100140722               H3riga = 17;
073110140722             when  wRGR = 'RL';
073120140722               H3riga = 19;
073130140722           endsl;
073140140722           H3colo = 35;
073150140722           V3Cmsg = TA12msg;
073160140722           leavesr;
073170140722         endif;
073180140722
073190140722         // -?Controllo se ora c'è o non c'è più alcuna nota?
073200140722         kNtcNk1 = %editc( DUTkci : 'X' );
073210140722         if  *in70  or  Not *in06;
073220140722           kNtcApl = 'C';
073230140722           kNtcNk1 = %trim(kNtcNk1) + %editc( V1Cksc : 'X' );
073240140722         else;
073250140722           kNtcApl = 'T';
073260140722           kNtcNk1 = %trim(kNtcNk1) + %editc( V1Cnrv : 'X' );
073270140722         endif;
073280140722         clear  kNtcNk2;
073290140722         select;
073300140722           // -?Note 02/03?
073310140722           when  wRGR  = 'RN';
073320140722             *in24 = *off;
073330140722             exsr  sr_CarNoteRN;
073340140722           // -?Note 05/06/T5/I5?
073350140722           when  wRGR  = 'RT';
073360140722             *in26 = *off;
073370140722             exsr  sr_CarNoteRT;
073380140722           // -?Note 07/08/T7/I7?
073390140722           when  wRGR  = 'RL';
073400140722             *in31 = *off;
073410140722             exsr  sr_CarNoteRL;
073420140722         endsl;
073430140722
073440140722         // -?Uscita?
073450140722         select;
073460140722           when  wRGR  = 'RN';
073470140722             clear  V3Cnt02;
073480140722             H3riga = 15;
073490140722           when  wRGR  = 'RT';
073500140722             clear  V3Cnt05;
073510140722             H3riga = 17;
073520140722           when  wRGR  = 'RL';
073530140722             clear  V3Cnt07;
073540140722             H3riga = 19;
073550140722         endsl;
073560140722         H3colo = 35;
073570140722
073580140722         *in90 = *on;
073590140722
073600140722       ENDSR;
073610140722
073620140722       //--------------------------------------------------------------
073630140722       //?Visualizzazione Contatti/Rubrica del cliente.                ?
073640140722       //--------------------------------------------------------------
073650140722       BEGSR  sr_CallTNTA12;
073660140722
073670140722         clear  TNTA12ds;
073680140722
073690140722         // -?Impostazione parametri?
073700140722         TA12ric  = wRich;
073710140722         if  *in70  or  Not *in06;
073720140722           TA12apl  = 'C';
073730140722           TA12ksc  = V1Cksc;
073740140722         else;
073750140722           TA12apl  = 'T';
073760140722           TA12nrv  = V1Cnrv;
073770140722         endif;
073780140722         TA12rag  = wRag;
073790140722         TA12tnt  = wTnt;
073800140722         TA12gcli = '1';
073810140722         if  *in01   or
073820140722            (Not *in01  and  Not *in05  and
073830140722             %parms() > 1    and
073840140722             TA60icli = '1');
073850140722           TA12icli = '1';
073860140722           TA12cmm  = %int(V3Cage);
073870140722         endif;
073880140722         TA12rgr  = wRGR;
073890140722
073900140722         // -?Richiamo *pgm TNTA12R?
073910140722         tnta12r ( KPJBA : TNTA12ds );
073920140722
073930140722       ENDSR;
073940140722
073950140722      /end-free
073960051125
073970051125      *------------------------------------------------------------------------*
073980051125      * CONTROLLO CODICE SOTTOCONTO FATTURAZIONE
073990051125      *------------------------------------------------------------------------*
074000051125     c     Sr_Ctrscf     BegSr
074010100127
074020100127      * gestisco il '?'
074030100729     c                   IF        %scan('?':v4cscf) > 0
074040100312      * e se c'è la partita IVA e non è $$
074050100312     c                             and v2civa <> *blanks and
074060100312     c                             v2ivaeu <> '$$'
074070100128     c                   exsr      sr_ta40
074080100127     c                   eval      h4riga = 07
074090100127     c                   eval      h4colo = 36
074100100127     c                   eval      *in90 = *on
074110100312     c                   leavesr
074120100127     c                   ENDIF
074130051125
074140051221      * Obbligatorio
074150100128     c                   If        (v4cscf = *Zeros or v4cscf = *blanks)
074160051221      * se non sono in gestione anagrafica provvisoria
074170051221     c                             and Not *In06
074180051125     c                   Eval      *In28 = *On
074190051130     c                   Eval      h4riga = 07
074200051130     c                   Eval      h4colo = 36
074210051130     c                   Eval      v4cmsg = msg(33)
074220051125     c                   Leavesr
074230051125     c                   EndIf
074240100127
074250100127      * deve essere numerico
074260100127     c                   IF        %check(digits:V4cscf) > 0
074270100127     c                   eval      *in28 = *on
074280100127     c                   eval      h4riga = 07
074290100127     c                   eval      h4colo = 36
074300100127     c                   eval      v4cmsg = msg(35)
074310100127     c                   leavesr
074320100127     c                   ENDIF
074330100316
074340100316      * se sono in immissione (non da trattativa)
074350100316      * non controllo esistenza del codice sottoconto fattura se = al
074360100316      * codice cliente che sto inserendo
074370170516      // non faccio questo controllo se cliente LOGISTICA
074380170516        IF  not ClienteLog;
074390100316     c                   if        *in01 and not *in06 and
074400100316     c                             %dec(v4cscf:7:0) = v1cksc
074410100316     c                   eval      v4dscf = v2crag
074420100316     c                   leavesr
074430100316     c                   endif
074440170516       ENDIF;
074450051125
074460051125      * Controllo se modificato il codice
074470170117      * o se convalida trattativa
074480100127     c                   If        v4cscf <> savscf
074490170117     c                             or *in07
074500051130     c                   Clear                   v4dscf
074510051125      * Controllo
074520051125     c                   Clear                   Tibs69ds
074530051130     c                   Move      v4cscf        I69kac
074540161129     c                   Move      v4cscf        I69kcp
074550051125     c                   Call      'TIBS69R'
074560051125     c                   Parm                    Tibs69ds
074570051125     c                   Parm                    ds_cnaco
074580051125     c                   Parm                    ds_cnind
074590051125     c                   Parm                    ds_cnclp
074600051125     c                   Parm                    ds_fncls
074610051125     c                   Eval      wtibs69 = *On
074620051125
074630051125      * Il sottoconto deve esistere
074640051125     c                   If        o69err <> *Blanks
074650051125     c                   Eval      *In28 = *On
074660051130     c                   Eval      h4riga = 07
074670051130     c                   Eval      h4colo = 36
074680051130     c                   Eval      v4cmsg = msg(35)
074690051125     c                   Leavesr
074700051125     c                   EndIf
074710051125
074720051125      * Non deve essere annullato
074730051125     c                   If        w_acoflg <> *Blanks
074740051125     c                   Eval      *In28 = *On
074750051130     c                   Eval      h4riga = 07
074760051130     c                   Eval      h4colo = 36
074770051130     c                   Eval      v4cmsg = msg(36)
074780051125     c                   Leavesr
074790051125     c                   EndIf
074800051125
074810051125      * Non si può inserire un sottoconto 8888 o 9999
074820070704      * E' possibile solo per clienti 8888 e 9999
074830070704     c                   Move      v4cscf        w0040
074840070704     c                   move      v1cksc        w0040ksc
074850070704     c                   if        *in06='1'
074860070704     c                             or (w0040ksc=8888 and w0040<>8888)
074870070704     c                             or (w0040ksc=9999 and w0040<>9999)
074880070704     c                             or (w0040ksc<>8888 and w0040ksc<>9999)
074890051125     c                   If        w0040 = 8888 or w0040 = 9999
074900051125     c                   Eval      *In28 = *On
074910051130     c                   Eval      h4riga = 07
074920051130     c                   Eval      h4colo = 36
074930051130     c                   Eval      v4cmsg = msg(34)
074940051125     c                   Leavesr
074950051125     c                   EndIf
074960070704     c                   EndIf
074970161130      /free
074980161130       //?Non posso immettere un codice sottoconto fattura in contenzioso
074990170117        IF  %dec(v4cscf:7:0) <> v1cksc;
075000170117          clear TIBS69DS;
075010170117          I69kcp = %dec(V4Cscf:7:0);
075020170117          tibs69r (TIBS69DS:ds_CNACO:ds_CNIND:ds_CNCLP:ds_FNCLS);
075030170117          wtibs69 = *on;
075040170117          IF  w_CLPcon <> *blanks;
075050170117            clear w_ds4W;
075060170117            kcod = '4W';
075070170117            kkey = w_CLPcon;
075080170117            chain (codut:kcod:kkey) TABEL00F;
075090170117            IF  %found(TABEL00F) and TBLflg = *blanks;
075100170117              w_ds4W = TBLuni;
075110170117            ENDIF;
075120170117            IF  w_§4Wtip <> *blanks;
075130170117              *in28 = *on;
075140170117              H4riga = 07;
075150170117              H4colo = 36;
075160170117              V4Cmsg = msg(73);
075170170117              leavesr;
075180170117            ENDIF;
075190170117          ENDIF;
075200161130        ENDIF;
075210161130      /end-free
075220051125
075230051125      * Cerco il network del codice
075240051130     c                   Movel     v4cscf        w0030
075250051125     c                   Clear                   og143
075260051125     c     w0030         Chain     Azorg01l
075270051125     c                   If        %Found(Azorg01l)
075280051125     c                   Eval      og143 = orgde3
075290051125     c                   EndIf
075300120806      * Se non trovo il network e sono in anafrafica provvisoria vado avanti
075310120806     c                   If        not %found(AZORG01L) and not *in06
075320120806     c                             and Not *In06
075330120806     c                   Eval      *In28 = *On
075340120806     c                   Eval      h4riga = 07
075350120806     c                   Eval      h4colo = 36
075360120806     c                   Eval      v4cmsg = msg(35)
075370120806     c                   Leavesr
075380120806     c                   ENDIF
075390081119     c                   eval      savntwscf = §ogntw
075400170516
075410170516       // Per i clienti LOG come sottoconto intestazione fattura accetto
075420170516       // solo codici di spedizione BRT
075430170516         IF  ClienteLog and §OGntw = 'LOG';
075440170516           *in28 = *on;
075450170516           H4riga = 07;
075460170516           H4colo = 36;
075470170516           V4Cmsg = msg(34);
075480170516           leavesr;
075490170516         ENDIF;
075500160905
075510160905      * Se non sono autorizzato non posso utilizzare lienti con ntw LOG
075520160905     c                   IF         §UTEKscLog <> 'S' and
075530160905     c                              §OGntw = 'LOG'
075540160905     c                   Eval      *In28 = *On
075550160905     c                   Eval      h4riga = 07
075560160905     c                   Eval      h4colo = 36
075570160905     c                   Eval      v4cmsg = msg(34)
075580160905     c                   Leavesr
075590160905     c                   ENDIF
075600051125
075610051125      * Se sono in filiale non posso inserire codici con ntw LOG o XXX
075620051125     c                   If        Not *In09 and
075630160905     c                             §ogntw = 'XXX'
075640051125     c                   Eval      *In28 = *On
075650051130     c                   Eval      h4riga = 07
075660051130     c                   Eval      h4colo = 36
075670051130     c                   Eval      v4cmsg = msg(34)
075680051125     c                   Leavesr
075690051125     c                   EndIf
075700051220
075710051220      * Non posso modificare un sottoconto fattura con p.o. 046
075720051220     c                   Movel     savscf        w0030
075730051220     c                   If        w0030 = 046
075740051220     c                   Eval      *In28 = *On
075750051220     c                   Eval      h4riga = 07
075760051220     c                   Eval      h4colo = 36
075770051220     c                   Eval      v4cmsg = msg(71)
075780051220     c                   Leavesr
075790051220     c                   EndIf
075800051220
075810081119      * Non posso inserire un sottoconto fattura con ntw LOG se ksc non
075820081119      * ha ntw LOG
075830081119     c                   Movel     v1cksc        w0030
075840081119     c                   Clear                   og143
075850081119     c     w0030         Chain     Azorg01l
075860081119     c                   If        %Found(Azorg01l)
075870081119     c                   Eval      og143 = orgde3
075880081119     c                   EndIf
075890081119     c                   if        §ogntw <> 'LOG' and savntwscf = 'LOG'
075900081119     c                   Eval      *In28 = *On
075910081119     c                   Eval      h4riga = 07
075920081119     c                   Eval      h4colo = 36
075930081119     c                   Eval      v4cmsg = msg(34)
075940081119     c                   Leavesr
075950081119     c                   EndIf
075960100127
075970100127      * devo ricontrollare la partita iva e il codice fiscale
075980100127      * perchè i controlli di questi 2 dati sono legati anche al codice
075990100127      * fatturazione
076000100127     c                   exsr      sr_ctrCDF
076010100127      * se torna errore avviso
076020100127     c                   IF        *in28
076030100127     c                   eval      v4cmsg = 'Messaggi in sospeso su -
076040100127     c                             altre videate.'
076050100127     c                   leavesr
076060100127     c                   ENDIF
076070100127     c                   exsr      sr_ctrIVA
076080100127      * se torna errore avviso
076090100127     c                   IF        *in28
076100100127     c                   eval      v4cmsg = 'Messaggi in sospeso su -
076110100127     c                             altre videate.'
076120100127     c                   leavesr
076130100127     c                   ENDIF
076140051125
076150051125      * Decodifico
076160051130     c                   Eval      v4dscf = w_acorag
076170051130     c                   Eval      savscf = v4cscf
076180051128
076190051125     c                   EndIf
076200051125
076210051125     c                   EndSr
076220051128
076230051128      *------------------------------------------------------------------------*
076240051128      * CONTROLLO FLAG FATTURA UNIFICATA
076250051128      *------------------------------------------------------------------------*
076260051128     c     Sr_Ctruni     BegSr
076270100127
076280100127     c                   move      v4cscf        w0070
076290051128
076300051128      * Se il codice sottoconto fatturazione è diverso dal codice cliente e
076310051128      * il flag di unificazione fattura non è stato impostato emetto un msg. inf
076320100127     c                   If        w0070  <> v1cksc and v4cuni = *Blanks and
076330100127     c                             wfscf = *Blanks and w0070  <> *Zeros
076340051128      * e non è da offerta
076350051128     c                             and not *In06
076360051128     c                   Eval      *In28 = *On
076370051130     c                   Eval      h4riga = 08
076380051130     c                   Eval      h4colo = 36
076390051130     c                   Eval      v4cmsg = msg(45)
076400051128     c                   Eval      wfscf = '1'
076410051128     c                   Leavesr
076420051128     c                   EndIf
076430051128
076440051128      * se modificato flag fattura unificata msg info x controllare anche il
076450051128      * flag nel sottoconto fattura
076460051128      * se immissione messaggio solo se codice cliente diverso da sottoconto fat
076470051128      * se manutenzione diversifico i messaggi
076480051130     c                   If        v4cuni <> savuni
076490051130     c                   Eval      savuni = v4cuni
076500051128     c                   Select
076510100127     c                   When      w0070  <> v1cksc
076520051128     c                   Eval      *In28 = *On
076530051130     c                   Eval      h4riga = 08
076540051130     c                   Eval      h4colo = 36
076550051130     c                   Eval      v4cmsg = msg(46)
076560051128     c                   Leavesr
076570100127     c                   When      w0070  = v1cksc and Not *In01
076580051128     c                             and Not *In07
076590051128     c                   Eval      *In28 = *On
076600051130     c                   Eval      h4riga = 08
076610051130     c                   Eval      h4colo = 36
076620051130     c                   Eval      v4cmsg = msg(47)
076630051128     c                   Leavesr
076640051128     c                   EndSl
076650051128     c                   EndIf
076660051128
076670051128     c                   EndSr
076680051125
076690051125      *------------------------------------------------------------------------*
076700051125      * CONTROLLO TIPO FATTURA
076710051125      *------------------------------------------------------------------------*
076720051125     c     Sr_Ctrtft     BegSr
076730051125
076740051125      * Ricerca
076750051130     c                   Clear                   v4dtft
076760051130     c                   If        v4ctft = '?'
076770051125     c                   Clear                   tibs02ds
076780051125     c                   Eval      t02mod = 'R'
076790051125     c                   Eval      t02sif = knsif
076800051125     c                   Eval      t02cod = 'TFT'
076810051125     c                   Call      'TIBS02R'
076820051125     c                   Parm                    kpjba
076830051125     c                   Parm                    tibs02ds
076840051125     c                   If        t02err = *Blanks
076850051130     c                   Eval      v4ctft = t02ke1
076860051125     c                   Eval      dtft = t02uni
076870051130     c                   Eval      v4dtft = §tftdes
076880051130     c                   Eval      h4riga = 09
076890051130     c                   Eval      h4colo = 36
076900051125     c                   Eval      *In90 = *On
076910051125     c                   Leavesr
076920051125     c                   EndIf
076930051128     c                   EndIf
076940051125
076950051125      * --> Controllo (obbligatorio in tabella non esiste il ' ')
076960051125     c                   Clear                   Tibs02ds
076970051125     c                   Eval      t02mod = 'C'
076980051125     c                   Eval      t02sif = knsif
076990051125     c                   Eval      t02cod = 'TFT'
077000051130     c                   Eval      t02ke1 = v4ctft
077010051125     c                   Call      'TIBS02R'
077020051125     c                   Parm                    kpjba
077030051125     c                   Parm                    Tibs02ds
077040051125     c                   If        t02err <> *Blanks
077050051125     c                   Eval      *In28 = *On
077060051130     c                   Eval      h4riga = 09
077070051130     c                   Eval      h4colo = 36
077080051130     c                   Eval      v4cmsg = msg(37)
077090051125     c                   Leavesr
077100051125     c                   EndIf
077110051125     c                   Eval      dtft = t02uni
077120051130     c                   Eval      v4dtft = §tftdes
077130051125
077140051125      * Controllo uso sede/p.o.
077150051125      * solo se variato
077160051130     c                   If        v4ctft <> savtft
077170051125     c                   Select
077180051125     c                   When      Not *In09 and §tftuti = 'S'
077190051125     c                   Eval      *In28 = *On
077200051130     c                   Eval      h4riga = 09
077210051130     c                   Eval      h4colo = 36
077220051130     c                   Eval      v4cmsg = msg(38)
077230051130     c                   Eval      v4cmsg = %trim(v4cmsg) + ' ' +
077240051125     c                             'filiale'
077250051125     c                   Leavesr
077260051125     c                   When      *In09 and §tftuti = 'F'
077270051125     c                   Eval      *In28 = *On
077280051130     c                   Eval      h4riga = 09
077290051130     c                   Eval      h4colo = 36
077300051130     c                   Eval      v4cmsg = msg(38)
077310051130     c                   Eval      v4cmsg = %trim(v4cmsg) + ' ' +
077320051125     c                             'sede'
077330051125     c                   Leavesr
077340051125     c                   EndSl
077350051125     c                   EndIf
077360090417
077370090417     c                   clear                   conta
077380090417
077390090417      * se non è anagrafica provvisoria
077400090417      * se tipo fattura è distinta verifico se il codice è un sottoconto
077410090417      * fattura e se i codici a lui collegati hanno la fatturazione
077420090417      * settimanale, in questo caso devo dare errore
077430090417     c                   if        not *in06 and
077440100127     c                             %editc(v1cksc:'X') = v4cscf
077450100127     c                             and v4ctft = '1'
077460090417     c/exec sql
077470090417     c+ select count(*) into :conta from cnclp00f where
077480090417     c+ clpkcc = 151 and clpscf = :v1cksc and clpfft = 1
077490090417     c/end-exec
077500090417      * se ho trovato almeno un rcd vuol dire che uno dei figli
077510090417      * legati al sottoconto fatturazione che sto modificando ha
077520090417      * la frequenza settimanale
077530090417     c                   if        conta > *zeros
077540090417     c                   eval      *in28 = *on
077550090417     c                   eval      h4riga = 09
077560090417     c                   eval      h4colo = 36
077570090417     c                   eval      v4cmsg = msg(82)
077580090417     c                   leavesr
077590090417     c                   endif
077600090417     c                   endif
077610051125
077620051125     c                   EndSr
077630051125
077640051125      *------------------------------------------------------------------------*
077650051125      * CONTROLLO FREQUENZA FATTURA
077660051125      *------------------------------------------------------------------------*
077670051125     c     Sr_Ctrfft     BegSr
077680051125
077690051125      * Ricerca
077700051130     c                   Clear                   v4dfft
077710051130     c                   If        v4cfft = '?'
077720051125     c                   Eval      kcod = 'FF'
077730051125     c                   call      'TRUL19R'
077740051125     c                   parm                    kcod
077750051125     c                   parm                    ordina
077760051125     c                   parm                    kkey
077770051125     c                   parm                    comando
077780051130     c                   Eval      v4cfft = kkey
077790051130     c                   Eval      h4riga = 10
077800051130     c                   Eval      h4colo = 36
077810051125     c                   Eval      *In90 = *On
077820051125     c                   EndIf
077830051125
077840051125      * --> Controllo (obbligatorio)
077850051130     c                   If        v4cfft = *Blanks
077860051125     c                   Eval      *In28 = *On
077870051130     c                   Eval      h4riga = 10
077880051130     c                   Eval      h4colo = 36
077890051130     c                   Eval      v4cmsg = msg(39)
077900051125     c                   Leavesr
077910051125     c                   EndIf
077920051125
077930051130     c                   Clear                   v4dfft
077940051125     c                   Eval      kcod = 'FF'
077950051130     c                   Eval      kkey = v4cfft
077960051125     c     kTabel        Chain     Tabel00f
077970051125     c                   If        Not %Found(Tabel00f) or
077980051125     c                             tblflg <> *Blanks
077990051125     c                   Eval      *In28 = *On
078000051130     c                   Eval      h4riga = 10
078010051130     c                   Eval      h4colo = 36
078020051130     c                   Eval      v4cmsg = msg(40)
078030051125     c                   Leavesr
078040051125     c                   EndIf
078050051125     c                   Eval      dsff = tbluni
078060051130     c                   Eval      v4dfft = §ffdes
078070051125
078080051125      * Se tipo fattura '1' non è ammessa la frequenza fattura <> dal mensile
078090051220     c                   If        v4ctft = '1' and v4cfft <> '4'
078100051125     c                   Eval      *In28 = *On
078110051130     c                   Eval      h4riga = 10
078120051130     c                   Eval      h4colo = 36
078130051130     c                   Eval      v4cmsg = msg(41)
078140051125     c                   Leavesr
078150051125     c                   EndIf
078160051125
078170120613      * La frequenza fattura settimanale (1) è ammessa solo se
078180120613      * utente abilitato
078190120613     c                   If        v4cfft = '1' and
078200120613     c                             §UTEclpfft <> 'S' and
078210051130     c                             v4cfft <> savfft
078220051125     c                   Eval      *In28 = *On
078230051130     c                   Eval      h4riga = 10
078240051130     c                   Eval      h4colo = 36
078250051130     c                   Eval      v4cmsg = msg(42)
078260051125     c                   Leavesr
078270051125     c                   EndIf
078280090416
078290090416      * se frequenza fattura settimanale controllo il tipo fattura del
078300090416      * sottoconto fatturazione (se diverso)
078310100127     c                   if        v4cfft = '1' and
078320100127     c                             v4cscf <> %editc(v1cksc:'X')
078330100127     c                   move      v4cscf        w0070
078340090416      * il sottoconto fatturazione non deve avere il tipo fattura distinta
078350090417     c/exec sql
078360090417     c+ select clptft into:w_clptft from cnclp00f where
078370100127     c+ clpkcc = 151 and clpksc = :w0070
078380090417     c/end-exec
078390090417     c                   if        sqlcod = 0 and w_clptft = 1
078400090416     c                   eval      *in28 = *on
078410090416     c                   eval      h4riga = 10
078420090416     c                   eval      h4colo = 36
078430090416     c                   eval      v4cmsg = msg(81)
078440090416     c                   leavesr
078450090417     c                   endif
078460090416     c                   endif
078470051125
078480051125     c                   EndSr
078490051125
078500051125      *------------------------------------------------------------------------*
078510051125      * CONTROLLO TIPO DATA FATTURA
078520051125      *------------------------------------------------------------------------*
078530051125     c     Sr_Ctrtdf     BegSr
078540051125
078550051125      * Ricerca
078560051130     c                   Clear                   v4dtdf
078570051130     c                   If        v4ctdf = '?'
078580051125     c                   Eval      kcod = '13'
078590051125     c                   call      'TRUL19R'
078600051125     c                   parm                    kcod
078610051125     c                   parm                    ordina
078620051125     c                   parm                    kkey
078630051125     c                   parm                    comando
078640051130     c                   Eval      v4ctdf = kkey
078650051130     c                   Eval      h4riga = 11
078660051130     c                   Eval      h4colo = 36
078670051125     c                   Eval      *In90 = *On
078680051125     c                   EndIf
078690051125
078700051125      * --> Controllo (obbligatorio)
078710051130     c                   If        v4ctdf = *Blanks
078720051125     c                   Eval      *In28 = *On
078730051130     c                   Eval      h4riga = 11
078740051130     c                   Eval      h4colo = 36
078750051130     c                   Eval      v4cmsg = msg(43)
078760051125     c                   Leavesr
078770051125     c                   EndIf
078780051125
078790051130     c                   Clear                   v4dtdf
078800051125     c                   Eval      kcod = '13'
078810051130     c                   Eval      kkey = v4ctdf
078820051125     c     kTabel        Chain     Tabel00f
078830051125     c                   If        Not %Found(Tabel00f) or
078840051125     c                             tblflg <> *Blanks
078850051125     c                   Eval      *In28 = *On
078860051130     c                   Eval      h4riga = 11
078870051130     c                   Eval      h4colo = 36
078880051130     c                   Eval      v4cmsg = msg(44)
078890051125     c                   Leavesr
078900051125     c                   EndIf
078910051125     c                   Eval      ds13 = tbluni
078920051130     c                   Eval      v4dtdf = §13des
078930051125
078940051125     c                   EndSr
078950051128
078960051128      *------------------------------------------------------------------------*
078970051128      * CONTROLLO SPESE INVIO FATTURA
078980051128      *------------------------------------------------------------------------*
078990051128     c     Sr_Ctrsin     BegSr
079000051128
079010051130     c                   If        v4csin > *Zeros
079020051128     c                   Reset                   xdecds
079030051130     c                   Z-add     v4csin        icdnum
079040051128     c                   Z-add     2             icdnrd
079050051128     c                   Call      'XDEC'
079060051128     c                   Parm                    xdecds
079070051128     c                   If        ocdesi <> *Off
079080051128     c                   Eval      *In28 = *On
079090051130     c                   Eval      h4riga = 12
079100051130     c                   Eval      h4colo = 36
079110051130     c                   Eval      v4cmsg = msg(48)
079120051128     c                   Leavesr
079130051128     c                   EndIf
079140051128     c                   EndIf
079150051128
079160051128     c                   EndSr
079170051128
079180051128      *------------------------------------------------------------------------*
079190051128      * CONTROLLO LUOGO 001 - INVIO FATTURA
079200051128      *------------------------------------------------------------------------*
079210051128     c     Sr_Ctrl001    BegSr
079220051128
079230051130if  1c                   If        v4cl001 <> *Blanks
079240051128
079250060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
079260060203     c                   If        *In05 and Not *In10
079270051128     c                   Eval      *In28 = *On
079280100727     c                   Eval      h4riga = 21
079290060203     c                   Eval      h4colo = 35
079300051130     c                   Eval      v4cmsg = msg(50)
079310051128     c                   Leavesr
079320051128     c                   EndIf
079330071128
079340071128      * se non è interrogazione controllo se luogo utilizzabile
079350071128     c                   if        not *in09 and not *in05 and $ufi001 <> 'S'
079360071128     c                   eval      *In28 = *on
079370100727     c                   eval      h4riga = 21
079380071128     c                   eval      h4colo = 35
079390071128     c                   eval      v4cmsg = msg(76)
079400071128     c                   leavesr
079410071128     c                   endif
079420051128
079430060203      * Imposto se interrogazione o gestione
079440051213     c                   Clear                   tnta81ds
079450060203     c   05              Eval      i81opz = '5'
079460060203     c  n05
079470060203     can 10              Eval      i81opz = '2'
079480051213     c                   Eval      i81cod = '001'
079490051213     c                   Eval      i81cli = v1cksc
079500060216     c                   Clear                   parmf11
079510060216     c                   Eval      i81gcli = '1'
079520060216     c   01              Eval      i81icli = '1'
079530060216     c                   Eval      kpjbu = parmf11
079540051213     c                   Call      'TNTA81R'
079550051213     c                   Parm                    kpjba
079560051213     c                   Parm                    tnta81ds
079570051128
079580051212      * Controllo se ora c'è o non c'è più il luogo
079590051216     c                   Eval      kspecli = v1cksc
079600051216     c                   Eval      kspecod = '001'
079610051128     c     kFnspe        Chain     Fnspe01l
079620051128     c                   If        %Found(Fnspe01l) and speflg = *Blanks
079630051128     c                   Eval      *In10 = *On
079640051212     c                   Else
079650051212     c                   Eval      *In10 = *Off
079660051128     c                   EndIf
079670051128
079680051130     c                   Clear                   v4cl001
079690100727     c                   Eval      h4riga = 21
079700060203     c                   Eval      h4colo = 35
079710051207     c                   Goto      emi04
079720051128
079730051129    1c                   EndIf
079740051128
079750051128     c                   EndSr
079760051122
079770051129      *------------------------------------------------------------------------*
079780051220      * CONTROLLO NOTA 84 - INVIO FATTURA
079790051129      *------------------------------------------------------------------------*
079800051129     c     Sr_Ctrnt84    BegSr
079810051129
079820051130if  1c                   If        v4cnt84 <> *Blanks
079830060203
079840060203      * Se sono in interrogazione non posso interrogare la nota se non c'è
079850060203     c                   If        *In05 and Not *In21
079860060203     c                   Eval      *In28 = *On
079870060203     c                   Eval      h4riga = 21
079880100727     c                   Eval      h4colo = 70
079890060203     c                   Eval      v4cmsg = msg(52)
079900060203     c                   Leavesr
079910060203     c                   EndIf
079920060203
079930060203      * Imposto se interrogazione o gestione
079940060203     c  n05              Eval      wrich = 'M'
079950060203     c   05              Eval      wrich = 'V'
079960060217     c                   Eval      wrag = v3crag
079970051212     c                   Eval      wtnt = '84'
079980051212     c                   ExSr      Sr_f02
079990060215
080000060215     c                   If        ta12err <> *Blanks
080010060215     c                   Eval      *In28 = *On
080020060215     c                   Eval      h4riga = 21
080030100727     c                   Eval      h4colo = 70
080040060215     c                   Eval      v4cmsg = ta12msg
080050060215     c                   Leavesr
080060060215     c                   EndIf
080070051129
080080060215      * Controllo se ora c'è o non c'è più la nota
080090091112     c   70
080100091112     corn06              Eval      kntcapl = 'C'
080110100806     c   06
080120091112     cann70              Eval      kntcapl = 'T'
080130051216     c                   Movel(p)  dutkci        kntcnk1
080140091112     c   70
080150091112     corn06              Move      v1cksc        kntcnk1
080160100806     c   06
080170100806     cann70              Move      v1cnrv        kntcnk1
080180051216     c                   Clear                   kntcnk2
080190051216     c                   Movel     '84'          kntctnt
080200091119     c     kTfntc        Chain(n)  Tfntc01l
080210051129     c                   If        %Found(Tfntc01l)
080220051129     c                   Eval      *In21 = *On
080230051212     c                   Else
080240051212     c                   Eval      *In21 = *Off
080250051129     c                   EndIf
080260051129
080270051130     c                   Clear                   v4cnt84
080280051212     c                   Eval      h4riga = 21
080290100727     c                   Eval      h4colo = 70
080300051212     c                   Goto      emi04
080310051129
080320051129    1c                   EndIf
080330051129
080340051129     c                   EndSr
080350051129
080360051129      *------------------------------------------------------------------------*
080370051130      * CONTROLLO CODICE PAGAMENTO FATTURE
080380051129      *------------------------------------------------------------------------*
080390051129     c     Sr_Ctrcdp     BegSr
080400051129
080410051129      * Ricerca
080420100727     c                   Eval      wpos = %scan('?':v4ccdp)
080430051129     c                   If        wpos > 0
080440100727     c                   Clear                   v4dcdp
080450100727     c                   Clear                   v4ccdp
080460051129     c                   Call      'XCDPAGR'
080470100727     c                   Parm                    v4ccdp
080480100727     c                   Parm                    v4dcdp
080490100728     c                   Eval      h4riga = 13
080500100728     c                   Eval      h4colo = 36
080510051129     c                   Eval      *In90 = *On
080520100727     c                   If        v4ccdp = '999'
080530100727     c                   Clear                   v4ccdp
080540051129     c                   EndIf
080550051129     c                   EndIf
080560051129
080570051129      * Controllo
080580100727     c                   Clear                   v4dcdp
080590051129     c                   Clear                   dsfa
080600051129     c                   Eval      kcod = 'FA'
080610100727     c                   Movel     v4ccdp        w004a
080620051129     c                   Move      '1'           w004a
080630051129     c                   Eval      kkey = w004a
080640051129     c     kTabel        Chain     Tabel00f
080650051129     c                   If        Not %Found(Tabel00f) or
080660051129     c                             tblflg <> *Blanks
080670051129     c                   Eval      *In28 = *On
080680100728     c                   Eval      h4riga = 13
080690100728     c                   Eval      h4colo = 36
080700100728     c                   Eval      v4cmsg = msg(53)
080710051129     c                   Leavesr
080720051129     c                   EndIf
080730051129     c                   Eval      dsfa = tbluni
080740100727     c                   Eval      v4dcdp = §fades
080750051129
080760051129      * Se capoconto cliente non posso utilizzare il codice 3
080770051129     c                   If        v1ckcc = 151 and §fatpg = '3'
080780051129     c                   Eval      *In28 = *On
080790100728     c                   Eval      h4riga = 13
080800100728     c                   Eval      h4colo = 36
080810100728     c                   Eval      v4cmsg = msg(53)
080820051129     c                   Leavesr
080830051129     c                   EndIf
080840080515
080850080515      * se utente di filiale non può inerire un pagamento con codice '2' RID
080860090416      * ora controllo il tipo utilizzo se S è solo SEDE
080870080515     c                   if        not *in09 and
080880100727     c                             v4ccdp <> %subst(indcdp:4:3) and
080890090421     c                             §fauti = 'S'
080900080515     c                   eval      *In28 = *On
080910100728     c                   eval      h4riga = 13
080920100728     c                   eval      h4colo = 36
080930100728     c                   eval      v4cmsg = msg(80)
080940080515     c                   leavesr
080950080515     c                   endif
080960150415      /free
080970160517       //?Se il cliente è sottoconto intestazione fattura
080980160517       //?controllo se OK RIBA
080990160518         IF  %editc(V1Cksc:'X') = V4Cscf and §FAtpg = '1';
081000160518       //?No RIBA (tipo pag. 1) se P.IVA estera
081010160518           IF  (V2ivaeu <> *blanks and V2ivaeu <> '$$' and
081020160518                V2ivaeu <> 'IT' and V2ivaeu <> 'SM') or
081030160518               (V2ivaeu = '$$' and V2Ccdf = *blanks);
081040160517             *in28 = *on;
081050160517             H4riga = 13;
081060160517             H4colo = 36;
081070160517             V4Cmsg = msg(89);
081080160517             leavesr;
081090160517           ENDIF;
081100160517       //?No RIBA (tipo pag. 1) su conto Bancoposta (ABI 07601)
081110160518           IF  V4Cabi = 07601;
081120160517             *in28 = *on;
081130160517             H4riga = 13;
081140160517             H4colo = 36;
081150160517             V4Cmsg = msg(90);
081160160517             leavesr;
081170160518           ENDIF;
081180160517         ENDIF;
081190160517
081200150415       //?Se utente di filiale e tipo pagamento utilizzabile solo da sede
081210150415       //?devo proteggere la Banca ABI/CAB del pagamento fatture
081220150415         *in82 = *off;
081230150415         IF  not *in09 and §FAuti = 'S';
081240150415           *in82 = *on;
081250150415         ENDIF;
081260150415      /end-free
081270051129
081280051129     c                   EndSr
081290051129
081300051129      *------------------------------------------------------------------------*
081310051129      * CONTROLLO BANCA APPOGGIO
081320051129      *------------------------------------------------------------------------*
081330051129     c     Sr_Ctrbna     BegSr
081340070216
081350070216     c                   clear                   wbanca
081360070216     c                   clear                   wagenzia
081370051129
081380051129      * Ricerca
081390100727     c                   Eval      wpos = %scan('?':v4cbna)
081400051129if  1c                   If        wpos > 0
081410100727     c                   Eval      pist = %subst(v4cbna:(wpos+1):(36-wpos))
081420100727     c                   Move      v4cabi        pabi
081430051129     c                   Clear                   pcab
081440051129     c                   Eval      ppro = v2cprv
081450051129     c                   Eval      pflg = 'R'
081460051129     c                   Eval      kpjbu = parmbanca
081470051129     c                   Call      'CNC592R'
081480051129     c                   Parm                    kpjba
081490051129     c                   Eval      parmbanca = kpjbu
081500051129      * Se è stato selezionato un codice
081510051129if  2c                   If        pcab <> *Zeros
081520100727     c                   Movel     pabi          v4cabi
081530100727     c                   Movel     pcab          v4ccab
081540100727     c                   Eval      kabi = v4cabi
081550100727     c                   Eval      kcab = v4ccab
081560100729     c                   exsr      DesBanca
081570100729     c                   eval      v4cbna = wDesBanca
081580051129    2c                   EndIf
081590051129     c                   Eval      *In90 = *On
081600100728     c                   Eval      h4riga = 14
081610100728     c                   Eval      h4colo = 36
081620051129     c                   Leavesr
081630051129    1c                   EndIf
081640051129
081650051129     c                   EndSr
081660051129
081670051129      *------------------------------------------------------------------------*
081680051130      * CONTROLLO ABI/CAB RELATIVO AL PAGAMENTO FATTURE
081690051129      *------------------------------------------------------------------------*
081700051130     c     Sr_Ctrabicabf BegSr
081710051129
081720051129      * Se non è da gestione visite/offerte controllo se abi e cab
081730051129      * obbligatori per pagamento con RB
081740051129     c                   If        Not *In06 and §fatpg = '1' and
081750100727     c                             v4cabi = *Zeros and v4ccab = *Zeros
081760051129     c                   Eval      *In28 = *On
081770100728     c                   Eval      h4riga = 15
081780100728     c                   Eval      h4colo = 36
081790100728     c                   Eval      v4cmsg = msg(54)
081800051129     c                   Leavesr
081810051129     c                   EndIf
081820090331
081830090331      * se pagamento con RB codice '1' non accetto ABI >= 90000
081840090331     c                   If        §fatpg = '1' and
081850100727     c                             v4cabi >= 90000
081860090331     c                   Eval      *In28 = *On
081870100728     c                   Eval      h4riga = 15
081880100728     c                   Eval      h4colo = 36
081890100728     c                   Eval      v4cmsg = msg(56)
081900090331     c                   Leavesr
081910090331     c                   EndIf
081920051130
081930051130      * Controllo
081940100727     c                   Eval      kabi = v4cabi
081950100727     c                   Eval      kcab = v4ccab
081960051130     c                   ExSr      Sr_Ctrabicab
081970051130     c                   If        *In28 = *On
081980100728     c                   Eval      h4riga = 15
081990100728     c                   Eval      h4colo = 36
082000051130     c                   Leavesr
082010051130     c                   EndIf
082020051130      * Creo descrizione della banca di appoggio
082030100727     c                   If        v4cabi <> 99999 or v4ccab <> 99999
082040100727     c                   Movel     wbanca        v4cbna
082050100727     c                   Move      wagenzia      v4cbna
082060051130     c                   EndIf
082070051130
082080051130     c                   EndSr
082090150424
082100150424      /free
082110150424       //--------------------------------------------------------------
082120150424       //?Controllo i pagamenti C/A - Danni - N.A.
082130150424       //--------------------------------------------------------------
082140150424       BEGSR CtrPagamenti;
082150150424
082160150424       //?Pagamenti CONTRASSEGNI
082170150424         clear ds4U;
082180150424         kcod = '4U';
082190150424         kkey = V5Cfpc;
082200150424         chain (codut:kcod:kkey) TABEL00F;
082210150424         IF  %found(TABEL00F);
082220150424           ds4U = TBLuni;
082230150424         ENDIF;
082240151112         IF  V5Cfpc <> '1';
082250150427         IF  not wForzaPag and §4Utip = 'I' and
082260150427            (§4Ucod <> 'B' or V5Ciban = *blanks);
082270150424           *in28 = *on;
082280150424           H5riga = 09;
082290150424           H5colo = 36;
082300150424           V5Cmsg = msg(87);
082310150424           wForzaPag = *on;
082320150424           leavesr;
082330150424         ENDIF;
082340151112         ENDIF;
082350150424
082360150424       //?Pagamenti DANNI
082370150424         clear ds4U;
082380150424         kcod = '4U';
082390150424         kkey = V5tpdn;
082400150424         chain (codut:kcod:kkey) TABEL00F;
082410150424         IF  %found(TABEL00F);
082420150424           ds4U = TBLuni;
082430150424         ENDIF;
082440151112         IF  V5tpdn <> '1';
082450150427         IF  not wForzaPag and §4Utip = 'I' and
082460150427            (§4Ucod <> 'B' or V5ibandn = *blanks);
082470150424           *in28 = *on;
082480150424           H5riga = 12;
082490150424           H5colo = 36;
082500150424           V5Cmsg = msg(87);
082510150424           wForzaPag = *on;
082520150424           leavesr;
082530150424         ENDIF;
082540151112         ENDIF;
082550150424
082560150424       //?Pagamenti NOTE ACCREDITO
082570150424         clear ds4U;
082580150424         kcod = '4U';
082590150424         kkey = V5tpna;
082600150424         chain (codut:kcod:kkey) TABEL00F;
082610150424         IF  %found(TABEL00F);
082620150424           ds4U = TBLuni;
082630150424         ENDIF;
082640151112         IF  V5tpna <> '1';
082650150427         IF  not wForzaPag and §4Utip = 'I' and
082660150427            (§4Ucod <> 'B' or V5ibanna = *blanks);
082670150424           *in28 = *on;
082680150424           H5riga = 15;
082690150427           H5colo = 37;
082700150424           V5Cmsg = msg(87);
082710150424           wForzaPag = *on;
082720150424           leavesr;
082730150424         ENDIF;
082740151112         ENDIF;
082750150424
082760150424       ENDSR;
082770150728
082780150728       //--------------------------------------------------------------
082790150728       //?Controllo i dati per la mail invio progetto di liquidazione.
082800150728       //--------------------------------------------------------------
082810150728       BEGSR CtrMailDanni;
082820150728
082830150728       //?Se esiste la nota 87 OK
082840150728         IF  *in18;
082850150728           leavesr;
082860150728         ENDIF;
082870150728
082880150728       //?Se esiste il luogo 008 e c'è la mail OK
082890150728         IF  *in17;
082900150728           kSPEcli = V1Cksc;
082910150728           kSPEcod = '008';
082920150728           chain (kSPEcli:kSPEcod:kSP2tpe) FNSP201L;
082930150728           IF  %Found(FNSP201L) and SP2flg = *blanks;
082940150728             leavesr;
082950150728           ENDIF;
082960150728         ENDIF;
082970150728
082980150728       //?Se arrivo qua vuol dire che non c'è la nota 87 oppure
082990150728       //?non c'è il luogo 008 o il luogo 008 non ha la mail
083000150728         IF  not wForzaMail;
083010150728           *in28 = *on;
083020150728           H7riga = 13;
083030150728           H7colo = 35;
083040150728           V7Cmsg = msg(88);
083050150728           wForzaMail = *on;
083060150728         ENDIF;
083070150728
083080150728       ENDSR;
083090150424      /end-free
083100051130
083110051130      *------------------------------------------------------------------------*
083120051130      * CONTROLLO CODICE PAGAMENTO CONTRASSEGNI
083130051130      *------------------------------------------------------------------------*
083140051130     c     Sr_Ctrfpc     BegSr
083150051219
083160051219     c                   Eval      *In22 = *Off
083170051219     c                   Eval      *In23 = *Off
083180051130
083190051130      * Ricerca
083200051130     c                   Clear                   v5dfpc
083210051130     c                   If        v5cfpc = '?'
083220051130     c                   Eval      kcod = '4U'
083230051130     c                   call      'TRUL19R'
083240051130     c                   parm                    kcod
083250051130     c                   parm                    ordina
083260051130     c                   parm                    kkey
083270051130     c                   parm                    comando
083280051130     c                   Eval      v5cfpc = kkey
083290100729     c                   Eval      h5riga = 09
083300100802     c                   Eval      h5colo = 36
083310051130     c                   Eval      *In90 = *On
083320051130     c                   EndIf
083330051130
083340051130      * Controllo
083350051130     c                   Clear                   v5dfpc
083360051130     c                   Clear                   ds4u
083370051130     c                   Eval      kcod = '4U'
083380051130     c                   Eval      kkey = v5cfpc
083390051130     c     kTabel        Chain     Tabel00f
083400051130     c                   If        Not %Found(Tabel00f) or
083410051130     c                             tblflg <> *Blanks
083420051130     c                   Eval      *In28 = *On
083430100729     c                   Eval      h5riga = 09
083440100802     c                   Eval      h5colo = 36
083450051130     c                   Eval      v5cmsg = msg(57)
083460051130     c                   Leavesr
083470051130     c                   EndIf
083480051130     c                   Eval      ds4u = tbluni
083490051130     c                   Eval      v5dfpc = §4udes
083500051130
083510051130      * Controllo se utilizzabile per i clienti vari
083520051130     c                   If        (wksc04 = 8888 or wksc04 = 9999) and
083530051130     c                              §4uvar = 'N'
083540051130     c                   Eval      *In28 = *On
083550100729     c                   Eval      h5riga = 09
083560100802     c                   Eval      h5colo = 36
083570051130     c                   Eval      v5cmsg = msg(58)
083580051130     c                   Leavesr
083590051130     c                   EndIf
083600151030
083610151030      /free
083620151030       //?Errore se utilizzabile solo da sede e NON sono sede
083630151112         IF  §4Usede <> *blanks and not *in09 and v5cfpc <> CLPfpc;
083640151030           *in28 = *on;
083650151030           H5riga = 09;
083660151030           H5colo = 36;
083670151030           V5Cmsg = msg(57);
083680151030           V5Cmsg = %trim(V5Cmsg) + '. Utilizzabili SOLO da SEDE';
083690151030           leavesr;
083700151030         ENDIF;
083710151030      /end-free
083720051219
083730051219      * Le coordinate bancare le visualizzo o le proteggo in base al
083740051219      * tipo pagamento
083750051219     c                   Select
083760051219      * Coordinate italiane, visualizzo
083770051219      * Immissione coordinate non richiesta, non visualizzo
083780051219     c                   When      §4uabi = 'N'
083790051219     c                   Eval      *In22 = *On
083800080116      * pulisco le coordinate bancarie
083810080116     c                   clear                   v5ciban
083820051219      * Se pagamento estero proteggo le coordinate bancarie perchè
083830051219      * gestite dalla sede.
083840080115     c                   when      §4utip = 'E'
083850051219     c                   Eval      *In23 = *On
083860051219     c                   EndSl
083870100729      * se il tipo pagamento del codice impostato a video è diverso da quello
083880100729      * memorizzato sul file ed è estero devo pulire il campo relativo all'IBAN
083890100729      * e avvisare con window per inviare mail alla sede
083900100729     c                   if        §4utip <> sav4utip and §4utip = 'E'
083910100729     c                   if        v5ciban <> *blanks
083920100729     c                   clear                   v5ciban
083930100729     c                   endif
083940100729      * window per inviare mail alla sede
083950100729      * solo se tipo pagamento prevede le coordinate bancarie
083960100729     c                   if        not $win and §4uabi = 'S'
083970101014     c                   eval      wuffsede = '"CONTRASSEGNI" di sede'
083980100729     c                   exsr      sr_winmail
083990100729     c                   eval      $win = *on
084000100729     c                   eval      *in90 = *on
084010100729     c                   leavesr
084020100729     c                   endif
084030100729     c                   endif
084040051130
084050051130     c                   EndSr
084060100729
084070100729      /free
084080100729       //--------------------------------------------------------------
084090100729       //?Controllo pagamento Danni.
084100100729       //--------------------------------------------------------------
084110100729       BEGSR CtrPagDN;
084120100729
084130100729         *in71 = *off;
084140100729         *in72 = *off;
084150100729
084160100729       //?Ricerco il codice pagamento
084170100729         clear v5dpdn;
084180100729         IF  %scan('?':v5tpdn) > 0;
084190100729           kcod = '4U';
084200100729           Trul19R (kcod:ordina:kkey:comando);
084210100729           v5tpdn = kkey;
084220100730           h5riga = 12;
084230100802           h5colo = 36;
084240100729           *in90 = *on;
084250100729         ENDIF;
084260100729
084270100729       //?Controllo
084280100729         clear v5dpdn;
084290100729         clear ds4u;
084300100729         kcod = '4U';
084310100729         kkey = v5tpdn;
084320100729         chain (codut:kcod:kkey) TABEL00F;
084330100729         IF  not %Found(TABEL00F) or TBLflg <> *blanks;
084340100729           *in28 = *on;
084350100730           h5riga = 12;
084360100802           h5colo = 36;
084370100729           v5cmsg = msg(57);
084380100729           leavesr;
084390100729         ENDIF;
084400100729         ds4u = TBLuni;
084410100730         v5dpdn = §4Udes;
084420100729
084430100729       //?Controllo se posso utilizzare per clienti vari
084440110131         IF  (wksc04 = 8888 or wksc04 = 9999) and §4Uvar = 'N';
084450100729           *in28 = *on;
084460100730           h5riga = 12;
084470100802           h5colo = 36;
084480100729           v5cmsg = msg(58);
084490100729           leavesr;
084500100729         ENDIF;
084510151030
084520151030       //?Errore se utilizzabile solo da sede e NON sono sede
084530151112         IF  §4Usede <> *blanks and not *in09 and
084540151112             v5tpdn <> %subst(CLStic:1:1);
084550151030           *in28 = *on;
084560151030           H5riga = 12;
084570151030           H5colo = 36;
084580151030           V5Cmsg = msg(57);
084590151030           V5Cmsg = %trim(V5Cmsg) + '. Utilizzabili SOLO da SEDE';
084600151030           leavesr;
084610151030         ENDIF;
084620151030
084630100729       //?Visualizzo il campo iban se coordinate bancarie obbligatorie
084640100730         *in71 = (§4Uabi = 'N');
084650100729
084660100729       //?Controllo se posso utilizzare in base alla tabella PAG
084670100729         clear TIBS02DS;
084680100729         clear dPAG;
084690100729         T02mod = 'C';
084700100729         T02cod = 'PAG';
084710100729         T02ke1 = 'DN';
084720100729         T02ke2 = v5tpdn;
084730100729         T02sif = KNSIF;
084740100729         TNTBE_RicercaControllo (kpjba : tibs02ds);
084750100729         IF  T02err <> *blanks;
084760100729           *in28 = *on;
084770100730           h5riga = 12;
084780100802           h5colo = 36;
084790100729           v5cmsg = msg(85);
084800100729           leavesr;
084810100729         ENDIF;
084820100729         IF  T02err = *blanks;
084830100729           dPAG = T02uni;
084840100729         ENDIF;
084850100729       //?controllo se la filiale può utilizzare il pagamento
084860151112         IF  d§PAGuti = 'NO' and not *in09 and
084870151112             v5tpdn <> %subst(CLStic:1:1);
084880100729           *in28 = *on;
084890100730           h5riga = 12;
084900100802           h5colo = 36;
084910100729           v5cmsg = msg(80);
084920100729           leavesr;
084930100729         ENDIF;
084940100730       //?proteggo il campo IBAN se pagamento estero
084950100730         *in72 = (§4Utip = 'E');
084960100909
084970100909       //?se pagamento con bonifico italia posso utilizzarlo solo
084980100909       //?per clienti 'T - D - A'
084990150506         //IF  not *in72 and d§PAGctr = 'SI' and
085000150506         //    v3cclv <> 'T' and  v3cclv <> 'D' and
085010150506         //    v3cclv <> 'A';
085020150506         //  *in28 = *on;
085030150506         //  h5riga = 12;
085040150506         //  h5colo = 36;
085050150506         //  v5cmsg = msg(85);
085060150506         //  v5cmsg = %trim(v5cmsg) + ' per clienti NON di tipo T-D-A';
085070150506         //  leavesr;
085080150506         //  ENDIF;
085090100730
085100100802       //?se è stato modificato il tipo pagamento
085110100909       //?e quello inserito non richiede le coordinate bancarie pulisco
085120100802       //?il campo dell'IBAN
085130100802         IF  v5tpdn <> savtipdn and d§PAGobb <> 'SI';
085140100802           clear v5ibandn;
085150100805           clear v5bicdn;
085160100802       //?se il tipo pagamento inserito è un tipo pagamento che lo richiede
085170100802       //?visualizzo window per invio mail
085180100802           IF  d§PAGobb = 'MA' and not $winDN;
085190101014             wuffsede = '"GESTIONE PAGAMENTI" di sede';
085200100802             exsr sr_winmail;
085210100802             $winDN = *on;
085220100802             *in90 = *on;
085230100802             leavesr;
085240100802           ENDIF;
085250100730         ENDIF;
085260100729
085270100729       ENDSR;
085280100729
085290100729       //--------------------------------------------------------------
085300100729       //?Controllo pagamento Note Accredito.
085310100729       //--------------------------------------------------------------
085320100729       BEGSR CtrPagNA;
085330100730
085340100730         *in73 = *off;
085350100730         *in74 = *off;
085360100730
085370100730       //?Ricerco il codice pagamento
085380100730         clear v5dpna;
085390100730         IF  %scan('?':v5tpna) > 0;
085400100730           kcod = '4U';
085410100730           Trul19R (kcod:ordina:kkey:comando);
085420100916           v5tpna = kkey;
085430100730           h5riga = 15;
085440150427           h5colo = 37;
085450100730           *in90 = *on;
085460100730         ENDIF;
085470100730
085480100730       //?Controllo
085490100730         clear v5dpna;
085500100730         clear ds4u;
085510100730         kcod = '4U';
085520100730         kkey = v5tpna;
085530100730         chain (codut:kcod:kkey) TABEL00F;
085540100730         IF  not %Found(TABEL00F) or TBLflg <> *blanks;
085550100730           *in28 = *on;
085560100730           h5riga = 15;
085570150427           h5colo = 37;
085580100730           v5cmsg = msg(57);
085590100730           leavesr;
085600100730         ENDIF;
085610100730         ds4u = TBLuni;
085620100730         v5dpna = §4Udes;
085630100730
085640100730       //?Controllo se posso utilizzare per clienti vari
085650110131         IF  (wksc04 = 8888 or wksc04 = 9999) and §4Uvar = 'N';
085660100730           *in28 = *on;
085670100730           h5riga = 15;
085680150427           h5colo = 37;
085690100730           v5cmsg = msg(58);
085700100730           leavesr;
085710100730         ENDIF;
085720151030
085730151030       //?Errore se utilizzabile solo da sede e NON sono sede
085740151112         IF  §4Usede <> *blanks and not *in09 and
085750151112             v5tpna <> %subst(CLStic:2:1);
085760151030           *in28 = *on;
085770151030           H5riga = 15;
085780151030           H5colo = 37;
085790151030           V5Cmsg = msg(57);
085800151030           V5Cmsg = %trim(V5Cmsg) + '. Utilizzabili SOLO da SEDE';
085810151030           leavesr;
085820151030         ENDIF;
085830151030
085840100730       //?Visualizzo il campo iban se coordinate bancarie obbligatorie
085850100730         *in73 = (§4Uabi = 'N');
085860100730
085870100730       //?Controllo se posso utilizzare in base alla tabella PAG
085880100730         clear TIBS02DS;
085890100730         clear dPAG;
085900100730         T02mod = 'C';
085910100730         T02cod = 'PAG';
085920100730         T02ke1 = 'NA';
085930100730         T02ke2 = v5tpna;
085940100730         T02sif = KNSIF;
085950100730         TNTBE_RicercaControllo (kpjba : tibs02ds);
085960100730         IF  T02err <> *blanks;
085970100730           *in28 = *on;
085980100730           h5riga = 15;
085990150427           h5colo = 37;
086000100730           v5cmsg = msg(85);
086010100730           leavesr;
086020100730         ENDIF;
086030100730         IF  T02err = *blanks;
086040100730           dPAG = T02uni;
086050100730         ENDIF;
086060100730       //?controllo se la filiale può utilizzare il pagamento
086070151112         IF  d§PAGuti = 'NO' and not *in09 and
086080151112             v5tpna <> %subst(CLStic:2:1);
086090100730           *in28 = *on;
086100100730           h5riga = 15;
086110150427           h5colo = 37;
086120100730           v5cmsg = msg(80);
086130100730           leavesr;
086140100730         ENDIF;
086150100730       //?proteggo il campo IBAN se pagamento estero
086160100730         *in74 = (§4Utip = 'E');
086170100730
086180100802       //?se è stato modificato il tipo pagamento
086190100909       //?e quello inserito non richiede le coordinate bancarie pulisco
086200100802       //?il campo dell'IBAN
086210100802         IF  v5tpna <> savtipna and d§PAGobb <> 'SI';
086220100802           clear v5ibanna;
086230100805           clear v5bicna;
086240100802       //?se il tipo pagamento inserito è un tipo pagamento che lo richiede
086250100802       //?visualizzo window per invio mail
086260100802           IF  d§PAGobb = 'MA' and not $winNA;
086270101220             wuffsede = '"GESTIONE PAGAMENTI" di sede';
086280100730             exsr sr_winmail;
086290100730             $winNA = *on;
086300100730             *in90 = *on;
086310100730             leavesr;
086320100730           ENDIF;
086330100730         ENDIF;
086340100729
086350100729       ENDSR;
086360100729      /end-free
086370080115
086380080115      *------------------------------------------------------------------------*
086390080115      * CONTROLLO CODICE IBAN
086400080115      *------------------------------------------------------------------------*
086410080115     c     sr_ctriban    begsr
086420080115
086430080115     c                   clear                   wbanca
086440080115     c                   clear                   wagenzia
086450080422
086460080422      * se tipo pagamento contrassegno italia posso accettare solo
086470090612      * un IBAN italia (IT)
086480080422     c                   if        §4utip = 'I' and
086490100729     c                             %subst(wiban:1:2) <> 'IT'
086500150609     c                             and %subst(wiban:1:2) <> 'SM'
086510150424     c                             and %subst(wiban:1:2) <> *blanks
086520080422     c                   eval      *in28 = *on
086530080422     c                   eval      v5cmsg = msg(79)
086540080422     c                   leavesr
086550080422     c                   endif
086560080116
086570080116      * richiamo programma di controllo codice IBAN (Proj)
086580080116     c                   clear                   parmiban
086590100730     c                   eval      parmstato = %subst(wiban:1:2)
086600100730     c                   eval      parmcheck = %subst(wiban:3:2)
086610100730     c                   eval      parmcoori = %subst(wiban:5:28)
086620080116     c                   call      'XCHKIBAN'
086630080116     c                   parm                    parmstato
086640080116     c                   parm                    parmcheck
086650080116     c                   parm                    parmcoori
086660080116     c                   parm                    erriban
086670080116      * se ritorna errore messaggio
086680080116     c                   if        erriban = *on
086690080116     c                   eval      *in28 = *on
086700080116     c                   eval      v5cmsg = msg(59)
086710080116     c                   leavesr
086720080116     c                   endif
086730080116      * decodifico la banca
086740100729     c                   clear                   wdesbanca
086750100729     c                   eval      kabi = %dec(%subst(wiban:6:5):5:0)
086760100729     c                   eval      kcab = %dec(%subst(wiban:11:5):5:0)
086770080116     c     kcnabi        chain     cnabi00f
086780080116     c                   if        %found(cnabi00f) and abiann <> '*'
086790080116      * compongo la descrizione della banca d'appoggio
086800080116     c                   exsr      sr_crebna
086810100730     c                   eval      wdesbanca = wbanca + wagenzia
086820080116     c                   endif
086830080115
086840080115     c                   endsr
086850051130
086860051130      *-------------------------------------------------------------------------
086870051130      *  Creo la descrizione della banca di appoggio in base ai codici ABI/CAB
086880051130      *-------------------------------------------------------------------------
086890051130     c     Sr_Crebna     BegSr
086900051130
086910051130if  1c                   If        abiabi <> 99999 or abicab <> 99999
086920051130     c                   Clear                   wagenzia
086930060221     c                   Clear                   wbanca
086940051130     c                   Eval      wbanca = abiist
086950051130      * Compongo il nome della banca con località o comune
086960051130if  2c                   If        abiage = *Blanks
086970051130     c                   Select
086980051130     c                   When      abiloc <> *Blanks
086990051130     c                   Eval      wagenzia = abiloc
087000051130     c                   When      abicom <> *Blanks
087010051130     c                   Eval      wagenzia = abicom
087020051130     c                   EndSl
087030051130      * Compongo il nome della banca con agenzia + località o comune
087040051130   x2c                   Else
087050051130      * Cerco il primo byte vuoto dell'agenzia
087060051130     c                   Eval      wpos = %checkr(' ':abiage)
087070051130
087080051130     c                   Eval      wagenzia = %subst(abiage:1:wpos)
087090051130     c                   Eval      wpos1 = wpos + 2
087100051130     c                   Eval      wpos2 = 16 - wpos1
087110060215      * Se wpos2 inferiore a zero lo imposto a 0
087120060215     c                   If        wpos2 < *Zeros
087130060215     c                   Clear                   wpos2
087140060215     c                   EndIf
087150051130      * Se posso aggiungere anche solo 1 carattere procedo
087160051130     c                   If        wpos > 1 and wpos < 16
087170051130      * Cerco di riempire quello che resta con la località o il comune
087180051130     c                   Select
087190051130     c                   When      abiloc <> *Blanks
087200060221     c                   Eval      wagenzia = %trim(wagenzia) + ' ' +
087210060221     c                             %subst(abiloc:1:wpos2)
087220051130     c                   When      abicom <> *Blanks
087230060221     c                   Eval      wagenzia = %trim(wagenzia) + ' ' +
087240060221     c                             %subst(abicom:1:wpos2)
087250051130     c                   EndSl
087260051130     c                   EndIf
087270051130
087280051130    2c                   EndIf
087290051130    1c                   EndIf
087300051130
087310051130     c                   EndSr
087320051207
087330051130      *------------------------------------------------------------------------*
087340051130      * CONTROLLO ABI/CAB GENERICO
087350051130      *------------------------------------------------------------------------*
087360051130     c     Sr_Ctrabicab  BegSr
087370051130
087380051130      * Controllo ABI/CAB inseriti
087390051130     c                   If        kabi = *Zeros and kcab = *Zeros
087400051130     c                   Leavesr
087410051130     c                   EndIf
087420051130
087430051130      * Se immesso il CAB immettere anche ABI
087440051130     c                   If        (kabi = *Zeros and kcab <> *Zeros) or
087450051130      * Se immesso il ABI immettere anche CAB
087460051130     c                             (kabi <> *Zeros and kcab = *Zeros)
087470051130     c                   Eval      *In28 = *On
087480101111     c                   Eval      v4cmsg = msg(56)
087490051130     c                   Leavesr
087500051130     c                   EndIf
087510051130      * Controllo se validi
087520051130     c     kCnabi        Chain     Cnabi00f
087530051130     c                   If        Not %Found(Cnabi00f) or abiann = '*'
087540051130     c                   Eval      *In28 = *On
087550101111     c                   Eval      v4cmsg = msg(56)
087560051130     c                   Leavesr
087570051130     c                   EndIf
087580051130
087590051130      * Compongo la descrizione della banca d'appoggio
087600051130     c                   ExSr      Sr_Crebna
087610051130
087620051130     c                   EndSr
087630051129
087640051206
087650051206      *------------------------------------------------------------------------*
087660051206      * CONTROLLO LUOGO 555 - INTESTAZIONE PAGAMENTO C/ASSEGNI
087670051206      *------------------------------------------------------------------------*
087680051206     c     Sr_Ctrl555    BegSr
087690051206
087700051206if  1c                   If        v5cl555 <> *Blanks
087710060203
087720060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
087730060203     c                   If        *In05 and Not *In11
087740051206     c                   Eval      *In28 = *On
087750100730     c                   Eval      h5riga = 19
087760060203     c                   Eval      h5colo = 35
087770051207     c                   Eval      v5cmsg = msg(50)
087780051206     c                   Leavesr
087790051206     c                   EndIf
087800071128
087810071128      * se non è interrogazione controllo se luogo utilizzabile
087820071128     c                   if        not *in09 and not *in05 and $ufi555 <> 'S'
087830071128     c                   eval      *In28 = *on
087840100730     c                   eval      h5riga = 19
087850071128     c                   eval      h5colo = 35
087860071128     c                   eval      v5cmsg = msg(76)
087870071128     c                   leavesr
087880071128     c                   endif
087890051206
087900060203      * Imposto se interrogazione o gestione
087910051213     c                   Clear                   tnta81ds
087920060203     c   05              Eval      i81opz = '5'
087930060203     c  n05
087940060214     can 11              Eval      i81opz = '2'
087950051213     c                   Eval      i81cod = '555'
087960051213     c                   Eval      i81cli = v1cksc
087970060216     c                   Eval      i81gcli = '1'
087980060216     c   01              Eval      i81icli = '1'
087990051213     c                   Call      'TNTA81R'
088000051213     c                   Parm                    kpjba
088010051213     c                   Parm                    tnta81ds
088020051206
088030051212      * Controllo se ora c'è o non c'è più il luogo
088040051216     c                   Eval      kspecli = v1cksc
088050051216     c                   Eval      kspecod = '555'
088060051206     c     kFnspe        Chain     Fnspe01l
088070051206     c                   If        %Found(Fnspe01l) and speflg = *Blanks
088080051212     c                   Eval      *In11 = *On
088090051212     c                   Else
088100051212     c                   Eval      *In11 = *Off
088110051206     c                   EndIf
088120051206
088130051206     c                   Clear                   v5cl555
088140100730     c                   Eval      h5riga = 19
088150060203     c                   Eval      h5colo = 35
088160051207     c                   Goto      emi05
088170051206
088180051206    1c                   EndIf
088190051206
088200051206     c                   EndSr
088210051207
088220051207      *------------------------------------------------------------------------*
088230051212      * CONTROLLO LUOGO 666 - LUOGO INVIO C/ASSEGNO MITTENTE
088240051207      *------------------------------------------------------------------------*
088250051207     c     Sr_Ctrl666    BegSr
088260051212
088270051212if  1c                   If        v5cl666 <> *Blanks
088280060203
088290060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
088300060203     c                   If        *In05 and Not *In12
088310051212     c                   Eval      *In28 = *On
088320100730     c                   Eval      h5riga = 20
088330060203     c                   Eval      h5colo = 35
088340051212     c                   Eval      v5cmsg = msg(50)
088350051212     c                   Leavesr
088360051212     c                   EndIf
088370071128
088380071128      * se non è interrogazione controllo se luogo utilizzabile
088390071128     c                   if        not *in09 and not *in05 and $ufi666 <> 'S'
088400071128     c                   eval      *In28 = *on
088410100730     c                   eval      h5riga = 20
088420071128     c                   eval      h5colo = 35
088430071128     c                   eval      v5cmsg = msg(76)
088440071128     c                   leavesr
088450071128     c                   endif
088460051212
088470060203      * Imposto se interrogazione o gestione
088480051213     c                   Clear                   tnta81ds
088490060203     c   05              Eval      i81opz = '5'
088500060203     c  n05
088510060214     can 12              Eval      i81opz = '2'
088520051213     c                   Eval      i81cod = '666'
088530051213     c                   Eval      i81cli = v1cksc
088540060216     c                   Eval      i81gcli = '1'
088550060216     c   01              Eval      i81icli = '1'
088560051213     c                   Call      'TNTA81R'
088570051213     c                   Parm                    kpjba
088580051213     c                   Parm                    tnta81ds
088590051212
088600051212      * Controllo se ora c'è o non c'è più il luogo
088610051216     c                   Eval      kspecli = v1cksc
088620051216     c                   Eval      kspecod = '666'
088630051212     c     kFnspe        Chain     Fnspe01l
088640051212     c                   If        %Found(Fnspe01l) and speflg = *Blanks
088650051212     c                   Eval      *In12 = *On
088660051212     c                   Else
088670051212     c                   Eval      *In12 = *Off
088680051212     c                   EndIf
088690051212
088700051212     c                   Clear                   v5cl666
088710100730     c                   Eval      h5riga = 20
088720060203     c                   Eval      h5colo = 35
088730051212     c                   Goto      emi05
088740051212
088750051212    1c                   EndIf
088760051207
088770051207     c                   EndSr
088780051206
088790051206      *------------------------------------------------------------------------*
088800051220      * CONTROLLO NOTA 85 - E-MAIL BONIFICI C/ASSEGNI
088810051206      *------------------------------------------------------------------------*
088820051212     c     Sr_Ctrnt85    BegSr
088830051206
088840051212if  1c                   If        v5cnt85 <> *Blanks
088850060203
088860060203      * Se sono in interrogazione non posso interrogare la nota se non c'è
088870060203     c                   If        *In05 and Not *In13
088880060203     c                   Eval      *In28 = *On
088890100730     c                   Eval      h5riga = 19
088900100730     c                   Eval      h5colo = 71
088910060203     c                   Eval      v5cmsg = msg(52)
088920060203     c                   Leavesr
088930060203     c                   EndIf
088940060203
088950060203      * Imposto se interrogazione o gestione
088960060203     c  n05              Eval      wrich = 'M'
088970060203     c   05              Eval      wrich = 'V'
088980060217     c                   Eval      wrag = v3crag
088990051212     c                   Eval      wtnt = '85'
089000051212     c                   ExSr      Sr_f02
089010060215
089020060215     c                   If        ta12err <> *Blanks
089030060215     c                   Eval      *In28 = *On
089040100730     c                   Eval      h5riga = 19
089050100730     c                   Eval      h5colo = 71
089060060215     c                   Eval      v5cmsg = ta12msg
089070060215     c                   Leavesr
089080060215     c                   EndIf
089090051206
089100051212      * Controllo se ora c'è o non c'è più la nota
089110091112     c   70
089120091112     corn06              Eval      kntcapl = 'C'
089130100806     c   06
089140091112     cann70              Eval      kntcapl = 'T'
089150051216     c                   Movel(p)  dutkci        kntcnk1
089160091112     c   70
089170091112     corn06              Move      v1cksc        kntcnk1
089180100806     c   06
089190100806     cann70              Move      v1cnrv        kntcnk1
089200051216     c                   Clear                   kntcnk2
089210051216     c                   Movel     '85'          kntctnt
089220091119     c     kTfntc        Chain(n)  Tfntc01l
089230051206     c                   If        %Found(Tfntc01l)
089240051212     c                   Eval      *In13 = *On
089250051212     c                   Else
089260051212     c                   Eval      *In13 = *Off
089270051206     c                   EndIf
089280051206
089290051212     c                   Clear                   v5cnt85
089300100730     c                   Eval      h5riga = 19
089310100730     c                   Eval      h5colo = 71
089320051212     c                   Goto      emi05
089330051206
089340051206    1c                   EndIf
089350051206
089360051206     c                   EndSr
089370090616
089380090616      *------------------------------------------------------------------------*
089390090616      * CONTROLLO NOTA 89 - E-MAIL RESPONSABILE AMMINISTRATIVO
089400090616      *------------------------------------------------------------------------*
089410090616     c     Sr_Ctrnt89    BegSr
089420090616
089430090616if  1c                   If        v5cnt89 <> *Blanks
089440090616
089450090616      * Se sono in interrogazione non posso interrogare la nota se non c'è
089460090616     c                   If        *In05 and Not *In40
089470090616     c                   Eval      *In28 = *On
089480100730     c                   Eval      h5riga = 20
089490100730     c                   Eval      h5colo = 71
089500090616     c                   Eval      v5cmsg = msg(52)
089510090616     c                   Leavesr
089520090616     c                   EndIf
089530090616
089540090616      * Imposto se interrogazione o gestione
089550090616     c  n05              Eval      wrich = 'M'
089560090616     c   05              Eval      wrich = 'V'
089570090616     c                   Eval      wrag = v3crag
089580090616     c                   Eval      wtnt = '89'
089590090616     c                   ExSr      Sr_f02
089600090616
089610090616     c                   If        ta12err <> *Blanks
089620090616     c                   Eval      *In28 = *On
089630100730     c                   Eval      h5riga = 20
089640100730     c                   Eval      h5colo = 71
089650090616     c                   Eval      v5cmsg = ta12msg
089660090616     c                   Leavesr
089670090616     c                   EndIf
089680090616
089690090616      * Controllo se ora c'è o non c'è più la nota
089700091112     c   70
089710091112     corn06              Eval      kntcapl = 'C'
089720100806     c   06
089730091112     cann70              Eval      kntcapl = 'T'
089740090616     c                   Movel(p)  dutkci        kntcnk1
089750091112     c   70
089760091112     corn06              Move      v1cksc        kntcnk1
089770100806     c   06
089780100806     cann70              Move      v1cnrv        kntcnk1
089790090616     c                   Clear                   kntcnk2
089800090616     c                   Movel     '89'          kntctnt
089810091119     c     kTfntc        Chain(n)  Tfntc01l
089820090616     c                   If        %Found(Tfntc01l)
089830090616     c                   Eval      *In40 = *On
089840090616     c                   Else
089850090616     c                   Eval      *In40 = *Off
089860090616     c                   EndIf
089870090616
089880090616     c                   Clear                   v5cnt89
089890100730     c                   Eval      h5riga = 20
089900100730     c                   Eval      h5colo = 71
089910090616     c                   Goto      emi05
089920090616
089930090616    1c                   EndIf
089940090616
089950090616     c                   EndSr
089960100729      /free
089970100729       //--------------------------------------------------------------
089980100729       //?Controllo nota 82 - E-mail Bonifico Danni
089990100729       //--------------------------------------------------------------
090000100729       BEGSR sr_CtrNt82;
090010100730
090020100730         IF  v5cnt82 <> *blanks;
090030100730           *in90 = *on;
090040100730       //?Se sono in interrogazione non posso interrogare la note
090050100730       //?se non è presente
090060100730           IF  *in05 and not *in68;
090070100730             *in28 = *on;
090080100730             h5riga = 21;
090090100730             h5colo = 71;
090100100730             v5cmsg = msg(52);
090110100730             leavesr;
090120100730           ENDIF;
090130100730       //?Imposto se interrogazione o gestione della nota
090140100730           IF  not *in05;
090150100730             wrich = 'M';
090160100730           ENDIF;
090170100730           IF  *in05;
090180100730             wrich = 'V';
090190100730           ENDIF;
090200100730           wrag = v3crag;
090210100730           wtnt = '82';
090220100730           exsr sr_f02;
090230100730           IF  ta12err <> *blanks;
090240100730             *in28 = *on;
090250100730             h5riga = 21;
090260100730             h5colo = 71;
090270100730             v5cmsg = ta12msg;
090280100730             leavesr;
090290100730           ENDIF;
090300100730       //?Controllo se ora c'è la nota oppure se è stata cancellata
090310100730           SELECT;
090320100730             WHEN  *in70 or not *in06;
090330100730               kntcapl = 'C';
090340100730               %subst(kntcnk1:5:7) = %editc(v1cksc:'X');
090350100806             WHEN  *in06 and not *in70;
090360100730               kntcapl = 'T';
090370100806               %subst(kntcnk1:5:7) = %editc(v1cnrv:'X');
090380100730           ENDSL;
090390100730           %subst(kntcnk1:1:4) = %editc(DUTkci:'X');
090400100730           clear kntcnk2;
090410100730           kntctnt = '82';
090420150515           chain(n) (kntcapl:kntcnk1:kntcnk2:kntctnt) TFNTC01L;
090430100730           IF  %found(TFNTC01L);
090440100730             *in68 = *on;
090450100730           ELSE;
090460100730             *in68 = *off;
090470100730           ENDIF;
090480100730           clear v5cnt82;
090490100730           h5riga = 21;
090500100730           h5colo = 71;
090510100730         ENDIF;
090520100729
090530100729       ENDSR;
090540100729      /end-free
090550051207
090560051207      *------------------------------------------------------------------------*
090570051212      * CONTROLLO TIPO COMUNICAZIONE MITTENTE
090580051207      *------------------------------------------------------------------------*
090590051207     c     Sr_Ctrtcm     BegSr
090600051212
090610051212      * Ricerca
090620051212     c                   Clear                   v6dtcm
090630051212     c                   If        v6ctcm = '?'
090640051212     c                   Eval      kcod = '2F'
090650051212     c                   call      'TRUL19R'
090660051212     c                   parm                    kcod
090670051212     c                   parm                    ordina
090680051212     c                   parm                    kkey
090690051212     c                   parm                    comando
090700051212     c                   Eval      v6ctcm = kkey
090710051212     c                   Eval      h6riga = 07
090720051212     c                   Eval      h6colo = 38
090730051212     c                   Eval      *In90 = *On
090740051212     c                   EndIf
090750120118
090760120118      /free
090770120118       //?Se anagrafica provvisoria accetto il campo vuoto
090780120118         IF  *in06 and V6Ctcm = *blanks;
090790120118           leavesr;
090800120118         ENDIF;
090810120118      /end-free
090820051212
090830051212      * Controllo
090840051212     c                   Clear                   v6dtcm
090850051212     c                   Clear                   ds2f
090860051212     c                   Eval      kcod = '2F'
090870051212     c                   Eval      kkey = v6ctcm
090880051212     c     kTabel        Chain     Tabel00f
090890051212     c                   If        Not %Found(Tabel00f) or
090900051212     c                             tblflg <> *Blanks
090910051212     c                   Eval      *In28 = *On
090920051212     c                   Eval      h6riga = 07
090930051212     c                   Eval      h6colo = 38
090940051212     c                   Eval      v6cmsg = msg(62)
090950051212     c                   Leavesr
090960051212     c                   EndIf
090970051212     c                   Eval      ds2f = tbluni
090980051212     c                   Eval      v6dtcm = §2fdes
090990051212
091000051212      * Se tipo comunicazione con invio tramite fax
091010051212if  1c                   If        §2fftp = 'F'
091020051212      * Non ammesso per i clienti vari
091030051212     c                   If        wksc04 = 8888 or wksc04 = 9999
091040051212     c                   Eval      *In28 = *On
091050051212     c                   Eval      h6riga = 07
091060051212     c                   Eval      h6colo = 38
091070051212     c                   Eval      v6cmsg = msg(63)
091080051212     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
091090051212     c                             'per clienti vari'
091100051212     c                   Leavesr
091110051212     c                   EndIf
091120051216      * Controllo se fax sul luogo 005 è esatto
091130051216if  2c                   If        *In14 and wfax005 <> *Blanks
091140051216     c                   Clear                   trul42ds
091150051216     c                   Eval      d42fax = wfax005
091160051216     c                   Call      'TRUL42R'
091170051216     c                   Parm                    trul42ds
091180051216if  3c                   If        d42err = '1'
091190051216     c                   Eval      *In28 = *On
091200051216     c                   Eval      h6riga = 07
091210051216     c                   Eval      h6colo = 38
091220051216     c                   Eval      v6cmsg = d42msg
091230051216     c                   Leavesr
091240051216    3c                   EndIf
091250051216    2c                   EndIf
091260051216      * Se non c'è il luogo o se il n. fax non è presente sul luogo
091270051216      * deve essere impostato sull'anagrafica
091280051216     c                   If        v2ctlf = *Blanks and wfax005 = *Blanks
091290051212     c                   Eval      *In28 = *On
091300051212     c                   Eval      h6riga = 07
091310051212     c                   Eval      h6colo = 38
091320051212     c                   Eval      v6cmsg = msg(63)
091330051212     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
091340051212     c                             'nr. fax obbligatorio'
091350051212     c                   Leavesr
091360051212     c                   EndIf
091370051212    1c                   EndIf
091380051219
091390051219      * Se tipo comunicazione con invio tramite mail
091400051219if  1c                   If        §2fftp = 'M'
091410051219      * Non ammesso per i clienti vari
091420051219     c                   If        wksc04 = 8888 or wksc04 = 9999
091430051219     c                   Eval      *In28 = *On
091440051219     c                   Eval      h6riga = 07
091450051219     c                   Eval      h6colo = 38
091460051219     c                   Eval      v6cmsg = msg(63)
091470051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
091480051219     c                             'per clienti vari'
091490051219     c                   Leavesr
091500051219     c                   EndIf
091510051219      * Controllo se c'è la mail nel luogo
091520051219     c                   If        *In14 and wmail005 = *Blanks
091530051219     c                   Eval      *In28 = *On
091540051219     c                   Eval      h6riga = 07
091550051219     c                   Eval      h6colo = 38
091560051219     c                   Eval      v6cmsg = msg(63)
091570051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
091580051219     c                             'mail non immessa nel luogo 005'
091590051219     c                   Leavesr
091600051219     c                   EndIf
091610051219      * Se non c'è il luogo deve esserci la nota 88
091620051219     c                   If        Not *In14 and Not *In15
091630051219     c                   Eval      *In28 = *On
091640051219     c                   Eval      h6riga = 07
091650051219     c                   Eval      h6colo = 38
091660051219     c                   Eval      v6cmsg = msg(63)
091670051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
091680051219     c                             'nota 88 non immessa'
091690051219     c                   Leavesr
091700051219     c                   EndIf
091710051219    1c                   EndIf
091720051207
091730051207     c                   EndSr
091740051207
091750051207      *------------------------------------------------------------------------*
091760051212      * CONTROLLO TIPO COMUNICAZIONE FINE GIACENZA
091770051207      *------------------------------------------------------------------------*
091780051207     c     Sr_Ctrtcf     BegSr
091790051212
091800051212      * Ricerca
091810051212     c                   Clear                   v6dtcf
091820051212     c                   If        v6ctcf = '?'
091830051212     c                   Eval      kcod = '2H'
091840051212     c                   call      'TRUL19R'
091850051212     c                   parm                    kcod
091860051212     c                   parm                    ordina
091870051212     c                   parm                    kkey
091880051212     c                   parm                    comando
091890051212     c                   Eval      v6ctcf = kkey
091900051212     c                   Eval      h6riga = 08
091910051212     c                   Eval      h6colo = 38
091920051212     c                   Eval      *In90 = *On
091930051212     c                   EndIf
091940051212
091950051212      * Controllo
091960051212     c                   Clear                   v6dtcf
091970051212     c                   Clear                   ds2h
091980051212     c                   Eval      kcod = '2H'
091990051212     c                   Eval      kkey = v6ctcf
092000051212     c     kTabel        Chain     Tabel00f
092010051212     c                   If        Not %Found(Tabel00f) or
092020051212     c                             tblflg <> *Blanks
092030051212     c                   Eval      *In28 = *On
092040051212     c                   Eval      h6riga = 08
092050051212     c                   Eval      h6colo = 38
092060060216     c                   Eval      v6cmsg = msg(70)
092070051212     c                   Leavesr
092080051212     c                   EndIf
092090051212     c                   Eval      ds2h = tbluni
092100051212     c                   Eval      v6dtcf = §2hdes
092110051212
092120051212      * Se tipo comunicazione con invio tramite fax
092130051212if  1c                   If        §2hfax = 'F'
092140051212      * Non ammesso per i clienti vari
092150051212     c                   If        wksc04 = 8888 or wksc04 = 9999
092160051212     c                   Eval      *In28 = *On
092170051212     c                   Eval      h6riga = 08
092180051212     c                   Eval      h6colo = 38
092190051212     c                   Eval      v6cmsg = msg(63)
092200051212     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
092210051212     c                             'per clienti vari'
092220051212     c                   Leavesr
092230051212     c                   EndIf
092240051216      * Controllo se fax sul luogo 005 è esatto
092250051216if  2c                   If        *In14 and wfax005 <> *Blanks
092260051216     c                   Clear                   trul42ds
092270051216     c                   Eval      d42fax = wfax005
092280051216     c                   Call      'TRUL42R'
092290051216     c                   Parm                    trul42ds
092300051216if  3c                   If        d42err = '1'
092310051216     c                   Eval      *In28 = *On
092320051216     c                   Eval      h6riga = 08
092330051216     c                   Eval      h6colo = 38
092340051216     c                   Eval      v6cmsg = d42msg
092350051216     c                   Leavesr
092360051216    3c                   EndIf
092370051216    2c                   EndIf
092380051216      * Se non c'è il luogo o se il n. fax non è presente sul luogo
092390051216      * deve essere impostato sull'anagrafica
092400051216     c                   If        v2ctlf = *Blanks and wfax005 = *Blanks
092410051212     c                   Eval      *In28 = *On
092420051212     c                   Eval      h6riga = 08
092430051212     c                   Eval      h6colo = 38
092440051212     c                   Eval      v6cmsg = msg(63)
092450051212     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
092460051212     c                             'nr. fax obbligatorio'
092470051212     c                   Leavesr
092480051212     c                   EndIf
092490051212    1c                   EndIf
092500051219      * Se tipo comunicazione con invio tramite mail
092510051219if  1c                   If        §2hfax = 'M'
092520051219      * Non ammesso per i clienti vari
092530051219     c                   If        wksc04 = 8888 or wksc04 = 9999
092540051219     c                   Eval      *In28 = *On
092550051219     c                   Eval      h6riga = 08
092560051219     c                   Eval      h6colo = 38
092570051219     c                   Eval      v6cmsg = msg(63)
092580051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
092590051219     c                             'per clienti vari'
092600051219     c                   Leavesr
092610051219     c                   EndIf
092620051219      * Controllo se c'è la mail nel luogo
092630051219     c                   If        *In14 and wmail005 = *Blanks
092640051219     c                   Eval      *In28 = *On
092650051219     c                   Eval      h6riga = 08
092660051219     c                   Eval      h6colo = 38
092670051219     c                   Eval      v6cmsg = msg(63)
092680051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
092690051219     c                             'mail non immessa nel luogo 005'
092700051219     c                   Leavesr
092710051219     c                   EndIf
092720051219      * Se non c'è il luogo deve esserci la nota 88
092730051219     c                   If        Not *In14 and Not *In15
092740051219     c                   Eval      *In28 = *On
092750051219     c                   Eval      h6riga = 08
092760051219     c                   Eval      h6colo = 38
092770051219     c                   Eval      v6cmsg = msg(63)
092780051219     c                   Eval      v6cmsg = %trim(v6cmsg) + ' ' +
092790051219     c                             'nota 88 non immessa'
092800051219     c                   Leavesr
092810051219     c                   EndIf
092820051219    1c                   EndIf
092830051207
092840051207     c                   EndSr
092850051207
092860051207      *------------------------------------------------------------------------*
092870051212      * CONTROLLO APERTURA AUTOMATICA GIACENZA
092880051207      *------------------------------------------------------------------------*
092890051207     c     Sr_Ctrapg     BegSr
092900051212
092910051212      * Ricerca
092920051212     c                   Clear                   v6dapg
092930051212     c                   If        v6capg = '?'
092940051212     c                   Eval      kcod = '2U'
092950051212     c                   call      'TRUL19R'
092960051212     c                   parm                    kcod
092970051212     c                   parm                    ordina
092980051212     c                   parm                    kkey
092990051212     c                   parm                    comando
093000051212     c                   Eval      v6capg = kkey
093010051212     c                   Eval      h6riga = 09
093020051212     c                   Eval      h6colo = 38
093030051212     c                   Eval      *In90 = *On
093040051212     c                   EndIf
093050051212
093060051212      * Controllo
093070051212     c                   Clear                   v6dapg
093080051212     c                   Clear                   ds2u
093090051212     c                   Eval      kcod = '2U'
093100051212     c                   Eval      kkey = v6capg
093110051212     c     kTabel        Chain     Tabel00f
093120051212     c                   If        Not %Found(Tabel00f) or
093130051212     c                             tblflg <> *Blanks
093140051212     c                   Eval      *In28 = *On
093150051212     c                   Eval      h6riga = 09
093160051212     c                   Eval      h6colo = 38
093170051212     c                   Eval      v6cmsg = msg(64)
093180051212     c                   Leavesr
093190051212     c                   EndIf
093200051212     c                   Eval      ds2u = tbluni
093210051212     c                   Eval      v6dapg = §2udes
093220051212
093230051212      * Non ammessa apertura automatica per i clienti vari
093240051212     c                   If        §2uape = 'S' and
093250051212     c                             (wksc04 = 8888 or wksc04 = 9999)
093260051212     c                   Eval      *In28 = *On
093270051212     c                   Eval      h6riga = 09
093280051212     c                   Eval      h6colo = 38
093290051212     c                   Eval      v6cmsg = msg(65)
093300051212     c                   Leavesr
093310051212     c                   EndIf
093320051207
093330051207     c                   EndSr
093340051207
093350051207      *------------------------------------------------------------------------*
093360051212      * CONTROLLO DISPOSIZIONI MITTENTE IN ARRIVO
093370051207      *------------------------------------------------------------------------*
093380051207     c     Sr_Ctrdmg     BegSr
093390051212
093400051212      * Non ammesse disposizioni mittente in arrivo per i clienti vari
093410051212     c                   If        v6cdmg = 'SI' and
093420051212     c                             (wksc04 = 8888 or wksc04 = 9999)
093430051212     c                   Eval      *In28 = *On
093440051212     c                   Eval      h6riga = 10
093450051212     c                   Eval      h6colo = 38
093460051212     c                   Eval      v6cmsg = msg(66)
093470051212     c                   Leavesr
093480051212     c                   EndIf
093490051207
093500051207     c                   EndSr
093510060801
093520060801      *------------------------------------------------------------------------*
093530060801      * CONTROLLO CHIUSURA AUTOMATICA GIACENZA
093540060801      *------------------------------------------------------------------------*
093550060801     c     Sr_Ctrcag     BegSr
093560060801
093570060801      * Non ammessa chiusura automatica giacenza per i clienti vari
093580060801     c                   If        v6ccag = 'SI' and
093590060801     c                             (wksc04 = 8888 or wksc04 = 9999)
093600060801     c                   Eval      *In28 = *On
093610060801     c                   Eval      h6riga = 11
093620060801     c                   Eval      h6colo = 38
093630060801     c                   Eval      v6cmsg = msg(74)
093640060801     c                   Leavesr
093650060801     c                   EndIf
093660060801
093670060801     c                   EndSr
093680051207
093690051207      *------------------------------------------------------------------------*
093700051220      * CONTROLLO LUOGO 005 - LUOGO SPEDIZIONE GIACENZA
093710051207      *------------------------------------------------------------------------*
093720051207     c     Sr_Ctrl005    BegSr
093730051212
093740051212if  1c                   If        v6cl005 <> *Blanks
093750060203
093760060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
093770060203     c                   If        *In05 and Not *In14
093780051212     c                   Eval      *In28 = *On
093790051212     c                   Eval      h6riga = 13
093800060203     c                   Eval      h6colo = 35
093810051212     c                   Eval      v6cmsg = msg(50)
093820051212     c                   Leavesr
093830051212     c                   EndIf
093840071128
093850071128      * se non è interrogazione controllo se luogo utilizzabile
093860071128     c                   if        not *in09 and not *in05 and $ufi005 <> 'S'
093870071128     c                   eval      *In28 = *on
093880071128     c                   eval      h6riga = 13
093890071128     c                   eval      h6colo = 35
093900071128     c                   eval      v6cmsg = msg(76)
093910071128     c                   leavesr
093920071128     c                   endif
093930051212
093940060203      * Imposto se interrogazione o gestione
093950051213     c                   Clear                   tnta81ds
093960060203     c   05              Eval      i81opz = '5'
093970060203     c  n05
093980060214     can 14              Eval      i81opz = '2'
093990051213     c                   Eval      i81cod = '005'
094000051213     c                   Eval      i81cli = v1cksc
094010060216     c                   Eval      i81gcli = '1'
094020060216     c   01              Eval      i81icli = '1'
094030051213     c                   Call      'TNTA81R'
094040051213     c                   Parm                    kpjba
094050051213     c                   Parm                    tnta81ds
094060051212
094070051212      * Controllo se ora c'è o non c'è più il luogo
094080060112     c                   Clear                   wfax005
094090060112     c                   Clear                   wmail005
094100051216     c                   Eval      kspecli = v1cksc
094110051216     c                   Eval      kspecod = '005'
094120051212     c     kFnspe        Chain     Fnspe01l
094130051212     c                   If        %Found(Fnspe01l) and speflg = *Blanks
094140051212     c                   Eval      *In14 = *On
094150101026     c                   Eval      wfax005 = spefax
094160101026     c     kFnsp2        Chain     Fnsp201l
094170101026     c                   If        %Found(Fnsp201l) and sp2flg = *Blanks
094180101026     c                   Eval      wmail005 = sp2est
094190101026     c                   EndIf
094200051212     c                   Else
094210051212     c                   Eval      *In14 = *Off
094220051212     c                   EndIf
094230051212
094240051212     c                   Clear                   v6cl005
094250051212     c                   Eval      h6riga = 13
094260060203     c                   Eval      h6colo = 35
094270051212     c                   Goto      emi06
094280051212
094290051212    1c                   EndIf
094300051207
094310051207     c                   EndSr
094320051207
094330051207      *------------------------------------------------------------------------*
094340051220      * CONTROLLO NOTA 88 - E-MAIL GIACENZE
094350051207      *------------------------------------------------------------------------*
094360051207     c     Sr_Ctrnt88    BegSr
094370051212
094380051212if  1c                   If        v6cnt88 <> *Blanks
094390060203
094400060203      * Se sono in interrogazione non posso interrogare la nota se non c'è
094410060203     c                   If        *In05 and Not *In15
094420051212     c                   Eval      *In28 = *On
094430051212     c                   Eval      h6riga = 14
094440060203     c                   Eval      h6colo = 35
094450051219     c                   Eval      v6cmsg = msg(52)
094460051212     c                   Leavesr
094470051212     c                   EndIf
094480051212
094490060203      * Imposto se interrogazione o gestione
094500060203     c  n05              Eval      wrich = 'M'
094510060203     c   05              Eval      wrich = 'V'
094520060217     c                   Eval      wrag = v3crag
094530051212     c                   Eval      wtnt = '88'
094540051212     c                   ExSr      Sr_f02
094550060215
094560060215     c                   If        ta12err <> *Blanks
094570060215     c                   Eval      *In28 = *On
094580060215     c                   Eval      h6riga = 14
094590060215     c                   Eval      h6colo = 35
094600060215     c                   Eval      v6cmsg = ta12msg
094610060215     c                   Leavesr
094620060215     c                   EndIf
094630051212
094640060215      * Controllo se ora c'è o non c'è più la nota
094650091112     c   70
094660091112     corn06              Eval      kntcapl = 'C'
094670100806     c   06
094680091112     cann70              Eval      kntcapl = 'T'
094690051216     c                   Movel(p)  dutkci        kntcnk1
094700091112     c   70
094710091112     corn06              Move      v1cksc        kntcnk1
094720100806     c   06
094730100806     cann70              Move      v1cnrv        kntcnk1
094740051216     c                   Clear                   kntcnk2
094750051216     c                   Movel     '88'          kntctnt
094760091119     c     kTfntc        Chain(n)  Tfntc01l
094770051212     c                   If        %Found(Tfntc01l)
094780051212     c                   Eval      *In15 = *On
094790051212     c                   Else
094800051212     c                   Eval      *In15 = *Off
094810051212     c                   EndIf
094820051212
094830051212     c                   Clear                   v6cnt88
094840051212     c                   Eval      h6riga = 14
094850060203     c                   Eval      h6colo = 35
094860051212     c                   Goto      emi06
094870051212
094880051212    1c                   EndIf
094890051207
094900051207     c                   EndSr
094910060208
094920060208      *------------------------------------------------------------------------*
094930060208      * CONTROLLO LUOGO 300 - MERCE RESA
094940060208      *------------------------------------------------------------------------*
094950060208     c     Sr_Ctrl300    BegSr
094960060208
094970060208if  1c                   If        v6cl300 <> *Blanks
094980060208
094990060208      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
095000060208     c                   If        *In05 and Not *In33
095010060208     c                   Eval      *In28 = *On
095020060208     c                   Eval      h6riga = 15
095030060208     c                   Eval      h6colo = 35
095040060208     c                   Eval      v6cmsg = msg(50)
095050060208     c                   Leavesr
095060060208     c                   EndIf
095070071128
095080071128      * se non è interrogazione controllo se luogo utilizzabile
095090071128     c                   if        not *in09 and not *in05 and $ufi300 <> 'S'
095100071128     c                   eval      *In28 = *on
095110071128     c                   eval      h6riga = 15
095120071128     c                   eval      h6colo = 35
095130071128     c                   eval      v6cmsg = msg(76)
095140071128     c                   leavesr
095150071128     c                   endif
095160060208
095170060208      * Imposto se interrogazione o gestione
095180060208     c                   Clear                   tnta81ds
095190060208     c   05              Eval      i81opz = '5'
095200060208     c  n05
095210060214     can 33              Eval      i81opz = '2'
095220060208     c                   Eval      i81cod = '300'
095230060208     c                   Eval      i81cli = v1cksc
095240060216     c                   Eval      i81gcli = '1'
095250060216     c   01              Eval      i81icli = '1'
095260060208     c                   Call      'TNTA81R'
095270060208     c                   Parm                    kpjba
095280060208     c                   Parm                    tnta81ds
095290060208
095300060208      * Controllo se ora c'è o non c'è più il luogo
095310060208     c                   Eval      kspecli = v1cksc
095320060208     c                   Eval      kspecod = '300'
095330060208     c     kFnspe        Chain     Fnspe01l
095340060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
095350060208     c                   Eval      *In33 = *On
095360060208     c                   Else
095370060208     c                   Eval      *In33 = *Off
095380060208     c                   EndIf
095390060208
095400060208     c                   Clear                   v6cl300
095410060208     c                   Eval      h6riga = 15
095420060208     c                   Eval      h6colo = 35
095430060208     c                   Goto      emi06
095440060208
095450060208    1c                   EndIf
095460060208
095470060208     c                   EndSr
095480051213
095490051213      *------------------------------------------------------------------------*
095500051213      * CONTROLLO LUOGO 006 - PREAVVISO/AVVISO DANNO
095510051213      *------------------------------------------------------------------------*
095520051213     c     Sr_Ctrl006    BegSr
095530051213
095540051213if  1c                   If        v7cl006 <> *Blanks
095550060203
095560060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
095570060203     c                   If        *In05 and Not *In16
095580051213     c                   Eval      *In28 = *On
095590051213     c                   Eval      h7riga = 12
095600060203     c                   Eval      h7colo = 35
095610051213     c                   Eval      v7cmsg = msg(50)
095620051213     c                   Leavesr
095630051213     c                   EndIf
095640071128
095650071128      * se non è interrogazione controllo se luogo utilizzabile
095660071128     c                   if        not *in09 and not *in05 and $ufi006 <> 'S'
095670071128     c                   eval      *In28 = *on
095680071128     c                   eval      h7riga = 12
095690071128     c                   eval      h7colo = 35
095700071128     c                   eval      v7cmsg = msg(76)
095710071128     c                   leavesr
095720071128     c                   endif
095730051213
095740060203      * Imposto se interrogazione o gestione
095750051213     c                   Clear                   tnta81ds
095760060203     c   05              Eval      i81opz = '5'
095770060203     c  n05
095780060214     can 16              Eval      i81opz = '2'
095790051213     c                   Eval      i81cod = '006'
095800051213     c                   Eval      i81cli = v1cksc
095810060216     c                   Eval      i81gcli = '1'
095820060216     c   01              Eval      i81icli = '1'
095830051213     c                   Call      'TNTA81R'
095840051213     c                   Parm                    kpjba
095850051213     c                   Parm                    tnta81ds
095860051213
095870051213      * Controllo se ora c'è o non c'è più il luogo
095880051216     c                   Eval      kspecli = v1cksc
095890051216     c                   Eval      kspecod = '006'
095900051213     c     kFnspe        Chain     Fnspe01l
095910051213     c                   If        %Found(Fnspe01l) and speflg = *Blanks
095920051213     c                   Eval      *In16 = *On
095930051213     c                   Else
095940051213     c                   Eval      *In16 = *Off
095950051213     c                   EndIf
095960051213
095970051213     c                   Clear                   v7cl006
095980051213     c                   Eval      h7riga = 12
095990060203     c                   Eval      h7colo = 35
096000051213     c                   Goto      emi07
096010051213
096020051213    1c                   EndIf
096030051213
096040051213     c                   EndSr
096050051213
096060051213      *------------------------------------------------------------------------*
096070051220      * CONTROLLO NOTA 87 - E-MAIL DANNI
096080051213      *------------------------------------------------------------------------*
096090051213     c     Sr_Ctrnt87    BegSr
096100051213
096110051213if  1c                   If        v7cnt87 <> *Blanks
096120060203
096130060203      * Se sono in interrogazione non posso interrogare la nota se non c'è
096140060203     c                   If        *In05 and Not *In18
096150051213     c                   Eval      *In28 = *On
096160051213     c                   Eval      h7riga = 13
096170060203     c                   Eval      h7colo = 35
096180051219     c                   Eval      v7cmsg = msg(52)
096190051213     c                   Leavesr
096200051213     c                   EndIf
096210051213
096220060203      * Imposto se interrogazione o gestione
096230060203     c  n05              Eval      wrich = 'M'
096240060203     c   05              Eval      wrich = 'V'
096250060217     c                   Eval      wrag = v3crag
096260051213     c                   Eval      wtnt = '87'
096270051213     c                   ExSr      Sr_f02
096280060215
096290060215     c                   If        ta12err <> *Blanks
096300060215     c                   Eval      *In28 = *On
096310060215     c                   Eval      h7riga = 13
096320060215     c                   Eval      h7colo = 35
096330060215     c                   Eval      v7cmsg = ta12msg
096340060215     c                   Leavesr
096350060215     c                   EndIf
096360051213
096370060215      * Controllo se ora c'è o non c'è più la nota
096380091112     c   70
096390091112     corn06              Eval      kntcapl = 'C'
096400100806     c   06
096410091112     cann70              Eval      kntcapl = 'T'
096420051216     c                   Movel(p)  dutkci        kntcnk1
096430091112     c   70
096440091112     corn06              Move      v1cksc        kntcnk1
096450100806     c   06
096460100806     cann70              Move      v1cnrv        kntcnk1
096470051216     c                   Clear                   kntcnk2
096480051216     c                   Movel     '87'          kntctnt
096490091119     c     kTfntc        Chain(n)  Tfntc01l
096500051213     c                   If        %Found(Tfntc01l)
096510051213     c                   Eval      *In18 = *On
096520051213     c                   Else
096530051213     c                   Eval      *In18 = *Off
096540051213     c                   EndIf
096550051213
096560051213     c                   Clear                   v7cnt87
096570051213     c                   Eval      h7riga = 13
096580060203     c                   Eval      h7colo = 35
096590051213     c                   Goto      emi07
096600051213
096610051213    1c                   EndIf
096620051213
096630051213     c                   EndSr
096640051213
096650051213      *------------------------------------------------------------------------*
096660051213      * CONTROLLO LUOGO 008 - PROGETTO DI LIQUIDAZIONE
096670051213      *------------------------------------------------------------------------*
096680051213     c     Sr_Ctrl008    BegSr
096690051213
096700051213if  1c                   If        v7cl008 <> *Blanks
096710060203
096720060203      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
096730060203     c                   If        *In05 and Not *In17
096740051213     c                   Eval      *In28 = *On
096750051213     c                   Eval      h7riga = 14
096760060203     c                   Eval      h7colo = 35
096770051213     c                   Eval      v7cmsg = msg(50)
096780051213     c                   Leavesr
096790051213     c                   EndIf
096800071128
096810071128      * se non è interrogazione controllo se luogo utilizzabile
096820071128     c                   if        not *in09 and not *in05 and $ufi008 <> 'S'
096830071128     c                   eval      *In28 = *on
096840071128     c                   eval      h7riga = 14
096850071128     c                   eval      h7colo = 35
096860071128     c                   eval      v7cmsg = msg(76)
096870071128     c                   leavesr
096880071128     c                   endif
096890051213
096900060203      * Imposto se interrogazione o gestione
096910051213     c                   Clear                   tnta81ds
096920060203     c   05              Eval      i81opz = '5'
096930060203     c  n05
096940060214     can 17              Eval      i81opz = '2'
096950051213     c                   Eval      i81cod = '008'
096960051213     c                   Eval      i81cli = v1cksc
096970060216     c                   Eval      i81gcli = '1'
096980060216     c   01              Eval      i81icli = '1'
096990051213     c                   Call      'TNTA81R'
097000051213     c                   Parm                    kpjba
097010051213     c                   Parm                    tnta81ds
097020051213
097030051213      * Controllo se ora c'è o non c'è più il luogo
097040051216     c                   Eval      kspecli = v1cksc
097050051216     c                   Eval      kspecod = '008'
097060051213     c     kFnspe        Chain     Fnspe01l
097070051213     c                   If        %Found(Fnspe01l) and speflg = *Blanks
097080051213     c                   Eval      *In17 = *On
097090051213     c                   Else
097100051213     c                   Eval      *In17 = *Off
097110051213     c                   EndIf
097120051213
097130051213     c                   Clear                   v7cl008
097140051213     c                   Eval      h7riga = 14
097150060203     c                   Eval      h7colo = 35
097160051213     c                   Goto      emi07
097170051213
097180051213    1c                   EndIf
097190051213
097200051213     c                   EndSr
097210051213
097220051213      *------------------------------------------------------------------------*
097230051213      * CONTROLLO NUMERO STOP PER RITIRO
097240051213      *------------------------------------------------------------------------*
097250051213     c     Sr_Ctrstp     BegSr
097260051213
097270051213     c                   If        v7cstp > 800
097280051213     c                   Eval      *In28 = *On
097290051213     c                   Eval      h7riga = 18
097300051213     c                   Eval      h7colo = 37
097310051213     c                   Eval      v7cmsg = msg(67)
097320051213     c                   Leavesr
097330051213     c                   EndIf
097340051213
097350051213     c                   EndSr
097360060208
097370060208      *------------------------------------------------------------------------*
097380060208      * CONTROLLO LUOGO 002 - INVIO ORM ESTERO
097390060208      *------------------------------------------------------------------------*
097400060208     c     Sr_Ctrl002    BegSr
097410060208
097420060208if  1c                   If        v7cl002 <> *Blanks
097430060208
097440060208      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
097450060208     c                   If        *In05 and Not *In35
097460060208     c                   Eval      *In28 = *On
097470060208     c                   Eval      h7riga = 20
097480060208     c                   Eval      h7colo = 35
097490060208     c                   Eval      v7cmsg = msg(50)
097500060208     c                   Leavesr
097510060208     c                   EndIf
097520071128
097530071128      * se non è interrogazione controllo se luogo utilizzabile
097540071128     c                   if        not *in09 and not *in05 and $ufi002 <> 'S'
097550071128     c                   eval      *In28 = *on
097560071128     c                   eval      h7riga = 20
097570071128     c                   eval      h7colo = 35
097580071128     c                   eval      v7cmsg = msg(76)
097590071128     c                   leavesr
097600071128     c                   endif
097610060208
097620060208      * Imposto se interrogazione o gestione
097630060208     c                   Clear                   tnta81ds
097640060208     c   05              Eval      i81opz = '5'
097650060208     c  n05
097660060214     can 35              Eval      i81opz = '2'
097670060208     c                   Eval      i81cod = '002'
097680060208     c                   Eval      i81cli = v1cksc
097690060216     c                   Eval      i81gcli = '1'
097700060216     c   01              Eval      i81icli = '1'
097710060208     c                   Call      'TNTA81R'
097720060208     c                   Parm                    kpjba
097730060208     c                   Parm                    tnta81ds
097740060208
097750060208      * Controllo se ora c'è o non c'è più il luogo
097760060208     c                   Eval      kspecli = v1cksc
097770060208     c                   Eval      kspecod = '002'
097780060208     c     kFnspe        Chain     Fnspe01l
097790060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
097800060208     c                   Eval      *In35 = *On
097810060208     c                   Else
097820060208     c                   Eval      *In35 = *Off
097830060208     c                   EndIf
097840060208
097850060208     c                   Clear                   v7cl002
097860060208     c                   Eval      h7riga = 20
097870060208     c                   Eval      h7colo = 35
097880060208     c                   Goto      emi07
097890060208
097900060208    1c                   EndIf
097910060208
097920060208     c                   EndSr
097930060208
097940060208      *------------------------------------------------------------------------*
097950060208      * CONTROLLO NOTA 83 - E-MAIL ORM ESTERO
097960060208      *------------------------------------------------------------------------*
097970060208     c     Sr_Ctrnt83    BegSr
097980060208
097990060208if  1c                   If        v7cnt83 <> *Blanks
098000060208
098010060208      * Se sono in interrogazione non posso interrogare la nota se non c'è
098020060208     c                   If        *In05 and Not *In34
098030060208     c                   Eval      *In28 = *On
098040060208     c                   Eval      h7riga = 21
098050060208     c                   Eval      h7colo = 35
098060060208     c                   Eval      v7cmsg = msg(52)
098070060208     c                   Leavesr
098080060208     c                   EndIf
098090060208
098100060208      * Imposto se interrogazione o gestione
098110060208     c  n05              Eval      wrich = 'M'
098120060208     c   05              Eval      wrich = 'V'
098130060217     c                   Eval      wrag = v3crag
098140060208     c                   Eval      wtnt = '83'
098150060208     c                   ExSr      Sr_f02
098160060215
098170060215     c                   If        ta12err <> *Blanks
098180060215     c                   Eval      *In28 = *On
098190060215     c                   Eval      h7riga = 21
098200060215     c                   Eval      h7colo = 35
098210060215     c                   Eval      v7cmsg = ta12msg
098220060215     c                   Leavesr
098230060215     c                   EndIf
098240060208
098250060215      * Controllo se ora c'è o non c'è più la nota
098260091112     c   70
098270091112     corn06              Eval      kntcapl = 'C'
098280100806     c   06
098290091112     cann70              Eval      kntcapl = 'T'
098300060208     c                   Movel(p)  dutkci        kntcnk1
098310091112     c   70
098320091112     corn06              Move      v1cksc        kntcnk1
098330100806     c   06
098340100806     cann70              Move      v1cnrv        kntcnk1
098350060208     c                   Clear                   kntcnk2
098360060208     c                   Movel     '83'          kntctnt
098370091119     c     kTfntc        Chain(n)  Tfntc01l
098380060208     c                   If        %Found(Tfntc01l)
098390060208     c                   Eval      *In34 = *On
098400060208     c                   Else
098410060208     c                   Eval      *In34 = *Off
098420060208     c                   EndIf
098430060208
098440060208     c                   Clear                   v7cnt83
098450060208     c                   Eval      h7riga = 21
098460060208     c                   Eval      h7colo = 35
098470060208     c                   Goto      emi07
098480060208
098490060208    1c                   EndIf
098500060208
098510060208     c                   EndSr
098520060208
098530060208      *------------------------------------------------------------------------*
098540060208      * CONTROLLO LUOGO 200 - INVIO DOC. DOGANA
098550060208      *------------------------------------------------------------------------*
098560060208     c     Sr_Ctrl200    BegSr
098570060208
098580060208if  1c                   If        v7cl200 <> *Blanks
098590060208
098600060208      * Se sono in interrogazione non posso visualizzare il luogo se non c'è
098610060208     c                   If        *In05 and Not *In37
098620060208     c                   Eval      *In28 = *On
098630060208     c                   Eval      h7riga = 20
098640060208     c                   Eval      h7colo = 72
098650060208     c                   Eval      v7cmsg = msg(50)
098660060208     c                   Leavesr
098670060208     c                   EndIf
098680071128
098690071128      * se non è interrogazione controllo se luogo utilizzabile
098700071128     c                   if        not *in09 and not *in05 and $ufi200 <> 'S'
098710071128     c                   eval      *In28 = *on
098720071128     c                   eval      h7riga = 20
098730071128     c                   eval      h7colo = 72
098740071128     c                   eval      v7cmsg = msg(76)
098750071128     c                   leavesr
098760071128     c                   endif
098770060208
098780060208      * Imposto se interrogazione o gestione
098790060208     c                   Clear                   tnta81ds
098800060208     c   05              Eval      i81opz = '5'
098810060208     c  n05
098820060214     can 37              Eval      i81opz = '2'
098830060208     c                   Eval      i81cod = '200'
098840060208     c                   Eval      i81cli = v1cksc
098850060216     c                   Eval      i81gcli = '1'
098860060216     c   01              Eval      i81icli = '1'
098870060208     c                   Call      'TNTA81R'
098880060208     c                   Parm                    kpjba
098890060208     c                   Parm                    tnta81ds
098900060208
098910060208      * Controllo se ora c'è o non c'è più il luogo
098920060208     c                   Eval      kspecli = v1cksc
098930060208     c                   Eval      kspecod = '200'
098940060208     c     kFnspe        Chain     Fnspe01l
098950060208     c                   If        %Found(Fnspe01l) and speflg = *Blanks
098960060208     c                   Eval      *In37 = *On
098970060208     c                   Else
098980060208     c                   Eval      *In37 = *Off
098990060208     c                   EndIf
099000060208
099010060208     c                   Clear                   v7cl200
099020060208     c                   Eval      h7riga = 20
099030060208     c                   Eval      h7colo = 72
099040060208     c                   Goto      emi07
099050060208
099060060208    1c                   EndIf
099070060208
099080060208     c                   EndSr
099090060208
099100060208      *------------------------------------------------------------------------*
099110060208      * CONTROLLO NOTA 86 - E-MAIL MANIFEST
099120060208      *------------------------------------------------------------------------*
099130060208     c     Sr_Ctrnt86    BegSr
099140060208
099150060208if  1c                   If        v7cnt86 <> *Blanks
099160060208
099170060208      * Se sono in interrogazione non posso interrogare la nota se non c'è
099180060208     c                   If        *In05 and Not *In36
099190060208     c                   Eval      *In28 = *On
099200060208     c                   Eval      h7riga = 21
099210060208     c                   Eval      h7colo = 72
099220060208     c                   Eval      v7cmsg = msg(52)
099230060208     c                   Leavesr
099240060208     c                   EndIf
099250060208
099260060208      * Imposto se interrogazione o gestione
099270060208     c  n05              Eval      wrich = 'M'
099280060208     c   05              Eval      wrich = 'V'
099290060217     c                   Eval      wrag = v3crag
099300060208     c                   Eval      wtnt = '86'
099310060208     c                   ExSr      Sr_f02
099320060215
099330060215     c                   If        ta12err <> *Blanks
099340060215     c                   Eval      *In28 = *On
099350060215     c                   Eval      h7riga = 21
099360060215     c                   Eval      h7colo = 72
099370060215     c                   Eval      v7cmsg = ta12msg
099380060215     c                   Leavesr
099390060215     c                   EndIf
099400060208
099410060215      * Controllo se ora c'è o non c'è più la nota
099420091112     c   70
099430091112     corn06              Eval      kntcapl = 'C'
099440100806     c   06
099450091112     cann70              Eval      kntcapl = 'T'
099460060208     c                   Movel(p)  dutkci        kntcnk1
099470091112     c   70
099480091112     corn06              Move      v1cksc        kntcnk1
099490100806     c   06
099500100806     cann70              Move      v1cnrv        kntcnk1
099510060208     c                   Clear                   kntcnk2
099520060208     c                   Movel     '86'          kntctnt
099530091119     c     kTfntc        Chain(n)  Tfntc01l
099540060208     c                   If        %Found(Tfntc01l)
099550060208     c                   Eval      *In36 = *On
099560060208     c                   Else
099570060208     c                   Eval      *In36 = *Off
099580060208     c                   EndIf
099590060208
099600060208     c                   Clear                   v7cnt86
099610060208     c                   Eval      h7riga = 21
099620060208     c                   Eval      h7colo = 72
099630060208     c                   Goto      emi07
099640060208
099650060208    1c                   EndIf
099660060208
099670060208     c                   EndSr
099680051220
099690051220      *-------------------------------------------------------------------------
099700051220      * CONTROLLO IL BLOCCO/SBLOCCO DEL CLIENTE
099710051220      *-------------------------------------------------------------------------
099720051220     c     Sr_Ctrabl     BegSr
099730051220
099740051220     c                   Eval      wokabl = *Off
099750051220     c                   Clear                   w02tes
099760051220     c                   Clear                   w02msg
099770051220
099780051220      * se ho aggiornato blocco/sblocco i clienti di fnacr
099790051220if  1c                   If        acoabl <> *Blanks and savabl = *Blanks
099800051220     c                   Eval      w02tes = 'Il cliente è stato bloccato'
099810051220      * cliente bloccato richiamo pgm esterno per bloccare fnacr
099820051220     c                   Clear                   fior50ds
099830051220     c                   Move      acoksc        i50ksc
099840140113     c                   eval      I50abl = 'B'
099850051220     c                   Call      'FIOR50R'
099860051220     c                   Parm                    fior50ds
099870051220      * se torna errore emetto il msg di errore
099880051220if  2c                   If        o50err <> *Blanks
099890051220     c                   Eval      w02msg = o50msg
099900051220      * emetto il msg che ho bloccato i clienti su fnacr
099910051220   x2c                   Else
099920051220     c                   Eval      w02msg = 'Anagrafiche clienti ritiro collega-
099930051220     c                             te bloccate'
099940051220    2c                   EndIf
099950051220     c                   Eval      wokabl = *On
099960051220    1c                   EndIf
099970051220if  1c                   If        acoabl = *Blanks and savabl <> *Blanks
099980051220     c                   Eval      w02tes = 'Il cliente è stato sbloccato'
099990051220      * cliente sbloccato emetto solo il msg
100000051220     c                   Eval      w02msg = 'Anagrafiche clienti ritiro collega-
100010051220     c                             te non aggiornate'
100020051220     c                   Eval      wokabl = *On
100030051220    1c                   EndIf
100040051220
100050051220      * Emetto la window x avvisare l'utente
100060051220     c                   If        wokabl = *On
100070051220     c                   Exfmt     ta60w02
100080051220     c                   EndIf
100090051220
100100051220     c                   EndSr
100110100128
100120100128      *-------------------------------------------------------------------------
100130100128      * Richiamo interrogazione codici fatturazione con stessa p.IVA
100140100128      *-------------------------------------------------------------------------
100150100128     c     sr_ta40       begsr
100160100128
100170100128     c                   clear                   tnta40ds
100180100128     c                   IF        v2ivaeu = *blanks
100190100128     c                   eval      ITA40iva = v2ivait
100200100128     c                   else
100210100128     c                   eval      ITA40iva = v2civa
100220100128     c                   ENDIF
100230100128     c  n06              eval      ITA40ksc = v1cksc
100240100128     c   06              eval      ITA40ksc = ta60cli
100250100128     c                   eval      ITA40scf = clpscf
100260100128     c                   eval      ITA40rag = v2crag
100270100128     c                   call      'TNTA40R'
100280100128     c                   parm                    kpjba
100290100128     c                   parm                    tnta40ds
100300100128     c                   IF        OTA40err <> *blanks
100310100128     c                   eval      v4cmsg = OTA40msg
100320100128     c                   eval      *in28 = *on
100330100312     c                   leavesr
100340100128     c                   ENDIF
100350100312     c                   IF        OTA40scf = 0
100360100312     c                   Eval      v4cmsg = msg(33)
100370100312     c                   eval      *in28 = *on
100380100312     c                   leavesr
100390100312     c                   ENDIF
100400100128     c                   eval      v4cscf = %editc(OTA40scf:'X')
100410100128     c                   eval      wtnta40 = *on
100420100201     c                   clear                   v4dscf
100430100201     c                   Clear                   Tibs69ds
100440100201     c                   Move      v4cscf        I69kac
100450100201     c                   Call      'TIBS69R'
100460100201     c                   Parm                    Tibs69ds
100470100201     c                   Parm                    ds_cnaco
100480100201     c                   Parm                    ds_cnind
100490100201     c                   Parm                    ds_cnclp
100500100201     c                   Parm                    ds_fncls
100510100201     c                   Eval      wtibs69 = *On
100520100201     c                   Movel     w_acorag      v4dscf
100530100128
100540100128     c                   endsr
100550051216
100560080222      *-------------------------------------------------------------------------
100570080222      * Reperisco la nazione del sottoconto intestazione fattura
100580080222      *-------------------------------------------------------------------------
100590080222     c     Sr_Repscfsta  BegSr
100600080222
100610080222     c                   clear                   w_scfsta
100620080222      * Controllo
100630100127     c                   if        v4cscf <> %editc(v1cksc:'X')
100640110427     c                             and v4cscf > *zeros
100650080222     c                   Clear                   Tibs69ds
100660080222     c                   Move      v4cscf        I69kin
100670080222     c                   Call      'TIBS69R'
100680080222     c                   Parm                    Tibs69ds
100690080222     c                   Parm                    ds_cnaco
100700080222     c                   Parm                    ds_cnind
100710080222     c                   Parm                    ds_cnclp
100720080222     c                   Parm                    ds_fncls
100730080222     c                   Eval      wtibs69 = *On
100740080307     c                   if        o69err=*blanks and w_indflg=*blanks
100750080222     c                   eval      w_scfsta=w_indsta
100760080222     c                   endif
100770080227     c                   else
100780080227     c                   eval      w_scfsta=v2csta
100790080227     c                   endif
100800080222
100810080222     c                   EndSr
100820080306      *------------------------------------------------------------------------*
100830080306      * Controllo presenza codice su altro potenziale come cod.fiscale
100840080306      *------------------------------------------------------------------------*
100850080306     c     Sr_CdfAltroP  BegSr
100860080306     c     codice        setll     tncpo05l
100870080306     c     codice        reade     tncpo05l
100880080306    3c                   dow       not %eof(tncpo05l)
100890080306     c* se trovato un record "filiale memorizzo ragione sociale del potenz.
100900080306     c                   eval      savrag=c_cporag
100910080306     c     codice        reade     tncpo05l
100920080306    3c                   enddo
100930080306    3c                   if        not %eof (tncpo05l)
100940080306     c                   eval      savrag=c_cporag
100950080306    3c                   endif
100960080306     c                   endsr
100970080306      *------------------------------------------------------------------------*
100980080306      * Controllo presenza codice su altro potenziale come piva
100990080306      *------------------------------------------------------------------------*
101000080306     c     Sr_PivAltroP  BegSr
101010080306     c     codice        setll     tncpo06l
101020080306     c     codice        reade     tncpo06l
101030080306    3c                   dow       not %eof(tncpo06l)
101040080306     c* se trovato un record "filiale memorizzo ragione sociale del potenz.
101050080306     c                   eval      savrag=c_cporag
101060080306     c     codice        reade     tncpo06l
101070080306    3c                   enddo
101080080306    3c                   if        not %eof (tncpo06l)
101090080306     c                   eval      savrag=c_cporag
101100080306    3c                   endif
101110080306     c                   endsr
101120080306      *------------------------------------------------------------------------*
101130080306      * Messaggio di errore codice fiscal già presente per altro potenz
101140080306      *------------------------------------------------------------------------*
101150080306     c     Sr_msgcdf     BegSr
101160080306     c* Errore forzabile se trovato un altro potenziale con lo stesso
101170080306     c* codice fiscale
101180080306    3c                   if        savrag<>*blanks
101190080306     c                   eval      cod_forzcdf=codice
101200080306     c                   seton                                        28
101210080306     c                   eval      h2riga = 12
101220080306     c                   eval      h2colo = 28
101230080306     c                   Eval      v2cmsg = msg(77)
101240080306     c                   Leavesr
101250080306    3c                   endif
101260080306     c                   endsr
101270080306      *------------------------------------------------------------------------*
101280080306      * Messaggio di errore partita iva   già presente per altro potenz
101290080306      *------------------------------------------------------------------------*
101300080306     c     Sr_msgiva     BegSr
101310080306
101320080306    3c                   if        savrag<>*blanks
101330080306     c                   eval      cod_forziva=v2civa
101340080306     c                   seton                                        28
101350080306     c                   Eval      h2riga = 12
101360080306     c                   Eval      h2colo = 52
101370080306     c                   Eval      v2cmsg = msg(78)
101380080306     c                   Leavesr
101390080306    3c                   endif
101400080306     c                   endsr
101410080422
101420080422      *------------------------------------------------------------------------*
101430080422      * Emissione Window per inviare mail alla sede
101440080422      *------------------------------------------------------------------------*
101450080422     c     sr_winmail    begsr
101460080422
101470100729     c                   eval      w04uffsede = wuffsede
101480080422Do  2c                   do        *hival
101490080422     c                   exfmt     ta60w04
101500080422If  3c                   if        *inkl
101510080422     c                   leavesr
101520080422    3c                   endif
101530080422    2c                   enddo
101540080422
101550080422     c                   endsr
101560100729      /free
101570100729       //--------------------------------------------------------------
101580100729       //?Descrizione banca.
101590100729       //--------------------------------------------------------------
101600100729       BEGSR DesBanca;
101610100729
101620100729         clear wDesBanca;
101630100729         chain (kabi:kcab) CNABI00F;
101640100729         IF  %Found(CNABI00F) and ABIann <> '*';
101650100729           exsr sr_crebna;
101660100729           wDesbanca = wbanca + wagenzia;
101670100729         ENDIF;
101680100729
101690100729       ENDSR;
101700150427
101710150427       //--------------------------------------------------------------
101720150427       //?Memorizza immagine precedente.
101730150427       //--------------------------------------------------------------
101740150427       BEGSR ImmaginePrima;
101750150427
101760150427         clear TIBS73DS;
101770150427         IBS73immag ='P';
101780150427         tibs73r (TIBS73DS:cnaco_73:cnind_73:cnclp_73:fncls_73);
101790150427         wtibs73 = *on;
101800150427
101810150427       ENDSR;
101820150427
101830150427       //--------------------------------------------------------------
101840150427       //?Memorizza immagine sucessiva.
101850150427       //--------------------------------------------------------------
101860150427       BEGSR ImmagineDopo;
101870150427
101880150427         clear TIBS73DS;
101890150427         IBS73pru = knmus;
101900150427         IBS73noj = knmeb;
101910150427         ibs73pgm = 'TNTA60R';
101920150427         IBS73immag ='D';
101930150427         IF  *in02;
101940150427           IBS73cta = 'M';
101950150427         ENDIF;
101960150427         IF  *in01;
101970150427           IBS73cta = 'I';
101980150427         ENDIF;
101990150427         tibs73r (TIBS73DS:cnaco_73:cnind_73:cnclp_73:fncls_73);
102000150427         wtibs73 = *on;
102010150427
102020150427       ENDSR;
102030160803
102040160803       //--------------------------------------------------------------
102050160803       //?Scrivo l'immagine Precedente.
102060160803       //--------------------------------------------------------------
102070160803       BEGSR ImmaginePrimaCpo;
102080160803
102090160803         clear TRMK30DS;
102100160803         IMK30immag = 'P';
102110160803         IMK30tvp = 'B';
102120160803         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
102130160803
102140160803       ENDSR;
102150160803
102160160803       //--------------------------------------------------------------
102170160803       //?Scrivo Storico Variazioni.
102180160803       //--------------------------------------------------------------
102190160803       BEGSR ScriviVarCpo;
102200160803
102210160803         clear TRMK30DS;
102220160803         IMK30pru = knmus;
102230160803         IMK30noj = knmeb;
102240160803         IMK30pgm = 'TNTA60R';
102250160803         IMK30immag = 'D';
102260160803         IMK30cta = 'M';
102270160803         IMK30tvp = 'B';
102280160803
102290160803         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
102300160803
102310160803       ENDSR;
102320150427
102330100729      /end-free
102340080222
102350051107      *------------------------------------------------------------------------*
102360051107      * ROUTINE INIZIALE
102370051107      *------------------------------------------------------------------------*
102380051107     c     *Inzsr        BegSr
102390051107
102400051107     c     *Entry        Plist
102410051107     c                   Parm                    Kpjba
102420051115     c                   Parm                    Tnta60ds
102430110407     c                   Parm                    ParmCbl
102440091104
102450091104     c                   eval      $interroga = *off
102460091124     c                   eval      $modifica  = *off
102470091106     c                   eval      $pgmrich   = *off
102480110407     c                   eval      $Blocco    = *off
102490171006       savin05 = *off;
102500051115
102510091104      * PGM Richiamato
102520051115     c                   If        %Parms > 1
102530091106     c                   eval      $pgmrich = *on
102540100806      * trattativa
102550100806     c                   if        ta60flg = 'T'
102560090914     c                   Eval      *In06 = *on
102570091112     c                   eval      *in70 = (ta60cli > 0)
102580091112     c   70              eval      v1cksc = ta60cli
102590091104      * cliente per aggancio a cnaco
102600090914     c                   else
102610090914     c                   eval      *in06 = *off
102620091112     c                   eval      *in70 = *off
102630090914     c                   endif
102640091104      * convalida offerta
102650091104      * sono in manutenzione perchè il cliente viene scritto dal chiamante
102660051115     c                   Eval      *In07 = (ta60flg = 'C')
102670091104      * codice visita/trattativa da agganciare
102680051222     c   06              Eval      v1cnrv = ta60nrv
102690091104      * codice cliente nuovo da agganciare
102700051222     c   07              Eval      v1cksc = ta60nrv
102710091104
102720091104      * inibisco il richiamo all'interrogazione potenziali per errore
102730091104      * chiamata ricorsiva se provengo da gestione potenziali
102740091104     c                   eval      *in65 = (ta60pot = '1')
102750091104      * interrogazione se richiamato da interrogazione tariffe
102760060207     c                   Eval      *In05 = (ta60flg = 'I')
102770171006       savin05 = *in05;
102780091104
102790091104      * richiamato per visualizzazione (nuovo campo della DS)
102800091104     c                   if        ta60int = 'S'
102810091104     c                   eval      $interroga = *on
102820091104      * devo avere il codice cliente/visita/trattativa
102830091104      * visto che non passo dalla prima videata dove viene richiesto il codice
102840091104      * per comodità ricevo tutti e 3 i codici nello stesso campo
102850091104      * tanto o sono da visita o sono da trattativa o sono da clienti
102860091104      * se il codice è a 0 torno l'errore al chiamante
102870091104     c                   if        ta60nrv = 0
102880091104     c                   eval      ta60msg = 'Codice non passato'
102890091104     c                   eval      ta60err = '1'
102900091124     c                   Eval      *In28 = *On
102910091104     c                   leavesr
102920091104     c                   endif
102930091111      * accendo l'indicatore di interrogazione
102940091111     c                   eval      *in05 = *on
102950171006       savin05 = *in05;
102960091111      * se flag TA60FLG = 'I'
102970091111      * imposto il codice cliente, in quanto è l'unico caso non
102980091104      * previsto nelle specifiche precedenti, visita e trattativa
102990091104      * sono già impostate se TA60FLG = 'T' o 'V'
103000091106     c                   if        ta60flg = 'I'
103010091111     c                   eval      v1cksc = ta60nrv
103020091104     c                   endif
103030091104     c                   endif
103040091124      * in caso di pgm richiamato in manutenzione
103050091124     c                   if        ta60flg = 'M'
103060091124     c                   if        ta60cli = 0
103070091124     c                   eval      ta60msg = 'Codice non passato'
103080091124     c                   eval      ta60err = '1'
103090091124     c                   Eval      *In28 = *On
103100091124     c                   leavesr
103110091124     c                   endif
103120091124     c                   eval      $modifica = *on
103130091124     c                   eval      v1cksc = ta60cli
103140091124     c                   endif
103150110407
103160110407      * PGM Richiamato per blocco cliente
103170110407     c                   IF        %Parms > 2
103180110407     c                   IF        Parmcbl = *blanks or Parmcbl = '000'
103190110407     c                   eval      ta60msg = 'Causale blocco errata'
103200110407     c                   eval      ta60err = '1'
103210110407     c                   Eval      *In28 = *On
103220110407     c                   leavesr
103230110407     c                   ENDIF
103240110407     c                   eval      $Blocco = *on
103250110407     c                   ENDIF
103260091104
103270091104      * NON richiamato
103280051115     c                   Else
103290091104      * Interrogazione
103300051115     c                   If        %subst(kpjbu:256:1) = 'V'
103310051115     c                   Eval      *In05 = *On
103320171006       savin05 = *in05;
103330051115     c                   EndIf
103340051222     c                   Clear                   v1cnrv
103350060308     c                   eval      *in65 = *off
103360051115     c                   EndIf
103370101130
103380101130      * Se pgm in interrogazione imposto subito il titolo 'VISUALIZZA'
103390101130     c   05              Eval      vtcmod = mod(03)
103400051107
103410051107     c     *dtaara       define    §azute        azuteds
103420051107     c     *dtaara       define    §datiute      ddatiute
103430051107     c                   in(E)     *dtaara
103440051117     c                   If        %error  or rsut = *blanks
103450051107     c                   Clear                   Tibs34ds
103460051107     c                   Call      'TIBS34R'
103470051107     c                   Parm                    Tibs34ds
103480051107     c                   In        *dtaara
103490051107     c                   EndIf
103500051107
103510051107     c                   Clear                   wabi
103520051107     c                   Clear                   dLat
103530051107
103540051107      * Verifica errori e autorità profilo
103550051107s   1c                   Select
103560051107      * se ho errori nei dati utente esco dal pgm
103570051117w   1c                   When      duterr = 'E'
103580051115     c                   Eval      *In08 = *On
103590051115     c                   Eval      *In28 = *On
103600051115     c                   Eval      v1cmsg = msg(01)
103610051115      * se richiamato passo il messaggio
103620051115     c                   If        *In06
103630051115     c                   Eval      ta60msg = v1cmsg
103640051115     c                   Eval      ta60err = '1'
103650051115     c                   EndIf
103660051115     c                   Leavesr
103670051107      * se non c'è l'abilitazione
103680051107      * --> se 1° livello, abilitazioni al terminal
103690051107      *     se 2° livello, abilitazioni al punto operativo
103700051107      *     se sede è impossibile ma se capita mando a fine il pgm
103710051117w   1c                   When      uteaut = *Blanks
103720051117if  2c                   If        dutlpo = '1'
103730051107     c                   Eval      wabi   = 'TP'
103740051107e   2c                   EndIf
103750051117if  2c                   If        dutlpo = '2'
103760051107     c                   Eval      wabi   = 'PO'
103770051107e   2c                   EndIf
103780051117if  2c                   If        dutlpo = 'S'
103790051115     c                   Eval      *In08 = *On
103800051115     c                   Eval      *In28 = *On
103810051115     c                   Eval      v1cmsg = msg(01)
103820051115      * se richiamato passo il messaggio
103830051115     c                   If        *In06
103840051115     c                   Eval      ta60msg = v1cmsg
103850051115     c                   Eval      ta60err = '1'
103860051115     c                   EndIf
103870051115     c                   Leavesr
103880051107e   2c                   EndIf
103890051107      * carica le abilitazioni del profilo
103900051107x   1c                   Other
103910051117     c                   Movel     utefaf        dute01
103920051117if  2c                   If        §utecli <> *Blanks
103930051117     c                   Eval      wabi = §utecli
103940051107   x2c                   Else
103950051117     c                   Eval      wabi = uteaut
103960051107e   2c                   EndIf
103970051107e   1c                   EndSl
103980051107
103990051107      * controllo se ok l'abilitazione dell'utente
104000051107     c                   Clear                   Tibs02ds
104010051116     c                   Eval      t02mod = 'C'
104020051116     c                   Eval      t02sif = knsif
104030051116     c                   Eval      t02cod = 'LAT'
104040051116     c                   Movel(p)  wabi          t02ke1
104050051107     c                   Call      'TIBS02R'
104060051107     c                   Parm                    kpjba
104070051107     c                   Parm                    Tibs02ds
104080051117     c                   If        t02err = *Blanks
104090051116     c                   Eval      dLat = t02uni
104100051117     c                   EndIf
104110051107      * errore o non abilitato
104120051117     c                   If        t02err <> *Blanks or §latabi = 'S'
104130051107     c                   Eval      *In08 = *On
104140051107     c                   Eval      *In28 = *On
104150051108     c                   Eval      v1cmsg = msg(01)
104160051115      * se richiamato passo il messaggio
104170051115     c                   If        *In06
104180051115     c                   Eval      ta60msg = v1cmsg
104190051115     c                   Eval      ta60err = '1'
104200051115     c                   EndIf
104210051115     c                   Leavesr
104220051117     c                   EndIf
104230051107
104240170914      * Reperimento delle Filiali da proporre a video per la Ricerca
104250170914     c                   clear                   TRUL31ds
104260170914     c*//*no:*           if        *in05  = *on   and
104270170914     c*// no:                      wAbi  <> 'DI'  and
104280170914     c*// no:                      wAbi  <> 'AZ'
104290170914if  1c                   if        ( wAbi = 'TP'  or
104300170914     c                               wAbi = 'PO' )
104310170914     c                   eval      i31abi = 'RA'
104320170915x   1c                   else
104330170915     c                   eval      i31abi = wabi
104340170915e   1c                   endif
104350170914     c                   eval      i31cdi = DUTdis
104360170914     c                   eval      i31car = DUTare
104370170914     c                   eval      i31cpo = DUTpou
104380170914     c                   call      'TRUL31R'
104390170914     c                   parm                    kpjba
104400170914     c                   parm                    TRUL31ds
104410170914if  2c                   if        o31pog > *zeros
104420170914     c                   movea     o31pog        skPog_RA
104430170914x   2c                   else
104440170914     c                   eval      *in08 = *On
104450170914     c                   eval      *in28 = *On
104460170914     c                   eval      V1Cmsg = Msg(01)
104470170914      * se richiamato passo il messaggio
104480170914if  3c                   if        *in06
104490170914     c                   eval      TA60msg = V1Cmsg
104500170914     c                   eval      TA60err = '1'
104510170914e   3c                   endif
104520170914     c                   leavesr
104530170914e   2c                   endif
104540170915e   1c*//*               endif
104550170914
104560051117      * Reperimento dei P.O. gestibili dall'utente per i codici clienti
104570051107     c                   Clear                   Trul31ds
104580051117     c                   Eval      i31abi = wabi
104590051117     c                   Eval      i31cdi = dutdis
104600051117     c                   Eval      i31car = dutare
104610051117     c                   Eval      i31cpo = dutpou
104620051107     c                   Call      'TRUL31R'
104630051117     c                   Parm                    kpjba
104640051107     c                   Parm                    Trul31ds
104650051117     c                   If        o31pog > *zeros
104660051117     c                   Movea     o31pog        skpog
104670051117     c                   Else
104680051107     c                   Eval      *In08 = *On
104690051107     c                   Eval      *In28 = *On
104700051107     c                   Eval      v1cmsg = msg(01)
104710051115      * se richiamato passo il messaggio
104720051115     c                   If        *In06
104730051115     c                   Eval      ta60msg = v1cmsg
104740051115     c                   Eval      ta60err = '1'
104750051115     c                   EndIf
104760051115     c                   Leavesr
104770051117     c                   EndIf
104780160905      /free
104790160905       //?Se utente abilitato alla gestione clienti logistica aggiungo
104800160905       //?alla sk SKPOG le filiali con NTW LOG
104810160905         IF  §UTEKscLog = 'S';
104820160905           exsr CaricaFilLog;
104830160905         ENDIF;
104840160905      /end-free
104850051115
104860051117      * Reperimento dei P.O. gestibili dall'utente per i codici potenziali
104870051117     c                   If        §utepot <> *Blanks
104880051117     c                   Eval      wabi = §utepot
104890051117     c                   EndIf
104900051117     c                   Clear                   Trul31ds
104910051117     c                   Eval      i31abi = wabi
104920051117     c                   Eval      i31cdi = dutdis
104930051117     c                   Eval      i31car = dutare
104940051117     c                   Eval      i31cpo = dutpou
104950051117     c                   Call      'TRUL31R'
104960051117     c                   Parm                    kpjba
104970051117     c                   Parm                    Trul31ds
104980051117     c                   If        O31pog > *zeros
104990051117     c                   Movea     O31pog        skpogcpo
105000051117     c                   EndIf
105010051117
105020051115      * Controllo se sono in sede
105030051115     c                   Eval      *In09 = (simfel = *zeros)
105040051128
105050051128      * Controllo se utente EDP
105060051129     c                   Eval      *In20 = (%subst(knmus:1:3) = 'EDP')
105070051116
105080060110      * Se sono in filiale
105090060110if  1c                   If        Not *In09
105100060110      * Verifico se il P.O. utente può manutenzionare l'anagrafico clienti
105110051116      * deve esistere la tabella MTC per il p.o. utente
105120051116     c                   Clear                   Tibs02ds
105130051116     c                   Clear                   dmtc
105140051116     c                   Eval      t02mod = 'C'
105150051116     c                   Eval      t02sif = knsif
105160051116     c                   Eval      t02cod = 'MTC'
105170051116     c                   Movel(p)  dutpou        t02ke1
105180051116     c                   Call      'TIBS02R'
105190051116     c                   Parm                    kpjba
105200051116     c                   Parm                    Tibs02ds
105210060110if  2c                   If        t02err <> *Blanks
105220051116     c                   Eval      *In28 = *On
105230051116     c                   Eval      v1cmsg = msg(01)
105240051116      * se richiamato passo il messaggio
105250051116     c                   If        *In06
105260051116     c                   Eval      ta60msg = v1cmsg
105270051116     c                   Eval      ta60err = '1'
105280051116     c                   EndIf
105290051116     c                   Leavesr
105300060110   x2c                   Else
105310051125     c                   Eval      dmtc = t02uni
105320060110    2c                   EndIf
105330060110
105340060110      * Se sono in sede deve esserci la tabella MTC key sede
105350060110   x1c                   Else
105360060110     c                   Clear                   Tibs02ds
105370060110     c                   Clear                   dmtc
105380060110     c                   Eval      t02mod = 'C'
105390060110     c                   Eval      t02sif = knsif
105400060110     c                   Eval      t02cod = 'MTC'
105410060110     c                   Movel(p)  'SEDE'        t02ke1
105420060110     c                   Call      'TIBS02R'
105430060110     c                   Parm                    kpjba
105440060110     c                   Parm                    Tibs02ds
105450060110if  2c                   If        t02err <> *Blanks
105460060110     c                   Eval      *In28 = *On
105470060110     c                   Eval      v1cmsg = msg(01)
105480060110      * se richiamato passo il messaggio
105490091106     c                   If        *In06
105500060110     c                   Eval      ta60msg = v1cmsg
105510060110     c                   Eval      ta60err = '1'
105520060110     c                   EndIf
105530060110     c                   Leavesr
105540060110   x2c                   Else
105550060110     c                   Eval      dmtc = t02uni
105560060110    2c                   EndIf
105570060110    1c                   EndIf
105580051115
105590051117      * Apro i file delle anagrafiche provvisorie se richiamato da gest.visite
105600051115     c                   If        *In06
105610051115     c                   Open      Tfaco00f
105620051115     c                   Open      Tfind00f
105630051115     c                   Open      Tfclp00f
105640051117     c                   Open      Tfcls01l
105650080306     c                   Open      Tncpo05l
105660080306     c                   Open      Tncpo06l
105670051115     c                   EndIf
105680051117
105690051117      * Apro i file delle anagrafiche effettive se non richiamato da gest.visite
105700051117     c                   If        Not *In06
105710051117     c                   Open      Cnind00f
105720060105     c                   Open      Cnind02l
105730051117     c                   Open      Cnclp00f
105740051220     c                   Open      Fnacr01l
105750051117     c                   Open      Fncls01l
105760051117     c                   Open      Fnspe01l
105770051219     c                   Open      Fnsp201l
105780060526     c                   EndIf
105790091112
105800091112      * Apro i file dei luoghi se trattativa su cliente
105810091112     c                   If        *In70
105820091112     c                   Open      Fnspe01l
105830091112     c                   Open      Fnsp201l
105840091112     c                   EndIf
105850051115
105860051222      * Passo i dati del capoconto se sono richiamato da visite/offerte
105870091106      * visto che salto la prima videata di richiesta codice
105880091124     c                   If        $pgmrich
105890051115     c                   Eval      v1ckcc = dutkci
105900051115     c                   EndIf
105910051107
105920051107      * Carico £4 capoconti gestiti solo in filiale e non in sede
105930051107     c                   Clear                   £4
105940051107     c                   Eval      kcod = '£4'
105950051108     c                   Eval      kkey = '       1'
105960051108     c     kTabel        Chain     Tabel00f
105970051107     c                   If        %Found(Tabel00f) and
105980051125     c                             tblflg = *Blanks
105990051125     c                   Movea     tbluni        £4
106000051107     c                   EndIf
106010051117
106020051117      * Recupero la divisa da tabella 'GED'
106030051117     c                   Clear                   Tibs02ds
106040051117     c                   Clear                   dged
106050051117     c                   Eval      t02mod = 'C'
106060051117     c                   Eval      t02sif = knsif
106070051117     c                   Eval      t02cod = 'GED'
106080051117     c                   Eval      t02ke1 = '1'
106090051117     c                   Call      'TIBS02R'
106100051117     c                   Parm                    kpjba
106110051117     c                   Parm                    Tibs02ds
106120051117     c                   If        t02err = *Blanks
106130051125     c                   Eval      dged = t02uni
106140051117     c                   EndIf
106150051117
106160051117      * Recupero il diritto di fatturazione di cartello
106170051117     c                   Clear                   Tibs02ds
106180051117     c                   Clear                   dgei
106190051117     c                   Eval      t02mod = 'C'
106200051117     c                   Eval      t02sif = knsif
106210051117     c                   Eval      t02cod = 'GEI'
106220051117     c                   Eval      t02ke1 = §gedcn
106230051117     c                   Call      'TIBS02R'
106240051117     c                   Parm                    kpjba
106250051117     c                   Parm                    Tibs02ds
106260051117     c                   If        t02err = *Blanks
106270051125     c                   Eval      dgei = t02uni
106280051117     c                   EndIf
106290051221
106300051221      * Recupero i dft tipo comunicazioni giacenze
106310051221     c                   Clear                   ds2g
106320051221     c                   Eval      kcod = '2G'
106330051221     c                   Eval      kkey = '1'
106340051221     c     kTabel        Chain     Tabel00f
106350051221     c                   If        %Found(Tabel00f) and
106360051221     c                             tblflg = *Blanks
106370051221     c                   Eval      ds2g = tbluni
106380051221     c                   EndIf
106390051219
106400051219      * Aggancio il capoconto su Cnaco per recuperare acoopz e acotic
106410051219      * uguali per tutti i clienti
106420051219     c     kCnaco1       Chain(n)  Cnaco00f
106430051219     c                   If        %Found(Cnaco00f)
106440051219     c                   Eval      wopz = acoopz
106450051219     c                   Eval      wtic = acotic
106460051219     c                   Else
106470051219     c                   Eval      wopz = '1100100001'
106480051219     c                   Eval      wtic = 'CG'
106490051219     c                   EndIf
106500051219
106510051129      * Imposto la data odierna
106520051129     c                   Z-add     *date         dataoggi
106530051129      * Imposto la data odierna -1 anno
106540051129     c                   Move      dataoggi      dataiso
106550051129     c                   Subdur    1:*y          dataiso
106560051129     c                   Move      dataiso       dataoggi1
106570100805
106580150424       //?cerco il tipo pag. di default da impostare in immissione
106590100805         exec sql
106600100805         declare PAG cursor for
106610100805         select TBEke1, TBEke2 from TNTBE00F
106620100805         where  TBEcod = 'PAG' and substr(TBEuni, 37, 2) = 'SI';
106630100805
106640100805         exec sql
106650100805           open PAG;
106660100805           IF sqlcode < 0;
106670100805             $End = *on;
106680100805           ENDIF;
106690100805
106700100805         DOW not $End;
106710100805           exec sql
106720100805             fetch next from PAG into :TBEke1, :TBEke2;
106730100805             IF sqlcod = 100 or sqlcod < 0;
106740100805               $End = *on;
106750100805               leave;
106760100805             ENDIF;
106770150424             IF  TBEke1 = 'DN';
106780100805               dfttipdn = TBEke2;
106790100805             ENDIF;
106800150424             IF  TBEke1 = 'NA';
106810100805               dfttipna = TBEke2;
106820100805             ENDIF;
106830100805         ENDDO;
106840100805
106850100805         exec sql close PAG;
106860170421
106870170421       //?carico in sk i clienti IMPORT DPD
106880170421         $End = *off;
106890170421         clear ww;
106900170421         exec sql
106910170421         declare TabLPD cursor for
106920170421         select TBEuni from TNTBE00F
106930170421         where TBEcod = 'LPD';
106940170421         exec sql open TabLPD;
106950170421         IF  sqlcode < 0;
106960170421           $End = *on;
106970170421         ENDIF;
106980170421         DOW  not $End;
106990170421           exec sql fetch next from TABLPD into :TBEuni;
107000170421           IF  sqlcod = 100 or sqlcod < 0;
107010170421             $End = *on;
107020170421             leave;
107030170421           ENDIF;
107040170421           dLPD = TBEUni;
107050170421           IF  §LPDksc = 0;
107060170421             iter;
107070170421           ENDIF;
107080170421           ww += 1;
107090170421           CliImport(ww) = §LPDksc;
107100170421         ENDDO;
107110170421         exec sql close TabLPD;
107120100805
107130100805      /end-free
107140051107
107150051107      * KLIST
107160051129     c     kCnabi        Klist
107170051129     c                   Kfld                    kabi
107180051129     c                   Kfld                    kcab
107190051129
107200051108     c     kCnaco        Klist
107210051108     c                   Kfld                    codut
107220051107     c                   Kfld                    v1ckcc
107230051222     c                   Kfld                    kksc
107240051116
107250051116     c     kCnaco1       Klist
107260051116     c                   Kfld                    codut
107270051116     c                   Kfld                    v1ckcc
107280051116     c                   Kfld                    wksc
107290060105
107300060105     c     kCnind        Klist
107310060105     c                   Kfld                    codut
107320060105     c                   Kfld                    v1ckcc
107330060109     c                   Kfld                    kindiva
107340051108
107350051108     c     kFnspe        Klist
107360051216     c                   Kfld                    kspefls
107370051216     c                   Kfld                    kspecli
107380051216     c                   Kfld                    kspecod
107390051219
107400051219     c     kFnsp2        Klist
107410051219     c                   Kfld                    kspecli
107420051219     c                   Kfld                    kspecod
107430051219     c                   Kfld                    ksp2tpe
107440051129
107450051129     c     kTabel        Klist
107460051129     c                   Kfld                    codut
107470051129     c                   Kfld                    kcod
107480051129     c                   Kfld                    kkey
107490051108
107500051129     c     kTfntc        klist
107510051216     c                   kfld                    kntcapl
107520051216     c                   kfld                    kntcnk1
107530051216     c                   kfld                    kntcnk2
107540051216     c                   kfld                    kntctnt
107550051107
107560051107     c                   EndSr
107570160905      /free
107580160905       //--------------------------------------------------------------
107590160905       //?Carico le filiali Logistica nella SKPOG.
107600160905       //--------------------------------------------------------------
107610160905       BEGSR CaricaFilLog;
107620160905
107630160905         $End = *off;
107640160905
107650160905         exec sql
107660160905         declare WLOG cursor for
107670160905         select ORGfil from AZORGLOG;
107680160905
107690160905         exec sql
107700160905         open WLOG;
107710160905         IF  sqlcode < 0;
107720160905           $End = *on;
107730160905         ENDIF;
107740160905
107750160905         DOW not $End;
107760160905           exec sql
107770160905           fetch next from WLOG into :ORGfil;
107780160905           IF  sqlcod = 100 or sqlcod < 0;
107790160905             $End = *on;
107800160905             leave;
107810160905           ENDIF;
107820160905
107830160905           IF  %lookup(%editc(ORGfil:'X'):SKpog) = 0;
107840160905             ww = %lookup(*zeros:SKpog);
107850170925             IF  ww > 0 and ww < 250;
107860160905               SKpog(ww) = %editc(ORGfil:'X');
107870160905             ENDIF;
107880160905           ENDIF;
107890170914
107900170914           If  %lookup( %editc(ORGfil:'X') : SKpog_RA ) = *zero;
107910170914             ww = %lookup( *zeros : SKpog_RA );
107920170925             if  ww > *zero  and  ww < %elem(skPog_RA);
107930170914               SKpog_RA(ww) = %editc(ORGfil:'X');
107940170914             endif;
107950170914           EndIf;
107960160905
107970160905         ENDDO;
107980160905
107990160905         exec sql close WLOG;
108000160905
108010160905       ENDSR;
108020160905      /end-free
108030051107      *------------------------------------------------------------------------*
108040051212** CAR  Lungh. 10
108050051212!:§@
108060051107** MOD  Lungh. 15                                                            *
108070051107  IMMISSIONE                                                                  01
108080051107 MANUTENZIONE                                                                 02
108090051107VISUALIZZAZIONE                                                               03
108100051107   ANNULLATO                                                                  04
108110051107** MSG  Lungh. 78                                                            *
108120051107Utente non autorizzato alla Gestione anagrafico clienti                       01
108130051107Immettere almeno un dato                                                      02
108140051107Immettere il capoconto                                                        03
108150051107Capoconto non gestibile                                                       04
108160051115Cliente inesistente                                                           05
108170051116Password errata                                                               06
108180051116Immettere la password                                                         07
108190051117Password scaduta                                                              08
108200051117Manca tabella di controllo scadenza password. TEL CED SEDE !!!!!              09
108210051117Cliente non gestibile dall'utente                                             10
108220051117Immettere la ragione sociale                                                  11
108230051118Immettere il codice potenziale                                                12
108240051118Codice potenziale errato                                                      13
108250051118Cliente potenziale non gestibile dall'utente                                  14
108260051118Stato che non prevede il cappario: utilizzabile solo per clienti bloccati     15
108270051118Impossibile utilizzare cap o localita' obsoleti                               16
108280051118Stato errato                                                                  17
108290051118Immettere la partita IVA                                                      18
108300051118Codice fiscale formalmente errato                                             19
108310051122Codice commerciale errato                                                     20
108320090617Fil. del codice commerciale incongruente con la fil. del cliente              21
108330051122Il cliente è da bloccare perchè inattivo                                      22
108340051122Stato del credito errato                                                      23
108350090617Immettere stato del credito gest. da fil. e senza passaggio al contenzioso    24
108360051122Causale blocco cliente errata                                                 25
108370051122Immettere la causale blocco perchè il cliente è bloccato                      26
108380051122Per immettere la causale blocco occorre bloccare il cliente                   27
108390051128Immettere il codice importanza cliente                                        28
108400051128Codice importanza cliente errata                                              29
108410080313Immettere la categoria Merceologica                                           30
108420080313Categoria Merceologica errata                                                 31
108430051212La ragione sociale contiene uno dei seguenti caratteri non validi -- ! : § @  32
108440051128Codice Sottoconto fattura non impostato                                       33
108450051128Codice Sottoconto fattura non utilizzabile                                    34
108460051128Codice Sottoconto fattura errato                                              35
108470051128Codice Sottoconto fattura annullato                                           36
108480051128Tipo fattura errato                                                           37
108490051128Tipo fattura non utilizzabile in                                              38
108500051128Immettere la frequenza fattura                                                39
108510051128Frequenza fattura errato                                                      40
108520051128Per tipo fattura "1" è ammessa solo la frequenza mensile                      41
108530120613Per frequenza settimanale chiedere autorizzazione alla DIR. AMMINISTRATIVA!!  42
108540051128Immettere il tipo data fattura                                                43
108550051128Tipo data fattura errato                                                      44
108560051128Sottoconto fattura diverso da codice cliente è possibile unificare            45
108570051128Verificare il flag Fattura Unificata anche nel sottoconto fattura             46
108580051128Verificare il flag Fattura Unificata anche nei codici collegati               47
108590051128Spese invio fattura errato. Troppi decimali, massimo 2                        48
108600060208Per interrogare le info.commerciali inserire il cod. potenziale               49
108610060203Il luogo non è stato immesso. Impossibile Visualizzare                        50
108620061030Immettere il codice fiscale                                                   51
108630060203La nota non è stata immessa. Impossibile Visualizzare                         52
108640051129Codice pagamento errato                                                       53
108650051129Codice ABI/CAB obbligatorio se pagamento con RB                               54
108660051129Codice ABI/CAB obbligatorio se immessa descrizione banca o nr. C/C            55
108670051130Codice ABI/CAB non valido                                                     56
108680100729Tipo pagamento errato                                                         57
108690100729Tipo pagamento non utilizzabile per clienti vari                              58
108700051130Coordinate bancarie errate. Correggere i dati immessi o contattare il cliente 59
108710051130Per l'estero l'ABI deve essere 99999 e il CAB deve essere 99999               60
108720051206Immettere i codici ABI/CAB                                                    61
108730051212Tipo Comunicazione al Mittente errato                                         62
108740051212Tipo invio comunicazione non utilizzabile                                     63
108750051212Apertura automatica giacenza errato                                           64
108760051212Apertura automatica giacenza non possibile per clienti vari                   65
108770051212Disposizione automatica giacenza non possibile per clienti vari               66
108780051213Immettere un numero stop ritiro minore di 800                                 67
108790051213Il codice richiesto è già in uso da altro utente. Riprovare più tardi         68
108800051216Impossibile confermare. Messaggi in sospeso su altre videate                  69
108810060216Tipo Comunicazione Fine Giacenza errato                                       70
108820051220Impossibile modificare cod.intestazione fattura inserito dalla SEDE           71
108830051220ATTENZIONE!!! Codice già presente in anagrafica clienti ritiro                72
108840170117Impossibile intestare fattura ad un codice in sofferenza.                     73
108850060801Chiusura automatica giacenza non possibile per clienti vari                   74
108860061113Inserire Codice Fiscale solo se diverso dalla Partita IVA. Enter per forzare. 75
108870071128Luogo non utilizzabile                                                        76
108880080306Cod.fiscale già presente su altri potenziali. Enter per forzare               77
108890080306Partita Iva già presente su altri potenziali. Enter per forzare               78
108900080422Non è possibile inserire coordinate bancarie estere se tipo pagamento italia. 79
108910080515Codice pagamento utilizzabile SOLO dalla SEDE                                 80
108920090421Non consentita fattura settimanale se cod.int.fattura con autofattura         81
108930090421No autofattura un codice collegato al cod.int.fattura ha frequenza settimanale82
108940090616Il cliente deve essere incluso nella procedura di sollecito automatico        83
108950090616Il cliente deve essere escluso dalla procedura di sollecito automatico        84
108960100729Codice pagamento NON utilizzabile                                             85
108970100730Per pagamento DANNI con Bonifico immettere la relativa e-mail                 86
108980150427Mancano i codici IBAN per i pagamenti tramite Bonifico. Enter x forzare.      87
108990150805Manca indirizzo mail per inoltro Progetto di Liquidazione. Enter x forzare.   88
109000160517Pagamento con RB non ammesso per cliente estero                               89
109010160517Pagamento con RB non ammesso su conto BancoPosta                              90
109020160914Filiale errata o non in gestione.                                             91
109030060731** TF04
109040060731F4=Abilitazioni  #
109050100524** TF22
109060100524F22=Richiesta Contatto  #
109070100507** TF05
109080100507F5=Attività  #
109090060203** TF12
109100060203F12=Ritorno  #
109110051206** TF14
109120051206F14=InfoCommerciali  #
109130051122** TF15
109140051129F15=Codici Collegati  #
109150060731** TF19
109160060526F19=Variazioni  #
109170100407** TF20
109180100407F20=Potenziali  #
109190101213** TF17
109200101213F17=Dati Fatturazione  #
109210110223** TF21
109220110223F21=Blocco Cliente  #
