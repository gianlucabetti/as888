000100990407     h*--------------------------------------------------------------------------------------------*
000200990728     h* Trascodifica tntas in TITAS
000300990407     k*--------------------------------------------------------------------------------------------*
000400990407     h DECEDIT('0,') DATEDIT(*DMY.)
000500990407     f*--------------------------------------------------------------------------------------------*
000600990407     f* Data base
000700990407     f*--------------------------------------------------------------------------------------------*
000800990621     ftita700f  o    E             DISK                                         *Metricubi  INFRA
000900990621     ftita7p0f  o    E             DISK                                         *Metricubi  INFRA
001000990625     fTITA400f  o    E             DISK                                         *Metricubi  INFRA
001100990621     ftita4p0f  o    E             DISK                                         *Metricubi  INFRA
001200990608     ftntmd01l  IF   E           K DISK                                         *Bolle
001300990625     ftitaa30c  uF A E           K DISK                                         *Bolle
001400990722     ftitas30c  IF A E           K DISK                                         *Bolle
001500990625     ffndct02l  IF   E           K DISK                                         *Bolle
001600990728     ftntas00f  IF   E             DISK    usropn                               *Bolle
001700990728     f                                     prefix(o)
001800990728     ftntas10f  IF   E             DISK    usropn                               *Bolle
001900990728     f                                     prefix(o)
002000990728     ftntasp0f  IF   E             DISK    usropn                               *Bolle
002100990608     f                                     prefix(o)
002200990608     ftnta730c  IF   E           K DISK                                         *Bolle
002300990608     f                                     prefix(o)
002400990609     fprtf198   O    F  198        PRINTER  oflind(*INOF)
002500990407     d*--------------------------------------------------------------------------------------------*
002600990407     d* Data structure
002700990407     d*--------------------------------------------------------------------------------------------*
002800990722     d wnrr            s              4b 0                                      *lettura tntas00f
002900990728     d wtibs69r        s              1                                         *lettura tntas00f
003000990722     d comopor         S                   LIKE(otaspor)                        *lettura tntas00f
003100990609     d comoinl         S                   LIKE(otasPOR)
003200990609     d comodif         S                   LIKE(otasPOR)
003300990609     d comores         S                   LIKE(otasPOR)
003400990625     d wesmd           S                   LIKE(otasll1)
003500990621     D                 DS
003600990621     D  oTASAAS                1      4  0
003700990621     D  oTASlnp                5      7  0
003800990621     D  oTASnrs                8      9  0
003900990621     D  oTASnsp               10     16  0
004000990621     D  oTASspe                1     16  0
004100990621     D                 DS
004200990621     D  savAAS                 1      4  0
004300990621     D  savlnp                 5      7  0
004400990621     D  savnrs                 8      9  0
004500990621     D  savnsp                10     16  0
004600990621     D  savspe                 1     16  0
004700990728     d
004800990728     d TIBS69DS      E DS
004900990728     d DS_cnaco      E DS                  extname(CNACO00F)
005000990728     d DS_cnind      E DS                  extname(CNIND00F)
005100990728     d DS_cnclp      E DS                  extname(CNCLP00F)
005200990728     d DS_fncls      E DS                  extname(FNCLS00F)
005300990407     c*--------------------------------------------------------------------------------------------*
005400990407     c* Main lines
005500990407     c*--------------------------------------------------------------------------------------------*
005600990722     c     *entry        plist
005700990728     c                   parm                    file              8
005800990728     c                   parm                    numero            8
005900990728     c
006000990728     c                   exsr      IMPOSTA
006100990728     c                   if        not *in02 and not *in03 and not *in04
006200990728     c                   goto      fine
006300990728     c                   endif
006400990728     c
006500990625     c                   clear                   savspe
006600990728     c                   clear                   titas000
006700990728     c                   clear                   titas010
006800990728     c                   clear                   titasp00
006900990722     c
007000990726     c                   if        numero>*zeros
007100990726     c                   movel     numero        w0080             8 0
007200990726     c                   z-add     w0080         wnrr
007300990728     c   02wnrr          chain     tntas00f                           99
007400990728     c   03wnrr          chain     tntas10f                           99
007500990728     c   04wnrr          chain     tntasp0f                           99
007600990722     c                   endif
007700990722     c
007800990722     c* se non passato numero o numero non trovato, leggo dall'inizio
007900990726     c                   if        numero<=*zeros  OR *in99
008000990728     c   02              READ      tntas00f                               99
008100990728     c   03              READ      tntas10f                               99
008200990728     c   04              READ      tntasp0f                               99
008300990722     c                   endif
008400990621     c
008500990407do  1c                   DOW       NOT *in99
008600990722     c
008700990621     c                   setoff                                       05
008800990621     c                   if        otasspe=savspe
008900990621     c                   seton                                        05
009000990621     c                   endif
009100990719     c
009200990625     c                   eval      wesmd='N'
009300990728     c   02              clear                   titas000
009400990728     c   03              clear                   titas010
009500990728     c   04              clear                   titasp00
009600990719     c* Imposto i campi delle varie, perche' quelle che rimangono vuote
009700990722     c*  devono essere riempite con gli altri valori
009800990719     c                   eval      tasva1=otasva1
009900990719     c                   eval      tasva2=otasva2
010000990719     c                   eval      tasva3=otasva3
010100990719     c                   eval      tassv1=otassv1
010200990719     c                   eval      tassv2=otassv2
010300990719     c                   eval      tassv3=otassv3
010400990719     c* imposto i campi eliminati
010500990608     c* anticipata
010600990609    2c                   if        otasant>0
010700990608     c                   eval      ta7svn='T'
010800990608     c                   eval      ta7van=otasant
010900990608     c                   EXSR      write7
011000990609    2c                   endif
011100990708     c
011200990608     c* bollo
011300990609    2c                   if        otasbol>0
011400990608     c                   eval      ta7svn='6'
011500990608     c                   eval      ta7van=otasbol
011600990608     c                   EXSR      write7
011700990609    2c                   endif
011800990728     c* iva --> solo per tas00f
011900990728    2c   02              if        otasiva>0
012000990608     c                   eval      ta7svn='8'
012100990608     c                   eval      ta7van=otasiva
012200990608     c                   EXSR      write7
012300990609    2c                   endif
012400990608     c* porto
012500990609    2c                   if        otaspor>99999999
012600990609     c* stampo intanto
012700990609     c                   except    bolla
012800990609     c                   eval      taspor=99999000
012900990609     c                   eval      comopor=otaspor-99999000
013000990609     c                   eval      comoinl=otasinl+comopor
013100990609
013200990609    3c                   if        comoinl>99999999
013300990609     c                   eval      ta7svn='2'
013400990609     c                   eval      ta7van=99999000
013500990609     c                   EXSR      write7
013600990609     c                   eval      comores=comoinl-99999000
013700990609     c                   eval      comodif=otasdif+comores
013800990609    4c                   if        comodif>99999999
013900990609     c* aggiorno dir.fisso com'e'
014000990609    5c                   if        otasdif>0
014100990609     c                   eval      ta7svn='H'
014200990609     c                   eval      ta7van=otasdif
014300990609     c                   EXSR      write7
014400990609    5c                   endif
014500990609     c* stampo
014600990609     c                   except    bollanoag
014700990609   x4c                   else
014800990609     c                   eval      ta7svn='H'
014900990609     c                   eval      ta7van=comodif
015000990609     c                   EXSR      write7
015100990609    4c                   endif
015200990609   x3c                   else
015300990609     c                   eval      ta7svn='2'
015400990609     c                   eval      ta7van=comoinl
015500990609     c                   EXSR      write7
015600990609     c* dif.fisso
015700990609    4c                   if        otasdif>0
015800990609     c                   eval      ta7svn='H'
015900990609     c                   eval      ta7van=otasdif
016000990609     c                   EXSR      write7
016100990609    4c                   endif
016200990609     c
016300990609    3c                   endif
016400990609     c
016500990609   x2c                   else
016600990609     c                   eval      taspor=otaspor
016700990609     c* inoltro
016800990609    3c                   if        otasinl>0
016900990609     c                   eval      ta7svn='2'
017000990609     c                   eval      ta7van=otasinl
017100990609     c                   EXSR      write7
017200990609    3c                   endif
017300990609     c* dir.fisso
017400990609    3c                   if        otasdif>0
017500990609     c                   eval      ta7svn='H'
017600990609     c                   eval      ta7van=otasdif
017700990609     c                   EXSR      write7
017800990609    3c                   endif
017900990609    2c                   endif
018000990608     c                   eval      tasdft=otasdft
018100990608     c                   eval      tasnft=otasnft
018200990608     c                   eval      tasfiv=otasfiv
018300990608     c                   eval      tasrmn=otasrmn
018400990608     c                   eval      tasccm=otasccm
018500990608     c                   eval      tasctr=otasctr
018600990608     c                   eval      tascts=otascts
018700990608     c                   eval      tasfin=otasfin
018800990608     c                   eval      tastsp=otastsp
018900990608     c                   eval      tasftc=otasftc
019000990608     c                   eval      tastc2=otastc2
019100990608     c                   eval      tascca=otascca
019200990608     c                   eval      tasfap=otasfap
019300990608     c                   eval      tasmct=otasmct
019400990608     c                   eval      tasaas=otasaas
019500990608     c                   eval      taslnp=otaslnp
019600990608     c                   eval      tasnrs=otasnrs
019700990608     c                   eval      tasnsp=otasnsp
019800990608     c                   eval      tastbl=otastbl
019900990609     c                   eval      tasmgs=otasmgs
020000990609     c                   eval      tasksc=otasksc
020100990609     c                   eval      taslna=otaslna
020200990608     c                   eval      tasncl=otasncl
020300990608     c                   eval      taspkb=otaspkb
020400990608     c                   eval      taspkf=otaspkb
020500990608     c                   eval      tasfpf='R'
020600990608     c                   eval      tasfpb='R'
020700990608     c                   eval      taspvl=otaspvl
020800990608     c                   eval      tasvlc=otasvlc
020900990608     c                   eval      tasncr=otasncr
021000990608     c                   eval      tasvlf=otasvlf
021100990608     c                   eval      tasfvf=otasfvf
021200990608     c                   eval      tasfvb=otasfvb
021300990608     c                   eval      tasvlb=otasvlb
021400990608     c                   eval      tasqft=otasqft
021500990608     c                   eval      tasftm=otasftm
021600990608     c                   eval      tasflb=otasflb
021700990608     c                   eval      tasflp=otasflp
021800990608     c                   eval      tasfl2=otasfl2
021900990608     c                   eval      tastfp=otastfp
022000990608     c                   eval      tastfa=otastfa
022100990608     c                   eval      tasdim=otasdim
022200990608     c                   eval      taspdr=otaspdr
022300990608     c                   eval      tasdrt=otasdrt
022400990608     c                   eval      tasnrt=otasnrt
022500990608     c                   eval      tasfpp=otasfpp
022600990608     c                   eval      tasstr=otasstr
022700990608     c                   eval      tasdcm=otasdcm
022800990608     c                   eval      tashmc=otashmc
022900990608     c                   eval      tasndc=otasndc
023000990608     c                   eval      tasstp=otasstp
023100990608     c                   eval      tasznc=otasznc
023200990608     c                   eval      tasffd=otasffd
023300990608     c                   eval      tasfcm=otasfcm
023400990608     c                   eval      tasfgc=otasfgc
023500990608     c                   eval      tassop=otassop
023600990608     c                   eval      tasfbr=otasfbr
023700990608     c                   eval      tasfa2=otasfa2
023800990608     c                   eval      tasfca=otasfca
023900990608     c                   eval      tasfa1=otasfa1
024000990608     c                   eval      tasfdn=otasfdn
024100990608     c                   eval      tasnzm=otasnzm
024200990608     c                   eval      tascam=otascam
024300990608     c                   eval      tasvas=otasvas
024400990608     c                   eval      tasias=otasias
024500990608     c                   eval      tasnpi=otasnpi
024600990608     c                   eval      tasnci=otasnci
024700990608     c                   eval      tasnrc=otasnrc
024800990608     c                   eval      tasgga=otasgga
024900990608     c                   eval      tasgma=otasgma
025000990608     c                   eval      tasgva=otasgva
025100990608     c                   eval      tasll1=otasll1
025200990608     c                   eval      tasll2=otasll2
025300990817     c                   eval      tasscl=otasscl
025400990621     c                   if        *in02
025500990608     c                   eval      tasfei=otasfei
025600990608     c                   eval      tasfts=otasfts
025700990608     c                   eval      tasfic=otasfic
025800990621     c                   endif
025900990621     c
026000990621     c* tas10 e tasp
026100990621     c                   if        *in03 or *in04
026200990621     c* sempre contabilizzate
026300990621     c                   eval      tasfic='1'
026400990621     c* se c'e' la data fattura metto anche IL FLAG DI TASSAZIONE
026500990621     c                   if        otasdft>0
026600990621     c                   eval      tasfTS='1'
026700990621     c                   endif
026800990621     c                   endif
026900990608     c* data part ultimo collo
027000990608     c                   eval      tasduc=otasdfv
027100990621     c* totale imponibile per 1 bolla testo il flag per tas10 e tasp
027200990621     c*  se ci sono degli importi lo metto
027300990621    2c                   if        otasfti='1'  OR *IN03 or *in04
027400990621     c
027500990608     C                   EVAL      tasimv = (otaspor+otasinl +otasdif +otasva1
027600990608     c                                    + otasva2 +otasva3)
027700990609    2c                   endif
027800990608     c* se le varie sono già tutte valorizzate vado a recuperarne delle altre
027900990608     c* nel file combinato TNTA7
028000990608
028100990720    2c                   if        otassv3<>' '
028200990608     c     ktas          setll     tnta730c
028300990608     c     ktas          reade     tnta730c                               30
028400990609    3c                   dow       not *in30
028500990729    4c                   if        otasfti='1' or *in03 or *in04
028600990609     c                   add       ota7van       tasimv
028700990609    4c                   endif
028800990609     c
028900990609     c* le scrivo nel nuovo file
029000990609     c                   eval      ta7aas=ota7aas
029100990609     c                   eval      ta7lnp=ota7lnp
029200990609     c                   eval      ta7nrs=ota7nrs
029300990609     c                   eval      ta7nsp=ota7nsp
029400990609     c                   eval      ta7tbl=ota7tbl
029500990609     c                   eval      ta7svn=ota7svn
029600990609     c                   eval      ta7van=ota7van
029700990621     c                   if        *in04
029800990728     c                   write     tita7p00                             95
029900990621     c                   else
030000990728     c                   write     tita7000                             95
030100990621     c                   endif
030200990609     c
030300990609     c     ktas          reade     tnta730c                               30
030400990609    3c                   enddo
030500990609    2c                   endif
030600990608     c* ha c.a.
030700990608     c     kbol          setll     fndct02l                               30
030800990609    2c                   if        *in30
030900990608     c                   eval      tasfda='S'
031000990621     c     kbol          chain     tntmd01l                           30
031100990625     c  n30              eval      wesmd='S'
031200990609    2c                   endif
031300990608     c* rif.mittente
031400990625    2c  N05              if        otasrma<>*blanks
031500990608     c                   eval      ta4trc='A'
031600990608     c                   movel(p)  otasrma       ta4not
031700990608     c                   exsr      write4
031800990609    2c                   endif
031900990608     c
032000990609     c                   movel     otastbl       flgtb1            1
032100990608     c                   move      otasksc       w0040             4 0
032200990608     c* FRANCO
032300990609    2c                   if        flgtb1='F'
032400990608     c* mittente
032500990609    3c                   if        w0040<>8888
032600990608     c                   eval      tasccm=otasksc
032700990609   x3c                   else
032800990608     c* scrivo anagrafica
032900990625     c                   z-add     8888          tasccm
033000990625     c                   movel     otaslnp       tasccm
033100990625     c* se è bolla doppia aggiorno anagrafica mittente
033200990625     c                   if        *in05
033300990625     c     kbol          chain     titaa30c                           33
033400990625     c                   else
033500990625     c                   seton                                        33
033600990625     c                   endif
033700990625     c   33              clear                   titaa000
033800990722     c   33              clear                   titaap00
033900990608     c                   eval      taatrc='M'
034000990621     c                   eval      taaaas=otasaas
034100990621     c                   eval      taalnp=otaslnp
034200990621     c                   eval      taanrs=otasnrs
034300990621     c                   eval      taansp=otasnsp
034400990621     c                   eval      taacap=otascam
034500990621     c                   eval      taanaz=otasnzm
034600990621     c
034700990621     c                   if        *in02
034800990608     c                   eval      taarsc=otasrsd
034900990608     c                   eval      taaind=otasind
035000990608     c                   eval      taaloc=otaslod
035100990608     c                   eval      taaprv=otasprv
035200990608     c                   eval      taacpi=otaspiv
035300990621     c                   else
035400990625     c* uso tmd se c'e'
035500990625     c                   if        wesmd='S'
035600990621     c                   eval      taarsc=tmdrsm
035700990621     c                   eval      taaind=tmdinm
035800990621     c                   eval      taaloc=tmdlom
035900990621     c                   eval      taaprv=tmdprm
036000990621     c                   endif
036100990621     c                   endif
036200990608     c
036300990621     c                   if        *in04
036400990728     c   33              write     titaap00                             95
036500990625     c  n33              update    titaap00
036600990621     c                   else
036700990728     c   33              write     titaa000                             95
036800990625     c  n33              update    titaa000
036900990609    3c                   endif
037000990621    3c                   endif
037100990609     c
037200990621     c* scrivo il DESTINATARIO: se c'e' tmd lo prendo da li' altrimenti
037300990608     c*  quello che c'e' in tas
037400990621     c                   eval      taScaD=otascad
037500990621     c                   eval      taSnzd=otasnzd
037600990621     c
037700990621     c* se bolla doppia il destinatario lo prendo dalla prima scritta
037800990621     c*  che e' per forza l'assegnato
037900990729     c**                 if        *in05
038000990729     c**                 eval      tasdest=savdest
038100990729     c**                 else
038200990621     c
038300990625    3c                   if        wesmd='S'
038400990621     c                   eval      tasrsd=tmdrsd
038500990621     c                   eval      tasind=tmdind
038600990621     c                   eval      taslod=tmdlod
038700990621     c                   eval      tasprd=tmdprd
038800990608     c                   else
038900990621     c* c'e' solo per tntas000
039000990621     c                   if        *in02
039100990621     c                   eval      tasrsd=otasrsc
039200990609    3c                   endif
039300990621    3c                   endif
039400990621     c
039500990729    3c**                 endif
039600990621     c
039700990609    2c                   endif
039800990609     c* ASSEGNATO
039900990609    2c                   if        flgtb1='A'
040000990609     c* scrivo sicuramente il destinatario
040100990621     c                   eval      tasnzd=otasnzd
040200990621     c                   eval      tascad=otascad
040300990621     c                   if        *in02
040400990621     c                   eval      tasrsd=otasrsd
040500990621     c                   eval      tasind=otasind
040600990621     c                   eval      taslod=otaslod
040700990621     c                   eval      tasprd=otasprv
040800990621     c                   eval      tascpd=otaspiv
040900990621     c                   endif
041000990609     c
041100990609     c* mittente
041200990609    3c                   if        otasccm=0
041300990609     c
041400990625     c                   z-add     8888          tasccm
041500990625     c                   movel     otaslnp       tasccm
041600990609     c                   clear                   titaa000
041700990722     c                   clear                   titaap00
041800990609     c                   seton                                        30
041900990609     c                   eval      taaaas=otasaas
042000990609     c                   eval      taalnp=otaslnp
042100990609     c                   eval      taanrs=otasnrs
042200990609     c                   eval      taansp=otasnsp
042300990609     c                   eval      taatrc='M'
042400990609     c                   eval      taacap=otascam
042500990609     c                   eval      taanaz=otasnzm
042600990609     c
042700990625    4c                   if        wesmd='S'
042800990609     c                   eval      taarsc=tmdrsm
042900990609     c                   eval      taaind=tmdinm
043000990609     c                   eval      taaloc=tmdlom
043100990609     c                   eval      taaprv=tmdprm
043200990609     c                   else
043300990621     c* solo in tntas000 c'e' la ragione sociale
043400990621     c                   if        *in02
043500990609     c                   eval      taarsc=otasrsc
043600990609    4c                   endif
043700990621    4c                   endif
043800990621     c
043900990621     c                   if        *in04
044000990728     c                   write     titaap00                             95
044100990621     c                   else
044200990728     c                   write     titaa000                             95
044300990609    3c                   endif
044400990621     c
044500990621    3c                   endif
044600990609    2c                   endif
044700990728     c
044800990728     c* Se non ho trovato il destinatario e si tratta di bolla assegnatoSF
044900990728     c*  leggo cnaco
045000990728     c                   if        flgtb1='A' and tasrsd=*blanks  and
045100990728     c                             w0040 <>9999
045200990728     c                   eval      wtibs69r='S'
045300990728     C                   clear                   TIBS69DS
045400990728     c                   z-add     tasksc        I69kac
045500990728     c                   z-add     tasksc        I69kin
045600990728      *
045700990728     C                   CALL      'TIBS69R'
045800990728     C                   PARM                    tibs69DS
045900990728     C                   PARM                    DS_cnaco
046000990728     C                   PARM                    DS_cnind
046100990728     C                   PARM                    DS_cnclp
046200990728     C                   PARM                    DS_fncls
046300990728     c                   if        o69err=' '
046400990728     c                   movel     acorag        tasrsd
046500990728     c                   movel     indvia        tasind
046600990728     c                   movel     indcit        taslod
046700990728     c                   movel     indprv        tasprd
046800990728     c                   movel     indiva        tascpd
046900990728     c                   endif
047000990728     c                   endif
047100990728     c
047200990621     c                   if        *in02
047300990728     c                   write     titas000                             95
047400990621     c                   endif
047500990621     c                   if        *in03
047600990728     c                   write     titas010                             95
047700990621     c                   endif
047800990621     c                   if        *in04
047900990728     c                   write     titasP00                             95
048000990621     c                   endif
048100990608     c
048200990621     c                   eval      savspe=otasspe
048300990729     c*********          eval      savdest=tasdest
048400990722     c
048500990728     c   02              READ      tntas000                               99
048600990728     c   03              READ      tntas010                               99
048700990728     c   04              READ      tntasp00                               99
048800990407e   1c                   ENDDO
048900990728     C                   IF        Wtibs69r  <>  *BLANKS
049000990728     C                   EVAL      I69TLA  = 'C'
049100990728     C                   CALL      'TIBS69R'
049200990728     C                   PARM                    TIBS69DS
049300990728     C                   ENDIF
049400990728     c
049500990728     c     fine          tag
049600990407     c*
049700990407     c                   SETON                                            lr
049800990407     c*--------------------------------------------------------------------------------------------*
049900990407     C* *inzsr - operazioni iniziali
050000990407     c*--------------------------------------------------------------------------------------------*
050100990722     c     *inzsr        begsr
050200990407     c* chiavi di lettura
050300990608     c     ktas          KLIST                                                  *tntas30c
050400990608     c                   KFLD                    otasaas                         -anno
050500990608     c                   KFLD                    otaslnp                         -linea partenza
050600990608     c                   KFLD                    otasnrs                         -serie
050700990608     c                   KFLD                    otasnsp                         -spedizione
050800990608     c                   KFLD                    otastbl                         -tipo bolla
050900990608     c     kbol          KLIST                                                  *tntas30c
051000990608     c                   KFLD                    otasaas                         -anno
051100990608     c                   KFLD                    otaslnp                         -linea partenza
051200990608     c                   KFLD                    otasnrs                         -serie
051300990608     c                   KFLD                    otasnsp                         -spedizione
051400990407     c*
051500990407     c                   ENDSR
051600990728     c*--------------------------------------------------------------------------------------------*
051700990728     C* impostazioni trascodifica file
051800990728     c*--------------------------------------------------------------------------------------------*
051900990728     c     IMPOSTA       begsr
052000990728     c                   setoff                                       020304
052100990728     c                   if        file='TNTAS00F' or file='tntas00f'
052200990728     C                   SETON                                        02
052300990728     c                   open      tntas00f
052400990728     c                   endif
052500990728     c                   if        file='TNTAS10F' or file='tntas10f'
052600990728     C                   SETON                                        03
052700990728     c                   open      tntas10f
052800990728     c                   endif
052900990728     c                   if        file='TNTASP0F' or file='tntasp0f'
053000990728     C                   SETON                                        04
053100990728     c                   open      tntasp0f
053200990728     c                   endif
053300990728     c
053400990728     c                   endsr
053500990608     c*--------------------------------------------------------------------------------------------*
053600990608     C* *scrivo le varie in tita7
053700990608     c*--------------------------------------------------------------------------------------------*
053800990608     C     write7        BEGSR
053900990708     c                   select
054000990719     c                   when      tassv1=' '
054100990708     c                   eval      tassv1=ta7svn
054200990708     c                   eval      tasva1=ta7van
054300990708     c
054400990719     c                   when      tassv2=' '
054500990708     c                   eval      tassv2=ta7svn
054600990708     c                   eval      tasva2=ta7van
054700990708
054800990719     c                   when      tassv3=' '
054900990708     c                   eval      tassv3=ta7svn
055000990708     c                   eval      tasva3=ta7van
055100990708
055200990708     c                   other
055300990708     c                   eval      ta7aas=otasaas
055400990608     c                   eval      ta7lnp=otaslnp
055500990608     c                   eval      ta7nrs=otasnrs
055600990608     c                   eval      ta7nsp=otasnsp
055700990608     c                   eval      ta7tbl=otastbl
055800990621     c                   if        *in04
055900990728     c                   write     tita7p00                             95
056000990621     c                   else
056100990728     c                   write     tita7000                             95
056200990621     c                   endif
056300990708     c                   endsl
056400990608     c                   ENDSR
056500990608     c*--------------------------------------------------------------------------------------------*
056600990608     C* *scrivo il riferimenti mitt.alfabetico in tita4
056700990608     c*--------------------------------------------------------------------------------------------*
056800990608     C     write4        BEGSR
056900990608     c                   eval      ta4aas=otasaas
057000990608     c                   eval      ta4lnp=otaslnp
057100990608     c                   eval      ta4nrs=otasnrs
057200990608     c                   eval      ta4nsp=otasnsp
057300990728     c                   if        *in04
057400990728     c                   write     tita4p00                             95
057500990728     c                   else
057600990728     c                   write     tita4000                             95
057700990621     c                   endif
057800990608     c                   ENDSR
057900990407     o*--------------------------------------------------------------------------------------------*
058000990609     Oprtf198   E            BOLLA       1
058100990609     o                                              'Bolla con porto>99.999.999'
058200990609     o                       otasaas       Z  +   1
058300990609     o                       otaslnp       Z  +   1
058400990609     o                       otasnrs       Z  +   1
058500990609     o                       otasnsp       Z  +   1
058600990609     o                       otastbl          +   1
058700990609     o                       otaspor       2  +   1
058800990609     o                       otasinl       2  +   1
058900990609     o                       otasdif       2  +   1
059000990609     o                       otassv1          +   1
059100990609     o                       otasva1       2  +   1
059200990609     o                       otassv2          +   1
059300990609     o                       otasva2       2  +   1
059400990609     o                       otassv3          +   1
059500990609     o                       otasva3       2  +   1
059600990609     O          E            BOLLAnoag   1
059700990609     o                                              '                          '
059800990609     o                       otasaas       Z  +   1
059900990609     o                       otaslnp       Z  +   1
060000990609     o                       otasnrs       Z  +   1
060100990609     o                       otasnsp       Z  +   1
060200990609     o                       otastbl          +   1
060300990609     o                       otaspor       2  +   1
060400990609     o                       otasinl       2  +   1
060500990609     o                       otasdif       2  +   1
060600990609     o                       otassv1          +   1
060700990609     o                       otasva1       2  +   1
060800990609     o                       otassv2          +   1
060900990609     o                       otasva2       2  +   1
061000990609     o                       otassv3          +   1
061100990609     o                       otasva3       2  +   1
061200990609     o                                        +   1 'Importo non aggiornato:'
061300990609     o                       comores       2  +   1
