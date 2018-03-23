000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060125      *****************************************************************
000300161110      * Il pgm estrae i record di log degli esiti palmare e gli orari
000400060125      *
000500060125      *****************************************************************
001000180302     ftitas38c  if   e           k disk
001001180302     f                                     ignore(titas000)
001002180302     f                                     ignore(titas010)
001003180306     FTITAA30C  iF   E           K DISK
001004180306     FTITA430C  iF   E           K DISK
001005180306     FFIAR531C  iF   E           K DISK
001006180306     Ftabel00f  iF   E           K DISK
001007180306     Ffiapd01l  IF   E           K DISK
001008180306     Ftilad01l  IF   E           K DISK
001009180307     Ftncsb03l  IF   E           K DISK
001100180302     Fbitasr0f  o    E             DISK
001200030429     F*---------------------------------------------------------------------------------
001300000607     D kpjba         e ds
001301180302     dparam            ds
001302180302     d datap                          8  0
001303180302     d datap2                         8  0
001304180307     d com04                          4  0
002200161110     d tmp             s             17
002201180306     d trcapd          s                   like(apdtip) inz('A')
002202180302      *
002203180302     d                 ds
002204180302     d tasaas                  1      4  0
002205180302     d tasmgs                  5      8  0
002206180302     d tasdsp                  1      8  0
002207180302     d tasanme                 1      6  0
002208180306      * RCD FAT FIAR5
002209180306     d dAR5fat       e ds
002210180306     d DTA4A         e ds
002211180307     d DTA4Q         e ds
002212180307     d Dstb          e ds
002213180306     d DTAsflo       e ds
002214180306     D*-------------------
002215180306     d TIBS69ds      e ds                  inz
002216180306     d DS_cnaco      e ds                  inz  extname(CNACO00F)
002217180306     d DS_cnind      e ds                  inz  extname(CNIND00F)
002218180306     d DS_cnclp      e ds                  inz  extname(CNCLP00F)
002219180306     d DS_fncls      e ds                  inz  extname(FNCLS00F)
002300020205     c**********************************************************************
002400180302      * senza data una-tantum copia tutto il TITASP da poortare in piattaforma BI
002401180307     c                   clear                   bitasr00
002402180302     c                   if        datap = 0
002500180302     c     *loval        setll     titas38c
002600161026     c                   do        *hival
002700180302     c                   read      titas38c
002800180302     c                   if        %eof(titas38c)
002900161026     c                   leave
003000161026     c                   endif
003001180306     c                   exsr      trasferisci
003003180302     c                   enddo
003004180302      *
003006180302     c                   else
003007180302      *
003008180302     c                   eval      tasdsp = datap
003010180302      * CON data Copia Il TITASP da portare in piattaforma BI NEL RANGE DI DATE (DATA SPEDIZIONE)
003011180302     c     ktas          setll     titas38c
003012180302     c                   do        *hival
003013180302     c                   read      titas38c
003014180302     c                   if        %eof(titas38c) OR
003015180302     C                             TASDSP > DATAP2
003016180302     c                   leave
003017180302     c                   endif
003018180306     c                   exsr      trasferisci
003020180302     c                   enddo
003500180302     c                   endif
003501180302      *
003600161110     c                   seton                                        lr
003601180306     C*------------------------------------------------------------------------*
003602180306     C*  trasferisci SCRITTURA FILE STORICO TITASS
003603180306     C*------------------------------------------------------------------------*
003604180306     C     trasferisci   BEGSR
003606180306      * verifico se bolla da trasferire se di recupero ignoro
003607180307     c                   movel(p)  tastbl        tblkey
003608180306     c     ktab          chain     tabel00f
003609180306     c                   if        not %found(tabel00f)
003610180306     c                   leavesr
003611180306     c                   end
003612180306     c                   movel(p)  tbluni        dstb
003613180306     c                   if        §TBRBL ='R'
003614180306     c                   leavesr
003615180306     c                   end
003616180306      * cerco dati per completamento campi informazioni bolla
003617180306     c                   exsr      setta4
003618180306     c                   exsr      settaa
003619180306     c                   exsr      recana
003620180306     c                   exsr      reciva
003621180306      *
003622180306     c                   movel     tasflo        dtasflo
003623180306     c* memorizzo directory laguna ter sapere dove è stata archiviata la spe
003624180306     c                   clear                   tasdtfp
003625180306     c                   if        §floiml <> ' '
003626180306     c                   move      tasaas        ladaas
003627180306     c                   move      taslnp        ladlnp
003628180306     c                   move      tasnrs        ladnrs
003629180306     c                   move      tasnsp        ladnsp
003630180307     c     keylad        chain     tilad01l
003631180306     c                   If        %found(tilad01l)
003632180306     c                   movel     laddir        tasdtfp
003633180306     c                   end
003634180306     c                   end
003635180306      * Decodifica padroncino
003636180306     c                   If        taspdc <> *zeros
003637180306     c     Keyapd        chain     fiapd01l
003638180306     c                   If        %found(fiapd01l)
003639180306     c                   eval      taspdd = apdrsc
003640180306     c                   endif
003641180306     c                   endif
003642180306
003643180306      * recupero c/assegno
003644180306     c                   if        tasfca = 'S'
003645180307     C     KEYtas        CHAIN     TNCSB03L
003646180307     C                   IF        %found(tncsb03l)                             --- 2 -->
003647180306     c                   z-add     csbcas        Tascas
003648180306     c                   movel     csbvca        tasvca
003649180307     c                   endif
003650180306     c                   endif
003651180306
003652180306     c                   move      tasksc        com04
003653180306      * PARTITA IVA / CODICE FISCALE
003654180306     c                   If        %subst(tastbl:1:1) = 'A'  and
003655180306     c                             tascpd <> *blanks
003656180306     c                   eval      tasiva  = tascpd
003657180306     c                   endif
003658180306      * se partita iva non valorizzata
003659180306     c                   If        tasiva = *blanks  or tascdf = *blanks
003660180306      * codificato franco o assegnato
003661180306     c                   if        com04  <>  9999  and com04  <>  8888
003662180306     c                   exsr      reciva
003663180306     c                   if        o69err <> *on
003664180306     c                   if        tascdf = *blanks
003665180306     c                   eval      tasiva = indiva
003666180306     c                   endif
003667180306      * se devo cercare anche il codife fiscale
003668180306     c                   if        tascdf = *blanks
003669180306     c                   eval      tascdf = indcdf
003670180306     c                   endif
003671180306
003672180306     c                   endif
003673180306     c                   endif
003674180306     c                   endif
003675180306      * valorizzo il flag che mi identifica le spedizioni senza segnacollo
003676180306     c                   If        Tasfns <> 'S' and tasncd = *zeros
003677180306     c                   eval      tassss = 'S'
003678180306     c                   endif
003679180306
003680180306      * verifico se la ds dar5fat è valorizzata
003681180307     c                   clear                   dar5fat
003682180307     C     KEYAR5        CHAIN     FIAR531C
003684180307      *se trovo rekord FAT mi salvo i dati
003685180307     c                   if        %found(fiar531c)
003686180307     c                   movel     ar5uni        dar5fat
003687180307     c                   endif
003688180306     c                   If        §ar5pktas > 0
003689180306     c                   eval      taspkf = §ar5pktas
003690180306     c                   endif
003691180306
003692180306     c                   If        §ar5vltas > 0
003693180306     c                   eval      tasvlf = §ar5vltas
003694180306     c                   endif
003695180306
003696180307     c                   write     bitasr00                             99
003697180306
003698180307     c                   clear                   bitasr00
003699180306     C                   ENDSR
003700180306     C*
003701180306     C*------------------------------------------------------------------------*
003702180306     C*  SETTA4 - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITA4 - RIFERIMENTI
003703180306     C*------------------------------------------------------------------------*
003704180306     C     SETTA4        BEGSR
003705180306     C*
003706180306     C* POSIZIONAMENTO E PRIMA LETTURA
003707180307     C     KEYTAs        setll     TITA430C
003708180306     c                   do        *hival
003709180307     C     KEYTAs        reade     TITA430C
003710180306     C                   IF        %eof(tita430c)
003711180306     C                   leave
003712180306      * recupero riferimento mittente alfanumerico e natura merce
003713180306     c                   endif
003714180306     c                   exsr      recta4
003715180306     C                   ENDdo
003716180306     C*
003717180306     C                   ENDSR
003718180306     C*------------------------------------------------------------------------*
003719180306     C*  RECTA4 - RECUPERO RIFERIMENTO MITTENTE ALFANUMERICO / CODICE FISCALE
003720180306     C*------------------------------------------------------------------------*
003721180306     C     RECTA4        BEGSR
003722180306      * riferimento mittente alfanumerico
003723180306     c                   IF        ta4trc = 'A'
003724180306     c                   movel     ta4not        dta4a
003725180306     c                   movel     §ta4aRMA      Tasrma
003726180306     c                   movel     §ta4anas      Tasnas
003727180306     c                   endif
003728180306      * CODICE FISCALE
003729180306     c                   IF        ta4trc = 'Q'
003730180306     c                   movel     ta4not        dta4q
003731180306      * VERIFICO SE FRANCO O ASSEGNATO
003732180306     c                   If        %subst(tastbl:1:1) = 'F'
003733180306     c                   movel     §TA4QCFM      Tascdf
003734180306     c                   endif
003735180306     c                   If        %subst(tastbl:1:1) = 'A'
003736180306     c                   movel     §TA4QCFD      Tascdf
003737180306     c                   endif
003738180306     c                   endif
003739180306
003740180306     c                   endsr
003741180306     C*------------------------------------------------------------------------*
003742180306     C*  SETTAA - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAA - ANAGRAFICHE
003743180306     C*------------------------------------------------------------------------*
003744180306     C     SETTAA        BEGSR
003745180306     C*
003749180306     C* POSIZIONAMENTO E PRIMA LETTURA
003754180306     C     KEYTAA        setll     TITAA30C
003755180306     c                   do        *hival
003756180306     C     KEYTAA        reade     TITAA30C
003757180306     C                   IF        %eof(titaa30c)
003758180306     C                   leave
003759180306      * recupero dati anagrafici del mittente
003760180306     c                   endif
003761180306     c                   exsr      rectaa
003762180306     C                   ENDdo
003763180306     C*
003764180306     C                   ENDSR
003784180306     C*------------------------------------------------------------------------*
003785180306     C*  RECTAA - RECUPERO DATI ANAGRAFICI MITTENTE
003786180306     C*------------------------------------------------------------------------*
003787180306     C     RECTAA        BEGSR
003788180306      * dati anagrafici mittente
003789180306     c                   IF        taatrc = 'M'
003790180306     c                   eval      tasrsm = taarsc
003791180306     c                   eval      tasinm = taaind
003792180306     c                   eval      tascam = taacap
003793180306     c                   eval      taslom = taaloc
003794180306     c                   eval      tasprm = taaprv
003795180306     c                   eval      tasnzm = taanaz
003796180306      * iva  per i franchi non coidficati
003797180306     c                   if        %subst(tastbl:1:1) = 'F'
003798180306     c                   eval      tasiva = taacpi
003799180306     c                   endif
003800180306
003801180306     c                   endif
003802180306
003803180306     c                   endsr
003804180306     C*------------------------------------------------------------------------*
003805180306     C*  RECANA - RECUPERO DA ANAGRAFICO CLIENTE
003806180306     C*------------------------------------------------------------------------*
003807180306     C     RECANA        BEGSR
003808180306     C                   clear                   ds_CNACO
003809180306     C                   clear                   ds_CNIND
003810180306     C                   clear                   ds_CNCLP
003811180306     C                   clear                   ds_FNCLS
003812180306     c                   z-add     TASccm        I69kac
003813180306     c                   z-add     TASccm        I69kin
003814180306     c                   call      'TIBS69R'
003815180306     c                   parm                    TIBS69ds
003816180306     c                   parm                    ds_CNACO
003817180306     c                   parm                    ds_CNIND
003818180306     c                   parm                    ds_CNCLP
003819180306     c                   parm                    ds_FNCLS
003820180306if  3c                   if        O69err <> *on
003821180306     c                   movel     ACOrag        TASrsm
003822180306     c                   movel     INDvia        TASinm
003823180306     c                   movel     INDcap        TAScam
003824180306     c                   movel     INDcit        TASlom
003825180306     c                   movel     INDprv        TASprm
003826180306     c                   movel     INDsta        TASnzm
003827180306e   3c                   endif
003828180306
003829180306     c                   endsr
003830180306     C*------------------------------------------------------------------------*
003831180306     C*  RECIVA - RECUPERO DA ANAGRAFICO CLIENTE IVA E/O CODICE FISCALE
003832180306     C*------------------------------------------------------------------------*
003833180306     C     RECIVA        BEGSR
003834180306     C                   clear                   ds_CNACO
003835180306     C                   clear                   ds_CNIND
003836180306     C                   clear                   ds_CNCLP
003837180306     C                   clear                   ds_FNCLS
003838180306     c                   z-add     TASKSC        I69kin
003839180306     c                   call      'TIBS69R'
003840180306     c                   parm                    TIBS69ds
003841180306     c                   parm                    ds_CNACO
003842180306     c                   parm                    ds_CNIND
003843180306     c                   parm                    ds_CNCLP
003844180306     c                   parm                    ds_FNCLS
003845180306
003846180306     c                   endsr
022300020205     C**-------------------------------------------------------------**
022400000607     c     *inzsr        begsr
022500060125     C**-------------------------------------------------------------**
022701180302     c     *entry        plist
022702180302     c                   parm                    kpjba
022703180302     c                   if        kpjbu = *blank
022704180302     c                   clear                   param
022705180302     c                   else
022706180302     c                   movel     kpjbu         param
022707180302     c                   endif
023800161025      *
023900180302     c     ktas          klist
024000180302     c                   kFLD                    tasaas
024100180302     c                   kFLD                    tasmgs
024101180306     c     ktab          klist
024102180306     c                   kFLD                    TBLKUT
024103180306     c                   kFLD                    TBLCOD
024104180306     c                   kFLD                    TBLKEY
024105180307     c                   z-add     1             TBLKUT
024106180307     c                   move      'TB'          TBLCOD
024107180306     C* CHIAVE LETTURA Fiapd01l
024108180306     C     KEYapd        KLIST
024109180306     C                   KFLD                    trcapd
024110180306     C                   KFLD                    taspdc
024111180306     C* CHIAVE LETTURA
024112180306     C     KEYTAs        KLIST
024113180306     C                   KFLD                    TAsAAS
024114180306     C                   KFLD                    TAsLNP
024115180306     C                   KFLD                    TAsNRS
024116180306     C                   KFLD                    TAsNSP
024117180306     C*
024118180307     C* CHIAVE LETTURA
024119180307     C     KEYlad        KLIST
024120180307     C                   KFLD                    ladaas
024121180307     C                   KFLD                    ladlnp
024122180307     C                   KFLD                    ladnrs
024123180307     C                   KFLD                    ladnsp
024124180307     C*
024125180307     C* CHIAVE LETTURA
024126180307     C     KEYTAa        KLIST
024127180307     C                   KFLD                    TAsAAS
024128180307     C                   KFLD                    TAsLNP
024129180307     C                   KFLD                    TAsNRS
024130180307     C                   KFLD                    TAsNSP
024131180307     C                   KFLD                    atrc
024132180307     C                   move      'M'           atrc              1
024133180307     C* CHIAVE LETTURA
024134180307     C     KEYar5        KLIST
024135180307     C                   KFLD                    TAsAAS
024136180307     C                   KFLD                    TAsLNP
024137180307     C                   KFLD                    TAsNRS
024138180307     C                   KFLD                    TAsNSP
024139180307     C                   KFLD                    trcar5
024140180307     C                   move      'FAT'         trcar5            3
026900020402     c                   endsr
