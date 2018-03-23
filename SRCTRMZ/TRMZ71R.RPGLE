000100060105     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300031103     Ftrmz71d   CF   E             WORKSTN
000400030630     Faitra02l  if   e           k disk
000500120329     Faitra04l  if   e           k disk    rename(aitra000:aitra4)
000600031103     Fazorg01l  if   e           k disk
000700060920     Fazorg02l  if   e           k disk    rename(azorg:azorg2)
000800060920     ftabel00f  if   e           k disk
000900031028     Fprtf198   O    F  198        PRINTER prtctl(prtds)
001000020304     dkpjba          e ds
001001170810     D TIBS02DS      E DS
001002170810     D TIBS34DS      E DS                                                       *Profili utente
001003170810     D DDATIUTE      E DS                                                       *Dati utente
001004170810     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
001005170810     D dute01        E DS                                                       *Utenti
001006170810     d dLat          e ds
001008170810     D TRUL31DS      E DS
001009170810     D TRUL31DS2     E DS
001200020404     d prtds           ds
001300020404     d   spab                         3  0
001400020404     d   spaa                         3  0
001500020404     d   skab                         3  0
001600020404     d   skaa                         3  0
001700020404     d   line                         3  0
001800020305     d dataiso         s               d   datfmt(*iso)
001900020305     d dataeur         s               d   datfmt(*eur)
002000030903     d wdateur         s               d   datfmt(*eur)
002100030630     d wdatiso         s               d   datfmt(*iso)
002200030903     d tradtms         s              8  0
002300030903     d tradt2s         s              8  0
002400031104     d totale          s              5  0
002500031104     d totfil          s              5  0
002600031104     d exfil           s                   like(trafil)
002700120329     d extrataa        s                   like(trataa)
002800120412     d extraaut        s                   like(traaut)
002900070523     D err             S             78    DIM(9) CTDATA PERRCD(1)
003000070523     D WLBDA8          DS
003100070523     D  G02DAT                 1      8  0
003200070523     D  G02INV                 9     16  0
003300070523     D  G02ERR                17     17
003400070523     D  G02TGI                18     22  0
003401170810     d wabi            s                   like(UteAut)
003402170810     d* p.o. abilitati
003403170810     D $pogn           s              3  0 dim(250)
003404170810     D $pog            s              3    dim(250)
003405170810     D $DIG            s              1    dim(20)
003406170810     D $ARG            s              3    dim(50)
003500000000     C*---------------------------------------------------------------*
003600020304      *
003700031103     c                   do        *hival
003800031103     c                   exfmt     video1
003900031105     c                   clear                   $msg
004000031104     c                   setoff                                       999796
004100060927     c                   setoff                                       959392
004200031103     c   kc              leave
004300031103     c                   exsr      controv1
004400031104     c   96              iter
004500060920     c                   if        *inkf
004600060920     c                   if        v1cfil <> *zeros  or
004700170810     c                             v1cfil = *zeros and dutpou = 046 and
004800060920     c                             v1care = *blank and v1cdis = *blank
004900060920     c                   exsr      aitra
005000060920     c                   else
005100060920     c                   exsr      areadis
005200060920     c                   endif
005300061019     c                   feod      prtf198
005400061019     c                   z-add     99            line
005500060920     c                   endif
005600060920
005700031103     c                   enddo
005800060921     c                   if        una = *blank and v1cfil <> *zeros
005900031104     c                   except    testata
006000031104     c                   except    noncene
006100031104     c                   endif
006200020308     c                   seton                                        lr
006300990113     c*-------------------------------------------------
006400030630     c     aitra         begsr
006500990113     c*-------------------------------------------------
006600020404     c                   clear                   una               1
006700031103     c                   if        v1cfil = *zeros
006800031103     c     *loval        setll     aitra02l
006900031103     c                   else
007000031103     c     v1cfil        setll     aitra02l
007100031103     c                   endif
007200981029     c                   do        *hival
007300031103     c                   if        v1cfil = *zeros
007400031103     c                   read      aitra02l                               98
007500031103     c                   else
007600031103     c     v1cfil        reade     aitra02l                               98
007700031103     c                   endif
007800031103     c   98              leave
007900061019     c                   if        exfil <> *zeros and trafil <> exfil
008000061019     c                             and una <> *blank
008100061020     c                   exsr      decodfil
008200031104     c                   except    filiale
008300031104     c                   clear                   totfil
008400031104     c                   clear                   una               1
008500031104     c                   endif
008600031104     c                   z-add     trafil        exfil
008700060105      *estrae tutte le anagrafiche con colore mezzo non impostato fuori dai  90 giorni
008800060920     c                   setoff                                       94
008900031003     c                   if        tratmp <> 'S' and tracor = *blank
009000031013     c                             and traris = ' ' and traann = *blank
009100030630     c                   if        tradin > 0
009200030630     c                   move      tradin        dataiso
009300030630     c     wdatiso       subdur    dataiso       diffe:*d          3 0
009400030630     c                   else
009500060105     c                   z-add     90            diffe
009600060920     c                   seton                                        94
009700030630     c                   end
009800120329     c                   if        diffe <  90
009900120427      * salvo date verniciatura prima di verificare accreditamento precedente
010000120427     c                   z-add     tradtm        savdtm            8 0
010100120427     c                   z-add     tradt2        savdt2            8 0
010200120329     c                   exsr      accr_prec
010300120427     c                   z-add     savdtm        tradtm
010400120427     c                   z-add     savdt2        tradt2
010500120427     c                   endif
010600060105     c                   if        diffe >= 90
010700030630     c                   if        line > 60
010800030630     c                   except    testata
010900030630     c                   endif
011000060920     C  n94              move      dataiso       dataeur
011100060920     C   94              clear                   dataeur
011200070730      * verifico se una delle 2 date è nel range di stampa
011300071005     c                   if        v1cda1g > 0
011400070730     c                   if        tradtm >= v1cda1g and tradtm <= v1cda2g
011500070730     c                             or tradt2 >= v1cda1g and tradt2 <= v1cda2g
011600070730     C                   clear                   tradtms
011700070730     C                   clear                   tradt2s
011800070730     c                   select
011900070730      * se la seconda è impostata deve essere nel range per essere stampata
012000070730     c                   when      tradt2 > 0
012100070730     c                   if        tradt2 >= v1cda1g and tradt2 <= v1cda2g
012200070730     c     *ymd          move      tradt2        wdateur
012300070730     c                   move      wdateur       tradt2s
012400071206     c                   if        tradtm > 0
012500071206     c     *ymd          move      tradtm        wdateur
012600071206     c                   move      wdateur       tradtms
012700071206     c                   endif
012800070730     c                   add       1             totfil
012900070730     c                   add       1             totale
013000070730     c                   move      'X'           una
013100070730     c                   except    detta
013200070730     c                   endif
013300070730      * se la prima è impostata deve essere nel range per essere stampata
013400070730      * solo se la seconda non è valorizzata
013500070730     c                   when      tradtm > 0
013600070730     c                   if        tradtm >= v1cda1g and tradtm <= v1cda2g
013700030903     c     *ymd          move      tradtm        wdateur
013800030903     c                   move      wdateur       tradtms
013900071206     c                   if        tradt2 > 0
014000071206     c     *ymd          move      tradt2        wdateur
014100071206     c                   move      wdateur       tradt2s
014200071206     c                   endif
014300070730     c                   add       1             totfil
014400070730     c                   add       1             totale
014500070730     c                   move      'X'           una
014600070730     c                   except    detta
014700030630     c                   endif
014800070730     c                   endsl
014900070523     c                   else
015000070523     c                   iter
015100070523     c                   endif
015200071005      * no date range
015300071005     c                   else
015400071005     C                   clear                   tradtms
015500071005     C                   clear                   tradt2s
015600071005     c                   if        tradt2 > 0
015700071005     c     *ymd          move      tradt2        wdateur
015800071005     c                   move      wdateur       tradt2s
015900071005     c                   add       1             totfil
016000071005     c                   add       1             totale
016100071005     c                   move      'X'           una
016200081103     c                   if        tradtm > 0
016300081103     c     *ymd          move      tradtm        wdateur
016400081103     c                   move      wdateur       tradtms
016500081103     c                   endif
016600071005     c                   except    detta
016700071005     c                   else
016800071005     c                   if        tradtm > 0
016900071005     c     *ymd          move      tradtm        wdateur
017000071005     c                   move      wdateur       tradtms
017100071005     c                   add       1             totfil
017200071005     c                   add       1             totale
017300071005     c                   move      'X'           una
017400071005     c                   except    detta
017500081031     c                   else
017600081031     c                   add       1             totfil
017700081031     c                   add       1             totale
017800081031     c                   move      'X'           una
017900081031     c                   except    detta
018000071005     c                   endif
018100071005     c                   endif
018200071005     c                   endif
018300031104
018400030630     c                   endif
018500030522     c                   endif
018600030630     c                   enddo
018700031105     c                   eval      $msg = 'Elenco stampato '
018800031105     c                   if        una <> *blank
018900061020     c                   exsr      decodfil
019000031104     c                   except    filiale
019100031105     c                   endif
019200031104     c                   if        v1cfil = 0
019300031104     c                   except    globale
019400031104     c                   endif
019500060920     c                   if        v1cdis = *blank and v1care = *blank
019600031104     c                   z-add     99            line
019700060922     c                   else
019800060922     c                   clear                   v1cfil
019900060920     c                   endif
020000060920
020100031104     c                   clear                   totfil
020200031104     c                   clear                   totale
020300031104     c                   clear                   exfil
020400020304
020500020304     c                   endsr
020600981029      *
020700120329     c*-------------------------------------------------
020800120329     c     accr_prec     begsr
020900120329     c*-------------------------------------------------
021000120329     c                   move      trataa        extrataa
021100120412     c                   move      traaut        extraaut
021200120329     c     ktra4         setgt     aitra04l
021300120329     c                   do        *hival
021400120329     c     ktra4         readpe    aitra04l
021500120329     c                   if        %eof(aitra04l)
021600120329     c                   leave
021700120329     c                   endif
021800120412     c                   if        traaut = *blank and trataa <> extrataa or
021900120412     c                             traaut = extraaut and trataa <> extrataa
022000120329     c                   iter
022100120329     c                   endif
022200120329     c                   if        tradfi = 0
022300120329     c                   iter
022400120329     c                   endif
022500120329     c                   move      tradin        dataiso
022600120329     c     wdatiso       subdur    dataiso       diffe:*d          3 0
022700120329     c                   leave
022800120329     c                   enddo
022900120329     c                   endsr
023000061020     c*-------------------------------------------------
023100061020     c     decodfil      begsr
023200061020     c*-------------------------------------------------
023300061020     c                   clear                   exfild           20
023400061020     c     exfil         chain     azorg01l
023500061020     c                   if        %found(azorg01l)
023600061020     c                   movel     orgdes        exfild
023700061020     c                   endif
023800061020
023900061020     c                   endsr
024000031103     c*-------------------------------------------------
024100031103     c     controv1      begsr
024200031103     c*-------------------------------------------------
024301170810     c                   clear                   v1cared
024302170810     c                   clear                   v1cdisd
024303170810     c                   clear                   v1dfil
024400060927      * selezione di P.O /Area/Distretto
024500060920     c                   if        v1cfil > 0 and v1care <> *blank or
024600060920     c                             v1cfil > 0 and v1cdis <> *blank or
024700060920     c                             v1cdis > *blank and v1care <> *blank
024800060927     c                   eval      $msg = err(5)
024900060927     c                   seton                                          96
025000060927     c                   goto      endrut
025100060920     c                   endif
025200060927      * profilo di area non può selezionare distretto
025300060927     c                   if        v1cdis <> *blank and
025400170810     c                             (wabi <> 'DI' and wabi <> 'AZ')
025500060927     c                   eval      $msg = err(1)
025600060927     c                   seton                                          96
025700060927     c                   goto      endrut
025800060927     c                   endif
025900170810      *  distretto non abilitato
026000170810     c                   if        v1cdis <> *blank
026001170810      * distretto
026002170810     C     v1cdis        ifeq      '?'
026003170810     C                   movel     '17'          kcod
026004170810     C                   clear                   kkey
026005170810     C                   call      'X§TABER'
026006170810     C                   parm                    kkut
026007170810     C                   parm                    kcod
026008170810     C                   parm                    kkey
026009170810     C                   parm                    §des
026010170810     c                   movel     §des          v1cdisd
026011170810     c                   movel     kkey          v1cdis
026012170810     C*
026013170810     C                   else
026014170810     C*
026015170810     C                   if        v1cdis <> *blank and v1cdis <> *zero
026016170810     C                   movel     '17'          kcod
026017170810     C                   movel(p)  v1cdis        kkey
026018170810     C     ktab          chain     tabel00f
026019170810     c                   if        %found(tabel00f) and tblflg = *blank
026020170810     C                   movel     tbluni        v1cdisd
026021170810     c                   else
026022170810     c                   movel     *all'?'       v1cdisd
026023170810     C                   end
026024170810     C                   end
026025170810     C                   end
026100170810     c     v1cdis        lookup    $dig                                   70
026101170810     c                   if        not *in70
026300060927     c                   eval      $msg = err(2)
026400060927     c                   seton                                          96
026500060927     c                   goto      endrut
026600060927     c                   endif
026601170810     c                   endif
026602170810      *  area non abilitata
026603170810     c                   if        v1care <> *blank
026604170810      *  parzializza area
026605170810     C     v1care        ifeq      '?'
026606170810     C                   movel     '05'          kcod
026607170810     C                   clear                   kkey
026608170810     C                   call      'X§TABER'
026609170810     C                   parm                    kkut
026610170810     C                   parm                    kcod
026611170810     C                   parm                    kkey
026612170810     C                   parm                    §des             30
026613170810     c                   movel     §des          v1cared
026614170810     c                   movel     kkey          v1care
026615170810     C*
026616170810     C                   else
026617170810     C*
026618170810     C                   if        v1care <> *blank and v1care <> *zero
026619170810     C                   movel     '05'          kcod
026620170810     C                   movel(p)  v1care        kkey
026621170810     C     ktab          chain     tabel00f
026622170810     c                   if        %found(tabel00f) and tblflg = *blank
026623170810     C                   movel     tbluni        v1cared
026624170810     c                   else
026625170810     c                   movel     *all'?'       v1cared
026626170810     C                   end
026627170810     C                   end
026628170810     C                   end
026629170810     c     v1care        lookup    $arg                                   70
026630170810     c                   if        not *in70
026631170810     c                   eval      $msg = err(3)
026632170810     c                   seton                                          96
026633170810     c                   goto      endrut
026634170810     c                   endif
026635170810     c                   endif
027100170810      * il po non è abilitato
027101170810     c                   if        v1cfil > 0
027102170810     c     v1cfil        chain     azorg01l
027103170810     c                   if        %found(azorg01l)
027104170810     c                   movel     orgdes        v1dfil
027105170810     c                   endif
027301170810     c     v1cfil        lookup    $pogn                                  70
027302170810     c                   if        not *in70
027400170810     c                   eval      $msg = err(4)
027500060927     c                   seton                                          96
027600060927     c                   goto      endrut
027700060927     c                   endif
027701170810     c                   endif
036400060920
036500060922     c                   if        *in90 and v1cfil = *zeros and
036600060922     c                             v1cdis = *blank and v1care = *blank
036700031104     c                   seton                                        9996
036800031103     c                   endif
036900070523      *verifica delle date
037000070523     c                   if        v1cda1 <> 0
037100070523     C                   MOVE      V1cda1        G02DAT
037200070523     C                   MOVEL     *BLANK        G02ERR
037300070523     C                   CALL      'XSRDA8'
037400070523     C                   PARM                    WLBDA8
037500070523     C     G02ERR        IFEQ      '1'
037600070523     C                   SETON                                          96
037700070523     c                   eval      $msg = err(8)
037800070523     C                   END
037900070523     c                   move      g02dat        v1cda1
038000070523     c                   move      g02inv        v1cda1g           8 0
038100070523     c                   else
038200070523     c                   clear                   v1cda1g
038300070523     c                   endif
038400070523     c                   if        v1cda2 <> 0
038500070523     C                   MOVE      V1cda2        G02DAT
038600070523     C                   MOVEL     *BLANK        G02ERR
038700070523     C                   CALL      'XSRDA8'
038800070523     C                   PARM                    WLBDA8
038900070523     C     G02ERR        IFEQ      '1'
039000070523     C                   SETON                                          96
039100070523     c                   eval      $msg = err(8)
039200070523     C                   END
039300070523     c                   move      g02dat        v1cda2
039400070523     c                   move      g02inv        v1cda2g           8 0
039500070523     c                   else
039600070523     c                   clear                   v1cda2g
039700070523     c                   endif
039800070523     c                   if        v1cda1g > v1cda2g
039900070523     C                   SETON                                          96
040000070523     c                   eval      $msg = err(9)
040100070523     c                   endif
040200060927     c     endrut        endsr
040300060920     c*-------------------------------------------------
040400060920     c     areadis       begsr
040500060920     c*-------------------------------------------------
040600060920     c     *loval        setll     azorg02l
040700060920     c                   do        *hival
040800060920     c                   read      azorg02l
040900060920      * solo i distretti fino al 5°
041000060920     c                   if        %eof(azorg02l) or orgfl3 > '5'
041100060920     c                   leave
041200060920     c                   endif
041300060920      * solo le filiali
041400060920     c                   if        orgfag <>'F' or %subst(orgde8:1:1) = *blank
041500060920     c                   iter
041600060920     c                   endif
041700060920      * selezionato distretto
041800060920     c                   if        v1cdis <> *blank and v1cdis = orgfl3
041900060920     c                   movel     orgfil        v1cfil
042000060920     c                   exsr      aitra
042100060920     c                   endif
042200060920      * selezionata area
042300060920     c                   move      orgcar        orgcara           3
042400060920     c                   if        v1care <> *blank and v1care = orgcara
042500060920     c                   movel     orgfil        v1cfil
042600060920     c                   exsr      aitra
042700060920     c                   endif
042800060920     c                   enddo
042900060920     c                   endsr
043000011026     c*-------------------------------------------------
043100011026     c     *inzsr        begsr
043200011026     c*-------------------------------------------------
043300020304     c     *entry        plist
043400020304     c                   parm                    kpjba
043500060920     c     ktab          klist
043600060920     c                   kfld                    kkut              1 0
043700060920     c                   kfld                    kcod              2
043800060920     c                   kfld                    kkey              8
043900060920     c                   z-add     1             kkut
044000060920     c                   move      '05'          kcod
044100011026
044200120329     c     ktra4         klist
044300120329     c                   kfld                    trafil
044400120329     c                   kfld                    traaut
044500120329
045000020218     C                   time                    w0120            14 0
045100020218     C                   move      w0120         wdat              8 0
045200030630     c                   move      wdat          dataeur
045300030630     c                   move      dataeur       dataiso
045400030630     c                   move      dataiso       wdatiso
045500030630
045600020404     c                   z-add     99            line
045601170810      *
045602170810     c                   exsr      repdatiute
046700170810     c                   move      dutpou        v1cfil
046800031103     c     v1cfil        chain     azorg01l
046900031103     c                   if        %found(azorg01l)
047000031103     c                   movel     orgdes        v1dfil
047100031103     c                   endif
047200060920      * verifica se profili di filiale con autorizzazione a area o distretto
047300060920      * oppure se profilo di sede nel caso abilita selezione area e distretto
047400170810     c                   if        wabi = 'RA' or wabi = 'DI' or
047600170810     c                             wabi = 'AZ'
047700060920     c                   seton                                        89
047800060927     c                   else
047801170810      * potrebbe avere abilitazione come terminal quindi non proteggo il po inserimento
047802170810     c                   if        wabi ='PO'
047900060927     c                   seton                                        88
047901170810     c                   endif
048000060920     c                   endif
048100011026     c                   endsr
048101170810     C*--------------------------------------------------------------------------------------------*
048102170810     C* REPERISCE I DATI UTENTE
048103170810     C*--------------------------------------------------------------------------------------------*
048104170810     C     REPDATIUTE    BEGSR
048105170810     C*
048106170810     C* INIZIALIZZA VARIABILI DI WRK
048107170810     C                   CLEAR                   TIBS34DS
048108170810     C                   CLEAR                   AZUTEDS
048109170810     C                   CLEAR                   DDATIUTE
048110170810     C*
048111170810     C     *DTAARA       DEFINE    §azute        azuteds
048112170810     C     *DTAARA       DEFINE    §datiute      ddatiute
048113170810     C                   IN(E)     *DTAARA
048114170810     C                   IF        %Error
048115170810     C                   EVAL      I34Tla = 'L'
048116170810     C                   CALL      'TIBS34R'
048117170810     C                   PARM                    Tibs34Ds
048118170810     C                   IN        *DTAARA
048119170810     C                   ENDIF
048120170810     c                   Clear                   wabi
048121170810     c                   Clear                   dLat
048122170810
048123170810      * Verifica errori e autorità profilo
048124170810s   1c                   Select
048125170810      * se ho errori nei dati utente esco dal pgm
048126170810w   1c                   When      DutErr = 'E'
048127170810     c                   seton                                        lr
048128170810     c                   return
048129170810      * se non c'è l'abilitazione
048130170810      * --> se 1° livello, abilitazioni al terminal
048131170810      *     se 2° livello, abilitazioni al punto operativo
048132170810w   1c                   When      UteAut = *Blanks
048133170810if  2c                   If        DutLpo = '1'
048134170810     c                   Eval      wabi   = 'TP'
048135170810e   2c                   EndIf
048136170810if  2c                   If        DutLpo = '2'
048137170810     c                   Eval      wabi   = 'PO'
048138170810e   2c                   EndIf
048139170810if  2c                   If        DutLpo = 'S'
048140170810     c                   Eval      wabi   = 'AZ'
048141170810e   2c                   EndIf
048142170810      * carica le abilitazioni del profilo
048143170810x   1c                   Other
048144170810     c                   Movel     UteFaf        Dute01
048145170901     c                   If        §Utecerv <> *Blanks
048148170810     c                   Eval      wabi = §utecerv
048149170810     c                   EndIf
048150170810e   1c                   EndSl
048151170810      * controllo se ok l'abilitazione dell'utente
048152170810     c                   Clear                   Tibs02ds
048153170810     c                   Eval      T02Mod = 'C'
048154170810     c                   Eval      T02Sif = knsif
048155170810     c                   Eval      T02Cod = 'LAT'
048156170810     c                   Movel(p)  wabi          T02Ke1
048157170810     c                   Call      'TIBS02R'
048158170810     c                   Parm                    kpjba
048159170810     c                   Parm                    Tibs02ds
048160170810if  1c                   If        T02Err = *Blanks
048161170810     c                   Eval      dLat = T02Uni
048162170810e   1c                   EndIf
048163170810      * errore o non abilitato (impossibile xchè controllato nel filtro)
048164170810if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S' or
048165170810     c                             wabi = 'NO' or wabi = *blank
048166170810     c                   seton                                        98
048167170810     c                   call      'XUTENOAB'
048168170810     c                   parm                    kpjba
048169170810     c                   seton                                        lr
048170170810     c                   return
048171170810   x1c                   endif
048172170810      * Reperimento dei P.O. gestibili dall'utente
048173170810     c                   clear                   TRUL31DS
048174170810     c                   clear                   TRUL31DS2
048175170810     c                   eval      I31abi = wabi
048176170810     c                   eval      I31cdi = DUTdis
048177170810     c                   eval      I31car = DUTare
048178170810     c                   eval      I31cpo = DUTpou
048179170810     c                   call      'TRUL31R'
048180170810     c                   parm                    KPJBA
048181170810     c                   parm                    TRUL31DS
048182170810     c                   parm                    TRUL31DS2
048183170810if  2c                   if        O31pog > *zeros
048184170810     c                   movea     O31pog        $pog
048185170810     c                   movea     O31arg        $arg
048186170810     c                   movea     O31dig        $dig
048187170810x   2c                   else
048188170810     c* impossibile perchè controllato nel filtrp
048189170810     c                   seton                                        lr
048190170810     c                   return
048191170810    1c                   endif
048192170810     c                   do        250           g                 3 0
048193170810     c                   if        $pog(g) > *zero
048194170810     c                   move      $pog(g)       $pogn(g)
048195170810     c                   else
048196170810     c                   move      999           $pogn(g)
048197170810     c                   endif
048198170810     c                   enddo
048202170810     c                   endsr
048203020404     C*--------------------------------------------------------------------
048300031028     Oprtf198   E            testata        1 02
048400030630     o                                           10 'TRMZ71R'
048500020404     o                       knmus               22
048600030630     o                                        +   1 'Lista autisti accreditati -
048700030630     o                                              con mezzo non verniciato'
048800030630     o                       udate         y  +   1
048900020404     O                       page          z  +   1
049000020404     O          E            testata     1  1
049100030630     o                                           20 'Autotrasportatore'
049200030630     o                                           45 'Società'
049300030909     o                                           65 'Targa'
049400030909     o                                           80 'P.O'
049500030909     o                                           92 'Data Accr.'
049600030909     o                                          104 'Prev.Vern.'
049700030909     o                                          116 'Prev.Vern.'
049800031028     o                                          130 'Note'
049900020404     O          E            detta       1
050000030630     o                       traaut              25
050100030630     o                       trasoc              52
050200030909     o                       trataa              67
050300030909     o                       trafil              80
050400030909     o                       dataeur             92
050500030909     o                       tradtms       8    104
050600030909     o                       tradt2s       8    116
050700031028     o                       trant1             190
050800031104     O          E            filiale     1
050900031104     o                                           25 'T o t a l e  p e r  P.O'
051000031104     o                       exfil               30
051100061019     o                       exfild              52
051200061019     o                       totfil        z     62
051300061019     O          E            filiale     0
051400061019     o                                           25 'T o t a l e  p e r  P.O'
051500061019     o                       exfil               30
051600061019     o                       exfild              52
051700061019     o                       totfil        z     62
051800061019     O          E            filiale        1
051900061019     o                                           25 'T o t a l e  p e r  P.O'
052000061019     o                       exfil               30
052100061019     o                       exfild              52
052200061019     o                       totfil        z     62
052300031104     O          E            globale     1
052400031104     o                                           30 'T o t a l e  G e n e r a -
052500031104     o                                              l e'
052600031104     o                       totale        z     40
052700020404     O          E            noncene     3
052800020404     o                                           80 'N O N   E S I S T O N O   -
052900030630     o                                              A U T I S T I   D A  C O N -
053000020404     o                                              V A L I D A R E'
053100060927      *__________________________________________________________________
053200060927** ERR
053300060927Profilo di area non può selezionare distretto                                  1
053400060927Il distretto richiesto è incongruente con il profilo                           2
053500170810Area richiesta è incongruente con il profilo                                   3
053600170810Il P.O richiesto non è abilitato per l'utente                                  4
053700060927Selezionare solo uno fra P.O / Area / Distretto                                5
053800070523L'area non è di competenza al profilo                                          6
053900070523L'area selezionata non appartiene al distretto di competenza                   7
054000070523Data non valida                                                                8
054100070523Incongruenza nelle date immesse                                                9
