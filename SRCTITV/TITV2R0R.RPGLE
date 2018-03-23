000100131108      /TITLE Upload via Internet
000200100308     H DFTACTGRP(*NO) BNDDIR('TRUL':'UBRTVNETA') ACTGRP('BARTVAS')
000300000313     F*
000400131108     Ffiar501l  uF   E           K DISK    usropn
000500131108     F                                     extfile(LibFileAR5)
000600990910     Ftivin00r  uF   E             DISK    usropn
000700000313     D*
000800040709     D*------------
000900040709     D* SCHIERE A PROGRAMMA
001000040709     D*------------
001100100317     D intcol          S            100    DIM(4) CTDATA PERRCD(1)
001200010330     D*----------------------------------------------------
001300010330     D* DICHIARAZIOINE VARIABILI DI WRK
001400010330     D*----------------------------------------------------
001500010330     D dscmz         e ds                  inz
001600010330     D psds           sds
001700010330     D  procname         *PROC
001800010330     D tivlrds       e ds                  extname(tivlr00f)
001900131108     D dar5emd       e ds                  inz
002000010330     D esito           s              1
002100010330     D prmlit          s             10
002200010330     D prmfir          s             10
002300010330     D wrkesito        s                   like(esito)
002400131108     D rrnum           s              6  0 inz(*zeros)
002500131108     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
002600100308     D currSysNeta     s              8A   inz('*NULL')
002700131108     D LibFileAR5      s             21A   inz
002800131108     D wSMS_STS        s              5A   inz
002900131108
003000131108
003100131108     D*------------------
003200131108     D* DS RIDEFINIZIONE CHIAVE SMS
003300131108     D*------------------
003400131108     D DS_SMS_KEY      DS                  qualified
003500131108     D  filler001                    15
003600131108     D  anno                          4  0
003700131108     D  lnp                           3  0
003800131108     D  nrs                           2  0
003900131108     D  nsp                           7  0
004000131108     D  filler002                     1
004100131108     D  type                          4
004200131108
004300131108
004400030715     D*------------------
004500030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004600030715     D*------------------
004700131108     D SkSplitFLD      S             10    DIM(100) INZ
004800131108     D SkSplitCSV      S            256    DIM(100) INZ
004900131108     D CharCSV         S              1    INZ
005000131108     D CharTXT         S              1    INZ
005100131108     D CharNUM         S              1    INZ
005200030715     D posDa           S              3  0 INZ(*zeros)
005300030715     D posA            S              3  0 INZ(*zeros)
005400030715     D i               s              3  0 INZ(1)
005500030715     D wGiro           s              1  0 INZ(*zeros)
005600030822     D*------------------
005700030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005800030822     D*------------------
005900030822     D posDaDft        S              3  0 INZ(*zeros)
006000030822     D posADft         S              3  0 INZ(*zeros)
006100030822     D j               s              3  0 INZ(1)
006200030822     D wGiroDft        s              1  0 INZ(*zeros)
006300010330     D*------------------
006400130626
006500080702
006600080702     D*------------------
006700080702     D* LINKING A DEFINIZIONI ESTERNE
006800080702     D*------------------
006900100308     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
007000100308     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
007100080702
007200010330
007300100308     C*
007400100308     C* Reperisco il sistema AS/400 corrente
007500100308     C                   callp     UBRTVNETA_Rtv(currSysNeta)
007600100308     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
007700131108     C                   eval      LibFileAR5 = 'FILTRA201/FIAR501L'
007800100308     C                   else
007900131108     C                   eval      LibFileAR5 = 'FILTRAPRD/FIAR501L'
008000100308     C                   endif
008100100308     C*
008200000913     C                   reset                   rrnum
008300990921     C                   reset                   esito
008400990921     C                   reset                   wrkesito
008500010601     C*
008600010601     C                   exsr      opeini
008700131108     C                   exsr      preela
008800131108     C                   exsr      rwfile
008900100308     C*
009000010601     C                   seton                                        lr
009100010601
009200010601
009300010601
009400010601
009500010601     C*--------------------------------------------------------
009600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009700010601     C*--------------------------------------------------------
009800010601     C     PREELA        BEGSR
009900010601     C*
010000010601     C* SE OCCORRE SPEDIRE IN FILIALE
010100041011     C                   if        flgGiro = '0'
010200010601     C*
010300010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010400010601     C                   eval      flgGiro = '1'
010500041011     C*
010600041011     C                   endif
010700010601     C*
010800010601     C                   ENDSR
010900010601     C***
011000010601
011100010601
011200131108     C     RWFILE        BEGSR
011300131108     C*
011400131108     C* Inizializzo subito ad OK l'esito della transazione
011500131108     C                   eval      wrkesito = '0'
011600010330     C*
011700010330     C                   if        not %open(tivin00r)
011800010330     C                   open      tivin00r
011900010330     C                   endif
012000131108     C*
012100131108     C                   if        not %open(fiar501l)
012200131108     C                   open      fiar501l
012300131108     C                   endif
012400100316     C*
012500100316     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
012600100316     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
012700100316     C                   exsr      inzvar
012800131108     C                   exsr      impfile
012900010330     C*
013000030822     C                   do        *HIVAL
013100010330     C*
013200030822     C                   read      tivin00r                               70
013300010618     C*
013400040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
013500040510     C                   z-add     *zeros        wLenVINDTA        4 0
013600040510     C                   z-add     *zeros        wFlgVINDTA        1 0
013700040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
013800040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
013900040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
014000040510     C                   z-add     1             wFlgVINDTA
014100040510     C                   leave
014200040510     C                   endif
014300040510     C                   enddo
014400040510     C*
014500040218     C                   if        vindta > *blanks AND
014600040510     C                             wFlgVINDTA = 1
014700010330     C                   add       1             rrnum
014800010330     C*
014900010601     C                   if        *in70 = *off and
015000010330     C                             (vinflg = *blanks
015100010330     C                              or vinflg = '0'
015200010330     C                              or vinflg = '2')
015300010330     C*
015400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
015500010711     C                   if        vinflg = *blanks or vinflg = '0'
015600010711     C                   clear                   vinmsg
015700010711     C                   endif
015800010601     C*
015900010330     C                   exsr      inzvar
016000131108     C                   exsr      impfile
016100010601     C*
016200010604     C* Ebbene...
016300030822     C                   if        wGiro = 2
016400131108     C                   if        not *in31
016500131108     C   20              exsr      exeSTSSMS
016600131108     C                   endif
016700030822     C                   endif
016800010604     C*
016900131108     C                   if        *in31 = *off
017000010604     C                   eval      vinflg = '1'
017100010604     C                   else
017200010604     C                   eval      vinflg = '2'
017300010604     C                   endif
017400010601     C*
017500010604     C                   endif
017600010604     C*
017700010330     C                   else
017800010330     C                   eval      vinflg = '1'
017900010330     C                   endif
018000010601     C*
018100010601     C  N70              update    tivin000
018200010330     C*
018300030822     C  N70              enddo
018400010330     C*
018500010330     C                   if        %open(tivin00r)
018600010330     C                   close     tivin00r
018700010330     C                   endif
018800131108     C*
018900131108     C                   if        %open(fiar501l)
019000131108     C                   close     fiar501l
019100131108     C                   endif
019200131108     C*
019300131108     C                   eval      wrkesito = '0'
019400010330     C*
019500010330     C                   ENDSR
019600010330     C***
019700010601
019800010601
019900131108     C     INZVAR        BEGSR
020000010330     C*
020100030822     C*
020200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
020300131108     C                   CLEAR                   DS_SMS_KEY
020400131108     C                   CLEAR                   wSMS_STS
020500131108     C                   CLEAR                   dar5emd
020600131108     C                   CLEAR                   SkSplitCSV
020700100308     C*
020800030822     C                   EVAL      i = 1
020900030822     C                   EVAL      posDa = *zeros
021000030822     C                   EVAL      posA  = *zeros
021100030822     C*
021200030822     C* Reimposto i valori di default
021300030822     C                   EXSR      DEFCAM
021400030822     C*
021500010330     C                   ENDSR
021600010330     C*----------------------------------------------------*
021700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
021800010330     C*----------------------------------------------------*
021900010330     C     DEFCAM        BEGSR
022000030822     C*
022100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
022200030715     C* e delimitatore testo.
022300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
022400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
022500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
022600030822     C*
022700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
022800030715     C                   IF        %subst(vlrppt:1:1) = 'S'
022900030715     C                   SETON                                        50
023000030715     C                   ELSE
023100030715     C                   SETOFF                                       50
023200030715     C                   ENDIF
023300040723     C*
023400131108 xxx C* Imposto i valori d default d alcuni campi
023500100309     C*
023600030822     C*
023700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
023800030822     C                   EVAL      posDaDft = 1
023900030822     C                   EVAL      posADft  = 0
024000030822     C                   EVAL      wGiroDft = 0
024100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
024200030822     C                             posDaDft > 0
024300030822     C*
024400030822     C* Gestisco il 1° giro
024500030822     C                   IF        wGiroDft = 0
024600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
024700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
024800030822     C* Incremento il contatore dei "giri"
024900030822     C                   EVAL      wGiroDft = 1
025000030822     C                   ELSE
025100030822     C                   EVAL      posDaDft = posADft
025200030822     C                   ENDIF
025300030822     C* Eseguo lo scan x trovare la fine del campo corrente
025400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
025500030822     C*
025600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
025700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
025800030822     C                   IF        posDaDft > 0 AND
025900030822     C                             posADft  > 0
026000131108 xxx C* XXX
026100131108     C                   IF        %subst(
026200131108     C                             %subst(vlrppt:posDaDft+1:
026300131108     C                             posADft-posDaDft-1):1:3)
026400131108     C                             = 'XXX'
026500131108     C***                EVAL      XXXXXX=%trim(%subst(
026600131108     C***                          %subst(vlrppt:posDaDft+1:
026700131108     C***                          posADft-posDaDft-1):4:posADft-posDaDft-1-3))
026800131108     C                   ENDIF
026900030822     C* ...
027000030822     C                   ENDIF
027100030822     C                   ENDDO
027200020204     C*
027300010330     C                   ENDSR
027400010607     C*----------------------------------------------------*
027500040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
027600010607     C*----------------------------------------------------*
027700131108     C     IMPFILE       BEGSR
027800010607     C*
027900131108     C* Resetto indicatori di errore
028000131108     C                   setoff                                       31
028100131108     C*
028200131108     C* Resetto indicatori di tipo status SMS
028300131108     C                   setoff                                       20
028400010607     C*
028500030715     C**********
028600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
028700030715     C**********
028800030715     C                   IF        wGiro = *zeros
028900040709     C*
029000040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
029100040709     C                   EVAL      depvinDTA = vinDTA
029200040709     C*
029300040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
029400040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
029500100317     C                                      %trim(intcol(3))+%trim(intcol(4))
029600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
029700050314     C**!!!minu:maiu     XLATE     vindta        vindta
029800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
029900040324     C                   DOW       posDa <= %len(%trimr(vindta))
030000030715     C*
030100030715     C* Gestisco il 1° campo
030200030715     C                   IF        i = 1
030300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
030400030715     C                   EVAL      posDa = 1
030500030715     C* Eseguo lo scan x trovare la fine del primo campo
030600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
030700030715     C* A questo punto "estraggo" il campo corrente
030800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
030900030715     C                                                    (posA-posDa))
031000030715     C* X i campi successivi al 1°
031100030715     C                   ELSE
031200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
031300030715     C                   EVAL      posDa = posA + 1
031400030715     C* Eseguo lo scan x trovare la fine del campo corrente
031500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
031600030715     C* Gestisco l'ultimo campo
031700030715     C                   IF        posA = *zeros
031800040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
031900030715     C* A questo punto "estraggo" il campo corrente
032000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
032100030715     C                                                    (posA-posDa))
032200030715     C                   LEAVE
032300030715     C                   ELSE
032400030715     C* A questo punto "estraggo" il campo corrente
032500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
032600030715     C                                                    (posA-posDa))
032700030715     C                   ENDIF
032800030715     C                   ENDIF
032900030715     C* Incremento il contatore d campo
033000030715     C                   EVAL      i = i +1
033100030715     C                   ENDDO
033200030715     C*
033300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
033400030715     C* delimitazione testo
033500030715     C                   EVAL      i = 1
033600030715     C                   DOW       i < %elem(SkSplitFLD)
033700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
033800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
033900030715     C                   EVAL      i = i + 1
034000030715     C                   ENDDO
034100030715     C                   EVAL      wGiro = 1
034200040709     C* Ripristino la 1° riga originale
034300040709     C                   EVAL      vinDTA = depvinDTA
034400030715     C                   ELSE
034500030715     C                   EVAL      wGiro = 2
034600031204     C**********
034700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
034800031204     C* d separatore campo
034900031204     C**********
035000040709     C                   EVAL      vindta = %trim(vindta)
035100040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
035200040709     C                   EVAL      vindta = %subst(vindta:2)
035300040709     C                   ENDDO
035400040709     C*
035500040709     C                   Z-ADD     *zeros        lunghInput        4 0
035600040709     C                   EVAL      lunghInput = %len(%trim(vindta))
035700040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
035800040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
035900040709     C                   EVAL      lunghInput = %len(%trim(vindta))
036000040709     C                   ENDDO
036100030715     C**********
036200030715     C* Effettuo lo split del campo dati d input
036300030715     C**********
036400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
036500040324     C                   DOW       posDa <= %len(%trimr(vindta))
036600030715     C*
036700030715     C* Gestisco il 1° campo
036800030715     C                   IF        i = 1
036900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
037000030715     C                   EVAL      posDa = 1
037100030715     C* Eseguo lo scan x trovare la fine del primo campo
037200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
037300030715     C* A questo punto "estraggo" il campo corrente
037400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
037500030715     C                                                    (posA-posDa))
037600030715     C* X i campi successivi al 1°
037700030715     C                   ELSE
037800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
037900030715     C                   EVAL      posDa = posA + 1
038000031204     C* Verifico che nn vi sia il campo nn valorizzato
038100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
038200031204     C* Se campo nn valorizzato skippo al prossimo
038300031204     C                   EVAL      SkSplitCSV(i) = *blanks
038400031204     C                   EVAL      posA  = posA + 1
038500031204     C                   ELSE
038600030715     C* Eseguo lo scan x trovare la fine del campo corrente
038700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
038800030715     C* Gestisco l'ultimo campo
038900030715     C                   IF        posA = *zeros
039000040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
039100030715     C* A questo punto "estraggo" il campo corrente
039200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
039300030715     C                                                    (posA-posDa))
039400030715     C                   LEAVE
039500030715     C                   ELSE
039600030715     C* A questo punto "estraggo" il campo corrente
039700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
039800030715     C                                                    (posA-posDa))
039900030715     C                   ENDIF
040000031204     C                   ENDIF
040100030715     C                   ENDIF
040200030715     C* Incremento il contatore d campo
040300030715     C                   EVAL      i = i +1
040400030715     C                   ENDDO
040500030715     C*
040600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
040700030715     C* delimitazione testo
040800030715     C                   EVAL      i = 1
040900030715     C                   DOW       i < %elem(SkSplitCSV)
041000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
041100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
041200030715     C                   EVAL      i = i + 1
041300030715     C                   ENDDO
041400030715     C*
041500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
041600030715     C                   EVAL      i = 1
041700030715     C                   DOW       i < %elem(SkSplitFLD)
041800030715     C*
041900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
042000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
042100030715     C                             SkSplitCSV(i) <> *blanks
042200131108     C*** SMS_KEY
042300131108     C                   IF        %trim(SkSplitFLD(i)) = 'SMS_KEY'
042400131108     C                   EVAL      DS_SMS_KEY = SkSplitCSV(i)
042500030721     C                   ENDIF
042600131108     C*** SMS_STS
042700131108     C                   IF        %trim(SkSplitFLD(i)) = 'SMS_STS'
042800140219     C                   IF        SkSplitCSV(i) <> *blanks
042900131108     C                   EVAL      wSMS_STS = SkSplitCSV(i)
043000140219     C                   ENDIF
043100131108     C                   ENDIF
043200090428     C*
043300090428     C********** GESTIONE CAMPI PERSONALIZZATI
043400090428     C*
043500090428     C*
043600090428     C****************************************
043700090428     C*
043800030715     C*
043900030715     C                   ENDIF
044000030715     C                   EVAL      i = i + 1
044100030715     C                   ENDDO
044200040723     C*
044300131108     C* Stabilisco la tipologia di  status SMS corrente
044400131108     C                   select
044500131108     C                   when      DS_SMS_KEY.type = 'SAAS'
044600131108     C                   seton                                        20
044700140219     C                   when      DS_SMS_KEY.type = 'ARIC'
044800140219     C                   seton                                        20
044900140612     C                   when      DS_SMS_KEY.type = 'AFFD'
045000140612     C                   seton                                        20
045100170216     C***                when      DS_SMS_KEY.type = 'OCON'
045200170216     C***                seton                                        20
045300170216     C***                when      DS_SMS_KEY.type = 'OAVV'
045400170216     C***                seton                                        20
045500170216     C                   when      DS_SMS_KEY.type = 'ACL1'
045600170216     C                   seton                                        20
045700170216     C                   when      DS_SMS_KEY.type = 'ACL3'
045800170216     C                   seton                                        20
045900131108     C                   endsl
046000131108     C*
046100030715     C                   ENDIF
046200020204     C*
046300010607     C                   ENDSR
046400010607     C*----------------------------------------------------*
046500131108
046600131108
046700131108     C     exeSTSSMS     BEGSR
046800131108     C*
046900131108     C* Chaino il record di FIAR5
047000131108     C                   eval      ar5AAS = DS_SMS_KEY.anno
047100131108     C                   eval      ar5LNP = DS_SMS_KEY.lnp
047200131108     C                   eval      ar5NRS = DS_SMS_KEY.nrs
047300131108     C                   eval      ar5NSP = DS_SMS_KEY.nsp
047400131108     C                   eval      ar5TRD = 'EMD'
047500131108     C     KEY_AR5_1P    chain     fiar501l
047600131108     C                   if        %found(fiar501l)
047700131108     C*
047800131108     C                   eval      dar5emd = ar5UNI
047900131108     C                   select
048000131108     C                   when      wSMS_STS = '1'
048100131108     C                   eval      §AR5SME  = 'O'
048200131108     C                   when      wSMS_STS = '2'
048300131108     C                   eval      §AR5SME  = 'K'
048400131108     C                   when      wSMS_STS = '16'
048500131108     C                   eval      §AR5SME  = 'N'
048600131108     C                   other
048700131108     C                   eval      §AR5SME  = '*'
048800131108     C                   endsl
048900131108     C*
049000131108     C                   eval      ar5UNI = dar5emd
049100131108     C                   update    fiar5000
049200131108     C*
049300131108     C                   endif
049400131108     C*
049500131108     C                   ENDSR
049600131108     C***
049700010601
049800010601
049900010601      /TITLE Invio dei dati al punto operativo.
050000010601     C     opeini        BEGSR
050100010601     C*
050200010601     C* Inizializzo flag e contatori operativi
050300010601     C                   movel     '0'           flgGiro           1
050400010601     C*
050500010601     C                   ENDSR
050600010601     C***
050700050421
050800050421
050900050421     C     *pssr         BEGSR
051000050421     C*
051100050421     C                   eval      wrkesito = '2'
051200050421     C*
051300050421     C                   ENDSR     '*CANCL'
051400050421     C***
051500010330
051600010330
051700000613     C     *inzsr        BEGSR
051800990910     C*
051900990910     C     *entry        plist
052000990920     C                   parm                    tivlrds
052100990921     C                   parm      wrkesito      esito
052200000724     C                   parm                    prmlit
052300000710     C                   parm                    prmfir
052400040723     C*
052500040723     C* Definizione chiavi di lettura
052600131108     C* FIAR501L - Parziale
052700131108     C     KEY_AR5_1P    klist
052800131108     C                   kfld                    ar5AAS
052900131108     C                   kfld                    ar5LNP
053000131108     C                   kfld                    ar5NRS
053100131108     C                   kfld                    ar5NSP
053200131108     C                   kfld                    ar5TRD
053300131108     C*
053400131108     C                   ENDSR
053500000613     C***
053600040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
053700131108SMS_KEY|SMS_NUM|SMS_DATA|SMS_STS
053800131108
053900131108
054000131108
