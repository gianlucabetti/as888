000100131108      /TITLE Upload via Internet
000200100308     H DFTACTGRP(*NO) BNDDIR('TRUL':'UBRTVNETA') ACTGRP('BARTVAS')
000300000313     F*
000400141211     Ftimat01i  uF a E           K DISK    prefix(i_)
000500990910     Ftivin00r  uF   E             DISK    usropn
000600000313     D*
000700040709     D*------------
000800040709     D* SCHIERE A PROGRAMMA
000900040709     D*------------
001000100317     D intcol          S            100    DIM(4) CTDATA PERRCD(1)
001100010330     D*----------------------------------------------------
001200010330     D* DICHIARAZIOINE VARIABILI DI WRK
001300010330     D*----------------------------------------------------
001400010330     D dscmz         e ds                  inz
001500010330     D psds           sds
001600010330     D  procname         *PROC
001700010330     D tivlrds       e ds                  extname(tivlr00f)
001800141211     D timat_00      e ds                  extname(timat00f)
001900141211     D timat_01      e ds                  extname(timat00f) prefix(i_)
002000010330     D esito           s              1
002100010330     D prmlit          s             10
002200010330     D prmfir          s             10
002300010330     D wrkesito        s                   like(esito)
002400131108     D rrnum           s              6  0 inz(*zeros)
002500141209     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
002600131108
002700131108
002800030715     D*------------------
002900030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003000030715     D*------------------
003100131108     D SkSplitFLD      S             10    DIM(100) INZ
003200141209     D SkSplitCSV      S            512    DIM(100) INZ
003300131108     D CharCSV         S              1    INZ
003400131108     D CharTXT         S              1    INZ
003500131108     D CharNUM         S              1    INZ
003600141218     D CharSEP         S              1    INZ
003700141218     D CharSEP_P       S              2    INZ
003800141218     D CharSEP_D       S              2    INZ
003900141218     D CharSEP_E       S              3    INZ
004000141218     D posDa           S              3  0 INZ(*zeros)
004100030715     D posA            S              3  0 INZ(*zeros)
004200030715     D i               s              3  0 INZ(1)
004300030715     D wGiro           s              1  0 INZ(*zeros)
004400030822     D*------------------
004500030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
004600030822     D*------------------
004700030822     D posDaDft        S              3  0 INZ(*zeros)
004800030822     D posADft         S              3  0 INZ(*zeros)
004900030822     D j               s              3  0 INZ(1)
005000030822     D wGiroDft        s              1  0 INZ(*zeros)
005100010330     D*------------------
005200080702
005300010330
005400100308     C*
005500000913     C                   reset                   rrnum
005600990921     C                   reset                   esito
005700990921     C                   reset                   wrkesito
005800010601     C*
005900010601     C                   exsr      opeini
006000131108     C                   exsr      preela
006100131108     C                   exsr      rwfile
006200100308     C*
006300010601     C                   seton                                        lr
006400010601
006500010601
006600010601
006700010601
006800010601     C*--------------------------------------------------------
006900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007000010601     C*--------------------------------------------------------
007100010601     C     PREELA        BEGSR
007200010601     C*
007300010601     C* SE OCCORRE SPEDIRE IN FILIALE
007400041011     C                   if        flgGiro = '0'
007500010601     C*
007600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007700010601     C                   eval      flgGiro = '1'
007800041011     C*
007900041011     C                   endif
008000010601     C*
008100010601     C                   ENDSR
008200010601     C***
008300010601
008400010601
008500131108     C     RWFILE        BEGSR
008600131108     C*
008700131108     C* Inizializzo subito ad OK l'esito della transazione
008800131108     C                   eval      wrkesito = '0'
008900150126     C                   setoff                                       32
009000010330     C*
009100010330     C                   if        not %open(tivin00r)
009200010330     C                   open      tivin00r
009300010330     C                   endif
009400100316     C*
009500100316     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
009600100316     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
009700100316     C                   exsr      inzvar
009800131108     C                   exsr      impfile
009900010330     C*
010000030822     C                   do        *HIVAL
010100010330     C*
010200030822     C                   read      tivin00r                               70
010300141218     C*
010400141218     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi SEP
010500141218     C                   z-add     *zeros        wLenVINDTA        4 0
010600141218     C                   z-add     *zeros        wFlgVINDTA        1 0
010700141218     C                   dow       wLenVINDTA < %len(%trim(vindta))
010800141218     C                   eval      wLenVINDTA = wLenVINDTA + 1
010900141218     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharSEP
011000141218     C                   z-add     1             wFlgVINDTA
011100141218     C                   leave
011200141218     C                   endif
011300141218     C                   enddo
011400040510     C*
011500040218     C                   if        vindta > *blanks AND
011600040510     C                             wFlgVINDTA = 1
011700010330     C                   add       1             rrnum
011800010330     C*
011900010601     C                   if        *in70 = *off and
012000010330     C                             (vinflg = *blanks
012100010330     C                              or vinflg = '0'
012200010330     C                              or vinflg = '2')
012300010330     C*
012400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
012500010711     C                   if        vinflg = *blanks or vinflg = '0'
012600010711     C                   clear                   vinmsg
012700010711     C                   endif
012800010601     C*
012900010330     C                   exsr      inzvar
013000131108     C                   exsr      impfile
013100010601     C*
013200010604     C* Ebbene...
013300030822     C                   if        wGiro = 2
013400131108     C                   if        not *in31
013500141210     C                   exsr      exeSTSEML
013600131108     C                   endif
013700030822     C                   endif
013800010604     C*
013900131108     C                   if        *in31 = *off
014000010604     C                   eval      vinflg = '1'
014100010604     C                   else
014200010604     C                   eval      vinflg = '2'
014300010604     C                   endif
014400010601     C*
014500010604     C                   endif
014600010604     C*
014700010330     C                   else
014800010330     C                   eval      vinflg = '1'
014900010330     C                   endif
015000010601     C*
015100010601     C  N70              update    tivin000
015200010330     C*
015300150126     C  N70              enddo
015400010330     C*
015500010330     C                   if        %open(tivin00r)
015600010330     C                   close     tivin00r
015700010330     C                   endif
015800131108     C*
015900150126     C* controllo se ci sono stati warning o errori
016000150126     C                   eval      wrkesito = '0'
016100150126     C                   select
016200150126     C                   when      *in32 = *on
016300150126     C                   eval      wrkesito = '1'
016400150126     C                   endsl
016500010330     C*
016600010330     C                   ENDSR
016700010330     C***
016800010601
016900010601
017000131108     C     INZVAR        BEGSR
017100010330     C*
017200030822     C*
017300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
017400141209     C                   CLEAR                   timat000
017500141212     C                   CLEAR                   timat_00
017600141212     C                   CLEAR                   timat_01
017700131108     C                   CLEAR                   SkSplitCSV
017800100308     C*
017900030822     C                   EVAL      i = 1
018000030822     C                   EVAL      posDa = *zeros
018100030822     C                   EVAL      posA  = *zeros
018200030822     C*
018300030822     C* Reimposto i valori di default
018400030822     C                   EXSR      DEFCAM
018500030822     C*
018600010330     C                   ENDSR
018700010330     C*----------------------------------------------------*
018800030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
018900010330     C*----------------------------------------------------*
019000010330     C     DEFCAM        BEGSR
019100030822     C*
019200030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
019300030715     C* e delimitatore testo.
019400141218     C                   EVAL      CharCSV = %subst(vlrppt:1:1)
019500141218     C                   EVAL      CharSEP = %subst(vlrppt:2:1)
019600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
019700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
019800141218     C                   EVAL      CharSEP_P = CharTXT+CharSEP
019900141218     C                   EVAL      CharSEP_D = CharSEP+CharTXT
020000141218     C                   EVAL      CharSEP_E = CharTXT+CharSEP+CharTXT
020100040723     C*
020200131108 xxx C* Imposto i valori d default d alcuni campi
020300100309     C*
020400030822     C*
020500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
020600030822     C                   EVAL      posDaDft = 1
020700030822     C                   EVAL      posADft  = 0
020800030822     C                   EVAL      wGiroDft = 0
020900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
021000030822     C                             posDaDft > 0
021100030822     C*
021200030822     C* Gestisco il 1° giro
021300030822     C                   IF        wGiroDft = 0
021400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
021500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
021600030822     C* Incremento il contatore dei "giri"
021700030822     C                   EVAL      wGiroDft = 1
021800030822     C                   ELSE
021900030822     C                   EVAL      posDaDft = posADft
022000030822     C                   ENDIF
022100030822     C* Eseguo lo scan x trovare la fine del campo corrente
022200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
022300030822     C*
022400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
022500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
022600030822     C                   IF        posDaDft > 0 AND
022700030822     C                             posADft  > 0
022800131108 xxx C* XXX
022900131108     C                   IF        %subst(
023000131108     C                             %subst(vlrppt:posDaDft+1:
023100131108     C                             posADft-posDaDft-1):1:3)
023200131108     C                             = 'XXX'
023300131108     C***                EVAL      XXXXXX=%trim(%subst(
023400131108     C***                          %subst(vlrppt:posDaDft+1:
023500131108     C***                          posADft-posDaDft-1):4:posADft-posDaDft-1-3))
023600131108     C                   ENDIF
023700141217     C* TRC
023800141217     C                   IF        %subst(
023900141217     C                             %subst(vlrppt:posDaDft+1:
024000141217     C                             posADft-posDaDft-1):1:3)
024100141217     C                             = 'TRC'
024200141217     C                   EVAL      MATTRC=%trim(%subst(
024300141217     C                             %subst(vlrppt:posDaDft+1:
024400141217     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
024500141217     C                   ENDIF
024600030822     C* ...
024700030822     C                   ENDIF
024800030822     C                   ENDDO
024900020204     C*
025000010330     C                   ENDSR
025100010607     C*----------------------------------------------------*
025200040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
025300010607     C*----------------------------------------------------*
025400131108     C     IMPFILE       BEGSR
025500010607     C*
025600131108     C* Resetto indicatori di errore
025700131108     C                   setoff                                       31
025800010607     C*
025900030715     C**********
026000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
026100030715     C**********
026200030715     C                   IF        wGiro = *zeros
026300040709     C*
026400040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
026500040709     C                   EVAL      depvinDTA = vinDTA
026600040709     C*
026700040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
026800040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
026900100317     C                                      %trim(intcol(3))+%trim(intcol(4))
027000031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
027100050314     C**!!!minu:maiu     XLATE     vindta        vindta
027200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
027300040324     C                   DOW       posDa <= %len(%trimr(vindta))
027400030715     C*
027500030715     C* Gestisco il 1° campo
027600030715     C                   IF        i = 1
027700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
027800030715     C                   EVAL      posDa = 1
027900030715     C* Eseguo lo scan x trovare la fine del primo campo
028000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
028100030715     C* A questo punto "estraggo" il campo corrente
028200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
028300030715     C                                                    (posA-posDa))
028400030715     C* X i campi successivi al 1°
028500030715     C                   ELSE
028600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
028700030715     C                   EVAL      posDa = posA + 1
028800030715     C* Eseguo lo scan x trovare la fine del campo corrente
028900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
029000030715     C* Gestisco l'ultimo campo
029100030715     C                   IF        posA = *zeros
029200040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
029300030715     C* A questo punto "estraggo" il campo corrente
029400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
029500030715     C                                                    (posA-posDa))
029600030715     C                   LEAVE
029700030715     C                   ELSE
029800030715     C* A questo punto "estraggo" il campo corrente
029900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
030000030715     C                                                    (posA-posDa))
030100030715     C                   ENDIF
030200030715     C                   ENDIF
030300030715     C* Incremento il contatore d campo
030400030715     C                   EVAL      i = i +1
030500030715     C                   ENDDO
030600030715     C*
030700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
030800030715     C* delimitazione testo
030900141218     C                   EVAL      i = 1
031000141218     C                   DOW       i < %elem(SkSplitFLD)
031100141218     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
031200141218     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
031300141218     C                   EVAL      i = i + 1
031400141218     C                   ENDDO
031500141218     C*
031600030715     C                   EVAL      wGiro = 1
031700040709     C* Ripristino la 1° riga originale
031800040709     C                   EVAL      vinDTA = depvinDTA
031900030715     C                   ELSE
032000030715     C                   EVAL      wGiro = 2
032100031204     C**********
032200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
032300141218     C* d separatore campo CSV
032400031204     C**********
032500040709     C                   EVAL      vindta = %trim(vindta)
032600040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
032700040709     C                   EVAL      vindta = %subst(vindta:2)
032800040709     C                   ENDDO
032900040709     C*
033000040709     C                   Z-ADD     *zeros        lunghInput        4 0
033100040709     C                   EVAL      lunghInput = %len(%trim(vindta))
033200040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
033300040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
033400040709     C                   EVAL      lunghInput = %len(%trim(vindta))
033500040709     C                   ENDDO
033600141218     C**********
033700141218     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
033800141218     C* d separatore campo SEP
033900141218     C**********
034000141218     C                   EVAL      vindta = %trim(vindta)
034100141218     C                   DOW       %subst(vindta:1:1) = CharSEP                 * all'inizio
034200141218     C                   EVAL      vindta = %subst(vindta:2)
034300141218     C                   ENDDO
034400141218     C*
034500141218     C                   Z-ADD     *zeros        lunghInput        4 0
034600141218     C                   EVAL      lunghInput = %len(%trim(vindta))
034700141218     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharSEP
034800141218     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
034900141218     C                   EVAL      lunghInput = %len(%trim(vindta))
035000141218     C                   ENDDO
035100141218     C**********
035200141218     C* Normalizzo i dati d input in modo tale che il separatore campo sia sempre e counque il "|"
035300141218     C**********
035400141218     C*
035500141218     C                   EVAL      vindta = %scanrpl(CharSEP_E:CharCSV:vindta)
035600141218     C                   EVAL      vindta = %scanrpl(CharSEP_P:CharCSV:vindta)
035700141218     C                   EVAL      vindta = %scanrpl(CharSEP_D:CharCSV:vindta)
035800030715     C**********
035900030715     C* Effettuo lo split del campo dati d input
036000030715     C**********
036100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
036200040324     C                   DOW       posDa <= %len(%trimr(vindta))
036300030715     C*
036400030715     C* Gestisco il 1° campo
036500030715     C                   IF        i = 1
036600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
036700030715     C                   EVAL      posDa = 1
036800030715     C* Eseguo lo scan x trovare la fine del primo campo
036900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
037000030715     C* A questo punto "estraggo" il campo corrente
037100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
037200030715     C                                                    (posA-posDa))
037300030715     C* X i campi successivi al 1°
037400030715     C                   ELSE
037500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
037600030715     C                   EVAL      posDa = posA + 1
037700031204     C* Verifico che nn vi sia il campo nn valorizzato
037800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
037900031204     C* Se campo nn valorizzato skippo al prossimo
038000031204     C                   EVAL      SkSplitCSV(i) = *blanks
038100031204     C                   EVAL      posA  = posA + 1
038200031204     C                   ELSE
038300030715     C* Eseguo lo scan x trovare la fine del campo corrente
038400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
038500030715     C* Gestisco l'ultimo campo
038600030715     C                   IF        posA = *zeros
038700040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
038800030715     C* A questo punto "estraggo" il campo corrente
038900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
039000030715     C                                                    (posA-posDa))
039100030715     C                   LEAVE
039200030715     C                   ELSE
039300030715     C* A questo punto "estraggo" il campo corrente
039400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
039500030715     C                                                    (posA-posDa))
039600030715     C                   ENDIF
039700031204     C                   ENDIF
039800030715     C                   ENDIF
039900030715     C* Incremento il contatore d campo
040000030715     C                   EVAL      i = i +1
040100030715     C                   ENDDO
040200030715     C*
040300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
040400030715     C* delimitazione testo
040500030715     C                   EVAL      i = 1
040600030715     C                   DOW       i < %elem(SkSplitCSV)
040700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
040800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
040900030715     C                   EVAL      i = i + 1
041000030715     C                   ENDDO
041100030715     C*
041200030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
041300030715     C                   EVAL      i = 1
041400030715     C                   DOW       i < %elem(SkSplitFLD)
041500030715     C*
041600030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
041700030715     C                   IF        SkSplitFLD(i) <> *blanks AND
041800030715     C                             SkSplitCSV(i) <> *blanks
041900141209     C*** EML_FROM
042000141209     C                   IF        %trim(SkSplitFLD(i)) = 'EML_FROM'
042100141209     C                   EVAL      MATFROM  = SkSplitCSV(i)
042200030721     C                   ENDIF
042300141209     C*** EML_TO
042400141209     C                   IF        %trim(SkSplitFLD(i)) = 'EML_TO'
042500141209     C                   EVAL      MATTO    = SkSplitCSV(i)
042600141209     C                   ENDIF
042700141209     C*** EML_SUBJ
042800141209     C                   IF        %trim(SkSplitFLD(i)) = 'EML_SUBJ'
042900141209     C                   EVAL      MATSUBJ  = SkSplitCSV(i)
043000141209     C                   ENDIF
043100141209     C*** EML_SRV
043200141209     C                   IF        %trim(SkSplitFLD(i)) = 'EML_SRV'
043300141209     C                   EVAL      MATIDSRV = SkSplitCSV(i)
043400141209     C                   ENDIF
043500141209     C*** EML_STS
043600141209     C                   IF        %trim(SkSplitFLD(i)) = 'EML_STS'
043700141209     C                   EVAL      MATSTS   = SkSplitCSV(i)
043800141209     C                   ENDIF
043900141209     C*** EML_MSG
044000141209     C                   IF        %trim(SkSplitFLD(i)) = 'EML_MSG'
044100141209     C                   EVAL      MATMSG   = SkSplitCSV(i)
044200141209     C                   ENDIF
044300141211     C*** EML_UNIQ
044400141211     C                   IF        %trim(SkSplitFLD(i)) = 'EML_UNIQ'
044500141211     C                   EVAL      PiStr=SkSplitCSV(i)
044600141211     C                   EXSR      CHKNUM
044700141211     C                   IF        PiInt=*on
044800141212     C                   EVAL      MATUNIQ = PiVal
044900141211     C                   ELSE
045000141211     C                   SETON                                        31
045100141211     C                   EVAL      vinmsg = %trimr(vinmsg)
045200141211     C                             + ' ' + 'MATUNIQ'
045300141211     C                   ENDIF
045400141211     C                   ENDIF
045500141209     C*** EML_MINS
045600141209     C                   IF        %trim(SkSplitFLD(i)) = 'EML_MINS'
045700150126     C* prevedo la possibilità che la data si in formato YYYYMMDDhhmmss o YYYY-MM-DD hh:mm:ss
045800150126     C                   SELECT
045900150126     C                   WHEN      %len(%trim(SkSplitCSV(i))) = 14
046000150126     C                   EVAL      PiStr=%trim(SkSplitCSV(i))
046100150126     C                   EXSR      CHKNUM
046200150126     C                   IF        PiInt=*on
046300150126     C                   Z-ADD     PiVal         MATMOMINS
046400150417     C                   EVAL      MATAAS =
046500150417     C                             %dec(%subst(%editc(matmomins:'X'):1:6):6:0)
046600150126     C                   ELSE
046700150126     C                   SETON                                        32
046800150126     C                   EVAL      MATMOMINS = 1
046900150126     C                   EVAL      vinmsg = %trimr(vinmsg)
047000150417     C                             + ' ' + 'MATMOMINS MATAAS'
047100150126     C                   ENDIF
047200150126
047300150126     C                   WHEN      %len(%trim(SkSplitCSV(i))) = 19
047400150126     C                   EVAL      PiStr=%subst(%trim(SkSplitCSV(i)) : 1 : 4) +
047500150126     C                                   %subst(%trim(SkSplitCSV(i)) : 6 : 2) +
047600150126     C                                   %subst(%trim(SkSplitCSV(i)) : 9 : 2) +
047700150126     C                                   %subst(%trim(SkSplitCSV(i)) : 12 : 2) +
047800150126     C                                   %subst(%trim(SkSplitCSV(i)) : 15 : 2) +
047900150126     C                                   %subst(%trim(SkSplitCSV(i)) : 18 : 2)
048000150126     C                   EXSR      CHKNUM
048100150126     C                   IF        PiInt=*on
048200150126     C                   Z-ADD     PiVal         MATMOMINS
048300150417     C                   EVAL      MATAAS =
048400150417     C                             %dec(%subst(%editc(matmomins:'X'):1:6):6:0)
048500150126     C                   ELSE
048600150126     C                   SETON                                        32
048700150126     C                   EVAL      MATMOMINS = 1
048800150126     C                   EVAL      vinmsg = %trimr(vinmsg)
048900150417     C                             + ' ' + 'MATMOMINS MATAAS'
049000150126     C                   ENDIF
049100150126
049200150126     C                   OTHER
049300150126     C                   SETON                                        32
049400150126     C                   EVAL      MATMOMINS = 1
049500150126     C                   EVAL      vinmsg = %trimr(vinmsg)
049600150417     C                             + ' ' + 'MATMOMINS MATAAS'
049700150126     C                   ENDSL
049800150126
049900141209     C                   ENDIF
050000141209     C*** EML_MSTS
050100141209     C                   IF        %trim(SkSplitFLD(i)) = 'EML_MSTS'
050200150126     C* prevedo la possibilità che la data si in formato YYYYMMDDhhmmss o YYYY-MM-DD hh:mm:ss
050300150126     C                   SELECT
050400150126     C                   WHEN      %len(%trim(SkSplitCSV(i))) = 14
050500150126     C                   EVAL      PiStr=%trim(SkSplitCSV(i))
050600150126     C                   EXSR      CHKNUM
050700150126     C                   IF        PiInt=*on
050800150126     C                   Z-ADD     PiVal         MATMOMSTS
050900150126     C                   ELSE
051000150126     C                   SETON                                        32
051100150126     C                   EVAL      MATMOMSTS = 1
051200150126     C                   EVAL      vinmsg = %trimr(vinmsg)
051300150126     C                             + ' ' + 'MATMOMSTS'
051400150126     C                   ENDIF
051500150126
051600150126     C                   WHEN      %len(%trim(SkSplitCSV(i))) = 19
051700150126     C                   EVAL      PiStr=%subst(%trim(SkSplitCSV(i)) : 1 : 4) +
051800150126     C                                   %subst(%trim(SkSplitCSV(i)) : 6 : 2) +
051900150126     C                                   %subst(%trim(SkSplitCSV(i)) : 9 : 2) +
052000150126     C                                   %subst(%trim(SkSplitCSV(i)) : 12 : 2) +
052100150126     C                                   %subst(%trim(SkSplitCSV(i)) : 15 : 2) +
052200150126     C                                   %subst(%trim(SkSplitCSV(i)) : 18 : 2)
052300150126     C                   EXSR      CHKNUM
052400150126     C                   IF        PiInt=*on
052500150126     C                   Z-ADD     PiVal         MATMOMSTS
052600150126     C                   ELSE
052700150126     C                   SETON                                        32
052800150126     C                   EVAL      MATMOMSTS = 1
052900150126     C                   EVAL      vinmsg = %trimr(vinmsg)
053000150126     C                             + ' ' + 'MATMOMSTS'
053100150126     C                   ENDIF
053200150126
053300150126     C                   OTHER
053400150126     C                   SETON                                        32
053500150126     C                   EVAL      MATMOMSTS = 1
053600150126     C                   EVAL      vinmsg = %trimr(vinmsg)
053700150126     C                             + ' ' + 'MATMOMSTS'
053800150126     C                   ENDSL
053900150126
054000141209     C                   ENDIF
054100090428     C*
054200090428     C********** GESTIONE CAMPI PERSONALIZZATI
054300090428     C*
054400090428     C*
054500090428     C****************************************
054600090428     C*
054700030715     C*
054800030715     C                   ENDIF
054900030715     C                   EVAL      i = i + 1
055000030715     C                   ENDDO
055100040723     C*
055200141209     C* Effettuo considerazioni finali pre-scrittura dati
055300141209     C****
055400141209     C*
055500141209     C*
055600141209     C*
055700141209     C****
055800131108     C*
055900030715     C                   ENDIF
056000020204     C*
056100010607     C                   ENDSR
056200010607     C*----------------------------------------------------*
056300131108
056400131108
056500141209     C     exeSTSEML     BEGSR
056600131108     C*
056700141209     C* Verifico se trattasi di status per invii BartMAIL o BartVAS
056800141209     C                   z-add     *zeros        wPos1             4 0
056900141209     C                   z-add     *zeros        wPos2             4 0
057000141209     C*
057100141209     C* Verifico se log di BartMail
057200141212     C                   eval      wPos1 = %scan(' (ID':MATSUBJ:1)
057300141209     C                   eval      wPos2 = %scan(')':MATSUBJ:
057400141209     C                                           %len(%trim(MATSUBJ)))
057500141209     C                   if        wPos1 > *zeros AND wPos2 > *zeros AND
057600141210     C                             wPos1 + 11 = wPos2
057700141209     C                   eval      MATTRC     = 'BM1'
057800141210     C                   eval      MATPROG    = %subst(MATSUBJ:wPos1+4:7)
057900141209     C                   endif
058000141209     C*
058100141209     C* Verifico se log di BartVAS
058200141210     C                   eval      wPos1 = %scan(' (BV':MATSUBJ:1)
058300141209     C                   eval      wPos2 = %scan(')':MATSUBJ:
058400141209     C                                           %len(%trim(MATSUBJ)))
058500141209     C                   if        wPos1 > *zeros AND wPos2 > *zeros AND
058600141210     C                             wPos1 + 11 = wPos2
058700141209     C                   eval      MATTRC     = 'BV1'
058800141210     C                   eval      MATPROG    = %subst(MATSUBJ:wPos1+4:7)
058900141209     C                   endif
059000131108     C*
059100141211     C* Verifico esistenza ID Univoco Invio corrente
059200151216     C     KEYMAT01_C    chain     timat01i
059300141211     C                   eval      timat_01 = timat_00
059400141211     C                   if        %found(timat01i)
059500141211     C                   update    timat000
059600141211     C                   else
059700141209     C                   write     timat000
059800141211     C                   endif
059900131108     C*
060000131108     C                   ENDSR
060100131108     C***
060200141209
060300141209
060400141209
060500141209     C*----------------------------------------------------*
060600141209     C*  CONTROLLO NUMERICITA' CAMPI
060700141209     C*----------------------------------------------------*
060800141209     C     CHKNUM        BEGSR
060900141209     C*
061000141209     C                   IF        PiDecChr = *blanks
061100141209     C                   EVAL      PiDecChr = CharNUM
061200141209     C                   ENDIF
061300141209     C*
061400141209     C                   CALL(e)   'ISNUMERIC'
061500141209     C                   PARM                    PiStr            30
061600141209     C                   PARM                    PiDecChr          1
061700141209     C                   PARM      *ZEROS        PiVal            30 9
061800141209     C                   PARM      '0'           PiInt             1
061900141209     C                   PARM      '0'           PiNum             1
062000141209     C                   IF        %error
062100141209     C                   EVAL      PiNum=*off
062200141209     C                   ENDIF
062300141209     C*
062400141209     C                   ENDSR
062500141209     C***
062600141209
062700141209
062800010601
062900010601
063000010601      /TITLE Invio dei dati al punto operativo.
063100010601     C     opeini        BEGSR
063200010601     C*
063300010601     C* Inizializzo flag e contatori operativi
063400010601     C                   movel     '0'           flgGiro           1
063500010601     C*
063600010601     C                   ENDSR
063700010601     C***
063800050421
063900050421
064000050421     C     *pssr         BEGSR
064100050421     C*
064200050421     C                   eval      wrkesito = '2'
064300050421     C*
064400050421     C                   ENDSR     '*CANCL'
064500050421     C***
064600010330
064700010330
064800000613     C     *inzsr        BEGSR
064900990910     C*
065000990910     C     *entry        plist
065100990920     C                   parm                    tivlrds
065200990921     C                   parm      wrkesito      esito
065300000724     C                   parm                    prmlit
065400000710     C                   parm                    prmfir
065500151216     C*
065600151216     C* Definizione chiave esterna su TIMAT01I - Totale
065700151216     C     KEYMAT01_C    klist
065800151216     C                   kfld                    MATUNIQ
065900151216     C                   kfld                    MATIDSRV
066000131108     C*
066100131108     C                   ENDSR
066200000613     C***
066300040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
066400141218EML_UNIQ|EML_FROM|EML_TO|EML_SUBJ|EML_XXX|EML_SRV|EML_STS|
066500141218EML_MINS|EML_XXX|EML_MSTS|EML_XXX
066600131108
066700131108
