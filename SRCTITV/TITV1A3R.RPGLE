000100070920      /TITLE Upload via Internet: traduzione in PCGAD
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600070920     FPCGAD     O    E             DISK    usropn
000700030715     Ftitvxlp   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900000313     D*
001000000613     D*------------
001100000613     D* COMANDI
001200000613     D*------------
001300011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001400040726     D*------------
001500040726     D* SCHIEDE A PROGRAMMA
001600040726     D*------------
001700060215     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
001800010330     D*----------------------------------------------------
001900010330     D* DICHIARAZIOINE VARIABILI DI WRK
002000010330     D*----------------------------------------------------
002100010330     D psds           sds
002200010330     D  procname         *PROC
002300010330     D tivlrds       e ds                  extname(tivlr00f)
002400010330     D esito           s              1
002500010330     D prmlit          s             10
002600010330     D prmfir          s             10
002700010330     D wrkesito        s                   like(esito)
002800010330     D wrkdata         s               d
002900010330     D wrkora          s               t
003000010330     D rrnum           s              6  0 INZ(*zeros)
003100011113     D depcmd          s            150    INZ(*blanks)
003200060215     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
003300070920     D wF15            s             30    INZ(*blanks)
003400030715     D*------------------
003500030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003600030715     D*------------------
003700030715     D SkSplitFLD      S             10    DIM(100)
003800030715     D SkSplitCSV      S            256    DIM(100)
003900030715     D CharCSV         S              1
004000030715     D CharTXT         S              1
004100030715     D CharNUM         S              1
004200030715     D posDa           S              3  0 INZ(*zeros)
004300030715     D posA            S              3  0 INZ(*zeros)
004400030715     D i               s              3  0 INZ(1)
004500030715     D wGiro           s              1  0 INZ(*zeros)
004600030822     D*------------------
004700030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
004800030822     D*------------------
004900030822     D posDaDft        S              3  0 INZ(*zeros)
005000030822     D posADft         S              3  0 INZ(*zeros)
005100030822     D j               s              3  0 INZ(1)
005200030822     D wGiroDft        s              1  0 INZ(*zeros)
005300010330     D*------------------
005400010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
005500010330     D*------------------
005600010330     D WLBDA8          DS                  INZ
005700010330     D  G08DAT                 1      8  0
005800010330     D  G08INV                 9     16  0
005900010330     D  G08ERR                17     17
006000010330     D  G08TGI                18     22  0
006100031201     D*------------------
006200031201     D* Costanti
006300031201     D*------------------
006400031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006500031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006600010330
006700010330
006800010330
006900990915     C                   time                    wrkdata
007000990915     C                   time                    wrkora
007100000913     C                   reset                   rrnum
007200990921     C                   reset                   esito
007300990921     C                   reset                   wrkesito
007400010601     C*
007500010601     C                   exsr      opeini
007600070920     C                   exsr      rwgad
007700010601     C*
007800010601     C                   seton                                        lr
007900010601
008000010601
008100000613
008200000613     C*--------------------------------------------------------
008300000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
008400000613     C*--------------------------------------------------------
008500000621     C     STPR          BEGSR
008600000613     C*
008700030715     C                   if        not %open(titvxlp)
008800030715     C   50              open      titvxlp
008900990915     C                   endif
009000990915     C*
009100030715     C   50              except    riepilogo
009200990915     C*
009300030715     C                   if        %open(titvxlp)
009400030715     C   50              close     titvxlp
009500990914     C                   endif
009600000613     C*
009700000613     C                   ENDSR
009800000613     C***
009900010601
010000010601
010100010601
010200010330     C*--------------------------------------------------------
010300070920     C* RWGAD   LEGGE TIVIN00R E SCRIVE PCGAD                 *
010400010330     C*--------------------------------------------------------
010500070920     C     RWGAD         BEGSR
010600010330     C*
010700010330     C                   if        not %open(tivin00r)
010800010330     C                   open      tivin00r
010900010330     C                   endif
011000070920     C                   if        not %open(pcgad)
011100070920     C                   open      pcgad
011200010330     C                   endif
011300010330     C*
011400010604     C                   clear                   §CTROK
011500010604     C                   clear                   §CTRMO
011600010604     C                   clear                   §CTRNO
011700040726     C*
011800040726     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
011900070920     C* colonne nel file d input (che contiene quindi solo le righe d dati).
012000040726     C                   exsr      inzvar
012100070920     C                   exsr      impgad
012200010330     C*
012300030822     C                   do        *HIVAL
012400010330     C*
012500030822     C                   read      tivin00r                               70
012600010618     C*
012700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
012800010618     C                   if        vinflg = '1'
012900010618     C                   eval      flgOk = '1'
013000010618     C                   endif
013100060216     C*
013200060216     C* Se byte inferiore a HEX40 è potenzialmente probabile ricorra 1 errore nella visualizzazione
013300060216     C* => gestisco sempre
013400060216     C                   z-add     *zeros        wLenVINDTA        4 0
013500060216     C                   dow       wLenVINDTA < %len(%trim(vindta))
013600060216     C                   eval      wLenVINDTA = wLenVINDTA + 1
013700060216     C                   if        %subst(vindta:wLenVINDTA:1) < X'40'
013800060216     C                   eval      %subst(vindta:wLenVINDTA:1) = *blanks
013900060216     C                   endif
014000060216     C                   enddo
014100010618     C*
014200040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
014300040510     C                   z-add     *zeros        wLenVINDTA        4 0
014400040510     C                   z-add     *zeros        wFlgVINDTA        1 0
014500040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
014600040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
014700040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
014800040510     C                   z-add     1             wFlgVINDTA
014900040510     C                   leave
015000040510     C                   endif
015100040510     C                   enddo
015200040510     C*
015300040218     C                   if        vindta > *blanks AND
015400040510     C                             wFlgVINDTA = 1
015500010330     C                   add       1             rrnum
015600010330     C*
015700010601     C                   if        *in70 = *off and
015800010330     C                             (vinflg = *blanks
015900010330     C                              or vinflg = '0'
016000010330     C                              or vinflg = '2')
016100010330     C*
016200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
016300010711     C                   if        vinflg = *blanks or vinflg = '0'
016400010711     C                   clear                   vinmsg
016500010711     C                   endif
016600010601     C*
016700010330     C                   exsr      inzvar
016800070920     C                   exsr      impgad
016900010601     C*
017000010604     C* Ebbene...
017100010604     C*
017200030822     C  N31              add       1             §CTROK            7 0
017300030822     C   32              add       1             §CTRMO            7 0
017400030822     C   31              add       1             §CTRNO            7 0
017500030822     C                   if        wGiro = 2
017600070920     C  N31              write     PCGAD000
017700030822     C                   endif
017800010604     C*
017900010604     C                   if        *in31 = *off and
018000010604     C                             *in32 = *off
018100010604     C                   eval      vinflg = '1'
018200010604     C                   else
018300010604     C                   eval      vinflg = '2'
018400010604     C                   endif
018500010601     C*
018600010604     C                   endif
018700010604     C*
018800010330     C                   else
018900010330     C                   eval      vinflg = '1'
019000010330     C                   endif
019100010601     C*
019200010601     C  N70              update    tivin000
019300010330     C*
019400030822     C  N70              enddo
019500010601     C
019600010601     C*
019700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
019800010601     C                   if        cntNonEl = *zeros or
019900010601     C                             flgMulti = '0'
020000010330     C* Se non ci sono record con errori ...
020100010601     C                   if        §ctrno = 0 and
020200010604     C                             §ctrmo = 0 and
020300010601     C                             flgStato <> '2'
020400010330     C* ... restituisco esito OK.
020500010330     C                   eval      wrkesito = '0'
020600010330     C                   else
020700010330     C                   if        §ctrok > 0
020800010330     C                   eval      wrkesito = '1'
020900010330     C                   else
021000010615     C                   if        flgOk = '0'
021100010615     C                   eval      wrkesito = '2'
021200010615     C                   else
021300010615     C                   eval      wrkesito = '6'
021400010615     C                   endif
021500010330     C                   endif
021600010330     C                   endif
021700010601     C                   else
021800010601     C                   eval      wrkesito = '9'
021900010601     C                   endif
022000010330     C*
022100010330     C                   if        %open(tivin00r)
022200010330     C                   close     tivin00r
022300010330     C                   endif
022400070920     C                   if        %open(pcgad)
022500070920     C                   close     pcgad
022600010330     C                   endif
022700070920     C*
022800010330     C                   ENDSR
022900010330     C***
023000010601
023100010601
023200010601
023300010330     C*----------------------------------------------------*
023400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
023500010330     C*----------------------------------------------------*
023600010330     C     INZVAR        BEGSR
023700010330     C*
023800030822     C* Inizializzo variabili di wrk
023900020204     C                   Z-ADD     *zeros        Num5_0            5 0
024000030822     C*
024100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
024200070920     C                   CLEAR                   PCGAD000
024300030822     C                   CLEAR                   SkSplitCSV
024400030822     C                   EVAL      i = 1
024500030822     C                   EVAL      posDa = *zeros
024600030822     C                   EVAL      posA  = *zeros
024700030822     C*
024800030822     C* Reimposto i valori di default
024900030822     C                   EXSR      DEFCAM
025000030822     C*
025100010330     C                   ENDSR
025200010330     C*----------------------------------------------------*
025300030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
025400010330     C*----------------------------------------------------*
025500010330     C     DEFCAM        BEGSR
025600030822     C*
025700030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
025800030715     C* e delimitatore testo.
025900030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
026000030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
026100030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
026200030822     C*
026300030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
026400030715     C                   IF        %subst(vlrppt:1:1) = 'S'
026500030715     C                   SETON                                        50
026600030715     C                   ELSE
026700030715     C                   SETOFF                                       50
026800030715     C                   ENDIF
026900030822     C*
027000030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
027100030822     C                   EVAL      posDaDft = 1
027200030822     C                   EVAL      posADft  = 0
027300030822     C                   EVAL      wGiroDft = 0
027400030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
027500030822     C                             posDaDft > 0
027600030822     C*
027700030822     C* Gestisco il 1° giro
027800030822     C                   IF        wGiroDft = 0
027900030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
028000030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
028100030822     C* Incremento il contatore dei "giri"
028200030822     C                   EVAL      wGiroDft = 1
028300030822     C                   ELSE
028400030822     C                   EVAL      posDaDft = posADft
028500030822     C                   ENDIF
028600030822     C* Eseguo lo scan x trovare la fine del campo corrente
028700030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
028800030822     C*
028900030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
029000030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
029100030822     C                   IF        posDaDft > 0 AND
029200030822     C                             posADft  > 0
029300070920     C* F1
029400030822     C                   IF        %subst(
029500030822     C                             %subst(vlrppt:posDaDft+1:
029600030822     C                             posADft-posDaDft-1):1:3)
029700070920     C                             = 'F1'
029800070920     C                   EVAL      F1=%trim(%subst(
029900030822     C                             %subst(vlrppt:posDaDft+1:
030000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
030100030822     C                   ENDIF
030200070920     C* F2
030300070920     C                   IF        %subst(
030400070920     C                             %subst(vlrppt:posDaDft+1:
030500070920     C                             posADft-posDaDft-1):1:3)
030600070920     C                             = 'F2'
030700070920     C                   EVAL      F2=%trim(%subst(
030800070920     C                             %subst(vlrppt:posDaDft+1:
030900070920     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
031000070920     C                   ENDIF
031100070920     C* F3
031200070920     C                   IF        %subst(
031300070920     C                             %subst(vlrppt:posDaDft+1:
031400070920     C                             posADft-posDaDft-1):1:3)
031500070920     C                             = 'F3'
031600070920     C                   EVAL      F3=%trim(%subst(
031700070920     C                             %subst(vlrppt:posDaDft+1:
031800070920     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
031900070920     C                   ENDIF
032000070920     C* F4
032100070920     C                   IF        %subst(
032200070920     C                             %subst(vlrppt:posDaDft+1:
032300070920     C                             posADft-posDaDft-1):1:3)
032400070920     C                             = 'F4'
032500070920     C                   EVAL      F4=%trim(%subst(
032600070920     C                             %subst(vlrppt:posDaDft+1:
032700070920     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
032800070920     C                   ENDIF
032900070920     C* F5
033000070920     C                   IF        %subst(
033100070920     C                             %subst(vlrppt:posDaDft+1:
033200070920     C                             posADft-posDaDft-1):1:3)
033300070920     C                             = 'F5'
033400070920     C                   EVAL      F5=%trim(%subst(
033500070920     C                             %subst(vlrppt:posDaDft+1:
033600070920     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
033700070920     C                   ENDIF
033800070920     C* F6
033900070920     C                   IF        %subst(
034000070920     C                             %subst(vlrppt:posDaDft+1:
034100070920     C                             posADft-posDaDft-1):1:3)
034200070920     C                             = 'F6'
034300070920     C                   EVAL      F6=%trim(%subst(
034400070920     C                             %subst(vlrppt:posDaDft+1:
034500070920     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
034600070920     C                   ENDIF
034700070920     C* F7
034800070920     C                   IF        %subst(
034900070920     C                             %subst(vlrppt:posDaDft+1:
035000070920     C                             posADft-posDaDft-1):1:3)
035100070920     C                             = 'F7'
035200070920     C                   EVAL      F7=%trim(%subst(
035300070920     C                             %subst(vlrppt:posDaDft+1:
035400070920     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
035500070920     C                   ENDIF
035600070920     C* F8
035700070920     C                   IF        %subst(
035800070920     C                             %subst(vlrppt:posDaDft+1:
035900070920     C                             posADft-posDaDft-1):1:3)
036000070920     C                             = 'F8'
036100070920     C                   EVAL      F8=%trim(%subst(
036200070920     C                             %subst(vlrppt:posDaDft+1:
036300070920     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036400070920     C                   ENDIF
036500070920     C* F9
036600070920     C                   IF        %subst(
036700070920     C                             %subst(vlrppt:posDaDft+1:
036800070920     C                             posADft-posDaDft-1):1:3)
036900070920     C                             = 'F9'
037000070920     C                   EVAL      F9=%trim(%subst(
037100070920     C                             %subst(vlrppt:posDaDft+1:
037200070920     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037300070920     C                   ENDIF
037400070920     C* F10
037500070920     C                   IF        %subst(
037600070920     C                             %subst(vlrppt:posDaDft+1:
037700070920     C                             posADft-posDaDft-1):1:3)
037800070920     C                             = 'F10'
037900070920     C                   EVAL      F10=%trim(%subst(
038000070920     C                             %subst(vlrppt:posDaDft+1:
038100070920     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
038200070920     C                   ENDIF
038300070920     C* F11
038400070920     C                   IF        %subst(
038500070920     C                             %subst(vlrppt:posDaDft+1:
038600070920     C                             posADft-posDaDft-1):1:3)
038700070920     C                             = 'F11'
038800070920     C                   EVAL      F11=%trim(%subst(
038900070920     C                             %subst(vlrppt:posDaDft+1:
039000070920     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039100070920     C                   ENDIF
039200070920     C* F12
039300070920     C                   IF        %subst(
039400070920     C                             %subst(vlrppt:posDaDft+1:
039500070920     C                             posADft-posDaDft-1):1:3)
039600070920     C                             = 'F12'
039700070920     C                   EVAL      F12=%trim(%subst(
039800070920     C                             %subst(vlrppt:posDaDft+1:
039900070920     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040000070920     C                   ENDIF
040100070920     C* F13
040200070920     C                   IF        %subst(
040300070920     C                             %subst(vlrppt:posDaDft+1:
040400070920     C                             posADft-posDaDft-1):1:3)
040500070920     C                             = 'F13'
040600070920     C                   EVAL      F13=%trim(%subst(
040700070920     C                             %subst(vlrppt:posDaDft+1:
040800070920     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040900070920     C                   ENDIF
041000070920     C* F14
041100070920     C                   IF        %subst(
041200070920     C                             %subst(vlrppt:posDaDft+1:
041300070920     C                             posADft-posDaDft-1):1:3)
041400070920     C                             = 'F14'
041500070920     C                   EVAL      F14=%trim(%subst(
041600070920     C                             %subst(vlrppt:posDaDft+1:
041700070920     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041800070920     C                   ENDIF
041900070920     C* F15
042000070920     C                   IF        %subst(
042100070920     C                             %subst(vlrppt:posDaDft+1:
042200070920     C                             posADft-posDaDft-1):1:3)
042300070920     C                             = 'F15'
042400070920     C                   EVAL      F15=%trim(%subst(
042500070920     C                             %subst(vlrppt:posDaDft+1:
042600070920     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042700070920     C                   ENDIF
042800070920     C* F16
042900070920     C                   IF        %subst(
043000070920     C                             %subst(vlrppt:posDaDft+1:
043100070920     C                             posADft-posDaDft-1):1:3)
043200070920     C                             = 'F16'
043300070920     C                   EVAL      F16=%trim(%subst(
043400070920     C                             %subst(vlrppt:posDaDft+1:
043500070920     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043600070920     C                   ENDIF
043700070920     C* F17
043800070920     C                   IF        %subst(
043900070920     C                             %subst(vlrppt:posDaDft+1:
044000070920     C                             posADft-posDaDft-1):1:3)
044100070920     C                             = 'F17'
044200070920     C                   EVAL      F17=%trim(%subst(
044300070920     C                             %subst(vlrppt:posDaDft+1:
044400070920     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044500070920     C                   ENDIF
044600070920     C* F18
044700070920     C                   IF        %subst(
044800070920     C                             %subst(vlrppt:posDaDft+1:
044900070920     C                             posADft-posDaDft-1):1:3)
045000070920     C                             = 'F18'
045100070920     C                   EVAL      F18=%trim(%subst(
045200070920     C                             %subst(vlrppt:posDaDft+1:
045300070920     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045400070920     C                   ENDIF
045500070920     C* F19
045600070920     C                   IF        %subst(
045700070920     C                             %subst(vlrppt:posDaDft+1:
045800070920     C                             posADft-posDaDft-1):1:3)
045900070920     C                             = 'F19'
046000070920     C                   EVAL      F19=%trim(%subst(
046100070920     C                             %subst(vlrppt:posDaDft+1:
046200070920     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046300070920     C                   ENDIF
046400070920     C* F20
046500070920     C                   IF        %subst(
046600070920     C                             %subst(vlrppt:posDaDft+1:
046700070920     C                             posADft-posDaDft-1):1:3)
046800070920     C                             = 'F20'
046900070920     C                   EVAL      F20=%trim(%subst(
047000070920     C                             %subst(vlrppt:posDaDft+1:
047100070920     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047200070920     C                   ENDIF
047300070920     C* F21
047400070920     C                   IF        %subst(
047500070920     C                             %subst(vlrppt:posDaDft+1:
047600070920     C                             posADft-posDaDft-1):1:3)
047700070920     C                             = 'F21'
047800070920     C                   EVAL      F21=%trim(%subst(
047900070920     C                             %subst(vlrppt:posDaDft+1:
048000070920     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048100070920     C                   ENDIF
048200070920     C* F22
048300070920     C                   IF        %subst(
048400070920     C                             %subst(vlrppt:posDaDft+1:
048500070920     C                             posADft-posDaDft-1):1:3)
048600070920     C                             = 'F22'
048700070920     C                   EVAL      F22=%trim(%subst(
048800070920     C                             %subst(vlrppt:posDaDft+1:
048900070920     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049000070920     C                   ENDIF
049100070920     C* F23
049200070920     C                   IF        %subst(
049300070920     C                             %subst(vlrppt:posDaDft+1:
049400070920     C                             posADft-posDaDft-1):1:3)
049500070920     C                             = 'F23'
049600070920     C                   EVAL      F23=%trim(%subst(
049700070920     C                             %subst(vlrppt:posDaDft+1:
049800070920     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049900070920     C                   ENDIF
050000070920     C* F24
050100070920     C                   IF        %subst(
050200070920     C                             %subst(vlrppt:posDaDft+1:
050300070920     C                             posADft-posDaDft-1):1:3)
050400070920     C                             = 'F24'
050500070920     C                   EVAL      F24=%trim(%subst(
050600070920     C                             %subst(vlrppt:posDaDft+1:
050700070920     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050800070920     C                   ENDIF
050900070920     C* F25
051000070920     C                   IF        %subst(
051100070920     C                             %subst(vlrppt:posDaDft+1:
051200070920     C                             posADft-posDaDft-1):1:3)
051300070920     C                             = 'F25'
051400070920     C                   EVAL      F25=%trim(%subst(
051500070920     C                             %subst(vlrppt:posDaDft+1:
051600070920     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051700070920     C                   ENDIF
051800030822     C* ...
051900030822     C                   ENDIF
052000030822     C                   ENDDO
052100020204     C*
052200010330     C                   ENDSR
052300010607     C*----------------------------------------------------*
052400070920     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X PCGAD)
052500010607     C*----------------------------------------------------*
052600070920     C     IMPGAD        BEGSR
052700010607     C*
052800010607     C                   SETOFF                                       3132
052900050321     C*
053000050512     C* 1° Giro x eliminare il separatore testo
053100060215     C                   IF        CharTXT <> *blanks
053200050321     C                   Z-ADD     1             wj                5 0
053300050321     C                   DOW       wj <= %len(vindta) - 1
053400050321     C                   IF        %subst(vindta:wj:1) = CharTXT
053500050512     C                   EVAL      vindta = %subst(vindta:1:wj-1) +
053600050512     C                                      %subst(vindta:wj+1)
053700050512     C                   EVAL      wj = wj - 1
053800050512     C*                  EVAL      %subst(vindta:wj:1) = *blanks
053900050321     C                   ENDIF
054000050321     C                   ADD       1             wj
054100050321     C                   ENDDO
054200060215     C                   ENDIF
054300050512     C*
054400060215     C* Se la stringa dati da splittare inizia con il carattere separatore campo aggiungo in testa
054500050512     C* 2 byte a blank
054600050512     C                   IF        %subst(vindta:1:1) = CharCSV
054700050512     C                   EVAL      vindta = '  ' + vindta
054800050512     C                   ENDIF
054900010607     C*
055000030715     C**********
055100070920     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne file in
055200030715     C**********
055300030715     C                   IF        wGiro = *zeros
055400040726     C*
055500040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
055600040726     C                   EVAL      depvinDTA = vinDTA
055700040726     C*
055800040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
055900040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
056000050321     C                                      %trim(intcol(3))+%trim(intcol(4))+
056100060215     C                                      %trim(intcol(5))
056200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
056300031201     C     minu:maiu     XLATE     vindta        vindta
056400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
056500040324     C                   DOW       posDa <= %len(%trimr(vindta))
056600030715     C*
056700030715     C* Gestisco il 1° campo
056800030715     C                   IF        i = 1
056900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
057000030715     C                   EVAL      posDa = 1
057100030715     C* Eseguo lo scan x trovare la fine del primo campo
057200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
057300030715     C* A questo punto "estraggo" il campo corrente
057400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
057500030715     C                                                    (posA-posDa))
057600030715     C* X i campi successivi al 1°
057700030715     C                   ELSE
057800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
057900030715     C                   EVAL      posDa = posA + 1
058000030715     C* Eseguo lo scan x trovare la fine del campo corrente
058100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
058200030715     C* Gestisco l'ultimo campo
058300030715     C                   IF        posA = *zeros
058400040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
058500030715     C* A questo punto "estraggo" il campo corrente
058600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
058700030715     C                                                    (posA-posDa))
058800030715     C                   LEAVE
058900030715     C                   ELSE
059000030715     C* A questo punto "estraggo" il campo corrente
059100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
059200030715     C                                                    (posA-posDa))
059300030715     C                   ENDIF
059400030715     C                   ENDIF
059500030715     C* Incremento il contatore d campo
059600030715     C                   EVAL      i = i +1
059700030715     C                   ENDDO
059800030715     C*
059900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
060000030715     C* delimitazione testo
060100030715     C                   EVAL      i = 1
060200030715     C                   DOW       i < %elem(SkSplitFLD)
060300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
060400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
060500030715     C                   EVAL      i = i + 1
060600030715     C                   ENDDO
060700030715     C                   EVAL      wGiro = 1
060800040726     C* Ripristino la 1° riga originale
060900040726     C                   EVAL      vinDTA = depvinDTA
061000030715     C                   ELSE
061100030715     C                   EVAL      wGiro = 2
061200031204     C**********
061300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
061400031204     C* d separatore campo
061500031204     C**********
061600060215     C                   EVAL      vindta = %trim(vindta)
061700060215     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
061800060215     C                   EVAL      vindta = %subst(vindta:2)
061900060215     C                   ENDDO
062000060215     C
062100060215     C                   Z-ADD     *zeros        lunghInput        4 0
062200060215     C                   EVAL      lunghInput = %len(%trimr(vindta))
062300060215     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
062400060215     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
062500060215     C                   EVAL      lunghInput = %len(%trim(vindta))
062600060215     C                   ENDDO
062700030715     C**********
062800030715     C* Effettuo lo split del campo dati d input
062900030715     C**********
063000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
063100040324     C                   DOW       posDa <= %len(%trimr(vindta))
063200030715     C*
063300030715     C* Gestisco il 1° campo
063400030715     C                   IF        i = 1
063500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
063600030715     C                   EVAL      posDa = 1
063700030715     C* Eseguo lo scan x trovare la fine del primo campo
063800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
063900030715     C* A questo punto "estraggo" il campo corrente
064000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
064100030715     C                                                    (posA-posDa))
064200030715     C* X i campi successivi al 1°
064300030715     C                   ELSE
064400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
064500030715     C                   EVAL      posDa = posA + 1
064600031204     C* Verifico che nn vi sia il campo nn valorizzato
064700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
064800031204     C* Se campo nn valorizzato skippo al prossimo
064900031204     C                   EVAL      SkSplitCSV(i) = *blanks
065000031204     C                   EVAL      posA  = posA + 1
065100031204     C                   ELSE
065200030715     C* Eseguo lo scan x trovare la fine del campo corrente
065300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
065400030715     C* Gestisco l'ultimo campo
065500030715     C                   IF        posA = *zeros
065600040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
065700030715     C* A questo punto "estraggo" il campo corrente
065800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
065900030715     C                                                    (posA-posDa))
066000030715     C                   LEAVE
066100030715     C                   ELSE
066200030715     C* A questo punto "estraggo" il campo corrente
066300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
066400030715     C                                                    (posA-posDa))
066500030715     C                   ENDIF
066600031204     C                   ENDIF
066700030715     C                   ENDIF
066800030715     C* Incremento il contatore d campo
066900030715     C                   EVAL      i = i +1
067000030715     C                   ENDDO
067100030715     C*
067200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
067300030715     C* delimitazione testo
067400030715     C                   EVAL      i = 1
067500030715     C                   DOW       i < %elem(SkSplitCSV)
067600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
067700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
067800030715     C                   EVAL      i = i + 1
067900030715     C                   ENDDO
068000030715     C*
068100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
068200030715     C                   EVAL      i = 1
068300030715     C                   DOW       i < %elem(SkSplitFLD)
068400030715     C*
068500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
068600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
068700030715     C                             SkSplitCSV(i) <> *blanks
068800070920     C*** F1
068900070920     C                   IF        %trim(SkSplitFLD(i)) = 'F1'
069000070926     C                   EVAL      F1 = %trim(SkSplitCSV(i))
069100030721     C                   ENDIF
069200070920     C*** F2
069300070920     C                   IF        %trim(SkSplitFLD(i)) = 'F2'
069400070926     C                   EVAL      F2 = %trim(SkSplitCSV(i))
069500070920     C                   ENDIF
069600070920     C*** F3
069700070920     C                   IF        %trim(SkSplitFLD(i)) = 'F3'
069800070926     C                   EVAL      F3 = %trim(SkSplitCSV(i))
069900070920     C                   ENDIF
070000070920     C*** F4
070100070920     C                   IF        %trim(SkSplitFLD(i)) = 'F4'
070200070926     C                   EVAL      F4 = %trim(SkSplitCSV(i))
070300070920     C                   ENDIF
070400070920     C*** F5
070500070920     C                   IF        %trim(SkSplitFLD(i)) = 'F5'
070600070926     C                   EVAL      F5 = %trim(SkSplitCSV(i))
070700070920     C                   ENDIF
070800070920     C*** F6
070900070920     C                   IF        %trim(SkSplitFLD(i)) = 'F6'
071000070926     C                   EVAL      F6 = %trim(SkSplitCSV(i))
071100070920     C                   ENDIF
071200070920     C*** F7
071300070920     C                   IF        %trim(SkSplitFLD(i)) = 'F7'
071400070926     C                   EVAL      F7 = %trim(SkSplitCSV(i))
071500070920     C                   ENDIF
071600070920     C*** F8
071700070920     C                   IF        %trim(SkSplitFLD(i)) = 'F8'
071800070926     C                   EVAL      F8 = %trim(SkSplitCSV(i))
071900070920     C                   ENDIF
072000070920     C*** F9
072100070920     C                   IF        %trim(SkSplitFLD(i)) = 'F9'
072200070926     C                   EVAL      F9 = %trim(SkSplitCSV(i))
072300070920     C                   ENDIF
072400070920     C*** F10
072500070920     C                   IF        %trim(SkSplitFLD(i)) = 'F10'
072600070926     C                   EVAL      F10 = %trim(SkSplitCSV(i))
072700070920     C                   ENDIF
072800070920     C*** F11
072900070920     C                   IF        %trim(SkSplitFLD(i)) = 'F11'
073000070926     C                   EVAL      F11 = %trim(SkSplitCSV(i))
073100070920     C                   ENDIF
073200070920     C*** F12
073300070920     C                   IF        %trim(SkSplitFLD(i)) = 'F12'
073400070926     C                   EVAL      F12 = %trim(SkSplitCSV(i))
073500070920     C                   ENDIF
073600070920     C*** F13
073700070920     C                   IF        %trim(SkSplitFLD(i)) = 'F13'
073800070926     C                   EVAL      F13 = %trim(SkSplitCSV(i))
073900070920     C                   ENDIF
074000070920     C*** F14
074100070920     C                   IF        %trim(SkSplitFLD(i)) = 'F14'
074200070926     C                   EVAL      F14 = %trim(SkSplitCSV(i))
074300070920     C                   ENDIF
074400070920     C*** F15
074500070920     C                   IF        %trim(SkSplitFLD(i)) = 'F15'
074600070926     C                   EVAL      F15 = %trim(SkSplitCSV(i))
074700070920     C                   EVAL      wF15 = %trim(F15)
074800070920     C                   ENDIF
074900070920     C*** F16
075000070920     C                   IF        %trim(SkSplitFLD(i)) = 'F16'
075100070926     C                   EVAL      F16 = %trim(SkSplitCSV(i))
075200070920     C                   ENDIF
075300070920     C*** F17
075400070920     C                   IF        %trim(SkSplitFLD(i)) = 'F17'
075500070926     C                   EVAL      F17 = %trim(SkSplitCSV(i))
075600070920     C                   ENDIF
075700070920     C*** F18
075800070920     C                   IF        %trim(SkSplitFLD(i)) = 'F18'
075900070926     C                   EVAL      F18 = %trim(SkSplitCSV(i))
076000070920     C                   ENDIF
076100070920     C*** F19
076200070920     C                   IF        %trim(SkSplitFLD(i)) = 'F19'
076300070926     C                   EVAL      F19 = %trim(SkSplitCSV(i))
076400070920     C                   ENDIF
076500070920     C*** F20
076600070920     C                   IF        %trim(SkSplitFLD(i)) = 'F20'
076700070926     C                   EVAL      F20 = %trim(SkSplitCSV(i))
076800070920     C                   ENDIF
076900070920     C*** F21
077000070920     C                   IF        %trim(SkSplitFLD(i)) = 'F21'
077100070926     C                   EVAL      F21 = %trim(SkSplitCSV(i))
077200070920     C                   ENDIF
077300070920     C*** F22
077400070920     C                   IF        %trim(SkSplitFLD(i)) = 'F22'
077500070926     C                   EVAL      F22 = %trim(SkSplitCSV(i))
077600070920     C                   ENDIF
077700070920     C*** F23
077800070920     C                   IF        %trim(SkSplitFLD(i)) = 'F23'
077900070926     C                   EVAL      F23 = %trim(SkSplitCSV(i))
078000070920     C                   ENDIF
078100070920     C*** F24
078200070920     C                   IF        %trim(SkSplitFLD(i)) = 'F24'
078300070926     C                   EVAL      F24 = %trim(SkSplitCSV(i))
078400070920     C                   ENDIF
078500070920     C*** F25
078600070920     C                   IF        %trim(SkSplitFLD(i)) = 'F25'
078700070926     C                   EVAL      F25 = %trim(SkSplitCSV(i))
078800070920     C                   ENDIF
078900030715     C*
079000030715     C                   ENDIF
079100030715     C                   EVAL      i = i + 1
079200030715     C                   ENDDO
079300070713     C*
079400030715     C                   ENDIF
079500020204     C*
079600010607     C                   ENDSR
079700010607     C*----------------------------------------------------*
079800020204
079900010330
080000010330
080100010330     C*----------------------------------------------------*
080200010330     C*  CONTROLLO NUMERICITA' CAMPI
080300010330     C*----------------------------------------------------*
080400010330     C     CHKNUM        BEGSR
080500010330     C*
080600010606     C                   IF        PiDecChr = *blanks
080700030715     C                   EVAL      PiDecChr = CharNUM
080800010606     C                   ENDIF
080900010606     C*
081000010606     C                   CALL(e)   'ISNUMERIC'
081100010330     C                   PARM                    PiStr            30
081200010606     C                   PARM                    PiDecChr          1
081300010330     C                   PARM      *ZEROS        PiVal            30 9
081400010330     C                   PARM      '0'           PiInt             1
081500010330     C                   PARM      '0'           PiNum             1
081600010330     C                   IF        %error
081700010606     C                   EVAL      PiNum=*off
081800010330     C                   ENDIF
081900010330     C*
082000010330     C                   ENDSR
082100010330     C***
082200990910
082300010601
082400010601
082500010601
082600010601
082700010601      /TITLE Invio dei dati al punto operativo.
082800010601     C     opeini        BEGSR
082900010601     C*
083000010601     C* Inizializzo flag e contatori operativi
083100010601     C                   movel     '0'           flgGiro           1
083200010601     C                   movel     '0'           flgMulti          1
083300010601     C                   movel     '1'           flgStato          1
083400010615     C                   movel     '0'           flgOk             1
083500010601     C                   z-add     *zeros        cntNonEl         10 0
083600010601     C*
083700010601     C                   ENDSR
083800010601     C***
083900010601
084000010601
084100050421
084200050421
084300050421     C     *pssr         BEGSR
084400050421     C*
084500050421     C                   eval      wrkesito = '2'
084600050421     C*
084700050421     C                   ENDSR     '*CANCL'
084800050421     C***
084900050421
085000010601
085100010330
085200010330
085300000613     C     *inzsr        BEGSR
085400990910     C*
085500990910     C     *entry        plist
085600990920     C                   parm                    tivlrds
085700990921     C                   parm      wrkesito      esito
085800000724     C                   parm                    prmlit
085900000710     C                   parm                    prmfir
086000010330     C*
086100010330     C* CALCOLA LA DATA CORRENTE
086200010330     C                   time                    wn14             14 0
086300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
086400010330     C                   z-add     wn8           g08dat
086500010330     C                   z-add     *zeros        g08inv
086600010330     C                   movel     '0'           g08err
086700010330     C                   call      'XSRDA8'
086800010330     C                   parm                    wlbda8
086900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
087000000613     C*
087100000613     C                   ENDSR
087200000613     C***
087300990908
087400030715     Otitvxlp   E            riepilogo         2
087500990915     O                                              'Upload via Internet'
087600990915     O                                           +1 'Traduzione TIVIN00R -'
087700070920     O                                           +1 'PCGAD'
087800990915     O          E            riepilogo   2
087900990915     O                       wrkdata
088000990915     O                       wrkora              +1
088100990915     O                       procname            +1
088200990915     O          E            riepilogo   2
088300070920     O                                              'Dipendente ..............:'
088400070920     O                       wF15                +1
088500990915     O          E            riepilogo   2
088600990920     O                                              'Riferimento Strategi.....:'
088700990920     O                       vlrhdl              +1
088800990915     O          E            riepilogo   2
088900990915     O                                              'Giusti...................:'
089000971022     O                       §CTROK        2   +  1
089100990915     O          E            riepilogo   2
089200990915     O                                              'Sbagliati e corretti.....:'
089300971022     O                       §CTRMO        2   +  1
089400990915     O          E            riepilogo   2
089500990915     O                                              'Sbagliati e scartati.....:'
089600971022     O                       §CTRNO        2   +  1
089700000613
089800000613** CMD - COMANDI CL
089900070920OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(PGCAD) OUTQ(
090000070920
090100070920DLTOVR FILE(TITVXLP) LVL(*)
090200010606
090300000613
090400040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
090500070920F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,
090600070920F21,F22,F23,F24,F25
090700070713
090800050321
090900060215
