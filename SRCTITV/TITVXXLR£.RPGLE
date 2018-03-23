000100080729      /TITLE Upload via Internet: traduzione in FIVAXWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600080729     FFIVAXwwr  O    E             DISK    usropn
000700030715     Ftitvxlp   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900030715     Ftitvxlps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600010330     D*----------------------------------------------------
001700010330     D* DICHIARAZIOINE VARIABILI DI WRK
001800010330     D*----------------------------------------------------
001900010330     D dscmz         e ds                  inz
002000010330     D psds           sds
002100010330     D  procname         *PROC
002200010330     D tivlrds       e ds                  extname(tivlr00f)
002300010330     D esito           s              1
002400010330     D prmlit          s             10
002500010330     D prmfir          s             10
002600010330     D wrkesito        s                   like(esito)
002700010330     D wrkdata         s               d
002800010330     D wrkora          s               t
002900010330     D rrnum           s              6  0 INZ(*zeros)
003000010330     D recko           s            150    INZ(*blanks)
003100011113     D depcmd          s            150    INZ(*blanks)
003200030715     D*------------------
003300030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003400030715     D*------------------
003500030715     D SkSplitFLD      S             10    DIM(100)
003600030715     D SkSplitCSV      S            256    DIM(100)
003700030715     D CharCSV         S              1
003800030715     D CharTXT         S              1
003900030715     D CharNUM         S              1
004000090403     D CharSOS         S              1
004100030715     D posDa           S              3  0 INZ(*zeros)
004200030715     D posA            S              3  0 INZ(*zeros)
004300030715     D i               s              3  0 INZ(1)
004400030715     D wGiro           s              1  0 INZ(*zeros)
004500030822     D*------------------
004600030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
004700030822     D*------------------
004800030822     D posDaDft        S              3  0 INZ(*zeros)
004900030822     D posADft         S              3  0 INZ(*zeros)
005000030822     D j               s              3  0 INZ(1)
005100030822     D wGiroDft        s              1  0 INZ(*zeros)
005200010330     D*------------------
005300010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
005400010330     D*------------------
005500010330     D WLBDA8          DS                  INZ
005600010330     D  G08DAT                 1      8  0
005700010330     D  G08INV                 9     16  0
005800010330     D  G08ERR                17     17
005900010330     D  G08TGI                18     22  0
006000031201     D*------------------
006100031201     D* Costanti
006200031201     D*------------------
006300031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006400031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006500010330
006600010330
006700010330
006800990915     C                   time                    wrkdata
006900990915     C                   time                    wrkora
007000000913     C                   reset                   rrnum
007100990921     C                   reset                   esito
007200990921     C                   reset                   wrkesito
007300010601     C*
007400010601     C                   exsr      opeini
007500080729     C                   exsr      rwvax
007600010601     C*
007700010601     C                   seton                                        lr
007800010601
007900010601
008000010601
008100010601
008200010601     C*--------------------------------------------------------
008300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008400010601     C*--------------------------------------------------------
008500010601     C     PREELA        BEGSR
008600010601     C*
008700010601     C* SE OCCORRE SPEDIRE IN FILIALE
008800010601     C                   if        invfil <> *zeros and
008900010601     C                             flgGiro = '0'
009000010601     C*
009100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009200010601     C                   eval      flgGiro = '1'
009300000724     C*
009400000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009500000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009600010601     C     invfil        chain     azorg01l
009700000724     C                   if        %found
009800000724     C                   movel(p)  CMD(1)        depcmd
009900020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010000000724     C*
010100000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010200011113     C                   Z-ADD     150           LENGH            15 5
010300000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010400000724     C                   PARM                    depcmd
010500000724     C                   PARM                    LENGH
010600000724     C*
010700000724     C                   endif
010800000616     C*
010900000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011000000616     C                   movel(p)  CMD(2)        depcmd
011100000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011200000616     C*
011300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
011400011113     C                   Z-ADD     150           LENGH            15 5
011500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011600000616     C                   PARM                    depcmd
011700000616     C                   PARM                    LENGH
011800000613     C*
011900030715     C                   if        not %open(titvxlps)
012000030715     C                   open      titvxlps
012100030715     C                   except    testdett
012200000613     C                   endif
012300010601     C*
012400010601     C                   endif
012500010601     C*
012600010601     C                   ENDSR
012700010601     C***
012800010601
012900010601
013000010601
013100010601
013200010601
013300010601
013400010601
013500010601     C*--------------------------------------------------------
013600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013700010601     C*--------------------------------------------------------
013800010601     C     ENDELA        BEGSR
013900990915     C*
014000000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
014100000613     C*
014200030715     C                   if        %open(titvxlps)
014300000616     C                   except    findett
014400030715     C                   close     titvxlps
014500000613     C                   endif
014600000616     C*
014700000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
014800011113     C                   Z-ADD     150           LENGH            15 5
014900010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
015000000616     C                   PARM                    CMD(3)
015100000616     C                   PARM                    LENGH
015200000616     C*
015300010601     C                   ENDSR
015400010601     C***
015500010601
015600010601
015700010601
015800010601
015900000613
016000000613     C*--------------------------------------------------------
016100000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
016200000613     C*--------------------------------------------------------
016300000621     C     STPR          BEGSR
016400000613     C*
016500030715     C                   if        not %open(titvxlp)
016600030715     C   50              open      titvxlp
016700990915     C                   endif
016800990915     C*
016900030715     C   50              except    riepilogo
017000990915     C*
017100030715     C                   if        %open(titvxlp)
017200030715     C   50              close     titvxlp
017300990914     C                   endif
017400000613     C*
017500000613     C                   ENDSR
017600000613     C***
017700000613
017800010330
017900010601
018000010601
018100010601
018200010330     C*--------------------------------------------------------
018300080729     C* RWVAX   LEGGE TIVIN00R E SCRIVE FIVAXWWF              *
018400010330     C*--------------------------------------------------------
018500080729     C     RWVAX         BEGSR
018600010330     C*
018700010330     C                   if        not %open(tivin00r)
018800010330     C                   open      tivin00r
018900010330     C                   endif
019000080729     C                   if        not %open(fivaxwwr)
019100080729     C                   open      fivaxwwr
019200010330     C                   endif
019300010330     C*
019400010604     C                   clear                   §CTROK
019500010604     C                   clear                   §CTRMO
019600010604     C                   clear                   §CTRNO
019700010330     C*
019800030822     C                   do        *HIVAL
019900010330     C*
020000030822     C                   read      tivin00r                               70
020100010618     C*
020200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020300010618     C                   if        vinflg = '1'
020400010618     C                   eval      flgOk = '1'
020500010618     C                   endif
020600010618     C*
020700040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
020800040510     C                   z-add     *zeros        wLenVINDTA        4 0
020900040510     C                   z-add     *zeros        wFlgVINDTA        1 0
021000040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
021100040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
021200040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
021300040510     C                   z-add     1             wFlgVINDTA
021400040510     C                   leave
021500040510     C                   endif
021600040510     C                   enddo
021700040510     C*
021800040218     C                   if        vindta > *blanks AND
021900040510     C                             wFlgVINDTA = 1
022000010330     C                   add       1             rrnum
022100010330     C*
022200010601     C                   if        *in70 = *off and
022300010330     C                             (vinflg = *blanks
022400010330     C                              or vinflg = '0'
022500010330     C                              or vinflg = '2')
022600010330     C*
022700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022800010711     C                   if        vinflg = *blanks or vinflg = '0'
022900010711     C                   clear                   vinmsg
023000010711     C                   endif
023100010601     C*
023200010330     C                   exsr      inzvar
023300090403     C*
023400090403     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
023500090403     C* separatori campi
023600090403     C                   if        CharTXT <> *blanks
023700090403     C                   z-add     *zeros        wLenVINDTA        4 0
023800090403     C                   z-add     *zeros        wFlgVINDTA        1 0
023900090403     C                   setoff                                       80
024000090403     C                   dow       wLenVINDTA < %len(%trim(vindta))
024100090403     C                   eval      wLenVINDTA = wLenVINDTA + 1
024200090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
024300090403     C                   if        not *in80
024400090403     C                   seton                                        80
024500090403     C                   else
024600090403     C                   setoff                                       80
024700090403     C                   endif
024800090403     C                   endif
024900090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
025000090403     C                             and *in80 = *on
025100090403     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
025200090403     C                   endif
025300090403     C                   enddo
025400090403     C                   endif
025500090403     C*
025600080729     C                   exsr      impvax
025700010601     C*
025800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
025900080729     C                   eval      depfil = VAXLNP
026000010601     C                   exsr      repfil
026100010601     C                   if        depfil = invfil
026200021025     C                   if        vlrpoi = 999
026300080729     C                   move(P)   invfil        VAXFGS
026400021025     C                   else
026500080729     C                   move(P)   vlrpoi        VAXFGS
026600021025     C                   endif
026700050113     C                   endif
026800010601     C*
026900010601     C                   exsr      PREELA
027000010601     C*
027100010604     C* Ebbene...
027200010604     C*
027300030822     C  N31              add       1             §CTROK            7 0
027400030822     C   32              add       1             §CTRMO            7 0
027500030822     C   31              add       1             §CTRNO            7 0
027600030822     C                   if        wGiro = 2
027700080729     C  N31              write     FIVAX000
027800030822     C                   endif
027900010604     C*
028000010604     C                   if        *in31 = *off and
028100010604     C                             *in32 = *off
028200010604     C                   eval      vinflg = '1'
028300010604     C                   else
028400010604     C                   eval      recko = vindta
028500010604     C                   except    rigadett
028600010604     C                   eval      vinflg = '2'
028700010604     C                   endif
028800010601     C*
028900010604     C                   endif
029000010604     C*
029100010330     C                   else
029200010330     C                   eval      vinflg = '1'
029300010330     C                   endif
029400010601     C*
029500010601     C  N70              update    tivin000
029600010330     C*
029700030822     C  N70              enddo
029800010601     C
029900010601     C*
030000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
030100010601     C                   if        cntNonEl = *zeros or
030200010601     C                             flgMulti = '0'
030300010330     C* Se non ci sono record con errori ...
030400010601     C                   if        §ctrno = 0 and
030500010604     C                             §ctrmo = 0 and
030600010601     C                             flgStato <> '2'
030700010330     C* ... restituisco esito OK.
030800010330     C                   eval      wrkesito = '0'
030900010330     C                   else
031000010330     C                   if        §ctrok > 0
031100010330     C                   eval      wrkesito = '1'
031200010330     C                   else
031300010615     C                   if        flgOk = '0'
031400010615     C                   eval      wrkesito = '2'
031500010615     C                   else
031600010615     C                   eval      wrkesito = '6'
031700010615     C                   endif
031800010330     C                   endif
031900010330     C                   endif
032000010601     C                   else
032100010601     C                   eval      wrkesito = '9'
032200010601     C                   endif
032300010330     C*
032400010330     C                   if        %open(tivin00r)
032500010330     C                   close     tivin00r
032600010330     C                   endif
032700080729     C                   if        %open(fivaxwwr)
032800080729     C                   close     fivaxwwr
032900010330     C                   endif
033000010601     C*
033100010601     C                   if        vlrpoi <> 999
033200010601     C                   eval      invfil = vlrpoi
033300010601     C                   endif
033400010330     C*
033500010330     C                   if        §ctrok > 0
033600010601     C                             and invfil > *zeros
033700010330     C                   exsr      invio
033800010330     C                   endif
033900010601     C*
034000010618     C                   if        flgGiro = '1'
034100010601     C                   exsr      endela
034200010618     C                   endif
034300010330     C*
034400010330     C                   ENDSR
034500010330     C***
034600010601
034700010601
034800010601
034900010330     C*----------------------------------------------------*
035000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
035100010330     C*----------------------------------------------------*
035200010330     C     INZVAR        BEGSR
035300010330     C*
035400030822     C* Inizializzo variabili di wrk
035500020204     C                   Z-ADD     *zeros        Num5_0            5 0
035600090403     C                   z-add     *zeros        invfil
035700030822     C*
035800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
035900080729     C                   CLEAR                   FIVAX000
036000030822     C                   CLEAR                   SkSplitCSV
036100030822     C                   EVAL      i = 1
036200030822     C                   EVAL      posDa = *zeros
036300030822     C                   EVAL      posA  = *zeros
036400030822     C*
036500030822     C* Reimposto i valori di default
036600030822     C                   EXSR      DEFCAM
036700030822     C*
036800010330     C                   ENDSR
036900010330     C*----------------------------------------------------*
037000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
037100010330     C*----------------------------------------------------*
037200010330     C     DEFCAM        BEGSR
037300030822     C*
037400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
037500030715     C* e delimitatore testo.
037600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
037700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
037800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
037900090403     C*
038000090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
038100090403     C                   EVAL      CharSOS = CharNUM
038200030822     C*
038300030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
038400030715     C                   IF        %subst(vlrppt:1:1) = 'S'
038500030715     C                   SETON                                        50
038600030715     C                   ELSE
038700030715     C                   SETOFF                                       50
038800030715     C                   ENDIF
038900030822     C*
039000030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
039100030822     C                   EVAL      posDaDft = 1
039200030822     C                   EVAL      posADft  = 0
039300030822     C                   EVAL      wGiroDft = 0
039400030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
039500030822     C                             posDaDft > 0
039600030822     C*
039700030822     C* Gestisco il 1° giro
039800030822     C                   IF        wGiroDft = 0
039900030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
040000030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
040100030822     C* Incremento il contatore dei "giri"
040200030822     C                   EVAL      wGiroDft = 1
040300030822     C                   ELSE
040400030822     C                   EVAL      posDaDft = posADft
040500030822     C                   ENDIF
040600030822     C* Eseguo lo scan x trovare la fine del campo corrente
040700030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
040800030822     C*
040900030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
041000030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
041100030822     C                   IF        posDaDft > 0 AND
041200030822     C                             posADft  > 0
041300030822     C* CCM
041400030822     C                   IF        %subst(
041500030822     C                             %subst(vlrppt:posDaDft+1:
041600030822     C                             posADft-posDaDft-1):1:3)
041700030822     C                             = 'CCM'
041800030822     C                   EVAL      PiStr=%trim(%subst(
041900030822     C                             %subst(vlrppt:posDaDft+1:
042000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042100030822     C                   EXSR      CHKNUM
042200030822     C                   IF        PiInt=*on
042300080729     C                   Z-ADD     PiVal         VAXCCM
042400030822     C                   ENDIF
042500030822     C                   ENDIF
042600030822     C* LNP
042700030822     C                   IF        %subst(
042800030822     C                             %subst(vlrppt:posDaDft+1:
042900030822     C                             posADft-posDaDft-1):1:3)
043000030822     C                             = 'LNP'
043100030822     C                   EVAL      PiStr=%trim(%subst(
043200030822     C                             %subst(vlrppt:posDaDft+1:
043300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043400030822     C                   EXSR      CHKNUM
043500030822     C                   IF        PiInt=*on
043600080729     C                   Z-ADD     PiVal         VAXLNP
043700030822     C                   ENDIF
043800030822     C                   ENDIF
043900080729     C* TMD
044000030822     C                   IF        %subst(
044100030822     C                             %subst(vlrppt:posDaDft+1:
044200030822     C                             posADft-posDaDft-1):1:3)
044300080729     C                             = 'TMD'
044400080729     C                   EVAL      VAXTMD=%trim(%subst(
044500030822     C                             %subst(vlrppt:posDaDft+1:
044600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044700030822     C                   ENDIF
044800080729     C* TRK
044900080729     C                   IF        %subst(
045000080729     C                             %subst(vlrppt:posDaDft+1:
045100080729     C                             posADft-posDaDft-1):1:3)
045200080729     C                             = 'TRK'
045300080729     C                   EVAL      VAXTRK=%trim(%subst(
045400080729     C                             %subst(vlrppt:posDaDft+1:
045500080729     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045600080729     C                   ENDIF
045700030822     C* ...
045800030822     C                   ENDIF
045900030822     C                   ENDDO
046000020204     C*
046100010330     C                   ENDSR
046200010607     C*----------------------------------------------------*
046300080729     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAX)
046400010607     C*----------------------------------------------------*
046500080729     C     IMPVAX        BEGSR
046600010607     C*
046700010607     C                   SETOFF                                       3132
046800030515     C*
046900080729     C                   MOVEL     datcor        VAXAAS
047000010607     C*
047100030715     C**********
047200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
047300030715     C**********
047400030715     C                   IF        wGiro = *zeros
047500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
047600031201     C     minu:maiu     XLATE     vindta        vindta
047700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
047800040324     C                   DOW       posDa <= %len(%trimr(vindta))
047900030715     C*
048000030715     C* Gestisco il 1° campo
048100030715     C                   IF        i = 1
048200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
048300030715     C                   EVAL      posDa = 1
048400030715     C* Eseguo lo scan x trovare la fine del primo campo
048500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
048600030715     C* A questo punto "estraggo" il campo corrente
048700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
048800030715     C                                                    (posA-posDa))
048900030715     C* X i campi successivi al 1°
049000030715     C                   ELSE
049100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
049200030715     C                   EVAL      posDa = posA + 1
049300030715     C* Eseguo lo scan x trovare la fine del campo corrente
049400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
049500030715     C* Gestisco l'ultimo campo
049600030715     C                   IF        posA = *zeros
049700040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
049800030715     C* A questo punto "estraggo" il campo corrente
049900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
050000030715     C                                                    (posA-posDa))
050100030715     C                   LEAVE
050200030715     C                   ELSE
050300030715     C* A questo punto "estraggo" il campo corrente
050400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
050500030715     C                                                    (posA-posDa))
050600030715     C                   ENDIF
050700030715     C                   ENDIF
050800030715     C* Incremento il contatore d campo
050900030715     C                   EVAL      i = i +1
051000030715     C                   ENDDO
051100030715     C*
051200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
051300030715     C* delimitazione testo
051400030715     C                   EVAL      i = 1
051500030715     C                   DOW       i < %elem(SkSplitFLD)
051600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
051700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
051800030715     C                   EVAL      i = i + 1
051900030715     C                   ENDDO
052000030715     C                   EVAL      wGiro = 1
052100030715     C                   ELSE
052200030715     C                   EVAL      wGiro = 2
052300031204     C**********
052400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
052500031204     C* d separatore campo
052600031204     C**********
052700040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
052800040304     C                   EVAL      vindta = ' ' + vindta
052900040304     C                   ENDIF
053000040304     C***                DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
053100040304     C***                EVAL      vindta = %subst(vindta:2)
053200040304     C***                ENDDO
053300031204     C*
053400031204     C                   Z-ADD     *zeros        lunghInput        4 0
053500040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
053600040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
053700040304     C                   EVAL      vindta = vindta + ' '
053800040304     C                   ENDIF
053900040324     C***                DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
054000040324     C***                EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
054100040304     C***                EVAL      lunghInput = %len(%trim(vindta))
054200040304     C***                ENDDO
054300030715     C**********
054400030715     C* Effettuo lo split del campo dati d input
054500030715     C**********
054600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
054700040324     C                   DOW       posDa <= %len(%trimr(vindta))
054800030715     C*
054900030715     C* Gestisco il 1° campo
055000030715     C                   IF        i = 1
055100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
055200030715     C                   EVAL      posDa = 1
055300030715     C* Eseguo lo scan x trovare la fine del primo campo
055400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
055500030715     C* A questo punto "estraggo" il campo corrente
055600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
055700030715     C                                                    (posA-posDa))
055800030715     C* X i campi successivi al 1°
055900030715     C                   ELSE
056000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
056100030715     C                   EVAL      posDa = posA + 1
056200031204     C* Verifico che nn vi sia il campo nn valorizzato
056300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
056400031204     C* Se campo nn valorizzato skippo al prossimo
056500031204     C                   EVAL      SkSplitCSV(i) = *blanks
056600031204     C                   EVAL      posA  = posA + 1
056700031204     C                   ELSE
056800030715     C* Eseguo lo scan x trovare la fine del campo corrente
056900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
057000030715     C* Gestisco l'ultimo campo
057100030715     C                   IF        posA = *zeros
057200040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
057300030715     C* A questo punto "estraggo" il campo corrente
057400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
057500030715     C                                                    (posA-posDa))
057600030715     C                   LEAVE
057700030715     C                   ELSE
057800030715     C* A questo punto "estraggo" il campo corrente
057900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
058000030715     C                                                    (posA-posDa))
058100030715     C                   ENDIF
058200031204     C                   ENDIF
058300030715     C                   ENDIF
058400030715     C* Incremento il contatore d campo
058500030715     C                   EVAL      i = i +1
058600030715     C                   ENDDO
058700030715     C*
058800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
058900030715     C* delimitazione testo
059000030715     C                   EVAL      i = 1
059100030715     C                   DOW       i < %elem(SkSplitCSV)
059200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
059300110527     C***                EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
059400030715     C                   EVAL      i = i + 1
059500030715     C                   ENDDO
059600030715     C*
059700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
059800030715     C                   EVAL      i = 1
059900030715     C                   DOW       i < %elem(SkSplitFLD)
060000030715     C*
060100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
060200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
060300030715     C                             SkSplitCSV(i) <> *blanks
060400030721     C*** ATB
060500080729     C                   IF        %trim(SkSplitFLD(i)) = 'VAXATB'
060600080729     C                   EVAL      VAXATB = SkSplitCSV(i)
060700030721     C                   ENDIF
060800080729     C*** TMD
060900080729     C                   IF        %trim(SkSplitFLD(i)) = 'VAXTMD'
061000080729     C                   EVAL      VAXTMD = SkSplitCSV(i)
061100030715     C                   ENDIF
061200080729     C*** TRK
061300080729     C                   IF        %trim(SkSplitFLD(i)) = 'VAXTRK'
061400080729     C                   EVAL      VAXTRK = SkSplitCSV(i)
061500030721     C                   ENDIF
061600080729     C*** DTT
061700080729     C                   IF        %trim(SkSplitFLD(i)) = 'VAXDTT'
061800080729     C                   EVAL      VAXDTT = SkSplitCSV(i)
061900080729     C                   ENDIF
062000030721     C*
062100030721     C* Reperisco quindi i campi numerici...
062200030721     C*** NSP
062300080729     C                   IF        %trim(SkSplitFLD(i)) = 'VAXNSP'
062400030721     C                   EVAL      PiStr=SkSplitCSV(i)
062500030721     C                   EXSR      CHKNUM
062600030721     C                   IF        PiInt=*on AND
062700030721     C                             PiVal<=9999999
062800080729     C                   Z-ADD     PiVal         VAXNSP
062900030721     C                   ELSE
063000030721     C                   SETON                                        32
063100080729     C                   EVAL      VAXNSP = *zeros
063200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
063300080729     C                             + ' ' + 'VAXNSP'
063400030721     C                   ENDIF
063500030721     C                   ENDIF
063600030721     C*** CCM
063700080729     C                   IF        %trim(SkSplitFLD(i)) = 'VAXCCM'
063800030721     C                   EVAL      PiStr=SkSplitCSV(i)
063900030721     C                   EXSR      CHKNUM
064000030721     C                   IF        PiInt=*on      AND
064100030721     C                             PiVal<=9999999 AND
064200030721     C                             PiVal>*zeros
064300080729     C                   Z-ADD     PiVal         VAXCCM
064400030721     C                   ELSE
064500030721     C                   SETON                                        32
064600080729     C                   EVAL      VAXCCM = *zeros
064700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
064800080729     C                             + ' ' + 'VAXCCM'
064900030721     C                   ENDIF
065000030721     C                   ENDIF
065100030721     C*** LNP
065200080729     C                   IF        %trim(SkSplitFLD(i)) = 'VAXLNP'
065300030721     C                   EVAL      PiStr=SkSplitCSV(i)
065400030721     C                   EXSR      CHKNUM
065500030721     C                   IF        PiInt=*on    AND
065600030721     C                             PiVal<=999   AND
065700030721     C                             PiVal>*zeros
065800080729     C                   Z-ADD     PiVal         VAXLNP
065900030721     C                   ELSE
066000030721     C                   SETON                                        32
066100080729     C                   EVAL      VAXLNP = *zeros
066200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
066300080729     C                             + ' ' + 'VAXLNP'
066400030721     C                   ENDIF
066500030721     C                   ENDIF
066600030721     C*** NRS
066700080729     C                   IF        %trim(SkSplitFLD(i)) = 'VAXNRS'
066800030721     C                   EVAL      PiStr=SkSplitCSV(i)
066900030721     C                   EXSR      CHKNUM
067000030721     C                   IF        PiInt=*on AND
067100030721     C                             PiVal<=99
067200080729     C                   Z-ADD     PiVal         VAXNRS
067300030721     C                   ELSE
067400030721     C                   SETON                                        32
067500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
067600080729     C                             + ' ' + 'VAXNRS'
067700030721     C                   ENDIF
067800030721     C                   ENDIF
067900080729     C*** PRR
068000080729     C                   IF        %trim(SkSplitFLD(i)) = 'VAXPRR'
068100080729     C                   EVAL      PiStr=SkSplitCSV(i)
068200080729     C                   EXSR      CHKNUM
068300080729     C                   IF        PiInt=*on AND
068400080729     C                             PiVal<=9999
068500080729     C                   Z-ADD     PiVal         VAXPRR
068600080729     C                   ELSE
068700080729     C                   SETON                                        32
068800080729     C                   EVAL      vinmsg = %trimr(vinmsg)
068900080729     C                             + ' ' + 'VAXPRR'
069000080729     C                   ENDIF
069100080729     C                   ENDIF
069200030715     C*
069300030715     C                   ENDIF
069400030715     C                   EVAL      i = i + 1
069500030715     C                   ENDDO
069600020204     C*
069700030715     C                   ENDIF
069800020204     C*
069900010607     C                   ENDSR
070000010607     C*----------------------------------------------------*
070100040714
070200010330
070300010330
070400010330     C*----------------------------------------------------*
070500010330     C*  CONTROLLO NUMERICITA' CAMPI
070600010330     C*----------------------------------------------------*
070700010330     C     CHKNUM        BEGSR
070800010330     C*
070900010606     C                   IF        PiDecChr = *blanks
071000030715     C                   EVAL      PiDecChr = CharNUM
071100010606     C                   ENDIF
071200010606     C*
071300010606     C                   CALL(e)   'ISNUMERIC'
071400010330     C                   PARM                    PiStr            30
071500010606     C                   PARM                    PiDecChr          1
071600010330     C                   PARM      *ZEROS        PiVal            30 9
071700010330     C                   PARM      '0'           PiInt             1
071800010330     C                   PARM      '0'           PiNum             1
071900010330     C                   IF        %error
072000010606     C                   EVAL      PiNum=*off
072100010330     C                   ENDIF
072200010330     C*
072300010330     C                   ENDSR
072400010330     C***
072500010330
072600010601
072700010601
072800010601
072900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
073000010601     C     repfil        BEGSR
073100010601     C*
073200010601     C                   if        invfil = *zeros and
073300010601     C                             depfil > *zeros and
073400010629     C                             (vinflg = *blanks or
073500010629     C                              vinflg = *zeros)
073600010601     C
073700010601     C                   eval      invfil = depfil
073800010601     C                   endif
073900010601     C*
074000010601     C                   if        depfil <> invfil and
074100010601     C                             invfil > *zeros
074200010601     C                   eval      flgMulti = '1'
074300010601     C                   if        vinflg = *blanks
074400010601     C                   add       1             cntNonEl
074500010601     C                   endif
074600010601     C                   endif
074700010601     C*
074800010601     C                   if        vinflg = '2'
074900010601     C                   eval      flgStato = '2'
075000010601     C                   endif
075100010601     C*
075200010601     C                   ENDSR
075300010601     C***
075400010601
075500010601
075600010601
075700010330
075800010330
075900010330
076000990920      /TITLE Invio dei dati al punto operativo.
076100000613     C     invio         BEGSR
076200990920     C*
076300990920     C                   reset                   dscmz
076400010601     C                   move      invfil        cmzdst
076500990920     C                   eval      cmzfld = vlrfou
076600990920     C                   eval      cmzmbd = vlrhdl
076700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
076800000710     C                   if        prmfir = *blanks
076900080729     C                   eval      cmzfla = 'FIVAX00F'
077000080729     C                   eval      cmzmba = 'FIVAX00F'
077100000710     C                   else
077200000710     C                   eval      cmzfla = prmfir
077300000710     C                   eval      cmzmba = prmfir
077400000710     C                   endif
077500990920     C                   eval      cmznrr = *zeros
077600990920     C                   move      §ctrok        cmznrr
077700021018     C                   eval      cmzlba = vlrfl1
077800990920     C                   call(e)   'TIS711C'
077900990920     C                   parm                    dscmz
078000990921     C                   parm      *blanks       esito
078100990920     C                   if        %error
078200990920     C                             or cmzerr = '1'
078300990921     C                             or esito  = '1'
078400000710     C                   eval      wrkesito = '3'
078500990920     C                   endif
078600990920     C*
078700000613     C                   ENDSR
078800990910
078900010601
079000010601
079100010601
079200010601
079300010601      /TITLE Invio dei dati al punto operativo.
079400010601     C     opeini        BEGSR
079500010601     C*
079600010601     C* Inizializzo flag e contatori operativi
079700010601     C                   movel     '0'           flgGiro           1
079800010601     C                   movel     '0'           flgMulti          1
079900010601     C                   movel     '1'           flgStato          1
080000010615     C                   movel     '0'           flgOk             1
080100010601     C                   z-add     *zeros        cntNonEl         10 0
080200010601     C                   z-add     *zeros        depfil            3 0
080300010601     C                   z-add     *zeros        invfil            3 0
080400010601     C*
080500010601     C                   ENDSR
080600010601     C***
080700050421
080800050421
080900050421
081000050421     C     *pssr         BEGSR
081100050421     C*
081200050421     C                   eval      wrkesito = '2'
081300050421     C*
081400050421     C                   ENDSR     '*CANCL'
081500050421     C***
081600010601
081700010601
081800010601
081900010330
082000010330
082100000613     C     *inzsr        BEGSR
082200990910     C*
082300990910     C     *entry        plist
082400990920     C                   parm                    tivlrds
082500990921     C                   parm      wrkesito      esito
082600000724     C                   parm                    prmlit
082700000710     C                   parm                    prmfir
082800010330     C*
082900010330     C* CALCOLA LA DATA CORRENTE
083000010330     C                   time                    wn14             14 0
083100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
083200010330     C                   z-add     wn8           g08dat
083300010330     C                   z-add     *zeros        g08inv
083400010330     C                   movel     '0'           g08err
083500010330     C                   call      'XSRDA8'
083600010330     C                   parm                    wlbda8
083700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
083800000613     C*
083900000613     C                   ENDSR
084000000613     C***
084100990908
084200030715     Otitvxlp   E            riepilogo         2
084300990915     O                                              'Upload via Internet'
084400990915     O                                           +1 'Traduzione TIVIN00R -'
084500080729     O                                           +1 'FIVAXWWR'
084600990915     O                                           +1 'Testate bolle'
084700990915     O          E            riepilogo   2
084800990915     O                       wrkdata
084900990915     O                       wrkora              +1
085000990915     O                       procname            +1
085100990915     O          E            riepilogo   2
085200990915     O                                              'Cliente..................:'
085300080729     O                       VAXCCM        z     +1
085400990915     O          E            riepilogo   2
085500990920     O                                              'Riferimento Strategi.....:'
085600990920     O                       vlrhdl              +1
085700990915     O          E            riepilogo   2
085800990915     O                                              'Giusti...................:'
085900971022     O                       §CTROK        2   +  1
086000990915     O          E            riepilogo   2
086100990915     O                                              'Sbagliati e corretti.....:'
086200971022     O                       §CTRMO        2   +  1
086300990915     O          E            riepilogo   2
086400990915     O                                              'Sbagliati e scartati.....:'
086500971022     O                       §CTRNO        2   +  1
086600000613
086700030715     Otitvxlps  E            testdett          2
086800000613     O                                              'Upload via Internet'
086900000613     O                                           +1 'Traduzione TIVIN00R -'
087000080729     O                                           +1 'FIVAXWWR'
087100000613     O                                           +1 'Testate bolle'
087200000616     O          E            testdett    3
087300000613     O                                           +2 'N° rec'
087400000613     O                                           +3 'Anteprima contenuto'
087500000616     O          E            rigadett    2
087600000613     O                       rrnum               +2
087700000621     O                       recko               +3
087800000616     O          E            findett     2
087900000613     O                       wrkdata
088000000613     O                       wrkora              +1
088100000613     O                       procname            +1
088200000616     O          E            findett     2
088300000613     O                                              'Cliente..................:'
088400080729     O                       VAXCCM        z     +1
088500000616     O          E            findett     2
088600000613     O                                              'Riferimento Strategi.....:'
088700000613     O                       vlrhdl              +1
088800000616     O          E            findett     2
088900000613     O                                              'Giusti...................:'
089000000613     O                       §CTROK        2   +  1
089100000616     O          E            findett     2
089200000613     O                                              'Sbagliati e corretti.....:'
089300000613     O                       §CTRMO        2   +  1
089400000616     O          E            findett     2
089500000613     O                                              'Sbagliati e scartati.....:'
089600000613     O                       §CTRNO        2   +  1
089700000613** CMD - COMANDI CL
089800030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
089900030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
090000030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
090100010606
090200000613
