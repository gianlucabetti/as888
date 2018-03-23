000100060622      /TITLE Upload via Internet: traduzione in FIVADWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600060622     FFIVADwwr  O    E             DISK    usropn
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
007500060622     C                   exsr      rwvad
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
018300060622     C* RWVAD   LEGGE TIVIN00R E SCRIVE FIVADWWF              *
018400010330     C*--------------------------------------------------------
018500060622     C     RWVAD         BEGSR
018600010330     C*
018700010330     C                   if        not %open(tivin00r)
018800010330     C                   open      tivin00r
018900010330     C                   endif
019000060622     C                   if        not %open(fivadwwr)
019100060622     C                   open      fivadwwr
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
025600060622     C                   exsr      impvad
025700010601     C*
025800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
025900060622     C                   eval      depfil = VADLNP
026000010601     C                   exsr      repfil
026100010601     C                   if        depfil = invfil
026200021025     C                   if        vlrpoi = 999
026300060622     C                   move(P)   invfil        VADFGS
026400021025     C                   else
026500060622     C                   move(P)   vlrpoi        VADFGS
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
027700060622     C  N31              write     FIVAD000
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
032700060622     C                   if        %open(fivadwwr)
032800060622     C                   close     fivadwwr
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
035900060622     C                   CLEAR                   FIVAD000
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
042300060622     C                   Z-ADD     PiVal         VADCCM
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
043600060622     C                   Z-ADD     PiVal         VADLNP
043700030822     C                   ENDIF
043800030822     C                   ENDIF
043900060622     C* NRS
044000060622     C                   IF        %subst(
044100060622     C                             %subst(vlrppt:posDaDft+1:
044200060622     C                             posADft-posDaDft-1):1:3)
044300060622     C                             = 'NRS'
044400060622     C                   EVAL      PiStr=%trim(%subst(
044500060622     C                             %subst(vlrppt:posDaDft+1:
044600060622     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044700060622     C                   EXSR      CHKNUM
044800060622     C                   IF        PiInt=*on
044900060622     C                   Z-ADD     PiVal         VADNRS
045000060622     C                   ENDIF
045100060622     C                   ENDIF
045200060622     C* NCL
045300060622     C                   IF        %subst(
045400060622     C                             %subst(vlrppt:posDaDft+1:
045500060622     C                             posADft-posDaDft-1):1:3)
045600060622     C                             = 'NCL'
045700060622     C                   EVAL      PiStr=%trim(%subst(
045800060622     C                             %subst(vlrppt:posDaDft+1:
045900060622     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046000060622     C                   EXSR      CHKNUM
046100060622     C                   IF        PiInt=*on
046200060622     C                   Z-ADD     PiVal         VADNCL
046300060622     C                   ENDIF
046400060622     C                   ENDIF
046500060622     C* CDP
046600030822     C                   IF        %subst(
046700030822     C                             %subst(vlrppt:posDaDft+1:
046800030822     C                             posADft-posDaDft-1):1:3)
046900060622     C                             = 'CDP'
047000060622     C                   EVAL      VADCDP=%trim(%subst(
047100030822     C                             %subst(vlrppt:posDaDft+1:
047200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047300030822     C                   ENDIF
047400030822     C* ...
047500030822     C                   ENDIF
047600030822     C                   ENDDO
047700020204     C*
047800010330     C                   ENDSR
047900010607     C*----------------------------------------------------*
048000060622     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAD)
048100010607     C*----------------------------------------------------*
048200060622     C     IMPVAD        BEGSR
048300010607     C*
048400010607     C                   SETOFF                                       3132
048500030515     C*
048600060622     C                   MOVEL     datcor        VADAAS
048700010607     C*
048800030715     C**********
048900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
049000030715     C**********
049100030715     C                   IF        wGiro = *zeros
049200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
049300031201     C     minu:maiu     XLATE     vindta        vindta
049400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
049500040324     C                   DOW       posDa <= %len(%trimr(vindta))
049600030715     C*
049700030715     C* Gestisco il 1° campo
049800030715     C                   IF        i = 1
049900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
050000030715     C                   EVAL      posDa = 1
050100030715     C* Eseguo lo scan x trovare la fine del primo campo
050200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
050300030715     C* A questo punto "estraggo" il campo corrente
050400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
050500030715     C                                                    (posA-posDa))
050600030715     C* X i campi successivi al 1°
050700030715     C                   ELSE
050800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
050900030715     C                   EVAL      posDa = posA + 1
051000030715     C* Eseguo lo scan x trovare la fine del campo corrente
051100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
051200030715     C* Gestisco l'ultimo campo
051300030715     C                   IF        posA = *zeros
051400040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
051500030715     C* A questo punto "estraggo" il campo corrente
051600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
051700030715     C                                                    (posA-posDa))
051800030715     C                   LEAVE
051900030715     C                   ELSE
052000030715     C* A questo punto "estraggo" il campo corrente
052100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
052200030715     C                                                    (posA-posDa))
052300030715     C                   ENDIF
052400030715     C                   ENDIF
052500030715     C* Incremento il contatore d campo
052600030715     C                   EVAL      i = i +1
052700030715     C                   ENDDO
052800030715     C*
052900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
053000030715     C* delimitazione testo
053100030715     C                   EVAL      i = 1
053200030715     C                   DOW       i < %elem(SkSplitFLD)
053300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
053400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
053500030715     C                   EVAL      i = i + 1
053600030715     C                   ENDDO
053700030715     C                   EVAL      wGiro = 1
053800030715     C                   ELSE
053900030715     C                   EVAL      wGiro = 2
054000031204     C**********
054100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
054200031204     C* d separatore campo
054300031204     C**********
054400040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
054500040304     C                   EVAL      vindta = ' ' + vindta
054600040304     C                   ENDIF
054700040304     C***                DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
054800040304     C***                EVAL      vindta = %subst(vindta:2)
054900040304     C***                ENDDO
055000031204     C*
055100031204     C                   Z-ADD     *zeros        lunghInput        4 0
055200040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
055300040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
055400040304     C                   EVAL      vindta = vindta + ' '
055500040304     C                   ENDIF
055600040324     C***                DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
055700040324     C***                EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
055800040304     C***                EVAL      lunghInput = %len(%trim(vindta))
055900040304     C***                ENDDO
056000030715     C**********
056100030715     C* Effettuo lo split del campo dati d input
056200030715     C**********
056300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
056400040324     C                   DOW       posDa <= %len(%trimr(vindta))
056500030715     C*
056600030715     C* Gestisco il 1° campo
056700030715     C                   IF        i = 1
056800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
056900030715     C                   EVAL      posDa = 1
057000030715     C* Eseguo lo scan x trovare la fine del primo campo
057100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
057200030715     C* A questo punto "estraggo" il campo corrente
057300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
057400030715     C                                                    (posA-posDa))
057500030715     C* X i campi successivi al 1°
057600030715     C                   ELSE
057700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
057800030715     C                   EVAL      posDa = posA + 1
057900031204     C* Verifico che nn vi sia il campo nn valorizzato
058000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
058100031204     C* Se campo nn valorizzato skippo al prossimo
058200031204     C                   EVAL      SkSplitCSV(i) = *blanks
058300031204     C                   EVAL      posA  = posA + 1
058400031204     C                   ELSE
058500030715     C* Eseguo lo scan x trovare la fine del campo corrente
058600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
058700030715     C* Gestisco l'ultimo campo
058800030715     C                   IF        posA = *zeros
058900040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
059000030715     C* A questo punto "estraggo" il campo corrente
059100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
059200030715     C                                                    (posA-posDa))
059300030715     C                   LEAVE
059400030715     C                   ELSE
059500030715     C* A questo punto "estraggo" il campo corrente
059600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
059700030715     C                                                    (posA-posDa))
059800030715     C                   ENDIF
059900031204     C                   ENDIF
060000030715     C                   ENDIF
060100030715     C* Incremento il contatore d campo
060200030715     C                   EVAL      i = i +1
060300030715     C                   ENDDO
060400030715     C*
060500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
060600030715     C* delimitazione testo
060700030715     C                   EVAL      i = 1
060800030715     C                   DOW       i < %elem(SkSplitCSV)
060900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
061000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
061100030715     C                   EVAL      i = i + 1
061200030715     C                   ENDDO
061300030715     C*
061400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
061500030715     C                   EVAL      i = 1
061600030715     C                   DOW       i < %elem(SkSplitFLD)
061700030715     C*
061800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
061900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
062000030715     C                             SkSplitCSV(i) <> *blanks
062100030721     C*** ATB
062200060622     C                   IF        %trim(SkSplitFLD(i)) = 'VADATB'
062300060622     C                   EVAL      VADATB = SkSplitCSV(i)
062400030721     C                   ENDIF
062500060622     C*** CDP
062600060622     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP'
062700060622     C                   EVAL      VADCDP = SkSplitCSV(i)
062800030715     C                   ENDIF
062900030721     C*
063000030721     C* Reperisco quindi i campi numerici...
063100030721     C*** NSP
063200060622     C                   IF        %trim(SkSplitFLD(i)) = 'VADNSP'
063300030721     C                   EVAL      PiStr=SkSplitCSV(i)
063400030721     C                   EXSR      CHKNUM
063500030721     C                   IF        PiInt=*on AND
063600030721     C                             PiVal<=9999999
063700060622     C                   Z-ADD     PiVal         VADNSP
063800030721     C                   ELSE
063900030721     C                   SETON                                        32
064000060622     C                   EVAL      VADNSP = *zeros
064100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
064200060622     C                             + ' ' + 'VADNSP'
064300030721     C                   ENDIF
064400030721     C                   ENDIF
064500030721     C*** CCM
064600060622     C                   IF        %trim(SkSplitFLD(i)) = 'VADCCM'
064700030721     C                   EVAL      PiStr=SkSplitCSV(i)
064800030721     C                   EXSR      CHKNUM
064900030721     C                   IF        PiInt=*on      AND
065000030721     C                             PiVal<=9999999 AND
065100030721     C                             PiVal>*zeros
065200060622     C                   Z-ADD     PiVal         VADCCM
065300030721     C                   ELSE
065400030721     C                   SETON                                        32
065500060622     C                   EVAL      VADCCM = *zeros
065600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
065700060622     C                             + ' ' + 'VADCCM'
065800030721     C                   ENDIF
065900030721     C                   ENDIF
066000030721     C*** LNP
066100060622     C                   IF        %trim(SkSplitFLD(i)) = 'VADLNP'
066200030721     C                   EVAL      PiStr=SkSplitCSV(i)
066300030721     C                   EXSR      CHKNUM
066400030721     C                   IF        PiInt=*on    AND
066500030721     C                             PiVal<=999   AND
066600030721     C                             PiVal>*zeros
066700060622     C                   Z-ADD     PiVal         VADLNP
066800030721     C                   ELSE
066900030721     C                   SETON                                        32
067000060622     C                   EVAL      VADLNP = *zeros
067100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
067200060622     C                             + ' ' + 'VADLNP'
067300030721     C                   ENDIF
067400030721     C                   ENDIF
067500030721     C*** NRS
067600060622     C                   IF        %trim(SkSplitFLD(i)) = 'VADNRS'
067700030721     C                   EVAL      PiStr=SkSplitCSV(i)
067800030721     C                   EXSR      CHKNUM
067900030721     C                   IF        PiInt=*on AND
068000030721     C                             PiVal<=99
068100060622     C                   Z-ADD     PiVal         VADNRS
068200030721     C                   ELSE
068300030721     C                   SETON                                        32
068400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
068500060622     C                             + ' ' + 'VADNRS'
068600030721     C                   ENDIF
068700030721     C                   ENDIF
068800060622     C*** NCL
068900060622     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL'
069000060622     C                   EVAL      PiStr=SkSplitCSV(i)
069100060622     C                   EXSR      CHKNUM
069200060622     C                   IF        PiInt=*on
069300060622     C                   Z-ADD     PiVal         VADNCL
069400060622     C                   ELSE
069500060622     C                   SETON                                        32
069600060622     C                   EVAL      vinmsg = %trimr(vinmsg)
069700060622     C                             + ' ' + 'VADNCL'
069800060622     C                   ENDIF
069900060622     C                   ENDIF
070000060622     C*** NCD
070100060622     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCD'
070200060622     C                   EVAL      PiStr=SkSplitCSV(i)
070300060622     C                   EXSR      CHKNUM
070400060622     C                   IF        PiInt=*on
070500060622     C                   Z-ADD     PiVal         VADNCD
070600060622     C                   ELSE
070700060622     C                   SETON                                        32
070800060622     C                   EVAL      vinmsg = %trimr(vinmsg)
070900060622     C                             + ' ' + 'VADNCD'
071000060622     C                   ENDIF
071100060622     C                   ENDIF
071200060622     C*** NCA
071300060622     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCA'
071400060622     C                   EVAL      PiStr=SkSplitCSV(i)
071500060622     C                   EXSR      CHKNUM
071600060622     C                   IF        PiInt=*on
071700060622     C                   Z-ADD     PiVal         VADNCA
071800060622     C                   ELSE
071900060622     C                   SETON                                        32
072000060622     C                   EVAL      vinmsg = %trimr(vinmsg)
072100060622     C                             + ' ' + 'VADNCA'
072200060622     C                   ENDIF
072300060622     C                   ENDIF
072400030715     C*
072500030715     C                   ENDIF
072600030715     C                   EVAL      i = i + 1
072700030715     C                   ENDDO
072800020204     C*
072900030715     C                   ENDIF
073000020204     C*
073100010607     C                   ENDSR
073200010607     C*----------------------------------------------------*
073300040714
073400010330
073500010330
073600010330     C*----------------------------------------------------*
073700010330     C*  CONTROLLO NUMERICITA' CAMPI
073800010330     C*----------------------------------------------------*
073900010330     C     CHKNUM        BEGSR
074000010330     C*
074100010606     C                   IF        PiDecChr = *blanks
074200030715     C                   EVAL      PiDecChr = CharNUM
074300010606     C                   ENDIF
074400010606     C*
074500010606     C                   CALL(e)   'ISNUMERIC'
074600010330     C                   PARM                    PiStr            30
074700010606     C                   PARM                    PiDecChr          1
074800010330     C                   PARM      *ZEROS        PiVal            30 9
074900010330     C                   PARM      '0'           PiInt             1
075000010330     C                   PARM      '0'           PiNum             1
075100010330     C                   IF        %error
075200010606     C                   EVAL      PiNum=*off
075300010330     C                   ENDIF
075400010330     C*
075500010330     C                   ENDSR
075600010330     C***
075700010330
075800010601
075900010601
076000010601
076100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
076200010601     C     repfil        BEGSR
076300010601     C*
076400010601     C                   if        invfil = *zeros and
076500010601     C                             depfil > *zeros and
076600010629     C                             (vinflg = *blanks or
076700010629     C                              vinflg = *zeros)
076800010601     C
076900010601     C                   eval      invfil = depfil
077000010601     C                   endif
077100010601     C*
077200010601     C                   if        depfil <> invfil and
077300010601     C                             invfil > *zeros
077400010601     C                   eval      flgMulti = '1'
077500010601     C                   if        vinflg = *blanks
077600010601     C                   add       1             cntNonEl
077700010601     C                   endif
077800010601     C                   endif
077900010601     C*
078000010601     C                   if        vinflg = '2'
078100010601     C                   eval      flgStato = '2'
078200010601     C                   endif
078300010601     C*
078400010601     C                   ENDSR
078500010601     C***
078600010601
078700010601
078800010601
078900010330
079000010330
079100010330
079200990920      /TITLE Invio dei dati al punto operativo.
079300000613     C     invio         BEGSR
079400990920     C*
079500990920     C                   reset                   dscmz
079600010601     C                   move      invfil        cmzdst
079700990920     C                   eval      cmzfld = vlrfou
079800990920     C                   eval      cmzmbd = vlrhdl
079900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
080000000710     C                   if        prmfir = *blanks
080100060622     C                   eval      cmzfla = 'FIVAD00F'
080200060622     C                   eval      cmzmba = 'FIVAD00F'
080300000710     C                   else
080400000710     C                   eval      cmzfla = prmfir
080500000710     C                   eval      cmzmba = prmfir
080600000710     C                   endif
080700990920     C                   eval      cmznrr = *zeros
080800990920     C                   move      §ctrok        cmznrr
080900021018     C                   eval      cmzlba = vlrfl1
081000990920     C                   call(e)   'TIS711C'
081100990920     C                   parm                    dscmz
081200990921     C                   parm      *blanks       esito
081300990920     C                   if        %error
081400990920     C                             or cmzerr = '1'
081500990921     C                             or esito  = '1'
081600000710     C                   eval      wrkesito = '3'
081700990920     C                   endif
081800990920     C*
081900000613     C                   ENDSR
082000990910
082100010601
082200010601
082300010601
082400010601
082500010601      /TITLE Invio dei dati al punto operativo.
082600010601     C     opeini        BEGSR
082700010601     C*
082800010601     C* Inizializzo flag e contatori operativi
082900010601     C                   movel     '0'           flgGiro           1
083000010601     C                   movel     '0'           flgMulti          1
083100010601     C                   movel     '1'           flgStato          1
083200010615     C                   movel     '0'           flgOk             1
083300010601     C                   z-add     *zeros        cntNonEl         10 0
083400010601     C                   z-add     *zeros        depfil            3 0
083500010601     C                   z-add     *zeros        invfil            3 0
083600010601     C*
083700010601     C                   ENDSR
083800010601     C***
083900050421
084000050421
084100050421
084200050421     C     *pssr         BEGSR
084300050421     C*
084400050421     C                   eval      wrkesito = '2'
084500050421     C*
084600050421     C                   ENDSR     '*CANCL'
084700050421     C***
084800010601
084900010601
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
087700060622     O                                           +1 'FIVADWWR'
087800990915     O                                           +1 'Testate bolle'
087900990915     O          E            riepilogo   2
088000990915     O                       wrkdata
088100990915     O                       wrkora              +1
088200990915     O                       procname            +1
088300990915     O          E            riepilogo   2
088400990915     O                                              'Cliente..................:'
088500060622     O                       VADCCM        z     +1
088600990915     O          E            riepilogo   2
088700990920     O                                              'Riferimento Strategi.....:'
088800990920     O                       vlrhdl              +1
088900990915     O          E            riepilogo   2
089000990915     O                                              'Giusti...................:'
089100971022     O                       §CTROK        2   +  1
089200990915     O          E            riepilogo   2
089300990915     O                                              'Sbagliati e corretti.....:'
089400971022     O                       §CTRMO        2   +  1
089500990915     O          E            riepilogo   2
089600990915     O                                              'Sbagliati e scartati.....:'
089700971022     O                       §CTRNO        2   +  1
089800000613
089900030715     Otitvxlps  E            testdett          2
090000000613     O                                              'Upload via Internet'
090100000613     O                                           +1 'Traduzione TIVIN00R -'
090200060622     O                                           +1 'FIVADWWR'
090300000613     O                                           +1 'Testate bolle'
090400000616     O          E            testdett    3
090500000613     O                                           +2 'N° rec'
090600000613     O                                           +3 'Anteprima contenuto'
090700000616     O          E            rigadett    2
090800000613     O                       rrnum               +2
090900000621     O                       recko               +3
091000000616     O          E            findett     2
091100000613     O                       wrkdata
091200000613     O                       wrkora              +1
091300000613     O                       procname            +1
091400000616     O          E            findett     2
091500000613     O                                              'Cliente..................:'
091600060622     O                       VADCCM        z     +1
091700000616     O          E            findett     2
091800000613     O                                              'Riferimento Strategi.....:'
091900000613     O                       vlrhdl              +1
092000000616     O          E            findett     2
092100000613     O                                              'Giusti...................:'
092200000613     O                       §CTROK        2   +  1
092300000616     O          E            findett     2
092400000613     O                                              'Sbagliati e corretti.....:'
092500000613     O                       §CTRMO        2   +  1
092600000616     O          E            findett     2
092700000613     O                                              'Sbagliati e scartati.....:'
092800000613     O                       §CTRNO        2   +  1
092900000613** CMD - COMANDI CL
093000030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
093100030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
093200030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
093300010606
093400000613
