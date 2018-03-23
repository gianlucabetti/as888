000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300070208     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800070208     Ftitvw6p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100070208     Ftitvw6ps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400000313     D*
001500000613     D*------------
001600000613     D* COMANDI
001700000613     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900040113     D*------------
002000040113     D* SCHIEDE A PROGRAMMA
002100040113     D*------------
002200040113     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
002300010330     D*----------------------------------------------------
002400010330     D* DICHIARAZIOINE VARIABILI DI WRK
002500010330     D*----------------------------------------------------
002600010330     D dscmz         e ds                  inz
002700010330     D psds           sds
002800010330     D  procname         *PROC
002900010330     D tivlrds       e ds                  extname(tivlr00f)
003000040706     D tisi95ds      e ds
003100010330     D esito           s              1
003200010330     D prmlit          s             10
003300010330     D prmfir          s             10
003400010330     D wrkesito        s                   like(esito)
003500010330     D wrkdata         s               d
003600010330     D wrkora          s               t
003700010330     D rrnum           s              6  0 INZ(*zeros)
003800010330     D recko           s            150    INZ(*blanks)
003900011113     D depcmd          s            150    INZ(*blanks)
004000040119     D parccm          s              8    INZ(*blanks)
004100040119     D parmbr          s             10    INZ(*blanks)
004200040119     D paropz          s              1    INZ(*blanks)
004300040119     D chkcall         s              1    INZ(*blanks)
004400040706     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
004500070208     D wNT2_SX         s             35    VARYING
004600070208     D wNT2_DX         s             35    VARYING
004700030715     D*------------------
004800030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004900030715     D*------------------
005000030715     D SkSplitFLD      S             10    DIM(100)
005100030715     D SkSplitCSV      S            256    DIM(100)
005200030715     D CharCSV         S              1
005300030715     D CharTXT         S              1
005400030715     D CharNUM         S              1
005500030715     D posDa           S              3  0 INZ(*zeros)
005600030715     D posA            S              3  0 INZ(*zeros)
005700030715     D i               s              3  0 INZ(1)
005800030715     D wGiro           s              1  0 INZ(*zeros)
005900030822     D*------------------
006000030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006100030822     D*------------------
006200030822     D posDaDft        S              3  0 INZ(*zeros)
006300030822     D posADft         S              3  0 INZ(*zeros)
006400030822     D j               s              3  0 INZ(1)
006500030822     D wGiroDft        s              1  0 INZ(*zeros)
006600010330     D*------------------
006700010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
006800010330     D*------------------
006900010330     D WLBDA8          DS                  INZ
007000010330     D  G08DAT                 1      8  0
007100010330     D  G08INV                 9     16  0
007200010330     D  G08ERR                17     17
007300010330     D  G08TGI                18     22  0
007400060327     D*------------------
007500060327     D* DS REPERIMENTO NUMERATORE
007600060327     D*------------------
007700060327     D trul33ds      e ds                  inz
007800060327     D*------------------
007900060327     D* DS ARCHITETTURA
008000060327     D*------------------
008100060327     D kpjba         e ds                  inz
008200031201     D*------------------
008300031201     D* Costanti
008400031201     D*------------------
008500031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008600031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008700010330
008800010330
008900010330
009000990915     C                   time                    wrkdata
009100990915     C                   time                    wrkora
009200000913     C                   reset                   rrnum
009300990921     C                   reset                   esito
009400990921     C                   reset                   wrkesito
009500010601     C*
009600010601     C                   exsr      opeini
009700010605     C                   exsr      rwvab
009800010601     C*
009900010601     C                   seton                                        lr
010000010601
010100010601
010200010601
010300010601     C*--------------------------------------------------------
010400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010500010601     C*--------------------------------------------------------
010600010601     C     PREELA        BEGSR
010700010601     C*
010800010601     C* SE OCCORRE SPEDIRE IN FILIALE
010900010601     C                   if        invfil <> *zeros and
011000010601     C                             flgGiro = '0'
011100010601     C*
011200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011300010601     C                   eval      flgGiro = '1'
011400000724     C*
011500000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
011600000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011700010601     C     invfil        chain     azorg01l
011800000724     C                   if        %found
011900000724     C                   movel(p)  CMD(1)        depcmd
012000020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
012100000724     C*
012200000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
012300011113     C                   Z-ADD     150           LENGH            15 5
012400000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012500000724     C                   PARM                    depcmd
012600000724     C                   PARM                    LENGH
012700000724     C*
012800000724     C                   endif
012900000616     C*
013000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013100000616     C                   movel(p)  CMD(2)        depcmd
013200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
013300000616     C*
013400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
013500011113     C                   Z-ADD     150           LENGH            15 5
013600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013700000616     C                   PARM                    depcmd
013800000616     C                   PARM                    LENGH
013900000613     C*
014000070208     C                   if        not %open(titvw6ps)
014100070208     C                   open      titvw6ps
014200030715     C                   except    testdett
014300000613     C                   endif
014400010601     C*
014500010601     C                   endif
014600010601     C*
014700010601     C                   ENDSR
014800010601     C***
014900010601
015000010601
015100010601
015200010601
015300010601     C*--------------------------------------------------------
015400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
015500010601     C*--------------------------------------------------------
015600010601     C     ENDELA        BEGSR
015700990915     C*
015800000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015900040119     C*
016000040119     C* Effettuo la chiamata al CLLE preposto
016100070208     C                   if        *in60 = *on
016200040119     C                   call(e)   'TITVVTC'
016300040119     C                   parm                    parccm
016400040119     C                   parm                    parmbr
016500040119     C                   parm      '2'           paropz
016600070208     C                   endif
016700050201     C*
016800050201     C* Effettuo lancio TISI95 solo x chiusura
016900050201     C                   CLEAR                   TISI95DS
017000050201     C                   EVAL      I95TLA = 'C'
017100050201     C                   CALL      'TISI95R'
017200050201     C                   PARM                    TISI95DS
017300060327     C*
017400060327     C* Effettuo lancio TRUL33 solo x chiusura
017500060327     C                   clear                   TRUL33DS
017600060327     C                   eval      I33TLA = 'C'
017700060327     C                   movel     TRUL33DS      KPJBU
017800060327     C                   call      'TRUL33R'
017900060327     C                   parm                    KPJBA
018000000613     C*
018100070208     C                   if        %open(titvw6ps)
018200000616     C                   except    findett
018300070208     C                   close     titvw6ps
018400000613     C                   endif
018500000616     C*
018600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018700011113     C                   Z-ADD     150           LENGH            15 5
018800010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
018900000616     C                   PARM                    CMD(3)
019000000616     C                   PARM                    LENGH
019100000616     C*
019200010601     C                   ENDSR
019300010601     C***
019400010601
019500010601
019600010601
019700000613
019800000613     C*--------------------------------------------------------
019900000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
020000000613     C*--------------------------------------------------------
020100000621     C     STPR          BEGSR
020200000613     C*
020300070208     C                   if        not %open(titvw6p)
020400070208     C   50              open      titvw6p
020500990915     C                   endif
020600990915     C*
020700030715     C   50              except    riepilogo
020800990915     C*
020900070208     C                   if        %open(titvw6p)
021000070208     C   50              close     titvw6p
021100990914     C                   endif
021200000613     C*
021300000613     C                   ENDSR
021400000613     C***
021500000613
021600010601
021700010601
021800010330     C*--------------------------------------------------------
021900021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
022000010330     C*--------------------------------------------------------
022100010605     C     RWVAB         BEGSR
022200010330     C*
022300010330     C                   if        not %open(tivin00r)
022400010330     C                   open      tivin00r
022500010330     C                   endif
022600021025     C                   if        not %open(fivabwwr)
022700021025     C                   open      fivabwwr
022800010330     C                   endif
022900040119     C*
023000040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
023100040119     C                   exsr      prevat
023200040119     C*
023300040119     C                   if        chkcall = '0'
023400040119     C*
023500070208     C                   if        *in60 = *on
023600040119     C                   if        not %open(fivatwwr)
023700040119     C                   open      fivatwwr
023800070208     C                   endif
023900040119     C                   endif
024000010330     C*
024100040119     C                   clear                   §CTROKVB          7 0
024200040119     C                   clear                   §CTROKVT          7 0
024300040119     C                   clear                   §CTRMO            7 0
024400040119     C                   clear                   §CTRNO            7 0
024500050330     C*
024600050330     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
024700050330     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
024800070208     C                   exsr      inzvar
024900070208     C                   exsr      impvab
025000050330     C*
025100050330     C                   do        *HIVAL
025200010330     C*
025300040706     C                   read      tivin00r                               70
025400010618     C*
025500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
025600010618     C                   if        vinflg = '1'
025700010618     C                   eval      flgOk = '1'
025800010618     C                   endif
025900050330     C*
026000050330     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
026100050330     C                   z-add     *zeros        wLenVINDTA        4 0
026200050330     C                   z-add     *zeros        wFlgVINDTA        1 0
026300050330     C                   dow       wLenVINDTA < %len(%trim(vindta))
026400050330     C                   eval      wLenVINDTA = wLenVINDTA + 1
026500050330     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
026600050330     C                   z-add     1             wFlgVINDTA
026700050330     C                   leave
026800050330     C                   endif
026900050330     C                   enddo
027000010618     C*
027100050330     C                   if        vindta > *blanks AND
027200050330     C                             wFlgVINDTA = 1
027300010330     C                   add       1             rrnum
027400010330     C*
027500010601     C                   if        *in70 = *off and
027600010330     C                             (vinflg = *blanks
027700010330     C                              or vinflg = '0'
027800010330     C                              or vinflg = '2')
027900010330     C*
028000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
028100010711     C                   if        vinflg = *blanks or vinflg = '0'
028200010711     C                   clear                   vinmsg
028300010711     C                   endif
028400010601     C*
028500010330     C                   exsr      inzvar
028600010605     C                   exsr      impvab
028700010601     C*
028800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
028900010605     C                   eval      depfil = VABLNP
029000010601     C                   exsr      repfil
029100010601     C                   if        depfil = invfil
029200021025     C                   if        vlrpoi = 999
029300030822     C                   move(P)   invfil        VABFGS
029400070208     C   60              move(P)   invfil        VATFGS
029500021025     C                   else
029600030822     C                   move(P)   vlrpoi        VABFGS
029700070208     C   60              move(P)   vlrpoi        VATFGS
029800021025     C                   endif
029900010601     C*
030000010601     C                   exsr      PREELA
030100010601     C*
030200010604     C* Ebbene...
030300010604     C*
030400040119     C  N31              add       1             §CTROKVB
030500040119     C   32              add       1             §CTRMO
030600040119     C   31              add       1             §CTRNO
030700030822     C                   if        wGiro = 2
030800040119     C  N31              exsr      wrivab
030900040119     C                   endif
031000010604     C*
031100010604     C                   if        *in31 = *off and
031200010604     C                             *in32 = *off
031300010604     C                   eval      vinflg = '1'
031400010604     C                   else
031500010604     C                   eval      recko = vindta
031600010604     C                   except    rigadett
031700010604     C                   eval      vinflg = '2'
031800010604     C                   endif
031900010604     C                   endif
032000010601     C*
032100010604     C                   endif
032200010604     C*
032300010330     C                   else
032400010330     C                   eval      vinflg = '1'
032500010330     C                   endif
032600010601     C*
032700010601     C  N70              update    tivin000
032800010330     C*
032900030822     C  N70              enddo
033000040119     C*
033100040119     C                   endif                                                  (endif - chkcall)
033200010601     C*
033300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
033400010601     C                   if        cntNonEl = *zeros or
033500010601     C                             flgMulti = '0'
033600010330     C* Se non ci sono record con errori ...
033700010601     C                   if        §ctrno = 0 and
033800010604     C                             §ctrmo = 0 and
033900010601     C                             flgStato <> '2'
034000010330     C* ... restituisco esito OK.
034100010330     C                   eval      wrkesito = '0'
034200010330     C                   else
034300040119     C                   if        §ctrokvb > 0
034400010330     C                   eval      wrkesito = '1'
034500010330     C                   else
034600010615     C                   if        flgOk = '0'
034700010615     C                   eval      wrkesito = '2'
034800010615     C                   else
034900010615     C                   eval      wrkesito = '6'
035000010615     C                   endif
035100010330     C                   endif
035200010330     C                   endif
035300010601     C                   else
035400010601     C                   eval      wrkesito = '9'
035500010601     C                   endif
035600010330     C*
035700010330     C                   if        %open(tivin00r)
035800010330     C                   close     tivin00r
035900010330     C                   endif
036000021025     C                   if        %open(fivabwwr)
036100021025     C                   close     fivabwwr
036200010330     C                   endif
036300070208     C                   if        *in60 = *on
036400040512     C                   if        %open(fivatwwr)
036500040512     C                   close     fivatwwr
036600070208     C                   endif
036700040512     C                   endif
036800010601     C*
036900010601     C                   if        vlrpoi <> 999
037000010601     C                   eval      invfil = vlrpoi
037100010601     C                   endif
037200010330     C*
037300040119     C                   if        §ctrokvb > 0
037400010601     C                             and invfil > *zeros
037500010330     C                   exsr      invio
037600010330     C                   endif
037700010601     C*
037800010618     C                   if        flgGiro = '1'
037900010601     C                   exsr      endela
038000010618     C                   endif
038100010330     C*
038200010330     C                   ENDSR
038300010330     C***
038400040119
038500040119
038600040119
038700040119     C*----------------------------------------------------*
038800040119     C*  SCARICAMENTO BUFFER RECORDS VAB
038900040119     C*----------------------------------------------------*
039000040119     C     WRIVAB        BEGSR
039100040119     C*
039200040119     C                   write     fivab000                                     => scarico il VAB
039300040119     C*
039400040119     C* Eseguo la scrittura dei "CHI SONO" della spedizione corrente
039500070208     C   60              exsr      wrivat
039600040119     C*
039700040119     C                   ENDSR
039800040119
039900040119
040000040119
040100060327     C*----------------------------------------------------*
040200060327     C*  SCARICAMENTO BUFFER RECORDS VAT
040300060327     C*----------------------------------------------------*
040400060327     C     WRIVAT        BEGSR
040500060327     C*
040600060327     C* Attribuisco la stessa chiave della testata bolle
040700060327     C                   EVAL      VATAAS = VABAAS
040800060327     C                   EVAL      VATLNP = VABLNP
040900060327     C                   EVAL      VATNSP = VABNSP
041000060327     C*
041100060327     C* Quindi se valorizzato il VATNOT scarico il buffer e incremento il contatore x il CMZ
041200060327     C                   IF        VATNOT <> *blanks
041300060327     C                   WRITE     FIVAT000
041400060327     C                   ADD       1             §CTROKVT
041500060327     C                   ENDIF
041600060327     C*
041700060327     C                   ENDSR
041800040119
041900040119
042000040119
042100040119     C*----------------------------------------------------*
042200040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
042300040119     C*----------------------------------------------------*
042400040119     C     PREVAT        BEGSR
042500070208     C*
042600070208     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
042700070208     C                   SETOFF                                       50
042800070208     C                   IF        %subst(vlrppt:1:1) = 'C'
042900070208     C                   SETON                                        60
043000070208     C                   ELSE
043100070208     C                   SETOFF                                       60
043200070208     C                   ENDIF
043300070208     C*
043400070208     C* Se richiesta elaborazione anxhe FNVAT => proseguo
043500070208     C                   if        *in60 = *on
043600040119     C*
043700040119     C* Compongo il nome del membro da dare al FIVATWWR
043800040119     C                   eval      parmbr = vlrhdl
043900040119     C                   movel     'M'           parmbr
044000050330     C                   eval      parccm = %subst(vlrKSC:2:7)
044100040119     C                   eval      paropz = '1'
044200040119     C* Effettuo la chiamata al CLLE preposto
044300040119     C                   call(e)   'TITVVTC'
044400040119     C                   parm                    parccm
044500040119     C                   parm                    parmbr
044600040119     C                   parm                    paropz
044700040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
044800040119     C                   if        %error
044900040119     C                   movel     '1'           chkcall
045000040119     C                   else
045100040119     C                   movel     '0'           chkcall
045200040119     C                   endif
045300070208     C*
045400070208     C                   else
045500070208     C                   movel     '0'           chkcall
045600070208     C                   endif
045700040119     C*
045800040119     C                   ENDSR
045900010601
046000010601
046100010601
046200010330     C*----------------------------------------------------*
046300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
046400010330     C*----------------------------------------------------*
046500040119     C     INZVAR        BEGSR
046600010330     C*
046700030822     C* Inizializzo variabili di wrk
046800020204     C                   Z-ADD     *zeros        Num5_0            5 0
046900070208     C                   EVAL      wNT2_SX = *blanks
047000070208     C                   EVAL      wNT2_DX = *blanks
047100070208     C                   MOVEL     *blanks       wRMN             15
047200030822     C*
047300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
047400030822     C                   CLEAR                   FIVAB000
047500070208     C   60              CLEAR                   FIVAT000
047600030822     C                   CLEAR                   SkSplitCSV
047700030822     C                   EVAL      i = 1
047800030822     C                   EVAL      posDa = *zeros
047900030822     C                   EVAL      posA  = *zeros
048000030822     C*
048100030822     C* Reimposto i valori di default
048200030822     C                   EXSR      DEFCAM
048300030822     C*
048400010330     C                   ENDSR
048500010330     C*----------------------------------------------------*
048600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
048700010330     C*----------------------------------------------------*
048800010330     C     DEFCAM        BEGSR
048900040119     C*
049000040119     C* Imposto i valori d default a programma dei campi fondamentali
049100070208     C                   EVAL      VABCCM = 0054072
049200070208     C   60              EVAL      VATCCM = 0054072
049300070223     C                   EVAL      VABLNP = 089
049400070223     C   60              EVAL      VATLNP = 089
049500070208     C                   EVAL      VABCTR = 100
049600040119     C                   EVAL      VABCBO = '1'
049700030822     C*
049800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
049900030715     C* e delimitatore testo.
050000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050300030822     C*
050400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
050500030822     C                   EVAL      posDaDft = 1
050600030822     C                   EVAL      posADft  = 0
050700030822     C                   EVAL      wGiroDft = 0
050800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
050900030822     C                             posDaDft > 0
051000030822     C*
051100030822     C* Gestisco il 1° giro
051200030822     C                   IF        wGiroDft = 0
051300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
051400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
051500030822     C* Incremento il contatore dei "giri"
051600030822     C                   EVAL      wGiroDft = 1
051700030822     C                   ELSE
051800030822     C                   EVAL      posDaDft = posADft
051900030822     C                   ENDIF
052000030822     C* Eseguo lo scan x trovare la fine del campo corrente
052100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
052200030822     C*
052300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
052400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
052500030822     C                   IF        posDaDft > 0 AND
052600030822     C                             posADft  > 0
052700030822     C* NCL
052800030822     C                   IF        %subst(
052900030822     C                             %subst(vlrppt:posDaDft+1:
053000030822     C                             posADft-posDaDft-1):1:3)
053100030822     C                             = 'NCL'
053200030822     C                   EVAL      PiStr=%trim(%subst(
053300030822     C                             %subst(vlrppt:posDaDft+1:
053400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053500030822     C                   EXSR      CHKNUM
053600030822     C                   IF        PiInt=*on
053700030822     C                   Z-ADD     PiVal         VABNCL
053800030822     C                   ENDIF
053900030822     C                   ENDIF
054000030822     C* CCM
054100030822     C                   IF        %subst(
054200030822     C                             %subst(vlrppt:posDaDft+1:
054300030822     C                             posADft-posDaDft-1):1:3)
054400030822     C                             = 'CCM'
054500030822     C                   EVAL      PiStr=%trim(%subst(
054600030822     C                             %subst(vlrppt:posDaDft+1:
054700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054800030822     C                   EXSR      CHKNUM
054900030822     C                   IF        PiInt=*on
055000030822     C                   Z-ADD     PiVal         VABCCM
055100070208     C   60              Z-ADD     PiVal         VATCCM
055200030822     C                   ENDIF
055300030822     C                   ENDIF
055400030822     C* LNP
055500030822     C                   IF        %subst(
055600030822     C                             %subst(vlrppt:posDaDft+1:
055700030822     C                             posADft-posDaDft-1):1:3)
055800030822     C                             = 'LNP'
055900030822     C                   EVAL      PiStr=%trim(%subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056200030822     C                   EXSR      CHKNUM
056300030822     C                   IF        PiInt=*on
056400030822     C                   Z-ADD     PiVal         VABLNP
056500070208     C   60              Z-ADD     PiVal         VATLNP
056600030822     C                   ENDIF
056700030822     C                   ENDIF
056800030822     C* NRS
056900030822     C                   IF        %subst(
057000030822     C                             %subst(vlrppt:posDaDft+1:
057100030822     C                             posADft-posDaDft-1):1:3)
057200030822     C                             = 'NRS'
057300030822     C                   EVAL      PiStr=%trim(%subst(
057400030822     C                             %subst(vlrppt:posDaDft+1:
057500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057600030822     C                   EXSR      CHKNUM
057700030822     C                   IF        PiInt=*on
057800030822     C                   Z-ADD     PiVal         VABNRS
057900030822     C                   ENDIF
058000030822     C                   ENDIF
058100030822     C* CTR
058200030822     C                   IF        %subst(
058300030822     C                             %subst(vlrppt:posDaDft+1:
058400030822     C                             posADft-posDaDft-1):1:3)
058500030822     C                             = 'CTR'
058600030822     C                   EVAL      PiStr=%trim(%subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058900030822     C                   EXSR      CHKNUM
059000030822     C                   IF        PiInt=*on
059100030822     C                   Z-ADD     PiVal         VABCTR
059200030822     C                   ENDIF
059300030822     C                   ENDIF
059400030822     C* PKB
059500030822     C                   IF        %subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):1:3)
059800030822     C                             = 'PKB'
059900030822     C                   EVAL      PiStr=%trim(%subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060200030822     C                   EXSR      CHKNUM
060300030822     C                   IF        PiNum=*on
060400050330     C                   Z-ADD     PiVal         VABPKB
060500030822     C                   ENDIF
060600030822     C                   ENDIF
060700030822     C* VLB
060800030822     C                   IF        %subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):1:3)
061100030822     C                             = 'VLB'
061200030822     C                   EVAL      PiStr=%trim(%subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061500030822     C                   EXSR      CHKNUM
061600030822     C                   IF        PiNum=*on
061700030822     C                   Z-ADD     PiVal         VABVLB
061800030822     C                   ENDIF
061900030822     C                   ENDIF
062000030822     C* QFT
062100030822     C                   IF        %subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):1:3)
062400030822     C                             = 'QFT'
062500030822     C                   EVAL      PiStr=%trim(%subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062800030822     C                   EXSR      CHKNUM
062900030822     C                   IF        PiNum=*on
063000030822     C                   Z-ADD     PiVal         VABQFT
063100030822     C                   ENDIF
063200030822     C                   ENDIF
063300030822     C* CBO
063400030822     C                   IF        %subst(
063500030822     C                             %subst(vlrppt:posDaDft+1:
063600030822     C                             posADft-posDaDft-1):1:3)
063700030822     C                             = 'CBO'
063800030822     C                   EVAL      VABCBO=%trim(%subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064100030822     C                   ENDIF
064200030822     C* TSP
064300030822     C                   IF        %subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):1:3)
064600030822     C                             = 'TSP'
064700030822     C                   EVAL      VABTSP=%trim(%subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065000030822     C                   ENDIF
065100030822     C* VAS
065200030822     C                   IF        %subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):1:3)
065500030822     C                             = 'VAS'
065600030822     C                   EVAL      VABVAS=%trim(%subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065900030822     C                   ENDIF
066000030822     C* VCA
066100030822     C                   IF        %subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):1:3)
066400030822     C                             = 'VCA'
066500030822     C                   EVAL      VABVCA=%trim(%subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066800030822     C                   ENDIF
066900030822     C* GCA
067000030822     C                   IF        %subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):1:3)
067300030822     C                             = 'GCA'
067400030822     C                   EVAL      VABGCA=%trim(%subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700030822     C                   ENDIF
067800030822     C* CTM
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'CTM'
068300030822     C                   EVAL      VABCTM=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   ENDIF
068700030822     C* FFD
068800030822     C                   IF        %subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):1:3)
069100030822     C                             = 'FFD'
069200030822     C                   EVAL      VABFFD=%trim(%subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500030822     C                   ENDIF
069600030822     C* VAD
069700030822     C                   IF        %subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):1:3)
070000030822     C                             = 'VAD'
070100030822     C                   EVAL      VABVAD=%trim(%subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400030822     C                   ENDIF
070500030822     C* GMA
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'GMA'
071000030822     C                   EVAL      VABGMA=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   ENDIF
071400030822     C* GGA
071500030822     C                   IF        %subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):1:3)
071800030822     C                             = 'GGA'
071900030822     C                   EVAL      VABGGA=%trim(%subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072200030822     C                   ENDIF
072300030822     C* GVA
072400030822     C                   IF        %subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):1:3)
072700030822     C                             = 'GVA'
072800030822     C                   EVAL      VABGVA=%trim(%subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100030822     C                   ENDIF
073200030822     C* TC1
073300030822     C                   IF        %subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):1:3)
073600030822     C                             = 'TC1'
073700030822     C                   EVAL      VABTC1=%trim(%subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000030822     C                   ENDIF
074100030822     C* TC2
074200030822     C                   IF        %subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):1:3)
074500030822     C                             = 'TC2'
074600030822     C                   EVAL      VABTC2=%trim(%subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074900030822     C                   ENDIF
075000030822     C* ...
075100030822     C                   ENDIF
075200030822     C                   ENDDO
075300020204     C*
075400010330     C                   ENDSR
075500010607     C*----------------------------------------------------*
075600021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
075700010607     C*----------------------------------------------------*
075800010607     C     IMPVAB        BEGSR
075900010607     C*
076000010607     C                   SETOFF                                       3132
076100030515     C*
076200010607     C                   MOVEL     datcor        VABAAS
076300070208     C   60              MOVEL     datcor        VATAAS
076400010607     C                   MOVE      datcor        VABMGS
076500010607     C*
076600030715     C**********
076700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
076800030715     C**********
076900030715     C                   IF        wGiro = *zeros
077000040706     C*
077100040706     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
077200040706     C                   EVAL      depvinDTA = vinDTA
077300040113     C*
077400040113     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
077500040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
077600040113     C                                      %trim(intcol(3))
077700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
077800031201     C     minu:maiu     XLATE     vindta        vindta
077900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
078000030715     C                   DOW       posDa <= %len(%trim(vindta))
078100030715     C*
078200030715     C* Gestisco il 1° campo
078300030715     C                   IF        i = 1
078400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
078500030715     C                   EVAL      posDa = 1
078600030715     C* Eseguo lo scan x trovare la fine del primo campo
078700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
078800030715     C* A questo punto "estraggo" il campo corrente
078900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
079000030715     C                                                    (posA-posDa))
079100030715     C* X i campi successivi al 1°
079200030715     C                   ELSE
079300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
079400030715     C                   EVAL      posDa = posA + 1
079500030715     C* Eseguo lo scan x trovare la fine del campo corrente
079600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079700030715     C* Gestisco l'ultimo campo
079800030715     C                   IF        posA = *zeros
079900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
080000030715     C* A questo punto "estraggo" il campo corrente
080100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080200030715     C                                                    (posA-posDa))
080300030715     C                   LEAVE
080400030715     C                   ELSE
080500030715     C* A questo punto "estraggo" il campo corrente
080600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080700030715     C                                                    (posA-posDa))
080800030715     C                   ENDIF
080900030715     C                   ENDIF
081000030715     C* Incremento il contatore d campo
081100030715     C                   EVAL      i = i +1
081200030715     C                   ENDDO
081300030715     C*
081400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
081500030715     C* delimitazione testo
081600030715     C                   EVAL      i = 1
081700030715     C                   DOW       i < %elem(SkSplitFLD)
081800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
081900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
082000030715     C                   EVAL      i = i + 1
082100030715     C                   ENDDO
082200030715     C                   EVAL      wGiro = 1
082300040706     C* Ripristino la 1° riga originale
082400040706     C                   EVAL      vinDTA = depvinDTA
082500030715     C                   ELSE
082600030715     C                   EVAL      wGiro = 2
082700031204     C**********
082800031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
082900031204     C* d separatore campo
083000031204     C**********
083100031204     C                   EVAL      vindta = %trim(vindta)
083200031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
083300031204     C                   EVAL      vindta = %subst(vindta:2)
083400031204     C                   ENDDO
083500031204     C*
083600031204     C                   Z-ADD     *zeros        lunghInput        4 0
083700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
083800031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
083900031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
084000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
084100031204     C                   ENDDO
084200030715     C**********
084300030715     C* Effettuo lo split del campo dati d input
084400030715     C**********
084500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
084600030715     C                   DOW       posDa <= %len(%trim(vindta))
084700030715     C*
084800030715     C* Gestisco il 1° campo
084900030715     C                   IF        i = 1
085000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
085100030715     C                   EVAL      posDa = 1
085200030715     C* Eseguo lo scan x trovare la fine del primo campo
085300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085400030715     C* A questo punto "estraggo" il campo corrente
085500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
085600030715     C                                                    (posA-posDa))
085700030715     C* X i campi successivi al 1°
085800030715     C                   ELSE
085900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
086000030715     C                   EVAL      posDa = posA + 1
086100031204     C* Verifico che nn vi sia il campo nn valorizzato
086200031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
086300031204     C* Se campo nn valorizzato skippo al prossimo
086400031204     C                   EVAL      SkSplitCSV(i) = *blanks
086500031204     C                   EVAL      posA  = posA + 1
086600031204     C                   ELSE
086700030715     C* Eseguo lo scan x trovare la fine del campo corrente
086800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086900030715     C* Gestisco l'ultimo campo
087000030715     C                   IF        posA = *zeros
087100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
087200030715     C* A questo punto "estraggo" il campo corrente
087300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087400030715     C                                                    (posA-posDa))
087500030715     C                   LEAVE
087600030715     C                   ELSE
087700030715     C* A questo punto "estraggo" il campo corrente
087800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087900030715     C                                                    (posA-posDa))
088000030715     C                   ENDIF
088100031204     C                   ENDIF
088200030715     C                   ENDIF
088300030715     C* Incremento il contatore d campo
088400030715     C                   EVAL      i = i +1
088500030715     C                   ENDDO
088600030715     C*
088700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088800030715     C* delimitazione testo
088900030715     C                   EVAL      i = 1
089000030715     C                   DOW       i < %elem(SkSplitCSV)
089100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
089200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
089300030715     C                   EVAL      i = i + 1
089400030715     C                   ENDDO
089500030715     C*
089600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
089700030715     C                   EVAL      i = 1
089800030715     C                   DOW       i < %elem(SkSplitFLD)
089900030715     C*
090000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
090100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
090200030715     C                             SkSplitCSV(i) <> *blanks
090300030721     C*** ATB
090400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
090500030721     C                   EVAL      VABATB = SkSplitCSV(i)
090600030721     C                   ENDIF
090700060327     C*** RSD
090800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
090900060327     C                   EVAL      VABRSD = SkSplitCSV(i)
091000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
091100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
091200030715     C* ==
091300030715     C                   ENDIF
091400030721     C*** RD2
091500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
091600030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
091700030721     C                   ENDIF
091800030715     C*** IND
091900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
092000030715     C                   EVAL      VABIND = SkSplitCSV(i)
092100030715     C                   ENDIF
092200030715     C*** LOD
092300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
092400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
092500030715     C                   ENDIF
092600030715     C*** PRD
092700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
092800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
092900030715     C                   ENDIF
093000030721     C*** NZD
093100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
093200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
093300030721     C                   ENDIF
093400030715     C*** RMA
093500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
093600030715     C                   EVAL      VABRMA = SkSplitCSV(i)
093700070208     C                   EVAL      wRMN = %trim(VABRMA)
093800070208     C                   EVALR     wRMN = %subst(wRMN:1:(%len(%trim(wRMN))-1))
093900070208     C                   EVAL      PiStr=wRMN
094000070208     C                   EXSR      CHKNUM
094100070208     C                   IF        PiInt=*on
094200070208     C                   Z-ADD     PiVal         VABRMN
094300070208     C                   ELSE
094400070208     C                   SETON                                        32
094500070208     C                   EVAL      vinmsg = %trimr(vinmsg)
094600070208     C                             + ' ' + 'VABRMN'
094700070208     C                   ENDIF
094800030715     C                   ENDIF
094900030715     C*** NOT
095000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
095100030715     C                   EVAL      VABNOT = SkSplitCSV(i)
095200030715     C                   ENDIF
095300030715     C*** NT2
095400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
095500030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
095600030715     C                   ENDIF
095700030715     C*** VCA
095800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
095900030715     C                   EVAL      VABVCA = SkSplitCSV(i)
096000030715     C                   ENDIF
096100030715     C*** VAS
096200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
096300030715     C                   EVAL      VABVAS = SkSplitCSV(i)
096400030715     C                   ENDIF
096500030715     C*** FFD
096600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
096700030715     C                   EVAL      VABFFD = SkSplitCSV(i)
096800030715     C                   ENDIF
096900030721     C*** GC1
097000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
097100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
097200030721     C                   ENDIF
097300030721     C*** GC2
097400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
097500030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
097600030721     C                   ENDIF
097700030721     C*** TSP
097800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
097900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
098000030721     C                   ENDIF
098100030721     C*** NAS
098200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
098300030721     C                   EVAL      VABNAS = SkSplitCSV(i)
098400030721     C                   ENDIF
098500030721     C*** TIC
098600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
098700030721     C                   EVAL      VABTIC = SkSplitCSV(i)
098800030721     C                   ENDIF
098900030721     C*** GCA
099000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
099100030721     C                   EVAL      VABGCA = SkSplitCSV(i)
099200030721     C                   ENDIF
099300030721     C*** XCO
099400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
099500030721     C                   EVAL      VABXCO = SkSplitCSV(i)
099600030721     C                   ENDIF
099700030721     C*** CTM
099800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
099900030721     C                   EVAL      VABCTM = SkSplitCSV(i)
100000030721     C                   ENDIF
100100030721     C*** TCR
100200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
100300030721     C                   EVAL      VABTCR = SkSplitCSV(i)
100400030721     C                   ENDIF
100500030721     C*** CTS
100600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
100700030721     C                   EVAL      VABCTS = SkSplitCSV(i)
100800030721     C                   ENDIF
100900030721     C*** FTM
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
101100030721     C                   EVAL      VABFTM = SkSplitCSV(i)
101200030721     C                   ENDIF
101300030721     C*** VAD
101400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
101500030721     C                   EVAL      VABVAD = SkSplitCSV(i)
101600030721     C                   ENDIF
101700030721     C*** GMA
101800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
101900030721     C                   EVAL      VABGMA = SkSplitCSV(i)
102000030721     C                   ENDIF
102100030721     C*** GGA
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
102300030721     C                   EVAL      VABGGA = SkSplitCSV(i)
102400030721     C                   ENDIF
102500030721     C*** GVA
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
102700030721     C                   EVAL      VABGVA = SkSplitCSV(i)
102800030721     C                   ENDIF
102900030721     C*** TC1
103000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
103100030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
103200030721     C                   ENDIF
103300030721     C*** TC2
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
103500030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
103600030721     C                   ENDIF
103700030721     C*** SCL
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
103900030721     C                   EVAL      VABSCL = SkSplitCSV(i)
104000030721     C                   ENDIF
104100030721     C*** RMO
104200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
104300030721     C                   EVAL      VABRMO = SkSplitCSV(i)
104400030721     C                   ENDIF
104500030721     C*** NMO
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
104700030721     C                   EVAL      VABNMO = SkSplitCSV(i)
104800030721     C                   ENDIF
104900040119     C*** CBO
105000040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
105100040706     C                   EVAL      VABCBO = SkSplitCSV(i)
105200040119     C                   ENDIF
105300030721     C*
105400040706     C* Reperisco quindi i campi numerici...
105500040706     C*** NSP
105600040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
105700040706     C                   EVAL      PiStr=SkSplitCSV(i)
105800040706     C                   EXSR      CHKNUM
105900040706     C                   IF        PiInt=*on
106000040706     C                   Z-ADD     PiVal         VABNSP
106100040706     C                   ELSE
106200040706     C                   SETON                                        31
106300040706     C                   EVAL      VABNSP = *zeros
106400040706     C                   EVAL      vinmsg = %trimr(vinmsg)
106500040706     C                             + ' ' + 'VABNSP'
106600040706     C                   ENDIF
106700040706     C                   ENDIF
106800070208     C*** RMN
106900040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
107000060530     C                   EVAL      PiStr=%subst(SkSplitCSV(i):2)
107100040706     C                   EXSR      CHKNUM
107200040706     C                   IF        PiInt=*on
107300040706     C                   Z-ADD     PiVal         VABRMN
107400040706     C                   ELSE
107500040706     C                   SETON                                        32
107600040706     C                   EVAL      VABRMN = 1
107700040706     C                   EVAL      vinmsg = %trimr(vinmsg)
107800060530     C                             + ' ' + 'VABRMN'
107900040706     C                   ENDIF
108000040706     C                   ENDIF
108100030721     C*** CAD
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
108300030721     C                   EVAL      PiStr=SkSplitCSV(i)
108400030721     C                   EXSR      CHKNUM
108500030721     C                   IF        PiInt=*on
108600030721     C                   Z-ADD     PiVal         Num5_0
108700030721     C                   MOVEL(p)  Num5_0        VABCAD
108800030721     C                   ELSE
108900030721     C                   SETON                                        32
109000030721     C                   EVAL      VABCAD = *zeros
109100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109200030721     C                             + ' ' + 'VABCAD'
109300030721     C                   ENDIF
109400030721     C                   ENDIF
109500030721     C*** NCL
109600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
109700030721     C                   EVAL      PiStr=SkSplitCSV(i)
109800030721     C                   EXSR      CHKNUM
109900030721     C                   IF        PiInt=*on
110000030721     C                   Z-ADD     PiVal         VABNCL
110100030721     C                   ELSE
110200030721     C                   SETON                                        32
110300030721     C                   EVAL      VABNCL = *zeros
110400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110500040706     C                             + ' ' + 'VABNCL'
110600030721     C                   ENDIF
110700030721     C                   ENDIF
110800030721     C*** PKB
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
111000030721     C                   EVAL      PiStr=SkSplitCSV(i)
111100030721     C                   EXSR      CHKNUM
111200030721     C                   IF        PiNum=*on
111300050330     C                   Z-ADD(h)  PiVal         VABPKB
111400030721     C                   ELSE
111500030721     C                   SETON                                        32
111600030721     C                   EVAL      VABPKB = *zeros
111700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111800030721     C                             + ' ' + 'VABPKB'
111900030721     C                   ENDIF
112000030721     C                   ENDIF
112100030721     C*** CAS
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
112300030721     C                   EVAL      PiStr=SkSplitCSV(i)
112400030721     C                   EXSR      CHKNUM
112500030721     C                   IF        PiNum=*on
112600030721     C                   Z-ADD     PiVal         VABCAS
112700030721     C                   ELSE
112800030721     C                   SETON                                        32
112900030721     C                   EVAL      VABCAS = *zeros
113000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113100030721     C                             + ' ' + 'VABCAS'
113200030721     C                   ENDIF
113300030721     C                   ENDIF
113400030721     C*** CCM
113500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
113600030721     C                   EVAL      PiStr=SkSplitCSV(i)
113700030721     C                   EXSR      CHKNUM
113800030721     C                   IF        PiInt=*on      AND
113900030721     C                             PiVal<=9999999 AND
114000030721     C                             PiVal>*zeros
114100030721     C                   Z-ADD     PiVal         VABCCM
114200070208     C   60              Z-ADD     PiVal         VATCCM
114300030721     C                   ELSE
114400030721     C                   SETON                                        32
114500030721     C                   EVAL      VABCCM = *zeros
114600070208     C  N60              EVAL      vinmsg = %trimr(vinmsg)
114700070208     C                             + ' ' + 'VABCCM'
114800070208     C   60              EVAL      vinmsg = %trimr(vinmsg)
114900070208     C                             + ' ' + 'VABCCM VATCCM'
115000030721     C                   ENDIF
115100030721     C                   ENDIF
115200030721     C*** LNP
115300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
115400030721     C                   EVAL      PiStr=SkSplitCSV(i)
115500030721     C                   EXSR      CHKNUM
115600030721     C                   IF        PiInt=*on    AND
115700030721     C                             PiVal<=999   AND
115800030721     C                             PiVal>*zeros
115900030721     C                   Z-ADD     PiVal         VABLNP
116000070208     C   60              Z-ADD     PiVal         VATLNP
116100030721     C                   ELSE
116200030721     C                   SETON                                        32
116300030721     C                   EVAL      VABLNP = *zeros
116400070208     C  N60              EVAL      vinmsg = %trimr(vinmsg)
116500070208     C                             + ' ' + 'VABLNP'
116600070208     C   60              EVAL      vinmsg = %trimr(vinmsg)
116700070208     C                             + ' ' + 'VABLNP VATLNP'
116800030721     C                   ENDIF
116900030721     C                   ENDIF
117000030721     C*** NRS
117100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
117200030721     C                   EVAL      PiStr=SkSplitCSV(i)
117300030721     C                   EXSR      CHKNUM
117400030721     C                   IF        PiInt=*on AND
117500030721     C                             PiVal<=99
117600030721     C                   Z-ADD     PiVal         VABNRS
117700030721     C                   ELSE
117800030721     C                   SETON                                        32
117900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118000030721     C                             + ' ' + 'VABNRS'
118100030721     C                   ENDIF
118200030721     C                   ENDIF
118300030721     C*** LNA
118400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
118500030721     C                   EVAL      PiStr=SkSplitCSV(i)
118600030721     C                   EXSR      CHKNUM
118700030721     C                   IF        PiInt=*on    AND
118800030721     C                             PiVal<=999   AND
118900030721     C                             PiVal>*zeros
119000030721     C                   Z-ADD     PiVal         VABLNA
119100030721     C                   ELSE
119200030721     C                   SETON                                        32
119300030721     C                   EVAL      VABLNA = *zeros
119400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119500030721     C                             + ' ' + 'VABLNA'
119600030721     C                   ENDIF
119700030721     C                   ENDIF
119800030721     C*** CTR
119900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
120000030721     C                   EVAL      PiStr=SkSplitCSV(i)
120100030721     C                   EXSR      CHKNUM
120200030721     C                   IF        PiInt=*on  AND
120300030721     C                             PiVal<=999
120400030721     C                   Z-ADD     PiVal         VABCTR
120500030721     C                   ELSE
120600030721     C                   SETON                                        32
120700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120800030721     C                             + ' ' + 'VABCTR'
120900030721     C                   ENDIF
121000030721     C                   ENDIF
121100030721     C*** IAS
121200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
121300030721     C                   EVAL      PiStr=SkSplitCSV(i)
121400030721     C                   EXSR      CHKNUM
121500030721     C                   IF        PiNum=*on
121600030721     C                   Z-ADD     PiVal         VABIAS
121700030721     C                   ELSE
121800030721     C                   SETON                                        32
121900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122000030721     C                             + ' ' + 'VABIAS'
122100030721     C                   ENDIF
122200030721     C                   ENDIF
122300030721     C*** VLB
122400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
122500030721     C                   EVAL      PiStr=SkSplitCSV(i)
122600030721     C                   EXSR      CHKNUM
122700030721     C                   IF        PiNum=*on
122800030721     C                   Z-ADD     PiVal         VABVLB
122900030721     C                   ELSE
123000030721     C                   SETON                                        32
123100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123200030721     C                             + ' ' + 'VABVLB'
123300030721     C                   ENDIF
123400030721     C                   ENDIF
123500030721     C*** QFT
123600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
123700030721     C                   EVAL      PiStr=SkSplitCSV(i)
123800030721     C                   EXSR      CHKNUM
123900030721     C                   IF        PiNum=*on
124000030721     C                   Z-ADD     PiVal         VABQFT
124100030721     C                   ELSE
124200030721     C                   SETON                                        32
124300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124400030721     C                             + ' ' + 'VABQFT'
124500030721     C                   ENDIF
124600030721     C                   ENDIF
124700030721     C*** NCD
124800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
124900030721     C                   EVAL      PiStr=SkSplitCSV(i)
125000030721     C                   EXSR      CHKNUM
125100030721     C                   IF        PiInt=*on      AND
125200030721     C                             PiVal<=9999999
125300030721     C                   Z-ADD     PiVal         VABNCD
125400030721     C                   ELSE
125500030721     C                   SETON                                        32
125600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125700030721     C                             + ' ' + 'VABNCD'
125800030721     C                   ENDIF
125900030721     C                   ENDIF
126000030721     C*** NCA
126100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
126200030721     C                   EVAL      PiStr=SkSplitCSV(i)
126300030721     C                   EXSR      CHKNUM
126400030721     C                   IF        PiInt=*on      AND
126500030721     C                             PiVal<=9999999
126600030721     C                   Z-ADD     PiVal         VABNCA
126700030721     C                   ELSE
126800030721     C                   SETON                                        32
126900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127000030721     C                             + ' ' + 'VABNCA'
127100030721     C                   ENDIF
127200030721     C                   ENDIF
127300030721     C*** ZNC
127400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
127500030721     C                   EVAL      PiStr=SkSplitCSV(i)
127600030721     C                   EXSR      CHKNUM
127700030721     C                   IF        PiInt=*on AND
127800030721     C                             PiVal<=99
127900030721     C                   Z-ADD     PiVal         VABZNC
128000030721     C                   ELSE
128100030721     C                   SETON                                        32
128200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128300030721     C                             + ' ' + 'VABZNC'
128400030721     C                   ENDIF
128500030721     C                   ENDIF
128600030721     C*** DCR
128700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
128800030721     C                   EVAL      PiStr=SkSplitCSV(i)
128900030721     C                   EXSR      CHKNUM
129000030721     C                   IF        PiInt=*on       AND
129100030721     C                             PiVal<=99999999
129200030721     C                   Z-ADD     PiVal         VABDCR
129300030721     C                   ELSE
129400030721     C                   SETON                                        32
129500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129600030721     C                             + ' ' + 'VABDCR'
129700030721     C                   ENDIF
129800030721     C                   ENDIF
129900030721     C*** HCR
130000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
130100030721     C                   EVAL      PiStr=SkSplitCSV(i)
130200030721     C                   EXSR      CHKNUM
130300030721     C                   IF        PiInt=*on   AND
130400030721     C                             PiVal<=9999
130500030721     C                   Z-ADD     PiVal         VABHCR
130600030721     C                   ELSE
130700030721     C                   SETON                                        32
130800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130900030721     C                             + ' ' + 'VABHCR'
131000030721     C                   ENDIF
131100030721     C                   ENDIF
131200030721     C*** VMD
131300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
131400030721     C                   EVAL      PiStr=SkSplitCSV(i)
131500030721     C                   EXSR      CHKNUM
131600030721     C                   IF        PiNum=*on
131700030721     C                   Z-ADD     PiVal         VABVMD
131800030721     C                   ELSE
131900030721     C                   SETON                                        32
132000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132100030721     C                             + ' ' + 'VABVMD'
132200030721     C                   ENDIF
132300030721     C                   ENDIF
132400030721     C*** ANT
132500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
132600030721     C                   EVAL      PiStr=SkSplitCSV(i)
132700030721     C                   EXSR      CHKNUM
132800030721     C                   IF        PiInt=*on        AND
132900030721     C                             PiVal<=999999999
133000030721     C                   Z-ADD     PiVal         VABANT
133100030721     C                   ELSE
133200030721     C                   SETON                                        32
133300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133400030721     C                             + ' ' + 'VABANT'
133500030721     C                   ENDIF
133600030721     C                   ENDIF
133700030721     C*** CMO
133800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
133900030721     C                   EVAL      PiStr=SkSplitCSV(i)
134000030721     C                   EXSR      CHKNUM
134100030721     C                   IF        PiInt=*on
134200030721     C                   Z-ADD     PiVal         Num5_0
134300030721     C                   MOVEL(p)  Num5_0        VABCMO
134400030721     C                   ELSE
134500030721     C                   SETON                                        32
134600030721     C                   EVAL      VABCMO = *zeros
134700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134800030721     C                             + ' ' + 'VABCMO'
134900030721     C                   ENDIF
135000030721     C                   ENDIF
135100040706     C*
135200040706     C* Gestisco i campi "personalizzati"
135300070208     C*** VABNT2_SX
135400070208     C                   IF        %trim(SkSplitFLD(i))='VABNT2_SX'
135500070208     C                   EVAL      wNT2_SX = %trim(SkSplitCSV(i))
135600040706     C                   ENDIF
135700070208     C*** VABNT2_DX
135800070208     C                   IF        %trim(SkSplitFLD(i))='VABNT2_DX'
135900070208     C                   EVAL      wNT2_DX = %trim(SkSplitCSV(i))
136000070208     C                   ENDIF
136100070208     C*** CONSEGNA
136200070208     C                   IF        %trim(SkSplitFLD(i))='CONSEGNA'
136300070212     C                   IF        %trim(SkSplitCSV(i)) = 'C'  OR
136400070212     C                             %trim(SkSplitCSV(i)) = 'RC' OR
136500070212     C                             %trim(SkSplitCSV(i)) = 'CR'
136600070208     C                   ELSE
136700070208     C                   SETON                                        31
136800070208     C                   ENDIF
136900070208     C                   ENDIF
137000030715     C*
137100030715     C                   ENDIF
137200030715     C                   EVAL      i = i + 1
137300030715     C                   ENDDO
137400070208     C*
137500070208     C* Effettuo considerazioni personalizzate
137600070208     C                   MOVEL     wNT2_SX       VABNT2
137700070208     C                   MOVE      wNT2_DX       VABNT2
137800040706     C*
137900040706     C* Se nn valorizzata la provincia destinatario la calcolo tramite TISI95
138000040706     C* Reperisco la provincia dal CAP e dalla località
138100040706     C                   IF        VABCAD <> *blanks
138200040706     C                   CLEAR                   TISI95DS
138300040706     C                   EVAL      I95TCN = '3'
138400040706     C                   Z-ADD     datcor        I95DAT
138500040706     C                   EVAL      I95CAP = VABCAD
138600040706     C                   EVAL      I95LOC = VABLOD
138700040706     C                   CALL      'TISI95R'
138800040706     C                   PARM                    TISI95DS
138900040706     C                   EVAL      VABPRD = O95PRV
139000040706     C                   ENDIF
139100070208     C*
139200060530     C* NSP => Stacco un numeratore da AZNUM se NSP a 0
139300070208     C                   IF        VABNSP = 0
139400060530     C                   clear                   TRUL33DS
139500060530     C                   eval      I33OPE = *zeros
139600060530     C                   eval      I33CNU = 302
139700060530     C                   eval      I33NUM = 1
139800060530     C                   movel     TRUL33DS      KPJBU
139900060530     C                   call      'TRUL33R'
140000060530     C                   parm                    KPJBA
140100060530     C                   movel     KPJBU         TRUL33DS
140200060530     C                   if        O33ERR = *zeros
140300060530     C                   z-add     O33NRF        VABNSP
140400070208     C   60              z-add     O33NRF        VATNSP
140500060530     C                   else
140600070208     C  N60              EVAL      vinmsg = %trimr(vinmsg)
140700070208     C                             + ' ' + 'VABNSP'
140800070208     C   60              EVAL      vinmsg = %trimr(vinmsg)
140900070208     C                             + ' ' + 'VABNSP VATNSP'
141000060530     C                   endif
141100060530     C                   ENDIF
141200040706     C*
141300040706     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
141400040706     C* d valorizzarli sempre entrambi
141500040706     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
141600040706     C                   Z-ADD     VABNSP        VABRMN
141700040706     C                   ENDIF
141800040706     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
141900040706     C                   MOVEL     VABRMN        VABRMA
142000040706     C                   ENDIF
142100040113     C*
142200040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
142300040113     C                   IF        VABCAS > *zeros
142400060327     C                   EVAL      VABVCA = 'EUR'
142500040119     C                   IF        VABCBO = '1'
142600040113     C                   EVAL      VABCBO = '4'
142700040119     C                   ENDIF
142800040119     C                   IF        VABCBO = '2'
142900040119     C                   EVAL      VABCBO = '6'
143000040119     C                   ENDIF
143100040113     C                   ENDIF
143200020204     C*
143300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
143400020204     C                   EXSR      CHKIMPDIV
143500030715     C                   ENDIF
143600020204     C*
143700010607     C                   ENDSR
143800010607     C*----------------------------------------------------*
143900010601
144000020204
144100020204     C*----------------------------------------------------*
144200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
144300020204     C*----------------------------------------------------*
144400020204     C     CHKIMPDIV     BEGSR
144500020204     C*
144600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
144700020204     C                   Z-ADD     *zeros        wrkDec            9 9
144800020204     C*
144900020204     C* Come prima cosa effettuo considerazioni sulla divisa
145000020204     C                   IF        vabIAS > *zeros
145100020204     C                   IF        vabVAS <> 'EUR'
145200020204     C                   EVAL      vabVAS =  'ITL'
145300020204     C                   ENDIF
145400020204     C                   ENDIF
145500020204     C*
145600020204     C                   IF        vabCAS > *zeros
145700020204     C                   IF        vabVCA <> 'EUR'
145800020204     C                   EVAL      vabVCA =  'ITL'
145900020204     C                   ENDIF
146000020204     C                   ENDIF
146100020204     C*
146200020204     C                   IF        vabVMD > *zeros
146300020204     C                   IF        vabVAD <> 'EUR'
146400020204     C                   EVAL      vabVAD =  'ITL'
146500020204     C                   ENDIF
146600020204     C                   ENDIF
146700020204     C*
146800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
146900020204     C                   Z-ADD     vabIAS        wrkDec
147000020204     C                   IF        wrkDec > *zeros
147100020204     C                   IF        vabVAS = 'ITL'
147200020204     C                   EVAL      vabIAS = *zeros
147300020204     C                   ENDIF
147400020204     C                   ENDIF
147500020204     C*
147600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
147700020204     C                   Z-ADD     vabCAS        wrkDec
147800020204     C                   IF        wrkDec > *zeros
147900020204     C                   IF        vabVCA = 'ITL'
148000020204     C                   EVAL      vabCAS = *zeros
148100020204     C                   ENDIF
148200020204     C                   ENDIF
148300020204     C*
148400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
148500020204     C                   Z-ADD     vabVMD        wrkDec
148600020204     C                   IF        wrkDec > *zeros
148700020204     C                   IF        vabVAD = 'ITL'
148800020204     C                   EVAL      vabVMD = *zeros
148900020204     C                   ENDIF
149000020204     C                   ENDIF
149100020204     C*
149200020204     C                   ENDSR
149300020204     C***
149400020204
149500010330
149600010330
149700010330     C*----------------------------------------------------*
149800010330     C*  CONTROLLO NUMERICITA' CAMPI
149900010330     C*----------------------------------------------------*
150000010330     C     CHKNUM        BEGSR
150100010330     C*
150200010606     C                   IF        PiDecChr = *blanks
150300030715     C                   EVAL      PiDecChr = CharNUM
150400010606     C                   ENDIF
150500010606     C*
150600010606     C                   CALL(e)   'ISNUMERIC'
150700010330     C                   PARM                    PiStr            30
150800010606     C                   PARM                    PiDecChr          1
150900010330     C                   PARM      *ZEROS        PiVal            30 9
151000010330     C                   PARM      '0'           PiInt             1
151100010330     C                   PARM      '0'           PiNum             1
151200010330     C                   IF        %error
151300010606     C                   EVAL      PiNum=*off
151400010330     C                   ENDIF
151500010330     C*
151600010330     C                   ENDSR
151700010330     C***
151800010330
151900010601
152000010601
152100010601
152200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
152300010601     C     repfil        BEGSR
152400010601     C*
152500010601     C                   if        invfil = *zeros and
152600010601     C                             depfil > *zeros and
152700010629     C                             (vinflg = *blanks or
152800010629     C                              vinflg = *zeros)
152900010601     C
153000010601     C                   eval      invfil = depfil
153100010601     C                   endif
153200010601     C*
153300010601     C                   if        depfil <> invfil and
153400010601     C                             invfil > *zeros
153500010601     C                   eval      flgMulti = '1'
153600010601     C                   if        vinflg = *blanks
153700010601     C                   add       1             cntNonEl
153800010601     C                   endif
153900010601     C                   endif
154000010601     C*
154100010601     C                   if        vinflg = '2'
154200010601     C                   eval      flgStato = '2'
154300010601     C                   endif
154400010601     C*
154500010601     C                   ENDSR
154600010601     C***
154700010601
154800010601
154900010601
155000010330
155100040119      /TITLE Invio dei dati al punto operativo.
155200040119     C     invio         BEGSR
155300040119     C*
155400040119     C* 1° invio FIVAT
155500070208     C                   if        *in60 = *on
155600040119     C                   reset                   dscmz
155700040119     C                   move      vlrpoi        cmzdst
155800040119     C                   eval      cmzfld = 'FIVATWWR'
155900040119     C                   eval      cmzmbd = vlrhdl
156000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
156100040119     C***                if        prmfir = *blanks
156200040119     C                   eval      cmzfla = 'FIVAT00F'
156300040119     C                   eval      cmzmba = 'FIVAT00F'
156400040119     C***                else
156500040119     C***                eval      cmzfla = prmfir
156600040119     C***                eval      cmzmba = prmfir
156700040119     C***                endif
156800040119     C                   eval      cmznrr = *zeros
156900040119     C                   move      §ctrokvt      cmznrr
157000040119     C                   eval      cmzlba = vlrfl1
157100040119     C                   call(e)   'TIS711C'
157200040119     C                   parm                    dscmz
157300040119     C                   parm      *blanks       esito
157400070208     C                   endif
157500040119     C                   if        %error
157600040119     C                             or cmzerr = '1'
157700040119     C                             or esito  = '1'
157800040119     C                   eval      wrkesito = '3'
157900040119     C                   else
158000040119     C*
158100040119     C* 2° invio FIVAB
158200040119     C                   reset                   dscmz
158300040119     C                   move      vlrpoi        cmzdst
158400040119     C                   eval      cmzfld = vlrfou
158500040119     C                   eval      cmzmbd = vlrhdl
158600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
158700040119     C***                if        prmfir = *blanks
158800040119     C                   eval      cmzfla = 'FIVAB00F'
158900040119     C                   eval      cmzmba = 'FIVAB00F'
159000040119     C***                else
159100040119     C***                eval      cmzfla = prmfir
159200040119     C***                eval      cmzmba = prmfir
159300040119     C***                endif
159400040119     C                   eval      cmznrr = *zeros
159500040119     C                   move      §ctrokvb      cmznrr
159600040119     C                   eval      cmzlba = vlrfl1
159700040119     C                   call(e)   'TIS711C'
159800040119     C                   parm                    dscmz
159900040119     C                   parm      *blanks       esito
160000040119     C                   if        %error
160100040119     C                             or cmzerr = '1'
160200040119     C                             or esito  = '1'
160300040119     C                   eval      wrkesito = '3'
160400040119     C                   endif
160500040119     C                   endif
160600040119     C*
160700040119     C                   ENDSR
160800040119     C***
160900010601
161000010601
161100010601
161200010601
161300010601      /TITLE Invio dei dati al punto operativo.
161400010601     C     opeini        BEGSR
161500010601     C*
161600010601     C* Inizializzo flag e contatori operativi
161700010601     C                   movel     '0'           flgGiro           1
161800010601     C                   movel     '0'           flgMulti          1
161900010601     C                   movel     '1'           flgStato          1
162000010615     C                   movel     '0'           flgOk             1
162100010601     C                   z-add     *zeros        cntNonEl         10 0
162200010601     C                   z-add     *zeros        depfil            3 0
162300010601     C                   z-add     *zeros        invfil            3 0
162400010601     C*
162500010601     C                   ENDSR
162600010601     C***
162700010601
162800010601
162900050421
163000050421
163100070411
163200070411     C     *pssr         BEGSR
163300070411     C*
163400070411     C                   if        %open(tivin00r)
163500070411     C                   close     tivin00r
163600070411     C                   endif
163700070411     C                   if        %open(fivabwwr)
163800070411     C                   close     fivabwwr
163900070411     C                   endif
164000070411     C                   if        %open(fivatwwr)
164100070411     C                   close     fivatwwr
164200070411     C                   endif
164300070411     C*
164400070411     C* Effettuo la chiamata al CLLE preposto
164500070411     C                   call(e)   'TITVVTC'
164600070411     C                   parm                    parccm
164700070411     C                   parm                    parmbr
164800070411     C                   parm      '2'           paropz
164900070411     C*
165000070411     C                   eval      wrkesito = '2'
165100070411     C*
165200070411     C                   seton                                        LR
165300070411     C*
165400070411     C                   ENDSR     '*CANCL'
165500070411     C***
165600070411
165700050421
165800010601
165900010330
166000010330
166100000613     C     *inzsr        BEGSR
166200990910     C*
166300990910     C     *entry        plist
166400990920     C                   parm                    tivlrds
166500990921     C                   parm      wrkesito      esito
166600000724     C                   parm                    prmlit
166700000710     C                   parm                    prmfir
166800010330     C*
166900010330     C* CALCOLA LA DATA CORRENTE
167000010330     C                   time                    wn14             14 0
167100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
167200010330     C                   z-add     wn8           g08dat
167300010330     C                   z-add     *zeros        g08inv
167400010330     C                   movel     '0'           g08err
167500010330     C                   call      'XSRDA8'
167600010330     C                   parm                    wlbda8
167700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
167800000613     C*
167900000613     C                   ENDSR
168000000613     C***
168100990908
168200070208     Otitvw6p   E            riepilogo         2
168300990915     O                                              'Upload via Internet'
168400990915     O                                           +1 'Traduzione TIVIN00R -'
168500021025     O                                           +1 'FIVABWWR'
168600990915     O                                           +1 'Testate bolle'
168700990915     O          E            riepilogo   2
168800990915     O                       wrkdata
168900990915     O                       wrkora              +1
169000990915     O                       procname            +1
169100990915     O          E            riepilogo   2
169200990915     O                                              'Cliente..................:'
169300010605     O                       VABCCM        z     +1
169400990915     O          E            riepilogo   2
169500990920     O                                              'Riferimento Strategi.....:'
169600990920     O                       vlrhdl              +1
169700990915     O          E            riepilogo   2
169800990915     O                                              'Giusti...................:'
169900040119     O                       §CTROKVB      2   +  1
170000990915     O          E            riepilogo   2
170100990915     O                                              'Sbagliati e corretti.....:'
170200971022     O                       §CTRMO        2   +  1
170300990915     O          E            riepilogo   2
170400990915     O                                              'Sbagliati e scartati.....:'
170500971022     O                       §CTRNO        2   +  1
170600000613
170700070208     Otitvw6ps  E            testdett          2
170800000613     O                                              'Upload via Internet'
170900000613     O                                           +1 'Traduzione TIVIN00R -'
171000021025     O                                           +1 'FIVABWWR'
171100000613     O                                           +1 'Testate bolle'
171200000616     O          E            testdett    3
171300000613     O                                           +2 'N° rec'
171400000613     O                                           +3 'Anteprima contenuto'
171500000616     O          E            rigadett    2
171600000613     O                       rrnum               +2
171700000621     O                       recko               +3
171800000616     O          E            findett     2
171900000613     O                       wrkdata
172000000613     O                       wrkora              +1
172100000613     O                       procname            +1
172200000616     O          E            findett     2
172300000613     O                                              'Cliente..................:'
172400010605     O                       VABCCM        z     +1
172500000616     O          E            findett     2
172600000613     O                                              'Riferimento Strategi.....:'
172700000613     O                       vlrhdl              +1
172800000616     O          E            findett     2
172900000613     O                                              'Giusti...................:'
173000040119     O                       §CTROKVB      2   +  1
173100000616     O          E            findett     2
173200000613     O                                              'Sbagliati e corretti.....:'
173300000613     O                       §CTRMO        2   +  1
173400000616     O          E            findett     2
173500000613     O                                              'Sbagliati e scartati.....:'
173600000613     O                       §CTRNO        2   +  1
173700000613** CMD - COMANDI CL
173800070208OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVW6P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
173900070208OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVW6PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
174000070208DLTOVR FILE(TITVW6P TITVW6PS) LVL(*)
174100010606
174200000613
174300040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
174400070208FILL1;VABNT2_SX;VABNT2_DX;FILL2;VABRMA;FILL3;VABRSD;
174500070208VABIND;VABCAD;VABLOD;VABPRD;VABNCL;VABPKB;CONSEGNA
174600040113
