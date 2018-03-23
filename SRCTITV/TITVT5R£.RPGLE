000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800060628     Ftitvt5p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100060628     Ftitvt5ps  O    f  198        PRINTER usropn
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
004500030715     D*------------------
004600030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004700030715     D*------------------
004800030715     D SkSplitFLD      S             10    DIM(100)
004900030715     D SkSplitCSV      S            256    DIM(100)
005000030715     D CharCSV         S              1
005100030715     D CharTXT         S              1
005200030715     D CharNUM         S              1
005300030715     D posDa           S              3  0 INZ(*zeros)
005400030715     D posA            S              3  0 INZ(*zeros)
005500030715     D i               s              3  0 INZ(1)
005600030715     D wGiro           s              1  0 INZ(*zeros)
005700030822     D*------------------
005800030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005900030822     D*------------------
006000030822     D posDaDft        S              3  0 INZ(*zeros)
006100030822     D posADft         S              3  0 INZ(*zeros)
006200030822     D j               s              3  0 INZ(1)
006300030822     D wGiroDft        s              1  0 INZ(*zeros)
006400010330     D*------------------
006500010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
006600010330     D*------------------
006700010330     D WLBDA8          DS                  INZ
006800010330     D  G08DAT                 1      8  0
006900010330     D  G08INV                 9     16  0
007000010330     D  G08ERR                17     17
007100010330     D  G08TGI                18     22  0
007200060628     D*------------------
007300060628     D* DS REPERIMENTO NUMERATORE
007400060628     D*------------------
007500060628     D trul33ds      e ds                  inz
007600060628     D*------------------
007700060628     D* DS ARCHITETTURA
007800060628     D*------------------
007900060628     D kpjba         e ds                  inz
008000031201     D*------------------
008100031201     D* Costanti
008200031201     D*------------------
008300031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008400031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008500010330
008600010330
008700010330
008800990915     C                   time                    wrkdata
008900990915     C                   time                    wrkora
009000000913     C                   reset                   rrnum
009100990921     C                   reset                   esito
009200990921     C                   reset                   wrkesito
009300010601     C*
009400010601     C                   exsr      opeini
009500010605     C                   exsr      rwvab
009600010601     C*
009700010601     C                   seton                                        lr
009800010601
009900010601
010000010601
010100010601     C*--------------------------------------------------------
010200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010300010601     C*--------------------------------------------------------
010400010601     C     PREELA        BEGSR
010500010601     C*
010600010601     C* SE OCCORRE SPEDIRE IN FILIALE
010700010601     C                   if        invfil <> *zeros and
010800010601     C                             flgGiro = '0'
010900010601     C*
011000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011100010601     C                   eval      flgGiro = '1'
011200000724     C*
011300000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
011400000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011500010601     C     invfil        chain     azorg01l
011600000724     C                   if        %found
011700000724     C                   movel(p)  CMD(1)        depcmd
011800020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011900000724     C*
012000000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
012100011113     C                   Z-ADD     150           LENGH            15 5
012200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012300000724     C                   PARM                    depcmd
012400000724     C                   PARM                    LENGH
012500000724     C*
012600000724     C                   endif
012700000616     C*
012800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012900000616     C                   movel(p)  CMD(2)        depcmd
013000000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
013100000616     C*
013200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
013300011113     C                   Z-ADD     150           LENGH            15 5
013400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013500000616     C                   PARM                    depcmd
013600000616     C                   PARM                    LENGH
013700000613     C*
013800060628     C                   if        not %open(titvt5ps)
013900060628     C                   open      titvt5ps
014000030715     C                   except    testdett
014100000613     C                   endif
014200010601     C*
014300010601     C                   endif
014400010601     C*
014500010601     C                   ENDSR
014600010601     C***
014700010601
014800010601
014900010601
015000010601
015100010601     C*--------------------------------------------------------
015200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
015300010601     C*--------------------------------------------------------
015400010601     C     ENDELA        BEGSR
015500990915     C*
015600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015700040119     C*
015800040119     C* Effettuo la chiamata al CLLE preposto
015900040119     C                   call(e)   'TITVVTC'
016000040119     C                   parm                    parccm
016100040119     C                   parm                    parmbr
016200040119     C                   parm      '2'           paropz
016300050201     C*
016400050201     C* Effettuo lancio TISI95 solo x chiusura
016500050201     C                   CLEAR                   TISI95DS
016600050201     C                   EVAL      I95TLA = 'C'
016700050201     C                   CALL      'TISI95R'
016800050201     C                   PARM                    TISI95DS
016900000613     C*
017000060628     C                   if        %open(titvt5ps)
017100000616     C                   except    findett
017200060628     C                   close     titvt5ps
017300000613     C                   endif
017400000616     C*
017500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
017600011113     C                   Z-ADD     150           LENGH            15 5
017700010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
017800000616     C                   PARM                    CMD(3)
017900000616     C                   PARM                    LENGH
018000000616     C*
018100010601     C                   ENDSR
018200010601     C***
018300010601
018400010601
018500010601
018600000613
018700000613     C*--------------------------------------------------------
018800000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018900000613     C*--------------------------------------------------------
019000000621     C     STPR          BEGSR
019100000613     C*
019200060628     C                   if        not %open(titvt5p)
019300060628     C   50              open      titvt5p
019400990915     C                   endif
019500990915     C*
019600030715     C   50              except    riepilogo
019700990915     C*
019800060628     C                   if        %open(titvt5p)
019900060628     C   50              close     titvt5p
020000990914     C                   endif
020100000613     C*
020200000613     C                   ENDSR
020300000613     C***
020400000613
020500010601
020600010601
020700010330     C*--------------------------------------------------------
020800021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
020900010330     C*--------------------------------------------------------
021000010605     C     RWVAB         BEGSR
021100010330     C*
021200010330     C                   if        not %open(tivin00r)
021300010330     C                   open      tivin00r
021400010330     C                   endif
021500021025     C                   if        not %open(fivabwwr)
021600021025     C                   open      fivabwwr
021700010330     C                   endif
021800040119     C*
021900040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
022000040119     C                   exsr      prevat
022100040119     C*
022200040119     C                   if        chkcall = '0'
022300040119     C*
022400040119     C                   if        not %open(fivatwwr)
022500040119     C                   open      fivatwwr
022600040119     C                   endif
022700010330     C*
022800040119     C                   clear                   §CTROKVB          7 0
022900040119     C                   clear                   §CTROKVT          7 0
023000040119     C                   clear                   §CTRMO            7 0
023100040119     C                   clear                   §CTRNO            7 0
023200050330     C*
023300050330     C                   do        *HIVAL
023400010330     C*
023500040706     C                   read      tivin00r                               70
023600010618     C*
023700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
023800010618     C                   if        vinflg = '1'
023900010618     C                   eval      flgOk = '1'
024000010618     C                   endif
024100050330     C*
024200050330     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
024300050330     C                   z-add     *zeros        wLenVINDTA        4 0
024400050330     C                   z-add     *zeros        wFlgVINDTA        1 0
024500050330     C                   dow       wLenVINDTA < %len(%trim(vindta))
024600050330     C                   eval      wLenVINDTA = wLenVINDTA + 1
024700050330     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
024800050330     C                   z-add     1             wFlgVINDTA
024900050330     C                   leave
025000050330     C                   endif
025100050330     C                   enddo
025200010618     C*
025300050330     C                   if        vindta > *blanks AND
025400050330     C                             wFlgVINDTA = 1
025500010330     C                   add       1             rrnum
025600010330     C*
025700010601     C                   if        *in70 = *off and
025800010330     C                             (vinflg = *blanks
025900010330     C                              or vinflg = '0'
026000010330     C                              or vinflg = '2')
026100010330     C*
026200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
026300010711     C                   if        vinflg = *blanks or vinflg = '0'
026400010711     C                   clear                   vinmsg
026500010711     C                   endif
026600010601     C*
026700010330     C                   exsr      inzvar
026800010605     C                   exsr      impvab
026900010601     C*
027000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
027100010605     C                   eval      depfil = VABLNP
027200010601     C                   exsr      repfil
027300010601     C                   if        depfil = invfil
027400021025     C                   if        vlrpoi = 999
027500030822     C                   move(P)   invfil        VABFGS
027600040119     C                   move(P)   invfil        VATFGS
027700021025     C                   else
027800030822     C                   move(P)   vlrpoi        VABFGS
027900040119     C                   move(P)   vlrpoi        VATFGS
028000021025     C                   endif
028100010601     C*
028200010601     C                   exsr      PREELA
028300010601     C*
028400010604     C* Ebbene...
028500010604     C*
028600040119     C  N31              add       1             §CTROKVB
028700040119     C   32              add       1             §CTRMO
028800040119     C   31              add       1             §CTRNO
028900030822     C                   if        wGiro = 2
029000040119     C  N31              exsr      wrivab
029100040119     C                   endif
029200010604     C*
029300010604     C                   if        *in31 = *off and
029400010604     C                             *in32 = *off
029500010604     C                   eval      vinflg = '1'
029600010604     C                   else
029700010604     C                   eval      recko = vindta
029800010604     C                   except    rigadett
029900010604     C                   eval      vinflg = '2'
030000010604     C                   endif
030100010604     C                   endif
030200010601     C*
030300010604     C                   endif
030400010604     C*
030500010330     C                   else
030600010330     C                   eval      vinflg = '1'
030700010330     C                   endif
030800010601     C*
030900010601     C  N70              update    tivin000
031000010330     C*
031100030822     C  N70              enddo
031200040119     C*
031300040119     C                   endif                                                  (endif - chkcall)
031400010601     C*
031500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
031600010601     C                   if        cntNonEl = *zeros or
031700010601     C                             flgMulti = '0'
031800010330     C* Se non ci sono record con errori ...
031900010601     C                   if        §ctrno = 0 and
032000010604     C                             §ctrmo = 0 and
032100010601     C                             flgStato <> '2'
032200010330     C* ... restituisco esito OK.
032300010330     C                   eval      wrkesito = '0'
032400010330     C                   else
032500040119     C                   if        §ctrokvb > 0
032600010330     C                   eval      wrkesito = '1'
032700010330     C                   else
032800010615     C                   if        flgOk = '0'
032900010615     C                   eval      wrkesito = '2'
033000010615     C                   else
033100010615     C                   eval      wrkesito = '6'
033200010615     C                   endif
033300010330     C                   endif
033400010330     C                   endif
033500010601     C                   else
033600010601     C                   eval      wrkesito = '9'
033700010601     C                   endif
033800010330     C*
033900010330     C                   if        %open(tivin00r)
034000010330     C                   close     tivin00r
034100010330     C                   endif
034200021025     C                   if        %open(fivabwwr)
034300021025     C                   close     fivabwwr
034400010330     C                   endif
034500040512     C                   if        %open(fivatwwr)
034600040512     C                   close     fivatwwr
034700040512     C                   endif
034800010601     C*
034900010601     C                   if        vlrpoi <> 999
035000010601     C                   eval      invfil = vlrpoi
035100010601     C                   endif
035200010330     C*
035300040119     C                   if        §ctrokvb > 0
035400010601     C                             and invfil > *zeros
035500010330     C                   exsr      invio
035600010330     C                   endif
035700010601     C*
035800010618     C                   if        flgGiro = '1'
035900010601     C                   exsr      endela
036000010618     C                   endif
036100010330     C*
036200010330     C                   ENDSR
036300010330     C***
036400040119
036500040119
036600040119
036700040119     C*----------------------------------------------------*
036800040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036900040119     C*----------------------------------------------------*
037000040119     C     WRIVAB        BEGSR
037100040119     C*
037200040119     C                   write     fivab000                                     => scarico il VAB
037300040119     C*
037400060628     C* Eseguo la scrittura del'estensione testata bolle
037500060628     C                   exsr      wrivat                                       => scarico il VAT
037600040119     C*
037700040119     C                   ENDSR
037800040119
037900040119
038000040119
038100040119     C*----------------------------------------------------*
038200040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038300040119     C*----------------------------------------------------*
038400040706     C     WRIVAT        BEGSR
038500040706     C*
038600040706     C* Attribuisco lo stesso numero numero spedizione del file testate
038700060628     C                   EVAL      VATNSP = VABNSP
038800040119     C*
038900060628     C                   EVAL      VATTRC = 'B'
039000060628     C*
039100060628     C* Scarico il buffer solo se valorizzato
039200060628     C                   if        VATNOT <> *blanks
039300050330     C                   write     FIVAT000
039400060628     C                   endif
039500040119     C*
039600040119     C                   ENDSR
039700040119
039800040119
039900040119
040000040119     C*----------------------------------------------------*
040100040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
040200040119     C*----------------------------------------------------*
040300040119     C     PREVAT        BEGSR
040400040119     C*
040500040119     C* Compongo il nome del membro da dare al FIVATWWR
040600040119     C                   eval      parmbr = vlrhdl
040700040119     C                   movel     'M'           parmbr
040800060628     C                   eval      parccm = vlrKSC
040900040119     C                   eval      paropz = '1'
041000040119     C* Effettuo la chiamata al CLLE preposto
041100040119     C                   call(e)   'TITVVTC'
041200040119     C                   parm                    parccm
041300040119     C                   parm                    parmbr
041400040119     C                   parm                    paropz
041500040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
041600040119     C                   if        %error
041700040119     C                   movel     '1'           chkcall
041800040119     C                   else
041900040119     C                   movel     '0'           chkcall
042000040119     C                   endif
042100040119     C*
042200040119     C                   ENDSR
042300010601
042400010601
042500010601
042600010330     C*----------------------------------------------------*
042700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
042800010330     C*----------------------------------------------------*
042900040119     C     INZVAR        BEGSR
043000010330     C*
043100030822     C* Inizializzo variabili di wrk
043200020204     C                   Z-ADD     *zeros        Num5_0            5 0
043300030822     C*
043400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
043500030822     C                   CLEAR                   FIVAB000
043600040119     C                   CLEAR                   FIVAT000
043700030822     C                   CLEAR                   SkSplitCSV
043800030822     C                   EVAL      i = 1
043900030822     C                   EVAL      posDa = *zeros
044000030822     C                   EVAL      posA  = *zeros
044100030822     C*
044200030822     C* Reimposto i valori di default
044300030822     C                   EXSR      DEFCAM
044400030822     C*
044500010330     C                   ENDSR
044600010330     C*----------------------------------------------------*
044700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
044800010330     C*----------------------------------------------------*
044900010330     C     DEFCAM        BEGSR
045000040119     C*
045100040119     C* Imposto i valori d default a programma dei campi fondamentali
045200060628     C                   EVAL      VABCCM = 1200960
045300060628     C                   EVAL      VATCCM = 1200960
045400060628     C                   EVAL      VABLNP = 120
045500060628     C                   EVAL      VATLNP = 120
045600060628     C                   EVAL      VABCTR = 300
045700040119     C                   EVAL      VABCBO = '1'
045800060703     C                   EVAL      VABVAS = 'EUR'
045900060703     C                   EVAL      VABVCA = 'EUR'
046000030822     C*
046100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
046200030715     C* e delimitatore testo.
046300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
046400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
046500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
046600050330     C*
046700050330     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
046800050330     C                   IF        %subst(vlrppt:1:1) = 'S'
046900050330     C                   SETON                                        50
047000050330     C                   ELSE
047100050330     C                   SETOFF                                       50
047200050330     C                   ENDIF
047300030822     C*
047400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
047500030822     C                   EVAL      posDaDft = 1
047600030822     C                   EVAL      posADft  = 0
047700030822     C                   EVAL      wGiroDft = 0
047800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
047900030822     C                             posDaDft > 0
048000030822     C*
048100030822     C* Gestisco il 1° giro
048200030822     C                   IF        wGiroDft = 0
048300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
048400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
048500030822     C* Incremento il contatore dei "giri"
048600030822     C                   EVAL      wGiroDft = 1
048700030822     C                   ELSE
048800030822     C                   EVAL      posDaDft = posADft
048900030822     C                   ENDIF
049000030822     C* Eseguo lo scan x trovare la fine del campo corrente
049100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
049200030822     C*
049300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
049400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
049500030822     C                   IF        posDaDft > 0 AND
049600030822     C                             posADft  > 0
049700030822     C* NCL
049800030822     C                   IF        %subst(
049900030822     C                             %subst(vlrppt:posDaDft+1:
050000030822     C                             posADft-posDaDft-1):1:3)
050100030822     C                             = 'NCL'
050200030822     C                   EVAL      PiStr=%trim(%subst(
050300030822     C                             %subst(vlrppt:posDaDft+1:
050400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050500030822     C                   EXSR      CHKNUM
050600030822     C                   IF        PiInt=*on
050700030822     C                   Z-ADD     PiVal         VABNCL
050800030822     C                   ENDIF
050900030822     C                   ENDIF
051000030822     C* CCM
051100030822     C                   IF        %subst(
051200030822     C                             %subst(vlrppt:posDaDft+1:
051300030822     C                             posADft-posDaDft-1):1:3)
051400030822     C                             = 'CCM'
051500030822     C                   EVAL      PiStr=%trim(%subst(
051600030822     C                             %subst(vlrppt:posDaDft+1:
051700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051800030822     C                   EXSR      CHKNUM
051900030822     C                   IF        PiInt=*on
052000030822     C                   Z-ADD     PiVal         VABCCM
052100040119     C                   Z-ADD     PiVal         VATCCM
052200030822     C                   ENDIF
052300030822     C                   ENDIF
052400030822     C* LNP
052500030822     C                   IF        %subst(
052600030822     C                             %subst(vlrppt:posDaDft+1:
052700030822     C                             posADft-posDaDft-1):1:3)
052800030822     C                             = 'LNP'
052900030822     C                   EVAL      PiStr=%trim(%subst(
053000030822     C                             %subst(vlrppt:posDaDft+1:
053100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053200030822     C                   EXSR      CHKNUM
053300030822     C                   IF        PiInt=*on
053400030822     C                   Z-ADD     PiVal         VABLNP
053500040119     C                   Z-ADD     PiVal         VATLNP
053600030822     C                   ENDIF
053700030822     C                   ENDIF
053800030822     C* NRS
053900030822     C                   IF        %subst(
054000030822     C                             %subst(vlrppt:posDaDft+1:
054100030822     C                             posADft-posDaDft-1):1:3)
054200030822     C                             = 'NRS'
054300030822     C                   EVAL      PiStr=%trim(%subst(
054400030822     C                             %subst(vlrppt:posDaDft+1:
054500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054600030822     C                   EXSR      CHKNUM
054700030822     C                   IF        PiInt=*on
054800030822     C                   Z-ADD     PiVal         VABNRS
054900030822     C                   ENDIF
055000030822     C                   ENDIF
055100030822     C* CTR
055200030822     C                   IF        %subst(
055300030822     C                             %subst(vlrppt:posDaDft+1:
055400030822     C                             posADft-posDaDft-1):1:3)
055500030822     C                             = 'CTR'
055600030822     C                   EVAL      PiStr=%trim(%subst(
055700030822     C                             %subst(vlrppt:posDaDft+1:
055800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055900030822     C                   EXSR      CHKNUM
056000030822     C                   IF        PiInt=*on
056100030822     C                   Z-ADD     PiVal         VABCTR
056200030822     C                   ENDIF
056300030822     C                   ENDIF
056400030822     C* PKB
056500030822     C                   IF        %subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):1:3)
056800030822     C                             = 'PKB'
056900030822     C                   EVAL      PiStr=%trim(%subst(
057000030822     C                             %subst(vlrppt:posDaDft+1:
057100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057200030822     C                   EXSR      CHKNUM
057300030822     C                   IF        PiNum=*on
057400050330     C                   Z-ADD     PiVal         VABPKB
057500030822     C                   ENDIF
057600030822     C                   ENDIF
057700030822     C* VLB
057800030822     C                   IF        %subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):1:3)
058100030822     C                             = 'VLB'
058200030822     C                   EVAL      PiStr=%trim(%subst(
058300030822     C                             %subst(vlrppt:posDaDft+1:
058400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058500030822     C                   EXSR      CHKNUM
058600030822     C                   IF        PiNum=*on
058700030822     C                   Z-ADD     PiVal         VABVLB
058800030822     C                   ENDIF
058900030822     C                   ENDIF
059000030822     C* QFT
059100030822     C                   IF        %subst(
059200030822     C                             %subst(vlrppt:posDaDft+1:
059300030822     C                             posADft-posDaDft-1):1:3)
059400030822     C                             = 'QFT'
059500030822     C                   EVAL      PiStr=%trim(%subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059800030822     C                   EXSR      CHKNUM
059900030822     C                   IF        PiNum=*on
060000030822     C                   Z-ADD     PiVal         VABQFT
060100030822     C                   ENDIF
060200030822     C                   ENDIF
060300030822     C* CBO
060400030822     C                   IF        %subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):1:3)
060700030822     C                             = 'CBO'
060800030822     C                   EVAL      VABCBO=%trim(%subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061100030822     C                   ENDIF
061200030822     C* TSP
061300030822     C                   IF        %subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):1:3)
061600030822     C                             = 'TSP'
061700030822     C                   EVAL      VABTSP=%trim(%subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062000030822     C                   ENDIF
062100030822     C* VAS
062200030822     C                   IF        %subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):1:3)
062500030822     C                             = 'VAS'
062600030822     C                   EVAL      VABVAS=%trim(%subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062900030822     C                   ENDIF
063000030822     C* VCA
063100030822     C                   IF        %subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):1:3)
063400030822     C                             = 'VCA'
063500030822     C                   EVAL      VABVCA=%trim(%subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800030822     C                   ENDIF
063900030822     C* GCA
064000030822     C                   IF        %subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):1:3)
064300030822     C                             = 'GCA'
064400030822     C                   EVAL      VABGCA=%trim(%subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700030822     C                   ENDIF
064800030822     C* CTM
064900030822     C                   IF        %subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):1:3)
065200030822     C                             = 'CTM'
065300030822     C                   EVAL      VABCTM=%trim(%subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065600030822     C                   ENDIF
065700030822     C* FFD
065800030822     C                   IF        %subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):1:3)
066100030822     C                             = 'FFD'
066200030822     C                   EVAL      VABFFD=%trim(%subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066500030822     C                   ENDIF
066600030822     C* VAD
066700030822     C                   IF        %subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):1:3)
067000030822     C                             = 'VAD'
067100030822     C                   EVAL      VABVAD=%trim(%subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400030822     C                   ENDIF
067500030822     C* GMA
067600030822     C                   IF        %subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):1:3)
067900030822     C                             = 'GMA'
068000030822     C                   EVAL      VABGMA=%trim(%subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068300030822     C                   ENDIF
068400030822     C* GGA
068500030822     C                   IF        %subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):1:3)
068800030822     C                             = 'GGA'
068900030822     C                   EVAL      VABGGA=%trim(%subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069200030822     C                   ENDIF
069300030822     C* GVA
069400030822     C                   IF        %subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):1:3)
069700030822     C                             = 'GVA'
069800030822     C                   EVAL      VABGVA=%trim(%subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070100030822     C                   ENDIF
070200030822     C* TC1
070300030822     C                   IF        %subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):1:3)
070600030822     C                             = 'TC1'
070700030822     C                   EVAL      VABTC1=%trim(%subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071000030822     C                   ENDIF
071100030822     C* TC2
071200030822     C                   IF        %subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):1:3)
071500030822     C                             = 'TC2'
071600030822     C                   EVAL      VABTC2=%trim(%subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071900030822     C                   ENDIF
072000030822     C* ...
072100030822     C                   ENDIF
072200030822     C                   ENDDO
072300020204     C*
072400010330     C                   ENDSR
072500010607     C*----------------------------------------------------*
072600021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
072700010607     C*----------------------------------------------------*
072800010607     C     IMPVAB        BEGSR
072900010607     C*
073000010607     C                   SETOFF                                       3132
073100030515     C*
073200010607     C                   MOVEL     datcor        VABAAS
073300040119     C                   MOVEL     datcor        VATAAS
073400010607     C                   MOVE      datcor        VABMGS
073500010607     C*
073600030715     C**********
073700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
073800030715     C**********
073900030715     C                   IF        wGiro = *zeros
074000040706     C*
074100040706     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
074200040706     C                   EVAL      depvinDTA = vinDTA
074300040113     C*
074400040113     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
074500040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
074600040113     C                                      %trim(intcol(3))
074700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
074800031201     C     minu:maiu     XLATE     vindta        vindta
074900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
075000030715     C                   DOW       posDa <= %len(%trim(vindta))
075100030715     C*
075200030715     C* Gestisco il 1° campo
075300030715     C                   IF        i = 1
075400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
075500030715     C                   EVAL      posDa = 1
075600030715     C* Eseguo lo scan x trovare la fine del primo campo
075700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
075800030715     C* A questo punto "estraggo" il campo corrente
075900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
076000030715     C                                                    (posA-posDa))
076100030715     C* X i campi successivi al 1°
076200030715     C                   ELSE
076300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
076400030715     C                   EVAL      posDa = posA + 1
076500030715     C* Eseguo lo scan x trovare la fine del campo corrente
076600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
076700030715     C* Gestisco l'ultimo campo
076800030715     C                   IF        posA = *zeros
076900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
077000030715     C* A questo punto "estraggo" il campo corrente
077100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
077200030715     C                                                    (posA-posDa))
077300030715     C                   LEAVE
077400030715     C                   ELSE
077500030715     C* A questo punto "estraggo" il campo corrente
077600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
077700030715     C                                                    (posA-posDa))
077800030715     C                   ENDIF
077900030715     C                   ENDIF
078000030715     C* Incremento il contatore d campo
078100030715     C                   EVAL      i = i +1
078200030715     C                   ENDDO
078300030715     C*
078400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
078500030715     C* delimitazione testo
078600030715     C                   EVAL      i = 1
078700030715     C                   DOW       i < %elem(SkSplitFLD)
078800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
078900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
079000030715     C                   EVAL      i = i + 1
079100030715     C                   ENDDO
079200030715     C                   EVAL      wGiro = 1
079300040706     C* Ripristino la 1° riga originale
079400040706     C                   EVAL      vinDTA = depvinDTA
079500030715     C                   ELSE
079600030715     C                   EVAL      wGiro = 2
079700031204     C**********
079800031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
079900031204     C* d separatore campo
080000031204     C**********
080100031204     C                   EVAL      vindta = %trim(vindta)
080200031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
080300031204     C                   EVAL      vindta = %subst(vindta:2)
080400031204     C                   ENDDO
080500031204     C*
080600031204     C                   Z-ADD     *zeros        lunghInput        4 0
080700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
080800031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
080900031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
081000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
081100031204     C                   ENDDO
081200030715     C**********
081300030715     C* Effettuo lo split del campo dati d input
081400030715     C**********
081500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081600030715     C                   DOW       posDa <= %len(%trim(vindta))
081700030715     C*
081800030715     C* Gestisco il 1° campo
081900030715     C                   IF        i = 1
082000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082100030715     C                   EVAL      posDa = 1
082200030715     C* Eseguo lo scan x trovare la fine del primo campo
082300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082400030715     C* A questo punto "estraggo" il campo corrente
082500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
082600030715     C                                                    (posA-posDa))
082700030715     C* X i campi successivi al 1°
082800030715     C                   ELSE
082900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083000030715     C                   EVAL      posDa = posA + 1
083100031204     C* Verifico che nn vi sia il campo nn valorizzato
083200031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
083300031204     C* Se campo nn valorizzato skippo al prossimo
083400031204     C                   EVAL      SkSplitCSV(i) = *blanks
083500031204     C                   EVAL      posA  = posA + 1
083600031204     C                   ELSE
083700030715     C* Eseguo lo scan x trovare la fine del campo corrente
083800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083900030715     C* Gestisco l'ultimo campo
084000030715     C                   IF        posA = *zeros
084100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
084200030715     C* A questo punto "estraggo" il campo corrente
084300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
084400030715     C                                                    (posA-posDa))
084500030715     C                   LEAVE
084600030715     C                   ELSE
084700030715     C* A questo punto "estraggo" il campo corrente
084800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
084900030715     C                                                    (posA-posDa))
085000030715     C                   ENDIF
085100031204     C                   ENDIF
085200030715     C                   ENDIF
085300030715     C* Incremento il contatore d campo
085400030715     C                   EVAL      i = i +1
085500030715     C                   ENDDO
085600030715     C*
085700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085800030715     C* delimitazione testo
085900030715     C                   EVAL      i = 1
086000030715     C                   DOW       i < %elem(SkSplitCSV)
086100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
086200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
086300030715     C                   EVAL      i = i + 1
086400030715     C                   ENDDO
086500030715     C*
086600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
086700030715     C                   EVAL      i = 1
086800030715     C                   DOW       i < %elem(SkSplitFLD)
086900030715     C*
087000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
087100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
087200030715     C                             SkSplitCSV(i) <> *blanks
087300030721     C*** ATB
087400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
087500030721     C                   EVAL      VABATB = SkSplitCSV(i)
087600030721     C                   ENDIF
087700030715     C*** RSD
087800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
087900060628     C                   EVAL      VABRSD = SkSplitCSV(i)
088000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
088100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
088200030715     C* ==
088300030715     C                   ENDIF
088400030721     C*** RD2
088500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
088600030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
088700030721     C                   ENDIF
088800030715     C*** IND
088900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
089000030715     C                   EVAL      VABIND = SkSplitCSV(i)
089100030715     C                   ENDIF
089200030715     C*** LOD
089300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
089400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
089500030715     C                   ENDIF
089600030715     C*** PRD
089700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
089800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
089900030715     C                   ENDIF
090000030721     C*** NZD
090100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
090200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
090300030721     C                   ENDIF
090400030715     C*** RMA
090500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
090600030715     C                   EVAL      VABRMA = SkSplitCSV(i)
090700030715     C                   ENDIF
090800030715     C*** NOT
090900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
091000030715     C                   EVAL      VABNOT = SkSplitCSV(i)
091100030715     C                   ENDIF
091200030715     C*** NT2
091300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
091400030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
091500030715     C                   ENDIF
091600030715     C*** VCA
091700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
091800030715     C                   EVAL      VABVCA = SkSplitCSV(i)
091900030715     C                   ENDIF
092000030715     C*** VAS
092100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
092200030715     C                   EVAL      VABVAS = SkSplitCSV(i)
092300030715     C                   ENDIF
092400030715     C*** FFD
092500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
092600030715     C                   EVAL      VABFFD = SkSplitCSV(i)
092700030715     C                   ENDIF
092800030721     C*** GC1
092900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
093000030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
093100030721     C                   ENDIF
093200030721     C*** GC2
093300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
093400030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
093500030721     C                   ENDIF
093600030721     C*** TSP
093700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
093800030721     C                   EVAL      VABTSP = SkSplitCSV(i)
093900030721     C                   ENDIF
094000030721     C*** NAS
094100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
094200030721     C                   EVAL      VABNAS = SkSplitCSV(i)
094300030721     C                   ENDIF
094400030721     C*** TIC
094500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
094600030721     C                   EVAL      VABTIC = SkSplitCSV(i)
094700030721     C                   ENDIF
094800030721     C*** GCA
094900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
095000030721     C                   EVAL      VABGCA = SkSplitCSV(i)
095100030721     C                   ENDIF
095200030721     C*** XCO
095300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
095400030721     C                   EVAL      VABXCO = SkSplitCSV(i)
095500030721     C                   ENDIF
095600030721     C*** CTM
095700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
095800030721     C                   EVAL      VABCTM = SkSplitCSV(i)
095900030721     C                   ENDIF
096000030721     C*** TCR
096100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
096200030721     C                   EVAL      VABTCR = SkSplitCSV(i)
096300030721     C                   ENDIF
096400030721     C*** CTS
096500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
096600030721     C                   EVAL      VABCTS = SkSplitCSV(i)
096700030721     C                   ENDIF
096800030721     C*** FTM
096900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
097000030721     C                   EVAL      VABFTM = SkSplitCSV(i)
097100030721     C                   ENDIF
097200030721     C*** VAD
097300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
097400030721     C                   EVAL      VABVAD = SkSplitCSV(i)
097500030721     C                   ENDIF
097600030721     C*** GMA
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
097800030721     C                   EVAL      VABGMA = SkSplitCSV(i)
097900030721     C                   ENDIF
098000030721     C*** GGA
098100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
098200030721     C                   EVAL      VABGGA = SkSplitCSV(i)
098300030721     C                   ENDIF
098400030721     C*** GVA
098500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
098600030721     C                   EVAL      VABGVA = SkSplitCSV(i)
098700030721     C                   ENDIF
098800030721     C*** TC1
098900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
099000030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
099100030721     C                   ENDIF
099200030721     C*** TC2
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
099400030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
099500030721     C                   ENDIF
099600030721     C*** SCL
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
099800030721     C                   EVAL      VABSCL = SkSplitCSV(i)
099900030721     C                   ENDIF
100000030721     C*** RMO
100100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
100200030721     C                   EVAL      VABRMO = SkSplitCSV(i)
100300030721     C                   ENDIF
100400030721     C*** NMO
100500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
100600030721     C                   EVAL      VABNMO = SkSplitCSV(i)
100700030721     C                   ENDIF
100800040119     C*** CBO
100900040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
101000040706     C                   EVAL      VABCBO = SkSplitCSV(i)
101100040119     C                   ENDIF
101200030721     C*
101300040706     C* Reperisco quindi i campi numerici...
101400040706     C*** NSP
101500040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
101600040706     C                   EVAL      PiStr=SkSplitCSV(i)
101700040706     C                   EXSR      CHKNUM
101800040706     C                   IF        PiInt=*on
101900040706     C                   Z-ADD     PiVal         VABNSP
102000040706     C                   ELSE
102100040706     C                   SETON                                        31
102200040706     C                   EVAL      VABNSP = *zeros
102300040706     C                   EVAL      vinmsg = %trimr(vinmsg)
102400040706     C                             + ' ' + 'VABNSP'
102500040706     C                   ENDIF
102600040706     C                   ENDIF
102700060628     C*** RMN
102800040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
102900040706     C                   EVAL      PiStr=SkSplitCSV(i)
103000060628     C                   EVAL      PiStr=%subst(PiStr:1:2)+
103100060628     C                                   %subst(PiStr:4:2)+
103200060628     C                                   %subst(PiStr:7:4)
103300040706     C                   EXSR      CHKNUM
103400040706     C                   IF        PiInt=*on
103500040706     C                   Z-ADD     PiVal         VABRMN
103600040706     C                   ELSE
103700040706     C                   SETON                                        32
103800040706     C                   EVAL      VABRMN = 1
103900040706     C                   EVAL      vinmsg = %trimr(vinmsg)
104000040706     C                             + ' ' + 'VABRMN'
104100040706     C                   ENDIF
104200040706     C                   ENDIF
104300030721     C*** CAD
104400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
104500030721     C                   EVAL      PiStr=SkSplitCSV(i)
104600030721     C                   EXSR      CHKNUM
104700030721     C                   IF        PiInt=*on
104800030721     C                   Z-ADD     PiVal         Num5_0
104900030721     C                   MOVEL(p)  Num5_0        VABCAD
105000030721     C                   ELSE
105100030721     C                   SETON                                        32
105200030721     C                   EVAL      VABCAD = *zeros
105300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105400030721     C                             + ' ' + 'VABCAD'
105500030721     C                   ENDIF
105600030721     C                   ENDIF
105700030721     C*** NCL
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
105900030721     C                   EVAL      PiStr=SkSplitCSV(i)
106000030721     C                   EXSR      CHKNUM
106100030721     C                   IF        PiInt=*on
106200030721     C                   Z-ADD     PiVal         VABNCL
106300030721     C                   ELSE
106400030721     C                   SETON                                        32
106500030721     C                   EVAL      VABNCL = *zeros
106600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106700040706     C                             + ' ' + 'VABNCL'
106800030721     C                   ENDIF
106900030721     C                   ENDIF
107000030721     C*** PKB
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
107200060628     C                   EVAL      PiStr=SkSplitCSV(i)
107300060628     C     'g':' '       XLATE     PiStr         PiStr
107400060628     C     'G':' '       XLATE     PiStr         PiStr
107500030721     C                   EXSR      CHKNUM
107600030721     C                   IF        PiNum=*on
107700060628     C                   EVAL(H)   VABPKB = PiVal / 1000                        * da grammi a Kg
107800030721     C                   ELSE
107900030721     C                   SETON                                        32
108000030721     C                   EVAL      VABPKB = *zeros
108100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108200030721     C                             + ' ' + 'VABPKB'
108300030721     C                   ENDIF
108400030721     C                   ENDIF
108500030721     C*** CAS
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
108700030721     C                   EVAL      PiStr=SkSplitCSV(i)
108800030721     C                   EXSR      CHKNUM
108900030721     C                   IF        PiNum=*on
109000030721     C                   Z-ADD     PiVal         VABCAS
109100030721     C                   ELSE
109200030721     C                   SETON                                        32
109300030721     C                   EVAL      VABCAS = *zeros
109400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109500030721     C                             + ' ' + 'VABCAS'
109600030721     C                   ENDIF
109700030721     C                   ENDIF
109800030721     C*** CCM
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
110000030721     C                   EVAL      PiStr=SkSplitCSV(i)
110100030721     C                   EXSR      CHKNUM
110200030721     C                   IF        PiInt=*on      AND
110300030721     C                             PiVal<=9999999 AND
110400030721     C                             PiVal>*zeros
110500030721     C                   Z-ADD     PiVal         VABCCM
110600040119     C                   Z-ADD     PiVal         VATCCM
110700030721     C                   ELSE
110800030721     C                   SETON                                        32
110900030721     C                   EVAL      VABCCM = *zeros
111000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111100040119     C                             + ' ' + 'VABCCM VATCCM'
111200030721     C                   ENDIF
111300030721     C                   ENDIF
111400030721     C*** LNP
111500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
111600030721     C                   EVAL      PiStr=SkSplitCSV(i)
111700030721     C                   EXSR      CHKNUM
111800030721     C                   IF        PiInt=*on    AND
111900030721     C                             PiVal<=999   AND
112000030721     C                             PiVal>*zeros
112100030721     C                   Z-ADD     PiVal         VABLNP
112200040119     C                   Z-ADD     PiVal         VATLNP
112300030721     C                   ELSE
112400030721     C                   SETON                                        32
112500030721     C                   EVAL      VABLNP = *zeros
112600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112700040119     C                             + ' ' + 'VABLNP VATLNP'
112800030721     C                   ENDIF
112900030721     C                   ENDIF
113000030721     C*** NRS
113100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
113200030721     C                   EVAL      PiStr=SkSplitCSV(i)
113300030721     C                   EXSR      CHKNUM
113400030721     C                   IF        PiInt=*on AND
113500030721     C                             PiVal<=99
113600030721     C                   Z-ADD     PiVal         VABNRS
113700030721     C                   ELSE
113800030721     C                   SETON                                        32
113900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114000030721     C                             + ' ' + 'VABNRS'
114100030721     C                   ENDIF
114200030721     C                   ENDIF
114300030721     C*** LNA
114400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
114500030721     C                   EVAL      PiStr=SkSplitCSV(i)
114600030721     C                   EXSR      CHKNUM
114700030721     C                   IF        PiInt=*on    AND
114800030721     C                             PiVal<=999   AND
114900030721     C                             PiVal>*zeros
115000030721     C                   Z-ADD     PiVal         VABLNA
115100030721     C                   ELSE
115200030721     C                   SETON                                        32
115300030721     C                   EVAL      VABLNA = *zeros
115400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115500030721     C                             + ' ' + 'VABLNA'
115600030721     C                   ENDIF
115700030721     C                   ENDIF
115800030721     C*** CTR
115900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
116000030721     C                   EVAL      PiStr=SkSplitCSV(i)
116100030721     C                   EXSR      CHKNUM
116200030721     C                   IF        PiInt=*on  AND
116300030721     C                             PiVal<=999
116400030721     C                   Z-ADD     PiVal         VABCTR
116500030721     C                   ELSE
116600030721     C                   SETON                                        32
116700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116800030721     C                             + ' ' + 'VABCTR'
116900030721     C                   ENDIF
117000030721     C                   ENDIF
117100030721     C*** IAS
117200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
117300030721     C                   EVAL      PiStr=SkSplitCSV(i)
117400030721     C                   EXSR      CHKNUM
117500030721     C                   IF        PiNum=*on
117600030721     C                   Z-ADD     PiVal         VABIAS
117700030721     C                   ELSE
117800030721     C                   SETON                                        32
117900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118000030721     C                             + ' ' + 'VABIAS'
118100030721     C                   ENDIF
118200030721     C                   ENDIF
118300030721     C*** VLB
118400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
118500030721     C                   EVAL      PiStr=SkSplitCSV(i)
118600030721     C                   EXSR      CHKNUM
118700030721     C                   IF        PiNum=*on
118800030721     C                   Z-ADD     PiVal         VABVLB
118900030721     C                   ELSE
119000030721     C                   SETON                                        32
119100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119200030721     C                             + ' ' + 'VABVLB'
119300030721     C                   ENDIF
119400030721     C                   ENDIF
119500030721     C*** QFT
119600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
119700030721     C                   EVAL      PiStr=SkSplitCSV(i)
119800030721     C                   EXSR      CHKNUM
119900030721     C                   IF        PiNum=*on
120000030721     C                   Z-ADD     PiVal         VABQFT
120100030721     C                   ELSE
120200030721     C                   SETON                                        32
120300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120400030721     C                             + ' ' + 'VABQFT'
120500030721     C                   ENDIF
120600030721     C                   ENDIF
120700030721     C*** NCD
120800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
120900030721     C                   EVAL      PiStr=SkSplitCSV(i)
121000030721     C                   EXSR      CHKNUM
121100030721     C                   IF        PiInt=*on      AND
121200030721     C                             PiVal<=9999999
121300030721     C                   Z-ADD     PiVal         VABNCD
121400030721     C                   ELSE
121500030721     C                   SETON                                        32
121600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121700030721     C                             + ' ' + 'VABNCD'
121800030721     C                   ENDIF
121900030721     C                   ENDIF
122000030721     C*** NCA
122100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
122200030721     C                   EVAL      PiStr=SkSplitCSV(i)
122300030721     C                   EXSR      CHKNUM
122400030721     C                   IF        PiInt=*on      AND
122500030721     C                             PiVal<=9999999
122600030721     C                   Z-ADD     PiVal         VABNCA
122700030721     C                   ELSE
122800030721     C                   SETON                                        32
122900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123000030721     C                             + ' ' + 'VABNCA'
123100030721     C                   ENDIF
123200030721     C                   ENDIF
123300030721     C*** ZNC
123400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
123500030721     C                   EVAL      PiStr=SkSplitCSV(i)
123600030721     C                   EXSR      CHKNUM
123700030721     C                   IF        PiInt=*on AND
123800030721     C                             PiVal<=99
123900030721     C                   Z-ADD     PiVal         VABZNC
124000030721     C                   ELSE
124100030721     C                   SETON                                        32
124200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124300030721     C                             + ' ' + 'VABZNC'
124400030721     C                   ENDIF
124500030721     C                   ENDIF
124600030721     C*** DCR
124700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
124800030721     C                   EVAL      PiStr=SkSplitCSV(i)
124900030721     C                   EXSR      CHKNUM
125000030721     C                   IF        PiInt=*on       AND
125100030721     C                             PiVal<=99999999
125200030721     C                   Z-ADD     PiVal         VABDCR
125300030721     C                   ELSE
125400030721     C                   SETON                                        32
125500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125600030721     C                             + ' ' + 'VABDCR'
125700030721     C                   ENDIF
125800030721     C                   ENDIF
125900030721     C*** HCR
126000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
126100030721     C                   EVAL      PiStr=SkSplitCSV(i)
126200030721     C                   EXSR      CHKNUM
126300030721     C                   IF        PiInt=*on   AND
126400030721     C                             PiVal<=9999
126500030721     C                   Z-ADD     PiVal         VABHCR
126600030721     C                   ELSE
126700030721     C                   SETON                                        32
126800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126900030721     C                             + ' ' + 'VABHCR'
127000030721     C                   ENDIF
127100030721     C                   ENDIF
127200030721     C*** VMD
127300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
127400030721     C                   EVAL      PiStr=SkSplitCSV(i)
127500030721     C                   EXSR      CHKNUM
127600030721     C                   IF        PiNum=*on
127700030721     C                   Z-ADD     PiVal         VABVMD
127800030721     C                   ELSE
127900030721     C                   SETON                                        32
128000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128100030721     C                             + ' ' + 'VABVMD'
128200030721     C                   ENDIF
128300030721     C                   ENDIF
128400030721     C*** ANT
128500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
128600030721     C                   EVAL      PiStr=SkSplitCSV(i)
128700030721     C                   EXSR      CHKNUM
128800030721     C                   IF        PiInt=*on        AND
128900030721     C                             PiVal<=999999999
129000030721     C                   Z-ADD     PiVal         VABANT
129100030721     C                   ELSE
129200030721     C                   SETON                                        32
129300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129400030721     C                             + ' ' + 'VABANT'
129500030721     C                   ENDIF
129600030721     C                   ENDIF
129700030721     C*** CMO
129800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
129900030721     C                   EVAL      PiStr=SkSplitCSV(i)
130000030721     C                   EXSR      CHKNUM
130100030721     C                   IF        PiInt=*on
130200030721     C                   Z-ADD     PiVal         Num5_0
130300030721     C                   MOVEL(p)  Num5_0        VABCMO
130400030721     C                   ELSE
130500030721     C                   SETON                                        32
130600030721     C                   EVAL      VABCMO = *zeros
130700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130800030721     C                             + ' ' + 'VABCMO'
130900030721     C                   ENDIF
131000030721     C                   ENDIF
131100040706     C*
131200040706     C* Gestisco i campi "personalizzati"
131300060628     C*** TELEFONO
131400060628     C                   IF        %trim(SkSplitFLD(i)) = 'TELEFONO'
131500060628     C                   EVAL      VATNOT = SkSplitCSV(i)
131600060704     C                   EVAL      VABNOT = SkSplitCSV(i)
131700030715     C                   ENDIF
131800060628     C*
131900060628     C                   ENDIF
132000030715     C                   EVAL      i = i + 1
132100030715     C                   ENDDO
132200040706     C*
132300040706     C* Se nn valorizzata la provincia destinatario la calcolo tramite TISI95
132400040706     C* Reperisco la provincia dal CAP e dalla località
132500040706     C                   IF        VABCAD <> *blanks
132600040706     C                   CLEAR                   TISI95DS
132700040706     C                   EVAL      I95TCN = '3'
132800040706     C                   Z-ADD     datcor        I95DAT
132900040706     C                   EVAL      I95CAP = VABCAD
133000040706     C                   EVAL      I95LOC = VABLOD
133100040706     C                   CALL      'TISI95R'
133200040706     C                   PARM                    TISI95DS
133300040706     C                   EVAL      VABPRD = O95PRV
133400040706     C                   ENDIF
133500060628     C*
133600060628     C* NSP => Stacco un numeratore da AZNUM (anche se Disk A mi serve x correlare i record
133700060628     C*        del file di estensione FIVAT)
133800060628     C                   clear                   TRUL33DS
133900060628     C                   eval      I33OPE = *zeros
134000060628     C                   eval      I33CNU = 302
134100060628     C                   eval      I33NUM = 1
134200060628     C                   movel     TRUL33DS      KPJBU
134300060628     C                   call      'TRUL33R'
134400060628     C                   parm                    KPJBA
134500060628     C                   movel     KPJBU         TRUL33DS
134600060628     C                   if        O33ERR = *zeros
134700060628     C                   z-add     O33NRF        VABNSP
134800060628     C                   z-add     O33NRF        VATNSP
134900060628     C                   else
135000060628     C                   seton                                        31
135100060628     C                   eval      vinmsg = %trimr(vinmsg)
135200060628     C                             + ' ' + 'VABNSP VATNSP'
135300060628     C                   endif
135400040706     C*
135500040706     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
135600040706     C* d valorizzarli sempre entrambi
135700040706     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
135800040706     C                   Z-ADD     VABNSP        VABRMN
135900040706     C                   ENDIF
136000040706     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
136100040706     C                   MOVEL     VABRMN        VABRMA
136200040706     C                   ENDIF
136300040113     C*
136400040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
136500040113     C                   IF        VABCAS > *zeros
136600040119     C                   IF        VABCBO = '1'
136700040113     C                   EVAL      VABCBO = '4'
136800040119     C                   ENDIF
136900040119     C                   IF        VABCBO = '2'
137000040119     C                   EVAL      VABCBO = '6'
137100040119     C                   ENDIF
137200040113     C                   ENDIF
137300020204     C*
137400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
137500020204     C                   EXSR      CHKIMPDIV
137600030715     C                   ENDIF
137700020204     C*
137800010607     C                   ENDSR
137900010607     C*----------------------------------------------------*
138000010601
138100020204
138200020204     C*----------------------------------------------------*
138300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
138400020204     C*----------------------------------------------------*
138500020204     C     CHKIMPDIV     BEGSR
138600020204     C*
138700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
138800020204     C                   Z-ADD     *zeros        wrkDec            9 9
138900020204     C*
139000020204     C* Come prima cosa effettuo considerazioni sulla divisa
139100020204     C                   IF        vabIAS > *zeros
139200020204     C                   IF        vabVAS <> 'EUR'
139300020204     C                   EVAL      vabVAS =  'ITL'
139400020204     C                   ENDIF
139500020204     C                   ENDIF
139600020204     C*
139700020204     C                   IF        vabCAS > *zeros
139800020204     C                   IF        vabVCA <> 'EUR'
139900020204     C                   EVAL      vabVCA =  'ITL'
140000020204     C                   ENDIF
140100020204     C                   ENDIF
140200020204     C*
140300020204     C                   IF        vabVMD > *zeros
140400020204     C                   IF        vabVAD <> 'EUR'
140500020204     C                   EVAL      vabVAD =  'ITL'
140600020204     C                   ENDIF
140700020204     C                   ENDIF
140800020204     C*
140900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
141000020204     C                   Z-ADD     vabIAS        wrkDec
141100020204     C                   IF        wrkDec > *zeros
141200020204     C                   IF        vabVAS = 'ITL'
141300020204     C                   EVAL      vabIAS = *zeros
141400020204     C                   ENDIF
141500020204     C                   ENDIF
141600020204     C*
141700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
141800020204     C                   Z-ADD     vabCAS        wrkDec
141900020204     C                   IF        wrkDec > *zeros
142000020204     C                   IF        vabVCA = 'ITL'
142100020204     C                   EVAL      vabCAS = *zeros
142200020204     C                   ENDIF
142300020204     C                   ENDIF
142400020204     C*
142500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
142600020204     C                   Z-ADD     vabVMD        wrkDec
142700020204     C                   IF        wrkDec > *zeros
142800020204     C                   IF        vabVAD = 'ITL'
142900020204     C                   EVAL      vabVMD = *zeros
143000020204     C                   ENDIF
143100020204     C                   ENDIF
143200020204     C*
143300020204     C                   ENDSR
143400020204     C***
143500020204
143600010330
143700010330
143800010330     C*----------------------------------------------------*
143900010330     C*  CONTROLLO NUMERICITA' CAMPI
144000010330     C*----------------------------------------------------*
144100010330     C     CHKNUM        BEGSR
144200010330     C*
144300010606     C                   IF        PiDecChr = *blanks
144400030715     C                   EVAL      PiDecChr = CharNUM
144500010606     C                   ENDIF
144600010606     C*
144700010606     C                   CALL(e)   'ISNUMERIC'
144800010330     C                   PARM                    PiStr            30
144900010606     C                   PARM                    PiDecChr          1
145000010330     C                   PARM      *ZEROS        PiVal            30 9
145100010330     C                   PARM      '0'           PiInt             1
145200010330     C                   PARM      '0'           PiNum             1
145300010330     C                   IF        %error
145400010606     C                   EVAL      PiNum=*off
145500010330     C                   ENDIF
145600010330     C*
145700010330     C                   ENDSR
145800010330     C***
145900010330
146000010601
146100010601
146200010601
146300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
146400010601     C     repfil        BEGSR
146500010601     C*
146600010601     C                   if        invfil = *zeros and
146700010601     C                             depfil > *zeros and
146800010629     C                             (vinflg = *blanks or
146900010629     C                              vinflg = *zeros)
147000010601     C
147100010601     C                   eval      invfil = depfil
147200010601     C                   endif
147300010601     C*
147400010601     C                   if        depfil <> invfil and
147500010601     C                             invfil > *zeros
147600010601     C                   eval      flgMulti = '1'
147700010601     C                   if        vinflg = *blanks
147800010601     C                   add       1             cntNonEl
147900010601     C                   endif
148000010601     C                   endif
148100010601     C*
148200010601     C                   if        vinflg = '2'
148300010601     C                   eval      flgStato = '2'
148400010601     C                   endif
148500010601     C*
148600010601     C                   ENDSR
148700010601     C***
148800010601
148900010601
149000010601
149100010330
149200040119      /TITLE Invio dei dati al punto operativo.
149300040119     C     invio         BEGSR
149400040119     C*
149500040119     C* 1° invio FIVAT
149600040119     C                   reset                   dscmz
149700040119     C                   move      vlrpoi        cmzdst
149800040119     C                   eval      cmzfld = 'FIVATWWR'
149900040119     C                   eval      cmzmbd = vlrhdl
150000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
150100040119     C***                if        prmfir = *blanks
150200040119     C                   eval      cmzfla = 'FIVAT00F'
150300040119     C                   eval      cmzmba = 'FIVAT00F'
150400040119     C***                else
150500040119     C***                eval      cmzfla = prmfir
150600040119     C***                eval      cmzmba = prmfir
150700040119     C***                endif
150800040119     C                   eval      cmznrr = *zeros
150900040119     C                   move      §ctrokvt      cmznrr
151000040119     C                   eval      cmzlba = vlrfl1
151100040119     C                   call(e)   'TIS711C'
151200040119     C                   parm                    dscmz
151300040119     C                   parm      *blanks       esito
151400040119     C                   if        %error
151500040119     C                             or cmzerr = '1'
151600040119     C                             or esito  = '1'
151700040119     C                   eval      wrkesito = '3'
151800040119     C                   else
151900040119     C*
152000040119     C* 2° invio FIVAB
152100040119     C                   reset                   dscmz
152200040119     C                   move      vlrpoi        cmzdst
152300040119     C                   eval      cmzfld = vlrfou
152400040119     C                   eval      cmzmbd = vlrhdl
152500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
152600040119     C***                if        prmfir = *blanks
152700040119     C                   eval      cmzfla = 'FIVAB00F'
152800040119     C                   eval      cmzmba = 'FIVAB00F'
152900040119     C***                else
153000040119     C***                eval      cmzfla = prmfir
153100040119     C***                eval      cmzmba = prmfir
153200040119     C***                endif
153300040119     C                   eval      cmznrr = *zeros
153400040119     C                   move      §ctrokvb      cmznrr
153500040119     C                   eval      cmzlba = vlrfl1
153600040119     C                   call(e)   'TIS711C'
153700040119     C                   parm                    dscmz
153800040119     C                   parm      *blanks       esito
153900040119     C                   if        %error
154000040119     C                             or cmzerr = '1'
154100040119     C                             or esito  = '1'
154200040119     C                   eval      wrkesito = '3'
154300040119     C                   endif
154400040119     C                   endif
154500040119     C*
154600040119     C                   ENDSR
154700040119     C***
154800010601
154900010601
155000010601
155100010601
155200010601      /TITLE Invio dei dati al punto operativo.
155300010601     C     opeini        BEGSR
155400010601     C*
155500010601     C* Inizializzo flag e contatori operativi
155600010601     C                   movel     '0'           flgGiro           1
155700010601     C                   movel     '0'           flgMulti          1
155800010601     C                   movel     '1'           flgStato          1
155900010615     C                   movel     '0'           flgOk             1
156000010601     C                   z-add     *zeros        cntNonEl         10 0
156100010601     C                   z-add     *zeros        depfil            3 0
156200010601     C                   z-add     *zeros        invfil            3 0
156300010601     C*
156400010601     C                   ENDSR
156500010601     C***
156600010601
156700010601
156800050421
156900050421
157000070411
157100070411     C     *pssr         BEGSR
157200070411     C*
157300070411     C                   if        %open(tivin00r)
157400070411     C                   close     tivin00r
157500070411     C                   endif
157600070411     C                   if        %open(fivabwwr)
157700070411     C                   close     fivabwwr
157800070411     C                   endif
157900070411     C                   if        %open(fivatwwr)
158000070411     C                   close     fivatwwr
158100070411     C                   endif
158200070411     C*
158300070411     C* Effettuo la chiamata al CLLE preposto
158400070411     C                   call(e)   'TITVVTC'
158500070411     C                   parm                    parccm
158600070411     C                   parm                    parmbr
158700070411     C                   parm      '2'           paropz
158800070411     C*
158900070411     C                   eval      wrkesito = '2'
159000070411     C*
159100070411     C                   seton                                        LR
159200070411     C*
159300070411     C                   ENDSR     '*CANCL'
159400070411     C***
159500070411
159600050421
159700010601
159800010330
159900010330
160000000613     C     *inzsr        BEGSR
160100990910     C*
160200990910     C     *entry        plist
160300990920     C                   parm                    tivlrds
160400990921     C                   parm      wrkesito      esito
160500000724     C                   parm                    prmlit
160600000710     C                   parm                    prmfir
160700010330     C*
160800010330     C* CALCOLA LA DATA CORRENTE
160900010330     C                   time                    wn14             14 0
161000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
161100010330     C                   z-add     wn8           g08dat
161200010330     C                   z-add     *zeros        g08inv
161300010330     C                   movel     '0'           g08err
161400010330     C                   call      'XSRDA8'
161500010330     C                   parm                    wlbda8
161600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
161700000613     C*
161800000613     C                   ENDSR
161900000613     C***
162000990908
162100060628     Otitvt5p   E            riepilogo         2
162200990915     O                                              'Upload via Internet'
162300990915     O                                           +1 'Traduzione TIVIN00R -'
162400021025     O                                           +1 'FIVABWWR'
162500990915     O                                           +1 'Testate bolle'
162600990915     O          E            riepilogo   2
162700990915     O                       wrkdata
162800990915     O                       wrkora              +1
162900990915     O                       procname            +1
163000990915     O          E            riepilogo   2
163100990915     O                                              'Cliente..................:'
163200010605     O                       VABCCM        z     +1
163300990915     O          E            riepilogo   2
163400990920     O                                              'Riferimento Strategi.....:'
163500990920     O                       vlrhdl              +1
163600990915     O          E            riepilogo   2
163700990915     O                                              'Giusti...................:'
163800040119     O                       §CTROKVB      2   +  1
163900990915     O          E            riepilogo   2
164000990915     O                                              'Sbagliati e corretti.....:'
164100971022     O                       §CTRMO        2   +  1
164200990915     O          E            riepilogo   2
164300990915     O                                              'Sbagliati e scartati.....:'
164400971022     O                       §CTRNO        2   +  1
164500000613
164600060628     Otitvt5ps  E            testdett          2
164700000613     O                                              'Upload via Internet'
164800000613     O                                           +1 'Traduzione TIVIN00R -'
164900021025     O                                           +1 'FIVABWWR'
165000000613     O                                           +1 'Testate bolle'
165100000616     O          E            testdett    3
165200000613     O                                           +2 'N° rec'
165300000613     O                                           +3 'Anteprima contenuto'
165400000616     O          E            rigadett    2
165500000613     O                       rrnum               +2
165600000621     O                       recko               +3
165700000616     O          E            findett     2
165800000613     O                       wrkdata
165900000613     O                       wrkora              +1
166000000613     O                       procname            +1
166100000616     O          E            findett     2
166200000613     O                                              'Cliente..................:'
166300010605     O                       VABCCM        z     +1
166400000616     O          E            findett     2
166500000613     O                                              'Riferimento Strategi.....:'
166600000613     O                       vlrhdl              +1
166700000616     O          E            findett     2
166800000613     O                                              'Giusti...................:'
166900040119     O                       §CTROKVB      2   +  1
167000000616     O          E            findett     2
167100000613     O                                              'Sbagliati e corretti.....:'
167200000613     O                       §CTRMO        2   +  1
167300000616     O          E            findett     2
167400000613     O                                              'Sbagliati e scartati.....:'
167500000613     O                       §CTRNO        2   +  1
167600000613** CMD - COMANDI CL
167700060628OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVT5P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
167800060628OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVT5PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
167900060628DLTOVR FILE(TITT56P TITVT5PS) LVL(*)
168000010606
168100000613
168200040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
168300060628FILLER1;VABRMN;VABRSD;VABIND;VABLOD;VABPRD;VABCAD;VABRD2;VABCAS;
168400060704TELEFONO;VABNCL;VABRMA;VABPKB;VABNT2
168500040113
