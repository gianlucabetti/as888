000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800040126     Ftitv93p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100040126     Ftitv93ps  O    f  198        PRINTER usropn
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
003000010330     D esito           s              1
003100010330     D prmlit          s             10
003200010330     D prmfir          s             10
003300010330     D wrkesito        s                   like(esito)
003400010330     D wrkdata         s               d
003500010330     D wrkora          s               t
003600010330     D rrnum           s              6  0 INZ(*zeros)
003700010330     D recko           s            150    INZ(*blanks)
003800011113     D depcmd          s            150    INZ(*blanks)
003900040119     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004000040119     D parccm          s              8    INZ(*blanks)
004100040119     D parmbr          s             10    INZ(*blanks)
004200040119     D paropz          s              1    INZ(*blanks)
004300040119     D chkcall         s              1    INZ(*blanks)
004400030715     D*------------------
004500030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004600030715     D*------------------
004700030715     D SkSplitFLD      S             10    DIM(100)
004800030715     D SkSplitCSV      S            256    DIM(100)
004900030715     D CharCSV         S              1
005000030715     D CharTXT         S              1
005100030715     D CharNUM         S              1
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
006400010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
006500010330     D*------------------
006600010330     D WLBDA8          DS                  INZ
006700010330     D  G08DAT                 1      8  0
006800010330     D  G08INV                 9     16  0
006900010330     D  G08ERR                17     17
007000010330     D  G08TGI                18     22  0
007100031201     D*------------------
007200031201     D* Costanti
007300031201     D*------------------
007400031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007500031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007600010330
007700010330
007800010330
007900990915     C                   time                    wrkdata
008000990915     C                   time                    wrkora
008100000913     C                   reset                   rrnum
008200990921     C                   reset                   esito
008300990921     C                   reset                   wrkesito
008400010601     C*
008500010601     C                   exsr      opeini
008600010605     C                   exsr      rwvab
008700010601     C*
008800010601     C                   seton                                        lr
008900010601
009000010601
009100010601
009200010601     C*--------------------------------------------------------
009300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009400010601     C*--------------------------------------------------------
009500010601     C     PREELA        BEGSR
009600010601     C*
009700010601     C* SE OCCORRE SPEDIRE IN FILIALE
009800010601     C                   if        invfil <> *zeros and
009900010601     C                             flgGiro = '0'
010000010601     C*
010100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010200010601     C                   eval      flgGiro = '1'
010300000724     C*
010400000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
010500000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010600010601     C     invfil        chain     azorg01l
010700000724     C                   if        %found
010800000724     C                   movel(p)  CMD(1)        depcmd
010900020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011000000724     C*
011100000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011200011113     C                   Z-ADD     150           LENGH            15 5
011300000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011400000724     C                   PARM                    depcmd
011500000724     C                   PARM                    LENGH
011600000724     C*
011700000724     C                   endif
011800000616     C*
011900000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012000000616     C                   movel(p)  CMD(2)        depcmd
012100000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012200000616     C*
012300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012400011113     C                   Z-ADD     150           LENGH            15 5
012500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012600000616     C                   PARM                    depcmd
012700000616     C                   PARM                    LENGH
012800000613     C*
012900040126     C                   if        not %open(titv93ps)
013000040126     C                   open      titv93ps
013100030715     C                   except    testdett
013200000613     C                   endif
013300010601     C*
013400010601     C                   endif
013500010601     C*
013600010601     C                   ENDSR
013700010601     C***
013800010601
013900010601
014000010601
014100010601
014200010601     C*--------------------------------------------------------
014300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014400010601     C*--------------------------------------------------------
014500010601     C     ENDELA        BEGSR
014600990915     C*
014700000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
014800040119     C*
014900040119     C* Effettuo la chiamata al CLLE preposto
015000040119     C                   call(e)   'TITVVTC'
015100040119     C                   parm                    parccm
015200040119     C                   parm                    parmbr
015300040119     C                   parm      '2'           paropz
015400000613     C*
015500040126     C                   if        %open(titv93ps)
015600000616     C                   except    findett
015700040126     C                   close     titv93ps
015800000613     C                   endif
015900000616     C*
016000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
016100011113     C                   Z-ADD     150           LENGH            15 5
016200010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
016300000616     C                   PARM                    CMD(3)
016400000616     C                   PARM                    LENGH
016500000616     C*
016600010601     C                   ENDSR
016700010601     C***
016800010601
016900010601
017000010601
017100000613
017200000613     C*--------------------------------------------------------
017300000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
017400000613     C*--------------------------------------------------------
017500000621     C     STPR          BEGSR
017600000613     C*
017700040126     C                   if        not %open(titv93p)
017800040126     C   50              open      titv93p
017900990915     C                   endif
018000990915     C*
018100030715     C   50              except    riepilogo
018200990915     C*
018300040126     C                   if        %open(titv93p)
018400040126     C   50              close     titv93p
018500990914     C                   endif
018600000613     C*
018700000613     C                   ENDSR
018800000613     C***
018900000613
019000010601
019100010601
019200010330     C*--------------------------------------------------------
019300021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
019400010330     C*--------------------------------------------------------
019500010605     C     RWVAB         BEGSR
019600010330     C*
019700010330     C                   if        not %open(tivin00r)
019800010330     C                   open      tivin00r
019900010330     C                   endif
020000021025     C                   if        not %open(fivabwwr)
020100021025     C                   open      fivabwwr
020200010330     C                   endif
020300040119     C*
020400040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
020500040119     C                   exsr      prevat
020600040119     C*
020700040119     C                   if        chkcall = '0'
020800040119     C*
020900040119     C                   if        not %open(fivatwwr)
021000040119     C                   open      fivatwwr
021100040119     C                   endif
021200010330     C*
021300040119     C                   clear                   §CTROKVB          7 0
021400040119     C                   clear                   §CTROKVT          7 0
021500040119     C                   clear                   §CTRMO            7 0
021600040119     C                   clear                   §CTRNO            7 0
021700010330     C*
021800030822     C                   do        *HIVAL
021900010330     C*
022000030822     C                   read      tivin00r                               70
022100010618     C*
022200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
022300010618     C                   if        vinflg = '1'
022400010618     C                   eval      flgOk = '1'
022500010618     C                   endif
022600010618     C*
022700010330     C                   if        vindta > *blanks
022800010330     C                   add       1             rrnum
022900010330     C*
023000010601     C                   if        *in70 = *off and
023100010330     C                             (vinflg = *blanks
023200010330     C                              or vinflg = '0'
023300010330     C                              or vinflg = '2')
023400010330     C*
023500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
023600010711     C                   if        vinflg = *blanks or vinflg = '0'
023700010711     C                   clear                   vinmsg
023800010711     C                   endif
023900010601     C*
024000010330     C                   exsr      inzvar
024100010605     C                   exsr      impvab
024200010601     C*
024300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
024400010605     C                   eval      depfil = VABLNP
024500010601     C                   exsr      repfil
024600010601     C                   if        depfil = invfil
024700021025     C                   if        vlrpoi = 999
024800030822     C                   move(P)   invfil        VABFGS
024900040119     C                   move(P)   invfil        VATFGS
025000021025     C                   else
025100030822     C                   move(P)   vlrpoi        VABFGS
025200040119     C                   move(P)   vlrpoi        VATFGS
025300021025     C                   endif
025400010601     C*
025500010601     C                   exsr      PREELA
025600010601     C*
025700010604     C* Ebbene...
025800010604     C*
025900040119     C  N31              add       1             §CTROKVB
026000040119     C   32              add       1             §CTRMO
026100040119     C   31              add       1             §CTRNO
026200030822     C                   if        wGiro = 2
026300040119     C                   if        depspe =  *zeros OR
026400040119     C                             depspe <> vabNSP
026500040119     C  N31              exsr      wrivab
026600040119     C  N31              eval      depspe = vabNSP
026700040119     C                   endif
026800040119     C                   endif
026900010604     C*
027000010604     C                   if        *in31 = *off and
027100010604     C                             *in32 = *off
027200010604     C                   eval      vinflg = '1'
027300010604     C                   else
027400010604     C                   eval      recko = vindta
027500010604     C                   except    rigadett
027600010604     C                   eval      vinflg = '2'
027700010604     C                   endif
027800010604     C                   endif
027900010601     C*
028000010604     C                   endif
028100010604     C*
028200010330     C                   else
028300010330     C                   eval      vinflg = '1'
028400010330     C                   endif
028500010601     C*
028600010601     C  N70              update    tivin000
028700010330     C*
028800030822     C  N70              enddo
028900040119     C*
029000040119     C                   endif                                                  (endif - chkcall)
029100010601     C*
029200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
029300010601     C                   if        cntNonEl = *zeros or
029400010601     C                             flgMulti = '0'
029500010330     C* Se non ci sono record con errori ...
029600010601     C                   if        §ctrno = 0 and
029700010604     C                             §ctrmo = 0 and
029800010601     C                             flgStato <> '2'
029900010330     C* ... restituisco esito OK.
030000010330     C                   eval      wrkesito = '0'
030100010330     C                   else
030200040119     C                   if        §ctrokvb > 0
030300010330     C                   eval      wrkesito = '1'
030400010330     C                   else
030500010615     C                   if        flgOk = '0'
030600010615     C                   eval      wrkesito = '2'
030700010615     C                   else
030800010615     C                   eval      wrkesito = '6'
030900010615     C                   endif
031000010330     C                   endif
031100010330     C                   endif
031200010601     C                   else
031300010601     C                   eval      wrkesito = '9'
031400010601     C                   endif
031500010330     C*
031600010330     C                   if        %open(tivin00r)
031700010330     C                   close     tivin00r
031800010330     C                   endif
031900021025     C                   if        %open(fivabwwr)
032000021025     C                   close     fivabwwr
032100010330     C                   endif
032200040512     C                   if        %open(fivatwwr)
032300040512     C                   close     fivatwwr
032400040512     C                   endif
032500010601     C*
032600010601     C                   if        vlrpoi <> 999
032700010601     C                   eval      invfil = vlrpoi
032800010601     C                   endif
032900010330     C*
033000040119     C                   if        §ctrokvb > 0
033100010601     C                             and invfil > *zeros
033200010330     C                   exsr      invio
033300010330     C                   endif
033400010601     C*
033500010618     C                   if        flgGiro = '1'
033600010601     C                   exsr      endela
033700010618     C                   endif
033800010330     C*
033900010330     C                   ENDSR
034000010330     C***
034100040119
034200040119
034300040119
034400040119     C*----------------------------------------------------*
034500040119     C*  SCARICAMENTO BUFFER RECORDS VAB
034600040119     C*----------------------------------------------------*
034700040119     C     WRIVAB        BEGSR
034800040119     C*
034900040119     C                   write     fivab000                                     => scarico il VAB
035000040119     C*
035100040126     C* Eseguo la scrittura del tipo record 'B' relativo al numero d telefono della spedizione
035200040119     C                   exsr      wrivat
035300040119     C*
035400040119     C                   ENDSR
035500040119
035600040119
035700040119
035800040119     C*----------------------------------------------------*
035900040119     C*  SCARICAMENTO BUFFER RECORDS VAT
036000040119     C*----------------------------------------------------*
036100040119     C     WRIVAT        BEGSR
036200040119     C*
036300040119     C                   write     FIVAT000
036400040119     C                   add       1             §CTROKVT
036500040119     C*
036600040119     C                   ENDSR
036700040119
036800040119
036900040119
037000040119     C*----------------------------------------------------*
037100040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
037200040119     C*----------------------------------------------------*
037300040119     C     PREVAT        BEGSR
037400040119     C*
037500040119     C* Compongo il nome del membro da dare al FIVATWWR
037600040119     C                   eval      parmbr = vlrhdl
037700040119     C                   movel     'M'           parmbr
037800040126     C                   eval      parccm = '02750752'
037900040119     C                   eval      paropz = '1'
038000040119     C* Effettuo la chiamata al CLLE preposto
038100040119     C                   call(e)   'TITVVTC'
038200040119     C                   parm                    parccm
038300040119     C                   parm                    parmbr
038400040119     C                   parm                    paropz
038500040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
038600040119     C                   if        %error
038700040119     C                   movel     '1'           chkcall
038800040119     C                   else
038900040119     C                   movel     '0'           chkcall
039000040119     C                   endif
039100040119     C*
039200040119     C                   ENDSR
039300010601
039400010601
039500010601
039600010330     C*----------------------------------------------------*
039700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
039800010330     C*----------------------------------------------------*
039900040119     C     INZVAR        BEGSR
040000010330     C*
040100030822     C* Inizializzo variabili di wrk
040200040126     C                   Z-ADD     *zeros        Num5_0            5 0
040300030822     C*
040400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
040500030822     C                   CLEAR                   FIVAB000
040600040119     C                   CLEAR                   FIVAT000
040700030822     C                   CLEAR                   SkSplitCSV
040800030822     C                   EVAL      i = 1
040900030822     C                   EVAL      posDa = *zeros
041000030822     C                   EVAL      posA  = *zeros
041100030822     C*
041200030822     C* Reimposto i valori di default
041300030822     C                   EXSR      DEFCAM
041400030822     C*
041500010330     C                   ENDSR
041600010330     C*----------------------------------------------------*
041700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
041800010330     C*----------------------------------------------------*
041900010330     C     DEFCAM        BEGSR
042000040119     C*
042100040119     C* Imposto i valori d default a programma dei campi fondamentali
042200040126     C                   EVAL      VABCCM = 2750752
042300040126     C                   EVAL      VATCCM = 2750752
042400040126     C                   EVAL      VABLNP = 275
042500040126     C                   EVAL      VATLNP = 275
042600040126     C                   EVAL      VABCTR = 000
042700040126     C                   EVAL      VATTRC = 'B'
042800030822     C*
042900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
043000030715     C* e delimitatore testo.
043100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
043200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
043300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
043400030822     C*
043500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
043600030715     C                   IF        %subst(vlrppt:1:1) = 'S'
043700030715     C                   SETON                                        50
043800030715     C                   ELSE
043900030715     C                   SETOFF                                       50
044000030715     C                   ENDIF
044100030822     C*
044200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
044300030822     C                   EVAL      posDaDft = 1
044400030822     C                   EVAL      posADft  = 0
044500030822     C                   EVAL      wGiroDft = 0
044600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
044700030822     C                             posDaDft > 0
044800030822     C*
044900030822     C* Gestisco il 1° giro
045000030822     C                   IF        wGiroDft = 0
045100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
045200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
045300030822     C* Incremento il contatore dei "giri"
045400030822     C                   EVAL      wGiroDft = 1
045500030822     C                   ELSE
045600030822     C                   EVAL      posDaDft = posADft
045700030822     C                   ENDIF
045800030822     C* Eseguo lo scan x trovare la fine del campo corrente
045900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
046000030822     C*
046100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
046200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
046300030822     C                   IF        posDaDft > 0 AND
046400030822     C                             posADft  > 0
046500030822     C* NCL
046600030822     C                   IF        %subst(
046700030822     C                             %subst(vlrppt:posDaDft+1:
046800030822     C                             posADft-posDaDft-1):1:3)
046900030822     C                             = 'NCL'
047000030822     C                   EVAL      PiStr=%trim(%subst(
047100030822     C                             %subst(vlrppt:posDaDft+1:
047200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047300030822     C                   EXSR      CHKNUM
047400030822     C                   IF        PiInt=*on
047500030822     C                   Z-ADD     PiVal         VABNCL
047600030822     C                   ENDIF
047700030822     C                   ENDIF
047800030822     C* CCM
047900030822     C                   IF        %subst(
048000030822     C                             %subst(vlrppt:posDaDft+1:
048100030822     C                             posADft-posDaDft-1):1:3)
048200030822     C                             = 'CCM'
048300030822     C                   EVAL      PiStr=%trim(%subst(
048400030822     C                             %subst(vlrppt:posDaDft+1:
048500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048600030822     C                   EXSR      CHKNUM
048700030822     C                   IF        PiInt=*on
048800030822     C                   Z-ADD     PiVal         VABCCM
048900040119     C                   Z-ADD     PiVal         VATCCM
049000030822     C                   ENDIF
049100030822     C                   ENDIF
049200030822     C* LNP
049300030822     C                   IF        %subst(
049400030822     C                             %subst(vlrppt:posDaDft+1:
049500030822     C                             posADft-posDaDft-1):1:3)
049600030822     C                             = 'LNP'
049700030822     C                   EVAL      PiStr=%trim(%subst(
049800030822     C                             %subst(vlrppt:posDaDft+1:
049900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050000030822     C                   EXSR      CHKNUM
050100030822     C                   IF        PiInt=*on
050200030822     C                   Z-ADD     PiVal         VABLNP
050300040119     C                   Z-ADD     PiVal         VATLNP
050400030822     C                   ENDIF
050500030822     C                   ENDIF
050600030822     C* NRS
050700030822     C                   IF        %subst(
050800030822     C                             %subst(vlrppt:posDaDft+1:
050900030822     C                             posADft-posDaDft-1):1:3)
051000030822     C                             = 'NRS'
051100030822     C                   EVAL      PiStr=%trim(%subst(
051200030822     C                             %subst(vlrppt:posDaDft+1:
051300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051400030822     C                   EXSR      CHKNUM
051500030822     C                   IF        PiInt=*on
051600030822     C                   Z-ADD     PiVal         VABNRS
051700030822     C                   ENDIF
051800030822     C                   ENDIF
051900030822     C* CTR
052000030822     C                   IF        %subst(
052100030822     C                             %subst(vlrppt:posDaDft+1:
052200030822     C                             posADft-posDaDft-1):1:3)
052300030822     C                             = 'CTR'
052400030822     C                   EVAL      PiStr=%trim(%subst(
052500030822     C                             %subst(vlrppt:posDaDft+1:
052600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052700030822     C                   EXSR      CHKNUM
052800030822     C                   IF        PiInt=*on
052900030822     C                   Z-ADD     PiVal         VABCTR
053000030822     C                   ENDIF
053100030822     C                   ENDIF
053200030822     C* PKB
053300030822     C                   IF        %subst(
053400030822     C                             %subst(vlrppt:posDaDft+1:
053500030822     C                             posADft-posDaDft-1):1:3)
053600030822     C                             = 'PKB'
053700030822     C                   EVAL      PiStr=%trim(%subst(
053800030822     C                             %subst(vlrppt:posDaDft+1:
053900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054000030822     C                   EXSR      CHKNUM
054100030822     C                   IF        PiNum=*on
054200030822     C                   Z-ADD     PiVal         VABPKB
054300030822     C                   ENDIF
054400030822     C                   ENDIF
054500030822     C* VLB
054600030822     C                   IF        %subst(
054700030822     C                             %subst(vlrppt:posDaDft+1:
054800030822     C                             posADft-posDaDft-1):1:3)
054900030822     C                             = 'VLB'
055000030822     C                   EVAL      PiStr=%trim(%subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055300030822     C                   EXSR      CHKNUM
055400030822     C                   IF        PiNum=*on
055500030822     C                   Z-ADD     PiVal         VABVLB
055600030822     C                   ENDIF
055700030822     C                   ENDIF
055800030822     C* QFT
055900030822     C                   IF        %subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):1:3)
056200030822     C                             = 'QFT'
056300030822     C                   EVAL      PiStr=%trim(%subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056600030822     C                   EXSR      CHKNUM
056700030822     C                   IF        PiNum=*on
056800030822     C                   Z-ADD     PiVal         VABQFT
056900030822     C                   ENDIF
057000030822     C                   ENDIF
057100030822     C* CBO
057200030822     C                   IF        %subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):1:3)
057500030822     C                             = 'CBO'
057600030822     C                   EVAL      VABCBO=%trim(%subst(
057700030822     C                             %subst(vlrppt:posDaDft+1:
057800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057900030822     C                   ENDIF
058000030822     C* TSP
058100030822     C                   IF        %subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):1:3)
058400030822     C                             = 'TSP'
058500030822     C                   EVAL      VABTSP=%trim(%subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058800030822     C                   ENDIF
058900030822     C* VAS
059000030822     C                   IF        %subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):1:3)
059300030822     C                             = 'VAS'
059400030822     C                   EVAL      VABVAS=%trim(%subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059700030822     C                   ENDIF
059800030822     C* VCA
059900030822     C                   IF        %subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):1:3)
060200030822     C                             = 'VCA'
060300030822     C                   EVAL      VABVCA=%trim(%subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060600030822     C                   ENDIF
060700030822     C* GCA
060800030822     C                   IF        %subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):1:3)
061100030822     C                             = 'GCA'
061200030822     C                   EVAL      VABGCA=%trim(%subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061500030822     C                   ENDIF
061600030822     C* CTM
061700030822     C                   IF        %subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):1:3)
062000030822     C                             = 'CTM'
062100030822     C                   EVAL      VABCTM=%trim(%subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062400030822     C                   ENDIF
062500030822     C* FFD
062600030822     C                   IF        %subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):1:3)
062900030822     C                             = 'FFD'
063000030822     C                   EVAL      VABFFD=%trim(%subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063300030822     C                   ENDIF
063400030822     C* VAD
063500030822     C                   IF        %subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):1:3)
063800030822     C                             = 'VAD'
063900030822     C                   EVAL      VABVAD=%trim(%subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064200030822     C                   ENDIF
064300030822     C* GMA
064400030822     C                   IF        %subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):1:3)
064700030822     C                             = 'GMA'
064800030822     C                   EVAL      VABGMA=%trim(%subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065100030822     C                   ENDIF
065200030822     C* GGA
065300030822     C                   IF        %subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):1:3)
065600030822     C                             = 'GGA'
065700030822     C                   EVAL      VABGGA=%trim(%subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066000030822     C                   ENDIF
066100030822     C* GVA
066200030822     C                   IF        %subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):1:3)
066500030822     C                             = 'GVA'
066600030822     C                   EVAL      VABGVA=%trim(%subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066900030822     C                   ENDIF
067000030822     C* TC1
067100030822     C                   IF        %subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):1:3)
067400030822     C                             = 'TC1'
067500030822     C                   EVAL      VABTC1=%trim(%subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067800030822     C                   ENDIF
067900030822     C* TC2
068000030822     C                   IF        %subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):1:3)
068300030822     C                             = 'TC2'
068400030822     C                   EVAL      VABTC2=%trim(%subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700030822     C                   ENDIF
068800030822     C* ...
068900030822     C                   ENDIF
069000030822     C                   ENDDO
069100020204     C*
069200010330     C                   ENDSR
069300010607     C*----------------------------------------------------*
069400021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
069500010607     C*----------------------------------------------------*
069600010607     C     IMPVAB        BEGSR
069700010607     C*
069800010607     C                   SETOFF                                       3132
069900030515     C*
070000010607     C                   MOVEL     datcor        VABAAS
070100040119     C                   MOVEL     datcor        VATAAS
070200010607     C                   MOVE      datcor        VABMGS
070300010607     C*
070400030715     C**********
070500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
070600030715     C**********
070700030715     C                   IF        wGiro = *zeros
070800040113     C*
070900040113     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
071000040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
071100040113     C                                      %trim(intcol(3))
071200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
071300031201     C     minu:maiu     XLATE     vindta        vindta
071400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
071500030715     C                   DOW       posDa <= %len(%trim(vindta))
071600030715     C*
071700030715     C* Gestisco il 1° campo
071800030715     C                   IF        i = 1
071900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
072000030715     C                   EVAL      posDa = 1
072100030715     C* Eseguo lo scan x trovare la fine del primo campo
072200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
072300030715     C* A questo punto "estraggo" il campo corrente
072400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
072500030715     C                                                    (posA-posDa))
072600030715     C* X i campi successivi al 1°
072700030715     C                   ELSE
072800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
072900030715     C                   EVAL      posDa = posA + 1
073000030715     C* Eseguo lo scan x trovare la fine del campo corrente
073100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
073200030715     C* Gestisco l'ultimo campo
073300030715     C                   IF        posA = *zeros
073400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
073500030715     C* A questo punto "estraggo" il campo corrente
073600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
073700030715     C                                                    (posA-posDa))
073800030715     C                   LEAVE
073900030715     C                   ELSE
074000030715     C* A questo punto "estraggo" il campo corrente
074100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
074200030715     C                                                    (posA-posDa))
074300030715     C                   ENDIF
074400030715     C                   ENDIF
074500030715     C* Incremento il contatore d campo
074600030715     C                   EVAL      i = i +1
074700030715     C                   ENDDO
074800030715     C*
074900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
075000030715     C* delimitazione testo
075100030715     C                   EVAL      i = 1
075200030715     C                   DOW       i < %elem(SkSplitFLD)
075300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
075400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
075500030715     C                   EVAL      i = i + 1
075600030715     C                   ENDDO
075700030715     C                   EVAL      wGiro = 1
075800030715     C                   ELSE
075900030715     C                   EVAL      wGiro = 2
076000031204     C**********
076100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
076200031204     C* d separatore campo
076300031204     C**********
076400031204     C                   EVAL      vindta = %trim(vindta)
076500031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
076600031204     C                   EVAL      vindta = %subst(vindta:2)
076700031204     C                   ENDDO
076800031204     C*
076900031204     C                   Z-ADD     *zeros        lunghInput        4 0
077000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
077100031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
077200031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
077300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
077400031204     C                   ENDDO
077500030715     C**********
077600030715     C* Effettuo lo split del campo dati d input
077700030715     C**********
077800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
077900030715     C                   DOW       posDa <= %len(%trim(vindta))
078000030715     C*
078100030715     C* Gestisco il 1° campo
078200030715     C                   IF        i = 1
078300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
078400030715     C                   EVAL      posDa = 1
078500030715     C* Eseguo lo scan x trovare la fine del primo campo
078600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
078700030715     C* A questo punto "estraggo" il campo corrente
078800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
078900030715     C                                                    (posA-posDa))
079000030715     C* X i campi successivi al 1°
079100030715     C                   ELSE
079200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
079300030715     C                   EVAL      posDa = posA + 1
079400031204     C* Verifico che nn vi sia il campo nn valorizzato
079500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
079600031204     C* Se campo nn valorizzato skippo al prossimo
079700031204     C                   EVAL      SkSplitCSV(i) = *blanks
079800031204     C                   EVAL      posA  = posA + 1
079900031204     C                   ELSE
080000030715     C* Eseguo lo scan x trovare la fine del campo corrente
080100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
080200030715     C* Gestisco l'ultimo campo
080300030715     C                   IF        posA = *zeros
080400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
080500030715     C* A questo punto "estraggo" il campo corrente
080600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
080700030715     C                                                    (posA-posDa))
080800030715     C                   LEAVE
080900030715     C                   ELSE
081000030715     C* A questo punto "estraggo" il campo corrente
081100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
081200030715     C                                                    (posA-posDa))
081300030715     C                   ENDIF
081400031204     C                   ENDIF
081500030715     C                   ENDIF
081600030715     C* Incremento il contatore d campo
081700030715     C                   EVAL      i = i +1
081800030715     C                   ENDDO
081900030715     C*
082000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
082100030715     C* delimitazione testo
082200030715     C                   EVAL      i = 1
082300030715     C                   DOW       i < %elem(SkSplitCSV)
082400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
082500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
082600030715     C                   EVAL      i = i + 1
082700030715     C                   ENDDO
082800030715     C*
082900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
083000030715     C                   EVAL      i = 1
083100030715     C                   DOW       i < %elem(SkSplitFLD)
083200030715     C*
083300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
083400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
083500030715     C                             SkSplitCSV(i) <> *blanks
083600030721     C*** ATB
083700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
083800030721     C                   EVAL      VABATB = SkSplitCSV(i)
083900030721     C                   ENDIF
084000030715     C*** RSD
084100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
084200030715     C                   EVAL      VABRSD = SkSplitCSV(i)
084300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
084400030715     C     '@':'A'       XLATE     VABRSD        VABRSD
084500030715     C* ==
084600030715     C                   ENDIF
084700030721     C*** RD2
084800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
084900030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
085000030721     C                   ENDIF
085100030715     C*** IND
085200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
085300030715     C                   EVAL      VABIND = SkSplitCSV(i)
085400030715     C                   ENDIF
085500030715     C*** LOD
085600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
085700030715     C                   EVAL      VABLOD = SkSplitCSV(i)
085800030715     C                   ENDIF
085900030715     C*** PRD
086000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
086100030715     C                   EVAL      VABPRD = SkSplitCSV(i)
086200030715     C                   ENDIF
086300030721     C*** NZD
086400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
086500030721     C                   EVAL      VABNZD = SkSplitCSV(i)
086600030721     C                   ENDIF
086700030715     C*** RMA
086800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
086900030715     C                   EVAL      VABRMA = SkSplitCSV(i)
087000030715     C                   ENDIF
087100030715     C*** NOT
087200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
087300030715     C                   EVAL      VABNOT = SkSplitCSV(i)
087400030715     C                   ENDIF
087500030715     C*** NT2
087600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
087700030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
087800030715     C                   ENDIF
087900030715     C*** VCA
088000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
088100030715     C                   EVAL      VABVCA = SkSplitCSV(i)
088200030715     C                   ENDIF
088300030715     C*** VAS
088400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
088500030715     C                   EVAL      VABVAS = SkSplitCSV(i)
088600030715     C                   ENDIF
088700030715     C*** FFD
088800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
088900030715     C                   EVAL      VABFFD = SkSplitCSV(i)
089000030715     C                   ENDIF
089100030721     C*** GC1
089200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
089300030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
089400030721     C                   ENDIF
089500030721     C*** GC2
089600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
089700030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
089800030721     C                   ENDIF
089900030721     C*** TSP
090000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
090100030721     C                   EVAL      VABTSP = SkSplitCSV(i)
090200030721     C                   ENDIF
090300030721     C*** NAS
090400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
090500030721     C                   EVAL      VABNAS = SkSplitCSV(i)
090600030721     C                   ENDIF
090700030721     C*** TIC
090800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
090900030721     C                   EVAL      VABTIC = SkSplitCSV(i)
091000030721     C                   ENDIF
091100030721     C*** GCA
091200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
091300030721     C                   EVAL      VABGCA = SkSplitCSV(i)
091400030721     C                   ENDIF
091500030721     C*** XCO
091600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
091700030721     C                   EVAL      VABXCO = SkSplitCSV(i)
091800030721     C                   ENDIF
091900030721     C*** CTM
092000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
092100030721     C                   EVAL      VABCTM = SkSplitCSV(i)
092200030721     C                   ENDIF
092300030721     C*** TCR
092400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
092500030721     C                   EVAL      VABTCR = SkSplitCSV(i)
092600030721     C                   ENDIF
092700030721     C*** CTS
092800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
092900030721     C                   EVAL      VABCTS = SkSplitCSV(i)
093000030721     C                   ENDIF
093100030721     C*** FTM
093200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
093300030721     C                   EVAL      VABFTM = SkSplitCSV(i)
093400030721     C                   ENDIF
093500030721     C*** VAD
093600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
093700030721     C                   EVAL      VABVAD = SkSplitCSV(i)
093800030721     C                   ENDIF
093900030721     C*** GMA
094000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
094100030721     C                   EVAL      VABGMA = SkSplitCSV(i)
094200030721     C                   ENDIF
094300030721     C*** GGA
094400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
094500030721     C                   EVAL      VABGGA = SkSplitCSV(i)
094600030721     C                   ENDIF
094700030721     C*** GVA
094800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
094900030721     C                   EVAL      VABGVA = SkSplitCSV(i)
095000030721     C                   ENDIF
095100030721     C*** TC1
095200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
095300030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
095400030721     C                   ENDIF
095500030721     C*** TC2
095600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
095700030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
095800030721     C                   ENDIF
095900030721     C*** SCL
096000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
096100030721     C                   EVAL      VABSCL = SkSplitCSV(i)
096200030721     C                   ENDIF
096300030721     C*** RMO
096400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
096500030721     C                   EVAL      VABRMO = SkSplitCSV(i)
096600030721     C                   ENDIF
096700030721     C*** NMO
096800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
096900030721     C                   EVAL      VABNMO = SkSplitCSV(i)
097000030721     C                   ENDIF
097100040119     C*
097200040126     C* Verifico i campi "personalizzati"
097300040119     C*** RMN
097400040126     C*** NSP
097500040126     C*** VATNSP
097600040119     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
097700040119     C                   EVAL      PiStr=SkSplitCSV(i)
097800040119     C                   EXSR      CHKNUM
097900040119     C                   IF        PiInt=*on
098000040119     C                   Z-ADD     PiVal         VABRMN
098100040119     C                   Z-ADD     PiVal         VABNSP
098200040119     C                   Z-ADD     PiVal         VATNSP
098300040119     C                   ELSE
098400040126     C                   SETON                                        31
098500040119     C                   EVAL      VABRMN = 1
098600040119     C                   EVAL      vinmsg = %trimr(vinmsg)
098700040126     C                             + ' ' + 'VABRMN VABNSP VATNSP'
098800040119     C                   ENDIF
098900040119     C                   ENDIF
099000040119     C*** CBO
099100040126     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
099200040126     C                   IF        %trim(SkSplitCSV(i)) = 'R01'
099300040119     C                   EVAL      VABCBO = '2'
099400040126     C                   ELSE
099500040119     C                   EVAL      VABCBO = '1'
099600040119     C                   ENDIF
099700040119     C                   ENDIF
099800040126     C*** VATNOT (tipo record 'B' = Telefono spedizione)
099900040126     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
100000040126     C                   EVAL      VATNOT = SkSplitCSV(i)
100100040126     C                   ENDIF
100200030721     C*
100300030721     C* Reperisco quindi i campi numerici...
100400030721     C*** CAD
100500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
100600030721     C                   EVAL      PiStr=SkSplitCSV(i)
100700030721     C                   EXSR      CHKNUM
100800030721     C                   IF        PiInt=*on
100900030721     C                   Z-ADD     PiVal         Num5_0
101000030721     C                   MOVEL(p)  Num5_0        VABCAD
101100030721     C                   ELSE
101200030721     C                   SETON                                        32
101300030721     C                   EVAL      VABCAD = *zeros
101400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101500030721     C                             + ' ' + 'VABCAD'
101600030721     C                   ENDIF
101700030721     C                   ENDIF
101800030721     C*** NCL
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
102000030721     C                   EVAL      PiStr=SkSplitCSV(i)
102100030721     C                   EXSR      CHKNUM
102200030721     C                   IF        PiInt=*on
102300030721     C                   Z-ADD     PiVal         VABNCL
102400030721     C                   ELSE
102500030721     C                   SETON                                        32
102600030721     C                   EVAL      VABNCL = *zeros
102700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102800040126     C                             + ' ' + 'VABNCL'
102900030721     C                   ENDIF
103000030721     C                   ENDIF
103100030721     C*** PKB
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
103300030721     C                   EVAL      PiStr=SkSplitCSV(i)
103400030721     C                   EXSR      CHKNUM
103500030721     C                   IF        PiNum=*on
103600030721     C                   Z-ADD     PiVal         VABPKB
103700030721     C                   ELSE
103800030721     C                   SETON                                        32
103900030721     C                   EVAL      VABPKB = *zeros
104000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104100030721     C                             + ' ' + 'VABPKB'
104200030721     C                   ENDIF
104300030721     C                   ENDIF
104400040126     C*** CAS
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
104600030721     C                   EVAL      PiStr=SkSplitCSV(i)
104700030721     C                   EXSR      CHKNUM
104800030721     C                   IF        PiNum=*on
104900030721     C                   Z-ADD     PiVal         VABCAS
105000030721     C                   ELSE
105100030721     C                   SETON                                        32
105200030721     C                   EVAL      VABCAS = *zeros
105300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105400030721     C                             + ' ' + 'VABCAS'
105500030721     C                   ENDIF
105600030721     C                   ENDIF
105700030721     C*** CCM
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
105900030721     C                   EVAL      PiStr=SkSplitCSV(i)
106000030721     C                   EXSR      CHKNUM
106100030721     C                   IF        PiInt=*on      AND
106200030721     C                             PiVal<=9999999 AND
106300030721     C                             PiVal>*zeros
106400030721     C                   Z-ADD     PiVal         VABCCM
106500040119     C                   Z-ADD     PiVal         VATCCM
106600030721     C                   ELSE
106700030721     C                   SETON                                        32
106800030721     C                   EVAL      VABCCM = *zeros
106900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107000040119     C                             + ' ' + 'VABCCM VATCCM'
107100030721     C                   ENDIF
107200030721     C                   ENDIF
107300030721     C*** LNP
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
107500030721     C                   EVAL      PiStr=SkSplitCSV(i)
107600030721     C                   EXSR      CHKNUM
107700030721     C                   IF        PiInt=*on    AND
107800030721     C                             PiVal<=999   AND
107900030721     C                             PiVal>*zeros
108000030721     C                   Z-ADD     PiVal         VABLNP
108100040119     C                   Z-ADD     PiVal         VATLNP
108200030721     C                   ELSE
108300030721     C                   SETON                                        32
108400030721     C                   EVAL      VABLNP = *zeros
108500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108600040119     C                             + ' ' + 'VABLNP VATLNP'
108700030721     C                   ENDIF
108800030721     C                   ENDIF
108900030721     C*** NRS
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
109100030721     C                   EVAL      PiStr=SkSplitCSV(i)
109200030721     C                   EXSR      CHKNUM
109300030721     C                   IF        PiInt=*on AND
109400030721     C                             PiVal<=99
109500030721     C                   Z-ADD     PiVal         VABNRS
109600030721     C                   ELSE
109700030721     C                   SETON                                        32
109800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109900030721     C                             + ' ' + 'VABNRS'
110000030721     C                   ENDIF
110100030721     C                   ENDIF
110200030721     C*** LNA
110300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
110400030721     C                   EVAL      PiStr=SkSplitCSV(i)
110500030721     C                   EXSR      CHKNUM
110600030721     C                   IF        PiInt=*on    AND
110700030721     C                             PiVal<=999   AND
110800030721     C                             PiVal>*zeros
110900030721     C                   Z-ADD     PiVal         VABLNA
111000030721     C                   ELSE
111100030721     C                   SETON                                        32
111200030721     C                   EVAL      VABLNA = *zeros
111300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111400030721     C                             + ' ' + 'VABLNA'
111500030721     C                   ENDIF
111600030721     C                   ENDIF
111700030721     C*** CTR
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
111900030721     C                   EVAL      PiStr=SkSplitCSV(i)
112000030721     C                   EXSR      CHKNUM
112100030721     C                   IF        PiInt=*on  AND
112200030721     C                             PiVal<=999
112300030721     C                   Z-ADD     PiVal         VABCTR
112400030721     C                   ELSE
112500030721     C                   SETON                                        32
112600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112700030721     C                             + ' ' + 'VABCTR'
112800030721     C                   ENDIF
112900030721     C                   ENDIF
113000030721     C*** IAS
113100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
113200030721     C                   EVAL      PiStr=SkSplitCSV(i)
113300030721     C                   EXSR      CHKNUM
113400030721     C                   IF        PiNum=*on
113500030721     C                   Z-ADD     PiVal         VABIAS
113600030721     C                   ELSE
113700030721     C                   SETON                                        32
113800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113900030721     C                             + ' ' + 'VABIAS'
114000030721     C                   ENDIF
114100030721     C                   ENDIF
114200030721     C*** VLB
114300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
114400030721     C                   EVAL      PiStr=SkSplitCSV(i)
114500030721     C                   EXSR      CHKNUM
114600030721     C                   IF        PiNum=*on
114700030721     C                   Z-ADD     PiVal         VABVLB
114800030721     C                   ELSE
114900030721     C                   SETON                                        32
115000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115100030721     C                             + ' ' + 'VABVLB'
115200030721     C                   ENDIF
115300030721     C                   ENDIF
115400030721     C*** QFT
115500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
115600030721     C                   EVAL      PiStr=SkSplitCSV(i)
115700030721     C                   EXSR      CHKNUM
115800030721     C                   IF        PiNum=*on
115900030721     C                   Z-ADD     PiVal         VABQFT
116000030721     C                   ELSE
116100030721     C                   SETON                                        32
116200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116300030721     C                             + ' ' + 'VABQFT'
116400030721     C                   ENDIF
116500030721     C                   ENDIF
116600030721     C*** NCD
116700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
116800030721     C                   EVAL      PiStr=SkSplitCSV(i)
116900030721     C                   EXSR      CHKNUM
117000030721     C                   IF        PiInt=*on      AND
117100030721     C                             PiVal<=9999999
117200030721     C                   Z-ADD     PiVal         VABNCD
117300030721     C                   ELSE
117400030721     C                   SETON                                        32
117500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117600030721     C                             + ' ' + 'VABNCD'
117700030721     C                   ENDIF
117800030721     C                   ENDIF
117900030721     C*** NCA
118000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
118100030721     C                   EVAL      PiStr=SkSplitCSV(i)
118200030721     C                   EXSR      CHKNUM
118300030721     C                   IF        PiInt=*on      AND
118400030721     C                             PiVal<=9999999
118500030721     C                   Z-ADD     PiVal         VABNCA
118600030721     C                   ELSE
118700030721     C                   SETON                                        32
118800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118900030721     C                             + ' ' + 'VABNCA'
119000030721     C                   ENDIF
119100030721     C                   ENDIF
119200030721     C*** ZNC
119300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
119400030721     C                   EVAL      PiStr=SkSplitCSV(i)
119500030721     C                   EXSR      CHKNUM
119600030721     C                   IF        PiInt=*on AND
119700030721     C                             PiVal<=99
119800030721     C                   Z-ADD     PiVal         VABZNC
119900030721     C                   ELSE
120000030721     C                   SETON                                        32
120100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120200030721     C                             + ' ' + 'VABZNC'
120300030721     C                   ENDIF
120400030721     C                   ENDIF
120500030721     C*** DCR
120600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
120700030721     C                   EVAL      PiStr=SkSplitCSV(i)
120800030721     C                   EXSR      CHKNUM
120900030721     C                   IF        PiInt=*on       AND
121000030721     C                             PiVal<=99999999
121100030721     C                   Z-ADD     PiVal         VABDCR
121200030721     C                   ELSE
121300030721     C                   SETON                                        32
121400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121500030721     C                             + ' ' + 'VABDCR'
121600030721     C                   ENDIF
121700030721     C                   ENDIF
121800030721     C*** HCR
121900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
122000030721     C                   EVAL      PiStr=SkSplitCSV(i)
122100030721     C                   EXSR      CHKNUM
122200030721     C                   IF        PiInt=*on   AND
122300030721     C                             PiVal<=9999
122400030721     C                   Z-ADD     PiVal         VABHCR
122500030721     C                   ELSE
122600030721     C                   SETON                                        32
122700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122800030721     C                             + ' ' + 'VABHCR'
122900030721     C                   ENDIF
123000030721     C                   ENDIF
123100030721     C*** VMD
123200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
123300030721     C                   EVAL      PiStr=SkSplitCSV(i)
123400030721     C                   EXSR      CHKNUM
123500030721     C                   IF        PiNum=*on
123600030721     C                   Z-ADD     PiVal         VABVMD
123700030721     C                   ELSE
123800030721     C                   SETON                                        32
123900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124000030721     C                             + ' ' + 'VABVMD'
124100030721     C                   ENDIF
124200030721     C                   ENDIF
124300030721     C*** ANT
124400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
124500030721     C                   EVAL      PiStr=SkSplitCSV(i)
124600030721     C                   EXSR      CHKNUM
124700030721     C                   IF        PiInt=*on        AND
124800030721     C                             PiVal<=999999999
124900030721     C                   Z-ADD     PiVal         VABANT
125000030721     C                   ELSE
125100030721     C                   SETON                                        32
125200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125300030721     C                             + ' ' + 'VABANT'
125400030721     C                   ENDIF
125500030721     C                   ENDIF
125600030721     C*** CMO
125700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
125800030721     C                   EVAL      PiStr=SkSplitCSV(i)
125900030721     C                   EXSR      CHKNUM
126000030721     C                   IF        PiInt=*on
126100030721     C                   Z-ADD     PiVal         Num5_0
126200030721     C                   MOVEL(p)  Num5_0        VABCMO
126300030721     C                   ELSE
126400030721     C                   SETON                                        32
126500030721     C                   EVAL      VABCMO = *zeros
126600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126700030721     C                             + ' ' + 'VABCMO'
126800030721     C                   ENDIF
126900030721     C                   ENDIF
127000030715     C*
127100030715     C                   ENDIF
127200030715     C                   EVAL      i = i + 1
127300030715     C                   ENDDO
127400040113     C*
127500040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
127600040113     C                   IF        VABCAS > *zeros
127700040119     C                   EVAL      VABVCA = 'EUR'
127800040119     C                   IF        VABCBO = '1'
127900040113     C                   EVAL      VABCBO = '4'
128000040119     C                   ENDIF
128100040119     C                   IF        VABCBO = '2'
128200040119     C                   EVAL      VABCBO = '6'
128300040119     C                   ENDIF
128400040113     C                   ENDIF
128500020204     C*
128600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
128700020204     C                   EXSR      CHKIMPDIV
128800030715     C                   ENDIF
128900020204     C*
129000010607     C                   ENDSR
129100010607     C*----------------------------------------------------*
129200010601
129300020204
129400020204     C*----------------------------------------------------*
129500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
129600020204     C*----------------------------------------------------*
129700020204     C     CHKIMPDIV     BEGSR
129800020204     C*
129900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
130000020204     C                   Z-ADD     *zeros        wrkDec            9 9
130100020204     C*
130200020204     C* Come prima cosa effettuo considerazioni sulla divisa
130300020204     C                   IF        vabIAS > *zeros
130400020204     C                   IF        vabVAS <> 'EUR'
130500020204     C                   EVAL      vabVAS =  'ITL'
130600020204     C                   ENDIF
130700020204     C                   ENDIF
130800020204     C*
130900020204     C                   IF        vabCAS > *zeros
131000020204     C                   IF        vabVCA <> 'EUR'
131100020204     C                   EVAL      vabVCA =  'ITL'
131200020204     C                   ENDIF
131300020204     C                   ENDIF
131400020204     C*
131500020204     C                   IF        vabVMD > *zeros
131600020204     C                   IF        vabVAD <> 'EUR'
131700020204     C                   EVAL      vabVAD =  'ITL'
131800020204     C                   ENDIF
131900020204     C                   ENDIF
132000020204     C*
132100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
132200020204     C                   Z-ADD     vabIAS        wrkDec
132300020204     C                   IF        wrkDec > *zeros
132400020204     C                   IF        vabVAS = 'ITL'
132500020204     C                   EVAL      vabIAS = *zeros
132600020204     C                   ENDIF
132700020204     C                   ENDIF
132800020204     C*
132900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
133000020204     C                   Z-ADD     vabCAS        wrkDec
133100020204     C                   IF        wrkDec > *zeros
133200020204     C                   IF        vabVCA = 'ITL'
133300020204     C                   EVAL      vabCAS = *zeros
133400020204     C                   ENDIF
133500020204     C                   ENDIF
133600020204     C*
133700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
133800020204     C                   Z-ADD     vabVMD        wrkDec
133900020204     C                   IF        wrkDec > *zeros
134000020204     C                   IF        vabVAD = 'ITL'
134100020204     C                   EVAL      vabVMD = *zeros
134200020204     C                   ENDIF
134300020204     C                   ENDIF
134400020204     C*
134500020204     C                   ENDSR
134600020204     C***
134700020204
134800010330
134900010330
135000010330     C*----------------------------------------------------*
135100010330     C*  CONTROLLO NUMERICITA' CAMPI
135200010330     C*----------------------------------------------------*
135300010330     C     CHKNUM        BEGSR
135400010330     C*
135500010606     C                   IF        PiDecChr = *blanks
135600030715     C                   EVAL      PiDecChr = CharNUM
135700010606     C                   ENDIF
135800010606     C*
135900010606     C                   CALL(e)   'ISNUMERIC'
136000010330     C                   PARM                    PiStr            30
136100010606     C                   PARM                    PiDecChr          1
136200010330     C                   PARM      *ZEROS        PiVal            30 9
136300010330     C                   PARM      '0'           PiInt             1
136400010330     C                   PARM      '0'           PiNum             1
136500010330     C                   IF        %error
136600010606     C                   EVAL      PiNum=*off
136700010330     C                   ENDIF
136800010330     C*
136900010330     C                   ENDSR
137000010330     C***
137100010330
137200010601
137300010601
137400010601
137500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
137600010601     C     repfil        BEGSR
137700010601     C*
137800010601     C                   if        invfil = *zeros and
137900010601     C                             depfil > *zeros and
138000010629     C                             (vinflg = *blanks or
138100010629     C                              vinflg = *zeros)
138200010601     C
138300010601     C                   eval      invfil = depfil
138400010601     C                   endif
138500010601     C*
138600010601     C                   if        depfil <> invfil and
138700010601     C                             invfil > *zeros
138800010601     C                   eval      flgMulti = '1'
138900010601     C                   if        vinflg = *blanks
139000010601     C                   add       1             cntNonEl
139100010601     C                   endif
139200010601     C                   endif
139300010601     C*
139400010601     C                   if        vinflg = '2'
139500010601     C                   eval      flgStato = '2'
139600010601     C                   endif
139700010601     C*
139800010601     C                   ENDSR
139900010601     C***
140000010601
140100010601
140200010601
140300010330
140400040119      /TITLE Invio dei dati al punto operativo.
140500040119     C     invio         BEGSR
140600040119     C*
140700040119     C* 1° invio FIVAT
140800040119     C                   reset                   dscmz
140900040119     C                   move      vlrpoi        cmzdst
141000040119     C                   eval      cmzfld = 'FIVATWWR'
141100040119     C                   eval      cmzmbd = vlrhdl
141200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
141300040119     C***                if        prmfir = *blanks
141400040119     C                   eval      cmzfla = 'FIVAT00F'
141500040119     C                   eval      cmzmba = 'FIVAT00F'
141600040119     C***                else
141700040119     C***                eval      cmzfla = prmfir
141800040119     C***                eval      cmzmba = prmfir
141900040119     C***                endif
142000040119     C                   eval      cmznrr = *zeros
142100040119     C                   move      §ctrokvt      cmznrr
142200040119     C                   eval      cmzlba = vlrfl1
142300040119     C                   call(e)   'TIS711C'
142400040119     C                   parm                    dscmz
142500040119     C                   parm      *blanks       esito
142600040119     C                   if        %error
142700040119     C                             or cmzerr = '1'
142800040119     C                             or esito  = '1'
142900040119     C                   eval      wrkesito = '3'
143000040119     C                   else
143100040119     C*
143200040119     C* 2° invio FIVAB
143300040119     C                   reset                   dscmz
143400040119     C                   move      vlrpoi        cmzdst
143500040119     C                   eval      cmzfld = vlrfou
143600040119     C                   eval      cmzmbd = vlrhdl
143700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
143800040119     C***                if        prmfir = *blanks
143900040119     C                   eval      cmzfla = 'FIVAB00F'
144000040119     C                   eval      cmzmba = 'FIVAB00F'
144100040119     C***                else
144200040119     C***                eval      cmzfla = prmfir
144300040119     C***                eval      cmzmba = prmfir
144400040119     C***                endif
144500040119     C                   eval      cmznrr = *zeros
144600040119     C                   move      §ctrokvb      cmznrr
144700040119     C                   eval      cmzlba = vlrfl1
144800040119     C                   call(e)   'TIS711C'
144900040119     C                   parm                    dscmz
145000040119     C                   parm      *blanks       esito
145100040119     C                   if        %error
145200040119     C                             or cmzerr = '1'
145300040119     C                             or esito  = '1'
145400040119     C                   eval      wrkesito = '3'
145500040119     C                   endif
145600040119     C                   endif
145700040119     C*
145800040119     C                   ENDSR
145900040119     C***
146000010601
146100010601
146200010601
146300010601
146400010601      /TITLE Invio dei dati al punto operativo.
146500010601     C     opeini        BEGSR
146600010601     C*
146700010601     C* Inizializzo flag e contatori operativi
146800010601     C                   movel     '0'           flgGiro           1
146900010601     C                   movel     '0'           flgMulti          1
147000010601     C                   movel     '1'           flgStato          1
147100010615     C                   movel     '0'           flgOk             1
147200010601     C                   z-add     *zeros        cntNonEl         10 0
147300010601     C                   z-add     *zeros        depfil            3 0
147400010601     C                   z-add     *zeros        invfil            3 0
147500010601     C*
147600010601     C                   ENDSR
147700010601     C***
147800050421
147900070411
148000070411     C     *pssr         BEGSR
148100070411     C*
148200070411     C                   if        %open(tivin00r)
148300070411     C                   close     tivin00r
148400070411     C                   endif
148500070411     C                   if        %open(fivabwwr)
148600070411     C                   close     fivabwwr
148700070411     C                   endif
148800070411     C                   if        %open(fivatwwr)
148900070411     C                   close     fivatwwr
149000070411     C                   endif
149100070411     C*
149200070411     C* Effettuo la chiamata al CLLE preposto
149300070411     C                   call(e)   'TITVVTC'
149400070411     C                   parm                    parccm
149500070411     C                   parm                    parmbr
149600070411     C                   parm      '2'           paropz
149700070411     C*
149800070411     C                   eval      wrkesito = '2'
149900070411     C*
150000070411     C                   seton                                        LR
150100070411     C*
150200070411     C                   ENDSR     '*CANCL'
150300070411     C***
150400070411
150500010601
150600010330
150700010330
150800000613     C     *inzsr        BEGSR
150900990910     C*
151000990910     C     *entry        plist
151100990920     C                   parm                    tivlrds
151200990921     C                   parm      wrkesito      esito
151300000724     C                   parm                    prmlit
151400000710     C                   parm                    prmfir
151500010330     C*
151600010330     C* CALCOLA LA DATA CORRENTE
151700010330     C                   time                    wn14             14 0
151800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
151900010330     C                   z-add     wn8           g08dat
152000010330     C                   z-add     *zeros        g08inv
152100010330     C                   movel     '0'           g08err
152200010330     C                   call      'XSRDA8'
152300010330     C                   parm                    wlbda8
152400010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
152500000613     C*
152600000613     C                   ENDSR
152700000613     C***
152800990908
152900040126     Otitv93p   E            riepilogo         2
153000990915     O                                              'Upload via Internet'
153100990915     O                                           +1 'Traduzione TIVIN00R -'
153200021025     O                                           +1 'FIVABWWR'
153300990915     O                                           +1 'Testate bolle'
153400990915     O          E            riepilogo   2
153500990915     O                       wrkdata
153600990915     O                       wrkora              +1
153700990915     O                       procname            +1
153800990915     O          E            riepilogo   2
153900990915     O                                              'Cliente..................:'
154000010605     O                       VABCCM        z     +1
154100990915     O          E            riepilogo   2
154200990920     O                                              'Riferimento Strategi.....:'
154300990920     O                       vlrhdl              +1
154400990915     O          E            riepilogo   2
154500990915     O                                              'Giusti...................:'
154600040119     O                       §CTROKVB      2   +  1
154700990915     O          E            riepilogo   2
154800990915     O                                              'Sbagliati e corretti.....:'
154900971022     O                       §CTRMO        2   +  1
155000990915     O          E            riepilogo   2
155100990915     O                                              'Sbagliati e scartati.....:'
155200971022     O                       §CTRNO        2   +  1
155300000613
155400040126     Otitv93ps  E            testdett          2
155500000613     O                                              'Upload via Internet'
155600000613     O                                           +1 'Traduzione TIVIN00R -'
155700021025     O                                           +1 'FIVABWWR'
155800000613     O                                           +1 'Testate bolle'
155900000616     O          E            testdett    3
156000000613     O                                           +2 'N° rec'
156100000613     O                                           +3 'Anteprima contenuto'
156200000616     O          E            rigadett    2
156300000613     O                       rrnum               +2
156400000621     O                       recko               +3
156500000616     O          E            findett     2
156600000613     O                       wrkdata
156700000613     O                       wrkora              +1
156800000613     O                       procname            +1
156900000616     O          E            findett     2
157000000613     O                                              'Cliente..................:'
157100010605     O                       VABCCM        z     +1
157200000616     O          E            findett     2
157300000613     O                                              'Riferimento Strategi.....:'
157400000613     O                       vlrhdl              +1
157500000616     O          E            findett     2
157600000613     O                                              'Giusti...................:'
157700040119     O                       §CTROKVB      2   +  1
157800000616     O          E            findett     2
157900000613     O                                              'Sbagliati e corretti.....:'
158000000613     O                       §CTRMO        2   +  1
158100000616     O          E            findett     2
158200000613     O                                              'Sbagliati e scartati.....:'
158300000613     O                       §CTRNO        2   +  1
158400000613** CMD - COMANDI CL
158500040126OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV93P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
158600040126OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV93PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
158700040126DLTOVR FILE(TITV93P TITV93PS) LVL(*)
158800010606
158900000613
159000040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
159100040126VABRMN;VABRMA;cod.destinazione;N^spedizione;VABCBO;VABNCL;VABPKB;
159200040126VABRSD;VABIND;VATNOT;VABCAD;VABLOD;VABPRD;VABCAS;
159300040113
