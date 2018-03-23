000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800051108     Ftitvo9p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100051108     Ftitvo9ps  O    f  198        PRINTER usropn
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
004000040709     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004100040119     D parccm          s              8    INZ(*blanks)
004200040119     D parmbr          s             10    INZ(*blanks)
004300040119     D paropz          s              1    INZ(*blanks)
004400040119     D chkcall         s              1    INZ(*blanks)
004500040719     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
004600030715     D*------------------
004700030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004800030715     D*------------------
004900030715     D SkSplitFLD      S             10    DIM(100)
005000030715     D SkSplitCSV      S            256    DIM(100)
005100030715     D CharCSV         S              1
005200030715     D CharTXT         S              1
005300030715     D CharNUM         S              1
005400030715     D posDa           S              3  0 INZ(*zeros)
005500030715     D posA            S              3  0 INZ(*zeros)
005600030715     D i               s              3  0 INZ(1)
005700030715     D wGiro           s              1  0 INZ(*zeros)
005800051108     D wVATNOT_A       s                   LIKE(vatnot) inz(*blanks)
005900051108     D wVATNOT_B       s                   LIKE(vatnot) inz(*blanks)
006000030822     D*------------------
006100030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006200030822     D*------------------
006300030822     D posDaDft        S              3  0 INZ(*zeros)
006400030822     D posADft         S              3  0 INZ(*zeros)
006500030822     D j               s              3  0 INZ(1)
006600030822     D wGiroDft        s              1  0 INZ(*zeros)
006700010330     D*------------------
006800010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
006900010330     D*------------------
007000010330     D WLBDA8          DS                  INZ
007100010330     D  G08DAT                 1      8  0
007200010330     D  G08INV                 9     16  0
007300010330     D  G08ERR                17     17
007400010330     D  G08TGI                18     22  0
007500031201     D*------------------
007600031201     D* Costanti
007700031201     D*------------------
007800031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007900031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008000010330
008100010330
008200010330
008300990915     C                   time                    wrkdata
008400990915     C                   time                    wrkora
008500000913     C                   reset                   rrnum
008600990921     C                   reset                   esito
008700990921     C                   reset                   wrkesito
008800010601     C*
008900010601     C                   exsr      opeini
009000010605     C                   exsr      rwvab
009100010601     C*
009200010601     C                   seton                                        lr
009300010601
009400010601
009500010601
009600010601     C*--------------------------------------------------------
009700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009800010601     C*--------------------------------------------------------
009900010601     C     PREELA        BEGSR
010000010601     C*
010100010601     C* SE OCCORRE SPEDIRE IN FILIALE
010200010601     C                   if        invfil <> *zeros and
010300010601     C                             flgGiro = '0'
010400010601     C*
010500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010600010601     C                   eval      flgGiro = '1'
010700000724     C*
010800000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
010900000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011000010601     C     invfil        chain     azorg01l
011100000724     C                   if        %found
011200000724     C                   movel(p)  CMD(1)        depcmd
011300020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011400000724     C*
011500000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011600011113     C                   Z-ADD     150           LENGH            15 5
011700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011800000724     C                   PARM                    depcmd
011900000724     C                   PARM                    LENGH
012000000724     C*
012100000724     C                   endif
012200000616     C*
012300000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012400000616     C                   movel(p)  CMD(2)        depcmd
012500000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012600000616     C*
012700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012800011113     C                   Z-ADD     150           LENGH            15 5
012900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013000000616     C                   PARM                    depcmd
013100000616     C                   PARM                    LENGH
013200000613     C*
013300051108     C                   if        not %open(titvo9ps)
013400051108     C                   open      titvo9ps
013500030715     C                   except    testdett
013600000613     C                   endif
013700010601     C*
013800010601     C                   endif
013900010601     C*
014000010601     C                   ENDSR
014100010601     C***
014200010601
014300010601
014400010601
014500010601
014600010601     C*--------------------------------------------------------
014700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014800010601     C*--------------------------------------------------------
014900010601     C     ENDELA        BEGSR
015000990915     C*
015100000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015200040119     C*
015300040119     C* Effettuo la chiamata al CLLE preposto
015400040119     C                   call(e)   'TITVVTC'
015500040119     C                   parm                    parccm
015600040119     C                   parm                    parmbr
015700040119     C                   parm      '2'           paropz
015800050201     C*
015900050201     C* Effettuo lancio TISI95 solo x chiusura
016000050201     C                   CLEAR                   TISI95DS
016100050201     C                   EVAL      I95TLA = 'C'
016200050201     C                   CALL      'TISI95R'
016300050201     C                   PARM                    TISI95DS
016400000613     C*
016500051108     C                   if        %open(titvo9ps)
016600000616     C                   except    findett
016700051108     C                   close     titvo9ps
016800000613     C                   endif
016900000616     C*
017000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
017100011113     C                   Z-ADD     150           LENGH            15 5
017200010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
017300000616     C                   PARM                    CMD(3)
017400000616     C                   PARM                    LENGH
017500000616     C*
017600010601     C                   ENDSR
017700010601     C***
017800010601
017900010601
018000010601
018100000613
018200000613     C*--------------------------------------------------------
018300000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018400000613     C*--------------------------------------------------------
018500000621     C     STPR          BEGSR
018600000613     C*
018700051108     C                   if        not %open(titvo9p)
018800051108     C   50              open      titvo9p
018900990915     C                   endif
019000990915     C*
019100030715     C   50              except    riepilogo
019200990915     C*
019300051108     C                   if        %open(titvo9p)
019400051108     C   50              close     titvo9p
019500990914     C                   endif
019600000613     C*
019700000613     C                   ENDSR
019800000613     C***
019900000613
020000010601
020100010601
020200010330     C*--------------------------------------------------------
020300021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
020400010330     C*--------------------------------------------------------
020500010605     C     RWVAB         BEGSR
020600010330     C*
020700010330     C                   if        not %open(tivin00r)
020800010330     C                   open      tivin00r
020900010330     C                   endif
021000021025     C                   if        not %open(fivabwwr)
021100021025     C                   open      fivabwwr
021200010330     C                   endif
021300040119     C*
021400040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
021500040119     C                   exsr      prevat
021600040119     C*
021700040119     C                   if        chkcall = '0'
021800040119     C*
021900040119     C                   if        not %open(fivatwwr)
022000040119     C                   open      fivatwwr
022100040119     C                   endif
022200010330     C*
022300040119     C                   clear                   §CTROKVB          7 0
022400040119     C                   clear                   §CTROKVT          7 0
022500040119     C                   clear                   §CTRMO            7 0
022600040119     C                   clear                   §CTRNO            7 0
022700040719     C*
022800040719     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
022900040719     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
023000040719     C                   exsr      inzvar
023100040719     C                   exsr      impvab
023200040706     C*
023300040706     C  N70              do        *HIVAL
023400010330     C*
023500040706     C                   read      tivin00r                               70
023600010618     C*
023700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
023800010618     C                   if        vinflg = '1'
023900010618     C                   eval      flgOk = '1'
024000010618     C                   endif
024100010618     C*
024200040316     C                   if        (vindta > *blanks AND
024300040316     C                              %trim(vindta) <> *all';') OR
024400040316     C                             wGiro = *zeros
024500010330     C                   add       1             rrnum
024600010330     C*
024700010601     C                   if        *in70 = *off and
024800010330     C                             (vinflg = *blanks
024900010330     C                              or vinflg = '0'
025000010330     C                              or vinflg = '2')
025100010330     C*
025200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
025300010711     C                   if        vinflg = *blanks or vinflg = '0'
025400010711     C                   clear                   vinmsg
025500010711     C                   endif
025600010601     C*
025700010330     C                   exsr      inzvar
025800010605     C                   exsr      impvab
025900010601     C*
026000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
026100010605     C                   eval      depfil = VABLNP
026200010601     C                   exsr      repfil
026300010601     C                   if        depfil = invfil
026400021025     C                   if        vlrpoi = 999
026500030822     C                   move(P)   invfil        VABFGS
026600040119     C                   move(P)   invfil        VATFGS
026700021025     C                   else
026800030822     C                   move(P)   vlrpoi        VABFGS
026900040119     C                   move(P)   vlrpoi        VATFGS
027000021025     C                   endif
027100010601     C*
027200010601     C                   exsr      PREELA
027300010601     C*
027400010604     C* Ebbene...
027500010604     C*
027600040119     C  N31              add       1             §CTROKVB
027700040119     C   32              add       1             §CTRMO
027800040119     C   31              add       1             §CTRNO
027900030822     C                   if        wGiro = 2
028000040119     C                   if        depspe =  *zeros OR
028100040119     C                             depspe <> vabNSP
028200040119     C  N31              exsr      wrivab
028300040119     C  N31              eval      depspe = vabNSP
028400040119     C                   endif
028500040119     C                   endif
028600010604     C*
028700010604     C                   if        *in31 = *off and
028800010604     C                             *in32 = *off
028900010604     C                   eval      vinflg = '1'
029000010604     C                   else
029100010604     C                   eval      recko = vindta
029200010604     C                   except    rigadett
029300010604     C                   eval      vinflg = '2'
029400010604     C                   endif
029500010604     C                   endif
029600010601     C*
029700010604     C                   endif
029800010604     C*
029900010330     C                   else
030000010330     C                   eval      vinflg = '1'
030100010330     C                   endif
030200010601     C*
030300010601     C  N70              update    tivin000
030400010330     C*
030500030822     C  N70              enddo
030600040119     C*
030700040119     C                   endif                                                  (endif - chkcall)
030800010601     C*
030900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
031000010601     C                   if        cntNonEl = *zeros or
031100010601     C                             flgMulti = '0'
031200010330     C* Se non ci sono record con errori ...
031300010601     C                   if        §ctrno = 0 and
031400010604     C                             §ctrmo = 0 and
031500010601     C                             flgStato <> '2'
031600010330     C* ... restituisco esito OK.
031700010330     C                   eval      wrkesito = '0'
031800010330     C                   else
031900040119     C                   if        §ctrokvb > 0
032000010330     C                   eval      wrkesito = '1'
032100010330     C                   else
032200010615     C                   if        flgOk = '0'
032300010615     C                   eval      wrkesito = '2'
032400010615     C                   else
032500010615     C                   eval      wrkesito = '6'
032600010615     C                   endif
032700010330     C                   endif
032800010330     C                   endif
032900010601     C                   else
033000010601     C                   eval      wrkesito = '9'
033100010601     C                   endif
033200010330     C*
033300010330     C                   if        %open(tivin00r)
033400010330     C                   close     tivin00r
033500010330     C                   endif
033600021025     C                   if        %open(fivabwwr)
033700021025     C                   close     fivabwwr
033800010330     C                   endif
033900040512     C                   if        %open(fivatwwr)
034000040512     C                   close     fivatwwr
034100040512     C                   endif
034200010601     C*
034300010601     C                   if        vlrpoi <> 999
034400010601     C                   eval      invfil = vlrpoi
034500010601     C                   endif
034600010330     C*
034700040119     C                   if        §ctrokvb > 0
034800010601     C                             and invfil > *zeros
034900010330     C                   exsr      invio
035000010330     C                   endif
035100010601     C*
035200010618     C                   if        flgGiro = '1'
035300010601     C                   exsr      endela
035400010618     C                   endif
035500010330     C*
035600010330     C                   ENDSR
035700010330     C***
035800040119
035900040119
036000040119
036100040119     C*----------------------------------------------------*
036200040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036300040119     C*----------------------------------------------------*
036400040119     C     WRIVAB        BEGSR
036500040119     C*
036600040119     C                   write     fivab000                                     => scarico il VAB
036700040119     C*
036800040119     C* Eseguo la scrittura dei "CHI SONO" della spedizione corrente
036900040119     C                   exsr      wrivat
037000040119     C*
037100040119     C                   ENDSR
037200040119
037300040119
037400040119
037500040119     C*----------------------------------------------------*
037600040119     C*  SCARICAMENTO BUFFER RECORDS VAT
037700040119     C*----------------------------------------------------*
037800040706     C     WRIVAT        BEGSR
037900040706     C*
038000040719     C* Attribuisco la stessa chiave della testata bolle
038100040719     C                   EVAL      VATAAS = VABAAS
038200040719     C                   EVAL      VATLNP = VABLNP
038300040719     C                   EVAL      VATNSP = VABNSP
038400040719     C*
038500040719     C* Imposto il tipo record sul file VAT a 'B' x indicare il telefono destinatario
038600051108     C                   EVAL      VATTRC = 'B'
038700051108     C                   EVAL      VATNOT = %trim(wVATNOT_B)
038800040719     C*
038900040719     C* Quindi se valorizzato il VATNOT scarico il buffer e incremento il contatore x il CMZ
039000040719     C                   IF        VATNOT <> *blanks
039100040706     C                   WRITE     FIVAT000
039200040706     C                   ADD       1             §CTROKVT
039300040719     C                   ENDIF
039400051108     C*
039500051108     C* Imposto il tipo record sul file VAT a 'A' x indicare il referente
039600051108     C                   EVAL      VATTRC = 'A'
039700051108     C                   EVAL      VATNOT = %trim(wVATNOT_A)
039800051108     C*
039900051108     C* Quindi se valorizzato il VATNOT scarico il buffer e incremento il contatore x il CMZ
040000051108     C                   IF        VATNOT <> *blanks
040100051108     C                   WRITE     FIVAT000
040200051108     C                   ADD       1             §CTROKVT
040300051108     C                   ENDIF
040400040119     C*
040500040119     C                   ENDSR
040600040119
040700040119
040800040119
040900040119     C*----------------------------------------------------*
041000040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
041100040119     C*----------------------------------------------------*
041200040119     C     PREVAT        BEGSR
041300040119     C*
041400040119     C* Compongo il nome del membro da dare al FIVATWWR
041500040119     C                   eval      parmbr = vlrhdl
041600040119     C                   movel     'M'           parmbr
041700051108     C                   eval      parccm = '00012782'
041800040119     C                   eval      paropz = '1'
041900040119     C* Effettuo la chiamata al CLLE preposto
042000040119     C                   call(e)   'TITVVTC'
042100040119     C                   parm                    parccm
042200040119     C                   parm                    parmbr
042300040119     C                   parm                    paropz
042400040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
042500040119     C                   if        %error
042600040119     C                   movel     '1'           chkcall
042700040119     C                   else
042800040119     C                   movel     '0'           chkcall
042900040119     C                   endif
043000040119     C*
043100040119     C                   ENDSR
043200010601
043300010601
043400010601
043500010330     C*----------------------------------------------------*
043600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
043700010330     C*----------------------------------------------------*
043800040119     C     INZVAR        BEGSR
043900010330     C*
044000030822     C* Inizializzo variabili di wrk
044100020204     C                   Z-ADD     *zeros        Num5_0            5 0
044200030822     C*
044300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
044400030822     C                   CLEAR                   FIVAB000
044500040119     C                   CLEAR                   FIVAT000
044600030822     C                   CLEAR                   SkSplitCSV
044700030822     C                   EVAL      i = 1
044800030822     C                   EVAL      posDa = *zeros
044900030822     C                   EVAL      posA  = *zeros
045000030822     C*
045100030822     C* Reimposto i valori di default
045200030822     C                   EXSR      DEFCAM
045300030822     C*
045400010330     C                   ENDSR
045500010330     C*----------------------------------------------------*
045600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
045700010330     C*----------------------------------------------------*
045800010330     C     DEFCAM        BEGSR
045900040119     C*
046000040119     C* Imposto i valori d default a programma dei campi fondamentali
046100051108     C                   EVAL      VABCCM = 0012782
046200051108     C                   EVAL      VATCCM = 0012782
046300040719     C                   EVAL      VABLNP = 001
046400040719     C                   EVAL      VATLNP = 001
046500040706     C                   EVAL      VABCTR = 000
046600040119     C                   EVAL      VABCBO = '1'
046700030822     C*
046800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
046900030715     C* e delimitatore testo.
047000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
047100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
047200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
047300030822     C*
047400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
047500030715     C                   IF        %subst(vlrppt:1:1) = 'S'
047600030715     C                   SETON                                        50
047700030715     C                   ELSE
047800030715     C                   SETOFF                                       50
047900030715     C                   ENDIF
048000030822     C*
048100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
048200030822     C                   EVAL      posDaDft = 1
048300030822     C                   EVAL      posADft  = 0
048400030822     C                   EVAL      wGiroDft = 0
048500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
048600030822     C                             posDaDft > 0
048700030822     C*
048800030822     C* Gestisco il 1° giro
048900030822     C                   IF        wGiroDft = 0
049000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
049100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
049200030822     C* Incremento il contatore dei "giri"
049300030822     C                   EVAL      wGiroDft = 1
049400030822     C                   ELSE
049500030822     C                   EVAL      posDaDft = posADft
049600030822     C                   ENDIF
049700030822     C* Eseguo lo scan x trovare la fine del campo corrente
049800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
049900030822     C*
050000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
050100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
050200030822     C                   IF        posDaDft > 0 AND
050300030822     C                             posADft  > 0
050400030822     C* NCL
050500030822     C                   IF        %subst(
050600030822     C                             %subst(vlrppt:posDaDft+1:
050700030822     C                             posADft-posDaDft-1):1:3)
050800030822     C                             = 'NCL'
050900030822     C                   EVAL      PiStr=%trim(%subst(
051000030822     C                             %subst(vlrppt:posDaDft+1:
051100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051200030822     C                   EXSR      CHKNUM
051300030822     C                   IF        PiInt=*on
051400030822     C                   Z-ADD     PiVal         VABNCL
051500030822     C                   ENDIF
051600030822     C                   ENDIF
051700030822     C* CCM
051800030822     C                   IF        %subst(
051900030822     C                             %subst(vlrppt:posDaDft+1:
052000030822     C                             posADft-posDaDft-1):1:3)
052100030822     C                             = 'CCM'
052200030822     C                   EVAL      PiStr=%trim(%subst(
052300030822     C                             %subst(vlrppt:posDaDft+1:
052400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052500030822     C                   EXSR      CHKNUM
052600030822     C                   IF        PiInt=*on
052700030822     C                   Z-ADD     PiVal         VABCCM
052800040119     C                   Z-ADD     PiVal         VATCCM
052900030822     C                   ENDIF
053000030822     C                   ENDIF
053100030822     C* LNP
053200030822     C                   IF        %subst(
053300030822     C                             %subst(vlrppt:posDaDft+1:
053400030822     C                             posADft-posDaDft-1):1:3)
053500030822     C                             = 'LNP'
053600030822     C                   EVAL      PiStr=%trim(%subst(
053700030822     C                             %subst(vlrppt:posDaDft+1:
053800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053900030822     C                   EXSR      CHKNUM
054000030822     C                   IF        PiInt=*on
054100030822     C                   Z-ADD     PiVal         VABLNP
054200040119     C                   Z-ADD     PiVal         VATLNP
054300030822     C                   ENDIF
054400030822     C                   ENDIF
054500030822     C* NRS
054600030822     C                   IF        %subst(
054700030822     C                             %subst(vlrppt:posDaDft+1:
054800030822     C                             posADft-posDaDft-1):1:3)
054900030822     C                             = 'NRS'
055000030822     C                   EVAL      PiStr=%trim(%subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055300030822     C                   EXSR      CHKNUM
055400030822     C                   IF        PiInt=*on
055500030822     C                   Z-ADD     PiVal         VABNRS
055600030822     C                   ENDIF
055700030822     C                   ENDIF
055800030822     C* CTR
055900030822     C                   IF        %subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):1:3)
056200030822     C                             = 'CTR'
056300030822     C                   EVAL      PiStr=%trim(%subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056600030822     C                   EXSR      CHKNUM
056700030822     C                   IF        PiInt=*on
056800030822     C                   Z-ADD     PiVal         VABCTR
056900030822     C                   ENDIF
057000030822     C                   ENDIF
057100030822     C* PKB
057200030822     C                   IF        %subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):1:3)
057500030822     C                             = 'PKB'
057600030822     C                   EVAL      PiStr=%trim(%subst(
057700030822     C                             %subst(vlrppt:posDaDft+1:
057800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057900030822     C                   EXSR      CHKNUM
058000030822     C                   IF        PiNum=*on
058100030822     C                   Z-ADD     PiVal         VABPKB
058200030822     C                   ENDIF
058300030822     C                   ENDIF
058400030822     C* VLB
058500030822     C                   IF        %subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):1:3)
058800030822     C                             = 'VLB'
058900030822     C                   EVAL      PiStr=%trim(%subst(
059000030822     C                             %subst(vlrppt:posDaDft+1:
059100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059200030822     C                   EXSR      CHKNUM
059300030822     C                   IF        PiNum=*on
059400030822     C                   Z-ADD     PiVal         VABVLB
059500030822     C                   ENDIF
059600030822     C                   ENDIF
059700030822     C* QFT
059800030822     C                   IF        %subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):1:3)
060100030822     C                             = 'QFT'
060200030822     C                   EVAL      PiStr=%trim(%subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060500030822     C                   EXSR      CHKNUM
060600030822     C                   IF        PiNum=*on
060700030822     C                   Z-ADD     PiVal         VABQFT
060800030822     C                   ENDIF
060900030822     C                   ENDIF
061000030822     C* CBO
061100030822     C                   IF        %subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):1:3)
061400030822     C                             = 'CBO'
061500030822     C                   EVAL      VABCBO=%trim(%subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061800030822     C                   ENDIF
061900030822     C* TSP
062000030822     C                   IF        %subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):1:3)
062300030822     C                             = 'TSP'
062400030822     C                   EVAL      VABTSP=%trim(%subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062700030822     C                   ENDIF
062800030822     C* VAS
062900030822     C                   IF        %subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):1:3)
063200030822     C                             = 'VAS'
063300030822     C                   EVAL      VABVAS=%trim(%subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600030822     C                   ENDIF
063700030822     C* VCA
063800030822     C                   IF        %subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):1:3)
064100030822     C                             = 'VCA'
064200030822     C                   EVAL      VABVCA=%trim(%subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064500030822     C                   ENDIF
064600030822     C* GCA
064700030822     C                   IF        %subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):1:3)
065000030822     C                             = 'GCA'
065100030822     C                   EVAL      VABGCA=%trim(%subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065400030822     C                   ENDIF
065500030822     C* CTM
065600030822     C                   IF        %subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):1:3)
065900030822     C                             = 'CTM'
066000030822     C                   EVAL      VABCTM=%trim(%subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066300030822     C                   ENDIF
066400030822     C* FFD
066500030822     C                   IF        %subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):1:3)
066800030822     C                             = 'FFD'
066900030822     C                   EVAL      VABFFD=%trim(%subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200030822     C                   ENDIF
067300030822     C* VAD
067400030822     C                   IF        %subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):1:3)
067700030822     C                             = 'VAD'
067800030822     C                   EVAL      VABVAD=%trim(%subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100030822     C                   ENDIF
068200030822     C* GMA
068300030822     C                   IF        %subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):1:3)
068600030822     C                             = 'GMA'
068700030822     C                   EVAL      VABGMA=%trim(%subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000030822     C                   ENDIF
069100030822     C* GGA
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'GGA'
069600030822     C                   EVAL      VABGGA=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   ENDIF
070000030822     C* GVA
070100030822     C                   IF        %subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):1:3)
070400030822     C                             = 'GVA'
070500030822     C                   EVAL      VABGVA=%trim(%subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800030822     C                   ENDIF
070900030822     C* TC1
071000030822     C                   IF        %subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):1:3)
071300030822     C                             = 'TC1'
071400030822     C                   EVAL      VABTC1=%trim(%subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700030822     C                   ENDIF
071800030822     C* TC2
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'TC2'
072300030822     C                   EVAL      VABTC2=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700030822     C* ...
072800030822     C                   ENDIF
072900030822     C                   ENDDO
073000020204     C*
073100010330     C                   ENDSR
073200010607     C*----------------------------------------------------*
073300021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
073400010607     C*----------------------------------------------------*
073500010607     C     IMPVAB        BEGSR
073600010607     C*
073700010607     C                   SETOFF                                       3132
073800030515     C*
073900010607     C                   MOVEL     datcor        VABAAS
074000040119     C                   MOVEL     datcor        VATAAS
074100010607     C                   MOVE      datcor        VABMGS
074200010607     C*
074300030715     C**********
074400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
074500030715     C**********
074600030715     C                   IF        wGiro = *zeros
074700040706     C*
074800040706     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
074900040706     C                   EVAL      depvinDTA = vinDTA
075000040113     C*
075100040113     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
075200040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
075300040113     C                                      %trim(intcol(3))
075400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
075500031201     C     minu:maiu     XLATE     vindta        vindta
075600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
075700030715     C                   DOW       posDa <= %len(%trim(vindta))
075800030715     C*
075900030715     C* Gestisco il 1° campo
076000030715     C                   IF        i = 1
076100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
076200030715     C                   EVAL      posDa = 1
076300030715     C* Eseguo lo scan x trovare la fine del primo campo
076400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
076500030715     C* A questo punto "estraggo" il campo corrente
076600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
076700030715     C                                                    (posA-posDa))
076800030715     C* X i campi successivi al 1°
076900030715     C                   ELSE
077000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
077100030715     C                   EVAL      posDa = posA + 1
077200030715     C* Eseguo lo scan x trovare la fine del campo corrente
077300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
077400030715     C* Gestisco l'ultimo campo
077500030715     C                   IF        posA = *zeros
077600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
077700030715     C* A questo punto "estraggo" il campo corrente
077800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
077900030715     C                                                    (posA-posDa))
078000030715     C                   LEAVE
078100030715     C                   ELSE
078200030715     C* A questo punto "estraggo" il campo corrente
078300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
078400030715     C                                                    (posA-posDa))
078500030715     C                   ENDIF
078600030715     C                   ENDIF
078700030715     C* Incremento il contatore d campo
078800030715     C                   EVAL      i = i +1
078900030715     C                   ENDDO
079000030715     C*
079100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
079200030715     C* delimitazione testo
079300030715     C                   EVAL      i = 1
079400030715     C                   DOW       i < %elem(SkSplitFLD)
079500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
079600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
079700030715     C                   EVAL      i = i + 1
079800030715     C                   ENDDO
079900030715     C                   EVAL      wGiro = 1
080000040706     C* Ripristino la 1° riga originale
080100040706     C                   EVAL      vinDTA = depvinDTA
080200030715     C                   ELSE
080300030715     C                   EVAL      wGiro = 2
080400031204     C**********
080500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
080600031204     C* d separatore campo
080700031204     C**********
080800031204     C                   EVAL      vindta = %trim(vindta)
080900031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
081000031204     C                   EVAL      vindta = %subst(vindta:2)
081100031204     C                   ENDDO
081200031204     C*
081300031204     C                   Z-ADD     *zeros        lunghInput        4 0
081400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
081500031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
081600031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
081700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
081800031204     C                   ENDDO
081900030715     C**********
082000030715     C* Effettuo lo split del campo dati d input
082100030715     C**********
082200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
082300030715     C                   DOW       posDa <= %len(%trim(vindta))
082400030715     C*
082500030715     C* Gestisco il 1° campo
082600030715     C                   IF        i = 1
082700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082800030715     C                   EVAL      posDa = 1
082900030715     C* Eseguo lo scan x trovare la fine del primo campo
083000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083100030715     C* A questo punto "estraggo" il campo corrente
083200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
083300030715     C                                                    (posA-posDa))
083400030715     C* X i campi successivi al 1°
083500030715     C                   ELSE
083600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083700030715     C                   EVAL      posDa = posA + 1
083800031204     C* Verifico che nn vi sia il campo nn valorizzato
083900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
084000031204     C* Se campo nn valorizzato skippo al prossimo
084100031204     C                   EVAL      SkSplitCSV(i) = *blanks
084200031204     C                   EVAL      posA  = posA + 1
084300031204     C                   ELSE
084400030715     C* Eseguo lo scan x trovare la fine del campo corrente
084500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084600030715     C* Gestisco l'ultimo campo
084700030715     C                   IF        posA = *zeros
084800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
084900030715     C* A questo punto "estraggo" il campo corrente
085000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
085100030715     C                                                    (posA-posDa))
085200030715     C                   LEAVE
085300030715     C                   ELSE
085400030715     C* A questo punto "estraggo" il campo corrente
085500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
085600030715     C                                                    (posA-posDa))
085700030715     C                   ENDIF
085800031204     C                   ENDIF
085900030715     C                   ENDIF
086000030715     C* Incremento il contatore d campo
086100030715     C                   EVAL      i = i +1
086200030715     C                   ENDDO
086300030715     C*
086400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
086500030715     C* delimitazione testo
086600030715     C                   EVAL      i = 1
086700030715     C                   DOW       i < %elem(SkSplitCSV)
086800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
086900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
087000030715     C                   EVAL      i = i + 1
087100030715     C                   ENDDO
087200030715     C*
087300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
087400030715     C                   EVAL      i = 1
087500030715     C                   DOW       i < %elem(SkSplitFLD)
087600030715     C*
087700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
087800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
087900030715     C                             SkSplitCSV(i) <> *blanks
088000030721     C*** ATB
088100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
088200030721     C                   EVAL      VABATB = SkSplitCSV(i)
088300030721     C                   ENDIF
088400030715     C*** RSD
088500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
088600030715     C                   EVAL      VABRSD = SkSplitCSV(i)
088700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
088800030715     C     '@':'A'       XLATE     VABRSD        VABRSD
088900030715     C* ==
089000030715     C                   ENDIF
089100030721     C*** RD2
089200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
089300030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
089400030721     C                   ENDIF
089500030715     C*** IND
089600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
089700030715     C                   EVAL      VABIND = SkSplitCSV(i)
089800030715     C                   ENDIF
089900030715     C*** LOD
090000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
090100030715     C                   EVAL      VABLOD = SkSplitCSV(i)
090200030715     C                   ENDIF
090300030715     C*** PRD
090400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
090500030715     C                   EVAL      VABPRD = SkSplitCSV(i)
090600030715     C                   ENDIF
090700030721     C*** NZD
090800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
090900030721     C                   EVAL      VABNZD = SkSplitCSV(i)
091000030721     C                   ENDIF
091100030715     C*** RMA
091200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
091300030715     C                   EVAL      VABRMA = SkSplitCSV(i)
091400030715     C                   ENDIF
091500030715     C*** NOT
091600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
091700030715     C                   EVAL      VABNOT = SkSplitCSV(i)
091800030715     C                   ENDIF
091900030715     C*** NT2
092000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
092100030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
092200030715     C                   ENDIF
092300030715     C*** VCA
092400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
092500030715     C                   EVAL      VABVCA = SkSplitCSV(i)
092600030715     C                   ENDIF
092700030715     C*** VAS
092800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
092900030715     C                   EVAL      VABVAS = SkSplitCSV(i)
093000030715     C                   ENDIF
093100030715     C*** FFD
093200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
093300030715     C                   EVAL      VABFFD = SkSplitCSV(i)
093400030715     C                   ENDIF
093500030721     C*** GC1
093600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
093700030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
093800030721     C                   ENDIF
093900030721     C*** GC2
094000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
094100030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
094200030721     C                   ENDIF
094300030721     C*** TSP
094400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
094500030721     C                   EVAL      VABTSP = SkSplitCSV(i)
094600030721     C                   ENDIF
094700030721     C*** NAS
094800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
094900030721     C                   EVAL      VABNAS = SkSplitCSV(i)
095000030721     C                   ENDIF
095100030721     C*** TIC
095200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
095300030721     C                   EVAL      VABTIC = SkSplitCSV(i)
095400030721     C                   ENDIF
095500030721     C*** GCA
095600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
095700030721     C                   EVAL      VABGCA = SkSplitCSV(i)
095800030721     C                   ENDIF
095900030721     C*** XCO
096000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
096100030721     C                   EVAL      VABXCO = SkSplitCSV(i)
096200030721     C                   ENDIF
096300030721     C*** CTM
096400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
096500030721     C                   EVAL      VABCTM = SkSplitCSV(i)
096600030721     C                   ENDIF
096700030721     C*** TCR
096800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
096900030721     C                   EVAL      VABTCR = SkSplitCSV(i)
097000030721     C                   ENDIF
097100030721     C*** CTS
097200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
097300030721     C                   EVAL      VABCTS = SkSplitCSV(i)
097400030721     C                   ENDIF
097500030721     C*** FTM
097600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
097700030721     C                   EVAL      VABFTM = SkSplitCSV(i)
097800030721     C                   ENDIF
097900030721     C*** VAD
098000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
098100030721     C                   EVAL      VABVAD = SkSplitCSV(i)
098200030721     C                   ENDIF
098300030721     C*** GMA
098400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
098500030721     C                   EVAL      VABGMA = SkSplitCSV(i)
098600030721     C                   ENDIF
098700030721     C*** GGA
098800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
098900030721     C                   EVAL      VABGGA = SkSplitCSV(i)
099000030721     C                   ENDIF
099100030721     C*** GVA
099200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
099300030721     C                   EVAL      VABGVA = SkSplitCSV(i)
099400030721     C                   ENDIF
099500030721     C*** TC1
099600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
099700030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
099800030721     C                   ENDIF
099900030721     C*** TC2
100000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
100100030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
100200030721     C                   ENDIF
100300030721     C*** SCL
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
100500030721     C                   EVAL      VABSCL = SkSplitCSV(i)
100600030721     C                   ENDIF
100700030721     C*** RMO
100800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
100900030721     C                   EVAL      VABRMO = SkSplitCSV(i)
101000030721     C                   ENDIF
101100030721     C*** NMO
101200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
101300030721     C                   EVAL      VABNMO = SkSplitCSV(i)
101400030721     C                   ENDIF
101500040719     C*** CBO
101600040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
101700040719     C                   IF        %trim(SkSplitCSV(i)) = 'STANDARD'
101800040719     C                   EVAL      VABCBO = '1'
101900040119     C                   ENDIF
102000040719     C                   IF        %trim(SkSplitCSV(i)) = 'CONTRASSEGNO'
102100040719     C                   EVAL      VABCBO = '4'
102200040719     C                   ENDIF
102300040719     C                   ENDIF
102400030721     C*
102500040706     C* Reperisco quindi i campi numerici...
102600040706     C*** NSP
102700040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
102800040706     C                   EVAL      PiStr=SkSplitCSV(i)
102900040706     C                   EXSR      CHKNUM
103000040706     C                   IF        PiInt=*on
103100040706     C                   Z-ADD     PiVal         VABNSP
103200051108     C                   Z-ADD     PiVal         VABRMN
103300040706     C                   ELSE
103400040706     C                   SETON                                        31
103500040706     C                   EVAL      VABNSP = *zeros
103600040706     C                   EVAL      vinmsg = %trimr(vinmsg)
103700051108     C                             + ' ' + 'VABNSP VABRMN'
103800040706     C                   ENDIF
103900040706     C                   ENDIF
104000040706     C*** RMN
104100040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
104200040706     C                   EVAL      PiStr=SkSplitCSV(i)
104300040706     C                   EXSR      CHKNUM
104400040706     C                   IF        PiInt=*on
104500040706     C                   Z-ADD     PiVal         VABRMN
104600040706     C                   ELSE
104700040706     C                   SETON                                        32
104800040706     C                   EVAL      VABRMN = 1
104900040706     C                   EVAL      vinmsg = %trimr(vinmsg)
105000040706     C                             + ' ' + 'VABRMN'
105100040706     C                   ENDIF
105200040706     C                   ENDIF
105300030721     C*** CAD
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
105500030721     C                   EVAL      PiStr=SkSplitCSV(i)
105600030721     C                   EXSR      CHKNUM
105700030721     C                   IF        PiInt=*on
105800030721     C                   Z-ADD     PiVal         Num5_0
105900030721     C                   MOVEL(p)  Num5_0        VABCAD
106000030721     C                   ELSE
106100030721     C                   SETON                                        32
106200030721     C                   EVAL      VABCAD = *zeros
106300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106400030721     C                             + ' ' + 'VABCAD'
106500030721     C                   ENDIF
106600030721     C                   ENDIF
106700030721     C*** NCL
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
106900030721     C                   EVAL      PiStr=SkSplitCSV(i)
107000030721     C                   EXSR      CHKNUM
107100030721     C                   IF        PiInt=*on
107200030721     C                   Z-ADD     PiVal         VABNCL
107300030721     C                   ELSE
107400030721     C                   SETON                                        32
107500030721     C                   EVAL      VABNCL = *zeros
107600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107700040706     C                             + ' ' + 'VABNCL'
107800030721     C                   ENDIF
107900030721     C                   ENDIF
108000030721     C*** PKB
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
108200030721     C                   EVAL      PiStr=SkSplitCSV(i)
108300030721     C                   EXSR      CHKNUM
108400030721     C                   IF        PiNum=*on
108500030721     C                   Z-ADD     PiVal         VABPKB
108600030721     C                   ELSE
108700030721     C                   SETON                                        32
108800030721     C                   EVAL      VABPKB = *zeros
108900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109000030721     C                             + ' ' + 'VABPKB'
109100030721     C                   ENDIF
109200030721     C                   ENDIF
109300030721     C*** CAS
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
109500030721     C                   EVAL      PiStr=SkSplitCSV(i)
109600030721     C                   EXSR      CHKNUM
109700030721     C                   IF        PiNum=*on
109800030721     C                   Z-ADD     PiVal         VABCAS
109900030721     C                   ELSE
110000030721     C                   SETON                                        32
110100030721     C                   EVAL      VABCAS = *zeros
110200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110300030721     C                             + ' ' + 'VABCAS'
110400030721     C                   ENDIF
110500030721     C                   ENDIF
110600030721     C*** CCM
110700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
110800030721     C                   EVAL      PiStr=SkSplitCSV(i)
110900030721     C                   EXSR      CHKNUM
111000030721     C                   IF        PiInt=*on      AND
111100030721     C                             PiVal<=9999999 AND
111200030721     C                             PiVal>*zeros
111300030721     C                   Z-ADD     PiVal         VABCCM
111400040119     C                   Z-ADD     PiVal         VATCCM
111500030721     C                   ELSE
111600030721     C                   SETON                                        32
111700030721     C                   EVAL      VABCCM = *zeros
111800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111900040119     C                             + ' ' + 'VABCCM VATCCM'
112000030721     C                   ENDIF
112100030721     C                   ENDIF
112200030721     C*** LNP
112300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
112400030721     C                   EVAL      PiStr=SkSplitCSV(i)
112500030721     C                   EXSR      CHKNUM
112600030721     C                   IF        PiInt=*on    AND
112700030721     C                             PiVal<=999   AND
112800030721     C                             PiVal>*zeros
112900030721     C                   Z-ADD     PiVal         VABLNP
113000040119     C                   Z-ADD     PiVal         VATLNP
113100030721     C                   ELSE
113200030721     C                   SETON                                        32
113300030721     C                   EVAL      VABLNP = *zeros
113400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113500040119     C                             + ' ' + 'VABLNP VATLNP'
113600030721     C                   ENDIF
113700030721     C                   ENDIF
113800030721     C*** NRS
113900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
114000030721     C                   EVAL      PiStr=SkSplitCSV(i)
114100030721     C                   EXSR      CHKNUM
114200030721     C                   IF        PiInt=*on AND
114300030721     C                             PiVal<=99
114400030721     C                   Z-ADD     PiVal         VABNRS
114500030721     C                   ELSE
114600030721     C                   SETON                                        32
114700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114800030721     C                             + ' ' + 'VABNRS'
114900030721     C                   ENDIF
115000030721     C                   ENDIF
115100030721     C*** LNA
115200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
115300030721     C                   EVAL      PiStr=SkSplitCSV(i)
115400030721     C                   EXSR      CHKNUM
115500030721     C                   IF        PiInt=*on    AND
115600030721     C                             PiVal<=999   AND
115700030721     C                             PiVal>*zeros
115800030721     C                   Z-ADD     PiVal         VABLNA
115900030721     C                   ELSE
116000030721     C                   SETON                                        32
116100030721     C                   EVAL      VABLNA = *zeros
116200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116300030721     C                             + ' ' + 'VABLNA'
116400030721     C                   ENDIF
116500030721     C                   ENDIF
116600030721     C*** CTR
116700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
116800030721     C                   EVAL      PiStr=SkSplitCSV(i)
116900030721     C                   EXSR      CHKNUM
117000030721     C                   IF        PiInt=*on  AND
117100030721     C                             PiVal<=999
117200030721     C                   Z-ADD     PiVal         VABCTR
117300030721     C                   ELSE
117400030721     C                   SETON                                        32
117500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117600030721     C                             + ' ' + 'VABCTR'
117700030721     C                   ENDIF
117800030721     C                   ENDIF
117900030721     C*** IAS
118000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
118100030721     C                   EVAL      PiStr=SkSplitCSV(i)
118200030721     C                   EXSR      CHKNUM
118300030721     C                   IF        PiNum=*on
118400030721     C                   Z-ADD     PiVal         VABIAS
118500030721     C                   ELSE
118600030721     C                   SETON                                        32
118700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118800030721     C                             + ' ' + 'VABIAS'
118900030721     C                   ENDIF
119000030721     C                   ENDIF
119100030721     C*** VLB
119200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
119300030721     C                   EVAL      PiStr=SkSplitCSV(i)
119400030721     C                   EXSR      CHKNUM
119500030721     C                   IF        PiNum=*on
119600030721     C                   Z-ADD     PiVal         VABVLB
119700030721     C                   ELSE
119800030721     C                   SETON                                        32
119900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120000030721     C                             + ' ' + 'VABVLB'
120100030721     C                   ENDIF
120200030721     C                   ENDIF
120300030721     C*** QFT
120400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
120500030721     C                   EVAL      PiStr=SkSplitCSV(i)
120600030721     C                   EXSR      CHKNUM
120700030721     C                   IF        PiNum=*on
120800030721     C                   Z-ADD     PiVal         VABQFT
120900030721     C                   ELSE
121000030721     C                   SETON                                        32
121100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121200030721     C                             + ' ' + 'VABQFT'
121300030721     C                   ENDIF
121400030721     C                   ENDIF
121500030721     C*** NCD
121600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
121700030721     C                   EVAL      PiStr=SkSplitCSV(i)
121800030721     C                   EXSR      CHKNUM
121900030721     C                   IF        PiInt=*on      AND
122000030721     C                             PiVal<=9999999
122100030721     C                   Z-ADD     PiVal         VABNCD
122200030721     C                   ELSE
122300030721     C                   SETON                                        32
122400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122500030721     C                             + ' ' + 'VABNCD'
122600030721     C                   ENDIF
122700030721     C                   ENDIF
122800030721     C*** NCA
122900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
123000030721     C                   EVAL      PiStr=SkSplitCSV(i)
123100030721     C                   EXSR      CHKNUM
123200030721     C                   IF        PiInt=*on      AND
123300030721     C                             PiVal<=9999999
123400030721     C                   Z-ADD     PiVal         VABNCA
123500030721     C                   ELSE
123600030721     C                   SETON                                        32
123700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123800030721     C                             + ' ' + 'VABNCA'
123900030721     C                   ENDIF
124000030721     C                   ENDIF
124100030721     C*** ZNC
124200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
124300030721     C                   EVAL      PiStr=SkSplitCSV(i)
124400030721     C                   EXSR      CHKNUM
124500030721     C                   IF        PiInt=*on AND
124600030721     C                             PiVal<=99
124700030721     C                   Z-ADD     PiVal         VABZNC
124800030721     C                   ELSE
124900030721     C                   SETON                                        32
125000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125100030721     C                             + ' ' + 'VABZNC'
125200030721     C                   ENDIF
125300030721     C                   ENDIF
125400030721     C*** DCR
125500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
125600030721     C                   EVAL      PiStr=SkSplitCSV(i)
125700030721     C                   EXSR      CHKNUM
125800030721     C                   IF        PiInt=*on       AND
125900030721     C                             PiVal<=99999999
126000030721     C                   Z-ADD     PiVal         VABDCR
126100030721     C                   ELSE
126200030721     C                   SETON                                        32
126300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126400030721     C                             + ' ' + 'VABDCR'
126500030721     C                   ENDIF
126600030721     C                   ENDIF
126700030721     C*** HCR
126800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
126900030721     C                   EVAL      PiStr=SkSplitCSV(i)
127000030721     C                   EXSR      CHKNUM
127100030721     C                   IF        PiInt=*on   AND
127200030721     C                             PiVal<=9999
127300030721     C                   Z-ADD     PiVal         VABHCR
127400030721     C                   ELSE
127500030721     C                   SETON                                        32
127600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127700030721     C                             + ' ' + 'VABHCR'
127800030721     C                   ENDIF
127900030721     C                   ENDIF
128000030721     C*** VMD
128100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
128200030721     C                   EVAL      PiStr=SkSplitCSV(i)
128300030721     C                   EXSR      CHKNUM
128400030721     C                   IF        PiNum=*on
128500030721     C                   Z-ADD     PiVal         VABVMD
128600030721     C                   ELSE
128700030721     C                   SETON                                        32
128800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128900030721     C                             + ' ' + 'VABVMD'
129000030721     C                   ENDIF
129100030721     C                   ENDIF
129200030721     C*** ANT
129300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
129400030721     C                   EVAL      PiStr=SkSplitCSV(i)
129500030721     C                   EXSR      CHKNUM
129600030721     C                   IF        PiInt=*on        AND
129700030721     C                             PiVal<=999999999
129800030721     C                   Z-ADD     PiVal         VABANT
129900030721     C                   ELSE
130000030721     C                   SETON                                        32
130100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130200030721     C                             + ' ' + 'VABANT'
130300030721     C                   ENDIF
130400030721     C                   ENDIF
130500030721     C*** CMO
130600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
130700030721     C                   EVAL      PiStr=SkSplitCSV(i)
130800030721     C                   EXSR      CHKNUM
130900030721     C                   IF        PiInt=*on
131000030721     C                   Z-ADD     PiVal         Num5_0
131100030721     C                   MOVEL(p)  Num5_0        VABCMO
131200030721     C                   ELSE
131300030721     C                   SETON                                        32
131400030721     C                   EVAL      VABCMO = *zeros
131500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131600030721     C                             + ' ' + 'VABCMO'
131700030721     C                   ENDIF
131800030721     C                   ENDIF
131900040706     C*
132000051108     C*** VATNOT TRC=A
132100051108     C                   IF        %trim(SkSplitFLD(i)) = 'TRC_A'
132200051108     C                   EVAL      wVATNOT_A = SkSplitCSV(i)
132300040719     C                   ENDIF
132400051108     C*** VATNOT TRC=B
132500051108     C                   IF        %trim(SkSplitFLD(i)) = 'TRC_B'
132600051108     C                   EVAL      wVATNOT_B = SkSplitCSV(i)
132700051108     C                   ENDIF
132800030715     C*
132900030715     C                   ENDIF
133000030715     C                   EVAL      i = i + 1
133100030715     C                   ENDDO
133200040706     C*
133300040706     C* Se nn valorizzata la provincia destinatario la calcolo tramite TISI95
133400040706     C* Reperisco la provincia dal CAP e dalla località
133500040706     C                   IF        VABCAD <> *blanks
133600040706     C                   CLEAR                   TISI95DS
133700040706     C                   EVAL      I95TCN = '3'
133800040706     C                   Z-ADD     datcor        I95DAT
133900040706     C                   EVAL      I95CAP = VABCAD
134000040706     C                   EVAL      I95LOC = VABLOD
134100040706     C                   CALL      'TISI95R'
134200040706     C                   PARM                    TISI95DS
134300040706     C                   EVAL      VABPRD = O95PRV
134400040706     C                   ENDIF
134500040719     C*
134600040719     C* Se NSP nn valorizzato considero errore bloccante in traduzione
134700040719     C                   IF        VABNSP = *zeros
134800040719     C                   SETON                                        31
134900040719     C                   ENDIF
135000040719     C*
135100040719     C* Se LNP nn valorizzato considero errore bloccante in traduzione
135200040719     C                   IF        VABLNP = *zeros
135300040719     C                   SETON                                        31
135400040719     C                   ENDIF
135500040706     C*
135600040706     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
135700040706     C* d valorizzarli sempre entrambi
135800040706     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
135900040706     C                   Z-ADD     VABNSP        VABRMN
136000040706     C                   ENDIF
136100040706     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
136200040706     C                   MOVEL     VABRMN        VABRMA
136300040706     C                   ENDIF
136400040113     C*
136500040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
136600040113     C                   IF        VABCAS > *zeros
136700040119     C                   EVAL      VABVCA = 'EUR'
136800040119     C                   IF        VABCBO = '1'
136900040113     C                   EVAL      VABCBO = '4'
137000040119     C                   ENDIF
137100040119     C                   IF        VABCBO = '2'
137200040119     C                   EVAL      VABCBO = '6'
137300040119     C                   ENDIF
137400040113     C                   ENDIF
137500020204     C*
137600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
137700020204     C                   EXSR      CHKIMPDIV
137800030715     C                   ENDIF
137900020204     C*
138000010607     C                   ENDSR
138100010607     C*----------------------------------------------------*
138200010601
138300020204
138400020204     C*----------------------------------------------------*
138500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
138600020204     C*----------------------------------------------------*
138700020204     C     CHKIMPDIV     BEGSR
138800020204     C*
138900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
139000020204     C                   Z-ADD     *zeros        wrkDec            9 9
139100020204     C*
139200020204     C* Come prima cosa effettuo considerazioni sulla divisa
139300020204     C                   IF        vabIAS > *zeros
139400020204     C                   IF        vabVAS <> 'EUR'
139500020204     C                   EVAL      vabVAS =  'ITL'
139600020204     C                   ENDIF
139700020204     C                   ENDIF
139800020204     C*
139900020204     C                   IF        vabCAS > *zeros
140000020204     C                   IF        vabVCA <> 'EUR'
140100020204     C                   EVAL      vabVCA =  'ITL'
140200020204     C                   ENDIF
140300020204     C                   ENDIF
140400020204     C*
140500020204     C                   IF        vabVMD > *zeros
140600020204     C                   IF        vabVAD <> 'EUR'
140700020204     C                   EVAL      vabVAD =  'ITL'
140800020204     C                   ENDIF
140900020204     C                   ENDIF
141000020204     C*
141100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
141200020204     C                   Z-ADD     vabIAS        wrkDec
141300020204     C                   IF        wrkDec > *zeros
141400020204     C                   IF        vabVAS = 'ITL'
141500020204     C                   EVAL      vabIAS = *zeros
141600020204     C                   ENDIF
141700020204     C                   ENDIF
141800020204     C*
141900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
142000020204     C                   Z-ADD     vabCAS        wrkDec
142100020204     C                   IF        wrkDec > *zeros
142200020204     C                   IF        vabVCA = 'ITL'
142300020204     C                   EVAL      vabCAS = *zeros
142400020204     C                   ENDIF
142500020204     C                   ENDIF
142600020204     C*
142700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
142800020204     C                   Z-ADD     vabVMD        wrkDec
142900020204     C                   IF        wrkDec > *zeros
143000020204     C                   IF        vabVAD = 'ITL'
143100020204     C                   EVAL      vabVMD = *zeros
143200020204     C                   ENDIF
143300020204     C                   ENDIF
143400020204     C*
143500020204     C                   ENDSR
143600020204     C***
143700020204
143800010330
143900010330
144000010330     C*----------------------------------------------------*
144100010330     C*  CONTROLLO NUMERICITA' CAMPI
144200010330     C*----------------------------------------------------*
144300010330     C     CHKNUM        BEGSR
144400010330     C*
144500010606     C                   IF        PiDecChr = *blanks
144600030715     C                   EVAL      PiDecChr = CharNUM
144700010606     C                   ENDIF
144800010606     C*
144900010606     C                   CALL(e)   'ISNUMERIC'
145000010330     C                   PARM                    PiStr            30
145100010606     C                   PARM                    PiDecChr          1
145200010330     C                   PARM      *ZEROS        PiVal            30 9
145300010330     C                   PARM      '0'           PiInt             1
145400010330     C                   PARM      '0'           PiNum             1
145500010330     C                   IF        %error
145600010606     C                   EVAL      PiNum=*off
145700010330     C                   ENDIF
145800010330     C*
145900010330     C                   ENDSR
146000010330     C***
146100010330
146200010601
146300010601
146400010601
146500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
146600010601     C     repfil        BEGSR
146700010601     C*
146800010601     C                   if        invfil = *zeros and
146900010601     C                             depfil > *zeros and
147000010629     C                             (vinflg = *blanks or
147100010629     C                              vinflg = *zeros)
147200010601     C
147300010601     C                   eval      invfil = depfil
147400010601     C                   endif
147500010601     C*
147600010601     C                   if        depfil <> invfil and
147700010601     C                             invfil > *zeros
147800010601     C                   eval      flgMulti = '1'
147900010601     C                   if        vinflg = *blanks
148000010601     C                   add       1             cntNonEl
148100010601     C                   endif
148200010601     C                   endif
148300010601     C*
148400010601     C                   if        vinflg = '2'
148500010601     C                   eval      flgStato = '2'
148600010601     C                   endif
148700010601     C*
148800010601     C                   ENDSR
148900010601     C***
149000010601
149100010601
149200010601
149300010330
149400040119      /TITLE Invio dei dati al punto operativo.
149500040119     C     invio         BEGSR
149600040119     C*
149700040119     C* 1° invio FIVAT
149800040119     C                   reset                   dscmz
149900040119     C                   move      vlrpoi        cmzdst
150000040119     C                   eval      cmzfld = 'FIVATWWR'
150100040119     C                   eval      cmzmbd = vlrhdl
150200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
150300040119     C***                if        prmfir = *blanks
150400040119     C                   eval      cmzfla = 'FIVAT00F'
150500040119     C                   eval      cmzmba = 'FIVAT00F'
150600040119     C***                else
150700040119     C***                eval      cmzfla = prmfir
150800040119     C***                eval      cmzmba = prmfir
150900040119     C***                endif
151000040119     C                   eval      cmznrr = *zeros
151100040119     C                   move      §ctrokvt      cmznrr
151200040119     C                   eval      cmzlba = vlrfl1
151300040119     C                   call(e)   'TIS711C'
151400040119     C                   parm                    dscmz
151500040119     C                   parm      *blanks       esito
151600040119     C                   if        %error
151700040119     C                             or cmzerr = '1'
151800040119     C                             or esito  = '1'
151900040119     C                   eval      wrkesito = '3'
152000040119     C                   else
152100040119     C*
152200040119     C* 2° invio FIVAB
152300040119     C                   reset                   dscmz
152400040119     C                   move      vlrpoi        cmzdst
152500040119     C                   eval      cmzfld = vlrfou
152600040119     C                   eval      cmzmbd = vlrhdl
152700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
152800040119     C***                if        prmfir = *blanks
152900040119     C                   eval      cmzfla = 'FIVAB00F'
153000040119     C                   eval      cmzmba = 'FIVAB00F'
153100040119     C***                else
153200040119     C***                eval      cmzfla = prmfir
153300040119     C***                eval      cmzmba = prmfir
153400040119     C***                endif
153500040119     C                   eval      cmznrr = *zeros
153600040119     C                   move      §ctrokvb      cmznrr
153700040119     C                   eval      cmzlba = vlrfl1
153800040119     C                   call(e)   'TIS711C'
153900040119     C                   parm                    dscmz
154000040119     C                   parm      *blanks       esito
154100040119     C                   if        %error
154200040119     C                             or cmzerr = '1'
154300040119     C                             or esito  = '1'
154400040119     C                   eval      wrkesito = '3'
154500040119     C                   endif
154600040119     C                   endif
154700040119     C*
154800040119     C                   ENDSR
154900040119     C***
155000010601
155100010601
155200010601
155300010601
155400010601      /TITLE Invio dei dati al punto operativo.
155500010601     C     opeini        BEGSR
155600010601     C*
155700010601     C* Inizializzo flag e contatori operativi
155800010601     C                   movel     '0'           flgGiro           1
155900010601     C                   movel     '0'           flgMulti          1
156000010601     C                   movel     '1'           flgStato          1
156100010615     C                   movel     '0'           flgOk             1
156200010601     C                   z-add     *zeros        cntNonEl         10 0
156300010601     C                   z-add     *zeros        depfil            3 0
156400010601     C                   z-add     *zeros        invfil            3 0
156500010601     C*
156600010601     C                   ENDSR
156700010601     C***
156800010601
156900010601
157000050421
157100050421
157200070411
157300070411     C     *pssr         BEGSR
157400070411     C*
157500070411     C                   if        %open(tivin00r)
157600070411     C                   close     tivin00r
157700070411     C                   endif
157800070411     C                   if        %open(fivabwwr)
157900070411     C                   close     fivabwwr
158000070411     C                   endif
158100070411     C                   if        %open(fivatwwr)
158200070411     C                   close     fivatwwr
158300070411     C                   endif
158400070411     C*
158500070411     C* Effettuo la chiamata al CLLE preposto
158600070411     C                   call(e)   'TITVVTC'
158700070411     C                   parm                    parccm
158800070411     C                   parm                    parmbr
158900070411     C                   parm      '2'           paropz
159000070411     C*
159100070411     C                   eval      wrkesito = '2'
159200070411     C*
159300070411     C                   seton                                        LR
159400070411     C*
159500070411     C                   ENDSR     '*CANCL'
159600070411     C***
159700070411
159800050421
159900010601
160000010330
160100010330
160200000613     C     *inzsr        BEGSR
160300990910     C*
160400990910     C     *entry        plist
160500990920     C                   parm                    tivlrds
160600990921     C                   parm      wrkesito      esito
160700000724     C                   parm                    prmlit
160800000710     C                   parm                    prmfir
160900010330     C*
161000010330     C* CALCOLA LA DATA CORRENTE
161100010330     C                   time                    wn14             14 0
161200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
161300010330     C                   z-add     wn8           g08dat
161400010330     C                   z-add     *zeros        g08inv
161500010330     C                   movel     '0'           g08err
161600010330     C                   call      'XSRDA8'
161700010330     C                   parm                    wlbda8
161800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
161900000613     C*
162000000613     C                   ENDSR
162100000613     C***
162200990908
162300051108     Otitvo9p   E            riepilogo         2
162400990915     O                                              'Upload via Internet'
162500990915     O                                           +1 'Traduzione TIVIN00R -'
162600021025     O                                           +1 'FIVABWWR'
162700990915     O                                           +1 'Testate bolle'
162800990915     O          E            riepilogo   2
162900990915     O                       wrkdata
163000990915     O                       wrkora              +1
163100990915     O                       procname            +1
163200990915     O          E            riepilogo   2
163300990915     O                                              'Cliente..................:'
163400010605     O                       VABCCM        z     +1
163500990915     O          E            riepilogo   2
163600990920     O                                              'Riferimento Strategi.....:'
163700990920     O                       vlrhdl              +1
163800990915     O          E            riepilogo   2
163900990915     O                                              'Giusti...................:'
164000040119     O                       §CTROKVB      2   +  1
164100990915     O          E            riepilogo   2
164200990915     O                                              'Sbagliati e corretti.....:'
164300971022     O                       §CTRMO        2   +  1
164400990915     O          E            riepilogo   2
164500990915     O                                              'Sbagliati e scartati.....:'
164600971022     O                       §CTRNO        2   +  1
164700000613
164800051108     Otitvo9ps  E            testdett          2
164900000613     O                                              'Upload via Internet'
165000000613     O                                           +1 'Traduzione TIVIN00R -'
165100021025     O                                           +1 'FIVABWWR'
165200000613     O                                           +1 'Testate bolle'
165300000616     O          E            testdett    3
165400000613     O                                           +2 'N° rec'
165500000613     O                                           +3 'Anteprima contenuto'
165600000616     O          E            rigadett    2
165700000613     O                       rrnum               +2
165800000621     O                       recko               +3
165900000616     O          E            findett     2
166000000613     O                       wrkdata
166100000613     O                       wrkora              +1
166200000613     O                       procname            +1
166300000616     O          E            findett     2
166400000613     O                                              'Cliente..................:'
166500010605     O                       VABCCM        z     +1
166600000616     O          E            findett     2
166700000613     O                                              'Riferimento Strategi.....:'
166800000613     O                       vlrhdl              +1
166900000616     O          E            findett     2
167000000613     O                                              'Giusti...................:'
167100040119     O                       §CTROKVB      2   +  1
167200000616     O          E            findett     2
167300000613     O                                              'Sbagliati e corretti.....:'
167400000613     O                       §CTRMO        2   +  1
167500000616     O          E            findett     2
167600000613     O                                              'Sbagliati e scartati.....:'
167700000613     O                       §CTRNO        2   +  1
167800000613** CMD - COMANDI CL
167900051108OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVO9P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
168000051108OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVO9PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
168100051108DLTOVR FILE(TITVO9P TITVO9PS) LVL(*)
168200010606
168300000613
168400040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
168500051108VABNSP;VABRSD;VABIND;VABLOD;VABPRD;VABCAD;TRC_A;
168600051108TRC_B;VABRMA;VABCAS;VABPKB;VABNCL
168700040113
