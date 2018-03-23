000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800060112     Ftitvq5p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000060112     Ftitvq5ps  O    f  198        PRINTER usropn
001100000621     F                                     oflind(*inob)
001200000313     D*
001300000613     D*------------
001400000613     D* COMANDI
001500000613     D*------------
001600011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700040113     D*------------
001800040113     D* SCHIEDE A PROGRAMMA
001900040113     D*------------
002000040113     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
002100010330     D*----------------------------------------------------
002200010330     D* DICHIARAZIOINE VARIABILI DI WRK
002300010330     D*----------------------------------------------------
002400010330     D dscmz         e ds                  inz
002500010330     D psds           sds
002600010330     D  procname         *PROC
002700010330     D tivlrds       e ds                  extname(tivlr00f)
002800040706     D tisi95ds      e ds
002900010330     D esito           s              1
003000010330     D prmlit          s             10
003100010330     D prmfir          s             10
003200010330     D wrkesito        s                   like(esito)
003300010330     D wrkdata         s               d
003400010330     D wrkora          s               t
003500010330     D rrnum           s              6  0 INZ(*zeros)
003600010330     D recko           s            150    INZ(*blanks)
003700011113     D depcmd          s            150    INZ(*blanks)
003800060112     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
003900040119     D parccm          s              8    INZ(*blanks)
004000040119     D parmbr          s             10    INZ(*blanks)
004100040119     D paropz          s              1    INZ(*blanks)
004200040119     D chkcall         s              1    INZ(*blanks)
004300040719     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
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
005600060112     D wVATNOT_A       s                   LIKE(vatnot) inz(*blanks)
005700060112     D wVATNOT_B       s                   LIKE(vatnot) inz(*blanks)
005800060112     D wVABRMN_Alfa    s              8    inz(*blanks)
005900060112     D wCELL           s             10    inz(*blanks)
006000060112     D wCONTRASS       s             50    inz(*blanks)
006100030822     D*------------------
006200030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006300030822     D*------------------
006400030822     D posDaDft        S              3  0 INZ(*zeros)
006500030822     D posADft         S              3  0 INZ(*zeros)
006600030822     D j               s              3  0 INZ(1)
006700030822     D wGiroDft        s              1  0 INZ(*zeros)
006800010330     D*------------------
006900010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
007000010330     D*------------------
007100010330     D WLBDA8          DS                  INZ
007200010330     D  G08DAT                 1      8  0
007300010330     D  G08INV                 9     16  0
007400010330     D  G08ERR                17     17
007500010330     D  G08TGI                18     22  0
007600031201     D*------------------
007700031201     D* Costanti
007800031201     D*------------------
007900031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008000031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008100060112     D*------------------
008200060112     D* DS REPERIMENTO NUMERATORE
008300060112     D*------------------
008400060112     D trul33ds      e ds                  inz
008500060112     D*------------------
008600060112     D* DS ARCHITETTURA
008700060112     D*------------------
008800060112     D kpjba         e ds                  inz
008900060112     D*------------------
009000060112     D* VARIABILI X "BONIFICA" STRINGHE
009100060112     D*------------------
009200060112     D TxtInOut        S           2048
009300060112     D ElencoChar      S            256
009400060112     D TipoElenco      S              1
009500060112     D CharSost        S              1
009600060112     D UpperCase       S              1
009700060112     D ChkNull         S              1
009800060112     D CharNull        S              1
009900060112     D EsitoBon        S              1
010000060112     D*------------------
010100010330
010200010330
010300010330
010400990915     C                   time                    wrkdata
010500990915     C                   time                    wrkora
010600000913     C                   reset                   rrnum
010700990921     C                   reset                   esito
010800990921     C                   reset                   wrkesito
010900010601     C*
011000010601     C                   exsr      opeini
011100010605     C                   exsr      rwvab
011200010601     C*
011300010601     C                   seton                                        lr
011400010601
011500010601
011600010601
011700010601     C*--------------------------------------------------------
011800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011900010601     C*--------------------------------------------------------
012000010601     C     PREELA        BEGSR
012100010601     C*
012200010601     C* SE OCCORRE SPEDIRE IN FILIALE
012300010601     C                   if        invfil <> *zeros and
012400010601     C                             flgGiro = '0'
012500010601     C*
012600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012700010601     C                   eval      flgGiro = '1'
012800000724     C*
012900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
013000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013100010601     C     invfil        chain     azorg01l
013200000724     C                   if        %found
013300000724     C                   movel(p)  CMD(1)        depcmd
013400020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
013500000724     C*
013600000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
013700011113     C                   Z-ADD     150           LENGH            15 5
013800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013900000724     C                   PARM                    depcmd
014000000724     C                   PARM                    LENGH
014100000724     C*
014200000724     C                   endif
014300000616     C*
014400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014500000616     C                   movel(p)  CMD(2)        depcmd
014600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
014700000616     C*
014800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
014900011113     C                   Z-ADD     150           LENGH            15 5
015000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015100000616     C                   PARM                    depcmd
015200000616     C                   PARM                    LENGH
015300000613     C*
015400060112     C                   if        not %open(titvq5ps)
015500060112     C                   open      titvq5ps
015600030715     C                   except    testdett
015700000613     C                   endif
015800010601     C*
015900010601     C                   endif
016000010601     C*
016100010601     C                   ENDSR
016200010601     C***
016300010601
016400010601
016500010601
016600010601
016700010601     C*--------------------------------------------------------
016800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016900010601     C*--------------------------------------------------------
017000010601     C     ENDELA        BEGSR
017100990915     C*
017200000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
017300040119     C*
017400040119     C* Effettuo la chiamata al CLLE preposto
017500040119     C                   call(e)   'TITVVTC'
017600040119     C                   parm                    parccm
017700040119     C                   parm                    parmbr
017800040119     C                   parm      '2'           paropz
017900050201     C*
018000050201     C* Effettuo lancio TISI95 solo x chiusura
018100050201     C                   CLEAR                   TISI95DS
018200050201     C                   EVAL      I95TLA = 'C'
018300050201     C                   CALL      'TISI95R'
018400050201     C                   PARM                    TISI95DS
018500000613     C*
018600060112     C                   if        %open(titvq5ps)
018700000616     C                   except    findett
018800060112     C                   close     titvq5ps
018900000613     C                   endif
019000000616     C*
019100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
019200011113     C                   Z-ADD     150           LENGH            15 5
019300010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
019400000616     C                   PARM                    CMD(3)
019500000616     C                   PARM                    LENGH
019600000616     C*
019700010601     C                   ENDSR
019800010601     C***
019900010601
020000010601
020100010601
020200000613
020300000613     C*--------------------------------------------------------
020400000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
020500000613     C*--------------------------------------------------------
020600000621     C     STPR          BEGSR
020700000613     C*
020800060112     C                   if        not %open(titvq5p)
020900060112     C   50              open      titvq5p
021000990915     C                   endif
021100990915     C*
021200030715     C   50              except    riepilogo
021300990915     C*
021400060112     C                   if        %open(titvq5p)
021500060112     C   50              close     titvq5p
021600990914     C                   endif
021700000613     C*
021800000613     C                   ENDSR
021900000613     C***
022000000613
022100010601
022200010601
022300010330     C*--------------------------------------------------------
022400021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
022500010330     C*--------------------------------------------------------
022600010605     C     RWVAB         BEGSR
022700010330     C*
022800010330     C                   if        not %open(tivin00r)
022900010330     C                   open      tivin00r
023000010330     C                   endif
023100021025     C                   if        not %open(fivabwwr)
023200021025     C                   open      fivabwwr
023300010330     C                   endif
023400040119     C*
023500040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
023600040119     C                   exsr      prevat
023700040119     C*
023800040119     C                   if        chkcall = '0'
023900040119     C*
024000040119     C                   if        not %open(fivatwwr)
024100040119     C                   open      fivatwwr
024200040119     C                   endif
024300010330     C*
024400040119     C                   clear                   §CTROKVB          7 0
024500040119     C                   clear                   §CTROKVT          7 0
024600040119     C                   clear                   §CTRMO            7 0
024700040119     C                   clear                   §CTRNO            7 0
024800040719     C*
024900040719     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
025000040719     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
025100060112     C***                exsr      inzvar
025200060112     C***                exsr      impvab
025300040706     C*
025400040706     C  N70              do        *HIVAL
025500010330     C*
025600040706     C                   read      tivin00r                               70
025700010618     C*
025800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
025900010618     C                   if        vinflg = '1'
026000010618     C                   eval      flgOk = '1'
026100010618     C                   endif
026200010618     C*
026300040316     C                   if        (vindta > *blanks AND
026400040316     C                              %trim(vindta) <> *all';') OR
026500040316     C                             wGiro = *zeros
026600010330     C                   add       1             rrnum
026700010330     C*
026800010601     C                   if        *in70 = *off and
026900010330     C                             (vinflg = *blanks
027000010330     C                              or vinflg = '0'
027100010330     C                              or vinflg = '2')
027200010330     C*
027300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
027400010711     C                   if        vinflg = *blanks or vinflg = '0'
027500010711     C                   clear                   vinmsg
027600010711     C                   endif
027700010601     C*
027800010330     C                   exsr      inzvar
027900010605     C                   exsr      impvab
028000010601     C*
028100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
028200010605     C                   eval      depfil = VABLNP
028300010601     C                   exsr      repfil
028400010601     C                   if        depfil = invfil
028500021025     C                   if        vlrpoi = 999
028600030822     C                   move(P)   invfil        VABFGS
028700040119     C                   move(P)   invfil        VATFGS
028800021025     C                   else
028900030822     C                   move(P)   vlrpoi        VABFGS
029000040119     C                   move(P)   vlrpoi        VATFGS
029100021025     C                   endif
029200010601     C*
029300010601     C                   exsr      PREELA
029400010601     C*
029500010604     C* Ebbene...
029600010604     C*
029700040119     C  N31              add       1             §CTROKVB
029800040119     C   32              add       1             §CTRMO
029900040119     C   31              add       1             §CTRNO
030000030822     C                   if        wGiro = 2
030100060112     C                   if        depspe =  *zeros OR
030200060112     C                             depspe <> vabNSP
030300040119     C  N31              exsr      wrivab
030400060112     C  N31              eval      depspe = vabNSP
030500060112     C                   endif
030600040119     C                   endif
030700010604     C*
030800010604     C                   if        *in31 = *off and
030900010604     C                             *in32 = *off
031000010604     C                   eval      vinflg = '1'
031100010604     C                   else
031200010604     C                   eval      recko = vindta
031300010604     C                   except    rigadett
031400010604     C                   eval      vinflg = '2'
031500010604     C                   endif
031600010604     C                   endif
031700010601     C*
031800010604     C                   endif
031900010604     C*
032000010330     C                   else
032100010330     C                   eval      vinflg = '1'
032200010330     C                   endif
032300010601     C*
032400010601     C  N70              update    tivin000
032500010330     C*
032600030822     C  N70              enddo
032700040119     C*
032800040119     C                   endif                                                  (endif - chkcall)
032900010601     C*
033000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
033100010601     C                   if        cntNonEl = *zeros or
033200010601     C                             flgMulti = '0'
033300010330     C* Se non ci sono record con errori ...
033400010601     C                   if        §ctrno = 0 and
033500010604     C                             §ctrmo = 0 and
033600010601     C                             flgStato <> '2'
033700010330     C* ... restituisco esito OK.
033800010330     C                   eval      wrkesito = '0'
033900010330     C                   else
034000040119     C                   if        §ctrokvb > 0
034100010330     C                   eval      wrkesito = '1'
034200010330     C                   else
034300010615     C                   if        flgOk = '0'
034400010615     C                   eval      wrkesito = '2'
034500010615     C                   else
034600010615     C                   eval      wrkesito = '6'
034700010615     C                   endif
034800010330     C                   endif
034900010330     C                   endif
035000010601     C                   else
035100010601     C                   eval      wrkesito = '9'
035200010601     C                   endif
035300010330     C*
035400010330     C                   if        %open(tivin00r)
035500010330     C                   close     tivin00r
035600010330     C                   endif
035700021025     C                   if        %open(fivabwwr)
035800021025     C                   close     fivabwwr
035900010330     C                   endif
036000040512     C                   if        %open(fivatwwr)
036100040512     C                   close     fivatwwr
036200040512     C                   endif
036300010601     C*
036400010601     C                   if        vlrpoi <> 999
036500010601     C                   eval      invfil = vlrpoi
036600010601     C                   endif
036700010330     C*
036800040119     C                   if        §ctrokvb > 0
036900010601     C                             and invfil > *zeros
037000010330     C                   exsr      invio
037100010330     C                   endif
037200010601     C*
037300010618     C                   if        flgGiro = '1'
037400010601     C                   exsr      endela
037500010618     C                   endif
037600010330     C*
037700010330     C                   ENDSR
037800010330     C***
037900040119
038000040119
038100040119
038200040119     C*----------------------------------------------------*
038300040119     C*  SCARICAMENTO BUFFER RECORDS VAB
038400040119     C*----------------------------------------------------*
038500040119     C     WRIVAB        BEGSR
038600040119     C*
038700040119     C                   write     fivab000                                     => scarico il VAB
038800040119     C*
038900040119     C* Eseguo la scrittura dei "CHI SONO" della spedizione corrente
039000040119     C                   exsr      wrivat
039100040119     C*
039200040119     C                   ENDSR
039300040119
039400040119
039500040119
039600040119     C*----------------------------------------------------*
039700040119     C*  SCARICAMENTO BUFFER RECORDS VAT
039800040119     C*----------------------------------------------------*
039900040706     C     WRIVAT        BEGSR
040000040706     C*
040100040719     C* Attribuisco la stessa chiave della testata bolle
040200040719     C                   EVAL      VATAAS = VABAAS
040300040719     C                   EVAL      VATLNP = VABLNP
040400040719     C                   EVAL      VATNSP = VABNSP
040500040719     C*
040600060112     C* Imposto il tipo record sul file VAT a 'B' x indicare il telefono destinatario
040700051108     C                   EVAL      VATTRC = 'B'
040800051108     C                   EVAL      VATNOT = %trim(wVATNOT_B)
040900040719     C*
041000040719     C* Quindi se valorizzato il VATNOT scarico il buffer e incremento il contatore x il CMZ
041100040719     C                   IF        VATNOT <> *blanks
041200040706     C                   WRITE     FIVAT000
041300040706     C                   ADD       1             §CTROKVT
041400040719     C                   ENDIF
041500051108     C*
041600051108     C* Imposto il tipo record sul file VAT a 'A' x indicare il referente
041700051108     C                   EVAL      VATTRC = 'A'
041800051108     C                   EVAL      VATNOT = %trim(wVATNOT_A)
041900051108     C*
042000051108     C* Quindi se valorizzato il VATNOT scarico il buffer e incremento il contatore x il CMZ
042100051108     C                   IF        VATNOT <> *blanks
042200051108     C                   WRITE     FIVAT000
042300051108     C                   ADD       1             §CTROKVT
042400051108     C                   ENDIF
042500040119     C*
042600040119     C                   ENDSR
042700040119
042800040119
042900040119
043000040119     C*----------------------------------------------------*
043100040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043200040119     C*----------------------------------------------------*
043300040119     C     PREVAT        BEGSR
043400040119     C*
043500040119     C* Compongo il nome del membro da dare al FIVATWWR
043600040119     C                   eval      parmbr = vlrhdl
043700040119     C                   movel     'M'           parmbr
043800060112     C                   eval      parccm = vlrksc
043900040119     C                   eval      paropz = '1'
044000040119     C* Effettuo la chiamata al CLLE preposto
044100040119     C                   call(e)   'TITVVTC'
044200040119     C                   parm                    parccm
044300040119     C                   parm                    parmbr
044400040119     C                   parm                    paropz
044500040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
044600040119     C                   if        %error
044700040119     C                   movel     '1'           chkcall
044800040119     C                   else
044900040119     C                   movel     '0'           chkcall
045000040119     C                   endif
045100040119     C*
045200040119     C                   ENDSR
045300010601
045400010601
045500010601
045600010330     C*----------------------------------------------------*
045700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
045800010330     C*----------------------------------------------------*
045900040119     C     INZVAR        BEGSR
046000010330     C*
046100030822     C* Inizializzo variabili di wrk
046200020204     C                   Z-ADD     *zeros        Num5_0            5 0
046300060112     C                   CLEAR                   wVATNOT_A
046400060112     C                   CLEAR                   wVATNOT_B
046500060112     C                   CLEAR                   wVABRMN_Alfa
046600060112     C                   CLEAR                   wCELL
046700060112     C                   CLEAR                   wCONTRASS
046800060112     C*
046900060112     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
047000060112     C                   MOVEL     '0'           FlgCAS            1
047100030822     C*
047200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
047300030822     C                   CLEAR                   FIVAB000
047400040119     C                   CLEAR                   FIVAT000
047500030822     C                   CLEAR                   SkSplitCSV
047600030822     C                   EVAL      i = 1
047700030822     C                   EVAL      posDa = *zeros
047800030822     C                   EVAL      posA  = *zeros
047900030822     C*
048000030822     C* Reimposto i valori di default
048100030822     C                   EXSR      DEFCAM
048200030822     C*
048300010330     C                   ENDSR
048400010330     C*----------------------------------------------------*
048500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
048600010330     C*----------------------------------------------------*
048700010330     C     DEFCAM        BEGSR
048800040119     C*
048900040119     C* Imposto i valori d default a programma dei campi fondamentali
049000060112     C                   MOVE(P)   vlrksc        VABCCM
049100060112     C                   MOVE(P)   vlrksc        VATCCM
049200060112     C                   EVAL      VABLNP = 120
049300060112     C                   EVAL      VATLNP = 120
049400060112     C                   EVAL      VABCTR = 333
049500040119     C                   EVAL      VABCBO = '1'
049600060112     C                   EVAL      VABNCL = 1
049700060112     C                   EVAL      VABPKB = 0,1
049800030822     C*
049900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050000030715     C* e delimitatore testo.
050100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050400030822     C*
050500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
050600030715     C                   IF        %subst(vlrppt:1:1) = 'S'
050700030715     C                   SETON                                        50
050800030715     C                   ELSE
050900030715     C                   SETOFF                                       50
051000030715     C                   ENDIF
051100030822     C*
051200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
051300030822     C                   EVAL      posDaDft = 1
051400030822     C                   EVAL      posADft  = 0
051500030822     C                   EVAL      wGiroDft = 0
051600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
051700030822     C                             posDaDft > 0
051800030822     C*
051900030822     C* Gestisco il 1° giro
052000030822     C                   IF        wGiroDft = 0
052100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
052200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
052300030822     C* Incremento il contatore dei "giri"
052400030822     C                   EVAL      wGiroDft = 1
052500030822     C                   ELSE
052600030822     C                   EVAL      posDaDft = posADft
052700030822     C                   ENDIF
052800030822     C* Eseguo lo scan x trovare la fine del campo corrente
052900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
053000030822     C*
053100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
053200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
053300030822     C                   IF        posDaDft > 0 AND
053400030822     C                             posADft  > 0
053500030822     C* NCL
053600030822     C                   IF        %subst(
053700030822     C                             %subst(vlrppt:posDaDft+1:
053800030822     C                             posADft-posDaDft-1):1:3)
053900030822     C                             = 'NCL'
054000030822     C                   EVAL      PiStr=%trim(%subst(
054100030822     C                             %subst(vlrppt:posDaDft+1:
054200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054300030822     C                   EXSR      CHKNUM
054400030822     C                   IF        PiInt=*on
054500030822     C                   Z-ADD     PiVal         VABNCL
054600030822     C                   ENDIF
054700030822     C                   ENDIF
054800030822     C* CCM
054900030822     C                   IF        %subst(
055000030822     C                             %subst(vlrppt:posDaDft+1:
055100030822     C                             posADft-posDaDft-1):1:3)
055200030822     C                             = 'CCM'
055300030822     C                   EVAL      PiStr=%trim(%subst(
055400030822     C                             %subst(vlrppt:posDaDft+1:
055500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055600030822     C                   EXSR      CHKNUM
055700030822     C                   IF        PiInt=*on
055800030822     C                   Z-ADD     PiVal         VABCCM
055900040119     C                   Z-ADD     PiVal         VATCCM
056000030822     C                   ENDIF
056100030822     C                   ENDIF
056200030822     C* LNP
056300030822     C                   IF        %subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):1:3)
056600030822     C                             = 'LNP'
056700030822     C                   EVAL      PiStr=%trim(%subst(
056800030822     C                             %subst(vlrppt:posDaDft+1:
056900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057000030822     C                   EXSR      CHKNUM
057100030822     C                   IF        PiInt=*on
057200030822     C                   Z-ADD     PiVal         VABLNP
057300040119     C                   Z-ADD     PiVal         VATLNP
057400030822     C                   ENDIF
057500030822     C                   ENDIF
057600030822     C* NRS
057700030822     C                   IF        %subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):1:3)
058000030822     C                             = 'NRS'
058100030822     C                   EVAL      PiStr=%trim(%subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058400030822     C                   EXSR      CHKNUM
058500030822     C                   IF        PiInt=*on
058600030822     C                   Z-ADD     PiVal         VABNRS
058700030822     C                   ENDIF
058800030822     C                   ENDIF
058900030822     C* CTR
059000030822     C                   IF        %subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):1:3)
059300030822     C                             = 'CTR'
059400030822     C                   EVAL      PiStr=%trim(%subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059700030822     C                   EXSR      CHKNUM
059800030822     C                   IF        PiInt=*on
059900030822     C                   Z-ADD     PiVal         VABCTR
060000030822     C                   ENDIF
060100030822     C                   ENDIF
060200030822     C* PKB
060300030822     C                   IF        %subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):1:3)
060600030822     C                             = 'PKB'
060700030822     C                   EVAL      PiStr=%trim(%subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061000030822     C                   EXSR      CHKNUM
061100030822     C                   IF        PiNum=*on
061200030822     C                   Z-ADD     PiVal         VABPKB
061300030822     C                   ENDIF
061400030822     C                   ENDIF
061500030822     C* VLB
061600030822     C                   IF        %subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):1:3)
061900030822     C                             = 'VLB'
062000030822     C                   EVAL      PiStr=%trim(%subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062300030822     C                   EXSR      CHKNUM
062400030822     C                   IF        PiNum=*on
062500030822     C                   Z-ADD     PiVal         VABVLB
062600030822     C                   ENDIF
062700030822     C                   ENDIF
062800030822     C* QFT
062900030822     C                   IF        %subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):1:3)
063200030822     C                             = 'QFT'
063300030822     C                   EVAL      PiStr=%trim(%subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600030822     C                   EXSR      CHKNUM
063700030822     C                   IF        PiNum=*on
063800030822     C                   Z-ADD     PiVal         VABQFT
063900030822     C                   ENDIF
064000030822     C                   ENDIF
064100030822     C* CBO
064200030822     C                   IF        %subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):1:3)
064500030822     C                             = 'CBO'
064600030822     C                   EVAL      VABCBO=%trim(%subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064900030822     C                   ENDIF
065000030822     C* TSP
065100030822     C                   IF        %subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):1:3)
065400030822     C                             = 'TSP'
065500030822     C                   EVAL      VABTSP=%trim(%subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065800030822     C                   ENDIF
065900030822     C* VAS
066000030822     C                   IF        %subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):1:3)
066300030822     C                             = 'VAS'
066400030822     C                   EVAL      VABVAS=%trim(%subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066700030822     C                   ENDIF
066800030822     C* VCA
066900030822     C                   IF        %subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):1:3)
067200030822     C                             = 'VCA'
067300030822     C                   EVAL      VABVCA=%trim(%subst(
067400030822     C                             %subst(vlrppt:posDaDft+1:
067500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067600030822     C                   ENDIF
067700030822     C* GCA
067800030822     C                   IF        %subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):1:3)
068100030822     C                             = 'GCA'
068200030822     C                   EVAL      VABGCA=%trim(%subst(
068300030822     C                             %subst(vlrppt:posDaDft+1:
068400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068500030822     C                   ENDIF
068600030822     C* CTM
068700030822     C                   IF        %subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):1:3)
069000030822     C                             = 'CTM'
069100030822     C                   EVAL      VABCTM=%trim(%subst(
069200030822     C                             %subst(vlrppt:posDaDft+1:
069300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069400030822     C                   ENDIF
069500030822     C* FFD
069600030822     C                   IF        %subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):1:3)
069900030822     C                             = 'FFD'
070000030822     C                   EVAL      VABFFD=%trim(%subst(
070100030822     C                             %subst(vlrppt:posDaDft+1:
070200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070300030822     C                   ENDIF
070400030822     C* VAD
070500030822     C                   IF        %subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):1:3)
070800030822     C                             = 'VAD'
070900030822     C                   EVAL      VABVAD=%trim(%subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071200030822     C                   ENDIF
071300030822     C* GMA
071400030822     C                   IF        %subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):1:3)
071700030822     C                             = 'GMA'
071800030822     C                   EVAL      VABGMA=%trim(%subst(
071900030822     C                             %subst(vlrppt:posDaDft+1:
072000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072100030822     C                   ENDIF
072200030822     C* GGA
072300030822     C                   IF        %subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):1:3)
072600030822     C                             = 'GGA'
072700030822     C                   EVAL      VABGGA=%trim(%subst(
072800030822     C                             %subst(vlrppt:posDaDft+1:
072900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073000030822     C                   ENDIF
073100030822     C* GVA
073200030822     C                   IF        %subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):1:3)
073500030822     C                             = 'GVA'
073600030822     C                   EVAL      VABGVA=%trim(%subst(
073700030822     C                             %subst(vlrppt:posDaDft+1:
073800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073900030822     C                   ENDIF
074000030822     C* TC1
074100030822     C                   IF        %subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):1:3)
074400030822     C                             = 'TC1'
074500030822     C                   EVAL      VABTC1=%trim(%subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074800030822     C                   ENDIF
074900030822     C* TC2
075000030822     C                   IF        %subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):1:3)
075300030822     C                             = 'TC2'
075400030822     C                   EVAL      VABTC2=%trim(%subst(
075500030822     C                             %subst(vlrppt:posDaDft+1:
075600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075700030822     C                   ENDIF
075800030822     C* ...
075900030822     C                   ENDIF
076000030822     C                   ENDDO
076100020204     C*
076200010330     C                   ENDSR
076300010607     C*----------------------------------------------------*
076400021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
076500010607     C*----------------------------------------------------*
076600010607     C     IMPVAB        BEGSR
076700010607     C*
076800010607     C                   SETOFF                                       3132
076900030515     C*
077000010607     C                   MOVEL     datcor        VABAAS
077100040119     C                   MOVEL     datcor        VATAAS
077200010607     C                   MOVE      datcor        VABMGS
077300010607     C*
077400030715     C**********
077500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
077600030715     C**********
077700030715     C                   IF        wGiro = *zeros
077800040706     C*
077900040706     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
078000040706     C                   EVAL      depvinDTA = vinDTA
078100040113     C*
078200040113     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
078300040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
078400040113     C                                      %trim(intcol(3))
078500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
078600031201     C     minu:maiu     XLATE     vindta        vindta
078700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
078800030715     C                   DOW       posDa <= %len(%trim(vindta))
078900030715     C*
079000030715     C* Gestisco il 1° campo
079100030715     C                   IF        i = 1
079200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
079300030715     C                   EVAL      posDa = 1
079400030715     C* Eseguo lo scan x trovare la fine del primo campo
079500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079600030715     C* A questo punto "estraggo" il campo corrente
079700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
079800030715     C                                                    (posA-posDa))
079900030715     C* X i campi successivi al 1°
080000030715     C                   ELSE
080100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
080200030715     C                   EVAL      posDa = posA + 1
080300030715     C* Eseguo lo scan x trovare la fine del campo corrente
080400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
080500030715     C* Gestisco l'ultimo campo
080600030715     C                   IF        posA = *zeros
080700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
080800030715     C* A questo punto "estraggo" il campo corrente
080900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081000030715     C                                                    (posA-posDa))
081100030715     C                   LEAVE
081200030715     C                   ELSE
081300030715     C* A questo punto "estraggo" il campo corrente
081400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081500030715     C                                                    (posA-posDa))
081600030715     C                   ENDIF
081700030715     C                   ENDIF
081800030715     C* Incremento il contatore d campo
081900030715     C                   EVAL      i = i +1
082000030715     C                   ENDDO
082100030715     C*
082200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
082300030715     C* delimitazione testo
082400030715     C                   EVAL      i = 1
082500030715     C                   DOW       i < %elem(SkSplitFLD)
082600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
082700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
082800030715     C                   EVAL      i = i + 1
082900030715     C                   ENDDO
083000030715     C                   EVAL      wGiro = 1
083100040706     C* Ripristino la 1° riga originale
083200040706     C                   EVAL      vinDTA = depvinDTA
083300030715     C                   ELSE
083400030715     C                   EVAL      wGiro = 2
083500031204     C**********
083600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
083700031204     C* d separatore campo
083800031204     C**********
083900031204     C                   EVAL      vindta = %trim(vindta)
084000031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
084100031204     C                   EVAL      vindta = %subst(vindta:2)
084200031204     C                   ENDDO
084300031204     C*
084400031204     C                   Z-ADD     *zeros        lunghInput        4 0
084500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
084600031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
084700031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
084800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
084900031204     C                   ENDDO
085000030715     C**********
085100030715     C* Effettuo lo split del campo dati d input
085200030715     C**********
085300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085400030715     C                   DOW       posDa <= %len(%trim(vindta))
085500030715     C*
085600030715     C* Gestisco il 1° campo
085700030715     C                   IF        i = 1
085800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
085900030715     C                   EVAL      posDa = 1
086000030715     C* Eseguo lo scan x trovare la fine del primo campo
086100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086200030715     C* A questo punto "estraggo" il campo corrente
086300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
086400030715     C                                                    (posA-posDa))
086500030715     C* X i campi successivi al 1°
086600030715     C                   ELSE
086700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
086800030715     C                   EVAL      posDa = posA + 1
086900031204     C* Verifico che nn vi sia il campo nn valorizzato
087000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
087100031204     C* Se campo nn valorizzato skippo al prossimo
087200031204     C                   EVAL      SkSplitCSV(i) = *blanks
087300031204     C                   EVAL      posA  = posA + 1
087400031204     C                   ELSE
087500030715     C* Eseguo lo scan x trovare la fine del campo corrente
087600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087700030715     C* Gestisco l'ultimo campo
087800030715     C                   IF        posA = *zeros
087900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
088000030715     C* A questo punto "estraggo" il campo corrente
088100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088200030715     C                                                    (posA-posDa))
088300030715     C                   LEAVE
088400030715     C                   ELSE
088500030715     C* A questo punto "estraggo" il campo corrente
088600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088700030715     C                                                    (posA-posDa))
088800030715     C                   ENDIF
088900031204     C                   ENDIF
089000030715     C                   ENDIF
089100030715     C* Incremento il contatore d campo
089200030715     C                   EVAL      i = i +1
089300030715     C                   ENDDO
089400030715     C*
089500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089600030715     C* delimitazione testo
089700030715     C                   EVAL      i = 1
089800030715     C                   DOW       i < %elem(SkSplitCSV)
089900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
090000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
090100030715     C                   EVAL      i = i + 1
090200030715     C                   ENDDO
090300030715     C*
090400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
090500030715     C                   EVAL      i = 1
090600030715     C                   DOW       i < %elem(SkSplitFLD)
090700030715     C*
090800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
090900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
091000030715     C                             SkSplitCSV(i) <> *blanks
091100030721     C*** ATB
091200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
091300030721     C                   EVAL      VABATB = SkSplitCSV(i)
091400030721     C                   ENDIF
091500030715     C*** RSD
091600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
091700030715     C                   EVAL      VABRSD = SkSplitCSV(i)
091800030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
091900030715     C     '@':'A'       XLATE     VABRSD        VABRSD
092000030715     C* ==
092100030715     C                   ENDIF
092200030721     C*** RD2
092300060112     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
092400030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
092500030721     C                   ENDIF
092600030715     C*** IND
092700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
092800030715     C                   EVAL      VABIND = SkSplitCSV(i)
092900030715     C                   ENDIF
093000030715     C*** LOD
093100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
093200030715     C                   EVAL      VABLOD = SkSplitCSV(i)
093300030715     C                   ENDIF
093400030715     C*** PRD
093500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
093600030715     C                   EVAL      VABPRD = SkSplitCSV(i)
093700030715     C                   ENDIF
093800030721     C*** NZD
093900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
094000030721     C                   EVAL      VABNZD = SkSplitCSV(i)
094100030721     C                   ENDIF
094200030715     C*** RMA
094300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
094400030715     C                   EVAL      VABRMA = SkSplitCSV(i)
094500030715     C                   ENDIF
094600030715     C*** NOT
094700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
094800030715     C                   EVAL      VABNOT = SkSplitCSV(i)
094900030715     C                   ENDIF
095000030715     C*** NT2
095100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
095200030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
095300030715     C                   ENDIF
095400030715     C*** VCA
095500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
095600030715     C                   EVAL      VABVCA = SkSplitCSV(i)
095700030715     C                   ENDIF
095800030715     C*** VAS
095900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
096000030715     C                   EVAL      VABVAS = SkSplitCSV(i)
096100030715     C                   ENDIF
096200030715     C*** FFD
096300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
096400030715     C                   EVAL      VABFFD = SkSplitCSV(i)
096500030715     C                   ENDIF
096600030721     C*** GC1
096700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
096800030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
096900030721     C                   ENDIF
097000030721     C*** GC2
097100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
097200030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
097300030721     C                   ENDIF
097400030721     C*** TSP
097500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
097600030721     C                   EVAL      VABTSP = SkSplitCSV(i)
097700030721     C                   ENDIF
097800030721     C*** NAS
097900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
098000030721     C                   EVAL      VABNAS = SkSplitCSV(i)
098100030721     C                   ENDIF
098200030721     C*** TIC
098300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
098400030721     C                   EVAL      VABTIC = SkSplitCSV(i)
098500030721     C                   ENDIF
098600030721     C*** GCA
098700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
098800030721     C                   EVAL      VABGCA = SkSplitCSV(i)
098900030721     C                   ENDIF
099000030721     C*** XCO
099100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
099200030721     C                   EVAL      VABXCO = SkSplitCSV(i)
099300030721     C                   ENDIF
099400030721     C*** CTM
099500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
099600030721     C                   EVAL      VABCTM = SkSplitCSV(i)
099700030721     C                   ENDIF
099800030721     C*** TCR
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
100000030721     C                   EVAL      VABTCR = SkSplitCSV(i)
100100030721     C                   ENDIF
100200030721     C*** CTS
100300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
100400030721     C                   EVAL      VABCTS = SkSplitCSV(i)
100500030721     C                   ENDIF
100600030721     C*** FTM
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
100800030721     C                   EVAL      VABFTM = SkSplitCSV(i)
100900030721     C                   ENDIF
101000030721     C*** VAD
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
101200030721     C                   EVAL      VABVAD = SkSplitCSV(i)
101300030721     C                   ENDIF
101400030721     C*** GMA
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
101600030721     C                   EVAL      VABGMA = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030721     C*** GGA
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
102000030721     C                   EVAL      VABGGA = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030721     C*** GVA
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
102400030721     C                   EVAL      VABGVA = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030721     C*** TC1
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
102800030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
102900030721     C                   ENDIF
103000030721     C*** TC2
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
103200030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*** SCL
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
103600030721     C                   EVAL      VABSCL = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*** RMO
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
104000030721     C                   EVAL      VABRMO = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** NMO
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
104400030721     C                   EVAL      VABNMO = SkSplitCSV(i)
104500030721     C                   ENDIF
104600040719     C*** CBO
104700040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
104800060112     C                   EVAL      VABCBO = SkSplitCSV(i)
104900040719     C                   ENDIF
105000030721     C*
105100040706     C* Reperisco quindi i campi numerici...
105200040706     C*** NSP
105300040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
105400040706     C                   EVAL      PiStr=SkSplitCSV(i)
105500040706     C                   EXSR      CHKNUM
105600040706     C                   IF        PiInt=*on
105700040706     C                   Z-ADD     PiVal         VABNSP
105800040706     C                   ELSE
105900040706     C                   SETON                                        31
106000040706     C                   EVAL      VABNSP = *zeros
106100040706     C                   EVAL      vinmsg = %trimr(vinmsg)
106200060112     C                             + ' ' + 'VABNSP'
106300040706     C                   ENDIF
106400040706     C                   ENDIF
106500040706     C*** RMN
106600040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
106700040706     C                   EVAL      PiStr=SkSplitCSV(i)
106800040706     C                   EXSR      CHKNUM
106900040706     C                   IF        PiInt=*on
107000040706     C                   Z-ADD     PiVal         VABRMN
107100040706     C                   ELSE
107200040706     C                   SETON                                        32
107300040706     C                   EVAL      VABRMN = 1
107400040706     C                   EVAL      vinmsg = %trimr(vinmsg)
107500040706     C                             + ' ' + 'VABRMN'
107600040706     C                   ENDIF
107700040706     C                   ENDIF
107800030721     C*** CAD
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
108000030721     C                   EVAL      PiStr=SkSplitCSV(i)
108100030721     C                   EXSR      CHKNUM
108200030721     C                   IF        PiInt=*on
108300030721     C                   Z-ADD     PiVal         Num5_0
108400030721     C                   MOVEL(p)  Num5_0        VABCAD
108500030721     C                   ELSE
108600030721     C                   SETON                                        32
108700030721     C                   EVAL      VABCAD = *zeros
108800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108900030721     C                             + ' ' + 'VABCAD'
109000030721     C                   ENDIF
109100030721     C                   ENDIF
109200030721     C*** NCL
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
109400030721     C                   EVAL      PiStr=SkSplitCSV(i)
109500030721     C                   EXSR      CHKNUM
109600030721     C                   IF        PiInt=*on
109700030721     C                   Z-ADD     PiVal         VABNCL
109800030721     C                   ELSE
109900030721     C                   SETON                                        32
110000030721     C                   EVAL      VABNCL = *zeros
110100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110200040706     C                             + ' ' + 'VABNCL'
110300030721     C                   ENDIF
110400030721     C                   ENDIF
110500030721     C*** PKB
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
110700030721     C                   EVAL      PiStr=SkSplitCSV(i)
110800030721     C                   EXSR      CHKNUM
110900030721     C                   IF        PiNum=*on
111000030721     C                   Z-ADD     PiVal         VABPKB
111100030721     C                   ELSE
111200030721     C                   SETON                                        32
111300030721     C                   EVAL      VABPKB = *zeros
111400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111500030721     C                             + ' ' + 'VABPKB'
111600030721     C                   ENDIF
111700030721     C                   ENDIF
111800030721     C*** CAS
111900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
112000030721     C                   EVAL      PiStr=SkSplitCSV(i)
112100030721     C                   EXSR      CHKNUM
112200030721     C                   IF        PiNum=*on
112300030721     C                   Z-ADD     PiVal         VABCAS
112400030721     C                   ELSE
112500030721     C                   SETON                                        32
112600030721     C                   EVAL      VABCAS = *zeros
112700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112800030721     C                             + ' ' + 'VABCAS'
112900030721     C                   ENDIF
113000030721     C                   ENDIF
113100030721     C*** CCM
113200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
113300030721     C                   EVAL      PiStr=SkSplitCSV(i)
113400030721     C                   EXSR      CHKNUM
113500030721     C                   IF        PiInt=*on      AND
113600030721     C                             PiVal<=9999999 AND
113700030721     C                             PiVal>*zeros
113800030721     C                   Z-ADD     PiVal         VABCCM
113900040119     C                   Z-ADD     PiVal         VATCCM
114000030721     C                   ELSE
114100030721     C                   SETON                                        32
114200030721     C                   EVAL      VABCCM = *zeros
114300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114400040119     C                             + ' ' + 'VABCCM VATCCM'
114500030721     C                   ENDIF
114600030721     C                   ENDIF
114700030721     C*** LNP
114800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
114900030721     C                   EVAL      PiStr=SkSplitCSV(i)
115000030721     C                   EXSR      CHKNUM
115100030721     C                   IF        PiInt=*on    AND
115200030721     C                             PiVal<=999   AND
115300030721     C                             PiVal>*zeros
115400030721     C                   Z-ADD     PiVal         VABLNP
115500040119     C                   Z-ADD     PiVal         VATLNP
115600030721     C                   ELSE
115700030721     C                   SETON                                        32
115800030721     C                   EVAL      VABLNP = *zeros
115900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116000040119     C                             + ' ' + 'VABLNP VATLNP'
116100030721     C                   ENDIF
116200030721     C                   ENDIF
116300030721     C*** NRS
116400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
116500030721     C                   EVAL      PiStr=SkSplitCSV(i)
116600030721     C                   EXSR      CHKNUM
116700030721     C                   IF        PiInt=*on AND
116800030721     C                             PiVal<=99
116900030721     C                   Z-ADD     PiVal         VABNRS
117000030721     C                   ELSE
117100030721     C                   SETON                                        32
117200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117300030721     C                             + ' ' + 'VABNRS'
117400030721     C                   ENDIF
117500030721     C                   ENDIF
117600030721     C*** LNA
117700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
117800030721     C                   EVAL      PiStr=SkSplitCSV(i)
117900030721     C                   EXSR      CHKNUM
118000030721     C                   IF        PiInt=*on    AND
118100030721     C                             PiVal<=999   AND
118200030721     C                             PiVal>*zeros
118300030721     C                   Z-ADD     PiVal         VABLNA
118400030721     C                   ELSE
118500030721     C                   SETON                                        32
118600030721     C                   EVAL      VABLNA = *zeros
118700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118800030721     C                             + ' ' + 'VABLNA'
118900030721     C                   ENDIF
119000030721     C                   ENDIF
119100030721     C*** CTR
119200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
119300030721     C                   EVAL      PiStr=SkSplitCSV(i)
119400030721     C                   EXSR      CHKNUM
119500030721     C                   IF        PiInt=*on  AND
119600030721     C                             PiVal<=999
119700030721     C                   Z-ADD     PiVal         VABCTR
119800030721     C                   ELSE
119900030721     C                   SETON                                        32
120000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120100030721     C                             + ' ' + 'VABCTR'
120200030721     C                   ENDIF
120300030721     C                   ENDIF
120400030721     C*** IAS
120500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
120600030721     C                   EVAL      PiStr=SkSplitCSV(i)
120700030721     C                   EXSR      CHKNUM
120800030721     C                   IF        PiNum=*on
120900030721     C                   Z-ADD     PiVal         VABIAS
121000030721     C                   ELSE
121100030721     C                   SETON                                        32
121200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121300030721     C                             + ' ' + 'VABIAS'
121400030721     C                   ENDIF
121500030721     C                   ENDIF
121600030721     C*** VLB
121700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
121800030721     C                   EVAL      PiStr=SkSplitCSV(i)
121900030721     C                   EXSR      CHKNUM
122000030721     C                   IF        PiNum=*on
122100030721     C                   Z-ADD     PiVal         VABVLB
122200030721     C                   ELSE
122300030721     C                   SETON                                        32
122400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122500030721     C                             + ' ' + 'VABVLB'
122600030721     C                   ENDIF
122700030721     C                   ENDIF
122800030721     C*** QFT
122900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
123000030721     C                   EVAL      PiStr=SkSplitCSV(i)
123100030721     C                   EXSR      CHKNUM
123200030721     C                   IF        PiNum=*on
123300030721     C                   Z-ADD     PiVal         VABQFT
123400030721     C                   ELSE
123500030721     C                   SETON                                        32
123600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123700030721     C                             + ' ' + 'VABQFT'
123800030721     C                   ENDIF
123900030721     C                   ENDIF
124000030721     C*** NCD
124100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
124200030721     C                   EVAL      PiStr=SkSplitCSV(i)
124300030721     C                   EXSR      CHKNUM
124400030721     C                   IF        PiInt=*on      AND
124500030721     C                             PiVal<=9999999
124600030721     C                   Z-ADD     PiVal         VABNCD
124700030721     C                   ELSE
124800030721     C                   SETON                                        32
124900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125000030721     C                             + ' ' + 'VABNCD'
125100030721     C                   ENDIF
125200030721     C                   ENDIF
125300030721     C*** NCA
125400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
125500030721     C                   EVAL      PiStr=SkSplitCSV(i)
125600030721     C                   EXSR      CHKNUM
125700030721     C                   IF        PiInt=*on      AND
125800030721     C                             PiVal<=9999999
125900030721     C                   Z-ADD     PiVal         VABNCA
126000030721     C                   ELSE
126100030721     C                   SETON                                        32
126200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126300030721     C                             + ' ' + 'VABNCA'
126400030721     C                   ENDIF
126500030721     C                   ENDIF
126600030721     C*** ZNC
126700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
126800030721     C                   EVAL      PiStr=SkSplitCSV(i)
126900030721     C                   EXSR      CHKNUM
127000030721     C                   IF        PiInt=*on AND
127100030721     C                             PiVal<=99
127200030721     C                   Z-ADD     PiVal         VABZNC
127300030721     C                   ELSE
127400030721     C                   SETON                                        32
127500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127600030721     C                             + ' ' + 'VABZNC'
127700030721     C                   ENDIF
127800030721     C                   ENDIF
127900030721     C*** DCR
128000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
128100030721     C                   EVAL      PiStr=SkSplitCSV(i)
128200030721     C                   EXSR      CHKNUM
128300030721     C                   IF        PiInt=*on       AND
128400030721     C                             PiVal<=99999999
128500030721     C                   Z-ADD     PiVal         VABDCR
128600030721     C                   ELSE
128700030721     C                   SETON                                        32
128800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128900030721     C                             + ' ' + 'VABDCR'
129000030721     C                   ENDIF
129100030721     C                   ENDIF
129200030721     C*** HCR
129300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
129400030721     C                   EVAL      PiStr=SkSplitCSV(i)
129500030721     C                   EXSR      CHKNUM
129600030721     C                   IF        PiInt=*on   AND
129700030721     C                             PiVal<=9999
129800030721     C                   Z-ADD     PiVal         VABHCR
129900030721     C                   ELSE
130000030721     C                   SETON                                        32
130100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130200030721     C                             + ' ' + 'VABHCR'
130300030721     C                   ENDIF
130400030721     C                   ENDIF
130500030721     C*** VMD
130600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
130700030721     C                   EVAL      PiStr=SkSplitCSV(i)
130800030721     C                   EXSR      CHKNUM
130900030721     C                   IF        PiNum=*on
131000030721     C                   Z-ADD     PiVal         VABVMD
131100030721     C                   ELSE
131200030721     C                   SETON                                        32
131300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131400030721     C                             + ' ' + 'VABVMD'
131500030721     C                   ENDIF
131600030721     C                   ENDIF
131700030721     C*** ANT
131800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
131900030721     C                   EVAL      PiStr=SkSplitCSV(i)
132000030721     C                   EXSR      CHKNUM
132100030721     C                   IF        PiInt=*on        AND
132200030721     C                             PiVal<=999999999
132300030721     C                   Z-ADD     PiVal         VABANT
132400030721     C                   ELSE
132500030721     C                   SETON                                        32
132600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132700030721     C                             + ' ' + 'VABANT'
132800030721     C                   ENDIF
132900030721     C                   ENDIF
133000030721     C*** CMO
133100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
133200030721     C                   EVAL      PiStr=SkSplitCSV(i)
133300030721     C                   EXSR      CHKNUM
133400030721     C                   IF        PiInt=*on
133500030721     C                   Z-ADD     PiVal         Num5_0
133600030721     C                   MOVEL(p)  Num5_0        VABCMO
133700030721     C                   ELSE
133800030721     C                   SETON                                        32
133900030721     C                   EVAL      VABCMO = *zeros
134000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134100030721     C                             + ' ' + 'VABCMO'
134200030721     C                   ENDIF
134300030721     C                   ENDIF
134400040706     C*
134500060112     C*** CELL
134600060112     C                   IF        %trim(SkSplitFLD(i)) = 'CELL'
134700060112     C                   EVAL      wCELL = SkSplitCSV(i)
134800060112     C                   ENDIF
134900051108     C*** VATNOT TRC=A
135000060112     C                   IF        %trim(SkSplitFLD(i)) = 'TRC_A'
135100051108     C                   EVAL      wVATNOT_A = SkSplitCSV(i)
135200040719     C                   ENDIF
135300051108     C*** VATNOT TRC=B
135400051108     C                   IF        %trim(SkSplitFLD(i)) = 'TRC_B'
135500051108     C                   EVAL      wVATNOT_B = SkSplitCSV(i)
135600051108     C                   ENDIF
135700060112     C*** VABNOT/VABNT2 NOTE
135800060112     C                   IF        %trim(SkSplitFLD(i)) = 'NOTE'
135900060112     C                   EVAL      VABNOT = %subst(SkSplitCSV(i):1:35)
136000060112     C                   EVAL      VABNT2 = %subst(SkSplitCSV(i):35+1:35)
136100060112     C                   ENDIF
136200060112     C*** CONTRASS
136300060112     C                   IF        %trim(SkSplitFLD(i)) = 'CONTRASS'
136400060112     C                   EVAL      wCONTRASS = SkSplitCSV(i)
136500060112     C                   ENDIF
136600061113     C*** APPUNTAMENTO
136700061113     C                   IF        %trim(SkSplitFLD(i)) = 'APPUNTAMEN'
136800061113     C                   IF        %trim(SkSplitCSV(i)) = 'APPUNTAMENTO' OR
136900061113     C                             %trim(SkSplitCSV(i)) = 'appuntamento' OR
137000061113     C                             %trim(SkSplitCSV(i)) = 'Appuntamento'
137100061113     C                   EVAL      VABTC1 = 'A'
137200061113     C                   ENDIF
137300061113     C                   ENDIF
137400030715     C*
137500030715     C                   ENDIF
137600030715     C                   EVAL      i = i + 1
137700030715     C                   ENDDO
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
139000060117     C                   EVAL      VABLOD = O95LOC
139100040706     C                   ENDIF
139200060112     C*
139300060112     C* NSP => Stacco un numeratore da AZNUM
139400060112     C                   clear                   TRUL33DS
139500060112     C                   eval      I33OPE = *zeros
139600060112     C                   eval      I33CNU = 302
139700060112     C                   eval      I33NUM = 1
139800060112     C                   movel     TRUL33DS      KPJBU
139900060112     C                   call      'TRUL33R'
140000060112     C                   parm                    KPJBA
140100060112     C                   movel     KPJBU         TRUL33DS
140200060112     C                   if        O33ERR = *zeros
140300060112     C                   z-add     O33NRF        VABNSP
140400060112     C                   z-add     O33NRF        VATNSP
140500060112     C                   else
140600060112     C                   EVAL      vinmsg = %trimr(vinmsg)
140700060112     C                             + ' ' + 'VABNSP VATNSP'
140800060112     C                   endif
140900060112     C*
141000060112     C* RMN
141100060112     C                   EVAL      wVABRMN_Alfa = %subst(VABRMA:1:2)+
141200060112     C                                            %subst(VABRMA:4:2)+
141300060112     C                                            %subst(VABRMA:7:4)
141400060112     C                   MOVE(P)   wVABRMN_Alfa  VABRMN
141500060112     C*
141600060112     C* RD2
141700060112     C                   EVAL      VABRD2 = wVATNOT_A
141800060112     C                   IF        wCELL <> *blanks
141900060112     C                   MOVE      wCELL         VABRD2
142000060112     C                   ENDIF
142100060112     C*
142200060112     C* CAS
142300060112     C                   IF        wCONTRASS <> *blanks
142400060112     C                   MOVEL     '1'           FlgCAS
142500060112     C*
142600060112     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
142700060112     C                   EVAL      TxtInOut   = wCONTRASS
142800060112     C                   EVAL      EsitoBon   = *blanks
142900060112     C*
143000060112     C                   CALL      'XCHKCHAR'
143100060112     C                   PARM                    TxtInOut
143200060112     C                   PARM                    ElencoChar
143300060112     C                   PARM                    TipoElenco
143400060112     C                   PARM                    CharSost
143500060112     C                   PARM                    UpperCase
143600060112     C                   PARM                    ChkNull
143700060112     C                   PARM                    CharNull
143800060112     C                   PARM                    EsitoBon
143900060112     C*
144000060112     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
144100060112     C                   IF        EsitoBon = '1'
144200060112     C                   EVAL      wCONTRASS = TxtInOut
144300060112     C                   ENDIF
144400060112     C*
144500060112     C* Determino il valore numerico
144600060112     C                   EVAL      PiStr=wCONTRASS
144700060112     C                   EXSR      CHKNUM
144800060112     C                   IF        PiNum=*on
144900060112     C                   Z-ADD     PiVal         VABCAS
145000060112     C                   ELSE
145100060112     C                   Z-ADD     *zeros        VABCAS
145200060112     C                   ENDIF
145300060112     C                   ENDIF
145400040719     C*
145500040719     C* Se NSP nn valorizzato considero errore bloccante in traduzione
145600060112     C                   IF        VABNSP = *zeros
145700040719     C                   SETON                                        31
145800040719     C                   ENDIF
145900040719     C*
146000040719     C* Se LNP nn valorizzato considero errore bloccante in traduzione
146100040719     C                   IF        VABLNP = *zeros
146200040719     C                   SETON                                        31
146300040719     C                   ENDIF
146400040706     C*
146500040706     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
146600040706     C* d valorizzarli sempre entrambi
146700040706     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
146800040706     C                   Z-ADD     VABNSP        VABRMN
146900040706     C                   ENDIF
147000040706     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
147100040706     C                   MOVEL     VABRMN        VABRMA
147200040706     C                   ENDIF
147300040113     C*
147400040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
147500060112     C                   IF        FlgCAS = '1'
147600040119     C                   EVAL      VABVCA = 'EUR'
147700040119     C                   IF        VABCBO = '1'
147800040113     C                   EVAL      VABCBO = '4'
147900040119     C                   ENDIF
148000040119     C                   IF        VABCBO = '2'
148100040119     C                   EVAL      VABCBO = '6'
148200040119     C                   ENDIF
148300040113     C                   ENDIF
148400020204     C*
148500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
148600020204     C                   EXSR      CHKIMPDIV
148700030715     C                   ENDIF
148800020204     C*
148900010607     C                   ENDSR
149000010607     C*----------------------------------------------------*
149100010601
149200020204
149300020204     C*----------------------------------------------------*
149400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
149500020204     C*----------------------------------------------------*
149600020204     C     CHKIMPDIV     BEGSR
149700020204     C*
149800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
149900020204     C                   Z-ADD     *zeros        wrkDec            9 9
150000020204     C*
150100020204     C* Come prima cosa effettuo considerazioni sulla divisa
150200020204     C                   IF        vabIAS > *zeros
150300020204     C                   IF        vabVAS <> 'EUR'
150400020204     C                   EVAL      vabVAS =  'ITL'
150500020204     C                   ENDIF
150600020204     C                   ENDIF
150700020204     C*
150800020204     C                   IF        vabCAS > *zeros
150900020204     C                   IF        vabVCA <> 'EUR'
151000020204     C                   EVAL      vabVCA =  'ITL'
151100020204     C                   ENDIF
151200020204     C                   ENDIF
151300020204     C*
151400020204     C                   IF        vabVMD > *zeros
151500020204     C                   IF        vabVAD <> 'EUR'
151600020204     C                   EVAL      vabVAD =  'ITL'
151700020204     C                   ENDIF
151800020204     C                   ENDIF
151900020204     C*
152000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
152100020204     C                   Z-ADD     vabIAS        wrkDec
152200020204     C                   IF        wrkDec > *zeros
152300020204     C                   IF        vabVAS = 'ITL'
152400020204     C                   EVAL      vabIAS = *zeros
152500020204     C                   ENDIF
152600020204     C                   ENDIF
152700020204     C*
152800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
152900020204     C                   Z-ADD     vabCAS        wrkDec
153000020204     C                   IF        wrkDec > *zeros
153100020204     C                   IF        vabVCA = 'ITL'
153200020204     C                   EVAL      vabCAS = *zeros
153300020204     C                   ENDIF
153400020204     C                   ENDIF
153500020204     C*
153600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
153700020204     C                   Z-ADD     vabVMD        wrkDec
153800020204     C                   IF        wrkDec > *zeros
153900020204     C                   IF        vabVAD = 'ITL'
154000020204     C                   EVAL      vabVMD = *zeros
154100020204     C                   ENDIF
154200020204     C                   ENDIF
154300020204     C*
154400020204     C                   ENDSR
154500020204     C***
154600020204
154700010330
154800010330
154900010330     C*----------------------------------------------------*
155000010330     C*  CONTROLLO NUMERICITA' CAMPI
155100010330     C*----------------------------------------------------*
155200010330     C     CHKNUM        BEGSR
155300010330     C*
155400010606     C                   IF        PiDecChr = *blanks
155500030715     C                   EVAL      PiDecChr = CharNUM
155600010606     C                   ENDIF
155700010606     C*
155800010606     C                   CALL(e)   'ISNUMERIC'
155900010330     C                   PARM                    PiStr            30
156000010606     C                   PARM                    PiDecChr          1
156100010330     C                   PARM      *ZEROS        PiVal            30 9
156200010330     C                   PARM      '0'           PiInt             1
156300010330     C                   PARM      '0'           PiNum             1
156400010330     C                   IF        %error
156500010606     C                   EVAL      PiNum=*off
156600010330     C                   ENDIF
156700010330     C*
156800010330     C                   ENDSR
156900010330     C***
157000010330
157100010601
157200010601
157300010601
157400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
157500010601     C     repfil        BEGSR
157600010601     C*
157700010601     C                   if        invfil = *zeros and
157800010601     C                             depfil > *zeros and
157900010629     C                             (vinflg = *blanks or
158000010629     C                              vinflg = *zeros)
158100010601     C
158200010601     C                   eval      invfil = depfil
158300010601     C                   endif
158400010601     C*
158500010601     C                   if        depfil <> invfil and
158600010601     C                             invfil > *zeros
158700010601     C                   eval      flgMulti = '1'
158800010601     C                   if        vinflg = *blanks
158900010601     C                   add       1             cntNonEl
159000010601     C                   endif
159100010601     C                   endif
159200010601     C*
159300010601     C                   if        vinflg = '2'
159400010601     C                   eval      flgStato = '2'
159500010601     C                   endif
159600010601     C*
159700010601     C                   ENDSR
159800010601     C***
159900010601
160000010601
160100010601
160200010330
160300040119      /TITLE Invio dei dati al punto operativo.
160400040119     C     invio         BEGSR
160500040119     C*
160600040119     C* 1° invio FIVAT
160700040119     C                   reset                   dscmz
160800040119     C                   move      vlrpoi        cmzdst
160900040119     C                   eval      cmzfld = 'FIVATWWR'
161000040119     C                   eval      cmzmbd = vlrhdl
161100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
161200040119     C***                if        prmfir = *blanks
161300040119     C                   eval      cmzfla = 'FIVAT00F'
161400040119     C                   eval      cmzmba = 'FIVAT00F'
161500040119     C***                else
161600040119     C***                eval      cmzfla = prmfir
161700040119     C***                eval      cmzmba = prmfir
161800040119     C***                endif
161900040119     C                   eval      cmznrr = *zeros
162000040119     C                   move      §ctrokvt      cmznrr
162100040119     C                   eval      cmzlba = vlrfl1
162200040119     C                   call(e)   'TIS711C'
162300040119     C                   parm                    dscmz
162400040119     C                   parm      *blanks       esito
162500040119     C                   if        %error
162600040119     C                             or cmzerr = '1'
162700040119     C                             or esito  = '1'
162800040119     C                   eval      wrkesito = '3'
162900040119     C                   else
163000040119     C*
163100040119     C* 2° invio FIVAB
163200040119     C                   reset                   dscmz
163300040119     C                   move      vlrpoi        cmzdst
163400040119     C                   eval      cmzfld = vlrfou
163500040119     C                   eval      cmzmbd = vlrhdl
163600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
163700040119     C***                if        prmfir = *blanks
163800040119     C                   eval      cmzfla = 'FIVAB00F'
163900040119     C                   eval      cmzmba = 'FIVAB00F'
164000040119     C***                else
164100040119     C***                eval      cmzfla = prmfir
164200040119     C***                eval      cmzmba = prmfir
164300040119     C***                endif
164400040119     C                   eval      cmznrr = *zeros
164500040119     C                   move      §ctrokvb      cmznrr
164600040119     C                   eval      cmzlba = vlrfl1
164700040119     C                   call(e)   'TIS711C'
164800040119     C                   parm                    dscmz
164900040119     C                   parm      *blanks       esito
165000040119     C                   if        %error
165100040119     C                             or cmzerr = '1'
165200040119     C                             or esito  = '1'
165300040119     C                   eval      wrkesito = '3'
165400040119     C                   endif
165500040119     C                   endif
165600040119     C*
165700040119     C                   ENDSR
165800040119     C***
165900010601
166000010601
166100010601
166200010601
166300010601      /TITLE Invio dei dati al punto operativo.
166400010601     C     opeini        BEGSR
166500010601     C*
166600010601     C* Inizializzo flag e contatori operativi
166700010601     C                   movel     '0'           flgGiro           1
166800010601     C                   movel     '0'           flgMulti          1
166900010601     C                   movel     '1'           flgStato          1
167000010615     C                   movel     '0'           flgOk             1
167100010601     C                   z-add     *zeros        cntNonEl         10 0
167200010601     C                   z-add     *zeros        depfil            3 0
167300010601     C                   z-add     *zeros        invfil            3 0
167400010601     C*
167500010601     C                   ENDSR
167600010601     C***
167700010601
167800010601
167900050421
168000050421
168100050421     C     *pssr         BEGSR
168200050421     C*
168300050421     C                   eval      wrkesito = '2'
168400050421     C*
168500050421     C                   ENDSR     '*CANCL'
168600050421     C***
168700050421
168800010601
168900010330
169000010330
169100000613     C     *inzsr        BEGSR
169200990910     C*
169300990910     C     *entry        plist
169400990920     C                   parm                    tivlrds
169500990921     C                   parm      wrkesito      esito
169600000724     C                   parm                    prmlit
169700000710     C                   parm                    prmfir
169800010330     C*
169900010330     C* CALCOLA LA DATA CORRENTE
170000010330     C                   time                    wn14             14 0
170100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
170200010330     C                   z-add     wn8           g08dat
170300010330     C                   z-add     *zeros        g08inv
170400010330     C                   movel     '0'           g08err
170500010330     C                   call      'XSRDA8'
170600010330     C                   parm                    wlbda8
170700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
170800000613     C*
170900000613     C                   ENDSR
171000000613     C***
171100990908
171200060112     Otitvq5p   E            riepilogo         2
171300990915     O                                              'Upload via Internet'
171400990915     O                                           +1 'Traduzione TIVIN00R -'
171500021025     O                                           +1 'FIVABWWR'
171600990915     O                                           +1 'Testate bolle'
171700990915     O          E            riepilogo   2
171800990915     O                       wrkdata
171900990915     O                       wrkora              +1
172000990915     O                       procname            +1
172100990915     O          E            riepilogo   2
172200990915     O                                              'Cliente..................:'
172300010605     O                       VABCCM        z     +1
172400990915     O          E            riepilogo   2
172500990920     O                                              'Riferimento Strategi.....:'
172600990920     O                       vlrhdl              +1
172700990915     O          E            riepilogo   2
172800990915     O                                              'Giusti...................:'
172900040119     O                       §CTROKVB      2   +  1
173000990915     O          E            riepilogo   2
173100990915     O                                              'Sbagliati e corretti.....:'
173200971022     O                       §CTRMO        2   +  1
173300990915     O          E            riepilogo   2
173400990915     O                                              'Sbagliati e scartati.....:'
173500971022     O                       §CTRNO        2   +  1
173600000613
173700060112     Otitvq5ps  E            testdett          2
173800000613     O                                              'Upload via Internet'
173900000613     O                                           +1 'Traduzione TIVIN00R -'
174000021025     O                                           +1 'FIVABWWR'
174100000613     O                                           +1 'Testate bolle'
174200000616     O          E            testdett    3
174300000613     O                                           +2 'N° rec'
174400000613     O                                           +3 'Anteprima contenuto'
174500000616     O          E            rigadett    2
174600000613     O                       rrnum               +2
174700000621     O                       recko               +3
174800000616     O          E            findett     2
174900000613     O                       wrkdata
175000000613     O                       wrkora              +1
175100000613     O                       procname            +1
175200000616     O          E            findett     2
175300000613     O                                              'Cliente..................:'
175400010605     O                       VABCCM        z     +1
175500000616     O          E            findett     2
175600000613     O                                              'Riferimento Strategi.....:'
175700000613     O                       vlrhdl              +1
175800000616     O          E            findett     2
175900000613     O                                              'Giusti...................:'
176000040119     O                       §CTROKVB      2   +  1
176100000616     O          E            findett     2
176200000613     O                                              'Sbagliati e corretti.....:'
176300000613     O                       §CTRMO        2   +  1
176400000616     O          E            findett     2
176500000613     O                                              'Sbagliati e scartati.....:'
176600000613     O                       §CTRNO        2   +  1
176700000613** CMD - COMANDI CL
176800060112OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVQ5P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
176900060112OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVQ5PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
177000060112DLTOVR FILE(TITVQ5P TITVQ5PS) LVL(*)
177100010606
177200000613
177300040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
177400061113VABRMA;TRC_A;Testo88;VABRSD;VABIND;CELL;VABCAD;
177500061113VABLOD;TRC_B;APPUNTAMEN;NOTE;CONTRASS;Testo5
177600040113
