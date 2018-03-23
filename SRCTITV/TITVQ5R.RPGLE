000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800060112     Ftitvq5p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100060112     Ftitvq5ps  O    f  198        PRINTER usropn
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
004000060112     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
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
005800060112     D wVATNOT_A       s                   LIKE(vatnot) inz(*blanks)
005900060112     D wVATNOT_B       s                   LIKE(vatnot) inz(*blanks)
006000060112     D wVABRMN_Alfa    s              8    inz(*blanks)
006100060112     D wCELL           s             10    inz(*blanks)
006200060112     D wCONTRASS       s             50    inz(*blanks)
006300030822     D*------------------
006400030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006500030822     D*------------------
006600030822     D posDaDft        S              3  0 INZ(*zeros)
006700030822     D posADft         S              3  0 INZ(*zeros)
006800030822     D j               s              3  0 INZ(1)
006900030822     D wGiroDft        s              1  0 INZ(*zeros)
007000010330     D*------------------
007100010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
007200010330     D*------------------
007300010330     D WLBDA8          DS                  INZ
007400010330     D  G08DAT                 1      8  0
007500010330     D  G08INV                 9     16  0
007600010330     D  G08ERR                17     17
007700010330     D  G08TGI                18     22  0
007800031201     D*------------------
007900031201     D* Costanti
008000031201     D*------------------
008100031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008200031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008300060112     D*------------------
008400060112     D* DS REPERIMENTO NUMERATORE
008500060112     D*------------------
008600060112     D trul33ds      e ds                  inz
008700060112     D*------------------
008800060112     D* DS ARCHITETTURA
008900060112     D*------------------
009000060112     D kpjba         e ds                  inz
009100060112     D*------------------
009200060112     D* VARIABILI X "BONIFICA" STRINGHE
009300060112     D*------------------
009400060112     D TxtInOut        S           2048
009500060112     D ElencoChar      S            256
009600060112     D TipoElenco      S              1
009700060112     D CharSost        S              1
009800060112     D UpperCase       S              1
009900060112     D ChkNull         S              1
010000060112     D CharNull        S              1
010100060112     D EsitoBon        S              1
010200060112     D*------------------
010300010330
010400010330
010500010330
010600990915     C                   time                    wrkdata
010700990915     C                   time                    wrkora
010800000913     C                   reset                   rrnum
010900990921     C                   reset                   esito
011000990921     C                   reset                   wrkesito
011100010601     C*
011200010601     C                   exsr      opeini
011300010605     C                   exsr      rwvab
011400010601     C*
011500010601     C                   seton                                        lr
011600010601
011700010601
011800010601
011900010601     C*--------------------------------------------------------
012000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012100010601     C*--------------------------------------------------------
012200010601     C     PREELA        BEGSR
012300010601     C*
012400010601     C* SE OCCORRE SPEDIRE IN FILIALE
012500010601     C                   if        invfil <> *zeros and
012600010601     C                             flgGiro = '0'
012700010601     C*
012800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012900010601     C                   eval      flgGiro = '1'
013000000724     C*
013100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
013200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013300010601     C     invfil        chain     azorg01l
013400000724     C                   if        %found
013500000724     C                   movel(p)  CMD(1)        depcmd
013600020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
013700000724     C*
013800000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
013900011113     C                   Z-ADD     150           LENGH            15 5
014000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014100000724     C                   PARM                    depcmd
014200000724     C                   PARM                    LENGH
014300000724     C*
014400000724     C                   endif
014500000616     C*
014600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014700000616     C                   movel(p)  CMD(2)        depcmd
014800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
014900000616     C*
015000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
015100011113     C                   Z-ADD     150           LENGH            15 5
015200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015300000616     C                   PARM                    depcmd
015400000616     C                   PARM                    LENGH
015500000613     C*
015600060112     C                   if        not %open(titvq5ps)
015700060112     C                   open      titvq5ps
015800030715     C                   except    testdett
015900000613     C                   endif
016000010601     C*
016100010601     C                   endif
016200010601     C*
016300010601     C                   ENDSR
016400010601     C***
016500010601
016600010601
016700010601
016800010601
016900010601     C*--------------------------------------------------------
017000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
017100010601     C*--------------------------------------------------------
017200010601     C     ENDELA        BEGSR
017300990915     C*
017400000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
017500040119     C*
017600040119     C* Effettuo la chiamata al CLLE preposto
017700040119     C                   call(e)   'TITVVTC'
017800040119     C                   parm                    parccm
017900040119     C                   parm                    parmbr
018000040119     C                   parm      '2'           paropz
018100050201     C*
018200050201     C* Effettuo lancio TISI95 solo x chiusura
018300050201     C                   CLEAR                   TISI95DS
018400050201     C                   EVAL      I95TLA = 'C'
018500050201     C                   CALL      'TISI95R'
018600050201     C                   PARM                    TISI95DS
018700000613     C*
018800060112     C                   if        %open(titvq5ps)
018900000616     C                   except    findett
019000060112     C                   close     titvq5ps
019100000613     C                   endif
019200000616     C*
019300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
019400011113     C                   Z-ADD     150           LENGH            15 5
019500010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
019600000616     C                   PARM                    CMD(3)
019700000616     C                   PARM                    LENGH
019800000616     C*
019900010601     C                   ENDSR
020000010601     C***
020100010601
020200010601
020300010601
020400000613
020500000613     C*--------------------------------------------------------
020600000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
020700000613     C*--------------------------------------------------------
020800000621     C     STPR          BEGSR
020900000613     C*
021000060112     C                   if        not %open(titvq5p)
021100060112     C   50              open      titvq5p
021200990915     C                   endif
021300990915     C*
021400030715     C   50              except    riepilogo
021500990915     C*
021600060112     C                   if        %open(titvq5p)
021700060112     C   50              close     titvq5p
021800990914     C                   endif
021900000613     C*
022000000613     C                   ENDSR
022100000613     C***
022200000613
022300010601
022400010601
022500010330     C*--------------------------------------------------------
022600021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
022700010330     C*--------------------------------------------------------
022800010605     C     RWVAB         BEGSR
022900010330     C*
023000010330     C                   if        not %open(tivin00r)
023100010330     C                   open      tivin00r
023200010330     C                   endif
023300021025     C                   if        not %open(fivabwwr)
023400021025     C                   open      fivabwwr
023500010330     C                   endif
023600040119     C*
023700040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
023800040119     C                   exsr      prevat
023900040119     C*
024000040119     C                   if        chkcall = '0'
024100040119     C*
024200040119     C                   if        not %open(fivatwwr)
024300040119     C                   open      fivatwwr
024400040119     C                   endif
024500010330     C*
024600040119     C                   clear                   §CTROKVB          7 0
024700040119     C                   clear                   §CTROKVT          7 0
024800040119     C                   clear                   §CTRMO            7 0
024900040119     C                   clear                   §CTRNO            7 0
025000040719     C*
025100040719     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
025200040719     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
025300060112     C***                exsr      inzvar
025400060112     C***                exsr      impvab
025500040706     C*
025600040706     C  N70              do        *HIVAL
025700010330     C*
025800040706     C                   read      tivin00r                               70
025900010618     C*
026000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
026100010618     C                   if        vinflg = '1'
026200010618     C                   eval      flgOk = '1'
026300010618     C                   endif
026400010618     C*
026500040316     C                   if        (vindta > *blanks AND
026600040316     C                              %trim(vindta) <> *all';') OR
026700040316     C                             wGiro = *zeros
026800010330     C                   add       1             rrnum
026900010330     C*
027000010601     C                   if        *in70 = *off and
027100010330     C                             (vinflg = *blanks
027200010330     C                              or vinflg = '0'
027300010330     C                              or vinflg = '2')
027400010330     C*
027500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
027600010711     C                   if        vinflg = *blanks or vinflg = '0'
027700010711     C                   clear                   vinmsg
027800010711     C                   endif
027900010601     C*
028000010330     C                   exsr      inzvar
028100010605     C                   exsr      impvab
028200010601     C*
028300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
028400010605     C                   eval      depfil = VABLNP
028500010601     C                   exsr      repfil
028600010601     C                   if        depfil = invfil
028700021025     C                   if        vlrpoi = 999
028800030822     C                   move(P)   invfil        VABFGS
028900040119     C                   move(P)   invfil        VATFGS
029000021025     C                   else
029100030822     C                   move(P)   vlrpoi        VABFGS
029200040119     C                   move(P)   vlrpoi        VATFGS
029300021025     C                   endif
029400010601     C*
029500010601     C                   exsr      PREELA
029600010601     C*
029700010604     C* Ebbene...
029800010604     C*
029900040119     C  N31              add       1             §CTROKVB
030000040119     C   32              add       1             §CTRMO
030100040119     C   31              add       1             §CTRNO
030200030822     C                   if        wGiro = 2
030300060112     C                   if        depspe =  *zeros OR
030400060112     C                             depspe <> vabNSP
030500040119     C  N31              exsr      wrivab
030600060112     C  N31              eval      depspe = vabNSP
030700060112     C                   endif
030800040119     C                   endif
030900010604     C*
031000010604     C                   if        *in31 = *off and
031100010604     C                             *in32 = *off
031200010604     C                   eval      vinflg = '1'
031300010604     C                   else
031400010604     C                   eval      recko = vindta
031500010604     C                   except    rigadett
031600010604     C                   eval      vinflg = '2'
031700010604     C                   endif
031800010604     C                   endif
031900010601     C*
032000010604     C                   endif
032100010604     C*
032200010330     C                   else
032300010330     C                   eval      vinflg = '1'
032400010330     C                   endif
032500010601     C*
032600010601     C  N70              update    tivin000
032700010330     C*
032800030822     C  N70              enddo
032900040119     C*
033000040119     C                   endif                                                  (endif - chkcall)
033100010601     C*
033200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
033300010601     C                   if        cntNonEl = *zeros or
033400010601     C                             flgMulti = '0'
033500010330     C* Se non ci sono record con errori ...
033600010601     C                   if        §ctrno = 0 and
033700010604     C                             §ctrmo = 0 and
033800010601     C                             flgStato <> '2'
033900010330     C* ... restituisco esito OK.
034000010330     C                   eval      wrkesito = '0'
034100010330     C                   else
034200040119     C                   if        §ctrokvb > 0
034300010330     C                   eval      wrkesito = '1'
034400010330     C                   else
034500010615     C                   if        flgOk = '0'
034600010615     C                   eval      wrkesito = '2'
034700010615     C                   else
034800010615     C                   eval      wrkesito = '6'
034900010615     C                   endif
035000010330     C                   endif
035100010330     C                   endif
035200010601     C                   else
035300010601     C                   eval      wrkesito = '9'
035400010601     C                   endif
035500010330     C*
035600010330     C                   if        %open(tivin00r)
035700010330     C                   close     tivin00r
035800010330     C                   endif
035900021025     C                   if        %open(fivabwwr)
036000021025     C                   close     fivabwwr
036100010330     C                   endif
036200040512     C                   if        %open(fivatwwr)
036300040512     C                   close     fivatwwr
036400040512     C                   endif
036500010601     C*
036600010601     C                   if        vlrpoi <> 999
036700010601     C                   eval      invfil = vlrpoi
036800010601     C                   endif
036900010330     C*
037000040119     C                   if        §ctrokvb > 0
037100010601     C                             and invfil > *zeros
037200010330     C                   exsr      invio
037300010330     C                   endif
037400010601     C*
037500010618     C                   if        flgGiro = '1'
037600010601     C                   exsr      endela
037700010618     C                   endif
037800010330     C*
037900010330     C                   ENDSR
038000010330     C***
038100040119
038200040119
038300040119
038400040119     C*----------------------------------------------------*
038500040119     C*  SCARICAMENTO BUFFER RECORDS VAB
038600040119     C*----------------------------------------------------*
038700040119     C     WRIVAB        BEGSR
038800040119     C*
038900040119     C                   write     fivab000                                     => scarico il VAB
039000040119     C*
039100040119     C* Eseguo la scrittura dei "CHI SONO" della spedizione corrente
039200040119     C                   exsr      wrivat
039300040119     C*
039400040119     C                   ENDSR
039500040119
039600040119
039700040119
039800040119     C*----------------------------------------------------*
039900040119     C*  SCARICAMENTO BUFFER RECORDS VAT
040000040119     C*----------------------------------------------------*
040100040706     C     WRIVAT        BEGSR
040200040706     C*
040300040719     C* Attribuisco la stessa chiave della testata bolle
040400040719     C                   EVAL      VATAAS = VABAAS
040500040719     C                   EVAL      VATLNP = VABLNP
040600040719     C                   EVAL      VATNSP = VABNSP
040700040719     C*
040800060112     C* Imposto il tipo record sul file VAT a 'B' x indicare il telefono destinatario
040900051108     C                   EVAL      VATTRC = 'B'
041000051108     C                   EVAL      VATNOT = %trim(wVATNOT_B)
041100040719     C*
041200040719     C* Quindi se valorizzato il VATNOT scarico il buffer e incremento il contatore x il CMZ
041300040719     C                   IF        VATNOT <> *blanks
041400040706     C                   WRITE     FIVAT000
041500040706     C                   ADD       1             §CTROKVT
041600040719     C                   ENDIF
041700051108     C*
041800051108     C* Imposto il tipo record sul file VAT a 'A' x indicare il referente
041900051108     C                   EVAL      VATTRC = 'A'
042000051108     C                   EVAL      VATNOT = %trim(wVATNOT_A)
042100051108     C*
042200051108     C* Quindi se valorizzato il VATNOT scarico il buffer e incremento il contatore x il CMZ
042300051108     C                   IF        VATNOT <> *blanks
042400051108     C                   WRITE     FIVAT000
042500051108     C                   ADD       1             §CTROKVT
042600051108     C                   ENDIF
042700040119     C*
042800040119     C                   ENDSR
042900040119
043000040119
043100040119
043200040119     C*----------------------------------------------------*
043300040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043400040119     C*----------------------------------------------------*
043500040119     C     PREVAT        BEGSR
043600040119     C*
043700040119     C* Compongo il nome del membro da dare al FIVATWWR
043800040119     C                   eval      parmbr = vlrhdl
043900040119     C                   movel     'M'           parmbr
044000060112     C                   eval      parccm = vlrksc
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
045300040119     C*
045400040119     C                   ENDSR
045500010601
045600010601
045700010601
045800010330     C*----------------------------------------------------*
045900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
046000010330     C*----------------------------------------------------*
046100040119     C     INZVAR        BEGSR
046200010330     C*
046300030822     C* Inizializzo variabili di wrk
046400020204     C                   Z-ADD     *zeros        Num5_0            5 0
046500060112     C                   CLEAR                   wVATNOT_A
046600060112     C                   CLEAR                   wVATNOT_B
046700060112     C                   CLEAR                   wVABRMN_Alfa
046800060112     C                   CLEAR                   wCELL
046900060112     C                   CLEAR                   wCONTRASS
047000060112     C*
047100060112     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
047200060112     C                   MOVEL     '0'           FlgCAS            1
047300030822     C*
047400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
047500030822     C                   CLEAR                   FIVAB000
047600040119     C                   CLEAR                   FIVAT000
047700030822     C                   CLEAR                   SkSplitCSV
047800030822     C                   EVAL      i = 1
047900030822     C                   EVAL      posDa = *zeros
048000030822     C                   EVAL      posA  = *zeros
048100030822     C*
048200030822     C* Reimposto i valori di default
048300030822     C                   EXSR      DEFCAM
048400030822     C*
048500010330     C                   ENDSR
048600010330     C*----------------------------------------------------*
048700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
048800010330     C*----------------------------------------------------*
048900010330     C     DEFCAM        BEGSR
049000040119     C*
049100040119     C* Imposto i valori d default a programma dei campi fondamentali
049200060112     C                   MOVE(P)   vlrksc        VABCCM
049300060112     C                   MOVE(P)   vlrksc        VATCCM
049400060112     C                   EVAL      VABLNP = 120
049500060112     C                   EVAL      VATLNP = 120
049600060112     C                   EVAL      VABCTR = 333
049700040119     C                   EVAL      VABCBO = '1'
049800060112     C                   EVAL      VABNCL = 1
049900060112     C                   EVAL      VABPKB = 0,1
050000030822     C*
050100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050200030715     C* e delimitatore testo.
050300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050600030822     C*
050700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
050800030715     C                   IF        %subst(vlrppt:1:1) = 'S'
050900030715     C                   SETON                                        50
051000030715     C                   ELSE
051100030715     C                   SETOFF                                       50
051200030715     C                   ENDIF
051300030822     C*
051400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
051500030822     C                   EVAL      posDaDft = 1
051600030822     C                   EVAL      posADft  = 0
051700030822     C                   EVAL      wGiroDft = 0
051800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
051900030822     C                             posDaDft > 0
052000030822     C*
052100030822     C* Gestisco il 1° giro
052200030822     C                   IF        wGiroDft = 0
052300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
052400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
052500030822     C* Incremento il contatore dei "giri"
052600030822     C                   EVAL      wGiroDft = 1
052700030822     C                   ELSE
052800030822     C                   EVAL      posDaDft = posADft
052900030822     C                   ENDIF
053000030822     C* Eseguo lo scan x trovare la fine del campo corrente
053100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
053200030822     C*
053300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
053400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
053500030822     C                   IF        posDaDft > 0 AND
053600030822     C                             posADft  > 0
053700030822     C* NCL
053800030822     C                   IF        %subst(
053900030822     C                             %subst(vlrppt:posDaDft+1:
054000030822     C                             posADft-posDaDft-1):1:3)
054100030822     C                             = 'NCL'
054200030822     C                   EVAL      PiStr=%trim(%subst(
054300030822     C                             %subst(vlrppt:posDaDft+1:
054400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054500030822     C                   EXSR      CHKNUM
054600030822     C                   IF        PiInt=*on
054700030822     C                   Z-ADD     PiVal         VABNCL
054800030822     C                   ENDIF
054900030822     C                   ENDIF
055000030822     C* CCM
055100030822     C                   IF        %subst(
055200030822     C                             %subst(vlrppt:posDaDft+1:
055300030822     C                             posADft-posDaDft-1):1:3)
055400030822     C                             = 'CCM'
055500030822     C                   EVAL      PiStr=%trim(%subst(
055600030822     C                             %subst(vlrppt:posDaDft+1:
055700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055800030822     C                   EXSR      CHKNUM
055900030822     C                   IF        PiInt=*on
056000030822     C                   Z-ADD     PiVal         VABCCM
056100040119     C                   Z-ADD     PiVal         VATCCM
056200030822     C                   ENDIF
056300030822     C                   ENDIF
056400030822     C* LNP
056500030822     C                   IF        %subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):1:3)
056800030822     C                             = 'LNP'
056900030822     C                   EVAL      PiStr=%trim(%subst(
057000030822     C                             %subst(vlrppt:posDaDft+1:
057100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057200030822     C                   EXSR      CHKNUM
057300030822     C                   IF        PiInt=*on
057400030822     C                   Z-ADD     PiVal         VABLNP
057500040119     C                   Z-ADD     PiVal         VATLNP
057600030822     C                   ENDIF
057700030822     C                   ENDIF
057800030822     C* NRS
057900030822     C                   IF        %subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):1:3)
058200030822     C                             = 'NRS'
058300030822     C                   EVAL      PiStr=%trim(%subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058600030822     C                   EXSR      CHKNUM
058700030822     C                   IF        PiInt=*on
058800030822     C                   Z-ADD     PiVal         VABNRS
058900030822     C                   ENDIF
059000030822     C                   ENDIF
059100030822     C* CTR
059200030822     C                   IF        %subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):1:3)
059500030822     C                             = 'CTR'
059600030822     C                   EVAL      PiStr=%trim(%subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059900030822     C                   EXSR      CHKNUM
060000030822     C                   IF        PiInt=*on
060100030822     C                   Z-ADD     PiVal         VABCTR
060200030822     C                   ENDIF
060300030822     C                   ENDIF
060400030822     C* PKB
060500030822     C                   IF        %subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):1:3)
060800030822     C                             = 'PKB'
060900030822     C                   EVAL      PiStr=%trim(%subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061200030822     C                   EXSR      CHKNUM
061300030822     C                   IF        PiNum=*on
061400030822     C                   Z-ADD     PiVal         VABPKB
061500030822     C                   ENDIF
061600030822     C                   ENDIF
061700030822     C* VLB
061800030822     C                   IF        %subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):1:3)
062100030822     C                             = 'VLB'
062200030822     C                   EVAL      PiStr=%trim(%subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062500030822     C                   EXSR      CHKNUM
062600030822     C                   IF        PiNum=*on
062700030822     C                   Z-ADD     PiVal         VABVLB
062800030822     C                   ENDIF
062900030822     C                   ENDIF
063000030822     C* QFT
063100030822     C                   IF        %subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):1:3)
063400030822     C                             = 'QFT'
063500030822     C                   EVAL      PiStr=%trim(%subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800030822     C                   EXSR      CHKNUM
063900030822     C                   IF        PiNum=*on
064000030822     C                   Z-ADD     PiVal         VABQFT
064100030822     C                   ENDIF
064200030822     C                   ENDIF
064300030822     C* CBO
064400030822     C                   IF        %subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):1:3)
064700030822     C                             = 'CBO'
064800030822     C                   EVAL      VABCBO=%trim(%subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065100030822     C                   ENDIF
065200030822     C* TSP
065300030822     C                   IF        %subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):1:3)
065600030822     C                             = 'TSP'
065700030822     C                   EVAL      VABTSP=%trim(%subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066000030822     C                   ENDIF
066100030822     C* VAS
066200030822     C                   IF        %subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):1:3)
066500030822     C                             = 'VAS'
066600030822     C                   EVAL      VABVAS=%trim(%subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066900030822     C                   ENDIF
067000030822     C* VCA
067100030822     C                   IF        %subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):1:3)
067400030822     C                             = 'VCA'
067500030822     C                   EVAL      VABVCA=%trim(%subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067800030822     C                   ENDIF
067900030822     C* GCA
068000030822     C                   IF        %subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):1:3)
068300030822     C                             = 'GCA'
068400030822     C                   EVAL      VABGCA=%trim(%subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700030822     C                   ENDIF
068800030822     C* CTM
068900030822     C                   IF        %subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):1:3)
069200030822     C                             = 'CTM'
069300030822     C                   EVAL      VABCTM=%trim(%subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069600030822     C                   ENDIF
069700030822     C* FFD
069800030822     C                   IF        %subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):1:3)
070100030822     C                             = 'FFD'
070200030822     C                   EVAL      VABFFD=%trim(%subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070500030822     C                   ENDIF
070600030822     C* VAD
070700030822     C                   IF        %subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):1:3)
071000030822     C                             = 'VAD'
071100030822     C                   EVAL      VABVAD=%trim(%subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071400030822     C                   ENDIF
071500030822     C* GMA
071600030822     C                   IF        %subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):1:3)
071900030822     C                             = 'GMA'
072000030822     C                   EVAL      VABGMA=%trim(%subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072300030822     C                   ENDIF
072400030822     C* GGA
072500030822     C                   IF        %subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):1:3)
072800030822     C                             = 'GGA'
072900030822     C                   EVAL      VABGGA=%trim(%subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073200030822     C                   ENDIF
073300030822     C* GVA
073400030822     C                   IF        %subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):1:3)
073700030822     C                             = 'GVA'
073800030822     C                   EVAL      VABGVA=%trim(%subst(
073900030822     C                             %subst(vlrppt:posDaDft+1:
074000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074100030822     C                   ENDIF
074200030822     C* TC1
074300030822     C                   IF        %subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):1:3)
074600030822     C                             = 'TC1'
074700030822     C                   EVAL      VABTC1=%trim(%subst(
074800030822     C                             %subst(vlrppt:posDaDft+1:
074900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075000030822     C                   ENDIF
075100030822     C* TC2
075200030822     C                   IF        %subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):1:3)
075500030822     C                             = 'TC2'
075600030822     C                   EVAL      VABTC2=%trim(%subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075900030822     C                   ENDIF
076000030822     C* ...
076100030822     C                   ENDIF
076200030822     C                   ENDDO
076300020204     C*
076400010330     C                   ENDSR
076500010607     C*----------------------------------------------------*
076600021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
076700010607     C*----------------------------------------------------*
076800010607     C     IMPVAB        BEGSR
076900010607     C*
077000010607     C                   SETOFF                                       3132
077100030515     C*
077200010607     C                   MOVEL     datcor        VABAAS
077300040119     C                   MOVEL     datcor        VATAAS
077400010607     C                   MOVE      datcor        VABMGS
077500010607     C*
077600030715     C**********
077700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
077800030715     C**********
077900030715     C                   IF        wGiro = *zeros
078000040706     C*
078100040706     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
078200040706     C                   EVAL      depvinDTA = vinDTA
078300040113     C*
078400040113     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
078500040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
078600040113     C                                      %trim(intcol(3))
078700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
078800031201     C     minu:maiu     XLATE     vindta        vindta
078900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
079000030715     C                   DOW       posDa <= %len(%trim(vindta))
079100030715     C*
079200030715     C* Gestisco il 1° campo
079300030715     C                   IF        i = 1
079400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
079500030715     C                   EVAL      posDa = 1
079600030715     C* Eseguo lo scan x trovare la fine del primo campo
079700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079800030715     C* A questo punto "estraggo" il campo corrente
079900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080000030715     C                                                    (posA-posDa))
080100030715     C* X i campi successivi al 1°
080200030715     C                   ELSE
080300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
080400030715     C                   EVAL      posDa = posA + 1
080500030715     C* Eseguo lo scan x trovare la fine del campo corrente
080600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
080700030715     C* Gestisco l'ultimo campo
080800030715     C                   IF        posA = *zeros
080900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
081000030715     C* A questo punto "estraggo" il campo corrente
081100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081200030715     C                                                    (posA-posDa))
081300030715     C                   LEAVE
081400030715     C                   ELSE
081500030715     C* A questo punto "estraggo" il campo corrente
081600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081700030715     C                                                    (posA-posDa))
081800030715     C                   ENDIF
081900030715     C                   ENDIF
082000030715     C* Incremento il contatore d campo
082100030715     C                   EVAL      i = i +1
082200030715     C                   ENDDO
082300030715     C*
082400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
082500030715     C* delimitazione testo
082600030715     C                   EVAL      i = 1
082700030715     C                   DOW       i < %elem(SkSplitFLD)
082800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
082900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
083000030715     C                   EVAL      i = i + 1
083100030715     C                   ENDDO
083200030715     C                   EVAL      wGiro = 1
083300040706     C* Ripristino la 1° riga originale
083400040706     C                   EVAL      vinDTA = depvinDTA
083500030715     C                   ELSE
083600030715     C                   EVAL      wGiro = 2
083700031204     C**********
083800031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
083900031204     C* d separatore campo
084000031204     C**********
084100031204     C                   EVAL      vindta = %trim(vindta)
084200031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
084300031204     C                   EVAL      vindta = %subst(vindta:2)
084400031204     C                   ENDDO
084500031204     C*
084600031204     C                   Z-ADD     *zeros        lunghInput        4 0
084700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
084800031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
084900031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
085000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
085100031204     C                   ENDDO
085200030715     C**********
085300030715     C* Effettuo lo split del campo dati d input
085400030715     C**********
085500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085600030715     C                   DOW       posDa <= %len(%trim(vindta))
085700030715     C*
085800030715     C* Gestisco il 1° campo
085900030715     C                   IF        i = 1
086000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
086100030715     C                   EVAL      posDa = 1
086200030715     C* Eseguo lo scan x trovare la fine del primo campo
086300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086400030715     C* A questo punto "estraggo" il campo corrente
086500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
086600030715     C                                                    (posA-posDa))
086700030715     C* X i campi successivi al 1°
086800030715     C                   ELSE
086900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
087000030715     C                   EVAL      posDa = posA + 1
087100031204     C* Verifico che nn vi sia il campo nn valorizzato
087200031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
087300031204     C* Se campo nn valorizzato skippo al prossimo
087400031204     C                   EVAL      SkSplitCSV(i) = *blanks
087500031204     C                   EVAL      posA  = posA + 1
087600031204     C                   ELSE
087700030715     C* Eseguo lo scan x trovare la fine del campo corrente
087800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087900030715     C* Gestisco l'ultimo campo
088000030715     C                   IF        posA = *zeros
088100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
088200030715     C* A questo punto "estraggo" il campo corrente
088300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088400030715     C                                                    (posA-posDa))
088500030715     C                   LEAVE
088600030715     C                   ELSE
088700030715     C* A questo punto "estraggo" il campo corrente
088800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088900030715     C                                                    (posA-posDa))
089000030715     C                   ENDIF
089100031204     C                   ENDIF
089200030715     C                   ENDIF
089300030715     C* Incremento il contatore d campo
089400030715     C                   EVAL      i = i +1
089500030715     C                   ENDDO
089600030715     C*
089700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089800030715     C* delimitazione testo
089900030715     C                   EVAL      i = 1
090000030715     C                   DOW       i < %elem(SkSplitCSV)
090100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
090200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
090300030715     C                   EVAL      i = i + 1
090400030715     C                   ENDDO
090500030715     C*
090600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
090700030715     C                   EVAL      i = 1
090800030715     C                   DOW       i < %elem(SkSplitFLD)
090900030715     C*
091000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
091100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
091200030715     C                             SkSplitCSV(i) <> *blanks
091300030721     C*** ATB
091400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
091500030721     C                   EVAL      VABATB = SkSplitCSV(i)
091600030721     C                   ENDIF
091700030715     C*** RSD
091800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
091900030715     C                   EVAL      VABRSD = SkSplitCSV(i)
092000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
092100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
092200030715     C* ==
092300030715     C                   ENDIF
092400030721     C*** RD2
092500060112     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
092600030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
092700030721     C                   ENDIF
092800030715     C*** IND
092900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
093000030715     C                   EVAL      VABIND = SkSplitCSV(i)
093100030715     C                   ENDIF
093200030715     C*** LOD
093300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
093400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
093500030715     C                   ENDIF
093600030715     C*** PRD
093700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
093800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
093900030715     C                   ENDIF
094000030721     C*** NZD
094100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
094200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
094300030721     C                   ENDIF
094400030715     C*** RMA
094500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
094600030715     C                   EVAL      VABRMA = SkSplitCSV(i)
094700030715     C                   ENDIF
094800030715     C*** NOT
094900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
095000030715     C                   EVAL      VABNOT = SkSplitCSV(i)
095100030715     C                   ENDIF
095200030715     C*** NT2
095300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
095400030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
095500030715     C                   ENDIF
095600030715     C*** VCA
095700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
095800030715     C                   EVAL      VABVCA = SkSplitCSV(i)
095900030715     C                   ENDIF
096000030715     C*** VAS
096100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
096200030715     C                   EVAL      VABVAS = SkSplitCSV(i)
096300030715     C                   ENDIF
096400030715     C*** FFD
096500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
096600030715     C                   EVAL      VABFFD = SkSplitCSV(i)
096700030715     C                   ENDIF
096800030721     C*** GC1
096900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
097000030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
097100030721     C                   ENDIF
097200030721     C*** GC2
097300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
097400030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
097500030721     C                   ENDIF
097600030721     C*** TSP
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
097800030721     C                   EVAL      VABTSP = SkSplitCSV(i)
097900030721     C                   ENDIF
098000030721     C*** NAS
098100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
098200030721     C                   EVAL      VABNAS = SkSplitCSV(i)
098300030721     C                   ENDIF
098400030721     C*** TIC
098500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
098600030721     C                   EVAL      VABTIC = SkSplitCSV(i)
098700030721     C                   ENDIF
098800030721     C*** GCA
098900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
099000030721     C                   EVAL      VABGCA = SkSplitCSV(i)
099100030721     C                   ENDIF
099200030721     C*** XCO
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
099400030721     C                   EVAL      VABXCO = SkSplitCSV(i)
099500030721     C                   ENDIF
099600030721     C*** CTM
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
099800030721     C                   EVAL      VABCTM = SkSplitCSV(i)
099900030721     C                   ENDIF
100000030721     C*** TCR
100100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
100200030721     C                   EVAL      VABTCR = SkSplitCSV(i)
100300030721     C                   ENDIF
100400030721     C*** CTS
100500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
100600030721     C                   EVAL      VABCTS = SkSplitCSV(i)
100700030721     C                   ENDIF
100800030721     C*** FTM
100900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
101000030721     C                   EVAL      VABFTM = SkSplitCSV(i)
101100030721     C                   ENDIF
101200030721     C*** VAD
101300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
101400030721     C                   EVAL      VABVAD = SkSplitCSV(i)
101500030721     C                   ENDIF
101600030721     C*** GMA
101700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
101800030721     C                   EVAL      VABGMA = SkSplitCSV(i)
101900030721     C                   ENDIF
102000030721     C*** GGA
102100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
102200030721     C                   EVAL      VABGGA = SkSplitCSV(i)
102300030721     C                   ENDIF
102400030721     C*** GVA
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
102600030721     C                   EVAL      VABGVA = SkSplitCSV(i)
102700030721     C                   ENDIF
102800030721     C*** TC1
102900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
103000030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
103100030721     C                   ENDIF
103200030721     C*** TC2
103300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
103400030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
103500030721     C                   ENDIF
103600030721     C*** SCL
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
103800030721     C                   EVAL      VABSCL = SkSplitCSV(i)
103900030721     C                   ENDIF
104000030721     C*** RMO
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
104200030721     C                   EVAL      VABRMO = SkSplitCSV(i)
104300030721     C                   ENDIF
104400030721     C*** NMO
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
104600030721     C                   EVAL      VABNMO = SkSplitCSV(i)
104700030721     C                   ENDIF
104800040719     C*** CBO
104900040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
105000060112     C                   EVAL      VABCBO = SkSplitCSV(i)
105100040719     C                   ENDIF
105200030721     C*
105300040706     C* Reperisco quindi i campi numerici...
105400040706     C*** NSP
105500040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
105600040706     C                   EVAL      PiStr=SkSplitCSV(i)
105700040706     C                   EXSR      CHKNUM
105800040706     C                   IF        PiInt=*on
105900040706     C                   Z-ADD     PiVal         VABNSP
106000040706     C                   ELSE
106100040706     C                   SETON                                        31
106200040706     C                   EVAL      VABNSP = *zeros
106300040706     C                   EVAL      vinmsg = %trimr(vinmsg)
106400060112     C                             + ' ' + 'VABNSP'
106500040706     C                   ENDIF
106600040706     C                   ENDIF
106700040706     C*** RMN
106800040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
106900040706     C                   EVAL      PiStr=SkSplitCSV(i)
107000040706     C                   EXSR      CHKNUM
107100040706     C                   IF        PiInt=*on
107200040706     C                   Z-ADD     PiVal         VABRMN
107300040706     C                   ELSE
107400040706     C                   SETON                                        32
107500040706     C                   EVAL      VABRMN = 1
107600040706     C                   EVAL      vinmsg = %trimr(vinmsg)
107700040706     C                             + ' ' + 'VABRMN'
107800040706     C                   ENDIF
107900040706     C                   ENDIF
108000030721     C*** CAD
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
108200030721     C                   EVAL      PiStr=SkSplitCSV(i)
108300030721     C                   EXSR      CHKNUM
108400030721     C                   IF        PiInt=*on
108500030721     C                   Z-ADD     PiVal         Num5_0
108600030721     C                   MOVEL(p)  Num5_0        VABCAD
108700030721     C                   ELSE
108800030721     C                   SETON                                        32
108900030721     C                   EVAL      VABCAD = *zeros
109000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109100030721     C                             + ' ' + 'VABCAD'
109200030721     C                   ENDIF
109300030721     C                   ENDIF
109400030721     C*** NCL
109500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
109600030721     C                   EVAL      PiStr=SkSplitCSV(i)
109700030721     C                   EXSR      CHKNUM
109800030721     C                   IF        PiInt=*on
109900030721     C                   Z-ADD     PiVal         VABNCL
110000030721     C                   ELSE
110100030721     C                   SETON                                        32
110200030721     C                   EVAL      VABNCL = *zeros
110300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110400040706     C                             + ' ' + 'VABNCL'
110500030721     C                   ENDIF
110600030721     C                   ENDIF
110700030721     C*** PKB
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
110900030721     C                   EVAL      PiStr=SkSplitCSV(i)
111000030721     C                   EXSR      CHKNUM
111100030721     C                   IF        PiNum=*on
111200030721     C                   Z-ADD     PiVal         VABPKB
111300030721     C                   ELSE
111400030721     C                   SETON                                        32
111500030721     C                   EVAL      VABPKB = *zeros
111600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111700030721     C                             + ' ' + 'VABPKB'
111800030721     C                   ENDIF
111900030721     C                   ENDIF
112000030721     C*** CAS
112100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
112200030721     C                   EVAL      PiStr=SkSplitCSV(i)
112300030721     C                   EXSR      CHKNUM
112400030721     C                   IF        PiNum=*on
112500030721     C                   Z-ADD     PiVal         VABCAS
112600030721     C                   ELSE
112700030721     C                   SETON                                        32
112800030721     C                   EVAL      VABCAS = *zeros
112900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113000030721     C                             + ' ' + 'VABCAS'
113100030721     C                   ENDIF
113200030721     C                   ENDIF
113300030721     C*** CCM
113400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
113500030721     C                   EVAL      PiStr=SkSplitCSV(i)
113600030721     C                   EXSR      CHKNUM
113700030721     C                   IF        PiInt=*on      AND
113800030721     C                             PiVal<=9999999 AND
113900030721     C                             PiVal>*zeros
114000030721     C                   Z-ADD     PiVal         VABCCM
114100040119     C                   Z-ADD     PiVal         VATCCM
114200030721     C                   ELSE
114300030721     C                   SETON                                        32
114400030721     C                   EVAL      VABCCM = *zeros
114500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114600040119     C                             + ' ' + 'VABCCM VATCCM'
114700030721     C                   ENDIF
114800030721     C                   ENDIF
114900030721     C*** LNP
115000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
115100030721     C                   EVAL      PiStr=SkSplitCSV(i)
115200030721     C                   EXSR      CHKNUM
115300030721     C                   IF        PiInt=*on    AND
115400030721     C                             PiVal<=999   AND
115500030721     C                             PiVal>*zeros
115600030721     C                   Z-ADD     PiVal         VABLNP
115700040119     C                   Z-ADD     PiVal         VATLNP
115800030721     C                   ELSE
115900030721     C                   SETON                                        32
116000030721     C                   EVAL      VABLNP = *zeros
116100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116200040119     C                             + ' ' + 'VABLNP VATLNP'
116300030721     C                   ENDIF
116400030721     C                   ENDIF
116500030721     C*** NRS
116600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
116700030721     C                   EVAL      PiStr=SkSplitCSV(i)
116800030721     C                   EXSR      CHKNUM
116900030721     C                   IF        PiInt=*on AND
117000030721     C                             PiVal<=99
117100030721     C                   Z-ADD     PiVal         VABNRS
117200030721     C                   ELSE
117300030721     C                   SETON                                        32
117400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117500030721     C                             + ' ' + 'VABNRS'
117600030721     C                   ENDIF
117700030721     C                   ENDIF
117800030721     C*** LNA
117900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
118000030721     C                   EVAL      PiStr=SkSplitCSV(i)
118100030721     C                   EXSR      CHKNUM
118200030721     C                   IF        PiInt=*on    AND
118300030721     C                             PiVal<=999   AND
118400030721     C                             PiVal>*zeros
118500030721     C                   Z-ADD     PiVal         VABLNA
118600030721     C                   ELSE
118700030721     C                   SETON                                        32
118800030721     C                   EVAL      VABLNA = *zeros
118900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119000030721     C                             + ' ' + 'VABLNA'
119100030721     C                   ENDIF
119200030721     C                   ENDIF
119300030721     C*** CTR
119400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
119500030721     C                   EVAL      PiStr=SkSplitCSV(i)
119600030721     C                   EXSR      CHKNUM
119700030721     C                   IF        PiInt=*on  AND
119800030721     C                             PiVal<=999
119900030721     C                   Z-ADD     PiVal         VABCTR
120000030721     C                   ELSE
120100030721     C                   SETON                                        32
120200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120300030721     C                             + ' ' + 'VABCTR'
120400030721     C                   ENDIF
120500030721     C                   ENDIF
120600030721     C*** IAS
120700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
120800030721     C                   EVAL      PiStr=SkSplitCSV(i)
120900030721     C                   EXSR      CHKNUM
121000030721     C                   IF        PiNum=*on
121100030721     C                   Z-ADD     PiVal         VABIAS
121200030721     C                   ELSE
121300030721     C                   SETON                                        32
121400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121500030721     C                             + ' ' + 'VABIAS'
121600030721     C                   ENDIF
121700030721     C                   ENDIF
121800030721     C*** VLB
121900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
122000030721     C                   EVAL      PiStr=SkSplitCSV(i)
122100030721     C                   EXSR      CHKNUM
122200030721     C                   IF        PiNum=*on
122300030721     C                   Z-ADD     PiVal         VABVLB
122400030721     C                   ELSE
122500030721     C                   SETON                                        32
122600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122700030721     C                             + ' ' + 'VABVLB'
122800030721     C                   ENDIF
122900030721     C                   ENDIF
123000030721     C*** QFT
123100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
123200030721     C                   EVAL      PiStr=SkSplitCSV(i)
123300030721     C                   EXSR      CHKNUM
123400030721     C                   IF        PiNum=*on
123500030721     C                   Z-ADD     PiVal         VABQFT
123600030721     C                   ELSE
123700030721     C                   SETON                                        32
123800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123900030721     C                             + ' ' + 'VABQFT'
124000030721     C                   ENDIF
124100030721     C                   ENDIF
124200030721     C*** NCD
124300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
124400030721     C                   EVAL      PiStr=SkSplitCSV(i)
124500030721     C                   EXSR      CHKNUM
124600030721     C                   IF        PiInt=*on      AND
124700030721     C                             PiVal<=9999999
124800030721     C                   Z-ADD     PiVal         VABNCD
124900030721     C                   ELSE
125000030721     C                   SETON                                        32
125100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125200030721     C                             + ' ' + 'VABNCD'
125300030721     C                   ENDIF
125400030721     C                   ENDIF
125500030721     C*** NCA
125600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
125700030721     C                   EVAL      PiStr=SkSplitCSV(i)
125800030721     C                   EXSR      CHKNUM
125900030721     C                   IF        PiInt=*on      AND
126000030721     C                             PiVal<=9999999
126100030721     C                   Z-ADD     PiVal         VABNCA
126200030721     C                   ELSE
126300030721     C                   SETON                                        32
126400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126500030721     C                             + ' ' + 'VABNCA'
126600030721     C                   ENDIF
126700030721     C                   ENDIF
126800030721     C*** ZNC
126900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
127000030721     C                   EVAL      PiStr=SkSplitCSV(i)
127100030721     C                   EXSR      CHKNUM
127200030721     C                   IF        PiInt=*on AND
127300030721     C                             PiVal<=99
127400030721     C                   Z-ADD     PiVal         VABZNC
127500030721     C                   ELSE
127600030721     C                   SETON                                        32
127700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127800030721     C                             + ' ' + 'VABZNC'
127900030721     C                   ENDIF
128000030721     C                   ENDIF
128100030721     C*** DCR
128200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
128300030721     C                   EVAL      PiStr=SkSplitCSV(i)
128400030721     C                   EXSR      CHKNUM
128500030721     C                   IF        PiInt=*on       AND
128600030721     C                             PiVal<=99999999
128700030721     C                   Z-ADD     PiVal         VABDCR
128800030721     C                   ELSE
128900030721     C                   SETON                                        32
129000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129100030721     C                             + ' ' + 'VABDCR'
129200030721     C                   ENDIF
129300030721     C                   ENDIF
129400030721     C*** HCR
129500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
129600030721     C                   EVAL      PiStr=SkSplitCSV(i)
129700030721     C                   EXSR      CHKNUM
129800030721     C                   IF        PiInt=*on   AND
129900030721     C                             PiVal<=9999
130000030721     C                   Z-ADD     PiVal         VABHCR
130100030721     C                   ELSE
130200030721     C                   SETON                                        32
130300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130400030721     C                             + ' ' + 'VABHCR'
130500030721     C                   ENDIF
130600030721     C                   ENDIF
130700030721     C*** VMD
130800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
130900030721     C                   EVAL      PiStr=SkSplitCSV(i)
131000030721     C                   EXSR      CHKNUM
131100030721     C                   IF        PiNum=*on
131200030721     C                   Z-ADD     PiVal         VABVMD
131300030721     C                   ELSE
131400030721     C                   SETON                                        32
131500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131600030721     C                             + ' ' + 'VABVMD'
131700030721     C                   ENDIF
131800030721     C                   ENDIF
131900030721     C*** ANT
132000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
132100030721     C                   EVAL      PiStr=SkSplitCSV(i)
132200030721     C                   EXSR      CHKNUM
132300030721     C                   IF        PiInt=*on        AND
132400030721     C                             PiVal<=999999999
132500030721     C                   Z-ADD     PiVal         VABANT
132600030721     C                   ELSE
132700030721     C                   SETON                                        32
132800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132900030721     C                             + ' ' + 'VABANT'
133000030721     C                   ENDIF
133100030721     C                   ENDIF
133200030721     C*** CMO
133300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
133400030721     C                   EVAL      PiStr=SkSplitCSV(i)
133500030721     C                   EXSR      CHKNUM
133600030721     C                   IF        PiInt=*on
133700030721     C                   Z-ADD     PiVal         Num5_0
133800030721     C                   MOVEL(p)  Num5_0        VABCMO
133900030721     C                   ELSE
134000030721     C                   SETON                                        32
134100030721     C                   EVAL      VABCMO = *zeros
134200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134300030721     C                             + ' ' + 'VABCMO'
134400030721     C                   ENDIF
134500030721     C                   ENDIF
134600040706     C*
134700060112     C*** CELL
134800060112     C                   IF        %trim(SkSplitFLD(i)) = 'CELL'
134900060112     C                   EVAL      wCELL = SkSplitCSV(i)
135000060112     C                   ENDIF
135100051108     C*** VATNOT TRC=A
135200060112     C                   IF        %trim(SkSplitFLD(i)) = 'TRC_A'
135300051108     C                   EVAL      wVATNOT_A = SkSplitCSV(i)
135400040719     C                   ENDIF
135500051108     C*** VATNOT TRC=B
135600051108     C                   IF        %trim(SkSplitFLD(i)) = 'TRC_B'
135700051108     C                   EVAL      wVATNOT_B = SkSplitCSV(i)
135800051108     C                   ENDIF
135900060112     C*** VABNOT/VABNT2 NOTE
136000060112     C                   IF        %trim(SkSplitFLD(i)) = 'NOTE'
136100060112     C                   EVAL      VABNOT = %subst(SkSplitCSV(i):1:35)
136200060112     C                   EVAL      VABNT2 = %subst(SkSplitCSV(i):35+1:35)
136300060112     C                   ENDIF
136400060112     C*** CONTRASS
136500060112     C                   IF        %trim(SkSplitFLD(i)) = 'CONTRASS'
136600060112     C                   EVAL      wCONTRASS = SkSplitCSV(i)
136700060112     C                   ENDIF
136800070201     C*** INFO_CORR
136900070201     C                   IF        %trim(SkSplitFLD(i)) = 'INFO_CORR'
137000061113     C                   IF        %trim(SkSplitCSV(i)) = 'APPUNTAMENTO' OR
137100061113     C                             %trim(SkSplitCSV(i)) = 'appuntamento' OR
137200061113     C                             %trim(SkSplitCSV(i)) = 'Appuntamento'
137300061113     C                   EVAL      VABTC1 = 'A'
137400061113     C                   ENDIF
137500061113     C                   ENDIF
137600030715     C*
137700030715     C                   ENDIF
137800030715     C                   EVAL      i = i + 1
137900030715     C                   ENDDO
138000040706     C*
138100040706     C* Se nn valorizzata la provincia destinatario la calcolo tramite TISI95
138200040706     C* Reperisco la provincia dal CAP e dalla località
138300040706     C                   IF        VABCAD <> *blanks
138400040706     C                   CLEAR                   TISI95DS
138500040706     C                   EVAL      I95TCN = '3'
138600040706     C                   Z-ADD     datcor        I95DAT
138700040706     C                   EVAL      I95CAP = VABCAD
138800040706     C                   EVAL      I95LOC = VABLOD
138900040706     C                   CALL      'TISI95R'
139000040706     C                   PARM                    TISI95DS
139100040706     C                   EVAL      VABPRD = O95PRV
139200060117     C                   EVAL      VABLOD = O95LOC
139300040706     C                   ENDIF
139400060112     C*
139500060112     C* NSP => Stacco un numeratore da AZNUM
139600060112     C                   clear                   TRUL33DS
139700060112     C                   eval      I33OPE = *zeros
139800060112     C                   eval      I33CNU = 302
139900060112     C                   eval      I33NUM = 1
140000060112     C                   movel     TRUL33DS      KPJBU
140100060112     C                   call      'TRUL33R'
140200060112     C                   parm                    KPJBA
140300060112     C                   movel     KPJBU         TRUL33DS
140400060112     C                   if        O33ERR = *zeros
140500060112     C                   z-add     O33NRF        VABNSP
140600060112     C                   z-add     O33NRF        VATNSP
140700060112     C                   else
140800060112     C                   EVAL      vinmsg = %trimr(vinmsg)
140900060112     C                             + ' ' + 'VABNSP VATNSP'
141000060112     C                   endif
141100060112     C*
141200060112     C* RMN
141300060112     C                   EVAL      wVABRMN_Alfa = %subst(VABRMA:1:2)+
141400060112     C                                            %subst(VABRMA:4:2)+
141500060112     C                                            %subst(VABRMA:7:4)
141600060112     C                   MOVE(P)   wVABRMN_Alfa  VABRMN
141700060112     C*
141800060112     C* RD2
141900060112     C                   EVAL      VABRD2 = wVATNOT_A
142000060112     C                   IF        wCELL <> *blanks
142100060112     C                   MOVE      wCELL         VABRD2
142200060112     C                   ENDIF
142300060112     C*
142400060112     C* CAS
142500060112     C                   IF        wCONTRASS <> *blanks
142600060112     C                   MOVEL     '1'           FlgCAS
142700060112     C*
142800060112     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
142900060112     C                   EVAL      TxtInOut   = wCONTRASS
143000060112     C                   EVAL      EsitoBon   = *blanks
143100060112     C*
143200060112     C                   CALL      'XCHKCHAR'
143300060112     C                   PARM                    TxtInOut
143400060112     C                   PARM                    ElencoChar
143500060112     C                   PARM                    TipoElenco
143600060112     C                   PARM                    CharSost
143700060112     C                   PARM                    UpperCase
143800060112     C                   PARM                    ChkNull
143900060112     C                   PARM                    CharNull
144000060112     C                   PARM                    EsitoBon
144100060112     C*
144200060112     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
144300060112     C                   IF        EsitoBon = '1'
144400060112     C                   EVAL      wCONTRASS = TxtInOut
144500060112     C                   ENDIF
144600060112     C*
144700060112     C* Determino il valore numerico
144800060112     C                   EVAL      PiStr=wCONTRASS
144900060112     C                   EXSR      CHKNUM
145000060112     C                   IF        PiNum=*on
145100060112     C                   Z-ADD     PiVal         VABCAS
145200060112     C                   ELSE
145300060112     C                   Z-ADD     *zeros        VABCAS
145400060112     C                   ENDIF
145500060112     C                   ENDIF
145600040719     C*
145700040719     C* Se NSP nn valorizzato considero errore bloccante in traduzione
145800060112     C                   IF        VABNSP = *zeros
145900040719     C                   SETON                                        31
146000040719     C                   ENDIF
146100040719     C*
146200040719     C* Se LNP nn valorizzato considero errore bloccante in traduzione
146300040719     C                   IF        VABLNP = *zeros
146400040719     C                   SETON                                        31
146500040719     C                   ENDIF
146600040706     C*
146700040706     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
146800040706     C* d valorizzarli sempre entrambi
146900040706     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
147000040706     C                   Z-ADD     VABNSP        VABRMN
147100040706     C                   ENDIF
147200040706     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
147300040706     C                   MOVEL     VABRMN        VABRMA
147400040706     C                   ENDIF
147500040113     C*
147600040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
147700060112     C                   IF        FlgCAS = '1'
147800040119     C                   EVAL      VABVCA = 'EUR'
147900040119     C                   IF        VABCBO = '1'
148000040113     C                   EVAL      VABCBO = '4'
148100040119     C                   ENDIF
148200040119     C                   IF        VABCBO = '2'
148300040119     C                   EVAL      VABCBO = '6'
148400040119     C                   ENDIF
148500040113     C                   ENDIF
148600020204     C*
148700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
148800020204     C                   EXSR      CHKIMPDIV
148900030715     C                   ENDIF
149000020204     C*
149100010607     C                   ENDSR
149200010607     C*----------------------------------------------------*
149300010601
149400020204
149500020204     C*----------------------------------------------------*
149600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
149700020204     C*----------------------------------------------------*
149800020204     C     CHKIMPDIV     BEGSR
149900020204     C*
150000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
150100020204     C                   Z-ADD     *zeros        wrkDec            9 9
150200020204     C*
150300020204     C* Come prima cosa effettuo considerazioni sulla divisa
150400020204     C                   IF        vabIAS > *zeros
150500020204     C                   IF        vabVAS <> 'EUR'
150600020204     C                   EVAL      vabVAS =  'ITL'
150700020204     C                   ENDIF
150800020204     C                   ENDIF
150900020204     C*
151000020204     C                   IF        vabCAS > *zeros
151100020204     C                   IF        vabVCA <> 'EUR'
151200020204     C                   EVAL      vabVCA =  'ITL'
151300020204     C                   ENDIF
151400020204     C                   ENDIF
151500020204     C*
151600020204     C                   IF        vabVMD > *zeros
151700020204     C                   IF        vabVAD <> 'EUR'
151800020204     C                   EVAL      vabVAD =  'ITL'
151900020204     C                   ENDIF
152000020204     C                   ENDIF
152100020204     C*
152200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
152300020204     C                   Z-ADD     vabIAS        wrkDec
152400020204     C                   IF        wrkDec > *zeros
152500020204     C                   IF        vabVAS = 'ITL'
152600020204     C                   EVAL      vabIAS = *zeros
152700020204     C                   ENDIF
152800020204     C                   ENDIF
152900020204     C*
153000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
153100020204     C                   Z-ADD     vabCAS        wrkDec
153200020204     C                   IF        wrkDec > *zeros
153300020204     C                   IF        vabVCA = 'ITL'
153400020204     C                   EVAL      vabCAS = *zeros
153500020204     C                   ENDIF
153600020204     C                   ENDIF
153700020204     C*
153800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
153900020204     C                   Z-ADD     vabVMD        wrkDec
154000020204     C                   IF        wrkDec > *zeros
154100020204     C                   IF        vabVAD = 'ITL'
154200020204     C                   EVAL      vabVMD = *zeros
154300020204     C                   ENDIF
154400020204     C                   ENDIF
154500020204     C*
154600020204     C                   ENDSR
154700020204     C***
154800020204
154900010330
155000010330
155100010330     C*----------------------------------------------------*
155200010330     C*  CONTROLLO NUMERICITA' CAMPI
155300010330     C*----------------------------------------------------*
155400010330     C     CHKNUM        BEGSR
155500010330     C*
155600010606     C                   IF        PiDecChr = *blanks
155700030715     C                   EVAL      PiDecChr = CharNUM
155800010606     C                   ENDIF
155900010606     C*
156000010606     C                   CALL(e)   'ISNUMERIC'
156100010330     C                   PARM                    PiStr            30
156200010606     C                   PARM                    PiDecChr          1
156300010330     C                   PARM      *ZEROS        PiVal            30 9
156400010330     C                   PARM      '0'           PiInt             1
156500010330     C                   PARM      '0'           PiNum             1
156600010330     C                   IF        %error
156700010606     C                   EVAL      PiNum=*off
156800010330     C                   ENDIF
156900010330     C*
157000010330     C                   ENDSR
157100010330     C***
157200010330
157300010601
157400010601
157500010601
157600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
157700010601     C     repfil        BEGSR
157800010601     C*
157900010601     C                   if        invfil = *zeros and
158000010601     C                             depfil > *zeros and
158100010629     C                             (vinflg = *blanks or
158200010629     C                              vinflg = *zeros)
158300010601     C
158400010601     C                   eval      invfil = depfil
158500010601     C                   endif
158600010601     C*
158700010601     C                   if        depfil <> invfil and
158800010601     C                             invfil > *zeros
158900010601     C                   eval      flgMulti = '1'
159000010601     C                   if        vinflg = *blanks
159100010601     C                   add       1             cntNonEl
159200010601     C                   endif
159300010601     C                   endif
159400010601     C*
159500010601     C                   if        vinflg = '2'
159600010601     C                   eval      flgStato = '2'
159700010601     C                   endif
159800010601     C*
159900010601     C                   ENDSR
160000010601     C***
160100010601
160200010601
160300010601
160400010330
160500040119      /TITLE Invio dei dati al punto operativo.
160600040119     C     invio         BEGSR
160700040119     C*
160800040119     C* 1° invio FIVAT
160900040119     C                   reset                   dscmz
161000040119     C                   move      vlrpoi        cmzdst
161100040119     C                   eval      cmzfld = 'FIVATWWR'
161200040119     C                   eval      cmzmbd = vlrhdl
161300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
161400040119     C***                if        prmfir = *blanks
161500040119     C                   eval      cmzfla = 'FIVAT00F'
161600040119     C                   eval      cmzmba = 'FIVAT00F'
161700040119     C***                else
161800040119     C***                eval      cmzfla = prmfir
161900040119     C***                eval      cmzmba = prmfir
162000040119     C***                endif
162100040119     C                   eval      cmznrr = *zeros
162200040119     C                   move      §ctrokvt      cmznrr
162300040119     C                   eval      cmzlba = vlrfl1
162400040119     C                   call(e)   'TIS711C'
162500040119     C                   parm                    dscmz
162600040119     C                   parm      *blanks       esito
162700040119     C                   if        %error
162800040119     C                             or cmzerr = '1'
162900040119     C                             or esito  = '1'
163000040119     C                   eval      wrkesito = '3'
163100040119     C                   else
163200040119     C*
163300040119     C* 2° invio FIVAB
163400040119     C                   reset                   dscmz
163500040119     C                   move      vlrpoi        cmzdst
163600040119     C                   eval      cmzfld = vlrfou
163700040119     C                   eval      cmzmbd = vlrhdl
163800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
163900040119     C***                if        prmfir = *blanks
164000040119     C                   eval      cmzfla = 'FIVAB00F'
164100040119     C                   eval      cmzmba = 'FIVAB00F'
164200040119     C***                else
164300040119     C***                eval      cmzfla = prmfir
164400040119     C***                eval      cmzmba = prmfir
164500040119     C***                endif
164600040119     C                   eval      cmznrr = *zeros
164700040119     C                   move      §ctrokvb      cmznrr
164800040119     C                   eval      cmzlba = vlrfl1
164900040119     C                   call(e)   'TIS711C'
165000040119     C                   parm                    dscmz
165100040119     C                   parm      *blanks       esito
165200040119     C                   if        %error
165300040119     C                             or cmzerr = '1'
165400040119     C                             or esito  = '1'
165500040119     C                   eval      wrkesito = '3'
165600040119     C                   endif
165700040119     C                   endif
165800040119     C*
165900040119     C                   ENDSR
166000040119     C***
166100010601
166200010601
166300010601
166400010601
166500010601      /TITLE Invio dei dati al punto operativo.
166600010601     C     opeini        BEGSR
166700010601     C*
166800010601     C* Inizializzo flag e contatori operativi
166900010601     C                   movel     '0'           flgGiro           1
167000010601     C                   movel     '0'           flgMulti          1
167100010601     C                   movel     '1'           flgStato          1
167200010615     C                   movel     '0'           flgOk             1
167300010601     C                   z-add     *zeros        cntNonEl         10 0
167400010601     C                   z-add     *zeros        depfil            3 0
167500010601     C                   z-add     *zeros        invfil            3 0
167600010601     C*
167700010601     C                   ENDSR
167800010601     C***
167900010601
168000010601
168100050421
168200050421
168300070411
168400070411     C     *pssr         BEGSR
168500070411     C*
168600070411     C                   if        %open(tivin00r)
168700070411     C                   close     tivin00r
168800070411     C                   endif
168900070411     C                   if        %open(fivabwwr)
169000070411     C                   close     fivabwwr
169100070411     C                   endif
169200070411     C                   if        %open(fivatwwr)
169300070411     C                   close     fivatwwr
169400070411     C                   endif
169500070411     C*
169600070411     C* Effettuo la chiamata al CLLE preposto
169700070411     C                   call(e)   'TITVVTC'
169800070411     C                   parm                    parccm
169900070411     C                   parm                    parmbr
170000070411     C                   parm      '2'           paropz
170100070411     C*
170200070411     C                   eval      wrkesito = '2'
170300070411     C*
170400070411     C                   seton                                        LR
170500070411     C*
170600070411     C                   ENDSR     '*CANCL'
170700070411     C***
170800070411
170900050421
171000010601
171100010330
171200010330
171300000613     C     *inzsr        BEGSR
171400990910     C*
171500990910     C     *entry        plist
171600990920     C                   parm                    tivlrds
171700990921     C                   parm      wrkesito      esito
171800000724     C                   parm                    prmlit
171900000710     C                   parm                    prmfir
172000010330     C*
172100010330     C* CALCOLA LA DATA CORRENTE
172200010330     C                   time                    wn14             14 0
172300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
172400010330     C                   z-add     wn8           g08dat
172500010330     C                   z-add     *zeros        g08inv
172600010330     C                   movel     '0'           g08err
172700010330     C                   call      'XSRDA8'
172800010330     C                   parm                    wlbda8
172900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
173000000613     C*
173100000613     C                   ENDSR
173200000613     C***
173300990908
173400060112     Otitvq5p   E            riepilogo         2
173500990915     O                                              'Upload via Internet'
173600990915     O                                           +1 'Traduzione TIVIN00R -'
173700021025     O                                           +1 'FIVABWWR'
173800990915     O                                           +1 'Testate bolle'
173900990915     O          E            riepilogo   2
174000990915     O                       wrkdata
174100990915     O                       wrkora              +1
174200990915     O                       procname            +1
174300990915     O          E            riepilogo   2
174400990915     O                                              'Cliente..................:'
174500010605     O                       VABCCM        z     +1
174600990915     O          E            riepilogo   2
174700990920     O                                              'Riferimento Strategi.....:'
174800990920     O                       vlrhdl              +1
174900990915     O          E            riepilogo   2
175000990915     O                                              'Giusti...................:'
175100040119     O                       §CTROKVB      2   +  1
175200990915     O          E            riepilogo   2
175300990915     O                                              'Sbagliati e corretti.....:'
175400971022     O                       §CTRMO        2   +  1
175500990915     O          E            riepilogo   2
175600990915     O                                              'Sbagliati e scartati.....:'
175700971022     O                       §CTRNO        2   +  1
175800000613
175900060112     Otitvq5ps  E            testdett          2
176000000613     O                                              'Upload via Internet'
176100000613     O                                           +1 'Traduzione TIVIN00R -'
176200021025     O                                           +1 'FIVABWWR'
176300000613     O                                           +1 'Testate bolle'
176400000616     O          E            testdett    3
176500000613     O                                           +2 'N° rec'
176600000613     O                                           +3 'Anteprima contenuto'
176700000616     O          E            rigadett    2
176800000613     O                       rrnum               +2
176900000621     O                       recko               +3
177000000616     O          E            findett     2
177100000613     O                       wrkdata
177200000613     O                       wrkora              +1
177300000613     O                       procname            +1
177400000616     O          E            findett     2
177500000613     O                                              'Cliente..................:'
177600010605     O                       VABCCM        z     +1
177700000616     O          E            findett     2
177800000613     O                                              'Riferimento Strategi.....:'
177900000613     O                       vlrhdl              +1
178000000616     O          E            findett     2
178100000613     O                                              'Giusti...................:'
178200040119     O                       §CTROKVB      2   +  1
178300000616     O          E            findett     2
178400000613     O                                              'Sbagliati e corretti.....:'
178500000613     O                       §CTRMO        2   +  1
178600000616     O          E            findett     2
178700000613     O                                              'Sbagliati e scartati.....:'
178800000613     O                       §CTRNO        2   +  1
178900000613** CMD - COMANDI CL
179000060112OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVQ5P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
179100060112OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVQ5PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
179200060112DLTOVR FILE(TITVQ5P TITVQ5PS) LVL(*)
179300010606
179400000613
179500040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
179600070201INFO_CORR;VABRMA;TRC_A;Testo88;VABRSD;VABIND;CELL;
179700070201VABCAD;VABLOD;TRC_B;NOTE;CONTRASS;Testo5
179800040113
