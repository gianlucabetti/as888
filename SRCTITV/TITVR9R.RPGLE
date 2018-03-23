000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800060310     Ftitvr9p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100060310     Ftitvr9ps  O    f  198        PRINTER usropn
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
005800060112     D wVATNOT_B       s                   LIKE(vatnot) inz(*blanks)
005900060112     D wCELL           s             10    inz(*blanks)
006000060310     D wNOTE           s            100    inz(*blanks)
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
015400060310     C                   if        not %open(titvr9ps)
015500060310     C                   open      titvr9ps
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
018600060310     C                   if        %open(titvr9ps)
018700000616     C                   except    findett
018800060310     C                   close     titvr9ps
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
020800060310     C                   if        not %open(titvr9p)
020900060310     C   50              open      titvr9p
021000990915     C                   endif
021100990915     C*
021200030715     C   50              except    riepilogo
021300990915     C*
021400060310     C                   if        %open(titvr9p)
021500060310     C   50              close     titvr9p
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
040600060310     C* Imposto il tipo record sul file VAT a 'B' x indicare il telefono destinatario
040700051108     C                   EVAL      VATTRC = 'B'
040800060310     C                   IF        wVATNOT_B <> *blanks
040900051108     C                   EVAL      VATNOT = %trim(wVATNOT_B)
041000060310     C                   ELSE
041100060310     C                   EVAL      VATNOT = %trim(wCELL)
041200060310     C                   ENDIF
041300040719     C*
041400040719     C* Quindi se valorizzato il VATNOT scarico il buffer e incremento il contatore x il CMZ
041500040719     C                   IF        VATNOT <> *blanks
041600040706     C                   WRITE     FIVAT000
041700040706     C                   ADD       1             §CTROKVT
041800040719     C                   ENDIF
041900040119     C*
042000040119     C                   ENDSR
042100040119
042200040119
042300040119
042400040119     C*----------------------------------------------------*
042500040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
042600040119     C*----------------------------------------------------*
042700040119     C     PREVAT        BEGSR
042800040119     C*
042900040119     C* Compongo il nome del membro da dare al FIVATWWR
043000040119     C                   eval      parmbr = vlrhdl
043100040119     C                   movel     'M'           parmbr
043200060112     C                   eval      parccm = vlrksc
043300040119     C                   eval      paropz = '1'
043400040119     C* Effettuo la chiamata al CLLE preposto
043500040119     C                   call(e)   'TITVVTC'
043600040119     C                   parm                    parccm
043700040119     C                   parm                    parmbr
043800040119     C                   parm                    paropz
043900040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
044000040119     C                   if        %error
044100040119     C                   movel     '1'           chkcall
044200040119     C                   else
044300040119     C                   movel     '0'           chkcall
044400040119     C                   endif
044500040119     C*
044600040119     C                   ENDSR
044700010601
044800010601
044900010601
045000010330     C*----------------------------------------------------*
045100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
045200010330     C*----------------------------------------------------*
045300040119     C     INZVAR        BEGSR
045400010330     C*
045500030822     C* Inizializzo variabili di wrk
045600020204     C                   Z-ADD     *zeros        Num5_0            5 0
045700060310     C                   CLEAR                   wVATNOT_B
045800060310     C                   CLEAR                   wCELL
045900060310     C                   CLEAR                   wNOTE
046000060112     C*
046100060112     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
046200060112     C                   MOVEL     '0'           FlgCAS            1
046300030822     C*
046400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
046500030822     C                   CLEAR                   FIVAB000
046600040119     C                   CLEAR                   FIVAT000
046700030822     C                   CLEAR                   SkSplitCSV
046800030822     C                   EVAL      i = 1
046900030822     C                   EVAL      posDa = *zeros
047000030822     C                   EVAL      posA  = *zeros
047100030822     C*
047200030822     C* Reimposto i valori di default
047300030822     C                   EXSR      DEFCAM
047400030822     C*
047500010330     C                   ENDSR
047600010330     C*----------------------------------------------------*
047700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
047800010330     C*----------------------------------------------------*
047900010330     C     DEFCAM        BEGSR
048000040119     C*
048100040119     C* Imposto i valori d default a programma dei campi fondamentali
048200060310     C                   EVAL      VABCCM = 0540580
048300060310     C                   EVAL      VATCCM = 0540580
048400060310     C                   EVAL      VABLNP = 054
048500060310     C                   EVAL      VATLNP = 054
048600060310     C                   EVAL      VABCTR = 000
048700040119     C                   EVAL      VABCBO = '1'
048800030822     C*
048900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
049000030715     C* e delimitatore testo.
049100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
049200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
049300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
049400030822     C*
049500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
049600030715     C                   IF        %subst(vlrppt:1:1) = 'S'
049700030715     C                   SETON                                        50
049800030715     C                   ELSE
049900030715     C                   SETOFF                                       50
050000030715     C                   ENDIF
050100030822     C*
050200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
050300030822     C                   EVAL      posDaDft = 1
050400030822     C                   EVAL      posADft  = 0
050500030822     C                   EVAL      wGiroDft = 0
050600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
050700030822     C                             posDaDft > 0
050800030822     C*
050900030822     C* Gestisco il 1° giro
051000030822     C                   IF        wGiroDft = 0
051100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
051200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
051300030822     C* Incremento il contatore dei "giri"
051400030822     C                   EVAL      wGiroDft = 1
051500030822     C                   ELSE
051600030822     C                   EVAL      posDaDft = posADft
051700030822     C                   ENDIF
051800030822     C* Eseguo lo scan x trovare la fine del campo corrente
051900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
052000030822     C*
052100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
052200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
052300030822     C                   IF        posDaDft > 0 AND
052400030822     C                             posADft  > 0
052500030822     C* NCL
052600030822     C                   IF        %subst(
052700030822     C                             %subst(vlrppt:posDaDft+1:
052800030822     C                             posADft-posDaDft-1):1:3)
052900030822     C                             = 'NCL'
053000030822     C                   EVAL      PiStr=%trim(%subst(
053100030822     C                             %subst(vlrppt:posDaDft+1:
053200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053300030822     C                   EXSR      CHKNUM
053400030822     C                   IF        PiInt=*on
053500030822     C                   Z-ADD     PiVal         VABNCL
053600030822     C                   ENDIF
053700030822     C                   ENDIF
053800030822     C* CCM
053900030822     C                   IF        %subst(
054000030822     C                             %subst(vlrppt:posDaDft+1:
054100030822     C                             posADft-posDaDft-1):1:3)
054200030822     C                             = 'CCM'
054300030822     C                   EVAL      PiStr=%trim(%subst(
054400030822     C                             %subst(vlrppt:posDaDft+1:
054500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054600030822     C                   EXSR      CHKNUM
054700030822     C                   IF        PiInt=*on
054800030822     C                   Z-ADD     PiVal         VABCCM
054900040119     C                   Z-ADD     PiVal         VATCCM
055000030822     C                   ENDIF
055100030822     C                   ENDIF
055200030822     C* LNP
055300030822     C                   IF        %subst(
055400030822     C                             %subst(vlrppt:posDaDft+1:
055500030822     C                             posADft-posDaDft-1):1:3)
055600030822     C                             = 'LNP'
055700030822     C                   EVAL      PiStr=%trim(%subst(
055800030822     C                             %subst(vlrppt:posDaDft+1:
055900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056000030822     C                   EXSR      CHKNUM
056100030822     C                   IF        PiInt=*on
056200030822     C                   Z-ADD     PiVal         VABLNP
056300040119     C                   Z-ADD     PiVal         VATLNP
056400030822     C                   ENDIF
056500030822     C                   ENDIF
056600030822     C* NRS
056700030822     C                   IF        %subst(
056800030822     C                             %subst(vlrppt:posDaDft+1:
056900030822     C                             posADft-posDaDft-1):1:3)
057000030822     C                             = 'NRS'
057100030822     C                   EVAL      PiStr=%trim(%subst(
057200030822     C                             %subst(vlrppt:posDaDft+1:
057300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057400030822     C                   EXSR      CHKNUM
057500030822     C                   IF        PiInt=*on
057600030822     C                   Z-ADD     PiVal         VABNRS
057700030822     C                   ENDIF
057800030822     C                   ENDIF
057900030822     C* CTR
058000030822     C                   IF        %subst(
058100030822     C                             %subst(vlrppt:posDaDft+1:
058200030822     C                             posADft-posDaDft-1):1:3)
058300030822     C                             = 'CTR'
058400030822     C                   EVAL      PiStr=%trim(%subst(
058500030822     C                             %subst(vlrppt:posDaDft+1:
058600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058700030822     C                   EXSR      CHKNUM
058800030822     C                   IF        PiInt=*on
058900030822     C                   Z-ADD     PiVal         VABCTR
059000030822     C                   ENDIF
059100030822     C                   ENDIF
059200030822     C* PKB
059300030822     C                   IF        %subst(
059400030822     C                             %subst(vlrppt:posDaDft+1:
059500030822     C                             posADft-posDaDft-1):1:3)
059600030822     C                             = 'PKB'
059700030822     C                   EVAL      PiStr=%trim(%subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060000030822     C                   EXSR      CHKNUM
060100030822     C                   IF        PiNum=*on
060200030822     C                   Z-ADD     PiVal         VABPKB
060300030822     C                   ENDIF
060400030822     C                   ENDIF
060500030822     C* VLB
060600030822     C                   IF        %subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):1:3)
060900030822     C                             = 'VLB'
061000030822     C                   EVAL      PiStr=%trim(%subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061300030822     C                   EXSR      CHKNUM
061400030822     C                   IF        PiNum=*on
061500030822     C                   Z-ADD     PiVal         VABVLB
061600030822     C                   ENDIF
061700030822     C                   ENDIF
061800030822     C* QFT
061900030822     C                   IF        %subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):1:3)
062200030822     C                             = 'QFT'
062300030822     C                   EVAL      PiStr=%trim(%subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062600030822     C                   EXSR      CHKNUM
062700030822     C                   IF        PiNum=*on
062800030822     C                   Z-ADD     PiVal         VABQFT
062900030822     C                   ENDIF
063000030822     C                   ENDIF
063100030822     C* CBO
063200030822     C                   IF        %subst(
063300030822     C                             %subst(vlrppt:posDaDft+1:
063400030822     C                             posADft-posDaDft-1):1:3)
063500030822     C                             = 'CBO'
063600030822     C                   EVAL      VABCBO=%trim(%subst(
063700030822     C                             %subst(vlrppt:posDaDft+1:
063800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063900030822     C                   ENDIF
064000030822     C* TSP
064100030822     C                   IF        %subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):1:3)
064400030822     C                             = 'TSP'
064500030822     C                   EVAL      VABTSP=%trim(%subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064800030822     C                   ENDIF
064900030822     C* VAS
065000030822     C                   IF        %subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):1:3)
065300030822     C                             = 'VAS'
065400030822     C                   EVAL      VABVAS=%trim(%subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065700030822     C                   ENDIF
065800030822     C* VCA
065900030822     C                   IF        %subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):1:3)
066200030822     C                             = 'VCA'
066300030822     C                   EVAL      VABVCA=%trim(%subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066600030822     C                   ENDIF
066700030822     C* GCA
066800030822     C                   IF        %subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):1:3)
067100030822     C                             = 'GCA'
067200030822     C                   EVAL      VABGCA=%trim(%subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067500030822     C                   ENDIF
067600030822     C* CTM
067700030822     C                   IF        %subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):1:3)
068000030822     C                             = 'CTM'
068100030822     C                   EVAL      VABCTM=%trim(%subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068400030822     C                   ENDIF
068500030822     C* FFD
068600030822     C                   IF        %subst(
068700030822     C                             %subst(vlrppt:posDaDft+1:
068800030822     C                             posADft-posDaDft-1):1:3)
068900030822     C                             = 'FFD'
069000030822     C                   EVAL      VABFFD=%trim(%subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069300030822     C                   ENDIF
069400030822     C* VAD
069500030822     C                   IF        %subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):1:3)
069800030822     C                             = 'VAD'
069900030822     C                   EVAL      VABVAD=%trim(%subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070200030822     C                   ENDIF
070300030822     C* GMA
070400030822     C                   IF        %subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):1:3)
070700030822     C                             = 'GMA'
070800030822     C                   EVAL      VABGMA=%trim(%subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071100030822     C                   ENDIF
071200030822     C* GGA
071300030822     C                   IF        %subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):1:3)
071600030822     C                             = 'GGA'
071700030822     C                   EVAL      VABGGA=%trim(%subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072000030822     C                   ENDIF
072100030822     C* GVA
072200030822     C                   IF        %subst(
072300030822     C                             %subst(vlrppt:posDaDft+1:
072400030822     C                             posADft-posDaDft-1):1:3)
072500030822     C                             = 'GVA'
072600030822     C                   EVAL      VABGVA=%trim(%subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072900030822     C                   ENDIF
073000030822     C* TC1
073100030822     C                   IF        %subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):1:3)
073400030822     C                             = 'TC1'
073500030822     C                   EVAL      VABTC1=%trim(%subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073800030822     C                   ENDIF
073900030822     C* TC2
074000030822     C                   IF        %subst(
074100030822     C                             %subst(vlrppt:posDaDft+1:
074200030822     C                             posADft-posDaDft-1):1:3)
074300030822     C                             = 'TC2'
074400030822     C                   EVAL      VABTC2=%trim(%subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074700030822     C                   ENDIF
074800030822     C* ...
074900030822     C                   ENDIF
075000030822     C                   ENDDO
075100020204     C*
075200010330     C                   ENDSR
075300010607     C*----------------------------------------------------*
075400021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
075500010607     C*----------------------------------------------------*
075600010607     C     IMPVAB        BEGSR
075700010607     C*
075800010607     C                   SETOFF                                       3132
075900030515     C*
076000010607     C                   MOVEL     datcor        VABAAS
076100040119     C                   MOVEL     datcor        VATAAS
076200010607     C                   MOVE      datcor        VABMGS
076300010607     C*
076400030715     C**********
076500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
076600030715     C**********
076700030715     C                   IF        wGiro = *zeros
076800040706     C*
076900040706     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
077000040706     C                   EVAL      depvinDTA = vinDTA
077100040113     C*
077200040113     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
077300040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
077400040113     C                                      %trim(intcol(3))
077500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
077600031201     C     minu:maiu     XLATE     vindta        vindta
077700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
077800030715     C                   DOW       posDa <= %len(%trim(vindta))
077900030715     C*
078000030715     C* Gestisco il 1° campo
078100030715     C                   IF        i = 1
078200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
078300030715     C                   EVAL      posDa = 1
078400030715     C* Eseguo lo scan x trovare la fine del primo campo
078500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
078600030715     C* A questo punto "estraggo" il campo corrente
078700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
078800030715     C                                                    (posA-posDa))
078900030715     C* X i campi successivi al 1°
079000030715     C                   ELSE
079100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
079200030715     C                   EVAL      posDa = posA + 1
079300030715     C* Eseguo lo scan x trovare la fine del campo corrente
079400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079500030715     C* Gestisco l'ultimo campo
079600030715     C                   IF        posA = *zeros
079700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
079800030715     C* A questo punto "estraggo" il campo corrente
079900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080000030715     C                                                    (posA-posDa))
080100030715     C                   LEAVE
080200030715     C                   ELSE
080300030715     C* A questo punto "estraggo" il campo corrente
080400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080500030715     C                                                    (posA-posDa))
080600030715     C                   ENDIF
080700030715     C                   ENDIF
080800030715     C* Incremento il contatore d campo
080900030715     C                   EVAL      i = i +1
081000030715     C                   ENDDO
081100030715     C*
081200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
081300030715     C* delimitazione testo
081400030715     C                   EVAL      i = 1
081500030715     C                   DOW       i < %elem(SkSplitFLD)
081600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
081700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
081800030715     C                   EVAL      i = i + 1
081900030715     C                   ENDDO
082000030715     C                   EVAL      wGiro = 1
082100040706     C* Ripristino la 1° riga originale
082200040706     C                   EVAL      vinDTA = depvinDTA
082300030715     C                   ELSE
082400030715     C                   EVAL      wGiro = 2
082500031204     C**********
082600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
082700031204     C* d separatore campo
082800031204     C**********
082900031204     C                   EVAL      vindta = %trim(vindta)
083000031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
083100031204     C                   EVAL      vindta = %subst(vindta:2)
083200031204     C                   ENDDO
083300031204     C*
083400031204     C                   Z-ADD     *zeros        lunghInput        4 0
083500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
083600031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
083700031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
083800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
083900031204     C                   ENDDO
084000030715     C**********
084100030715     C* Effettuo lo split del campo dati d input
084200030715     C**********
084300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
084400030715     C                   DOW       posDa <= %len(%trim(vindta))
084500030715     C*
084600030715     C* Gestisco il 1° campo
084700030715     C                   IF        i = 1
084800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
084900030715     C                   EVAL      posDa = 1
085000030715     C* Eseguo lo scan x trovare la fine del primo campo
085100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085200030715     C* A questo punto "estraggo" il campo corrente
085300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
085400030715     C                                                    (posA-posDa))
085500030715     C* X i campi successivi al 1°
085600030715     C                   ELSE
085700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085800030715     C                   EVAL      posDa = posA + 1
085900031204     C* Verifico che nn vi sia il campo nn valorizzato
086000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
086100031204     C* Se campo nn valorizzato skippo al prossimo
086200031204     C                   EVAL      SkSplitCSV(i) = *blanks
086300031204     C                   EVAL      posA  = posA + 1
086400031204     C                   ELSE
086500030715     C* Eseguo lo scan x trovare la fine del campo corrente
086600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086700030715     C* Gestisco l'ultimo campo
086800030715     C                   IF        posA = *zeros
086900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
087000030715     C* A questo punto "estraggo" il campo corrente
087100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087200030715     C                                                    (posA-posDa))
087300030715     C                   LEAVE
087400030715     C                   ELSE
087500030715     C* A questo punto "estraggo" il campo corrente
087600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087700030715     C                                                    (posA-posDa))
087800030715     C                   ENDIF
087900031204     C                   ENDIF
088000030715     C                   ENDIF
088100030715     C* Incremento il contatore d campo
088200030715     C                   EVAL      i = i +1
088300030715     C                   ENDDO
088400030715     C*
088500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088600030715     C* delimitazione testo
088700030715     C                   EVAL      i = 1
088800030715     C                   DOW       i < %elem(SkSplitCSV)
088900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
089000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
089100030715     C                   EVAL      i = i + 1
089200030715     C                   ENDDO
089300030715     C*
089400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
089500030715     C                   EVAL      i = 1
089600030715     C                   DOW       i < %elem(SkSplitFLD)
089700030715     C*
089800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
089900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
090000030715     C                             SkSplitCSV(i) <> *blanks
090100030721     C*** ATB
090200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
090300030721     C                   EVAL      VABATB = SkSplitCSV(i)
090400030721     C                   ENDIF
090500030715     C*** RSD
090600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
090700030715     C                   EVAL      VABRSD = SkSplitCSV(i)
090800030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
090900030715     C     '@':'A'       XLATE     VABRSD        VABRSD
091000030715     C* ==
091100030715     C                   ENDIF
091200030721     C*** RD2
091300060112     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
091400030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
091500030721     C                   ENDIF
091600030715     C*** IND
091700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
091800030715     C                   EVAL      VABIND = SkSplitCSV(i)
091900030715     C                   ENDIF
092000030715     C*** LOD
092100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
092200030715     C                   EVAL      VABLOD = SkSplitCSV(i)
092300030715     C                   ENDIF
092400030715     C*** PRD
092500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
092600030715     C                   EVAL      VABPRD = SkSplitCSV(i)
092700030715     C                   ENDIF
092800030721     C*** NZD
092900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
093000030721     C                   EVAL      VABNZD = SkSplitCSV(i)
093100030721     C                   ENDIF
093200030715     C*** RMA
093300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
093400030715     C                   EVAL      VABRMA = SkSplitCSV(i)
093500030715     C                   ENDIF
093600030715     C*** NOT
093700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
093800030715     C                   EVAL      VABNOT = SkSplitCSV(i)
093900030715     C                   ENDIF
094000030715     C*** NT2
094100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
094200030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
094300030715     C                   ENDIF
094400030715     C*** VCA
094500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
094600030715     C                   EVAL      VABVCA = SkSplitCSV(i)
094700030715     C                   ENDIF
094800030715     C*** VAS
094900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
095000030715     C                   EVAL      VABVAS = SkSplitCSV(i)
095100030715     C                   ENDIF
095200030715     C*** FFD
095300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
095400030715     C                   EVAL      VABFFD = SkSplitCSV(i)
095500030715     C                   ENDIF
095600030721     C*** GC1
095700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
095800030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
095900030721     C                   ENDIF
096000030721     C*** GC2
096100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
096200030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
096300030721     C                   ENDIF
096400030721     C*** TSP
096500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
096600030721     C                   EVAL      VABTSP = SkSplitCSV(i)
096700030721     C                   ENDIF
096800030721     C*** NAS
096900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
097000030721     C                   EVAL      VABNAS = SkSplitCSV(i)
097100030721     C                   ENDIF
097200030721     C*** TIC
097300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
097400030721     C                   EVAL      VABTIC = SkSplitCSV(i)
097500030721     C                   ENDIF
097600030721     C*** GCA
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
097800030721     C                   EVAL      VABGCA = SkSplitCSV(i)
097900030721     C                   ENDIF
098000030721     C*** XCO
098100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
098200030721     C                   EVAL      VABXCO = SkSplitCSV(i)
098300030721     C                   ENDIF
098400030721     C*** CTM
098500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
098600030721     C                   EVAL      VABCTM = SkSplitCSV(i)
098700030721     C                   ENDIF
098800030721     C*** TCR
098900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
099000030721     C                   EVAL      VABTCR = SkSplitCSV(i)
099100030721     C                   ENDIF
099200030721     C*** CTS
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
099400030721     C                   EVAL      VABCTS = SkSplitCSV(i)
099500030721     C                   ENDIF
099600030721     C*** FTM
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
099800030721     C                   EVAL      VABFTM = SkSplitCSV(i)
099900030721     C                   ENDIF
100000030721     C*** VAD
100100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
100200030721     C                   EVAL      VABVAD = SkSplitCSV(i)
100300030721     C                   ENDIF
100400030721     C*** GMA
100500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
100600030721     C                   EVAL      VABGMA = SkSplitCSV(i)
100700030721     C                   ENDIF
100800030721     C*** GGA
100900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
101000030721     C                   EVAL      VABGGA = SkSplitCSV(i)
101100030721     C                   ENDIF
101200030721     C*** GVA
101300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
101400030721     C                   EVAL      VABGVA = SkSplitCSV(i)
101500030721     C                   ENDIF
101600030721     C*** TC1
101700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
101800030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
101900030721     C                   ENDIF
102000030721     C*** TC2
102100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
102200030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
102300030721     C                   ENDIF
102400030721     C*** SCL
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
102600030721     C                   EVAL      VABSCL = SkSplitCSV(i)
102700030721     C                   ENDIF
102800030721     C*** RMO
102900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
103000030721     C                   EVAL      VABRMO = SkSplitCSV(i)
103100030721     C                   ENDIF
103200030721     C*** NMO
103300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
103400030721     C                   EVAL      VABNMO = SkSplitCSV(i)
103500030721     C                   ENDIF
103600040719     C*** CBO
103700040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
103800060112     C                   EVAL      VABCBO = SkSplitCSV(i)
103900040719     C                   ENDIF
104000030721     C*
104100040706     C* Reperisco quindi i campi numerici...
104200040706     C*** NSP
104300040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
104400040706     C                   EVAL      PiStr=SkSplitCSV(i)
104500040706     C                   EXSR      CHKNUM
104600040706     C                   IF        PiInt=*on
104700040706     C                   Z-ADD     PiVal         VABNSP
104800040706     C                   ELSE
104900040706     C                   SETON                                        31
105000040706     C                   EVAL      VABNSP = *zeros
105100040706     C                   EVAL      vinmsg = %trimr(vinmsg)
105200060112     C                             + ' ' + 'VABNSP'
105300040706     C                   ENDIF
105400040706     C                   ENDIF
105500040706     C*** RMN
105600040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
105700040706     C                   EVAL      PiStr=SkSplitCSV(i)
105800040706     C                   EXSR      CHKNUM
105900040706     C                   IF        PiInt=*on
106000040706     C                   Z-ADD     PiVal         VABRMN
106100040706     C                   ELSE
106200040706     C                   SETON                                        32
106300040706     C                   EVAL      VABRMN = 1
106400040706     C                   EVAL      vinmsg = %trimr(vinmsg)
106500040706     C                             + ' ' + 'VABRMN'
106600040706     C                   ENDIF
106700040706     C                   ENDIF
106800030721     C*** CAD
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
107000030721     C                   EVAL      PiStr=SkSplitCSV(i)
107100030721     C                   EXSR      CHKNUM
107200030721     C                   IF        PiInt=*on
107300030721     C                   Z-ADD     PiVal         Num5_0
107400030721     C                   MOVEL(p)  Num5_0        VABCAD
107500030721     C                   ELSE
107600030721     C                   SETON                                        32
107700030721     C                   EVAL      VABCAD = *zeros
107800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107900030721     C                             + ' ' + 'VABCAD'
108000030721     C                   ENDIF
108100030721     C                   ENDIF
108200030721     C*** NCL
108300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
108400030721     C                   EVAL      PiStr=SkSplitCSV(i)
108500030721     C                   EXSR      CHKNUM
108600030721     C                   IF        PiInt=*on
108700030721     C                   Z-ADD     PiVal         VABNCL
108800030721     C                   ELSE
108900030721     C                   SETON                                        32
109000030721     C                   EVAL      VABNCL = *zeros
109100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109200040706     C                             + ' ' + 'VABNCL'
109300030721     C                   ENDIF
109400030721     C                   ENDIF
109500030721     C*** PKB
109600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
109700030721     C                   EVAL      PiStr=SkSplitCSV(i)
109800030721     C                   EXSR      CHKNUM
109900030721     C                   IF        PiNum=*on
110000030721     C                   Z-ADD     PiVal         VABPKB
110100030721     C                   ELSE
110200030721     C                   SETON                                        32
110300030721     C                   EVAL      VABPKB = *zeros
110400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110500030721     C                             + ' ' + 'VABPKB'
110600030721     C                   ENDIF
110700030721     C                   ENDIF
110800030721     C*** CAS
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
111000030721     C                   EVAL      PiStr=SkSplitCSV(i)
111100030721     C                   EXSR      CHKNUM
111200030721     C                   IF        PiNum=*on
111300030721     C                   Z-ADD     PiVal         VABCAS
111400030721     C                   ELSE
111500030721     C                   SETON                                        32
111600030721     C                   EVAL      VABCAS = *zeros
111700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111800030721     C                             + ' ' + 'VABCAS'
111900030721     C                   ENDIF
112000030721     C                   ENDIF
112100030721     C*** CCM
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
112300030721     C                   EVAL      PiStr=SkSplitCSV(i)
112400030721     C                   EXSR      CHKNUM
112500030721     C                   IF        PiInt=*on      AND
112600030721     C                             PiVal<=9999999 AND
112700030721     C                             PiVal>*zeros
112800030721     C                   Z-ADD     PiVal         VABCCM
112900040119     C                   Z-ADD     PiVal         VATCCM
113000030721     C                   ELSE
113100030721     C                   SETON                                        32
113200030721     C                   EVAL      VABCCM = *zeros
113300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113400040119     C                             + ' ' + 'VABCCM VATCCM'
113500030721     C                   ENDIF
113600030721     C                   ENDIF
113700030721     C*** LNP
113800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
113900030721     C                   EVAL      PiStr=SkSplitCSV(i)
114000030721     C                   EXSR      CHKNUM
114100030721     C                   IF        PiInt=*on    AND
114200030721     C                             PiVal<=999   AND
114300030721     C                             PiVal>*zeros
114400030721     C                   Z-ADD     PiVal         VABLNP
114500040119     C                   Z-ADD     PiVal         VATLNP
114600030721     C                   ELSE
114700030721     C                   SETON                                        32
114800030721     C                   EVAL      VABLNP = *zeros
114900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115000040119     C                             + ' ' + 'VABLNP VATLNP'
115100030721     C                   ENDIF
115200030721     C                   ENDIF
115300030721     C*** NRS
115400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
115500030721     C                   EVAL      PiStr=SkSplitCSV(i)
115600030721     C                   EXSR      CHKNUM
115700030721     C                   IF        PiInt=*on AND
115800030721     C                             PiVal<=99
115900030721     C                   Z-ADD     PiVal         VABNRS
116000030721     C                   ELSE
116100030721     C                   SETON                                        32
116200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116300030721     C                             + ' ' + 'VABNRS'
116400030721     C                   ENDIF
116500030721     C                   ENDIF
116600030721     C*** LNA
116700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
116800030721     C                   EVAL      PiStr=SkSplitCSV(i)
116900030721     C                   EXSR      CHKNUM
117000030721     C                   IF        PiInt=*on    AND
117100030721     C                             PiVal<=999   AND
117200030721     C                             PiVal>*zeros
117300030721     C                   Z-ADD     PiVal         VABLNA
117400030721     C                   ELSE
117500030721     C                   SETON                                        32
117600030721     C                   EVAL      VABLNA = *zeros
117700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117800030721     C                             + ' ' + 'VABLNA'
117900030721     C                   ENDIF
118000030721     C                   ENDIF
118100030721     C*** CTR
118200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
118300030721     C                   EVAL      PiStr=SkSplitCSV(i)
118400030721     C                   EXSR      CHKNUM
118500030721     C                   IF        PiInt=*on  AND
118600030721     C                             PiVal<=999
118700030721     C                   Z-ADD     PiVal         VABCTR
118800030721     C                   ELSE
118900030721     C                   SETON                                        32
119000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119100030721     C                             + ' ' + 'VABCTR'
119200030721     C                   ENDIF
119300030721     C                   ENDIF
119400030721     C*** IAS
119500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
119600030721     C                   EVAL      PiStr=SkSplitCSV(i)
119700030721     C                   EXSR      CHKNUM
119800030721     C                   IF        PiNum=*on
119900030721     C                   Z-ADD     PiVal         VABIAS
120000030721     C                   ELSE
120100030721     C                   SETON                                        32
120200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120300030721     C                             + ' ' + 'VABIAS'
120400030721     C                   ENDIF
120500030721     C                   ENDIF
120600030721     C*** VLB
120700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
120800030721     C                   EVAL      PiStr=SkSplitCSV(i)
120900030721     C                   EXSR      CHKNUM
121000030721     C                   IF        PiNum=*on
121100030721     C                   Z-ADD     PiVal         VABVLB
121200030721     C                   ELSE
121300030721     C                   SETON                                        32
121400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121500030721     C                             + ' ' + 'VABVLB'
121600030721     C                   ENDIF
121700030721     C                   ENDIF
121800030721     C*** QFT
121900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
122000030721     C                   EVAL      PiStr=SkSplitCSV(i)
122100030721     C                   EXSR      CHKNUM
122200030721     C                   IF        PiNum=*on
122300030721     C                   Z-ADD     PiVal         VABQFT
122400030721     C                   ELSE
122500030721     C                   SETON                                        32
122600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122700030721     C                             + ' ' + 'VABQFT'
122800030721     C                   ENDIF
122900030721     C                   ENDIF
123000030721     C*** NCD
123100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
123200030721     C                   EVAL      PiStr=SkSplitCSV(i)
123300030721     C                   EXSR      CHKNUM
123400030721     C                   IF        PiInt=*on      AND
123500030721     C                             PiVal<=9999999
123600030721     C                   Z-ADD     PiVal         VABNCD
123700030721     C                   ELSE
123800030721     C                   SETON                                        32
123900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124000030721     C                             + ' ' + 'VABNCD'
124100030721     C                   ENDIF
124200030721     C                   ENDIF
124300030721     C*** NCA
124400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
124500030721     C                   EVAL      PiStr=SkSplitCSV(i)
124600030721     C                   EXSR      CHKNUM
124700030721     C                   IF        PiInt=*on      AND
124800030721     C                             PiVal<=9999999
124900030721     C                   Z-ADD     PiVal         VABNCA
125000030721     C                   ELSE
125100030721     C                   SETON                                        32
125200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125300030721     C                             + ' ' + 'VABNCA'
125400030721     C                   ENDIF
125500030721     C                   ENDIF
125600030721     C*** ZNC
125700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
125800030721     C                   EVAL      PiStr=SkSplitCSV(i)
125900030721     C                   EXSR      CHKNUM
126000030721     C                   IF        PiInt=*on AND
126100030721     C                             PiVal<=99
126200030721     C                   Z-ADD     PiVal         VABZNC
126300030721     C                   ELSE
126400030721     C                   SETON                                        32
126500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126600030721     C                             + ' ' + 'VABZNC'
126700030721     C                   ENDIF
126800030721     C                   ENDIF
126900030721     C*** DCR
127000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
127100030721     C                   EVAL      PiStr=SkSplitCSV(i)
127200030721     C                   EXSR      CHKNUM
127300030721     C                   IF        PiInt=*on       AND
127400030721     C                             PiVal<=99999999
127500030721     C                   Z-ADD     PiVal         VABDCR
127600030721     C                   ELSE
127700030721     C                   SETON                                        32
127800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127900030721     C                             + ' ' + 'VABDCR'
128000030721     C                   ENDIF
128100030721     C                   ENDIF
128200030721     C*** HCR
128300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
128400030721     C                   EVAL      PiStr=SkSplitCSV(i)
128500030721     C                   EXSR      CHKNUM
128600030721     C                   IF        PiInt=*on   AND
128700030721     C                             PiVal<=9999
128800030721     C                   Z-ADD     PiVal         VABHCR
128900030721     C                   ELSE
129000030721     C                   SETON                                        32
129100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129200030721     C                             + ' ' + 'VABHCR'
129300030721     C                   ENDIF
129400030721     C                   ENDIF
129500030721     C*** VMD
129600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
129700030721     C                   EVAL      PiStr=SkSplitCSV(i)
129800030721     C                   EXSR      CHKNUM
129900030721     C                   IF        PiNum=*on
130000030721     C                   Z-ADD     PiVal         VABVMD
130100030721     C                   ELSE
130200030721     C                   SETON                                        32
130300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130400030721     C                             + ' ' + 'VABVMD'
130500030721     C                   ENDIF
130600030721     C                   ENDIF
130700030721     C*** ANT
130800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
130900030721     C                   EVAL      PiStr=SkSplitCSV(i)
131000030721     C                   EXSR      CHKNUM
131100030721     C                   IF        PiInt=*on        AND
131200030721     C                             PiVal<=999999999
131300030721     C                   Z-ADD     PiVal         VABANT
131400030721     C                   ELSE
131500030721     C                   SETON                                        32
131600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131700030721     C                             + ' ' + 'VABANT'
131800030721     C                   ENDIF
131900030721     C                   ENDIF
132000030721     C*** CMO
132100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
132200030721     C                   EVAL      PiStr=SkSplitCSV(i)
132300030721     C                   EXSR      CHKNUM
132400030721     C                   IF        PiInt=*on
132500030721     C                   Z-ADD     PiVal         Num5_0
132600030721     C                   MOVEL(p)  Num5_0        VABCMO
132700030721     C                   ELSE
132800030721     C                   SETON                                        32
132900030721     C                   EVAL      VABCMO = *zeros
133000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133100030721     C                             + ' ' + 'VABCMO'
133200030721     C                   ENDIF
133300030721     C                   ENDIF
133400040706     C*
133500060112     C*** CELL
133600060112     C                   IF        %trim(SkSplitFLD(i)) = 'CELL'
133700060112     C                   EVAL      wCELL = SkSplitCSV(i)
133800060112     C                   ENDIF
133900051108     C*** VATNOT TRC=B
134000051108     C                   IF        %trim(SkSplitFLD(i)) = 'TRC_B'
134100051108     C                   EVAL      wVATNOT_B = SkSplitCSV(i)
134200051108     C                   ENDIF
134300060112     C*** VABNOT/VABNT2 NOTE
134400060112     C                   IF        %trim(SkSplitFLD(i)) = 'NOTE'
134500060310     C                   EVAL      wNOTE  = SkSplitCSV(i)
134600060310     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
134700060310     C                   EVAL      VABNT2 = %subst(wNOTE:1+35:35)
134800060112     C                   ENDIF
134900060310     C*** APPUNTAMENTO
135000060310     C                   IF        %trim(SkSplitFLD(i)) = 'APPUNTAM'
135100060310     C                   IF        SkSplitCSV(i) = 'X'
135200060310     C                   EVAL      VABTC1 = 'A'
135300060310     C                   ENDIF
135400060310     C                   ENDIF
135500030715     C*
135600030715     C                   ENDIF
135700030715     C                   EVAL      i = i + 1
135800030715     C                   ENDDO
135900040706     C*
136000040706     C* Se nn valorizzata la provincia destinatario la calcolo tramite TISI95
136100040706     C* Reperisco la provincia dal CAP e dalla località
136200060310     C                   IF        VABPRD =  *blanks AND
136300060310     C                             VABCAD <> *blanks
136400040706     C                   CLEAR                   TISI95DS
136500040706     C                   EVAL      I95TCN = '3'
136600040706     C                   Z-ADD     datcor        I95DAT
136700040706     C                   EVAL      I95CAP = VABCAD
136800040706     C                   EVAL      I95LOC = VABLOD
136900040706     C                   CALL      'TISI95R'
137000040706     C                   PARM                    TISI95DS
137100040706     C                   EVAL      VABPRD = O95PRV
137200060117     C                   EVAL      VABLOD = O95LOC
137300040706     C                   ENDIF
137400060112     C*
137500060112     C* NSP => Stacco un numeratore da AZNUM
137600060112     C                   clear                   TRUL33DS
137700060112     C                   eval      I33OPE = *zeros
137800060112     C                   eval      I33CNU = 302
137900060112     C                   eval      I33NUM = 1
138000060112     C                   movel     TRUL33DS      KPJBU
138100060112     C                   call      'TRUL33R'
138200060112     C                   parm                    KPJBA
138300060112     C                   movel     KPJBU         TRUL33DS
138400060112     C                   if        O33ERR = *zeros
138500060112     C                   z-add     O33NRF        VABNSP
138600060112     C                   z-add     O33NRF        VATNSP
138700060112     C                   else
138800060112     C                   EVAL      vinmsg = %trimr(vinmsg)
138900060112     C                             + ' ' + 'VABNSP VATNSP'
139000060112     C                   endif
139100040719     C*
139200040719     C* Se NSP nn valorizzato considero errore bloccante in traduzione
139300060112     C                   IF        VABNSP = *zeros
139400040719     C                   SETON                                        31
139500040719     C                   ENDIF
139600040719     C*
139700040719     C* Se LNP nn valorizzato considero errore bloccante in traduzione
139800040719     C                   IF        VABLNP = *zeros
139900040719     C                   SETON                                        31
140000040719     C                   ENDIF
140100040706     C*
140200040706     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
140300040706     C* d valorizzarli sempre entrambi
140400040706     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
140500040706     C                   Z-ADD     VABNSP        VABRMN
140600040706     C                   ENDIF
140700040706     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
140800040706     C                   MOVEL     VABRMN        VABRMA
140900040706     C                   ENDIF
141000040113     C*
141100040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
141200060112     C                   IF        FlgCAS = '1'
141300040119     C                   EVAL      VABVCA = 'EUR'
141400040119     C                   IF        VABCBO = '1'
141500040113     C                   EVAL      VABCBO = '4'
141600040119     C                   ENDIF
141700040119     C                   IF        VABCBO = '2'
141800040119     C                   EVAL      VABCBO = '6'
141900040119     C                   ENDIF
142000040113     C                   ENDIF
142100020204     C*
142200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
142300020204     C                   EXSR      CHKIMPDIV
142400030715     C                   ENDIF
142500020204     C*
142600010607     C                   ENDSR
142700010607     C*----------------------------------------------------*
142800010601
142900020204
143000020204     C*----------------------------------------------------*
143100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
143200020204     C*----------------------------------------------------*
143300020204     C     CHKIMPDIV     BEGSR
143400020204     C*
143500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
143600020204     C                   Z-ADD     *zeros        wrkDec            9 9
143700020204     C*
143800020204     C* Come prima cosa effettuo considerazioni sulla divisa
143900020204     C                   IF        vabIAS > *zeros
144000020204     C                   IF        vabVAS <> 'EUR'
144100020204     C                   EVAL      vabVAS =  'ITL'
144200020204     C                   ENDIF
144300020204     C                   ENDIF
144400020204     C*
144500020204     C                   IF        vabCAS > *zeros
144600020204     C                   IF        vabVCA <> 'EUR'
144700020204     C                   EVAL      vabVCA =  'ITL'
144800020204     C                   ENDIF
144900020204     C                   ENDIF
145000020204     C*
145100020204     C                   IF        vabVMD > *zeros
145200020204     C                   IF        vabVAD <> 'EUR'
145300020204     C                   EVAL      vabVAD =  'ITL'
145400020204     C                   ENDIF
145500020204     C                   ENDIF
145600020204     C*
145700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
145800020204     C                   Z-ADD     vabIAS        wrkDec
145900020204     C                   IF        wrkDec > *zeros
146000020204     C                   IF        vabVAS = 'ITL'
146100020204     C                   EVAL      vabIAS = *zeros
146200020204     C                   ENDIF
146300020204     C                   ENDIF
146400020204     C*
146500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
146600020204     C                   Z-ADD     vabCAS        wrkDec
146700020204     C                   IF        wrkDec > *zeros
146800020204     C                   IF        vabVCA = 'ITL'
146900020204     C                   EVAL      vabCAS = *zeros
147000020204     C                   ENDIF
147100020204     C                   ENDIF
147200020204     C*
147300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
147400020204     C                   Z-ADD     vabVMD        wrkDec
147500020204     C                   IF        wrkDec > *zeros
147600020204     C                   IF        vabVAD = 'ITL'
147700020204     C                   EVAL      vabVMD = *zeros
147800020204     C                   ENDIF
147900020204     C                   ENDIF
148000020204     C*
148100020204     C                   ENDSR
148200020204     C***
148300020204
148400010330
148500010330
148600010330     C*----------------------------------------------------*
148700010330     C*  CONTROLLO NUMERICITA' CAMPI
148800010330     C*----------------------------------------------------*
148900010330     C     CHKNUM        BEGSR
149000010330     C*
149100010606     C                   IF        PiDecChr = *blanks
149200030715     C                   EVAL      PiDecChr = CharNUM
149300010606     C                   ENDIF
149400010606     C*
149500010606     C                   CALL(e)   'ISNUMERIC'
149600010330     C                   PARM                    PiStr            30
149700010606     C                   PARM                    PiDecChr          1
149800010330     C                   PARM      *ZEROS        PiVal            30 9
149900010330     C                   PARM      '0'           PiInt             1
150000010330     C                   PARM      '0'           PiNum             1
150100010330     C                   IF        %error
150200010606     C                   EVAL      PiNum=*off
150300010330     C                   ENDIF
150400010330     C*
150500010330     C                   ENDSR
150600010330     C***
150700010330
150800010601
150900010601
151000010601
151100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
151200010601     C     repfil        BEGSR
151300010601     C*
151400010601     C                   if        invfil = *zeros and
151500010601     C                             depfil > *zeros and
151600010629     C                             (vinflg = *blanks or
151700010629     C                              vinflg = *zeros)
151800010601     C
151900010601     C                   eval      invfil = depfil
152000010601     C                   endif
152100010601     C*
152200010601     C                   if        depfil <> invfil and
152300010601     C                             invfil > *zeros
152400010601     C                   eval      flgMulti = '1'
152500010601     C                   if        vinflg = *blanks
152600010601     C                   add       1             cntNonEl
152700010601     C                   endif
152800010601     C                   endif
152900010601     C*
153000010601     C                   if        vinflg = '2'
153100010601     C                   eval      flgStato = '2'
153200010601     C                   endif
153300010601     C*
153400010601     C                   ENDSR
153500010601     C***
153600010601
153700010601
153800010601
153900010330
154000040119      /TITLE Invio dei dati al punto operativo.
154100040119     C     invio         BEGSR
154200040119     C*
154300040119     C* 1° invio FIVAT
154400040119     C                   reset                   dscmz
154500040119     C                   move      vlrpoi        cmzdst
154600040119     C                   eval      cmzfld = 'FIVATWWR'
154700040119     C                   eval      cmzmbd = vlrhdl
154800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
154900040119     C***                if        prmfir = *blanks
155000040119     C                   eval      cmzfla = 'FIVAT00F'
155100040119     C                   eval      cmzmba = 'FIVAT00F'
155200040119     C***                else
155300040119     C***                eval      cmzfla = prmfir
155400040119     C***                eval      cmzmba = prmfir
155500040119     C***                endif
155600040119     C                   eval      cmznrr = *zeros
155700040119     C                   move      §ctrokvt      cmznrr
155800040119     C                   eval      cmzlba = vlrfl1
155900040119     C                   call(e)   'TIS711C'
156000040119     C                   parm                    dscmz
156100040119     C                   parm      *blanks       esito
156200040119     C                   if        %error
156300040119     C                             or cmzerr = '1'
156400040119     C                             or esito  = '1'
156500040119     C                   eval      wrkesito = '3'
156600040119     C                   else
156700040119     C*
156800040119     C* 2° invio FIVAB
156900040119     C                   reset                   dscmz
157000040119     C                   move      vlrpoi        cmzdst
157100040119     C                   eval      cmzfld = vlrfou
157200040119     C                   eval      cmzmbd = vlrhdl
157300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
157400040119     C***                if        prmfir = *blanks
157500040119     C                   eval      cmzfla = 'FIVAB00F'
157600040119     C                   eval      cmzmba = 'FIVAB00F'
157700040119     C***                else
157800040119     C***                eval      cmzfla = prmfir
157900040119     C***                eval      cmzmba = prmfir
158000040119     C***                endif
158100040119     C                   eval      cmznrr = *zeros
158200040119     C                   move      §ctrokvb      cmznrr
158300040119     C                   eval      cmzlba = vlrfl1
158400040119     C                   call(e)   'TIS711C'
158500040119     C                   parm                    dscmz
158600040119     C                   parm      *blanks       esito
158700040119     C                   if        %error
158800040119     C                             or cmzerr = '1'
158900040119     C                             or esito  = '1'
159000040119     C                   eval      wrkesito = '3'
159100040119     C                   endif
159200040119     C                   endif
159300040119     C*
159400040119     C                   ENDSR
159500040119     C***
159600010601
159700010601
159800010601
159900010601
160000010601      /TITLE Invio dei dati al punto operativo.
160100010601     C     opeini        BEGSR
160200010601     C*
160300010601     C* Inizializzo flag e contatori operativi
160400010601     C                   movel     '0'           flgGiro           1
160500010601     C                   movel     '0'           flgMulti          1
160600010601     C                   movel     '1'           flgStato          1
160700010615     C                   movel     '0'           flgOk             1
160800010601     C                   z-add     *zeros        cntNonEl         10 0
160900010601     C                   z-add     *zeros        depfil            3 0
161000010601     C                   z-add     *zeros        invfil            3 0
161100010601     C*
161200010601     C                   ENDSR
161300010601     C***
161400010601
161500010601
161600050421
161700050421
161800070411
161900070411     C     *pssr         BEGSR
162000070411     C*
162100070411     C                   if        %open(tivin00r)
162200070411     C                   close     tivin00r
162300070411     C                   endif
162400070411     C                   if        %open(fivabwwr)
162500070411     C                   close     fivabwwr
162600070411     C                   endif
162700070411     C                   if        %open(fivatwwr)
162800070411     C                   close     fivatwwr
162900070411     C                   endif
163000070411     C*
163100070411     C* Effettuo la chiamata al CLLE preposto
163200070411     C                   call(e)   'TITVVTC'
163300070411     C                   parm                    parccm
163400070411     C                   parm                    parmbr
163500070411     C                   parm      '2'           paropz
163600070411     C*
163700070411     C                   eval      wrkesito = '2'
163800070411     C*
163900070411     C                   seton                                        LR
164000070411     C*
164100070411     C                   ENDSR     '*CANCL'
164200070411     C***
164300050421
164400010601
164500010330
164600010330
164700000613     C     *inzsr        BEGSR
164800990910     C*
164900990910     C     *entry        plist
165000990920     C                   parm                    tivlrds
165100990921     C                   parm      wrkesito      esito
165200000724     C                   parm                    prmlit
165300000710     C                   parm                    prmfir
165400010330     C*
165500010330     C* CALCOLA LA DATA CORRENTE
165600010330     C                   time                    wn14             14 0
165700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
165800010330     C                   z-add     wn8           g08dat
165900010330     C                   z-add     *zeros        g08inv
166000010330     C                   movel     '0'           g08err
166100010330     C                   call      'XSRDA8'
166200010330     C                   parm                    wlbda8
166300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
166400000613     C*
166500000613     C                   ENDSR
166600000613     C***
166700990908
166800060310     Otitvr9p   E            riepilogo         2
166900990915     O                                              'Upload via Internet'
167000990915     O                                           +1 'Traduzione TIVIN00R -'
167100021025     O                                           +1 'FIVABWWR'
167200990915     O                                           +1 'Testate bolle'
167300990915     O          E            riepilogo   2
167400990915     O                       wrkdata
167500990915     O                       wrkora              +1
167600990915     O                       procname            +1
167700990915     O          E            riepilogo   2
167800990915     O                                              'Cliente..................:'
167900010605     O                       VABCCM        z     +1
168000990915     O          E            riepilogo   2
168100990920     O                                              'Riferimento Strategi.....:'
168200990920     O                       vlrhdl              +1
168300990915     O          E            riepilogo   2
168400990915     O                                              'Giusti...................:'
168500040119     O                       §CTROKVB      2   +  1
168600990915     O          E            riepilogo   2
168700990915     O                                              'Sbagliati e corretti.....:'
168800971022     O                       §CTRMO        2   +  1
168900990915     O          E            riepilogo   2
169000990915     O                                              'Sbagliati e scartati.....:'
169100971022     O                       §CTRNO        2   +  1
169200000613
169300060310     Otitvr9ps  E            testdett          2
169400000613     O                                              'Upload via Internet'
169500000613     O                                           +1 'Traduzione TIVIN00R -'
169600021025     O                                           +1 'FIVABWWR'
169700000613     O                                           +1 'Testate bolle'
169800000616     O          E            testdett    3
169900000613     O                                           +2 'N° rec'
170000000613     O                                           +3 'Anteprima contenuto'
170100000616     O          E            rigadett    2
170200000613     O                       rrnum               +2
170300000621     O                       recko               +3
170400000616     O          E            findett     2
170500000613     O                       wrkdata
170600000613     O                       wrkora              +1
170700000613     O                       procname            +1
170800000616     O          E            findett     2
170900000613     O                                              'Cliente..................:'
171000010605     O                       VABCCM        z     +1
171100000616     O          E            findett     2
171200000613     O                                              'Riferimento Strategi.....:'
171300000613     O                       vlrhdl              +1
171400000616     O          E            findett     2
171500000613     O                                              'Giusti...................:'
171600040119     O                       §CTROKVB      2   +  1
171700000616     O          E            findett     2
171800000613     O                                              'Sbagliati e corretti.....:'
171900000613     O                       §CTRMO        2   +  1
172000000616     O          E            findett     2
172100000613     O                                              'Sbagliati e scartati.....:'
172200000613     O                       §CTRNO        2   +  1
172300000613** CMD - COMANDI CL
172400060310OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVR9P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
172500060310OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVR9PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
172600060310DLTOVR FILE(TITVR9P TITVR9PS) LVL(*)
172700010606
172800000613
172900040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
173000060310VABRMN;VABRMA;VABRSD;VABRD2;VABIND;VABCAD;VABLOD;VABPRD;
173100060310TRC_B;CELL;APPUNTAM;NOTE;VABNCL;VABPKB;VABCAS;VABCBO
173200040113
