000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400041130     Fazorg01l  IF   e           K DISK
000500041130     Ftivin00r  UF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800050202     Ftitvh6p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100050202     Ftitvh6ps  O    f  198        PRINTER usropn
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
002200041130     D intcol          S            100    DIM(4) CTDATA PERRCD(1)
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
004600050202     D wVATNOT_DA      s             30  0
004700050202     D wVATNOT_A       s             30  0
004800050202     D wVATNOT         s             30  0
004900041130     D*------------------
005000041130     D* DS REPERIMENTO NUMERATORE
005100041130     D*------------------
005200041130     D trul33ds      e ds                  inz
005300041130     D*------------------
005400041130     D* DS ARCHITETTURA
005500041130     D*------------------
005600041130     D kpjba         e ds                  inz
005700030715     D*------------------
005800041130     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005900030715     D*------------------
006000041201     D SkSplitNOT      S             35    DIM(50)
006100030715     D CharCSV         S              1
006200030715     D CharTXT         S              1
006300030715     D CharNUM         S              1
006400041130     D i               s              5  0 INZ(1)
006500041130     D y               s              3  0 INZ(1)
006600030715     D wGiro           s              1  0 INZ(*zeros)
006700041130     D wOldSep         s              3    INZ(*blanks)
006800041130     D wNewSep         s              3    INZ(*blanks)
006900060626     D wSkParam        s          30000
007000030822     D*------------------
007100030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007200030822     D*------------------
007300030822     D posDaDft        S              3  0 INZ(*zeros)
007400030822     D posADft         S              3  0 INZ(*zeros)
007500030822     D j               s              3  0 INZ(1)
007600030822     D wGiroDft        s              1  0 INZ(*zeros)
007700041130     D*------------------
007800041130     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
007900041130     D*------------------
008000060427     D InStringa       S          10240    VARYING                              (stringa input)
008100041130     D InSepara        S             10                                         (separatore)
008200041130     D OutArray        S               *                                        (array output)
008300041130     D OutErrore       S              1                                         (flag errore)
008400041130     D OutMsg          S             80                                         (messaggio errore)
008500060427     D SkSplitFLD      S             20    DIM(1000) VARYING
008600060427     D SkSplitCSV      S           4096    DIM(1000) VARYING
008601070507     D SkSplitWRK      S           4096    DIM(1000)
008700070507     D*SkSplitWRK      S           4096    DIM(1000) VARYING
008800070507     D*                                    BASED(OutArray)
008900010330     D*------------------
009000010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
009100010330     D*------------------
009200010330     D WLBDA8          DS                  INZ
009300010330     D  G08DAT                 1      8  0
009400010330     D  G08INV                 9     16  0
009500010330     D  G08ERR                17     17
009600010330     D  G08TGI                18     22  0
009700010330
009800010330
009900010330
010000990915     C                   time                    wrkdata
010100990915     C                   time                    wrkora
010200000913     C                   reset                   rrnum
010300990921     C                   reset                   esito
010400990921     C                   reset                   wrkesito
010500010601     C*
010600010601     C                   exsr      opeini
010700010605     C                   exsr      rwvab
010800010601     C*
010900010601     C                   seton                                        lr
011000010601
011100010601
011200010601
011300010601     C*--------------------------------------------------------
011400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011500010601     C*--------------------------------------------------------
011600010601     C     PREELA        BEGSR
011700010601     C*
011800010601     C* SE OCCORRE SPEDIRE IN FILIALE
011900010601     C                   if        invfil <> *zeros and
012000010601     C                             flgGiro = '0'
012100010601     C*
012200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012300010601     C                   eval      flgGiro = '1'
012400000724     C*
012500000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
012600000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012700010601     C     invfil        chain     azorg01l
012800000724     C                   if        %found
012900000724     C                   movel(p)  CMD(1)        depcmd
013000020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
013100000724     C*
013200000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
013300011113     C                   Z-ADD     150           LENGH            15 5
013400000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013500000724     C                   PARM                    depcmd
013600000724     C                   PARM                    LENGH
013700000724     C*
013800000724     C                   endif
013900000616     C*
014000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014100000616     C                   movel(p)  CMD(2)        depcmd
014200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
014300000616     C*
014400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
014500011113     C                   Z-ADD     150           LENGH            15 5
014600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014700000616     C                   PARM                    depcmd
014800000616     C                   PARM                    LENGH
014900000613     C*
015000050202     C                   if        not %open(titvh6ps)
015100050202     C                   open      titvh6ps
015200030715     C                   except    testdett
015300000613     C                   endif
015400010601     C*
015500010601     C                   endif
015600010601     C*
015700010601     C                   ENDSR
015800010601     C***
015900010601
016000010601
016100010601
016200010601
016300010601     C*--------------------------------------------------------
016400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016500010601     C*--------------------------------------------------------
016600010601     C     ENDELA        BEGSR
016700990915     C*
016800000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
016900040119     C*
017000040119     C* Effettuo la chiamata al CLLE preposto
017100040119     C                   call(e)   'TITVVTC'
017200040119     C                   parm                    parccm
017300040119     C                   parm                    parmbr
017400040119     C                   parm      '2'           paropz
017500050201     C*
017600050201     C* Effettuo lancio TISI95 solo x chiusura
017700050201     C                   CLEAR                   TISI95DS
017800050201     C                   EVAL      I95TLA = 'C'
017900050201     C                   CALL      'TISI95R'
018000050201     C                   PARM                    TISI95DS
018100060428     C*
018200060428     C* Effettuo lancio XSPLIT solo x chiusura
018300060428     C                   CALL(e)   'XSPLIT'
018400060428     C                   PARM      *blanks       InStringa
018500060428     C                   PARM                    InSepara
018600060626     C                   PARM                    wSkParam
018700060428     C                   PARM                    OutErrore
018800060428     C                   PARM                    OutMsg
018900000613     C*
019000050202     C                   if        %open(titvh6ps)
019100000616     C                   except    findett
019200050202     C                   close     titvh6ps
019300000613     C                   endif
019400000616     C*
019500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
019600011113     C                   Z-ADD     150           LENGH            15 5
019700010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
019800000616     C                   PARM                    CMD(3)
019900000616     C                   PARM                    LENGH
020000000616     C*
020100010601     C                   ENDSR
020200010601     C***
020300010601
020400010601
020500010601
020600000613
020700000613     C*--------------------------------------------------------
020800000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
020900000613     C*--------------------------------------------------------
021000000621     C     STPR          BEGSR
021100000613     C*
021200050202     C                   if        not %open(titvh6p)
021300050202     C   50              open      titvh6p
021400990915     C                   endif
021500990915     C*
021600030715     C   50              except    riepilogo
021700990915     C*
021800050202     C                   if        %open(titvh6p)
021900050202     C   50              close     titvh6p
022000990914     C                   endif
022100000613     C*
022200000613     C                   ENDSR
022300000613     C***
022400000613
022500010601
022600010601
022700010330     C*--------------------------------------------------------
022800021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
022900010330     C*--------------------------------------------------------
023000010605     C     RWVAB         BEGSR
023100010330     C*
023200010330     C                   if        not %open(tivin00r)
023300010330     C                   open      tivin00r
023400010330     C                   endif
023500021025     C                   if        not %open(fivabwwr)
023600021025     C                   open      fivabwwr
023700010330     C                   endif
023800040119     C*
023900040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
024000040119     C                   exsr      prevat
024100040119     C*
024200040119     C                   if        chkcall = '0'
024300040119     C*
024400040119     C                   if        not %open(fivatwwr)
024500040119     C                   open      fivatwwr
024600040119     C                   endif
024700010330     C*
024800040119     C                   clear                   §CTROKVB          7 0
024900040119     C                   clear                   §CTROKVT          7 0
025000040119     C                   clear                   §CTRMO            7 0
025100040119     C                   clear                   §CTRNO            7 0
025200040706     C*
025300040706     C  N70              do        *HIVAL
025400010330     C*
025500040706     C                   read      tivin00r                               70
025600010618     C*
025700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
025800010618     C                   if        vinflg = '1'
025900010618     C                   eval      flgOk = '1'
026000010618     C                   endif
026100010618     C*
026200040316     C                   if        (vindta > *blanks AND
026300040316     C                              %trim(vindta) <> *all';') OR
026400040316     C                             wGiro = *zeros
026500010330     C                   add       1             rrnum
026600010330     C*
026700010601     C                   if        *in70 = *off and
026800010330     C                             (vinflg = *blanks
026900010330     C                              or vinflg = '0'
027000010330     C                              or vinflg = '2')
027100010330     C*
027200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
027300010711     C                   if        vinflg = *blanks or vinflg = '0'
027400010711     C                   clear                   vinmsg
027500010711     C                   endif
027600010601     C*
027700010330     C                   exsr      inzvar
027800010605     C                   exsr      impvab
027900010601     C*
028000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
028100010605     C                   eval      depfil = VABLNP
028200010601     C                   exsr      repfil
028300010601     C                   if        depfil = invfil
028400021025     C                   if        vlrpoi = 999
028500030822     C                   move(P)   invfil        VABFGS
028600040119     C                   move(P)   invfil        VATFGS
028700021025     C                   else
028800030822     C                   move(P)   vlrpoi        VABFGS
028900040119     C                   move(P)   vlrpoi        VATFGS
029000021025     C                   endif
029100010601     C*
029200010601     C                   exsr      PREELA
029300010601     C*
029400010604     C* Ebbene...
029500010604     C*
029600040119     C  N31              add       1             §CTROKVB
029700040119     C   32              add       1             §CTRMO
029800040119     C   31              add       1             §CTRNO
029900030822     C                   if        wGiro = 2
030000040119     C                   if        depspe =  *zeros OR
030100040119     C                             depspe <> vabNSP
030200040119     C  N31              exsr      wrivab
030300040119     C  N31              eval      depspe = vabNSP
030400040119     C                   endif
030500040119     C                   endif
030600010604     C*
030700010604     C                   if        *in31 = *off and
030800010604     C                             *in32 = *off
030900010604     C                   eval      vinflg = '1'
031000010604     C                   else
031100010604     C                   eval      recko = vindta
031200010604     C                   except    rigadett
031300010604     C                   eval      vinflg = '2'
031400010604     C                   endif
031500010604     C                   endif
031600010601     C*
031700010604     C                   endif
031800010604     C*
031900010330     C                   else
032000010330     C                   eval      vinflg = '1'
032100010330     C                   endif
032200010601     C*
032300010601     C  N70              update    tivin000
032400010330     C*
032500030822     C  N70              enddo
032600040119     C*
032700040119     C                   endif                                                  (endif - chkcall)
032800010601     C*
032900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
033000010601     C                   if        cntNonEl = *zeros or
033100010601     C                             flgMulti = '0'
033200010330     C* Se non ci sono record con errori ...
033300010601     C                   if        §ctrno = 0 and
033400010604     C                             §ctrmo = 0 and
033500010601     C                             flgStato <> '2'
033600010330     C* ... restituisco esito OK.
033700010330     C                   eval      wrkesito = '0'
033800010330     C                   else
033900040119     C                   if        §ctrokvb > 0
034000010330     C                   eval      wrkesito = '1'
034100010330     C                   else
034200010615     C                   if        flgOk = '0'
034300010615     C                   eval      wrkesito = '2'
034400010615     C                   else
034500010615     C                   eval      wrkesito = '6'
034600010615     C                   endif
034700010330     C                   endif
034800010330     C                   endif
034900010601     C                   else
035000010601     C                   eval      wrkesito = '9'
035100010601     C                   endif
035200010330     C*
035300010330     C                   if        %open(tivin00r)
035400010330     C                   close     tivin00r
035500010330     C                   endif
035600021025     C                   if        %open(fivabwwr)
035700021025     C                   close     fivabwwr
035800010330     C                   endif
035900040512     C                   if        %open(fivatwwr)
036000040512     C                   close     fivatwwr
036100040512     C                   endif
036200010601     C*
036300010601     C                   if        vlrpoi <> 999
036400010601     C                   eval      invfil = vlrpoi
036500010601     C                   endif
036600010330     C*
036700040119     C                   if        §ctrokvb > 0
036800010601     C                             and invfil > *zeros
036900010330     C                   exsr      invio
037000010330     C                   endif
037100010601     C*
037200010618     C                   if        flgGiro = '1'
037300010601     C                   exsr      endela
037400010618     C                   endif
037500010330     C*
037600010330     C                   ENDSR
037700010330     C***
037800040119
037900040119
038000040119
038100040119     C*----------------------------------------------------*
038200050202     C*  SCARICAMENTO BUFFER RECORDS VAB
038300040119     C*----------------------------------------------------*
038400040119     C     WRIVAB        BEGSR
038500040119     C*
038600040119     C                   write     fivab000                                     => scarico il VAB
038700040119     C*
038800040119     C                   ENDSR
038900040119
039000040119
039100040119
039200040119     C*----------------------------------------------------*
039300040119     C*  SCARICAMENTO BUFFER RECORDS VAT
039400040119     C*----------------------------------------------------*
039500041130     C     WRIVAT        BEGSR
039600040706     C*
039700040719     C* Attribuisco la stessa chiave della testata bolle
039800050224     C                   EVAL      VATFGS = VABFGS
039900040719     C                   EVAL      VATAAS = VABAAS
040000040719     C                   EVAL      VATLNP = VABLNP
040100050224     C                   EVAL      VATNRS = VABNRS
040200040719     C                   EVAL      VATNSP = VABNSP
040300040719     C*
040400041130     C* Imposto il tipo record sul file VAT a 'E' x indicare i "CHI SONO"
040500041130     C                   EVAL      VATTRC = 'E'
040600050202     C*
040700050202     C* Scarico il buffer così valorizzato
040800041130     C                   WRITE     FIVAT000
040900040119     C*
041000040119     C                   ENDSR
041100040119
041200040119
041300040119
041400040119     C*----------------------------------------------------*
041500040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
041600040119     C*----------------------------------------------------*
041700040119     C     PREVAT        BEGSR
041800040119     C*
041900040119     C* Compongo il nome del membro da dare al FIVATWWR
042000040119     C                   eval      parmbr = vlrhdl
042100040119     C                   movel     'M'           parmbr
042200041130     C                   eval      parccm = vlrksc
042300040119     C                   eval      paropz = '1'
042400040119     C* Effettuo la chiamata al CLLE preposto
042500040119     C                   call(e)   'TITVVTC'
042600040119     C                   parm                    parccm
042700040119     C                   parm                    parmbr
042800040119     C                   parm                    paropz
042900040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
043000040119     C                   if        %error
043100040119     C                   movel     '1'           chkcall
043200040119     C                   else
043300040119     C                   movel     '0'           chkcall
043400040119     C                   endif
043500040119     C*
043600040119     C                   ENDSR
043700010601
043800010601
043900010601
044000010330     C*----------------------------------------------------*
044100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
044200010330     C*----------------------------------------------------*
044300040119     C     INZVAR        BEGSR
044400010330     C*
044500030822     C* Inizializzo variabili di wrk
044600020204     C                   Z-ADD     *zeros        Num5_0            5 0
044700041201     C                   Z-ADD     *zeros        wNumVAT           3 0
044800030822     C*
044900030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
045000030822     C                   CLEAR                   FIVAB000
045100040119     C                   CLEAR                   FIVAT000
045200030822     C                   CLEAR                   SkSplitCSV
045300041130     C                   CLEAR                   SkSplitNOT
045400030822     C                   EVAL      i = 1
045500030822     C*
045600030822     C* Reimposto i valori di default
045700030822     C                   EXSR      DEFCAM
045800030822     C*
045900010330     C                   ENDSR
046000010330     C*----------------------------------------------------*
046100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
046200010330     C*----------------------------------------------------*
046300010330     C     DEFCAM        BEGSR
046400040119     C*
046500040119     C* Imposto i valori d default a programma dei campi fondamentali
046600050301     C                   EVAL      VABCCM = 0660576
046700050301     C                   EVAL      VATCCM = 0660576
046800050301     C                   EVAL      VABLNP = 066
046900050301     C                   EVAL      VATLNP = 066
047000040706     C                   EVAL      VABCTR = 000
047100050224     C                   EVAL      VABCTM = '7Q'
047200040119     C                   EVAL      VABCBO = '1'
047300041130     C                   EVAL      VABTSP = 'C'
047400041130     C                   EVAL      VATTRC = 'E'
047500050301     C                   EVAL      VABFGS = 066
047600050301     C                   EVAL      VATFGS = 066
047700030822     C*
047800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
047900030715     C* e delimitatore testo.
048000041130     C                   EVAL      CharCSV = ','
048100041130     C                   EVAL      CharTXT = '"'
048200041130     C                   EVAL      CharNUM = '.'
048300041130     C*
048400041130     C* Compongo la sequenza separatore testo + separatore campo
048500041130     C                   EVAL      wOldSep = CharTXT + CharCSV + CharTXT
048600041130     C                   EVAL      wNewSep = CharTXT + ';' + CharTXT
048700030822     C*
048800030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
048900030715     C                   IF        %subst(vlrppt:1:1) = 'S'
049000030715     C                   SETON                                        50
049100030715     C                   ELSE
049200030715     C                   SETOFF                                       50
049300030715     C                   ENDIF
049400030822     C*
049500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
049600030822     C                   EVAL      posDaDft = 1
049700030822     C                   EVAL      posADft  = 0
049800030822     C                   EVAL      wGiroDft = 0
049900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
050000030822     C                             posDaDft > 0
050100030822     C*
050200030822     C* Gestisco il 1° giro
050300030822     C                   IF        wGiroDft = 0
050400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
050500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
050600030822     C* Incremento il contatore dei "giri"
050700030822     C                   EVAL      wGiroDft = 1
050800030822     C                   ELSE
050900030822     C                   EVAL      posDaDft = posADft
051000030822     C                   ENDIF
051100030822     C* Eseguo lo scan x trovare la fine del campo corrente
051200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
051300030822     C*
051400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
051500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
051600030822     C                   IF        posDaDft > 0 AND
051700030822     C                             posADft  > 0
051800030822     C* NCL
051900030822     C                   IF        %subst(
052000030822     C                             %subst(vlrppt:posDaDft+1:
052100030822     C                             posADft-posDaDft-1):1:3)
052200030822     C                             = 'NCL'
052300030822     C                   EVAL      PiStr=%trim(%subst(
052400030822     C                             %subst(vlrppt:posDaDft+1:
052500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052600030822     C                   EXSR      CHKNUM
052700030822     C                   IF        PiInt=*on
052800030822     C                   Z-ADD     PiVal         VABNCL
052900030822     C                   ENDIF
053000030822     C                   ENDIF
053100030822     C* CCM
053200030822     C                   IF        %subst(
053300030822     C                             %subst(vlrppt:posDaDft+1:
053400030822     C                             posADft-posDaDft-1):1:3)
053500030822     C                             = 'CCM'
053600030822     C                   EVAL      PiStr=%trim(%subst(
053700030822     C                             %subst(vlrppt:posDaDft+1:
053800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053900030822     C                   EXSR      CHKNUM
054000030822     C                   IF        PiInt=*on
054100030822     C                   Z-ADD     PiVal         VABCCM
054200040119     C                   Z-ADD     PiVal         VATCCM
054300030822     C                   ENDIF
054400030822     C                   ENDIF
054500030822     C* LNP
054600030822     C                   IF        %subst(
054700030822     C                             %subst(vlrppt:posDaDft+1:
054800030822     C                             posADft-posDaDft-1):1:3)
054900030822     C                             = 'LNP'
055000030822     C                   EVAL      PiStr=%trim(%subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055300030822     C                   EXSR      CHKNUM
055400030822     C                   IF        PiInt=*on
055500030822     C                   Z-ADD     PiVal         VABLNP
055600040119     C                   Z-ADD     PiVal         VATLNP
055700030822     C                   ENDIF
055800030822     C                   ENDIF
055900030822     C* NRS
056000030822     C                   IF        %subst(
056100030822     C                             %subst(vlrppt:posDaDft+1:
056200030822     C                             posADft-posDaDft-1):1:3)
056300030822     C                             = 'NRS'
056400030822     C                   EVAL      PiStr=%trim(%subst(
056500030822     C                             %subst(vlrppt:posDaDft+1:
056600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056700030822     C                   EXSR      CHKNUM
056800030822     C                   IF        PiInt=*on
056900030822     C                   Z-ADD     PiVal         VABNRS
057000030822     C                   ENDIF
057100030822     C                   ENDIF
057200030822     C* CTR
057300030822     C                   IF        %subst(
057400030822     C                             %subst(vlrppt:posDaDft+1:
057500030822     C                             posADft-posDaDft-1):1:3)
057600030822     C                             = 'CTR'
057700030822     C                   EVAL      PiStr=%trim(%subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058000030822     C                   EXSR      CHKNUM
058100030822     C                   IF        PiInt=*on
058200030822     C                   Z-ADD     PiVal         VABCTR
058300030822     C                   ENDIF
058400030822     C                   ENDIF
058500030822     C* PKB
058600030822     C                   IF        %subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):1:3)
058900030822     C                             = 'PKB'
059000030822     C                   EVAL      PiStr=%trim(%subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059300030822     C                   EXSR      CHKNUM
059400030822     C                   IF        PiNum=*on
059500030822     C                   Z-ADD     PiVal         VABPKB
059600030822     C                   ENDIF
059700030822     C                   ENDIF
059800030822     C* VLB
059900030822     C                   IF        %subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):1:3)
060200030822     C                             = 'VLB'
060300030822     C                   EVAL      PiStr=%trim(%subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060600030822     C                   EXSR      CHKNUM
060700030822     C                   IF        PiNum=*on
060800030822     C                   Z-ADD     PiVal         VABVLB
060900030822     C                   ENDIF
061000030822     C                   ENDIF
061100030822     C* QFT
061200030822     C                   IF        %subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):1:3)
061500030822     C                             = 'QFT'
061600030822     C                   EVAL      PiStr=%trim(%subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061900030822     C                   EXSR      CHKNUM
062000030822     C                   IF        PiNum=*on
062100030822     C                   Z-ADD     PiVal         VABQFT
062200030822     C                   ENDIF
062300030822     C                   ENDIF
062400030822     C* CBO
062500030822     C                   IF        %subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):1:3)
062800030822     C                             = 'CBO'
062900030822     C                   EVAL      VABCBO=%trim(%subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063200030822     C                   ENDIF
063300030822     C* TSP
063400030822     C                   IF        %subst(
063500030822     C                             %subst(vlrppt:posDaDft+1:
063600030822     C                             posADft-posDaDft-1):1:3)
063700030822     C                             = 'TSP'
063800030822     C                   EVAL      VABTSP=%trim(%subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064100030822     C                   ENDIF
064200030822     C* VAS
064300030822     C                   IF        %subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):1:3)
064600030822     C                             = 'VAS'
064700030822     C                   EVAL      VABVAS=%trim(%subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065000030822     C                   ENDIF
065100030822     C* VCA
065200030822     C                   IF        %subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):1:3)
065500030822     C                             = 'VCA'
065600030822     C                   EVAL      VABVCA=%trim(%subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065900030822     C                   ENDIF
066000030822     C* GCA
066100030822     C                   IF        %subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):1:3)
066400030822     C                             = 'GCA'
066500030822     C                   EVAL      VABGCA=%trim(%subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066800030822     C                   ENDIF
066900030822     C* CTM
067000030822     C                   IF        %subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):1:3)
067300030822     C                             = 'CTM'
067400030822     C                   EVAL      VABCTM=%trim(%subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700030822     C                   ENDIF
067800030822     C* FFD
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'FFD'
068300030822     C                   EVAL      VABFFD=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   ENDIF
068700030822     C* VAD
068800030822     C                   IF        %subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):1:3)
069100030822     C                             = 'VAD'
069200030822     C                   EVAL      VABVAD=%trim(%subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500030822     C                   ENDIF
069600030822     C* GMA
069700030822     C                   IF        %subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):1:3)
070000030822     C                             = 'GMA'
070100030822     C                   EVAL      VABGMA=%trim(%subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400030822     C                   ENDIF
070500030822     C* GGA
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'GGA'
071000030822     C                   EVAL      VABGGA=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   ENDIF
071400030822     C* GVA
071500030822     C                   IF        %subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):1:3)
071800030822     C                             = 'GVA'
071900030822     C                   EVAL      VABGVA=%trim(%subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072200030822     C                   ENDIF
072300030822     C* TC1
072400030822     C                   IF        %subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):1:3)
072700030822     C                             = 'TC1'
072800030822     C                   EVAL      VABTC1=%trim(%subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100030822     C                   ENDIF
073200030822     C* TC2
073300030822     C                   IF        %subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):1:3)
073600030822     C                             = 'TC2'
073700030822     C                   EVAL      VABTC2=%trim(%subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000030822     C                   ENDIF
074100030822     C* ...
074200030822     C                   ENDIF
074300030822     C                   ENDDO
074400020204     C*
074500010330     C                   ENDSR
074600010607     C*----------------------------------------------------*
074700021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
074800010607     C*----------------------------------------------------*
074900010607     C     IMPVAB        BEGSR
075000010607     C*
075100010607     C                   SETOFF                                       3132
075200050202     C                   Z-ADD     *zeros        wVATNOT_DA
075300050202     C                   Z-ADD     *zeros        wVATNOT_A
075400030515     C*
075500010607     C                   MOVEL     datcor        VABAAS
075600040119     C                   MOVEL     datcor        VATAAS
075700010607     C                   MOVE      datcor        VABMGS
075800041130     C*
075900041130     C* Salvo sempre la riga originale x le eventuali successive sovrapposizioni
076000041130     C                   EVAL      depvinDTA = vinDTA
076100041130     C*
076200041130     C* Dato che c'è 1 conflitto d separatori campi/barcodes => sostituisco quello campi
076300041130     C                   EVAL      CharCSV = ';'
076400041130     C*
076500041130     C                   Z-ADD     1             wj                5 0
076600041130     C                   DOW       wj <= %len(vindta) - %len(wOldSep)
076700041130     C                   IF        %subst(vindta:wj:3) = wOldSep
076800041130     C                   EVAL      %subst(vindta:wj:3) = wNewSep
076900041130     C                   ENDIF
077000041130     C                   ADD       1             wj
077100041130     C                   ENDDO
077200010607     C*
077300030715     C**********
077400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
077500030715     C**********
077600030715     C                   IF        wGiro = *zeros
077700040113     C*
077800041130     C* Se necessario inserisco qui la forzatura della intesatazione colonne personalizzata
077900041130     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
078000041130     C                             %trim(intcol(3))+%trim(intcol(4)) <> *blanks
078100041130     C*
078200041130     C* Effettuo la sovrapposizione d intestazioni
078300041130     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
078400041130     C                                      %trim(intcol(3))+%trim(intcol(4))
078500041130     C                   ENDIF
078600041130     C*
078700041130     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
078800041130     C                   CALL      'XSPLIT'
078900041130     C                   PARM      vindta        InStringa
079000041130     C                   PARM      CharCSV       InSepara
079100060626     C                   PARM                    wSkParam
079200041130     C                   PARM                    OutErrore
079300041130     C                   PARM                    OutMsg
079400060626     C                   MOVEA     wSkParam      SkSplitWRK
079500041130     C*
079600041201     C* Valorizzo la schiera delle intestazioni colonne (elementi)
079700041130     C                   EVAL      i = 1
079800041130     C                   DOW       i <= %elem(SkSplitWRK)
079900041130     C                   EVAL      SkSplitFLD(i) = SkSplitWRK(i)
080000041130     C                   ADD       1             i
080100041130     C                   ENDDO
080200030715     C*
080300041130     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
080400030715     C* delimitazione testo
080500030715     C                   EVAL      i = 1
080600041130     C                   DOW       i <= %elem(SkSplitFLD)
080700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
080800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
080900030715     C                   EVAL      i = i + 1
081000030715     C                   ENDDO
081100030715     C                   EVAL      wGiro = 1
081200030715     C                   ELSE
081300030715     C                   EVAL      wGiro = 2
081400030715     C**********
081500030715     C* Effettuo lo split del campo dati d input
081600030715     C**********
081700041130     C*
081800041130     C* Chiamo il *pgm d utilità x lo "split" della riga dati corrente
081900041130     C                   CALL(e)   'XSPLIT'
082000041130     C                   PARM      vindta        InStringa
082100041130     C                   PARM      CharCSV       InSepara
082200060626     C                   PARM                    wSkParam
082300041130     C                   PARM                    OutErrore
082400041130     C                   PARM                    OutMsg
082500060626     C                   MOVEA     wSkParam      SkSplitWRK
082600060626     C                   MOVEL     SkSplitWRK    SkSplitCSV
082700030715     C*
082800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
082900030715     C* delimitazione testo
083000030715     C                   EVAL      i = 1
083100041130     C                   DOW       i <= %elem(SkSplitCSV)
083200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
083300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
083400030715     C                   EVAL      i = i + 1
083500030715     C                   ENDDO
083600030715     C*
083700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
083800030715     C                   EVAL      i = 1
083900041130     C                   DOW       i <= %elem(SkSplitFLD)
084000030715     C*
084100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
084200041201     C                   IF        SkSplitFLD(i) <> *blanks
084300030721     C*** ATB
084400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
084500030721     C                   EVAL      VABATB = SkSplitCSV(i)
084600030721     C                   ENDIF
084700030715     C*** RSD
084800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
084900030715     C                   EVAL      VABRSD = SkSplitCSV(i)
085000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
085100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
085200030715     C* ==
085300030715     C                   ENDIF
085400030721     C*** RD2
085500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
085600030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
085700030721     C                   ENDIF
085800030715     C*** IND
085900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
086000030715     C                   EVAL      VABIND = SkSplitCSV(i)
086100030715     C                   ENDIF
086200030715     C*** LOD
086300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
086400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
086500030715     C                   ENDIF
086600030715     C*** PRD
086700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
086800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
086900030715     C                   ENDIF
087000030721     C*** NZD
087100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
087200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
087300030721     C                   ENDIF
087400030715     C*** RMA
087500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
087600030715     C                   EVAL      VABRMA = SkSplitCSV(i)
087700030715     C                   ENDIF
087800030715     C*** NOT
087900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
088000030715     C                   EVAL      VABNOT = SkSplitCSV(i)
088100030715     C                   ENDIF
088200030715     C*** NT2
088300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
088400030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
088500030715     C                   ENDIF
088600030715     C*** VCA
088700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
088800030715     C                   EVAL      VABVCA = SkSplitCSV(i)
088900030715     C                   ENDIF
089000030715     C*** VAS
089100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
089200030715     C                   EVAL      VABVAS = SkSplitCSV(i)
089300030715     C                   ENDIF
089400030715     C*** FFD
089500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
089600030715     C                   EVAL      VABFFD = SkSplitCSV(i)
089700030715     C                   ENDIF
089800030721     C*** GC1
089900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
090000030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
090100030721     C                   ENDIF
090200030721     C*** GC2
090300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
090400030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
090500030721     C                   ENDIF
090600030721     C*** TSP
090700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
090800030721     C                   EVAL      VABTSP = SkSplitCSV(i)
090900030721     C                   ENDIF
091000030721     C*** NAS
091100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
091200030721     C                   EVAL      VABNAS = SkSplitCSV(i)
091300030721     C                   ENDIF
091400030721     C*** TIC
091500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
091600030721     C                   EVAL      VABTIC = SkSplitCSV(i)
091700030721     C                   ENDIF
091800030721     C*** GCA
091900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
092000030721     C                   EVAL      VABGCA = SkSplitCSV(i)
092100030721     C                   ENDIF
092200030721     C*** XCO
092300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
092400030721     C                   EVAL      VABXCO = SkSplitCSV(i)
092500030721     C                   ENDIF
092600030721     C*** CTM
092700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
092800030721     C                   EVAL      VABCTM = SkSplitCSV(i)
092900030721     C                   ENDIF
093000030721     C*** TCR
093100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
093200030721     C                   EVAL      VABTCR = SkSplitCSV(i)
093300030721     C                   ENDIF
093400030721     C*** CTS
093500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
093600030721     C                   EVAL      VABCTS = SkSplitCSV(i)
093700030721     C                   ENDIF
093800030721     C*** FTM
093900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
094000030721     C                   EVAL      VABFTM = SkSplitCSV(i)
094100030721     C                   ENDIF
094200030721     C*** VAD
094300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
094400030721     C                   EVAL      VABVAD = SkSplitCSV(i)
094500030721     C                   ENDIF
094600030721     C*** GMA
094700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
094800030721     C                   EVAL      VABGMA = SkSplitCSV(i)
094900030721     C                   ENDIF
095000030721     C*** GGA
095100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
095200030721     C                   EVAL      VABGGA = SkSplitCSV(i)
095300030721     C                   ENDIF
095400030721     C*** GVA
095500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
095600030721     C                   EVAL      VABGVA = SkSplitCSV(i)
095700030721     C                   ENDIF
095800030721     C*** TC1
095900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
096000030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
096100030721     C                   ENDIF
096200030721     C*** TC2
096300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
096400030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
096500030721     C                   ENDIF
096600030721     C*** SCL
096700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
096800030721     C                   EVAL      VABSCL = SkSplitCSV(i)
096900030721     C                   ENDIF
097000030721     C*** RMO
097100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
097200030721     C                   EVAL      VABRMO = SkSplitCSV(i)
097300030721     C                   ENDIF
097400030721     C*** NMO
097500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
097600030721     C                   EVAL      VABNMO = SkSplitCSV(i)
097700030721     C                   ENDIF
097800041130     C*
097900050224     C* Reperisco quindi i campi numerici...
098000040706     C*** NSP
098100040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
098200040706     C                   EVAL      PiStr=SkSplitCSV(i)
098300040706     C                   EXSR      CHKNUM
098400041130     C                   IF        PiInt=*on AND PiVal <> *zeros
098500040706     C                   Z-ADD     PiVal         VABNSP
098600040706     C                   ENDIF
098700040706     C                   ENDIF
098800040706     C*** RMN
098900040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
099000040706     C                   EVAL      PiStr=SkSplitCSV(i)
099100040706     C                   EXSR      CHKNUM
099200040706     C                   IF        PiInt=*on
099300040706     C                   Z-ADD     PiVal         VABRMN
099400040706     C                   ELSE
099500040706     C                   SETON                                        32
099600040706     C                   EVAL      VABRMN = 1
099700040706     C                   EVAL      vinmsg = %trimr(vinmsg)
099800040706     C                             + ' ' + 'VABRMN'
099900040706     C                   ENDIF
100000040706     C                   ENDIF
100100030721     C*** CAD
100200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
100300030721     C                   EVAL      PiStr=SkSplitCSV(i)
100400030721     C                   EXSR      CHKNUM
100500030721     C                   IF        PiInt=*on
100600030721     C                   Z-ADD     PiVal         Num5_0
100700030721     C                   MOVEL(p)  Num5_0        VABCAD
100800030721     C                   ELSE
100900030721     C                   SETON                                        32
101000030721     C                   EVAL      VABCAD = *zeros
101100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101200030721     C                             + ' ' + 'VABCAD'
101300030721     C                   ENDIF
101400030721     C                   ENDIF
101500030721     C*** NCL
101600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
101700030721     C                   EVAL      PiStr=SkSplitCSV(i)
101800030721     C                   EXSR      CHKNUM
101900030721     C                   IF        PiInt=*on
102000030721     C                   Z-ADD     PiVal         VABNCL
102100030721     C                   ELSE
102200030721     C                   SETON                                        32
102300030721     C                   EVAL      VABNCL = *zeros
102400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102500040706     C                             + ' ' + 'VABNCL'
102600030721     C                   ENDIF
102700030721     C                   ENDIF
102800030721     C*** PKB
102900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
103000030721     C                   EVAL      PiStr=SkSplitCSV(i)
103100030721     C                   EXSR      CHKNUM
103200030721     C                   IF        PiNum=*on
103300030721     C                   Z-ADD     PiVal         VABPKB
103400030721     C                   ELSE
103500030721     C                   SETON                                        32
103600030721     C                   EVAL      VABPKB = *zeros
103700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103800030721     C                             + ' ' + 'VABPKB'
103900030721     C                   ENDIF
104000030721     C                   ENDIF
104100030721     C*** CAS
104200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
104300030721     C                   EVAL      PiStr=SkSplitCSV(i)
104400030721     C                   EXSR      CHKNUM
104500030721     C                   IF        PiNum=*on
104600030721     C                   Z-ADD     PiVal         VABCAS
104700030721     C                   ELSE
104800030721     C                   SETON                                        32
104900030721     C                   EVAL      VABCAS = *zeros
105000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105100030721     C                             + ' ' + 'VABCAS'
105200030721     C                   ENDIF
105300030721     C                   ENDIF
105400030721     C*** LNP
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
105600030721     C                   EVAL      PiStr=SkSplitCSV(i)
105700030721     C                   EXSR      CHKNUM
105800030721     C                   IF        PiInt=*on    AND
105900030721     C                             PiVal<=999   AND
106000030721     C                             PiVal>*zeros
106100030721     C                   Z-ADD     PiVal         VABLNP
106200040119     C                   Z-ADD     PiVal         VATLNP
106300030721     C                   ELSE
106400030721     C                   SETON                                        32
106500030721     C                   EVAL      VABLNP = *zeros
106600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106700040119     C                             + ' ' + 'VABLNP VATLNP'
106800030721     C                   ENDIF
106900030721     C                   ENDIF
107000030721     C*** NRS
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
107200030721     C                   EVAL      PiStr=SkSplitCSV(i)
107300030721     C                   EXSR      CHKNUM
107400030721     C                   IF        PiInt=*on AND
107500030721     C                             PiVal<=99
107600030721     C                   Z-ADD     PiVal         VABNRS
107700030721     C                   ELSE
107800030721     C                   SETON                                        32
107900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108000030721     C                             + ' ' + 'VABNRS'
108100030721     C                   ENDIF
108200030721     C                   ENDIF
108300030721     C*** LNA
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
108500030721     C                   EVAL      PiStr=SkSplitCSV(i)
108600030721     C                   EXSR      CHKNUM
108700030721     C                   IF        PiInt=*on    AND
108800030721     C                             PiVal<=999   AND
108900030721     C                             PiVal>*zeros
109000030721     C                   Z-ADD     PiVal         VABLNA
109100030721     C                   ELSE
109200030721     C                   SETON                                        32
109300030721     C                   EVAL      VABLNA = *zeros
109400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109500030721     C                             + ' ' + 'VABLNA'
109600030721     C                   ENDIF
109700030721     C                   ENDIF
109800030721     C*** CTR
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
110000030721     C                   EVAL      PiStr=SkSplitCSV(i)
110100030721     C                   EXSR      CHKNUM
110200030721     C                   IF        PiInt=*on  AND
110300030721     C                             PiVal<=999
110400030721     C                   Z-ADD     PiVal         VABCTR
110500030721     C                   ELSE
110600030721     C                   SETON                                        32
110700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110800030721     C                             + ' ' + 'VABCTR'
110900030721     C                   ENDIF
111000030721     C                   ENDIF
111100030721     C*** IAS
111200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
111300030721     C                   EVAL      PiStr=SkSplitCSV(i)
111400030721     C                   EXSR      CHKNUM
111500030721     C                   IF        PiNum=*on
111600030721     C                   Z-ADD     PiVal         VABIAS
111700030721     C                   ELSE
111800030721     C                   SETON                                        32
111900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112000030721     C                             + ' ' + 'VABIAS'
112100030721     C                   ENDIF
112200030721     C                   ENDIF
112300030721     C*** VLB
112400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
112500030721     C                   EVAL      PiStr=SkSplitCSV(i)
112600030721     C                   EXSR      CHKNUM
112700030721     C                   IF        PiNum=*on
112800030721     C                   Z-ADD     PiVal         VABVLB
112900030721     C                   ELSE
113000030721     C                   SETON                                        32
113100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113200030721     C                             + ' ' + 'VABVLB'
113300030721     C                   ENDIF
113400030721     C                   ENDIF
113500030721     C*** QFT
113600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
113700030721     C                   EVAL      PiStr=SkSplitCSV(i)
113800030721     C                   EXSR      CHKNUM
113900030721     C                   IF        PiNum=*on
114000030721     C                   Z-ADD     PiVal         VABQFT
114100030721     C                   ELSE
114200030721     C                   SETON                                        32
114300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114400030721     C                             + ' ' + 'VABQFT'
114500030721     C                   ENDIF
114600030721     C                   ENDIF
114700050202     C*** NCD
114800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
114900030721     C                   EVAL      PiStr=SkSplitCSV(i)
115000030721     C                   EXSR      CHKNUM
115100050202     C                   IF        PiInt=*on
115200050202     C                   Z-ADD     PiVal         wVATNOT_DA
115300030721     C                   ELSE
115400030721     C                   SETON                                        32
115500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115600030721     C                             + ' ' + 'VABNCD'
115700030721     C                   ENDIF
115800030721     C                   ENDIF
115900050202     C*** NCA
116000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
116100030721     C                   EVAL      PiStr=SkSplitCSV(i)
116200030721     C                   EXSR      CHKNUM
116300050202     C                   IF        PiInt=*on
116400050202     C                   Z-ADD     PiVal         wVATNOT_A
116500030721     C                   ELSE
116600030721     C                   SETON                                        32
116700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116800030721     C                             + ' ' + 'VABNCA'
116900030721     C                   ENDIF
117000030721     C                   ENDIF
117100030721     C*** ZNC
117200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
117300030721     C                   EVAL      PiStr=SkSplitCSV(i)
117400030721     C                   EXSR      CHKNUM
117500030721     C                   IF        PiInt=*on AND
117600030721     C                             PiVal<=99
117700030721     C                   Z-ADD     PiVal         VABZNC
117800030721     C                   ELSE
117900030721     C                   SETON                                        32
118000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118100030721     C                             + ' ' + 'VABZNC'
118200030721     C                   ENDIF
118300030721     C                   ENDIF
118400030721     C*** DCR
118500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
118600030721     C                   EVAL      PiStr=SkSplitCSV(i)
118700030721     C                   EXSR      CHKNUM
118800030721     C                   IF        PiInt=*on       AND
118900030721     C                             PiVal<=99999999
119000030721     C                   Z-ADD     PiVal         VABDCR
119100030721     C                   ELSE
119200030721     C                   SETON                                        32
119300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119400030721     C                             + ' ' + 'VABDCR'
119500030721     C                   ENDIF
119600030721     C                   ENDIF
119700030721     C*** HCR
119800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
119900030721     C                   EVAL      PiStr=SkSplitCSV(i)
120000030721     C                   EXSR      CHKNUM
120100030721     C                   IF        PiInt=*on   AND
120200030721     C                             PiVal<=9999
120300030721     C                   Z-ADD     PiVal         VABHCR
120400030721     C                   ELSE
120500030721     C                   SETON                                        32
120600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120700030721     C                             + ' ' + 'VABHCR'
120800030721     C                   ENDIF
120900030721     C                   ENDIF
121000030721     C*** VMD
121100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
121200030721     C                   EVAL      PiStr=SkSplitCSV(i)
121300030721     C                   EXSR      CHKNUM
121400030721     C                   IF        PiNum=*on
121500030721     C                   Z-ADD     PiVal         VABVMD
121600030721     C                   ELSE
121700030721     C                   SETON                                        32
121800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121900030721     C                             + ' ' + 'VABVMD'
122000030721     C                   ENDIF
122100030721     C                   ENDIF
122200030721     C*** ANT
122300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
122400030721     C                   EVAL      PiStr=SkSplitCSV(i)
122500030721     C                   EXSR      CHKNUM
122600030721     C                   IF        PiInt=*on        AND
122700030721     C                             PiVal<=999999999
122800030721     C                   Z-ADD     PiVal         VABANT
122900030721     C                   ELSE
123000030721     C                   SETON                                        32
123100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123200030721     C                             + ' ' + 'VABANT'
123300030721     C                   ENDIF
123400030721     C                   ENDIF
123500030721     C*** CMO
123600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
123700030721     C                   EVAL      PiStr=SkSplitCSV(i)
123800030721     C                   EXSR      CHKNUM
123900030721     C                   IF        PiInt=*on
124000030721     C                   Z-ADD     PiVal         Num5_0
124100030721     C                   MOVEL(p)  Num5_0        VABCMO
124200030721     C                   ELSE
124300030721     C                   SETON                                        32
124400030721     C                   EVAL      VABCMO = *zeros
124500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124600030721     C                             + ' ' + 'VABCMO'
124700030721     C                   ENDIF
124800030721     C                   ENDIF
124900030715     C*
125000030715     C                   ENDIF
125100030715     C                   EVAL      i = i + 1
125200030715     C                   ENDDO
125300040706     C*
125400040706     C* Se nn valorizzata la provincia destinatario la calcolo tramite TISI95
125500040706     C* Reperisco la provincia dal CAP e dalla località
125600040706     C                   IF        VABCAD <> *blanks
125700040706     C                   CLEAR                   TISI95DS
125800041130     C                   EVAL      I95TCN = '4'
125900040706     C                   Z-ADD     datcor        I95DAT
126000040706     C                   EVAL      I95CAP = VABCAD
126100040706     C                   EVAL      I95LOC = VABLOD
126200040706     C                   CALL      'TISI95R'
126300040706     C                   PARM                    TISI95DS
126400040706     C                   EVAL      VABPRD = O95PRV
126500040706     C                   ENDIF
126600040719     C*
126700050224     C* Se valore NSP passato dal clinte nn è valido o nn passato => stacco un numeratore da AZNUM
126800040719     C                   IF        VABNSP = *zeros
126900050224     C                   clear                   TRUL33DS
127000050224     C                   eval      I33OPE = *zeros
127100050224     C                   eval      I33CNU = 302
127200050224     C                   eval      I33NUM = 1
127300050224     C                   movel     TRUL33DS      KPJBU
127400050224     C                   call      'TRUL33R'
127500050224     C                   parm                    KPJBA
127600050224     C                   movel     KPJBU         TRUL33DS
127700050224     C                   if        O33ERR = *zeros
127800050224     C                   z-add     O33NRF        VABNSP
127900050224     C                   else
128000050224     C                   seton                                        31
128100050224     C                   EVAL      VABNSP = *zeros
128200050224     C                   EVAL      vinmsg = %trimr(vinmsg)
128300050224     C                             + ' ' + 'VABNSP VATNSP'
128400050224     C                   endif
128500050224     C                   ENDIF
128600040719     C*
128700040719     C* Se LNP nn valorizzato considero errore bloccante in traduzione
128800040719     C                   IF        VABLNP = *zeros
128900040719     C                   SETON                                        31
129000041130     C                   EVAL      vinmsg = %trimr(vinmsg)
129100041130     C                             + ' ' + 'VABLNP'
129200040719     C                   ENDIF
129300050202     C*
129400050202     C* Se testata tutto ok => eseguo scrittura "chi sono"
129500050202     C                   IF        *IN31 = *OFF
129600050202     C                   IF        wVATNOT_DA > *zeros AND
129700050202     C                             wVATNOT_A  > *zeros
129800050202     C                   Z-ADD     wVATNOT_DA    wVATNOT
129900050202     C                   DOW       wVATNOT <= wVATNOT_A
130000050202     C                   EVAL      VATNOT = %trim(%editc(wVATNOT:'Z'))
130100050202     C* Eseguo la scrittura dei "CHI SONO" della spedizione corrente
130200050202     C                   EXSR      wrivat
130300050202     C                   ADD       1             wVATNOT
130400050202     C                   ENDDO
130500050202     C                   ENDIF
130600050202     C                   ENDIF
130700040706     C*
130800040706     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
130900040706     C* d valorizzarli sempre entrambi
131000040706     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
131100040706     C                   Z-ADD     VABNSP        VABRMN
131200040706     C                   ENDIF
131300040706     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
131400040706     C                   MOVEL     VABRMN        VABRMA
131500040706     C                   ENDIF
131600040113     C*
131700040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
131800040113     C                   IF        VABCAS > *zeros
131900040119     C                   EVAL      VABVCA = 'EUR'
132000040119     C                   IF        VABCBO = '1'
132100040113     C                   EVAL      VABCBO = '4'
132200040119     C                   ENDIF
132300040119     C                   IF        VABCBO = '2'
132400040119     C                   EVAL      VABCBO = '6'
132500040119     C                   ENDIF
132600040113     C                   ENDIF
132700020204     C*
132800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
132900020204     C                   EXSR      CHKIMPDIV
133000030715     C                   ENDIF
133100041130     C*
133200041130     C* Ripristino la riga originale
133300041130     C                   EVAL      vinDTA = depvinDTA
133400020204     C*
133500010607     C                   ENDSR
133600010607     C*----------------------------------------------------*
133700010601
133800020204
133900020204     C*----------------------------------------------------*
134000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
134100020204     C*----------------------------------------------------*
134200020204     C     CHKIMPDIV     BEGSR
134300020204     C*
134400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
134500020204     C                   Z-ADD     *zeros        wrkDec            9 9
134600020204     C*
134700020204     C* Come prima cosa effettuo considerazioni sulla divisa
134800020204     C                   IF        vabIAS > *zeros
134900020204     C                   IF        vabVAS <> 'EUR'
135000020204     C                   EVAL      vabVAS =  'ITL'
135100020204     C                   ENDIF
135200020204     C                   ENDIF
135300020204     C*
135400020204     C                   IF        vabCAS > *zeros
135500020204     C                   IF        vabVCA <> 'EUR'
135600020204     C                   EVAL      vabVCA =  'ITL'
135700020204     C                   ENDIF
135800020204     C                   ENDIF
135900020204     C*
136000020204     C                   IF        vabVMD > *zeros
136100020204     C                   IF        vabVAD <> 'EUR'
136200020204     C                   EVAL      vabVAD =  'ITL'
136300020204     C                   ENDIF
136400020204     C                   ENDIF
136500020204     C*
136600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
136700020204     C                   Z-ADD     vabIAS        wrkDec
136800020204     C                   IF        wrkDec > *zeros
136900020204     C                   IF        vabVAS = 'ITL'
137000020204     C                   EVAL      vabIAS = *zeros
137100020204     C                   ENDIF
137200020204     C                   ENDIF
137300020204     C*
137400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
137500020204     C                   Z-ADD     vabCAS        wrkDec
137600020204     C                   IF        wrkDec > *zeros
137700020204     C                   IF        vabVCA = 'ITL'
137800020204     C                   EVAL      vabCAS = *zeros
137900020204     C                   ENDIF
138000020204     C                   ENDIF
138100020204     C*
138200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
138300020204     C                   Z-ADD     vabVMD        wrkDec
138400020204     C                   IF        wrkDec > *zeros
138500020204     C                   IF        vabVAD = 'ITL'
138600020204     C                   EVAL      vabVMD = *zeros
138700020204     C                   ENDIF
138800020204     C                   ENDIF
138900020204     C*
139000020204     C                   ENDSR
139100020204     C***
139200020204
139300010330
139400010330
139500010330     C*----------------------------------------------------*
139600010330     C*  CONTROLLO NUMERICITA' CAMPI
139700010330     C*----------------------------------------------------*
139800010330     C     CHKNUM        BEGSR
139900010330     C*
140000010606     C                   IF        PiDecChr = *blanks
140100030715     C                   EVAL      PiDecChr = CharNUM
140200010606     C                   ENDIF
140300010606     C*
140400010606     C                   CALL(e)   'ISNUMERIC'
140500010330     C                   PARM                    PiStr            30
140600010606     C                   PARM                    PiDecChr          1
140700010330     C                   PARM      *ZEROS        PiVal            30 9
140800010330     C                   PARM      '0'           PiInt             1
140900010330     C                   PARM      '0'           PiNum             1
141000010330     C                   IF        %error
141100010606     C                   EVAL      PiNum=*off
141200010330     C                   ENDIF
141300010330     C*
141400010330     C                   ENDSR
141500010330     C***
141600010330
141700010601
141800010601
141900010601
142000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
142100010601     C     repfil        BEGSR
142200010601     C*
142300010601     C                   if        invfil = *zeros and
142400010601     C                             depfil > *zeros and
142500010629     C                             (vinflg = *blanks or
142600010629     C                              vinflg = *zeros)
142700010601     C
142800010601     C                   eval      invfil = depfil
142900010601     C                   endif
143000010601     C*
143100010601     C                   if        depfil <> invfil and
143200010601     C                             invfil > *zeros
143300010601     C                   eval      flgMulti = '1'
143400010601     C                   if        vinflg = *blanks
143500010601     C                   add       1             cntNonEl
143600010601     C                   endif
143700010601     C                   endif
143800010601     C*
143900010601     C                   if        vinflg = '2'
144000010601     C                   eval      flgStato = '2'
144100010601     C                   endif
144200010601     C*
144300010601     C                   ENDSR
144400010601     C***
144500010601
144600010601
144700010601
144800010330
144900040119      /TITLE Invio dei dati al punto operativo.
145000040119     C     invio         BEGSR
145100040119     C*
145200040119     C* 1° invio FIVAT
145300040119     C                   reset                   dscmz
145400040119     C                   move      vlrpoi        cmzdst
145500040119     C                   eval      cmzfld = 'FIVATWWR'
145600040119     C                   eval      cmzmbd = vlrhdl
145700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
145800040119     C***                if        prmfir = *blanks
145900040119     C                   eval      cmzfla = 'FIVAT00F'
146000040119     C                   eval      cmzmba = 'FIVAT00F'
146100040119     C***                else
146200040119     C***                eval      cmzfla = prmfir
146300040119     C***                eval      cmzmba = prmfir
146400040119     C***                endif
146500040119     C                   eval      cmznrr = *zeros
146600040119     C                   move      §ctrokvt      cmznrr
146700040119     C                   eval      cmzlba = vlrfl1
146800040119     C                   call(e)   'TIS711C'
146900040119     C                   parm                    dscmz
147000040119     C                   parm      *blanks       esito
147100040119     C                   if        %error
147200040119     C                             or cmzerr = '1'
147300040119     C                             or esito  = '1'
147400040119     C                   eval      wrkesito = '3'
147500040119     C                   else
147600040119     C*
147700040119     C* 2° invio FIVAB
147800040119     C                   reset                   dscmz
147900040119     C                   move      vlrpoi        cmzdst
148000040119     C                   eval      cmzfld = vlrfou
148100040119     C                   eval      cmzmbd = vlrhdl
148200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
148300040119     C***                if        prmfir = *blanks
148400040119     C                   eval      cmzfla = 'FIVAB00F'
148500040119     C                   eval      cmzmba = 'FIVAB00F'
148600040119     C***                else
148700040119     C***                eval      cmzfla = prmfir
148800040119     C***                eval      cmzmba = prmfir
148900040119     C***                endif
149000040119     C                   eval      cmznrr = *zeros
149100040119     C                   move      §ctrokvb      cmznrr
149200040119     C                   eval      cmzlba = vlrfl1
149300040119     C                   call(e)   'TIS711C'
149400040119     C                   parm                    dscmz
149500040119     C                   parm      *blanks       esito
149600040119     C                   if        %error
149700040119     C                             or cmzerr = '1'
149800040119     C                             or esito  = '1'
149900040119     C                   eval      wrkesito = '3'
150000040119     C                   endif
150100040119     C                   endif
150200040119     C*
150300040119     C                   ENDSR
150400040119     C***
150500010601
150600010601
150700010601
150800010601
150900010601      /TITLE Invio dei dati al punto operativo.
151000010601     C     opeini        BEGSR
151100010601     C*
151200010601     C* Inizializzo flag e contatori operativi
151300010601     C                   movel     '0'           flgGiro           1
151400010601     C                   movel     '0'           flgMulti          1
151500010601     C                   movel     '1'           flgStato          1
151600010615     C                   movel     '0'           flgOk             1
151700010601     C                   z-add     *zeros        cntNonEl         10 0
151800010601     C                   z-add     *zeros        depfil            3 0
151900010601     C                   z-add     *zeros        invfil            3 0
152000010601     C*
152100010601     C                   ENDSR
152200010601     C***
152300010601
152400010601
152500050421
152600050421
152700070411
152800070411     C     *pssr         BEGSR
152900070411     C*
153000070411     C                   if        %open(tivin00r)
153100070411     C                   close     tivin00r
153200070411     C                   endif
153300070411     C                   if        %open(fivabwwr)
153400070411     C                   close     fivabwwr
153500070411     C                   endif
153600070411     C                   if        %open(fivatwwr)
153700070411     C                   close     fivatwwr
153800070411     C                   endif
153900070411     C*
154000070411     C* Effettuo la chiamata al CLLE preposto
154100070411     C                   call(e)   'TITVVTC'
154200070411     C                   parm                    parccm
154300070411     C                   parm                    parmbr
154400070411     C                   parm      '2'           paropz
154500070411     C*
154600070411     C                   eval      wrkesito = '2'
154700070411     C*
154800070411     C                   seton                                        LR
154900070411     C*
155000070411     C                   ENDSR     '*CANCL'
155100070411     C***
155200070411
155300050421
155400010601
155500010330
155600010330
155700000613     C     *inzsr        BEGSR
155800990910     C*
155900990910     C     *entry        plist
156000990920     C                   parm                    tivlrds
156100990921     C                   parm      wrkesito      esito
156200000724     C                   parm                    prmlit
156300000710     C                   parm                    prmfir
156400010330     C*
156500010330     C* CALCOLA LA DATA CORRENTE
156600010330     C                   time                    wn14             14 0
156700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
156800010330     C                   z-add     wn8           g08dat
156900010330     C                   z-add     *zeros        g08inv
157000010330     C                   movel     '0'           g08err
157100010330     C                   call      'XSRDA8'
157200010330     C                   parm                    wlbda8
157300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
157400000613     C*
157500000613     C                   ENDSR
157600000613     C***
157700990908
157800050202     Otitvh6p   E            riepilogo         2
157900990915     O                                              'Upload via Internet'
158000990915     O                                           +1 'Traduzione TIVIN00R -'
158100021025     O                                           +1 'FIVABWWR'
158200990915     O                                           +1 'Testate bolle'
158300990915     O          E            riepilogo   2
158400990915     O                       wrkdata
158500990915     O                       wrkora              +1
158600990915     O                       procname            +1
158700990915     O          E            riepilogo   2
158800990915     O                                              'Cliente..................:'
158900010605     O                       VABCCM        z     +1
159000990915     O          E            riepilogo   2
159100990920     O                                              'Riferimento Strategi.....:'
159200990920     O                       vlrhdl              +1
159300990915     O          E            riepilogo   2
159400990915     O                                              'Giusti...................:'
159500040119     O                       §CTROKVB      2   +  1
159600990915     O          E            riepilogo   2
159700990915     O                                              'Sbagliati e corretti.....:'
159800971022     O                       §CTRMO        2   +  1
159900990915     O          E            riepilogo   2
160000990915     O                                              'Sbagliati e scartati.....:'
160100971022     O                       §CTRNO        2   +  1
160200000613
160300050202     Otitvh6ps  E            testdett          2
160400000613     O                                              'Upload via Internet'
160500000613     O                                           +1 'Traduzione TIVIN00R -'
160600021025     O                                           +1 'FIVABWWR'
160700000613     O                                           +1 'Testate bolle'
160800000616     O          E            testdett    3
160900000613     O                                           +2 'N° rec'
161000000613     O                                           +3 'Anteprima contenuto'
161100000616     O          E            rigadett    2
161200000613     O                       rrnum               +2
161300000621     O                       recko               +3
161400000616     O          E            findett     2
161500000613     O                       wrkdata
161600000613     O                       wrkora              +1
161700000613     O                       procname            +1
161800000616     O          E            findett     2
161900000613     O                                              'Cliente..................:'
162000010605     O                       VABCCM        z     +1
162100000616     O          E            findett     2
162200000613     O                                              'Riferimento Strategi.....:'
162300000613     O                       vlrhdl              +1
162400000616     O          E            findett     2
162500000613     O                                              'Giusti...................:'
162600040119     O                       §CTROKVB      2   +  1
162700000616     O          E            findett     2
162800000613     O                                              'Sbagliati e corretti.....:'
162900000613     O                       §CTRMO        2   +  1
163000000616     O          E            findett     2
163100000613     O                                              'Sbagliati e scartati.....:'
163200000613     O                       §CTRNO        2   +  1
163300000613** CMD - COMANDI CL
163400050202OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVH6P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
163500050202OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVH6PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
163600050202DLTOVR FILE(TITVH6P TITVH6PS) LVL(*)
163700010606
163800000613
163900040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
164000050202
164100050202
164200050202
164300050202
