000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200080418     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800080418     Ftitv1f9p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070329     F                                     infsr(*pssr)
001100080418     Ftitv1f9ps O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070329     F                                     infsr(*pssr)
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
003000040128     D tisi95ds      e ds
003100010330     D esito           s              1
003200010330     D prmlit          s             10
003300010330     D prmfir          s             10
003400010330     D wrkesito        s                   like(esito)
003500010330     D wrkdata         s               d
003600010330     D wrkora          s               t
003700010330     D rrnum           s              6  0 INZ(*zeros)
003800010330     D recko           s            150    INZ(*blanks)
003900011113     D depcmd          s            150    INZ(*blanks)
004000040510     D parccm          s              8    INZ(*blanks)
004100040119     D parmbr          s             10    INZ(*blanks)
004200040119     D paropz          s              1    INZ(*blanks)
004300040119     D chkcall         s              1    INZ(*blanks)
004400080418     D wFlgCAS         s              1    INZ(*blanks)
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
005700070823     D wVATNOT_A       s                   LIKE(VATNOT)
005800060331     D wVATNOT_B       s                   LIKE(VATNOT)
005900060331     D wVATNOT_E       s                   LIKE(VATNOT)
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
007500071029     D*------------------
007600071029     D* DS REPERIMENTO NUMERATORE
007700071029     D*------------------
007800071029     D trul33ds      e ds                  inz
007900071029     D*------------------
008000071029     D* DS ARCHITETTURA
008100071029     D*------------------
008200071029     D kpjba         e ds                  inz
008300080418     D*------------------
008400080418     D* Costanti
008500080418     D*------------------
008600080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008700080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008800080418     D*------------------
008900080418     D* VARIABILI X "XCHKCHAR"
009000080418     D*------------------
009100080418     D TxtInOut        S           2048
009200080418     D ElencoChar      S            256
009300080418     D TipoElenco      S              1
009400080418     D CharSost        S              1
009500080418     D UpperCase       S              1
009600080418     D ChkNull         S              1
009700080418     D CharNull        S              1
009800080418     D EsitoBon        S              1
009900080418     D*------------------
010000010330
010100010330
010200010330
010300990915     C                   time                    wrkdata
010400990915     C                   time                    wrkora
010500000913     C                   reset                   rrnum
010600990921     C                   reset                   esito
010700990921     C                   reset                   wrkesito
010800010601     C*
010900010601     C                   exsr      opeini
011000010605     C                   exsr      rwvab
011100010601     C*
011200010601     C                   seton                                        lr
011300010601
011400010601
011500010601
011600010601     C*--------------------------------------------------------
011700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011800010601     C*--------------------------------------------------------
011900010601     C     PREELA        BEGSR
012000010601     C*
012100010601     C* SE OCCORRE SPEDIRE IN FILIALE
012200010601     C                   if        invfil <> *zeros and
012300010601     C                             flgGiro = '0'
012400010601     C*
012500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012600010601     C                   eval      flgGiro = '1'
012700000724     C*
012800000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
012900000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013000010601     C     invfil        chain     azorg01l
013100000724     C                   if        %found
013200000724     C                   movel(p)  CMD(1)        depcmd
013300020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
013400000724     C*
013500000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
013600011113     C                   Z-ADD     150           LENGH            15 5
013700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013800000724     C                   PARM                    depcmd
013900000724     C                   PARM                    LENGH
014000000724     C*
014100000724     C                   endif
014200000616     C*
014300000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014400000616     C                   movel(p)  CMD(2)        depcmd
014500000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
014600000616     C*
014700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
014800011113     C                   Z-ADD     150           LENGH            15 5
014900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015000000616     C                   PARM                    depcmd
015100000616     C                   PARM                    LENGH
015200000613     C*
015300080418     C                   if        not %open(titv1f9ps)
015400080418     C                   open      titv1f9ps
015500070329     C                   except    testdett
015600000613     C                   endif
015700010601     C*
015800010601     C                   endif
015900010601     C*
016000010601     C                   ENDSR
016100010601     C***
016200010601
016300010601
016400010601
016500010601
016600010601     C*--------------------------------------------------------
016700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016800010601     C*--------------------------------------------------------
016900010601     C     ENDELA        BEGSR
017000990915     C*
017100000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
017200040119     C*
017300040119     C* Effettuo la chiamata al CLLE preposto
017400040119     C                   call(e)   'TITVVTC'
017500040510     C                   parm                    parccm
017600040119     C                   parm                    parmbr
017700040119     C                   parm      '2'           paropz
017800050201     C*
017900050201     C* Effettuo lancio TISI95 solo x chiusura
018000050201     C                   CLEAR                   TISI95DS
018100050201     C                   EVAL      I95TLA = 'C'
018200050201     C                   CALL      'TISI95R'
018300050201     C                   PARM                    TISI95DS
018400000613     C*
018500080418     C                   if        %open(titv1f9ps)
018600000616     C                   except    findett
018700080418     C                   close     titv1f9ps
018800000613     C                   endif
018900000616     C*
019000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
019100011113     C                   Z-ADD     150           LENGH            15 5
019200010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
019300000616     C                   PARM                    CMD(3)
019400000616     C                   PARM                    LENGH
019500000616     C*
019600010601     C                   ENDSR
019700010601     C***
019800010601
019900010601
020000010601
020100000613
020200000613     C*--------------------------------------------------------
020300000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
020400000613     C*--------------------------------------------------------
020500000621     C     STPR          BEGSR
020600000613     C*
020700080418     C                   if        not %open(titv1f9p)
020800080418     C   50              open      titv1f9p
020900990915     C                   endif
021000990915     C*
021100030715     C   50              except    riepilogo
021200990915     C*
021300080418     C                   if        %open(titv1f9p)
021400080418     C   50              close     titv1f9p
021500990914     C                   endif
021600000613     C*
021700000613     C                   ENDSR
021800000613     C***
021900000613
022000010601
022100010601
022200010330     C*--------------------------------------------------------
022300021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
022400010330     C*--------------------------------------------------------
022500010605     C     RWVAB         BEGSR
022600010330     C*
022700010330     C                   if        not %open(tivin00r)
022800010330     C                   open      tivin00r
022900010330     C                   endif
023000021025     C                   if        not %open(fivabwwr)
023100021025     C                   open      fivabwwr
023200010330     C                   endif
023300040119     C*
023400040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
023500040119     C                   exsr      prevat
023600040119     C*
023700040119     C                   if        chkcall = '0'
023800040119     C*
023900040119     C                   if        not %open(fivatwwr)
024000040119     C                   open      fivatwwr
024100040119     C                   endif
024200010330     C*
024300040119     C                   clear                   §CTROKVB          7 0
024400040119     C                   clear                   §CTROKVT          7 0
024500040119     C                   clear                   §CTRMO            7 0
024600040119     C                   clear                   §CTRNO            7 0
024700070823     C*
024800070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
024900070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
025000080418     C***                exsr      inzvar
025100080418     C***                exsr      impvab
025200010330     C*
025300030822     C                   do        *HIVAL
025400010330     C*
025500030822     C                   read      tivin00r                               70
025600010618     C*
025700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
025800010618     C                   if        vinflg = '1'
025900010618     C                   eval      flgOk = '1'
026000010618     C                   endif
026100040510     C*
026200040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
026300040510     C                   z-add     *zeros        wLenVINDTA        4 0
026400040510     C                   z-add     *zeros        wFlgVINDTA        1 0
026500040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
026600040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
026700040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
026800040510     C                   z-add     1             wFlgVINDTA
026900040510     C                   leave
027000040510     C                   endif
027100040510     C                   enddo
027200010618     C*
027300040510     C                   if        vindta > *blanks AND
027400040510     C                             wFlgVINDTA = 1
027500010330     C                   add       1             rrnum
027600010330     C*
027700010601     C                   if        *in70 = *off and
027800010330     C                             (vinflg = *blanks
027900010330     C                              or vinflg = '0'
028000010330     C                              or vinflg = '2')
028100010330     C*
028200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
028300010711     C                   if        vinflg = *blanks or vinflg = '0'
028400010711     C                   clear                   vinmsg
028500010711     C                   endif
028600010601     C*
028700070925     C* Aggiungo un byte a blanks all'inizio d ogni record
028800070925     C                   eval      vindta  = ' ' + %trim(vindta)
028900070925     C*
029000010330     C                   exsr      inzvar
029100010605     C                   exsr      impvab
029200010601     C*
029300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
029400010605     C                   eval      depfil = VABLNP
029500010601     C                   exsr      repfil
029600010601     C                   if        depfil = invfil
029700021025     C                   if        vlrpoi = 999
029800030822     C                   move(P)   invfil        VABFGS
029900021025     C                   else
030000030822     C                   move(P)   vlrpoi        VABFGS
030100021025     C                   endif
030200010601     C*
030300010601     C                   exsr      PREELA
030400010601     C*
030500010604     C* Ebbene...
030600010604     C*
030700040119     C  N31              add       1             §CTROKVB
030800040119     C   32              add       1             §CTRMO
030900040119     C   31              add       1             §CTRNO
031000080418     C*
031100030822     C                   if        wGiro = 2
031200040510     C  N31              exsr      wrivat
031300040119     C  N31              exsr      wrivab
031400040119     C                   endif
031500010604     C*
031600010604     C                   if        *in31 = *off and
031700010604     C                             *in32 = *off
031800010604     C                   eval      vinflg = '1'
031900010604     C                   else
032000010604     C                   eval      recko = vindta
032100010604     C                   except    rigadett
032200010604     C                   eval      vinflg = '2'
032300010604     C                   endif
032400010604     C                   endif
032500010601     C*
032600010604     C                   endif
032700010604     C*
032800010330     C                   else
032900010330     C                   eval      vinflg = '1'
033000010330     C                   endif
033100010601     C*
033200010601     C  N70              update    tivin000
033300010330     C*
033400030822     C  N70              enddo
033500040119     C*
033600040119     C                   endif                                                  (endif - chkcall)
033700010601     C*
033800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
033900010601     C                   if        cntNonEl = *zeros or
034000010601     C                             flgMulti = '0'
034100010330     C* Se non ci sono record con errori ...
034200010601     C                   if        §ctrno = 0 and
034300010604     C                             §ctrmo = 0 and
034400010601     C                             flgStato <> '2'
034500010330     C* ... restituisco esito OK.
034600010330     C                   eval      wrkesito = '0'
034700010330     C                   else
034800040119     C                   if        §ctrokvb > 0
034900010330     C                   eval      wrkesito = '1'
035000010330     C                   else
035100010615     C                   if        flgOk = '0'
035200010615     C                   eval      wrkesito = '2'
035300010615     C                   else
035400010615     C                   eval      wrkesito = '6'
035500010615     C                   endif
035600010330     C                   endif
035700010330     C                   endif
035800010601     C                   else
035900010601     C                   eval      wrkesito = '9'
036000010601     C                   endif
036100010330     C*
036200010330     C                   if        %open(tivin00r)
036300010330     C                   close     tivin00r
036400010330     C                   endif
036500021025     C                   if        %open(fivabwwr)
036600021025     C                   close     fivabwwr
036700010330     C                   endif
036800040512     C                   if        %open(fivatwwr)
036900040512     C                   close     fivatwwr
037000040512     C                   endif
037100010601     C*
037200010601     C                   if        vlrpoi <> 999
037300010601     C                   eval      invfil = vlrpoi
037400010601     C                   endif
037500010330     C*
037600040119     C                   if        §ctrokvb > 0
037700010601     C                             and invfil > *zeros
037800010330     C                   exsr      invio
037900010330     C                   endif
038000010601     C*
038100010618     C                   if        flgGiro = '1'
038200010601     C                   exsr      endela
038300010618     C                   endif
038400010330     C*
038500010330     C                   ENDSR
038600010330     C***
038700040119
038800040119
038900040119
039000040119     C*----------------------------------------------------*
039100040119     C*  SCARICAMENTO BUFFER RECORDS VAB
039200040119     C*----------------------------------------------------*
039300040510     C     WRIVAB        BEGSR
039400040119     C*
039500070301     C                   if        wVATNOT_E <> *blanks
039600070301     C                   eval      VABCTM = '7Q'
039700070301     C                   endif
039800040119     C                   write     fivab000                                     => scarico il VAB
039900040119     C*
040000040119     C                   ENDSR
040100040119
040200040119
040300040119
040400040119     C*----------------------------------------------------*
040500040119     C*  SCARICAMENTO BUFFER RECORDS VAT
040600040119     C*----------------------------------------------------*
040700040510     C     WRIVAT        BEGSR
040800060512     C*
040900060512     C* Inizializzo i campi chiave
041000060512     C                   eval      VATFGS = VABFGS
041100060512     C                   eval      VATAAS = VABAAS
041200060512     C                   eval      VATCCM = VABCCM
041300070823     C                   eval      VATNRS = VABNRS
041400060512     C                   eval      VATNSP = VABNSP
041500060512     C                   eval      VATLNP = VABLNP
041600040714     C*
041700040714     C* Solo se VATNOT valorizzato scarico il buffer
041800040714     C                   if        VATNOT <> *blanks
041900040119     C*
042000040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
042100040510     C                   if        VATTRC = *blanks
042200040510     C                   eval      VATTRC = 'E'
042300040510     C                   endif
042400040510     C                   write     FIVAT000
042500040119     C                   add       1             §CTROKVT
042600040714     C*
042700040714     C                   endif
042800060331     C*
042900060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
043000060331     C                   if        wVATNOT_A <> *blanks
043100060331     C                   eval      VATNOT = wVATNOT_A
043200060331     C                   eval      VATTRC = 'A'
043300060331     C                   write     FIVAT000
043400060331     C                   add       1             §CTROKVT
043500060331     C                   endif
043600060331     C*
043700060331     C                   if        wVATNOT_B <> *blanks
043800060331     C                   eval      VATNOT = wVATNOT_B
043900060331     C                   eval      VATTRC = 'B'
044000060331     C                   write     FIVAT000
044100060331     C                   add       1             §CTROKVT
044200060331     C                   endif
044300060331     C*
044400060331     C                   if        wVATNOT_E <> *blanks
044500060331     C                   eval      VATNOT = wVATNOT_E
044600060331     C                   eval      VATTRC = 'E'
044700060331     C                   write     FIVAT000
044800060331     C                   add       1             §CTROKVT
044900060331     C                   endif
045000040119     C*
045100040119     C                   ENDSR
045200040119
045300040119
045400040119
045500040119     C*----------------------------------------------------*
045600040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
045700040119     C*----------------------------------------------------*
045800040119     C     PREVAT        BEGSR
045900040119     C*
046000040119     C* Compongo il nome del membro da dare al FIVATWWR
046100040119     C                   eval      parmbr = vlrhdl
046200040119     C                   movel     'M'           parmbr
046300040510     C                   eval      parccm = vlrksc
046400040119     C                   eval      paropz = '1'
046500040119     C* Effettuo la chiamata al CLLE preposto
046600040119     C                   call(e)   'TITVVTC'
046700040119     C                   parm                    parccm
046800040119     C                   parm                    parmbr
046900040119     C                   parm                    paropz
047000040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047100040119     C                   if        %error
047200040119     C                   movel     '1'           chkcall
047300040119     C                   else
047400040119     C                   movel     '0'           chkcall
047500040119     C                   endif
047600040119     C*
047700040119     C                   ENDSR
047800010601
047900010601
048000010601
048100010330     C*----------------------------------------------------*
048200030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
048300010330     C*----------------------------------------------------*
048400040119     C     INZVAR        BEGSR
048500010330     C*
048600030822     C* Inizializzo variabili di wrk
048700020204     C                   Z-ADD     *zeros        Num5_0            5 0
048800060331     C                   MOVEL     *blanks       wVATNOT_A
048900060331     C                   MOVEL     *blanks       wVATNOT_B
049000060331     C                   MOVEL     *blanks       wVATNOT_E
049100030822     C*
049200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
049300030822     C                   CLEAR                   FIVAB000
049400040119     C                   CLEAR                   FIVAT000
049500030822     C                   CLEAR                   SkSplitCSV
049600030822     C                   EVAL      i = 1
049700030822     C                   EVAL      posDa = *zeros
049800030822     C                   EVAL      posA  = *zeros
049900030822     C*
050000030822     C* Reimposto i valori di default
050100030822     C                   EXSR      DEFCAM
050200030822     C*
050300010330     C                   ENDSR
050400010330     C*----------------------------------------------------*
050500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
050600010330     C*----------------------------------------------------*
050700010330     C     DEFCAM        BEGSR
050800030822     C*
050900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
051000030715     C* e delimitatore testo.
051100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
051200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
051300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
051400030822     C*
051500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
051600030715     C                   IF        %subst(vlrppt:1:1) = 'S'
051700030715     C                   SETON                                        50
051800030715     C                   ELSE
051900030715     C                   SETOFF                                       50
052000030715     C                   ENDIF
052100030822     C*
052200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
052300030822     C                   EVAL      posDaDft = 1
052400030822     C                   EVAL      posADft  = 0
052500030822     C                   EVAL      wGiroDft = 0
052600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
052700030822     C                             posDaDft > 0
052800030822     C*
052900030822     C* Gestisco il 1° giro
053000030822     C                   IF        wGiroDft = 0
053100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
053300030822     C* Incremento il contatore dei "giri"
053400030822     C                   EVAL      wGiroDft = 1
053500030822     C                   ELSE
053600030822     C                   EVAL      posDaDft = posADft
053700030822     C                   ENDIF
053800030822     C* Eseguo lo scan x trovare la fine del campo corrente
053900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
054000030822     C*
054100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
054300030822     C                   IF        posDaDft > 0 AND
054400030822     C                             posADft  > 0
054500030822     C* NCL
054600030822     C                   IF        %subst(
054700030822     C                             %subst(vlrppt:posDaDft+1:
054800030822     C                             posADft-posDaDft-1):1:3)
054900030822     C                             = 'NCL'
055000030822     C                   EVAL      PiStr=%trim(%subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055300030822     C                   EXSR      CHKNUM
055400030822     C                   IF        PiInt=*on
055500030822     C                   Z-ADD     PiVal         VABNCL
055600030822     C                   ENDIF
055700030822     C                   ENDIF
055800030822     C* CCM
055900030822     C                   IF        %subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):1:3)
056200030822     C                             = 'CCM'
056300030822     C                   EVAL      PiStr=%trim(%subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056600030822     C                   EXSR      CHKNUM
056700030822     C                   IF        PiInt=*on
056800030822     C                   Z-ADD     PiVal         VABCCM
056900030822     C                   ENDIF
057000030822     C                   ENDIF
057100030822     C* LNP
057200030822     C                   IF        %subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):1:3)
057500030822     C                             = 'LNP'
057600030822     C                   EVAL      PiStr=%trim(%subst(
057700030822     C                             %subst(vlrppt:posDaDft+1:
057800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057900030822     C                   EXSR      CHKNUM
058000030822     C                   IF        PiInt=*on
058100030822     C                   Z-ADD     PiVal         VABLNP
058200030822     C                   ENDIF
058300030822     C                   ENDIF
058400030822     C* NRS
058500030822     C                   IF        %subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):1:3)
058800030822     C                             = 'NRS'
058900030822     C                   EVAL      PiStr=%trim(%subst(
059000030822     C                             %subst(vlrppt:posDaDft+1:
059100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059200030822     C                   EXSR      CHKNUM
059300030822     C                   IF        PiInt=*on
059400030822     C                   Z-ADD     PiVal         VABNRS
059500030822     C                   ENDIF
059600030822     C                   ENDIF
059700030822     C* CTR
059800030822     C                   IF        %subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):1:3)
060100030822     C                             = 'CTR'
060200030822     C                   EVAL      PiStr=%trim(%subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060500030822     C                   EXSR      CHKNUM
060600030822     C                   IF        PiInt=*on
060700030822     C                   Z-ADD     PiVal         VABCTR
060800030822     C                   ENDIF
060900030822     C                   ENDIF
061000030822     C* PKB
061100030822     C                   IF        %subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):1:3)
061400030822     C                             = 'PKB'
061500030822     C                   EVAL      PiStr=%trim(%subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061800030822     C                   EXSR      CHKNUM
061900030822     C                   IF        PiNum=*on
062000030822     C                   Z-ADD     PiVal         VABPKB
062100030822     C                   ENDIF
062200030822     C                   ENDIF
062300030822     C* VLB
062400030822     C                   IF        %subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):1:3)
062700030822     C                             = 'VLB'
062800030822     C                   EVAL      PiStr=%trim(%subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063100030822     C                   EXSR      CHKNUM
063200030822     C                   IF        PiNum=*on
063300030822     C                   Z-ADD     PiVal         VABVLB
063400030822     C                   ENDIF
063500030822     C                   ENDIF
063600030822     C* QFT
063700030822     C                   IF        %subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):1:3)
064000030822     C                             = 'QFT'
064100030822     C                   EVAL      PiStr=%trim(%subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064400030822     C                   EXSR      CHKNUM
064500030822     C                   IF        PiNum=*on
064600030822     C                   Z-ADD     PiVal         VABQFT
064700030822     C                   ENDIF
064800030822     C                   ENDIF
064900030822     C* CBO
065000030822     C                   IF        %subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):1:3)
065300030822     C                             = 'CBO'
065400030822     C                   EVAL      VABCBO=%trim(%subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065700030822     C                   ENDIF
065800030822     C* TSP
065900030822     C                   IF        %subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):1:3)
066200030822     C                             = 'TSP'
066300030822     C                   EVAL      VABTSP=%trim(%subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066600030822     C                   ENDIF
066700030822     C* VAS
066800030822     C                   IF        %subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):1:3)
067100030822     C                             = 'VAS'
067200030822     C                   EVAL      VABVAS=%trim(%subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067500030822     C                   ENDIF
067600030822     C* VCA
067700030822     C                   IF        %subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):1:3)
068000030822     C                             = 'VCA'
068100030822     C                   EVAL      VABVCA=%trim(%subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068400030822     C                   ENDIF
068500070925     C* TIC
068600070925     C                   IF        %subst(
068700070925     C                             %subst(vlrppt:posDaDft+1:
068800070925     C                             posADft-posDaDft-1):1:3)
068900070925     C                             = 'TIC'
069000070925     C                   EVAL      VABTIC=%trim(%subst(
069100070925     C                             %subst(vlrppt:posDaDft+1:
069200070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069300070925     C                   ENDIF
069400030822     C* GCA
069500030822     C                   IF        %subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):1:3)
069800030822     C                             = 'GCA'
069900030822     C                   EVAL      VABGCA=%trim(%subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070200030822     C                   ENDIF
070300030822     C* CTM
070400030822     C                   IF        %subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):1:3)
070700030822     C                             = 'CTM'
070800030822     C                   EVAL      VABCTM=%trim(%subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071100030822     C                   ENDIF
071200030822     C* FFD
071300030822     C                   IF        %subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):1:3)
071600030822     C                             = 'FFD'
071700030822     C                   EVAL      VABFFD=%trim(%subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072000030822     C                   ENDIF
072100030822     C* VAD
072200030822     C                   IF        %subst(
072300030822     C                             %subst(vlrppt:posDaDft+1:
072400030822     C                             posADft-posDaDft-1):1:3)
072500030822     C                             = 'VAD'
072600030822     C                   EVAL      VABVAD=%trim(%subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072900030822     C                   ENDIF
073000030822     C* GMA
073100030822     C                   IF        %subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):1:3)
073400030822     C                             = 'GMA'
073500030822     C                   EVAL      VABGMA=%trim(%subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073800030822     C                   ENDIF
073900030822     C* GGA
074000030822     C                   IF        %subst(
074100030822     C                             %subst(vlrppt:posDaDft+1:
074200030822     C                             posADft-posDaDft-1):1:3)
074300030822     C                             = 'GGA'
074400030822     C                   EVAL      VABGGA=%trim(%subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074700030822     C                   ENDIF
074800030822     C* GVA
074900030822     C                   IF        %subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):1:3)
075200030822     C                             = 'GVA'
075300030822     C                   EVAL      VABGVA=%trim(%subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075600030822     C                   ENDIF
075700030822     C* TC1
075800030822     C                   IF        %subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):1:3)
076100030822     C                             = 'TC1'
076200030822     C                   EVAL      VABTC1=%trim(%subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076500030822     C                   ENDIF
076600030822     C* TC2
076700030822     C                   IF        %subst(
076800030822     C                             %subst(vlrppt:posDaDft+1:
076900030822     C                             posADft-posDaDft-1):1:3)
077000030822     C                             = 'TC2'
077100030822     C                   EVAL      VABTC2=%trim(%subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077400030822     C                   ENDIF
077500040714     C* VATTRC
077600040714     C                   IF        %subst(
077700040714     C                             %subst(vlrppt:posDaDft+1:
077800040714     C                             posADft-posDaDft-1):1:3)
077900040714     C                             = 'TRC'
078000040714     C                   EVAL      VATTRC=%trim(%subst(
078100040714     C                             %subst(vlrppt:posDaDft+1:
078200040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078300040714     C                   ENDIF
078400030822     C* ...
078500030822     C                   ENDIF
078600030822     C                   ENDDO
078700020204     C*
078800010330     C                   ENDSR
078900010607     C*----------------------------------------------------*
079000021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
079100010607     C*----------------------------------------------------*
079200010607     C     IMPVAB        BEGSR
079300010607     C*
079400010607     C                   SETOFF                                       3132
079500030515     C*
079600010607     C                   MOVEL     datcor        VABAAS
079700010607     C                   MOVE      datcor        VABMGS
079800010607     C*
079900030715     C**********
080000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080100030715     C**********
080200030715     C                   IF        wGiro = *zeros
080300040113     C*
080400040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
080500040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
080600040510     C                             %trim(intcol(3))<>*blanks
080700040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
080800040113     C                                      %trim(intcol(3))
080900070823     C     ';':CharCSV   XLATE     vindta        vindta
081000040510     C                   ENDIF
081100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081200031201     C     minu:maiu     XLATE     vindta        vindta
081300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081400030715     C                   DOW       posDa <= %len(%trim(vindta))
081500030715     C*
081600030715     C* Gestisco il 1° campo
081700030715     C                   IF        i = 1
081800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
081900030715     C                   EVAL      posDa = 1
082000030715     C* Eseguo lo scan x trovare la fine del primo campo
082100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082200030715     C* A questo punto "estraggo" il campo corrente
082300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082400030715     C                                                    (posA-posDa))
082500030715     C* X i campi successivi al 1°
082600030715     C                   ELSE
082700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
082800030715     C                   EVAL      posDa = posA + 1
082900030715     C* Eseguo lo scan x trovare la fine del campo corrente
083000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083100030715     C* Gestisco l'ultimo campo
083200030715     C                   IF        posA = *zeros
083300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083400030715     C* A questo punto "estraggo" il campo corrente
083500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083600030715     C                                                    (posA-posDa))
083700030715     C                   LEAVE
083800030715     C                   ELSE
083900030715     C* A questo punto "estraggo" il campo corrente
084000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084100030715     C                                                    (posA-posDa))
084200030715     C                   ENDIF
084300030715     C                   ENDIF
084400030715     C* Incremento il contatore d campo
084500030715     C                   EVAL      i = i +1
084600030715     C                   ENDDO
084700030715     C*
084800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
084900030715     C* delimitazione testo
085000030715     C                   EVAL      i = 1
085100030715     C                   DOW       i < %elem(SkSplitFLD)
085200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
085300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
085400030715     C                   EVAL      i = i + 1
085500030715     C                   ENDDO
085600030715     C                   EVAL      wGiro = 1
085700030715     C                   ELSE
085800030715     C                   EVAL      wGiro = 2
085900031204     C**********
086000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
086100031204     C* d separatore campo
086200031204     C**********
086300031204     C                   EVAL      vindta = %trim(vindta)
086400031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
086500031204     C                   EVAL      vindta = %subst(vindta:2)
086600031204     C                   ENDDO
086700031204     C*
086800031204     C                   Z-ADD     *zeros        lunghInput        4 0
086900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087000031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
087100031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
087200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087300031204     C                   ENDDO
087400030715     C**********
087500030715     C* Effettuo lo split del campo dati d input
087600030715     C**********
087700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
087800030715     C                   DOW       posDa <= %len(%trim(vindta))
087900030715     C*
088000030715     C* Gestisco il 1° campo
088100030715     C                   IF        i = 1
088200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088300030715     C                   EVAL      posDa = 1
088400030715     C* Eseguo lo scan x trovare la fine del primo campo
088500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088600030715     C* A questo punto "estraggo" il campo corrente
088700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088800030715     C                                                    (posA-posDa))
088900030715     C* X i campi successivi al 1°
089000030715     C                   ELSE
089100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089200030715     C                   EVAL      posDa = posA + 1
089300031204     C* Verifico che nn vi sia il campo nn valorizzato
089400031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
089500031204     C* Se campo nn valorizzato skippo al prossimo
089600031204     C                   EVAL      SkSplitCSV(i) = *blanks
089700031204     C                   EVAL      posA  = posA + 1
089800031204     C                   ELSE
089900030715     C* Eseguo lo scan x trovare la fine del campo corrente
090000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090100030715     C* Gestisco l'ultimo campo
090200030715     C                   IF        posA = *zeros
090300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090400030715     C* A questo punto "estraggo" il campo corrente
090500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090600030715     C                                                    (posA-posDa))
090700030715     C                   LEAVE
090800030715     C                   ELSE
090900030715     C* A questo punto "estraggo" il campo corrente
091000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091100030715     C                                                    (posA-posDa))
091200030715     C                   ENDIF
091300031204     C                   ENDIF
091400030715     C                   ENDIF
091500030715     C* Incremento il contatore d campo
091600030715     C                   EVAL      i = i +1
091700030715     C                   ENDDO
091800030715     C*
091900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092000030715     C* delimitazione testo
092100030715     C                   EVAL      i = 1
092200030715     C                   DOW       i < %elem(SkSplitCSV)
092300030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092400030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
092500030715     C                   EVAL      i = i + 1
092600030715     C                   ENDDO
092700030715     C*
092800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
092900030715     C                   EVAL      i = 1
093000030715     C                   DOW       i < %elem(SkSplitFLD)
093100030715     C*
093200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
093300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
093400030715     C                             SkSplitCSV(i) <> *blanks
093500030721     C*** ATB
093600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
093700030721     C                   EVAL      VABATB = SkSplitCSV(i)
093800030721     C                   ENDIF
093900030715     C*** RSD
094000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
094100030715     C                   EVAL      VABRSD = SkSplitCSV(i)
094200030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
094300030715     C     '@':'A'       XLATE     VABRSD        VABRSD
094400030715     C* ==
094500030715     C                   ENDIF
094600030721     C*** RD2
094700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
094800030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
094900030721     C                   ENDIF
095000030715     C*** IND
095100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
095200030715     C                   EVAL      VABIND = SkSplitCSV(i)
095300030715     C                   ENDIF
095400030715     C*** LOD
095500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
095600030715     C                   EVAL      VABLOD = SkSplitCSV(i)
095700030715     C                   ENDIF
095800030715     C*** PRD
095900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
096000030715     C                   EVAL      VABPRD = SkSplitCSV(i)
096100030715     C                   ENDIF
096200030721     C*** NZD
096300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
096400030721     C                   EVAL      VABNZD = SkSplitCSV(i)
096500030721     C                   ENDIF
096600030715     C*** RMA
096700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
096800030715     C                   EVAL      VABRMA = SkSplitCSV(i)
096900030715     C                   ENDIF
097000030715     C*** NOT
097100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
097200030715     C                   EVAL      VABNOT = SkSplitCSV(i)
097300030715     C                   ENDIF
097400030715     C*** NT2
097500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
097600030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
097700030715     C                   ENDIF
097800030715     C*** VCA
097900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
098000030715     C                   EVAL      VABVCA = SkSplitCSV(i)
098100030715     C                   ENDIF
098200030715     C*** VAS
098300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
098400030715     C                   EVAL      VABVAS = SkSplitCSV(i)
098500030715     C                   ENDIF
098600030715     C*** FFD
098700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
098800030715     C                   EVAL      VABFFD = SkSplitCSV(i)
098900030715     C                   ENDIF
099000030721     C*** GC1
099100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
099200030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
099300030721     C                   ENDIF
099400030721     C*** GC2
099500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
099600030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
099700030721     C                   ENDIF
099800030721     C*** TSP
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
100000030721     C                   EVAL      VABTSP = SkSplitCSV(i)
100100030721     C                   ENDIF
100200040510     C*** CBO
100300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
100400040510     C                   EVAL      VABCBO = SkSplitCSV(i)
100500040510     C                   ENDIF
100600030721     C*** NAS
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
100800030721     C                   EVAL      VABNAS = SkSplitCSV(i)
100900030721     C                   ENDIF
101000030721     C*** TIC
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
101200030721     C                   EVAL      VABTIC = SkSplitCSV(i)
101300030721     C                   ENDIF
101400030721     C*** GCA
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
101600030721     C                   EVAL      VABGCA = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030721     C*** XCO
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
102000030721     C                   EVAL      VABXCO = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030721     C*** CTM
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
102400030721     C                   EVAL      VABCTM = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030721     C*** TCR
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
102800030721     C                   EVAL      VABTCR = SkSplitCSV(i)
102900030721     C                   ENDIF
103000030721     C*** CTS
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
103200030721     C                   EVAL      VABCTS = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*** FTM
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
103600030721     C                   EVAL      VABFTM = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*** VAD
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
104000030721     C                   EVAL      VABVAD = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** GMA
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
104400030721     C                   EVAL      VABGMA = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** GGA
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
104800030721     C                   EVAL      VABGGA = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030721     C*** GVA
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
105200030721     C                   EVAL      VABGVA = SkSplitCSV(i)
105300030721     C                   ENDIF
105400030721     C*** TC1
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
105600030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** TC2
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
106000030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** SCL
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
106400030721     C                   EVAL      VABSCL = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*** RMO
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
106800030721     C                   EVAL      VABRMO = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*** NMO
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
107200030721     C                   EVAL      VABNMO = SkSplitCSV(i)
107300030721     C                   ENDIF
107400030721     C*
107500030721     C* Reperisco quindi i campi numerici...
107600040510     C*** NSP
107700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
107800040510     C                   EVAL      PiStr=SkSplitCSV(i)
107900040510     C                   EXSR      CHKNUM
108000040510     C                   IF        PiInt=*on
108100040510     C                   Z-ADD     PiVal         VABNSP
108200040510     C                   ELSE
108300040510     C                   SETON                                        31
108400040510     C                   EVAL      VABNSP = *zeros
108500040510     C                   EVAL      vinmsg = %trimr(vinmsg)
108600040510     C                             + ' ' + 'VABNSP'
108700040510     C                   ENDIF
108800040510     C                   ENDIF
108900071106     C*** RMN
109000071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
109100071106     C                   EVAL      PiStr=SkSplitCSV(i)
109200071106     C                   EXSR      CHKNUM
109300071106     C                   IF        PiInt=*on
109400071106     C                   Z-ADD     PiVal         VABRMN
109500071106     C                   ELSE
109600071106     C                   SETON                                        32
109700071106     C                   EVAL      VABRMN = *zeros
109800071106     C                   EVAL      vinmsg = %trimr(vinmsg)
109900071106     C                             + ' ' + 'VABRMN'
110000071106     C                   ENDIF
110100071106     C                   ENDIF
110200030721     C*** CAD
110300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
110400030721     C                   EVAL      PiStr=SkSplitCSV(i)
110500030721     C                   EXSR      CHKNUM
110600030721     C                   IF        PiInt=*on
110700030721     C                   Z-ADD     PiVal         Num5_0
110800030721     C                   MOVEL(p)  Num5_0        VABCAD
110900030721     C                   ELSE
111000030721     C                   SETON                                        32
111100030721     C                   EVAL      VABCAD = *zeros
111200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111300030721     C                             + ' ' + 'VABCAD'
111400030721     C                   ENDIF
111500030721     C                   ENDIF
111600030721     C*** NCL
111700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
111800030721     C                   EVAL      PiStr=SkSplitCSV(i)
111900030721     C                   EXSR      CHKNUM
112000030721     C                   IF        PiInt=*on
112100030721     C                   Z-ADD     PiVal         VABNCL
112200030721     C                   ELSE
112300030721     C                   SETON                                        32
112400030721     C                   EVAL      VABNCL = *zeros
112500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112600040122     C                             + ' ' + 'VABNCL'
112700030721     C                   ENDIF
112800030721     C                   ENDIF
112900030721     C*** PKB
113000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
113100030721     C                   EVAL      PiStr=SkSplitCSV(i)
113200030721     C                   EXSR      CHKNUM
113300030721     C                   IF        PiNum=*on
113400030721     C                   Z-ADD     PiVal         VABPKB
113500030721     C                   ELSE
113600030721     C                   SETON                                        32
113700030721     C                   EVAL      VABPKB = *zeros
113800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113900030721     C                             + ' ' + 'VABPKB'
114000030721     C                   ENDIF
114100030721     C                   ENDIF
114200030721     C*** CAS
114300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
114400030721     C                   EVAL      PiStr=SkSplitCSV(i)
114500030721     C                   EXSR      CHKNUM
114600030721     C                   IF        PiNum=*on
114700030721     C                   Z-ADD     PiVal         VABCAS
114800030721     C                   ELSE
114900030721     C                   SETON                                        32
115000030721     C                   EVAL      VABCAS = *zeros
115100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115200030721     C                             + ' ' + 'VABCAS'
115300030721     C                   ENDIF
115400030721     C                   ENDIF
115500030721     C*** CCM
115600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
115700030721     C                   EVAL      PiStr=SkSplitCSV(i)
115800030721     C                   EXSR      CHKNUM
115900030721     C                   IF        PiInt=*on      AND
116000030721     C                             PiVal<=9999999 AND
116100030721     C                             PiVal>*zeros
116200030721     C                   Z-ADD     PiVal         VABCCM
116300030721     C                   ELSE
116400030721     C                   SETON                                        32
116500030721     C                   EVAL      VABCCM = *zeros
116600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116700040510     C                             + ' ' + 'VABCCM'
116800030721     C                   ENDIF
116900030721     C                   ENDIF
117000030721     C*** LNP
117100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
117200030721     C                   EVAL      PiStr=SkSplitCSV(i)
117300030721     C                   EXSR      CHKNUM
117400030721     C                   IF        PiInt=*on    AND
117500030721     C                             PiVal<=999   AND
117600030721     C                             PiVal>*zeros
117700030721     C                   Z-ADD     PiVal         VABLNP
117800030721     C                   ELSE
117900040715     C                   SETON                                        32
118000030721     C                   EVAL      VABLNP = *zeros
118100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118200040510     C                             + ' ' + 'VABLNP'
118300030721     C                   ENDIF
118400030721     C                   ENDIF
118500030721     C*** NRS
118600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
118700030721     C                   EVAL      PiStr=SkSplitCSV(i)
118800030721     C                   EXSR      CHKNUM
118900030721     C                   IF        PiInt=*on AND
119000030721     C                             PiVal<=99
119100030721     C                   Z-ADD     PiVal         VABNRS
119200030721     C                   ELSE
119300030721     C                   SETON                                        32
119400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119500030721     C                             + ' ' + 'VABNRS'
119600030721     C                   ENDIF
119700030721     C                   ENDIF
119800030721     C*** LNA
119900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
120000030721     C                   EVAL      PiStr=SkSplitCSV(i)
120100030721     C                   EXSR      CHKNUM
120200030721     C                   IF        PiInt=*on    AND
120300030721     C                             PiVal<=999   AND
120400030721     C                             PiVal>*zeros
120500030721     C                   Z-ADD     PiVal         VABLNA
120600030721     C                   ELSE
120700030721     C                   SETON                                        32
120800030721     C                   EVAL      VABLNA = *zeros
120900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121000030721     C                             + ' ' + 'VABLNA'
121100030721     C                   ENDIF
121200030721     C                   ENDIF
121300030721     C*** CTR
121400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
121500030721     C                   EVAL      PiStr=SkSplitCSV(i)
121600030721     C                   EXSR      CHKNUM
121700030721     C                   IF        PiInt=*on  AND
121800030721     C                             PiVal<=999
121900030721     C                   Z-ADD     PiVal         VABCTR
122000030721     C                   ELSE
122100030721     C                   SETON                                        32
122200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122300030721     C                             + ' ' + 'VABCTR'
122400030721     C                   ENDIF
122500030721     C                   ENDIF
122600030721     C*** IAS
122700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
122800030721     C                   EVAL      PiStr=SkSplitCSV(i)
122900030721     C                   EXSR      CHKNUM
123000030721     C                   IF        PiNum=*on
123100030721     C                   Z-ADD     PiVal         VABIAS
123200030721     C                   ELSE
123300030721     C                   SETON                                        32
123400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123500030721     C                             + ' ' + 'VABIAS'
123600030721     C                   ENDIF
123700030721     C                   ENDIF
123800030721     C*** VLB
123900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
124000030721     C                   EVAL      PiStr=SkSplitCSV(i)
124100030721     C                   EXSR      CHKNUM
124200030721     C                   IF        PiNum=*on
124300030721     C                   Z-ADD     PiVal         VABVLB
124400030721     C                   ELSE
124500030721     C                   SETON                                        32
124600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124700030721     C                             + ' ' + 'VABVLB'
124800030721     C                   ENDIF
124900030721     C                   ENDIF
125000030721     C*** QFT
125100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
125200030721     C                   EVAL      PiStr=SkSplitCSV(i)
125300030721     C                   EXSR      CHKNUM
125400030721     C                   IF        PiNum=*on
125500030721     C                   Z-ADD     PiVal         VABQFT
125600030721     C                   ELSE
125700030721     C                   SETON                                        32
125800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125900030721     C                             + ' ' + 'VABQFT'
126000030721     C                   ENDIF
126100030721     C                   ENDIF
126200030721     C*** NCD
126300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
126400030721     C                   EVAL      PiStr=SkSplitCSV(i)
126500030721     C                   EXSR      CHKNUM
126600030721     C                   IF        PiInt=*on      AND
126700030721     C                             PiVal<=9999999
126800030721     C                   Z-ADD     PiVal         VABNCD
126900030721     C                   ELSE
127000030721     C                   SETON                                        32
127100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127200030721     C                             + ' ' + 'VABNCD'
127300030721     C                   ENDIF
127400030721     C                   ENDIF
127500030721     C*** NCA
127600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
127700030721     C                   EVAL      PiStr=SkSplitCSV(i)
127800030721     C                   EXSR      CHKNUM
127900030721     C                   IF        PiInt=*on      AND
128000030721     C                             PiVal<=9999999
128100030721     C                   Z-ADD     PiVal         VABNCA
128200030721     C                   ELSE
128300030721     C                   SETON                                        32
128400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128500030721     C                             + ' ' + 'VABNCA'
128600030721     C                   ENDIF
128700030721     C                   ENDIF
128800030721     C*** ZNC
128900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
129000030721     C                   EVAL      PiStr=SkSplitCSV(i)
129100030721     C                   EXSR      CHKNUM
129200030721     C                   IF        PiInt=*on AND
129300030721     C                             PiVal<=99
129400030721     C                   Z-ADD     PiVal         VABZNC
129500030721     C                   ELSE
129600030721     C                   SETON                                        32
129700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129800030721     C                             + ' ' + 'VABZNC'
129900030721     C                   ENDIF
130000030721     C                   ENDIF
130100030721     C*** DCR
130200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
130300030721     C                   EVAL      PiStr=SkSplitCSV(i)
130400030721     C                   EXSR      CHKNUM
130500030721     C                   IF        PiInt=*on       AND
130600030721     C                             PiVal<=99999999
130700030721     C                   Z-ADD     PiVal         VABDCR
130800030721     C                   ELSE
130900030721     C                   SETON                                        32
131000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131100030721     C                             + ' ' + 'VABDCR'
131200030721     C                   ENDIF
131300030721     C                   ENDIF
131400030721     C*** HCR
131500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
131600030721     C                   EVAL      PiStr=SkSplitCSV(i)
131700030721     C                   EXSR      CHKNUM
131800030721     C                   IF        PiInt=*on   AND
131900030721     C                             PiVal<=9999
132000030721     C                   Z-ADD     PiVal         VABHCR
132100030721     C                   ELSE
132200030721     C                   SETON                                        32
132300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132400030721     C                             + ' ' + 'VABHCR'
132500030721     C                   ENDIF
132600030721     C                   ENDIF
132700030721     C*** VMD
132800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
132900030721     C                   EVAL      PiStr=SkSplitCSV(i)
133000030721     C                   EXSR      CHKNUM
133100030721     C                   IF        PiNum=*on
133200030721     C                   Z-ADD     PiVal         VABVMD
133300030721     C                   ELSE
133400030721     C                   SETON                                        32
133500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133600030721     C                             + ' ' + 'VABVMD'
133700030721     C                   ENDIF
133800030721     C                   ENDIF
133900030721     C*** ANT
134000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
134100030721     C                   EVAL      PiStr=SkSplitCSV(i)
134200030721     C                   EXSR      CHKNUM
134300030721     C                   IF        PiInt=*on        AND
134400030721     C                             PiVal<=999999999
134500030721     C                   Z-ADD     PiVal         VABANT
134600030721     C                   ELSE
134700030721     C                   SETON                                        32
134800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134900030721     C                             + ' ' + 'VABANT'
135000030721     C                   ENDIF
135100030721     C                   ENDIF
135200030721     C*** CMO
135300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
135400030721     C                   EVAL      PiStr=SkSplitCSV(i)
135500030721     C                   EXSR      CHKNUM
135600030721     C                   IF        PiInt=*on
135700030721     C                   Z-ADD     PiVal         Num5_0
135800030721     C                   MOVEL(p)  Num5_0        VABCMO
135900030721     C                   ELSE
136000030721     C                   SETON                                        32
136100030721     C                   EVAL      VABCMO = *zeros
136200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136300030721     C                             + ' ' + 'VABCMO'
136400030721     C                   ENDIF
136500030721     C                   ENDIF
136600040510     C*
136700040510     C* Reperisco i campi del FIVAT
136800040510     C*** VATNOT
136900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
137000040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
137100040510     C                   ENDIF
137200040510     C*** VATTRC
137300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
137400040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
137500040510     C                   ENDIF
137600060331     C*** VATNOT_A
137700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
137800060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
137900060331     C                   ENDIF
138000060331     C*** VATNOT_B
138100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
138200060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
138300060331     C                   ENDIF
138400060331     C*** VATNOT_E
138500070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
138600060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
138700060331     C                   ENDIF
138800070823     C*
138900071029     C***  ===>  Gestione campi "particolari" dentro ciclo
139000071029     C* NSP => Stacco un numeratore da AZNUM
139100071029     C                   clear                   TRUL33DS
139200071029     C                   eval      I33OPE = *zeros
139300071029     C                   eval      I33CNU = 302
139400071029     C                   eval      I33NUM = 1
139500071029     C                   movel     TRUL33DS      KPJBU
139600071029     C                   call      'TRUL33R'
139700071029     C                   parm                    KPJBA
139800071029     C                   movel     KPJBU         TRUL33DS
139900071029     C                   if        O33ERR = *zeros
140000071029     C                   z-add     O33NRF        VABNSP
140100071029     C                   z-add     O33NRF        VATNSP
140200071029     C                   else
140300071029     C                   SETON                                        31
140400071029     C                   EVAL      vinmsg = %trimr(vinmsg)
140500071029     C                             + ' ' + 'VABNSP VATNSP'
140600071029     C                   endif
140700071106     C*** RMA/RMN
140800080418     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMA'
140900071106     C                   EVAL      VABRMA = SkSplitCSV(i)
141000080418     C                   EXSR      exeBon
141100080418     C*
141200080418     C* Se ok re-imposto con la stringa così "bonificata"
141300080418     C                   IF        EsitoBon = '0' OR EsitoBon = '1'
141400080418     C                   EVAL      PiStr = TxtInOut
141500071106     C                   EXSR      CHKNUM
141600071106     C                   IF        PiInt=*on
141700071106     C                   Z-ADD     PiVal         VABRMN
141800071106     C                   ELSE
141900071106     C                   SETON                                        32
142000071106     C                   EVAL      VABRMN = *zeros
142100071106     C                   EVAL      vinmsg = %trimr(vinmsg)
142200071106     C                             + ' ' + 'VABRMN'
142300071106     C                   ENDIF
142400071106     C                   ENDIF
142500080418     C                   ENDIF
142600070925     C*
142700071029     C***  <===  -----------------------------------------
142800040510     C*
142900030715     C                   ENDIF
143000030715     C                   EVAL      i = i + 1
143100030715     C                   ENDDO
143200070925     C*
143300071029     C***  ===>  Gestione campi "particolari" fuori ciclo
143400070925     C*
143500071029     C***  <===  -----------------------------------------
143600070925     C*
143700070925     C* Considerazioni finali su CBO/CAS
143800080418     C                   IF        wFlgCAS = '1'
143900070925     C                   IF        VABCBO = '1'
144000070925     C                   EVAL      VABCBO = '4'
144100070925     C                   ENDIF
144200070925     C                   IF        VABCBO = '2'
144300070925     C                   EVAL      VABCBO = '6'
144400070925     C                   ENDIF
144500070925     C                   ENDIF
144600070925     C***  <===  ----------------------------
144700040510     C*
144800040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
144900040510     C                   IF        VABNSP = *zeros
145000040510     C                   SETON                                        31
145100040510     C                   ENDIF
145200040715     C*
145300040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
145400040715     C                   IF        VABLNP = *zeros
145500040715     C                   SETON                                        31
145600040715     C                   ENDIF
145700040510     C*
145800040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
145900040510     C                   IF        VABRMN = *zeros
146000040510     C                   Z-ADD     VABNSP        VABRMN
146100040510     C                   ENDIF
146200040510     C*
146300040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
146400040510     C                   IF        VABRMA = *blanks
146500040714     C                   MOVEL     VABRMN        VABRMA
146600040510     C                   ENDIF
146700040128     C*
146800040510     C* Se provincia nn valorizzata la reperisco
146900040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
147000040510     C                   IF        VABPRD = *blanks
147100040128     C                   CLEAR                   TISI95DS
147200040128     C                   EVAL      I95TCN = '3'
147300040128     C                   Z-ADD     datcor        I95DAT
147400040128     C                   EVAL      I95NAR = VABNZD
147500040128     C                   EVAL      I95CAP = VABCAD
147600040128     C                   EVAL      I95LOC = VABLOD
147700040128     C                   CALL      'TISI95R'
147800040128     C                   PARM                    TISI95DS
147900040128     C                   EVAL      VABPRD = O95PRV
148000040510     C                   ENDIF
148100040113     C*
148200040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
148300040113     C                   IF        VABCAS > *zeros
148400040119     C                   EVAL      VABVCA = 'EUR'
148500040119     C                   IF        VABCBO = '1'
148600040113     C                   EVAL      VABCBO = '4'
148700040119     C                   ENDIF
148800040119     C                   IF        VABCBO = '2'
148900040119     C                   EVAL      VABCBO = '6'
149000040119     C                   ENDIF
149100040113     C                   ENDIF
149200020204     C*
149300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
149400020204     C                   EXSR      CHKIMPDIV
149500030715     C                   ENDIF
149600020204     C*
149700010607     C                   ENDSR
149800010607     C*----------------------------------------------------*
149900010601
150000020204
150100020204     C*----------------------------------------------------*
150200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
150300020204     C*----------------------------------------------------*
150400020204     C     CHKIMPDIV     BEGSR
150500020204     C*
150600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
150700020204     C                   Z-ADD     *zeros        wrkDec            9 9
150800020204     C*
150900020204     C* Come prima cosa effettuo considerazioni sulla divisa
151000020204     C                   IF        vabIAS > *zeros
151100020204     C                   IF        vabVAS <> 'EUR'
151200020204     C                   EVAL      vabVAS =  'ITL'
151300020204     C                   ENDIF
151400020204     C                   ENDIF
151500020204     C*
151600020204     C                   IF        vabCAS > *zeros
151700020204     C                   IF        vabVCA <> 'EUR'
151800020204     C                   EVAL      vabVCA =  'ITL'
151900020204     C                   ENDIF
152000020204     C                   ENDIF
152100020204     C*
152200020204     C                   IF        vabVMD > *zeros
152300020204     C                   IF        vabVAD <> 'EUR'
152400020204     C                   EVAL      vabVAD =  'ITL'
152500020204     C                   ENDIF
152600020204     C                   ENDIF
152700020204     C*
152800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
152900020204     C                   Z-ADD     vabIAS        wrkDec
153000020204     C                   IF        wrkDec > *zeros
153100020204     C                   IF        vabVAS = 'ITL'
153200020204     C                   EVAL      vabIAS = *zeros
153300020204     C                   ENDIF
153400020204     C                   ENDIF
153500020204     C*
153600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
153700020204     C                   Z-ADD     vabCAS        wrkDec
153800020204     C                   IF        wrkDec > *zeros
153900020204     C                   IF        vabVCA = 'ITL'
154000020204     C                   EVAL      vabCAS = *zeros
154100020204     C                   ENDIF
154200020204     C                   ENDIF
154300020204     C*
154400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
154500020204     C                   Z-ADD     vabVMD        wrkDec
154600020204     C                   IF        wrkDec > *zeros
154700020204     C                   IF        vabVAD = 'ITL'
154800020204     C                   EVAL      vabVMD = *zeros
154900020204     C                   ENDIF
155000020204     C                   ENDIF
155100020204     C*
155200020204     C                   ENDSR
155300020204     C***
155400020204
155500010330
155600010330
155700010330     C*----------------------------------------------------*
155800010330     C*  CONTROLLO NUMERICITA' CAMPI
155900010330     C*----------------------------------------------------*
156000010330     C     CHKNUM        BEGSR
156100010330     C*
156200010606     C                   IF        PiDecChr = *blanks
156300030715     C                   EVAL      PiDecChr = CharNUM
156400010606     C                   ENDIF
156500010606     C*
156600010606     C                   CALL(e)   'ISNUMERIC'
156700010330     C                   PARM                    PiStr            30
156800010606     C                   PARM                    PiDecChr          1
156900010330     C                   PARM      *ZEROS        PiVal            30 9
157000010330     C                   PARM      '0'           PiInt             1
157100010330     C                   PARM      '0'           PiNum             1
157200010330     C                   IF        %error
157300010606     C                   EVAL      PiNum=*off
157400010330     C                   ENDIF
157500010330     C*
157600010330     C                   ENDSR
157700010330     C***
157800010330
157900010601
158000010601
158100010601
158200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
158300010601     C     repfil        BEGSR
158400010601     C*
158500010601     C                   if        invfil = *zeros and
158600010601     C                             depfil > *zeros and
158700010629     C                             (vinflg = *blanks or
158800010629     C                              vinflg = *zeros)
158900010601     C
159000010601     C                   eval      invfil = depfil
159100010601     C                   endif
159200010601     C*
159300010601     C                   if        depfil <> invfil and
159400010601     C                             invfil > *zeros
159500010601     C                   eval      flgMulti = '1'
159600010601     C                   if        vinflg = *blanks
159700010601     C                   add       1             cntNonEl
159800010601     C                   endif
159900010601     C                   endif
160000010601     C*
160100010601     C                   if        vinflg = '2'
160200010601     C                   eval      flgStato = '2'
160300010601     C                   endif
160400010601     C*
160500010601     C                   ENDSR
160600010601     C***
160700010601
160800010601
160900010601
161000010330
161100040119      /TITLE Invio dei dati al punto operativo.
161200040119     C     invio         BEGSR
161300040119     C*
161400040119     C* 1° invio FIVAT
161500040119     C                   reset                   dscmz
161600040119     C                   move      vlrpoi        cmzdst
161700040119     C                   eval      cmzfld = 'FIVATWWR'
161800040119     C                   eval      cmzmbd = vlrhdl
161900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
162000040119     C***                if        prmfir = *blanks
162100040119     C                   eval      cmzfla = 'FIVAT00F'
162200040119     C                   eval      cmzmba = 'FIVAT00F'
162300040119     C***                else
162400040119     C***                eval      cmzfla = prmfir
162500040119     C***                eval      cmzmba = prmfir
162600040119     C***                endif
162700040119     C                   eval      cmznrr = *zeros
162800040119     C                   move      §ctrokvt      cmznrr
162900040119     C                   eval      cmzlba = vlrfl1
163000040119     C                   call(e)   'TIS711C'
163100040119     C                   parm                    dscmz
163200040119     C                   parm      *blanks       esito
163300040119     C                   if        %error
163400040119     C                             or cmzerr = '1'
163500040119     C                             or esito  = '1'
163600040119     C                   eval      wrkesito = '3'
163700040119     C                   else
163800040119     C*
163900040119     C* 2° invio FIVAB
164000040119     C                   reset                   dscmz
164100040119     C                   move      vlrpoi        cmzdst
164200040119     C                   eval      cmzfld = vlrfou
164300040119     C                   eval      cmzmbd = vlrhdl
164400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164500040119     C***                if        prmfir = *blanks
164600040119     C                   eval      cmzfla = 'FIVAB00F'
164700040119     C                   eval      cmzmba = 'FIVAB00F'
164800040119     C***                else
164900040119     C***                eval      cmzfla = prmfir
165000040119     C***                eval      cmzmba = prmfir
165100040119     C***                endif
165200040119     C                   eval      cmznrr = *zeros
165300040119     C                   move      §ctrokvb      cmznrr
165400040119     C                   eval      cmzlba = vlrfl1
165500040119     C                   call(e)   'TIS711C'
165600040119     C                   parm                    dscmz
165700040119     C                   parm      *blanks       esito
165800040119     C                   if        %error
165900040119     C                             or cmzerr = '1'
166000040119     C                             or esito  = '1'
166100040119     C                   eval      wrkesito = '3'
166200040119     C                   endif
166300040119     C                   endif
166400040119     C*
166500040119     C                   ENDSR
166600040119     C***
166700010601
166800010601
166900010601
167000010601
167100010601      /TITLE Invio dei dati al punto operativo.
167200010601     C     opeini        BEGSR
167300010601     C*
167400010601     C* Inizializzo flag e contatori operativi
167500010601     C                   movel     '0'           flgGiro           1
167600010601     C                   movel     '0'           flgMulti          1
167700010601     C                   movel     '1'           flgStato          1
167800010615     C                   movel     '0'           flgOk             1
167900010601     C                   z-add     *zeros        cntNonEl         10 0
168000010601     C                   z-add     *zeros        depfil            3 0
168100010601     C                   z-add     *zeros        invfil            3 0
168200010601     C*
168300010601     C                   ENDSR
168400010601     C***
168500070326
168600070326
168700070326
168800070326
168900070326     C     *pssr         BEGSR
169000070329     C*
169100070329     C                   if        %open(tivin00r)
169200070329     C                   close     tivin00r
169300070329     C                   endif
169400070329     C                   if        %open(fivabwwr)
169500070329     C                   close     fivabwwr
169600070329     C                   endif
169700070329     C                   if        %open(fivatwwr)
169800070329     C                   close     fivatwwr
169900070329     C                   endif
170000070326     C*
170100070326     C* Effettuo la chiamata al CLLE preposto
170200070326     C                   call(e)   'TITVVTC'
170300070326     C                   parm                    parccm
170400070326     C                   parm                    parmbr
170500070326     C                   parm      '2'           paropz
170600070326     C*
170700070326     C                   eval      wrkesito = '2'
170800070404     C*
170900070404     C                   seton                                        LR
171000070326     C*
171100070326     C                   ENDSR     '*CANCL'
171200070326     C***
171300080418
171400080418
171500080418
171600080418      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
171700080418     C     exeBon        BEGSR
171800080418     C*
171900080418     C* Imposto i parametri x il *pgm d "bonifica"
172000080418     C                   EVAL      ElencoChar = '1234567890'
172100080418     C                   EVAL      TipoElenco = *blanks
172200080418     C                   EVAL      CharSost   = *blanks
172300080418     C                   EVAL      UpperCase  = *blanks
172400080418     C                   EVAL      ChkNull    = '1'
172500080418     C                   EVAL      CharNull   = *blanks
172600080418     C*
172700080418     C* Effettuo la chiamata al *pgm d "bonifica" x il campo VABRMA
172800080418     C                   EVAL      TxtInOut   = VABRMA
172900080418     C                   EVAL      EsitoBon   = *blanks
173000080418     C*
173100080418     C                   CALL      'XCHKCHAR'
173200080418     C                   PARM                    TxtInOut
173300080418     C                   PARM                    ElencoChar
173400080418     C                   PARM                    TipoElenco
173500080418     C                   PARM                    CharSost
173600080418     C                   PARM                    UpperCase
173700080418     C                   PARM                    ChkNull
173800080418     C                   PARM                    CharNull
173900080418     C                   PARM                    EsitoBon
174000080418     C*
174100080418     C                   ENDSR
174200070326
174300070326
174400010330
174500010330
174600000613     C     *inzsr        BEGSR
174700990910     C*
174800990910     C     *entry        plist
174900990920     C                   parm                    tivlrds
175000990921     C                   parm      wrkesito      esito
175100000724     C                   parm                    prmlit
175200000710     C                   parm                    prmfir
175300010330     C*
175400010330     C* CALCOLA LA DATA CORRENTE
175500010330     C                   time                    wn14             14 0
175600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
175700010330     C                   z-add     wn8           g08dat
175800010330     C                   z-add     *zeros        g08inv
175900010330     C                   movel     '0'           g08err
176000010330     C                   call      'XSRDA8'
176100010330     C                   parm                    wlbda8
176200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
176300000613     C*
176400000613     C                   ENDSR
176500000613     C***
176600990908
176700080418     Otitv1f9p  E            riepilogo         2
176800990915     O                                              'Upload via Internet'
176900990915     O                                           +1 'Traduzione TIVIN00R -'
177000021025     O                                           +1 'FIVABWWR'
177100990915     O                                           +1 'Testate bolle'
177200990915     O          E            riepilogo   2
177300990915     O                       wrkdata
177400990915     O                       wrkora              +1
177500990915     O                       procname            +1
177600990915     O          E            riepilogo   2
177700990915     O                                              'Cliente..................:'
177800010605     O                       VABCCM        z     +1
177900990915     O          E            riepilogo   2
178000990920     O                                              'Riferimento Strategi.....:'
178100990920     O                       vlrhdl              +1
178200990915     O          E            riepilogo   2
178300990915     O                                              'Giusti...................:'
178400040119     O                       §CTROKVB      2   +  1
178500990915     O          E            riepilogo   2
178600990915     O                                              'Sbagliati e corretti.....:'
178700971022     O                       §CTRMO        2   +  1
178800990915     O          E            riepilogo   2
178900990915     O                                              'Sbagliati e scartati.....:'
179000971022     O                       §CTRNO        2   +  1
179100000613
179200080418     Otitv1f9ps E            testdett          2
179300000613     O                                              'Upload via Internet'
179400000613     O                                           +1 'Traduzione TIVIN00R -'
179500021025     O                                           +1 'FIVABWWR'
179600000613     O                                           +1 'Testate bolle'
179700000616     O          E            testdett    3
179800000613     O                                           +2 'N° rec'
179900000613     O                                           +3 'Anteprima contenuto'
180000000616     O          E            rigadett    2
180100000613     O                       rrnum               +2
180200000621     O                       recko               +3
180300000616     O          E            findett     2
180400000613     O                       wrkdata
180500000613     O                       wrkora              +1
180600000613     O                       procname            +1
180700000616     O          E            findett     2
180800000613     O                                              'Cliente..................:'
180900010605     O                       VABCCM        z     +1
181000000616     O          E            findett     2
181100000613     O                                              'Riferimento Strategi.....:'
181200000613     O                       vlrhdl              +1
181300000616     O          E            findett     2
181400000613     O                                              'Giusti...................:'
181500040119     O                       §CTROKVB      2   +  1
181600000616     O          E            findett     2
181700000613     O                                              'Sbagliati e corretti.....:'
181800000613     O                       §CTRMO        2   +  1
181900000616     O          E            findett     2
182000000613     O                                              'Sbagliati e scartati.....:'
182100000613     O                       §CTRNO        2   +  1
182200000613** CMD - COMANDI CL
182300080418OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1F9P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
182400080418OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1F9PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
182500080418DLTOVR FILE(TITV1F9P TITV1F9PS) LVL(*)
182600010606
182700000613
182800040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
182900080418VABRSD;VABRD2;VABIND;VABCAD;VABLOD;VABPRD;VABNCL;VABPKB;FILL01;W_VABRMA;
183000080506VATNOT_A;VATNOT_B;VABNOT;VABNT2;FILL02;FILL03
183100080418
