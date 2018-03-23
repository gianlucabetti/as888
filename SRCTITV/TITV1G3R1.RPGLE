000100080623      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200080418     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600080623     FEDIVABwr  O    E             DISK    usropn
000700080623     FEDIVATwr  O    E             DISK    usropn
000800080513     Ftitv1g3p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070329     F                                     infsr(*pssr)
001100080513     Ftitv1g3ps O    f  198        PRINTER usropn
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
002200080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
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
004400080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004500080418     D wFlgCAS         s              1    INZ(*blanks)
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
005800070823     D wVATNOT_A       s                   LIKE(VATNOT)
005900060331     D wVATNOT_B       s                   LIKE(VATNOT)
006000060331     D wVATNOT_E       s                   LIKE(VATNOT)
006100080513     D wVATNOT_B1      s                   LIKE(VATNOT)
006200080513     D wVATNOT_B2      s                   LIKE(VATNOT)
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
007800080418     D*------------------
007900080418     D* Costanti
008000080418     D*------------------
008100080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008200080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008300080418     D*------------------
008400080418     D* VARIABILI X "XCHKCHAR"
008500080418     D*------------------
008600080418     D TxtInOut        S           2048
008700080418     D ElencoChar      S            256
008800080418     D TipoElenco      S              1
008900080418     D CharSost        S              1
009000080418     D UpperCase       S              1
009100080418     D ChkNull         S              1
009200080418     D CharNull        S              1
009300080418     D EsitoBon        S              1
009400080418     D*------------------
009500010330
009600010330
009700010330
009800990915     C                   time                    wrkdata
009900990915     C                   time                    wrkora
010000000913     C                   reset                   rrnum
010100990921     C                   reset                   esito
010200990921     C                   reset                   wrkesito
010300010601     C*
010400010601     C                   exsr      opeini
010500010605     C                   exsr      rwvab
010600010601     C*
010700010601     C                   seton                                        lr
010800010601
010900010601
011000010601
011100010601     C*--------------------------------------------------------
011200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011300010601     C*--------------------------------------------------------
011400010601     C     PREELA        BEGSR
011500010601     C*
011600010601     C* SE OCCORRE SPEDIRE IN FILIALE
011700010601     C                   if        invfil <> *zeros and
011800010601     C                             flgGiro = '0'
011900010601     C*
012000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012100010601     C                   eval      flgGiro = '1'
012200000724     C*
012300000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
012400000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012500010601     C     invfil        chain     azorg01l
012600000724     C                   if        %found
012700000724     C                   movel(p)  CMD(1)        depcmd
012800020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
012900000724     C*
013000000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
013100011113     C                   Z-ADD     150           LENGH            15 5
013200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013300000724     C                   PARM                    depcmd
013400000724     C                   PARM                    LENGH
013500000724     C*
013600000724     C                   endif
013700000616     C*
013800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013900000616     C                   movel(p)  CMD(2)        depcmd
014000000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
014100000616     C*
014200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
014300011113     C                   Z-ADD     150           LENGH            15 5
014400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014500000616     C                   PARM                    depcmd
014600000616     C                   PARM                    LENGH
014700000613     C*
014800080513     C                   if        not %open(titv1g3ps)
014900080513     C                   open      titv1g3ps
015000070329     C                   except    testdett
015100000613     C                   endif
015200010601     C*
015300010601     C                   endif
015400010601     C*
015500010601     C                   ENDSR
015600010601     C***
015700010601
015800010601
015900010601
016000010601
016100010601     C*--------------------------------------------------------
016200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016300010601     C*--------------------------------------------------------
016400010601     C     ENDELA        BEGSR
016500990915     C*
016600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
016700040119     C*
016800040119     C* Effettuo la chiamata al CLLE preposto
016900080623     C                   call(e)   'TITVEVTC'
017000040510     C                   parm                    parccm
017100040119     C                   parm                    parmbr
017200040119     C                   parm      '2'           paropz
017300050201     C*
017400050201     C* Effettuo lancio TISI95 solo x chiusura
017500050201     C                   CLEAR                   TISI95DS
017600050201     C                   EVAL      I95TLA = 'C'
017700050201     C                   CALL      'TISI95R'
017800050201     C                   PARM                    TISI95DS
017900000613     C*
018000080513     C                   if        %open(titv1g3ps)
018100000616     C                   except    findett
018200080513     C                   close     titv1g3ps
018300000613     C                   endif
018400000616     C*
018500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018600011113     C                   Z-ADD     150           LENGH            15 5
018700010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
018800000616     C                   PARM                    CMD(3)
018900000616     C                   PARM                    LENGH
019000000616     C*
019100010601     C                   ENDSR
019200010601     C***
019300010601
019400010601
019500010601
019600000613
019700000613     C*--------------------------------------------------------
019800000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
019900000613     C*--------------------------------------------------------
020000000621     C     STPR          BEGSR
020100000613     C*
020200080513     C                   if        not %open(titv1g3p)
020300080513     C   50              open      titv1g3p
020400990915     C                   endif
020500990915     C*
020600030715     C   50              except    riepilogo
020700990915     C*
020800080513     C                   if        %open(titv1g3p)
020900080513     C   50              close     titv1g3p
021000990914     C                   endif
021100000613     C*
021200000613     C                   ENDSR
021300000613     C***
021400000613
021500010601
021600010601
021700010330     C*--------------------------------------------------------
021800080623     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
021900010330     C*--------------------------------------------------------
022000010605     C     RWVAB         BEGSR
022100010330     C*
022200010330     C                   if        not %open(tivin00r)
022300010330     C                   open      tivin00r
022400010330     C                   endif
022500080623     C                   if        not %open(edivabwr)
022600080623     C                   open      edivabwr
022700010330     C                   endif
022800040119     C*
022900080623     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
023000040119     C                   exsr      prevat
023100040119     C*
023200040119     C                   if        chkcall = '0'
023300040119     C*
023400080623     C                   if        not %open(edivatwr)
023500080623     C                   open      edivatwr
023600040119     C                   endif
023700010330     C*
023800040119     C                   clear                   §CTROKVB          7 0
023900040119     C                   clear                   §CTROKVT          7 0
024000040119     C                   clear                   §CTRMO            7 0
024100040119     C                   clear                   §CTRNO            7 0
024200070823     C*
024300070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
024400070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
024500080513     C***                exsr      inzvar
024600080513     C***                exsr      impvab
024700010330     C*
024800030822     C                   do        *HIVAL
024900010330     C*
025000030822     C                   read      tivin00r                               70
025100010618     C*
025200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
025300010618     C                   if        vinflg = '1'
025400010618     C                   eval      flgOk = '1'
025500010618     C                   endif
025600040510     C*
025700040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
025800040510     C                   z-add     *zeros        wLenVINDTA        4 0
025900040510     C                   z-add     *zeros        wFlgVINDTA        1 0
026000040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
026100040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
026200040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
026300040510     C                   z-add     1             wFlgVINDTA
026400040510     C                   leave
026500040510     C                   endif
026600040510     C                   enddo
026700010618     C*
026800040510     C                   if        vindta > *blanks AND
026900040510     C                             wFlgVINDTA = 1
027000010330     C                   add       1             rrnum
027100010330     C*
027200010601     C                   if        *in70 = *off and
027300010330     C                             (vinflg = *blanks
027400010330     C                              or vinflg = '0'
027500010330     C                              or vinflg = '2')
027600010330     C*
027700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
027800010711     C                   if        vinflg = *blanks or vinflg = '0'
027900010711     C                   clear                   vinmsg
028000010711     C                   endif
028100010601     C*
028200070925     C* Aggiungo un byte a blanks all'inizio d ogni record
028300070925     C                   eval      vindta  = ' ' + %trim(vindta)
028400070925     C*
028500010330     C                   exsr      inzvar
028600010605     C                   exsr      impvab
028700010601     C*
028800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
028900010605     C                   eval      depfil = VABLNP
029000010601     C                   exsr      repfil
029100010601     C                   if        depfil = invfil
029200021025     C                   if        vlrpoi = 999
029300030822     C                   move(P)   invfil        VABFGS
029400021025     C                   else
029500030822     C                   move(P)   vlrpoi        VABFGS
029600021025     C                   endif
029700010601     C*
029800010601     C                   exsr      PREELA
029900010601     C*
030000010604     C* Ebbene...
030100010604     C*
030200040119     C  N31              add       1             §CTROKVB
030300040119     C   32              add       1             §CTRMO
030400040119     C   31              add       1             §CTRNO
030500080418     C*
030600030822     C                   if        wGiro = 2
030700080513     C* Scrivo sempre il VAT
030800080513     C  N31              exsr      wrivat
030900080513     C* Scrivo il VAB solo a rottura d spedizione
031000080513     C                   if        depspe =  *zeros OR
031100080513     C                             depspe <> vabNSP
031200080513     C  N31              exsr      wrivab
031300080513     C  N31              eval      depspe = vabNSP
031400080513     C                   endif
031500080513     C                   endif
031600010604     C*
031700010604     C                   if        *in31 = *off and
031800010604     C                             *in32 = *off
031900010604     C                   eval      vinflg = '1'
032000010604     C                   else
032100010604     C                   eval      recko = vindta
032200010604     C                   except    rigadett
032300010604     C                   eval      vinflg = '2'
032400010604     C                   endif
032500010604     C                   endif
032600010601     C*
032700010604     C                   endif
032800010604     C*
032900010330     C                   else
033000010330     C                   eval      vinflg = '1'
033100010330     C                   endif
033200010601     C*
033300010601     C  N70              update    tivin000
033400010330     C*
033500030822     C  N70              enddo
033600040119     C*
033700040119     C                   endif                                                  (endif - chkcall)
033800010601     C*
033900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
034000010601     C                   if        cntNonEl = *zeros or
034100010601     C                             flgMulti = '0'
034200010330     C* Se non ci sono record con errori ...
034300010601     C                   if        §ctrno = 0 and
034400010604     C                             §ctrmo = 0 and
034500010601     C                             flgStato <> '2'
034600010330     C* ... restituisco esito OK.
034700010330     C                   eval      wrkesito = '0'
034800010330     C                   else
034900040119     C                   if        §ctrokvb > 0
035000010330     C                   eval      wrkesito = '1'
035100010330     C                   else
035200010615     C                   if        flgOk = '0'
035300010615     C                   eval      wrkesito = '2'
035400010615     C                   else
035500010615     C                   eval      wrkesito = '6'
035600010615     C                   endif
035700010330     C                   endif
035800010330     C                   endif
035900010601     C                   else
036000010601     C                   eval      wrkesito = '9'
036100010601     C                   endif
036200010330     C*
036300010330     C                   if        %open(tivin00r)
036400010330     C                   close     tivin00r
036500010330     C                   endif
036600080623     C                   if        %open(edivabwr)
036700080623     C                   close     edivabwr
036800010330     C                   endif
036900080623     C                   if        %open(edivatwr)
037000080623     C                   close     edivatwr
037100040512     C                   endif
037200010601     C*
037300010601     C                   if        vlrpoi <> 999
037400010601     C                   eval      invfil = vlrpoi
037500010601     C                   endif
037600010330     C*
037700040119     C                   if        §ctrokvb > 0
037800010601     C                             and invfil > *zeros
037900010330     C                   exsr      invio
038000010330     C                   endif
038100010601     C*
038200010618     C                   if        flgGiro = '1'
038300010601     C                   exsr      endela
038400010618     C                   endif
038500010330     C*
038600010330     C                   ENDSR
038700010330     C***
038800040119
038900040119
039000040119
039100040119     C*----------------------------------------------------*
039200040119     C*  SCARICAMENTO BUFFER RECORDS VAB
039300040119     C*----------------------------------------------------*
039400040510     C     WRIVAB        BEGSR
039401080623     C*
039402080623     C* Valorizzo i campi relativi al CMR
039403080623     C                   MOVEL     datoracor     VABCMR
039404080623     C                   EVAL      VABDCM = datcor
039405080623     C                   EVAL      VABDTS = datcor
039406080623     C                   EVAL      VABHMS = oracor
039407080623     C                   EVAL      VABCNT = 1
039500080513     C*
039600080513     C* Gestisco l'eventuale rottura x numero spedizione
039700080513     C                   if        VABNSP <> depspe
039800070301     C                   if        wVATNOT_E <> *blanks
039900070301     C                   eval      VABCTM = '7Q'
040000070301     C                   endif
040100080623     C                   write     edivab00                                     => scarico il VAB
040200080513     C                   endif
040300040119     C*
040400040119     C                   ENDSR
040500040119
040600040119
040700040119
040800040119     C*----------------------------------------------------*
040900040119     C*  SCARICAMENTO BUFFER RECORDS VAT
041000040119     C*----------------------------------------------------*
041100040510     C     WRIVAT        BEGSR
041101080623     C*
041103080623     C* Valorizzo i campi relativi al CMR
041104080623     C                   MOVEL     datoracor     VATCMR
041108080623     C                   EVAL      VATCNT = 1
041200060512     C*
041300060512     C* Inizializzo i campi chiave
041400060512     C                   eval      VATFGS = VABFGS
041500060512     C                   eval      VATAAS = VABAAS
041600060512     C                   eval      VATCCM = VABCCM
041700070823     C                   eval      VATNRS = VABNRS
041800060512     C                   eval      VATNSP = VABNSP
041900060512     C                   eval      VATLNP = VABLNP
042000060331     C*
042100060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
042200060331     C                   if        wVATNOT_A <> *blanks
042300060331     C                   eval      VATNOT = wVATNOT_A
042400060331     C                   eval      VATTRC = 'A'
042500080623     C                   write     EDIVAT00
042600060331     C                   add       1             §CTROKVT
042700060331     C                   endif
042800060331     C*
042900060331     C                   if        wVATNOT_B <> *blanks
043000060331     C                   eval      VATNOT = wVATNOT_B
043100060331     C                   eval      VATTRC = 'B'
043200080623     C                   write     EDIVAT00
043300060331     C                   add       1             §CTROKVT
043400060331     C                   endif
043500060331     C*
043600060331     C                   if        wVATNOT_E <> *blanks
043700060331     C                   eval      VATNOT = wVATNOT_E
043800080514     C     '''':' '      xlate     VATNOT        VATNOT
043900080514     C                   eval      VATNOT = %trim(VATNOT)
044000060331     C                   eval      VATTRC = 'E'
044100080514     C*
044200080514     C* Compongo tanti "chi sono" quanti sono i colli della spedizione
044300080522     C                   MOVEL     VATNOT        wVATNOT          35
044400080514     C     1             DO        VABNCL        wColloNum         3 0
044500080514     C                   MOVE(P)   wColloNum     wColloAlf         3
044600080522     C                   EVAL      VATNOT = %trim(wVATNOT)+wColloAlf
044700080623     C                   WRITE     EDIVAT00
044800080514     C                   add       1             §CTROKVT
044900080514     C                   ENDDO
045000060331     C                   endif
045100040119     C*
045200040119     C                   ENDSR
045300040119
045400040119
045500040119
045600040119     C*----------------------------------------------------*
045700080623     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
045800040119     C*----------------------------------------------------*
045900040119     C     PREVAT        BEGSR
046000040119     C*
046100080623     C* Compongo il nome del membro da dare al EDIVATWR
046200040119     C                   eval      parmbr = vlrhdl
046300040119     C                   movel     'M'           parmbr
046400040510     C                   eval      parccm = vlrksc
046500040119     C                   eval      paropz = '1'
046600040119     C* Effettuo la chiamata al CLLE preposto
046700080623     C                   call(e)   'TITVEVTC'
046800040119     C                   parm                    parccm
046900040119     C                   parm                    parmbr
047000040119     C                   parm                    paropz
047100040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047200040119     C                   if        %error
047300040119     C                   movel     '1'           chkcall
047400040119     C                   else
047500040119     C                   movel     '0'           chkcall
047600040119     C                   endif
047700040119     C*
047800040119     C                   ENDSR
047900010601
048000010601
048100010601
048200010330     C*----------------------------------------------------*
048300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
048400010330     C*----------------------------------------------------*
048500040119     C     INZVAR        BEGSR
048600010330     C*
048700030822     C* Inizializzo variabili di wrk
048800020204     C                   Z-ADD     *zeros        Num5_0            5 0
048900060331     C                   MOVEL     *blanks       wVATNOT_A
049000060331     C                   MOVEL     *blanks       wVATNOT_B
049100060331     C                   MOVEL     *blanks       wVATNOT_E
049200030822     C*
049300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
049400080623     C                   CLEAR                   EDIVAB00
049500080623     C                   CLEAR                   EDIVAT00
049600030822     C                   CLEAR                   SkSplitCSV
049700030822     C                   EVAL      i = 1
049800030822     C                   EVAL      posDa = *zeros
049900030822     C                   EVAL      posA  = *zeros
050000030822     C*
050100030822     C* Reimposto i valori di default
050200030822     C                   EXSR      DEFCAM
050300030822     C*
050400010330     C                   ENDSR
050500010330     C*----------------------------------------------------*
050600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
050700010330     C*----------------------------------------------------*
050800010330     C     DEFCAM        BEGSR
050900030822     C*
051000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
051100030715     C* e delimitatore testo.
051200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
051300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
051400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
051500030822     C*
051600030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
051700030715     C                   IF        %subst(vlrppt:1:1) = 'S'
051800030715     C                   SETON                                        50
051900030715     C                   ELSE
052000030715     C                   SETOFF                                       50
052100030715     C                   ENDIF
052200030822     C*
052300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
052400030822     C                   EVAL      posDaDft = 1
052500030822     C                   EVAL      posADft  = 0
052600030822     C                   EVAL      wGiroDft = 0
052700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
052800030822     C                             posDaDft > 0
052900030822     C*
053000030822     C* Gestisco il 1° giro
053100030822     C                   IF        wGiroDft = 0
053200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
053400030822     C* Incremento il contatore dei "giri"
053500030822     C                   EVAL      wGiroDft = 1
053600030822     C                   ELSE
053700030822     C                   EVAL      posDaDft = posADft
053800030822     C                   ENDIF
053900030822     C* Eseguo lo scan x trovare la fine del campo corrente
054000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
054100030822     C*
054200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
054400030822     C                   IF        posDaDft > 0 AND
054500030822     C                             posADft  > 0
054600030822     C* NCL
054700030822     C                   IF        %subst(
054800030822     C                             %subst(vlrppt:posDaDft+1:
054900030822     C                             posADft-posDaDft-1):1:3)
055000030822     C                             = 'NCL'
055100030822     C                   EVAL      PiStr=%trim(%subst(
055200030822     C                             %subst(vlrppt:posDaDft+1:
055300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055400030822     C                   EXSR      CHKNUM
055500030822     C                   IF        PiInt=*on
055600030822     C                   Z-ADD     PiVal         VABNCL
055700030822     C                   ENDIF
055800030822     C                   ENDIF
055900030822     C* CCM
056000030822     C                   IF        %subst(
056100030822     C                             %subst(vlrppt:posDaDft+1:
056200030822     C                             posADft-posDaDft-1):1:3)
056300030822     C                             = 'CCM'
056400030822     C                   EVAL      PiStr=%trim(%subst(
056500030822     C                             %subst(vlrppt:posDaDft+1:
056600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056700030822     C                   EXSR      CHKNUM
056800030822     C                   IF        PiInt=*on
056900030822     C                   Z-ADD     PiVal         VABCCM
057000030822     C                   ENDIF
057100030822     C                   ENDIF
057200030822     C* LNP
057300030822     C                   IF        %subst(
057400030822     C                             %subst(vlrppt:posDaDft+1:
057500030822     C                             posADft-posDaDft-1):1:3)
057600030822     C                             = 'LNP'
057700030822     C                   EVAL      PiStr=%trim(%subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058000030822     C                   EXSR      CHKNUM
058100030822     C                   IF        PiInt=*on
058200030822     C                   Z-ADD     PiVal         VABLNP
058300030822     C                   ENDIF
058400030822     C                   ENDIF
058500030822     C* NRS
058600030822     C                   IF        %subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):1:3)
058900030822     C                             = 'NRS'
059000030822     C                   EVAL      PiStr=%trim(%subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059300030822     C                   EXSR      CHKNUM
059400030822     C                   IF        PiInt=*on
059500030822     C                   Z-ADD     PiVal         VABNRS
059600030822     C                   ENDIF
059700030822     C                   ENDIF
059800030822     C* CTR
059900030822     C                   IF        %subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):1:3)
060200030822     C                             = 'CTR'
060300030822     C                   EVAL      PiStr=%trim(%subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060600030822     C                   EXSR      CHKNUM
060700030822     C                   IF        PiInt=*on
060800030822     C                   Z-ADD     PiVal         VABCTR
060900030822     C                   ENDIF
061000030822     C                   ENDIF
061100030822     C* PKB
061200030822     C                   IF        %subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):1:3)
061500030822     C                             = 'PKB'
061600030822     C                   EVAL      PiStr=%trim(%subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061900030822     C                   EXSR      CHKNUM
062000030822     C                   IF        PiNum=*on
062100030822     C                   Z-ADD     PiVal         VABPKB
062200030822     C                   ENDIF
062300030822     C                   ENDIF
062400030822     C* VLB
062500030822     C                   IF        %subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):1:3)
062800030822     C                             = 'VLB'
062900030822     C                   EVAL      PiStr=%trim(%subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063200030822     C                   EXSR      CHKNUM
063300030822     C                   IF        PiNum=*on
063400030822     C                   Z-ADD     PiVal         VABVLB
063500030822     C                   ENDIF
063600030822     C                   ENDIF
063700030822     C* QFT
063800030822     C                   IF        %subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):1:3)
064100030822     C                             = 'QFT'
064200030822     C                   EVAL      PiStr=%trim(%subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064500030822     C                   EXSR      CHKNUM
064600030822     C                   IF        PiNum=*on
064700030822     C                   Z-ADD     PiVal         VABQFT
064800030822     C                   ENDIF
064900030822     C                   ENDIF
065000030822     C* CBO
065100030822     C                   IF        %subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):1:3)
065400030822     C                             = 'CBO'
065500030822     C                   EVAL      VABCBO=%trim(%subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065800030822     C                   ENDIF
065900030822     C* TSP
066000030822     C                   IF        %subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):1:3)
066300030822     C                             = 'TSP'
066400030822     C                   EVAL      VABTSP=%trim(%subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066700030822     C                   ENDIF
066800030822     C* VAS
066900030822     C                   IF        %subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):1:3)
067200030822     C                             = 'VAS'
067300030822     C                   EVAL      VABVAS=%trim(%subst(
067400030822     C                             %subst(vlrppt:posDaDft+1:
067500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067600030822     C                   ENDIF
067700030822     C* VCA
067800030822     C                   IF        %subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):1:3)
068100030822     C                             = 'VCA'
068200030822     C                   EVAL      VABVCA=%trim(%subst(
068300030822     C                             %subst(vlrppt:posDaDft+1:
068400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068500030822     C                   ENDIF
068600070925     C* TIC
068700070925     C                   IF        %subst(
068800070925     C                             %subst(vlrppt:posDaDft+1:
068900070925     C                             posADft-posDaDft-1):1:3)
069000070925     C                             = 'TIC'
069100070925     C                   EVAL      VABTIC=%trim(%subst(
069200070925     C                             %subst(vlrppt:posDaDft+1:
069300070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069400070925     C                   ENDIF
069500030822     C* GCA
069600030822     C                   IF        %subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):1:3)
069900030822     C                             = 'GCA'
070000030822     C                   EVAL      VABGCA=%trim(%subst(
070100030822     C                             %subst(vlrppt:posDaDft+1:
070200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070300030822     C                   ENDIF
070400030822     C* CTM
070500030822     C                   IF        %subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):1:3)
070800030822     C                             = 'CTM'
070900030822     C                   EVAL      VABCTM=%trim(%subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071200030822     C                   ENDIF
071300030822     C* FFD
071400030822     C                   IF        %subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):1:3)
071700030822     C                             = 'FFD'
071800030822     C                   EVAL      VABFFD=%trim(%subst(
071900030822     C                             %subst(vlrppt:posDaDft+1:
072000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072100030822     C                   ENDIF
072200030822     C* VAD
072300030822     C                   IF        %subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):1:3)
072600030822     C                             = 'VAD'
072700030822     C                   EVAL      VABVAD=%trim(%subst(
072800030822     C                             %subst(vlrppt:posDaDft+1:
072900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073000030822     C                   ENDIF
073100030822     C* GMA
073200030822     C                   IF        %subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):1:3)
073500030822     C                             = 'GMA'
073600030822     C                   EVAL      VABGMA=%trim(%subst(
073700030822     C                             %subst(vlrppt:posDaDft+1:
073800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073900030822     C                   ENDIF
074000030822     C* GGA
074100030822     C                   IF        %subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):1:3)
074400030822     C                             = 'GGA'
074500030822     C                   EVAL      VABGGA=%trim(%subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074800030822     C                   ENDIF
074900030822     C* GVA
075000030822     C                   IF        %subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):1:3)
075300030822     C                             = 'GVA'
075400030822     C                   EVAL      VABGVA=%trim(%subst(
075500030822     C                             %subst(vlrppt:posDaDft+1:
075600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075700030822     C                   ENDIF
075800030822     C* TC1
075900030822     C                   IF        %subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):1:3)
076200030822     C                             = 'TC1'
076300030822     C                   EVAL      VABTC1=%trim(%subst(
076400030822     C                             %subst(vlrppt:posDaDft+1:
076500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076600030822     C                   ENDIF
076700030822     C* TC2
076800030822     C                   IF        %subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):1:3)
077100030822     C                             = 'TC2'
077200030822     C                   EVAL      VABTC2=%trim(%subst(
077300030822     C                             %subst(vlrppt:posDaDft+1:
077400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077500030822     C                   ENDIF
077600040714     C* VATTRC
077700040714     C                   IF        %subst(
077800040714     C                             %subst(vlrppt:posDaDft+1:
077900040714     C                             posADft-posDaDft-1):1:3)
078000040714     C                             = 'TRC'
078100040714     C                   EVAL      VATTRC=%trim(%subst(
078200040714     C                             %subst(vlrppt:posDaDft+1:
078300040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078400040714     C                   ENDIF
078500030822     C* ...
078600030822     C                   ENDIF
078700030822     C                   ENDDO
078800020204     C*
078900010330     C                   ENDSR
079000010607     C*----------------------------------------------------*
079100080623     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
079200010607     C*----------------------------------------------------*
079300010607     C     IMPVAB        BEGSR
079400010607     C*
079500010607     C                   SETOFF                                       3132
079600030515     C*
079700010607     C                   MOVEL     datcor        VABAAS
079800010607     C                   MOVE      datcor        VABMGS
079900010607     C*
080000030715     C**********
080100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080200030715     C**********
080300030715     C                   IF        wGiro = *zeros
080400040113     C*
080500040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
080600040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
080700080429     C                             %trim(intcol(3))+%trim(intcol(4))+
080800080429     C                             %trim(intcol(5))<>*blanks
080900040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
081000080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
081100080429     C                                      %trim(intcol(5))
081200070823     C     ';':CharCSV   XLATE     vindta        vindta
081300040510     C                   ENDIF
081400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081500031201     C     minu:maiu     XLATE     vindta        vindta
081600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081700030715     C                   DOW       posDa <= %len(%trim(vindta))
081800030715     C*
081900030715     C* Gestisco il 1° campo
082000030715     C                   IF        i = 1
082100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082200030715     C                   EVAL      posDa = 1
082300030715     C* Eseguo lo scan x trovare la fine del primo campo
082400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082500030715     C* A questo punto "estraggo" il campo corrente
082600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082700030715     C                                                    (posA-posDa))
082800030715     C* X i campi successivi al 1°
082900030715     C                   ELSE
083000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083100030715     C                   EVAL      posDa = posA + 1
083200030715     C* Eseguo lo scan x trovare la fine del campo corrente
083300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083400030715     C* Gestisco l'ultimo campo
083500030715     C                   IF        posA = *zeros
083600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083700030715     C* A questo punto "estraggo" il campo corrente
083800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083900030715     C                                                    (posA-posDa))
084000030715     C                   LEAVE
084100030715     C                   ELSE
084200030715     C* A questo punto "estraggo" il campo corrente
084300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084400030715     C                                                    (posA-posDa))
084500030715     C                   ENDIF
084600030715     C                   ENDIF
084700030715     C* Incremento il contatore d campo
084800030715     C                   EVAL      i = i +1
084900030715     C                   ENDDO
085000030715     C*
085100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085200030715     C* delimitazione testo
085300030715     C                   EVAL      i = 1
085400030715     C                   DOW       i < %elem(SkSplitFLD)
085500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
085600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
085700030715     C                   EVAL      i = i + 1
085800030715     C                   ENDDO
085900030715     C                   EVAL      wGiro = 1
086000030715     C                   ELSE
086100030715     C                   EVAL      wGiro = 2
086200031204     C**********
086300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
086400031204     C* d separatore campo
086500031204     C**********
086600031204     C                   EVAL      vindta = %trim(vindta)
086700031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
086800031204     C                   EVAL      vindta = %subst(vindta:2)
086900031204     C                   ENDDO
087000031204     C*
087100031204     C                   Z-ADD     *zeros        lunghInput        4 0
087200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087300031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
087400031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
087500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087600031204     C                   ENDDO
087700030715     C**********
087800030715     C* Effettuo lo split del campo dati d input
087900030715     C**********
088000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088100030715     C                   DOW       posDa <= %len(%trim(vindta))
088200030715     C*
088300030715     C* Gestisco il 1° campo
088400030715     C                   IF        i = 1
088500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088600030715     C                   EVAL      posDa = 1
088700030715     C* Eseguo lo scan x trovare la fine del primo campo
088800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088900030715     C* A questo punto "estraggo" il campo corrente
089000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089100030715     C                                                    (posA-posDa))
089200030715     C* X i campi successivi al 1°
089300030715     C                   ELSE
089400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089500030715     C                   EVAL      posDa = posA + 1
089600031204     C* Verifico che nn vi sia il campo nn valorizzato
089700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
089800031204     C* Se campo nn valorizzato skippo al prossimo
089900031204     C                   EVAL      SkSplitCSV(i) = *blanks
090000031204     C                   EVAL      posA  = posA + 1
090100031204     C                   ELSE
090200030715     C* Eseguo lo scan x trovare la fine del campo corrente
090300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090400030715     C* Gestisco l'ultimo campo
090500030715     C                   IF        posA = *zeros
090600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090700030715     C* A questo punto "estraggo" il campo corrente
090800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090900030715     C                                                    (posA-posDa))
091000030715     C                   LEAVE
091100030715     C                   ELSE
091200030715     C* A questo punto "estraggo" il campo corrente
091300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091400030715     C                                                    (posA-posDa))
091500030715     C                   ENDIF
091600031204     C                   ENDIF
091700030715     C                   ENDIF
091800030715     C* Incremento il contatore d campo
091900030715     C                   EVAL      i = i +1
092000030715     C                   ENDDO
092100030715     C*
092200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092300030715     C* delimitazione testo
092400030715     C                   EVAL      i = 1
092500030715     C                   DOW       i < %elem(SkSplitCSV)
092600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
092800030715     C                   EVAL      i = i + 1
092900030715     C                   ENDDO
093000030715     C*
093100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093200030715     C                   EVAL      i = 1
093300030715     C                   DOW       i < %elem(SkSplitFLD)
093400030715     C*
093500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
093600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
093700030715     C                             SkSplitCSV(i) <> *blanks
093800030721     C*** ATB
093900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
094000030721     C                   EVAL      VABATB = SkSplitCSV(i)
094100030721     C                   ENDIF
094200030715     C*** RSD
094300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
094400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
094500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
094600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
094700030715     C* ==
094800030715     C                   ENDIF
094900030721     C*** RD2
095000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
095100030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
095200030721     C                   ENDIF
095300030715     C*** IND
095400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
095500030715     C                   EVAL      VABIND = SkSplitCSV(i)
095600030715     C                   ENDIF
095700030715     C*** LOD
095800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
095900030715     C                   EVAL      VABLOD = SkSplitCSV(i)
096000030715     C                   ENDIF
096100030715     C*** PRD
096200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
096300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
096400030715     C                   ENDIF
096500030721     C*** NZD
096600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
096700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
096800030721     C                   ENDIF
096900030715     C*** RMA
097000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
097100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
097200030715     C                   ENDIF
097300030715     C*** NOT
097400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
097500030715     C                   EVAL      VABNOT = SkSplitCSV(i)
097600030715     C                   ENDIF
097700030715     C*** NT2
097800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
097900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
098000030715     C                   ENDIF
098100030715     C*** VCA
098200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
098300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
098400030715     C                   ENDIF
098500030715     C*** VAS
098600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
098700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
098800030715     C                   ENDIF
098900030715     C*** FFD
099000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
099100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
099200030715     C                   ENDIF
099300030721     C*** GC1
099400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
099500030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
099600030721     C                   ENDIF
099700030721     C*** GC2
099800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
099900030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
100000030721     C                   ENDIF
100100030721     C*** TSP
100200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
100300030721     C                   EVAL      VABTSP = SkSplitCSV(i)
100400030721     C                   ENDIF
100500040510     C*** CBO
100600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
100700040510     C                   EVAL      VABCBO = SkSplitCSV(i)
100800040510     C                   ENDIF
100900030721     C*** NAS
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
101100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
101200030721     C                   ENDIF
101300030721     C*** TIC
101400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
101500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
101600030721     C                   ENDIF
101700030721     C*** GCA
101800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
101900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
102000030721     C                   ENDIF
102100030721     C*** XCO
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
102300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
102400030721     C                   ENDIF
102500030721     C*** CTM
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
102700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
102800030721     C                   ENDIF
102900030721     C*** TCR
103000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
103100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
103200030721     C                   ENDIF
103300030721     C*** CTS
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
103500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
103600030721     C                   ENDIF
103700030721     C*** FTM
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
103900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
104000030721     C                   ENDIF
104100030721     C*** VAD
104200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
104300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
104400030721     C                   ENDIF
104500030721     C*** GMA
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
104700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
104800030721     C                   ENDIF
104900030721     C*** GGA
105000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
105100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
105200030721     C                   ENDIF
105300030721     C*** GVA
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
105500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
105600030721     C                   ENDIF
105700030721     C*** TC1
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
105900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
106000030721     C                   ENDIF
106100030721     C*** TC2
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
106300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
106400030721     C                   ENDIF
106500030721     C*** SCL
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
106700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
106800030721     C                   ENDIF
106900030721     C*** RMO
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
107100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
107200030721     C                   ENDIF
107300030721     C*** NMO
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
107500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
107600030721     C                   ENDIF
107700030721     C*
107800030721     C* Reperisco quindi i campi numerici...
107900040510     C*** NSP
108000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
108100040510     C                   EVAL      PiStr=SkSplitCSV(i)
108200040510     C                   EXSR      CHKNUM
108300040510     C                   IF        PiInt=*on
108400040510     C                   Z-ADD     PiVal         VABNSP
108500040510     C                   ELSE
108600040510     C                   SETON                                        31
108700040510     C                   EVAL      VABNSP = *zeros
108800040510     C                   EVAL      vinmsg = %trimr(vinmsg)
108900040510     C                             + ' ' + 'VABNSP'
109000040510     C                   ENDIF
109100040510     C                   ENDIF
109200071106     C*** RMN
109300071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
109400071106     C                   EVAL      PiStr=SkSplitCSV(i)
109500071106     C                   EXSR      CHKNUM
109600071106     C                   IF        PiInt=*on
109700071106     C                   Z-ADD     PiVal         VABRMN
109800071106     C                   ELSE
109900071106     C                   SETON                                        32
110000071106     C                   EVAL      VABRMN = *zeros
110100071106     C                   EVAL      vinmsg = %trimr(vinmsg)
110200071106     C                             + ' ' + 'VABRMN'
110300071106     C                   ENDIF
110400071106     C                   ENDIF
110500030721     C*** CAD
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
110700030721     C                   EVAL      PiStr=SkSplitCSV(i)
110800030721     C                   EXSR      CHKNUM
110900030721     C                   IF        PiInt=*on
111000030721     C                   Z-ADD     PiVal         Num5_0
111100030721     C                   MOVEL(p)  Num5_0        VABCAD
111200030721     C                   ELSE
111300030721     C                   SETON                                        32
111400030721     C                   EVAL      VABCAD = *zeros
111500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111600030721     C                             + ' ' + 'VABCAD'
111700030721     C                   ENDIF
111800030721     C                   ENDIF
111900030721     C*** NCL
112000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
112100030721     C                   EVAL      PiStr=SkSplitCSV(i)
112200030721     C                   EXSR      CHKNUM
112300030721     C                   IF        PiInt=*on
112400030721     C                   Z-ADD     PiVal         VABNCL
112500030721     C                   ELSE
112600030721     C                   SETON                                        32
112700030721     C                   EVAL      VABNCL = *zeros
112800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112900040122     C                             + ' ' + 'VABNCL'
113000030721     C                   ENDIF
113100030721     C                   ENDIF
113200030721     C*** PKB
113300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
113400030721     C                   EVAL      PiStr=SkSplitCSV(i)
113500030721     C                   EXSR      CHKNUM
113600030721     C                   IF        PiNum=*on
113700030721     C                   Z-ADD     PiVal         VABPKB
113800030721     C                   ELSE
113900030721     C                   SETON                                        32
114000030721     C                   EVAL      VABPKB = *zeros
114100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114200030721     C                             + ' ' + 'VABPKB'
114300030721     C                   ENDIF
114400030721     C                   ENDIF
114500030721     C*** CAS
114600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
114700030721     C                   EVAL      PiStr=SkSplitCSV(i)
114800030721     C                   EXSR      CHKNUM
114900030721     C                   IF        PiNum=*on
115000030721     C                   Z-ADD     PiVal         VABCAS
115100030721     C                   ELSE
115200030721     C                   SETON                                        32
115300030721     C                   EVAL      VABCAS = *zeros
115400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115500030721     C                             + ' ' + 'VABCAS'
115600030721     C                   ENDIF
115700030721     C                   ENDIF
115800030721     C*** CCM
115900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
116000030721     C                   EVAL      PiStr=SkSplitCSV(i)
116100030721     C                   EXSR      CHKNUM
116200030721     C                   IF        PiInt=*on      AND
116300030721     C                             PiVal<=9999999 AND
116400030721     C                             PiVal>*zeros
116500030721     C                   Z-ADD     PiVal         VABCCM
116600030721     C                   ELSE
116700030721     C                   SETON                                        32
116800030721     C                   EVAL      VABCCM = *zeros
116900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117000040510     C                             + ' ' + 'VABCCM'
117100030721     C                   ENDIF
117200030721     C                   ENDIF
117300030721     C*** LNP
117400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
117500030721     C                   EVAL      PiStr=SkSplitCSV(i)
117600030721     C                   EXSR      CHKNUM
117700030721     C                   IF        PiInt=*on    AND
117800030721     C                             PiVal<=999   AND
117900030721     C                             PiVal>*zeros
118000030721     C                   Z-ADD     PiVal         VABLNP
118100030721     C                   ELSE
118200040715     C                   SETON                                        32
118300030721     C                   EVAL      VABLNP = *zeros
118400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118500040510     C                             + ' ' + 'VABLNP'
118600030721     C                   ENDIF
118700030721     C                   ENDIF
118800030721     C*** NRS
118900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
119000030721     C                   EVAL      PiStr=SkSplitCSV(i)
119100030721     C                   EXSR      CHKNUM
119200030721     C                   IF        PiInt=*on AND
119300030721     C                             PiVal<=99
119400030721     C                   Z-ADD     PiVal         VABNRS
119500030721     C                   ELSE
119600030721     C                   SETON                                        32
119700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119800030721     C                             + ' ' + 'VABNRS'
119900030721     C                   ENDIF
120000030721     C                   ENDIF
120100030721     C*** LNA
120200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
120300030721     C                   EVAL      PiStr=SkSplitCSV(i)
120400030721     C                   EXSR      CHKNUM
120500030721     C                   IF        PiInt=*on    AND
120600030721     C                             PiVal<=999   AND
120700030721     C                             PiVal>*zeros
120800030721     C                   Z-ADD     PiVal         VABLNA
120900030721     C                   ELSE
121000030721     C                   SETON                                        32
121100030721     C                   EVAL      VABLNA = *zeros
121200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121300030721     C                             + ' ' + 'VABLNA'
121400030721     C                   ENDIF
121500030721     C                   ENDIF
121600030721     C*** CTR
121700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
121800030721     C                   EVAL      PiStr=SkSplitCSV(i)
121900030721     C                   EXSR      CHKNUM
122000030721     C                   IF        PiInt=*on  AND
122100030721     C                             PiVal<=999
122200030721     C                   Z-ADD     PiVal         VABCTR
122300030721     C                   ELSE
122400030721     C                   SETON                                        32
122500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122600030721     C                             + ' ' + 'VABCTR'
122700030721     C                   ENDIF
122800030721     C                   ENDIF
122900030721     C*** IAS
123000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
123100030721     C                   EVAL      PiStr=SkSplitCSV(i)
123200030721     C                   EXSR      CHKNUM
123300030721     C                   IF        PiNum=*on
123400030721     C                   Z-ADD     PiVal         VABIAS
123500030721     C                   ELSE
123600030721     C                   SETON                                        32
123700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123800030721     C                             + ' ' + 'VABIAS'
123900030721     C                   ENDIF
124000030721     C                   ENDIF
124100030721     C*** VLB
124200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
124300030721     C                   EVAL      PiStr=SkSplitCSV(i)
124400030721     C                   EXSR      CHKNUM
124500030721     C                   IF        PiNum=*on
124600030721     C                   Z-ADD     PiVal         VABVLB
124700030721     C                   ELSE
124800030721     C                   SETON                                        32
124900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125000030721     C                             + ' ' + 'VABVLB'
125100030721     C                   ENDIF
125200030721     C                   ENDIF
125300030721     C*** QFT
125400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
125500030721     C                   EVAL      PiStr=SkSplitCSV(i)
125600030721     C                   EXSR      CHKNUM
125700030721     C                   IF        PiNum=*on
125800030721     C                   Z-ADD     PiVal         VABQFT
125900030721     C                   ELSE
126000030721     C                   SETON                                        32
126100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126200030721     C                             + ' ' + 'VABQFT'
126300030721     C                   ENDIF
126400030721     C                   ENDIF
126500030721     C*** NCD
126600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
126700030721     C                   EVAL      PiStr=SkSplitCSV(i)
126800030721     C                   EXSR      CHKNUM
126900030721     C                   IF        PiInt=*on      AND
127000030721     C                             PiVal<=9999999
127100030721     C                   Z-ADD     PiVal         VABNCD
127200030721     C                   ELSE
127300030721     C                   SETON                                        32
127400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127500030721     C                             + ' ' + 'VABNCD'
127600030721     C                   ENDIF
127700030721     C                   ENDIF
127800030721     C*** NCA
127900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
128000030721     C                   EVAL      PiStr=SkSplitCSV(i)
128100030721     C                   EXSR      CHKNUM
128200030721     C                   IF        PiInt=*on      AND
128300030721     C                             PiVal<=9999999
128400030721     C                   Z-ADD     PiVal         VABNCA
128500030721     C                   ELSE
128600030721     C                   SETON                                        32
128700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128800030721     C                             + ' ' + 'VABNCA'
128900030721     C                   ENDIF
129000030721     C                   ENDIF
129100030721     C*** ZNC
129200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
129300030721     C                   EVAL      PiStr=SkSplitCSV(i)
129400030721     C                   EXSR      CHKNUM
129500030721     C                   IF        PiInt=*on AND
129600030721     C                             PiVal<=99
129700030721     C                   Z-ADD     PiVal         VABZNC
129800030721     C                   ELSE
129900030721     C                   SETON                                        32
130000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130100030721     C                             + ' ' + 'VABZNC'
130200030721     C                   ENDIF
130300030721     C                   ENDIF
130400030721     C*** DCR
130500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
130600030721     C                   EVAL      PiStr=SkSplitCSV(i)
130700030721     C                   EXSR      CHKNUM
130800030721     C                   IF        PiInt=*on       AND
130900030721     C                             PiVal<=99999999
131000030721     C                   Z-ADD     PiVal         VABDCR
131100030721     C                   ELSE
131200030721     C                   SETON                                        32
131300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131400030721     C                             + ' ' + 'VABDCR'
131500030721     C                   ENDIF
131600030721     C                   ENDIF
131700030721     C*** HCR
131800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
131900030721     C                   EVAL      PiStr=SkSplitCSV(i)
132000030721     C                   EXSR      CHKNUM
132100030721     C                   IF        PiInt=*on   AND
132200030721     C                             PiVal<=9999
132300030721     C                   Z-ADD     PiVal         VABHCR
132400030721     C                   ELSE
132500030721     C                   SETON                                        32
132600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132700030721     C                             + ' ' + 'VABHCR'
132800030721     C                   ENDIF
132900030721     C                   ENDIF
133000030721     C*** VMD
133100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
133200030721     C                   EVAL      PiStr=SkSplitCSV(i)
133300030721     C                   EXSR      CHKNUM
133400030721     C                   IF        PiNum=*on
133500030721     C                   Z-ADD     PiVal         VABVMD
133600030721     C                   ELSE
133700030721     C                   SETON                                        32
133800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133900030721     C                             + ' ' + 'VABVMD'
134000030721     C                   ENDIF
134100030721     C                   ENDIF
134200030721     C*** ANT
134300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
134400030721     C                   EVAL      PiStr=SkSplitCSV(i)
134500030721     C                   EXSR      CHKNUM
134600030721     C                   IF        PiInt=*on        AND
134700030721     C                             PiVal<=999999999
134800030721     C                   Z-ADD     PiVal         VABANT
134900030721     C                   ELSE
135000030721     C                   SETON                                        32
135100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135200030721     C                             + ' ' + 'VABANT'
135300030721     C                   ENDIF
135400030721     C                   ENDIF
135500030721     C*** CMO
135600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
135700030721     C                   EVAL      PiStr=SkSplitCSV(i)
135800030721     C                   EXSR      CHKNUM
135900030721     C                   IF        PiInt=*on
136000030721     C                   Z-ADD     PiVal         Num5_0
136100030721     C                   MOVEL(p)  Num5_0        VABCMO
136200030721     C                   ELSE
136300030721     C                   SETON                                        32
136400030721     C                   EVAL      VABCMO = *zeros
136500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136600030721     C                             + ' ' + 'VABCMO'
136700030721     C                   ENDIF
136800030721     C                   ENDIF
136900040510     C*
137000080623     C* Reperisco i campi del EDIVAT
137100040510     C*** VATNOT
137200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
137300040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
137400040510     C                   ENDIF
137500040510     C*** VATTRC
137600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
137700040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
137800040510     C                   ENDIF
137900060331     C*** VATNOT_A
138000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
138100060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
138200060331     C                   ENDIF
138300060331     C*** VATNOT_B
138400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
138500060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
138600060331     C                   ENDIF
138700060331     C*** VATNOT_E
138800070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
138900060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
139000060331     C                   ENDIF
139100070823     C*
139200071029     C***  ===>  Gestione campi "particolari" dentro ciclo
139300080617     C*** VABRSD
139400080617     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
139500080617     C                   EVAL      VABRSD = %subst(VABRSD:8)
139600080617     C                   ENDIF
139700080513     C*** VATNOT_B_1
139800080513     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B_1'
139900080513     C                   EVAL      wVATNOT_B1 = %trim(SkSplitCSV(i))
140000080513     C                   ENDIF
140100080513     C*** VATNOT_B_2
140200080513     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B_2'
140300080513     C                   EVAL      wVATNOT_B2 = %trim(SkSplitCSV(i))
140400080513     C                   ENDIF
140500080513     C*** NSP/RMN
140600080513     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
140700080513     C                   EVAL      VABRMA=SkSplitCSV(i)
140800080513     C                   EVAL      PiStr=SkSplitCSV(i)
140900080513     C                   EXSR      CHKNUM
141000080513     C                   IF        PiInt=*on
141100080513     C                   Z-ADD     PiVal         VABNSP
141200080513     C                   Z-ADD     PiVal         VATNSP
141300080513     C                   Z-ADD     PiVal         VABRMN
141400080513     C                   ELSE
141500080513     C                   SETON                                        31
141600080513     C                   EVAL      VABNSP = *zeros
141700080513     C                   EVAL      VATNSP = *zeros
141800080513     C                   EVAL      VABRMN = 1
141900080513     C                   EVAL      vinmsg = %trimr(vinmsg)
142000080513     C                             + ' ' + 'VABNSP VATNSP VABRMN'
142100080513     C                   ENDIF
142200080513     C                   ENDIF
142300080514     C*** VABTIC
142400080514     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABTIC'
142500080514     C                   IF        %subst(%trim(SkSplitCSV(i)):1:3)='100'
142600080514     C                   EVAL      VABTIC = *blanks
142700080514     C                   ENDIF
142800080514     C                   IF        %subst(%trim(SkSplitCSV(i)):1:3)='101'
142900080514     C                   EVAL      VABTIC = 'BM'
143000080514     C                   ENDIF
143100080514     C                   ENDIF
143200070925     C*
143300071029     C***  <===  -----------------------------------------
143400040510     C*
143500030715     C                   ENDIF
143600030715     C                   EVAL      i = i + 1
143700030715     C                   ENDDO
143800070925     C*
143900071029     C***  ===>  Gestione campi "particolari" fuori ciclo
144000080513     C*
144100080513     C* VATNOT_B
144200080513     C                   EVAL      wVATNOT_B = %trim(wVATNOT_B1) +
144300080513     C                                         %trim(wVATNOT_B2)
144400080514     C*
144500080514     C* VABNZD
144600080514     C                   IF        VABNZD = 'I'
144700080514     C                   EVAL      VABNZD = *blanks
144800080514     C                   ENDIF
144900070925     C*
145000080513     C***  <===  -----------------------------------------
145100040510     C*
145200040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
145300040510     C                   IF        VABNSP = *zeros
145400040510     C                   SETON                                        31
145500040510     C                   ENDIF
145600040715     C*
145700040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
145800040715     C                   IF        VABLNP = *zeros
145900040715     C                   SETON                                        31
146000040715     C                   ENDIF
146100040510     C*
146200040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
146300040510     C                   IF        VABRMN = *zeros
146400040510     C                   Z-ADD     VABNSP        VABRMN
146500040510     C                   ENDIF
146600040510     C*
146700040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
146800040510     C                   IF        VABRMA = *blanks
146900040714     C                   MOVEL     VABRMN        VABRMA
147000040510     C                   ENDIF
147100040128     C*
147200040510     C* Se provincia nn valorizzata la reperisco
147300040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
147400040510     C                   IF        VABPRD = *blanks
147500040128     C                   CLEAR                   TISI95DS
147600040128     C                   EVAL      I95TCN = '3'
147700040128     C                   Z-ADD     datcor        I95DAT
147800040128     C                   EVAL      I95NAR = VABNZD
147900040128     C                   EVAL      I95CAP = VABCAD
148000040128     C                   EVAL      I95LOC = VABLOD
148100040128     C                   CALL      'TISI95R'
148200040128     C                   PARM                    TISI95DS
148300040128     C                   EVAL      VABPRD = O95PRV
148400040510     C                   ENDIF
148500040113     C*
148600040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
148700040113     C                   IF        VABCAS > *zeros
148800040119     C                   EVAL      VABVCA = 'EUR'
148900040119     C                   IF        VABCBO = '1'
149000040113     C                   EVAL      VABCBO = '4'
149100040119     C                   ENDIF
149200040119     C                   IF        VABCBO = '2'
149300040119     C                   EVAL      VABCBO = '6'
149400040119     C                   ENDIF
149500040113     C                   ENDIF
149600020204     C*
149700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
149800080623     C                   EXSR      CHKIMPDIV
149808080623     C*
149900030715     C                   ENDIF
150000020204     C*
150100010607     C                   ENDSR
150200010607     C*----------------------------------------------------*
150300010601
150400020204
150500020204     C*----------------------------------------------------*
150600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
150700020204     C*----------------------------------------------------*
150800020204     C     CHKIMPDIV     BEGSR
150900020204     C*
151000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
151100020204     C                   Z-ADD     *zeros        wrkDec            9 9
151200020204     C*
151300020204     C* Come prima cosa effettuo considerazioni sulla divisa
151400020204     C                   IF        vabIAS > *zeros
151500020204     C                   IF        vabVAS <> 'EUR'
151600020204     C                   EVAL      vabVAS =  'ITL'
151700020204     C                   ENDIF
151800020204     C                   ENDIF
151900020204     C*
152000020204     C                   IF        vabCAS > *zeros
152100020204     C                   IF        vabVCA <> 'EUR'
152200020204     C                   EVAL      vabVCA =  'ITL'
152300020204     C                   ENDIF
152400020204     C                   ENDIF
152500020204     C*
152600020204     C                   IF        vabVMD > *zeros
152700020204     C                   IF        vabVAD <> 'EUR'
152800020204     C                   EVAL      vabVAD =  'ITL'
152900020204     C                   ENDIF
153000020204     C                   ENDIF
153100020204     C*
153200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
153300020204     C                   Z-ADD     vabIAS        wrkDec
153400020204     C                   IF        wrkDec > *zeros
153500020204     C                   IF        vabVAS = 'ITL'
153600020204     C                   EVAL      vabIAS = *zeros
153700020204     C                   ENDIF
153800020204     C                   ENDIF
153900020204     C*
154000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
154100020204     C                   Z-ADD     vabCAS        wrkDec
154200020204     C                   IF        wrkDec > *zeros
154300020204     C                   IF        vabVCA = 'ITL'
154400020204     C                   EVAL      vabCAS = *zeros
154500020204     C                   ENDIF
154600020204     C                   ENDIF
154700020204     C*
154800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
154900020204     C                   Z-ADD     vabVMD        wrkDec
155000020204     C                   IF        wrkDec > *zeros
155100020204     C                   IF        vabVAD = 'ITL'
155200020204     C                   EVAL      vabVMD = *zeros
155300020204     C                   ENDIF
155400020204     C                   ENDIF
155500020204     C*
155600020204     C                   ENDSR
155700020204     C***
155800020204
155900010330
156000010330
156100010330     C*----------------------------------------------------*
156200010330     C*  CONTROLLO NUMERICITA' CAMPI
156300010330     C*----------------------------------------------------*
156400010330     C     CHKNUM        BEGSR
156500010330     C*
156600010606     C                   IF        PiDecChr = *blanks
156700030715     C                   EVAL      PiDecChr = CharNUM
156800010606     C                   ENDIF
156900010606     C*
157000010606     C                   CALL(e)   'ISNUMERIC'
157100010330     C                   PARM                    PiStr            30
157200010606     C                   PARM                    PiDecChr          1
157300010330     C                   PARM      *ZEROS        PiVal            30 9
157400010330     C                   PARM      '0'           PiInt             1
157500010330     C                   PARM      '0'           PiNum             1
157600010330     C                   IF        %error
157700010606     C                   EVAL      PiNum=*off
157800010330     C                   ENDIF
157900010330     C*
158000010330     C                   ENDSR
158100010330     C***
158200010330
158300010601
158400010601
158500010601
158600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
158700010601     C     repfil        BEGSR
158800010601     C*
158900010601     C                   if        invfil = *zeros and
159000010601     C                             depfil > *zeros and
159100010629     C                             (vinflg = *blanks or
159200010629     C                              vinflg = *zeros)
159300010601     C
159400010601     C                   eval      invfil = depfil
159500010601     C                   endif
159600010601     C*
159700010601     C                   if        depfil <> invfil and
159800010601     C                             invfil > *zeros
159900010601     C                   eval      flgMulti = '1'
160000010601     C                   if        vinflg = *blanks
160100010601     C                   add       1             cntNonEl
160200010601     C                   endif
160300010601     C                   endif
160400010601     C*
160500010601     C                   if        vinflg = '2'
160600010601     C                   eval      flgStato = '2'
160700010601     C                   endif
160800010601     C*
160900010601     C                   ENDSR
161000010601     C***
161100010601
161200010601
161300010601
161400010330
161500040119      /TITLE Invio dei dati al punto operativo.
161600040119     C     invio         BEGSR
161700040119     C*
161800080623     C* 1° invio EDIVAT
161900040119     C                   reset                   dscmz
162000040119     C                   move      vlrpoi        cmzdst
162100080623     C                   eval      cmzfld = 'EDIVATWR'
162200040119     C                   eval      cmzmbd = vlrhdl
162300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
162400040119     C***                if        prmfir = *blanks
162500080623     C                   eval      cmzfla = 'EDIVAT0F'
162600080623     C                   eval      cmzmba = 'EDIVAT0F'
162700040119     C***                else
162800040119     C***                eval      cmzfla = prmfir
162900040119     C***                eval      cmzmba = prmfir
163000040119     C***                endif
163100040119     C                   eval      cmznrr = *zeros
163200040119     C                   move      §ctrokvt      cmznrr
163300040119     C                   eval      cmzlba = vlrfl1
163400040119     C                   call(e)   'TIS711C'
163500040119     C                   parm                    dscmz
163600040119     C                   parm      *blanks       esito
163700040119     C                   if        %error
163800040119     C                             or cmzerr = '1'
163900040119     C                             or esito  = '1'
164000040119     C                   eval      wrkesito = '3'
164100040119     C                   else
164200040119     C*
164300080623     C* 2° invio EDIVAB
164400040119     C                   reset                   dscmz
164500040119     C                   move      vlrpoi        cmzdst
164600040119     C                   eval      cmzfld = vlrfou
164700040119     C                   eval      cmzmbd = vlrhdl
164800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164900040119     C***                if        prmfir = *blanks
165000080623     C                   eval      cmzfla = 'EDIVAB0F'
165100080623     C                   eval      cmzmba = 'EDIVAB0F'
165200040119     C***                else
165300040119     C***                eval      cmzfla = prmfir
165400040119     C***                eval      cmzmba = prmfir
165500040119     C***                endif
165600040119     C                   eval      cmznrr = *zeros
165700040119     C                   move      §ctrokvb      cmznrr
165800040119     C                   eval      cmzlba = vlrfl1
165900040119     C                   call(e)   'TIS711C'
166000040119     C                   parm                    dscmz
166100040119     C                   parm      *blanks       esito
166200040119     C                   if        %error
166300040119     C                             or cmzerr = '1'
166400040119     C                             or esito  = '1'
166500040119     C                   eval      wrkesito = '3'
166600040119     C                   endif
166700040119     C                   endif
166800040119     C*
166900040119     C                   ENDSR
167000040119     C***
167100010601
167200010601
167300010601
167400010601
167500010601      /TITLE Invio dei dati al punto operativo.
167600010601     C     opeini        BEGSR
167700010601     C*
167800010601     C* Inizializzo flag e contatori operativi
167900010601     C                   movel     '0'           flgGiro           1
168000010601     C                   movel     '0'           flgMulti          1
168100010601     C                   movel     '1'           flgStato          1
168200010615     C                   movel     '0'           flgOk             1
168300010601     C                   z-add     *zeros        cntNonEl         10 0
168400010601     C                   z-add     *zeros        depfil            3 0
168500010601     C                   z-add     *zeros        invfil            3 0
168600010601     C*
168700010601     C                   ENDSR
168800010601     C***
168900070326
169000070326
169100070326
169200070326
169300070326     C     *pssr         BEGSR
169400070329     C*
169500070329     C                   if        %open(tivin00r)
169600070329     C                   close     tivin00r
169700070329     C                   endif
169800080623     C                   if        %open(edivabwr)
169900080623     C                   close     edivabwr
170000070329     C                   endif
170100080623     C                   if        %open(edivatwr)
170200080623     C                   close     edivatwr
170300070329     C                   endif
170400070326     C*
170500070326     C* Effettuo la chiamata al CLLE preposto
170600080623     C                   call(e)   'TITVEVTC'
170700070326     C                   parm                    parccm
170800070326     C                   parm                    parmbr
170900070326     C                   parm      '2'           paropz
171000070326     C*
171100070326     C                   eval      wrkesito = '2'
171200070404     C*
171300070404     C                   seton                                        LR
171400070326     C*
171500070326     C                   ENDSR     '*CANCL'
171600070326     C***
171700080418
171800080418
171900080418
172000080418      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
172100080418     C     exeBon        BEGSR
172200080418     C*
172300080418     C* Imposto i parametri x il *pgm d "bonifica"
172400080418     C                   EVAL      ElencoChar = '1234567890'
172500080418     C                   EVAL      TipoElenco = *blanks
172600080418     C                   EVAL      CharSost   = *blanks
172700080418     C                   EVAL      UpperCase  = *blanks
172800080418     C                   EVAL      ChkNull    = '1'
172900080418     C                   EVAL      CharNull   = *blanks
173000080418     C*
173100080418     C* Effettuo la chiamata al *pgm d "bonifica" x il campo VABRMA
173200080418     C                   EVAL      TxtInOut   = VABRMA
173300080418     C                   EVAL      EsitoBon   = *blanks
173400080418     C*
173500080418     C                   CALL      'XCHKCHAR'
173600080418     C                   PARM                    TxtInOut
173700080418     C                   PARM                    ElencoChar
173800080418     C                   PARM                    TipoElenco
173900080418     C                   PARM                    CharSost
174000080418     C                   PARM                    UpperCase
174100080418     C                   PARM                    ChkNull
174200080418     C                   PARM                    CharNull
174300080418     C                   PARM                    EsitoBon
174400080418     C*
174500080418     C                   ENDSR
174600070326
174700070326
174800010330
174900010330
175000000613     C     *inzsr        BEGSR
175100990910     C*
175200990910     C     *entry        plist
175300990920     C                   parm                    tivlrds
175400990921     C                   parm      wrkesito      esito
175500000724     C                   parm                    prmlit
175600000710     C                   parm                    prmfir
175700010330     C*
175800010330     C* CALCOLA LA DATA CORRENTE
175900010330     C                   time                    wn14             14 0
176000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
176001080623     C                   movel     wn14          oracor            6 0          *ORA  (8) IN HH/MM/S
176100010330     C                   z-add     wn8           g08dat
176200010330     C                   z-add     *zeros        g08inv
176300010330     C                   movel     '0'           g08err
176400010330     C                   call      'XSRDA8'
176500010330     C                   parm                    wlbda8
176600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
176601080623     C                   movel     datcor        datoracor        14 0          *DATA/ORA CORRENTE
176602080623     C                   move      oracor        datoracor
176700000613     C*
176800000613     C                   ENDSR
176900000613     C***
177000990908
177100080513     Otitv1g3p  E            riepilogo         2
177200990915     O                                              'Upload via Internet'
177300990915     O                                           +1 'Traduzione TIVIN00R -'
177400080623     O                                           +1 'EDOVABWR'
177500990915     O                                           +1 'Testate bolle'
177600990915     O          E            riepilogo   2
177700990915     O                       wrkdata
177800990915     O                       wrkora              +1
177900990915     O                       procname            +1
178000990915     O          E            riepilogo   2
178100990915     O                                              'Cliente..................:'
178200010605     O                       VABCCM        z     +1
178300990915     O          E            riepilogo   2
178400990920     O                                              'Riferimento Strategi.....:'
178500990920     O                       vlrhdl              +1
178600990915     O          E            riepilogo   2
178700990915     O                                              'Giusti...................:'
178800040119     O                       §CTROKVB      2   +  1
178900990915     O          E            riepilogo   2
179000990915     O                                              'Sbagliati e corretti.....:'
179100971022     O                       §CTRMO        2   +  1
179200990915     O          E            riepilogo   2
179300990915     O                                              'Sbagliati e scartati.....:'
179400971022     O                       §CTRNO        2   +  1
179500000613
179600080513     Otitv1g3ps E            testdett          2
179700000613     O                                              'Upload via Internet'
179800000613     O                                           +1 'Traduzione TIVIN00R -'
179900080623     O                                           +1 'EDIVABWR'
180000000613     O                                           +1 'Testate bolle'
180100000616     O          E            testdett    3
180200000613     O                                           +2 'N° rec'
180300000613     O                                           +3 'Anteprima contenuto'
180400000616     O          E            rigadett    2
180500000613     O                       rrnum               +2
180600000621     O                       recko               +3
180700000616     O          E            findett     2
180800000613     O                       wrkdata
180900000613     O                       wrkora              +1
181000000613     O                       procname            +1
181100000616     O          E            findett     2
181200000613     O                                              'Cliente..................:'
181300010605     O                       VABCCM        z     +1
181400000616     O          E            findett     2
181500000613     O                                              'Riferimento Strategi.....:'
181600000613     O                       vlrhdl              +1
181700000616     O          E            findett     2
181800000613     O                                              'Giusti...................:'
181900040119     O                       §CTROKVB      2   +  1
182000000616     O          E            findett     2
182100000613     O                                              'Sbagliati e corretti.....:'
182200000613     O                       §CTRMO        2   +  1
182300000616     O          E            findett     2
182400000613     O                                              'Sbagliati e scartati.....:'
182500000613     O                       §CTRNO        2   +  1
182600000613** CMD - COMANDI CL
182700080513OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1G3P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
182800080513OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1G3PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
182900080513DLTOVR FILE(TITV1G3P TITV1G3PS) LVL(*)
183000010606
183100000613
183200040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
183300080513FIL01;VABNSP;FIL02;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABNZD;FIL03;
183400080514VABCAS;VABPKB;VABVLB;VATNOT_E;VABNCL;FIL04;VABNOT;VABNT2;
183500080514VATNOT_B_1;VATNOT_B_2;W_VABTIC;
183600080513
183700080429
