000100091021      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200081217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400000724     Fazorg01l  if   e           k disk
000500091015     FTITV1S41L uF   E           k DISK    prefix(W_)
000600091021     FEDIVABwr  O    E             DISK    usropn
000700091021     FEDIVATwr  O    E             DISK    usropn
000800091022     Ftitv1s5p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100091022     Ftitv1s5ps O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400990908
001500000512     D*------------
001600000512     D* COMANDI
001700000512     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900000801     D*----------------------------------------------------
002000000801     D* DICHIARAZIOINE VARIABILI DI WRK
002100000801     D*----------------------------------------------------
002200990920     D dscmz         e ds                  inz
002300990910     D psds           sds
002400990910     D  procname         *PROC
002500990920     D tivlrds       e ds                  extname(tivlr00f)
002600990910     D esito           s              1
002700000724     D prmlit          s             10
002800000710     D prmfir          s             10
002900990921     D wrkesito        s                   like(esito)
003000990915     D wrkdata         s               d
003100990915     D wrkora          s               t
003200000621     D recko           s            150    INZ(*blanks)
003300011113     D depcmd          s            150    INZ(*blanks)
003400010202     D parccm          s              8    INZ(*blanks)
003500010202     D parmbr          s             10    INZ(*blanks)
003600010202     D paropz          s              1    INZ(*blanks)
003700010202     D chkcall         s              1    INZ(*blanks)
003800091015     D titv1s4ds_wk  e ds                  extname(titv1s40f)
003900091015     D                                     prefix(W_)
004000091015     D titv1s4ds_ok  e ds                  extname(titv1s40f)
004100091015     D curSped         ds                  qualified
004200091015     D   TV1S4KEY                          like(W_TV1S4KEY)
004300091015     D   vabCCM                            like(W_vabCCM)
004400091015     D   vabAAS                            like(W_vabAAS)
004500091015     D   vabLNP                            like(W_vabLNP)
004600091015     D   vabNRS                            like(W_vabNRS)
004700091015     D   vabNSP                            like(W_vabNSP)
004800091013     D depSped         ds                  likeds(curSped)
004900010201
005000081217
005100081217     C
005200990915     C                   time                    wrkdata
005300990915     C                   time                    wrkora
005400990921     C                   reset                   esito
005500990921     C                   reset                   wrkesito
005600000724     C*
005700000724     C* SE OCCORRE SPEDIRE IN FILIALE
005800000724     C                   if        vlrpoi <> *zeros
005900000724     C*
006000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006200000724     C     vlrpoi        chain     azorg01l
006300000724     C                   if        %found
006400000616     C                   movel(p)  CMD(1)        depcmd
006500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
006600000616     C*
006700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
006800011113     C                   Z-ADD     150           LENGH            15 5
006900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007000000616     C                   PARM                    depcmd
007100000616     C                   PARM                    LENGH
007200000724     C*
007300000724     C                   endif
007400000724     C                   endif
007500000616     C*
007600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007700000616     C                   movel(p)  CMD(2)        depcmd
007800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
007900000616     C*
008000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008100011113     C                   Z-ADD     150           LENGH            15 5
008200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008300000616     C                   PARM                    depcmd
008400000616     C                   PARM                    LENGH
008500000616     C*
008600091022     C                   if        not %open(titv1s5ps)
008700091022     C                   open      titv1s5ps
008800000616     C                   except    testdett
008900000613     C                   endif
009000000613     C*
009100091110     C                   EXSR      DEFCAM
009200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009400000613     C*
009500010202     C* Effettuo la chiamata al CLLE preposto
009600091014     C                   if        vlrtip = 'VB'
009700091021     C                   call(e)   'TITVEVTC'
009800010202     C                   parm                    parccm
009900010202     C                   parm                    parmbr
010000010202     C                   parm      '2'           paropz
010100091014     C                   endif
010200091014     C*
010300091014     C                   if        vlrtip = 'VT'
010400091021     C                   call(e)   'TITVEVBC'
010500091014     C                   parm                    parccm
010600091014     C                   parm                    parmbr
010700091014     C                   parm      '2'           paropz
010800091014     C                   endif
010900010202     C*
011000091022     C                   if        %open(titv1s5ps)
011100000616     C                   except    findett
011200091022     C                   close     titv1s5ps
011300000613     C                   endif
011400000616     C*
011500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011600011113     C                   Z-ADD     150           LENGH            15 5
011700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011800000616     C                   PARM                    CMD(3)
011900000616     C                   PARM                    LENGH
012000000616     C*
012100000801     C
012200010201     C                   seton                                        LR
012300091110
012400091110
012500091110     C*----------------------------------------------------*
012600091110     C*  IMPOSTAZIONE CAMPI COSTANTI
012700091110     C*----------------------------------------------------*
012800091110     C     DEFCAM        BEGSR
012900091123     C*
013000091123     C                   SETOFF                                       50
013100091110     C*
013200091110     C* Imposto i valori di default...
013300091110     C* ... e poi verifico se sono stati passati come parametri
013400091110     C                   IF        %subst(vlrppt:21) > *blanks
013500091123     C                   IF        %subst(vlrppt:21:1) = 'X'
013600091123     C                   SETON                                        50
013700091123     C                   ENDIF
013800091110     C                   ENDIF
013900091110     C*
014000091110     C                   ENDSR
014100000613
014200000613
014300000613     C*--------------------------------------------------------
014400000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014500000613     C*--------------------------------------------------------
014600000621     C     STPR          BEGSR
014700000613     C*
014800091022     C                   if        not %open(titv1s5p)
014900091022     C                   open      titv1s5p
015000990915     C                   endif
015100990915     C*
015200990915     C                   except    riepilogo
015300990915     C*
015400091022     C                   if        %open(titv1s5p)
015500091022     C                   close     titv1s5p
015600990914     C                   endif
015700990910     C*
015800000613     C                   ENDSR
015900000613     C***
016000990908
016100000801
016200910830     C*--------------------------------------------------------
016300091021     C* RWFILE   LEGGE TITV1S40F E SCRIVE EDIVABWR e EDIVATWR
016400910830     C*--------------------------------------------------------
016500040526     C     RWFILE        BEGSR
016600070103     C*
016700091021     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
016800091014     C                   exsr      prevabt
016900010201     C*
017000010202     C                   if        chkcall = '0'
017100091014     C*
017200091021     C                   if        not %open(edivabwr)
017300091021     C                   open      edivabwr
017400091014     C                   endif
017500010202     C*
017600091021     C                   if        not %open(edivatwr)
017700091021     C                   open      edivatwr
017800010201     C                   endif
017900091013     C*
018000091013     C                   clear                   §CTROKVB          5 0
018100091013     C                   clear                   §CTROKVT          5 0
018200091013     C                   clear                   §CTRNO            5 0
018300091013     C*
018400091022     C                   EVAL      W_TV1S4KEY  = '02101177_BOLLE'
018500990910     C*
018600091015     C     W_TV1S4KEY    setll     TITV1S41L
018700091013     C                   if        %found(TITV1S41L)
018800091015     C     W_TV1S4KEY    reade     TITV1S41L
018900091013     C                   dow       not %eof(TITV1S41L)
019000000801     C*
019100091123     C                   setoff                                       51
019200091123     C*
019300091123     C                   select
019400091123     C                   when      W_TV1S4VAB = 'X' and
019500091123     C                             W_TV1S4VAT = 'X'
019600091123     C                   seton                                        51
019700091123     C                   when      W_TV1S4VAB = 'X'  and
019800091123     C                             vlrtip     = 'VB' and
019900091123     C                             *IN50      = *on
020000091123     C                   seton                                        51
020100091123     C                   when      W_TV1S4VAT = 'X'  and
020200091123     C                             vlrtip     = 'VT' and
020300091123     C                             *IN50      = *on
020400091123     C                   seton                                        51
020500091123     C                   endsl
020600091123     C*
020700091123     C* ---- Record buoni da inviare in filiale
020800091123     C                   if        *in51 = *on
020900091015     C*
021000091015     C                   eval      curSped.TV1S4KEY = W_TV1S4KEY
021100091015     C                   eval      curSped.vabCCM   = W_vabCCM
021200091015     C                   eval      curSped.vabAAS   = W_vabAAS
021300091015     C                   eval      curSped.vabLNP   = W_vabLNP
021400091015     C                   eval      curSped.vabNRS   = W_vabNRS
021500091015     C                   eval      curSped.vabNSP   = W_vabNSP
021600091015     C*
021700070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
021800091013     C                   if        curSped <> depSped
021900070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
022000091015     C                   if        §CTROKVT > *zeros
022100091013     C                   exsr      wrivab                                       => scarico VAB
022200091015     C                   endif
022300091015     C* Assegno il buffer del formato record corrente
022400091015     C                   eval      titv1s4ds_ok =  titv1s4ds_wk
022500091015     C*
022600091013     C                   exsr      wrivat                                       => scarico VAT
022700070103     C* Salvo il raggruppamento spedizione cliente corrente
022800091013     C                   eval      depSped = curSped
022900070213     C*
023000070213     C* Se collo successivo x stessa bolla
023100070213     C                   else
023200091015     C* Assegno il buffer del formato record corrente
023300091015     C                   eval      titv1s4ds_ok =  titv1s4ds_wk
023400091015     C*
023500091013     C                   exsr      wrivat                                       => scarico VAT
023600070103     C                   endif
023700000905     C*
023800091013     C                   delete    TITV1S41L
023900050628     C                   endif
024000091013     C* ----
024100091013     C*
024200091013     C*
024300091013     C* ---- Record con errori (da gestire in qualche modo)
024400091015     C                   if        W_TV1S4VAB = '*' or
024500091015     C                             W_TV1S4VAT = '*'
024600091013     C                   ADD       1             §CTRNO
024700091013     C                   endif
024800091013     C* ----
024900091013     C*
025000091015     C     W_TV1S4KEY    reade     TITV1S41L
025100091013     C*
025200091013     C                   ENDdo
025300091015     C*
025400091015     C* Scarico le testate in sospeso
025500091123     C                   setoff                                       51
025600091123     C*
025700091123     C                   select
025800091123     C                   when      W_TV1S4VAB = 'X'  and
025900091123     C                             vlrtip     = 'VB' and
026000091123     C                             *IN50      = *on
026100091123     C                   seton                                        51
026200091123     C                   when      W_TV1S4VAT = 'X'  and
026300091123     C                             vlrtip     = 'VT' and
026400091123     C                             *IN50      = *on
026500091123     C                   seton                                        51
026600091123     C                   endsl
026700091123     C*
026800091123     C                   if        *in51 = *on
026900091015     C                   exsr      wrivab                                       => scarico VAB
027000091015     C                   endif
027100010202     C*
027200010202     C                   endif
027300091013     C*
027400091013     C                   endif
027500091013
027600990910
027700990910     C* Se non ci sono record con errori ...
027800000710     C                   if        §ctrno = 0
027900990910     C* ... restituisco esito OK.
028000990921     C                   eval      wrkesito = '0'
028100990910     C                   else
028200010201     C                   if        §ctrokvb > 0
028300990921     C                   eval      wrkesito = '1'
028400000710     C                   else
028500000710     C                   eval      wrkesito = '2'
028600990910     C                   endif
028700000710     C                   endif
028800990910     C*
028900091021     C                   if        %open(edivabwr)
029000091021     C                   close     edivabwr
029100990914     C                   endif
029200091021     C                   if        %open(edivatwr)
029300091021     C                   close     edivatwr
029400010201     C                   endif
029500990910     C*
029600091013     C                   if        (§ctrokvb > 0 or
029700091013     C                              §ctrokvt > 0)
029800000724     C                             and vlrpoi <> *zeros
029900010202     C                   exsr      invio
030000990920     C                   endif
030100990920     C*
030200910830     C                   ENDSR
030300000613     C***
030400010305
030500010305     C*----------------------------------------------------*
030600020305     C*  SCARICAMENTO BUFFER RECORDS VAB
030700010305     C*----------------------------------------------------*
030800020305     C     WRIVAB        BEGSR
030900010305     C*
031000060225     C* Quindi scarico il buffer del file d testata
031100091021     C                   write     edivab00                                     => scarico il VAB
031200091014     C                   ADD       1             §ctrokvb
031300010305     C*
031400010305     C                   ENDSR
031500091013
031600010201     C*----------------------------------------------------*
031700091021     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
031800010201     C*----------------------------------------------------*
031900020305     C     WRIVAT        BEGSR
032000091022     C*
032100060223     C* Scrivo solo se valorizzato qualcosa
032200060223     C                   IF        VATNOT <> *blanks
032300091021     C                   WRITE     EDIVAT00
032400090922     C                   ADD       1             §ctrokvt
032500091123     C                   ELSE
032600091123     C   50              ADD       1             §ctrokvt
032700060223     C                   ENDIF
032800010201     C*
032900010201     C                   ENDSR
033000091013
033100010202     C*----------------------------------------------------*
033200091021     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
033300010202     C*----------------------------------------------------*
033400091014     C     PREVABT       BEGSR
033500010202     C*
033600091021     C* Compongo il nome del membro da dare al EDIVATWR
033700010202     C                   eval      parmbr = vlrhdl
033800010202     C                   movel     'M'           parmbr
033900050627     C                   eval      parccm = %subst(vlrKSC:2:7)
034000010202     C                   eval      paropz = '1'
034100010202     C* Effettuo la chiamata al CLLE preposto
034200091014     C                   if        vlrtip = 'VB'
034300091021     C                   call(e)   'TITVEVTC'
034400010202     C                   parm                    parccm
034500010202     C                   parm                    parmbr
034600010202     C                   parm                    paropz
034700091014     C                   endif
034800091014     C*
034900091014     C                   if        vlrtip = 'VT'
035000091021     C                   call(e)   'TITVEVBC'
035100091014     C                   parm                    parccm
035200091014     C                   parm                    parmbr
035300091014     C                   parm                    paropz
035400091014     C                   endif
035500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
035600010202     C                   if        %error
035700010202     C                   movel     '1'           chkcall
035800010202     C                   else
035900010202     C                   movel     '0'           chkcall
036000010202     C                   endif
036100010202     C*
036200010202     C                   ENDSR
036300011113
036400000801
036500000801
036600990920      /TITLE Invio dei dati al punto operativo.
036700010202     C     invio         BEGSR
036800990920     C*
036900091021     C* 1° invio EDIVAT
037000010201     C                   reset                   dscmz
037100010201     C                   move      vlrpoi        cmzdst
037200091021     C                   eval      cmzfld = 'EDIVATWR'
037300010201     C                   eval      cmzmbd = vlrhdl
037400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
037500021009     C***                if        prmfir = *blanks
037600091021     C                   eval      cmzfla = 'EDIVAT0F'
037700091021     C                   eval      cmzmba = 'EDIVAT0F'
037800021009     C***                else
037900021009     C***                eval      cmzfla = prmfir
038000021009     C***                eval      cmzmba = prmfir
038100021009     C***                endif
038200010201     C                   eval      cmznrr = *zeros
038300020305     C                   move      §ctrokvt      cmznrr
038400021018     C                   eval      cmzlba = vlrfl1
038500010201     C                   call(e)   'TIS711C'
038600010201     C                   parm                    dscmz
038700010201     C                   parm      *blanks       esito
038800010205     C                   if        %error
038900010205     C                             or cmzerr = '1'
039000010205     C                             or esito  = '1'
039100010205     C                   eval      wrkesito = '3'
039200010205     C                   else
039300010201     C*
039400091021     C* 2° invio EDIVAB
039500010201     C                   reset                   dscmz
039600010201     C                   move      vlrpoi        cmzdst
039700091021     C                   eval      cmzfld = 'EDIVABWR'
039800010201     C                   eval      cmzmbd = vlrhdl
039900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
040000021009     C***                if        prmfir = *blanks
040100091021     C                   eval      cmzfla = 'EDIVAB0F'
040200091021     C                   eval      cmzmba = 'EDIVAB0F'
040300021009     C***                else
040400021009     C***                eval      cmzfla = prmfir
040500021009     C***                eval      cmzmba = prmfir
040600021009     C***                endif
040700010201     C                   eval      cmznrr = *zeros
040800010201     C                   move      §ctrokvb      cmznrr
040900021018     C                   eval      cmzlba = vlrfl1
041000010201     C                   call(e)   'TIS711C'
041100010201     C                   parm                    dscmz
041200010201     C                   parm      *blanks       esito
041300010201     C                   if        %error
041400010201     C                             or cmzerr = '1'
041500010201     C                             or esito  = '1'
041600010201     C                   eval      wrkesito = '3'
041700010201     C                   endif
041800010205     C                   endif
041900990920     C*
042000000613     C                   ENDSR
042100000613     C***
042200090331
042300090331
042400090331
042500070411     C     *pssr         BEGSR
042600070411     C*
042700091021     C                   if        %open(edivabwr)
042800091021     C                   close     edivabwr
042900070411     C                   endif
043000091021     C                   if        %open(edivatwr)
043100091021     C                   close     edivatwr
043200070411     C                   endif
043300070411     C*
043400070411     C* Effettuo la chiamata al CLLE preposto
043500091014     C                   if        vlrtip = 'VB'
043600091021     C                   call(e)   'TITVEVTC'
043700070411     C                   parm                    parccm
043800070411     C                   parm                    parmbr
043900070411     C                   parm      '2'           paropz
044000091014     C                   endif
044100091014     C*
044200091014     C                   if        vlrtip = 'VT'
044300091021     C                   call(e)   'TITVEVBC'
044400091014     C                   parm                    parccm
044500091014     C                   parm                    parmbr
044600091014     C                   parm      '2'           paropz
044700091014     C                   endif
044800070411     C*
044900070411     C                   eval      wrkesito = '2'
045000070411     C*
045100070411     C                   seton                                        LR
045200070411     C*
045300070411     C                   ENDSR     '*CANCL'
045400070411     C***
045500070411
045600090331
045700990910
045800000613     C     *inzsr        BEGSR
045900990910     C*
046000990910     C     *entry        plist
046100990920     C                   parm                    tivlrds
046200990921     C                   parm      wrkesito      esito
046300000724     C                   parm                    prmlit
046400000710     C                   parm                    prmfir
046500091021     C*
046600091021     C* CALCOLA LA DATA CORRENTE
046700091021     C                   time                    wn14             14 0
046800091021     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
046900091021     C                   movel     wn14          oracor            6 0          *ORA
047000091110     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
047100091110     C                   eval      datcor = %dec(%date() : *ISO)
047200000830     C*
047300000613     C                   ENDSR
047400000613     C***
047500990908
047600090331
047700090331
047800091022     Otitv1s5p  E            riepilogo         2
047900990915     O                                              'Upload via Internet'
048000091013     O                                           +1 'Traduzione TITV1S40F-'
048100091021     O                                           +1 'EDIVABWR/EDIVATWR'
048200010201     O                                           +1 'Report testate bolle'
048300990915     O          E            riepilogo   2
048400990915     O                       wrkdata
048500990915     O                       wrkora              +1
048600990915     O                       procname            +1
048700990915     O          E            riepilogo   2
048800990915     O                                              'Cliente..................:'
048900091015     O                       W_VABCCM      z     +1
049000990915     O          E            riepilogo   2
049100990920     O                                              'Riferimento Strategi.....:'
049200990920     O                       vlrhdl              +1
049300990915     O          E            riepilogo   2
049400990915     O                                              'Giusti...................:'
049500010201     O                       §CTROKVB      2   +  1
049600990915     O          E            riepilogo   2
049700990915     O                                              'Sbagliati e corretti.....:'
049800990915     O          E            riepilogo   2
049900990915     O                                              'Sbagliati e scartati.....:'
050000971022     O                       §CTRNO        2   +  1
050100000613
050200091022     Otitv1s5ps E            testdett          2
050300000613     O                                              'Upload via Internet'
050400091013     O                                           +1 'Traduzione TITV1S40F-'
050500091021     O                                           +1 'EDIVABWR/EDIVATWR'
050600010201     O                                           +1 'Report testate bolle'
050700000616     O          E            testdett    3
050800000613     O                                           +2 'N° rec'
050900000613     O                                           +3 'Anteprima contenuto'
051000000616     O          E            rigadett    2
051100000621     O                       recko               +3
051200000616     O          E            findett     2
051300000613     O                       wrkdata
051400000613     O                       wrkora              +1
051500000613     O                       procname            +1
051600000616     O          E            findett     2
051700000613     O                                              'Cliente..................:'
051800091015     O                       W_VABCCM      z     +1
051900000616     O          E            findett     2
052000000613     O                                              'Riferimento Strategi.....:'
052100000613     O                       vlrhdl              +1
052200000616     O          E            findett     2
052300000613     O                                              'Giusti...................:'
052400010201     O                       §CTROKVB      2   +  1
052500000616     O          E            findett     2
052600000613     O                                              'Sbagliati e corretti.....:'
052700000616     O          E            findett     2
052800000613     O                                              'Sbagliati e scartati.....:'
052900000613     O                       §CTRNO        2   +  1
053000000512** CMD - COMANDI CL
053100091022OVRPRTF FILE(TITV1S5P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
053200091022OVRPRTF FILE(TITV1S5PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
053300091022DLTOVR FILE(TITV1S5P TITV1S5PS) LVL(*)
053400000512
053500000512
