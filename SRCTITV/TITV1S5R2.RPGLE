000100091021      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200081217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400091015     FTITV1S41L uF   E           k DISK    prefix(W_)
000500091021     FEDIVABwr  O    E             DISK    usropn
000600091021     FEDIVATwr  O    E             DISK    usropn
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990920     D dscmz         e ds                  inz
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500990910     D esito           s              1
001600000724     D prmlit          s             10
001700000710     D prmfir          s             10
001800990921     D wrkesito        s                   like(esito)
001900010202     D parccm          s              8    INZ(*blanks)
002000010202     D parmbr          s             10    INZ(*blanks)
002100010202     D paropz          s              1    INZ(*blanks)
002200010202     D chkcall         s              1    INZ(*blanks)
002300091015     D titv1s4ds_wk  e ds                  extname(titv1s40f)
002400091015     D                                     prefix(W_)
002500091015     D titv1s4ds_ok  e ds                  extname(titv1s40f)
002600091015     D curSped         ds                  qualified
002700091015     D   TV1S4KEY                          like(W_TV1S4KEY)
002800091015     D   vabCCM                            like(W_vabCCM)
002900091015     D   vabAAS                            like(W_vabAAS)
003000091015     D   vabLNP                            like(W_vabLNP)
003100091015     D   vabNRS                            like(W_vabNRS)
003200091015     D   vabNSP                            like(W_vabNSP)
003300091013     D depSped         ds                  likeds(curSped)
003400010201
003500081217
003600081217     C
003700990921     C                   reset                   esito
003800990921     C                   reset                   wrkesito
003900000613     C*
004000091110     C                   EXSR      DEFCAM
004100040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004200000613     C*
004300010202     C* Effettuo la chiamata al CLLE preposto
004400091014     C                   if        vlrtip = 'VB'
004500091021     C                   call(e)   'TITVEVTC'
004600010202     C                   parm                    parccm
004700010202     C                   parm                    parmbr
004800010202     C                   parm      '2'           paropz
004900091014     C                   endif
005000091014     C*
005100091014     C                   if        vlrtip = 'VT'
005200091021     C                   call(e)   'TITVEVBC'
005300091014     C                   parm                    parccm
005400091014     C                   parm                    parmbr
005500091014     C                   parm      '2'           paropz
005600091014     C                   endif
005700000616     C*
005800000801     C
005900010201     C                   seton                                        LR
006000091110
006100091110
006200091110     C*----------------------------------------------------*
006300091110     C*  IMPOSTAZIONE CAMPI COSTANTI
006400091110     C*----------------------------------------------------*
006500091110     C     DEFCAM        BEGSR
006600091123     C*
006700091123     C                   SETOFF                                       50
006800091110     C*
006900091110     C* Imposto i valori di default...
007000091110     C* ... e poi verifico se sono stati passati come parametri
007100091110     C                   IF        %subst(vlrppt:21) > *blanks
007200091123     C                   IF        %subst(vlrppt:21:1) = 'X'
007300091123     C                   SETON                                        50
007400091123     C                   ENDIF
007500091110     C                   ENDIF
007600091110     C*
007700091110     C                   ENDSR
007800990908
007900000801
008000910830     C*--------------------------------------------------------
008100091021     C* RWFILE   LEGGE TITV1S40F E SCRIVE EDIVABWR e EDIVATWR
008200910830     C*--------------------------------------------------------
008300040526     C     RWFILE        BEGSR
008400070103     C*
008500091021     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
008600091014     C                   exsr      prevabt
008700010201     C*
008800010202     C                   if        chkcall = '0'
008900091014     C*
009000091021     C                   if        not %open(edivabwr)
009100091021     C                   open      edivabwr
009200091014     C                   endif
009300010202     C*
009400091021     C                   if        not %open(edivatwr)
009500091021     C                   open      edivatwr
009600010201     C                   endif
009700091013     C*
009800091013     C                   clear                   §CTROKVB          5 0
009900091013     C                   clear                   §CTROKVT          5 0
010000091013     C                   clear                   §CTRNO            5 0
010100091013     C*
010200091022     C                   EVAL      W_TV1S4KEY  = '02101177_BOLLE'
010300990910     C*
010400091015     C     W_TV1S4KEY    setll     TITV1S41L
010500091013     C                   if        %found(TITV1S41L)
010600091015     C     W_TV1S4KEY    reade     TITV1S41L
010700091013     C                   dow       not %eof(TITV1S41L)
010800000801     C*
010900091123     C                   setoff                                       51
011000091123     C*
011100091123     C                   select
011200091123     C                   when      W_TV1S4VAB = 'X' and
011300091123     C                             W_TV1S4VAT = 'X'
011400091123     C                   seton                                        51
011500091123     C                   when      W_TV1S4VAB = 'X'  and
011600091123     C                             vlrtip     = 'VB' and
011700091123     C                             *IN50      = *on
011800091123     C                   seton                                        51
011900091123     C                   when      W_TV1S4VAT = 'X'  and
012000091123     C                             vlrtip     = 'VT' and
012100091123     C                             *IN50      = *on
012200091123     C                   seton                                        51
012300091123     C                   endsl
012400091123     C*
012500091123     C* ---- Record buoni da inviare in filiale
012600091123     C                   if        *in51 = *on
012700091015     C*
012800091015     C                   eval      curSped.TV1S4KEY = W_TV1S4KEY
012900091015     C                   eval      curSped.vabCCM   = W_vabCCM
013000091015     C                   eval      curSped.vabAAS   = W_vabAAS
013100091015     C                   eval      curSped.vabLNP   = W_vabLNP
013200091015     C                   eval      curSped.vabNRS   = W_vabNRS
013300091015     C                   eval      curSped.vabNSP   = W_vabNSP
013400091015     C*
013500070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
013600091013     C                   if        curSped <> depSped
013700070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
013800091015     C                   if        §CTROKVT > *zeros
013900091013     C                   exsr      wrivab                                       => scarico VAB
014000091015     C                   endif
014100091015     C* Assegno il buffer del formato record corrente
014200091015     C                   eval      titv1s4ds_ok =  titv1s4ds_wk
014300091015     C*
014400091013     C                   exsr      wrivat                                       => scarico VAT
014500070103     C* Salvo il raggruppamento spedizione cliente corrente
014600091013     C                   eval      depSped = curSped
014700070213     C*
014800070213     C* Se collo successivo x stessa bolla
014900070213     C                   else
015000091015     C* Assegno il buffer del formato record corrente
015100091015     C                   eval      titv1s4ds_ok =  titv1s4ds_wk
015200091015     C*
015300091013     C                   exsr      wrivat                                       => scarico VAT
015400070103     C                   endif
015500000905     C*
015600091013     C                   delete    TITV1S41L
015700050628     C                   endif
015800091013     C* ----
015900091013     C*
016000091013     C*
016100091013     C* ---- Record con errori (da gestire in qualche modo)
016200091015     C                   if        W_TV1S4VAB = '*' or
016300091015     C                             W_TV1S4VAT = '*'
016400091013     C                   ADD       1             §CTRNO
016500091013     C                   endif
016600091013     C* ----
016700091013     C*
016800091015     C     W_TV1S4KEY    reade     TITV1S41L
016900091013     C*
017000091013     C                   ENDdo
017100091015     C*
017200091015     C* Scarico le testate in sospeso
017300091123     C                   setoff                                       51
017400091123     C*
017500091123     C                   select
017600091123     C                   when      W_TV1S4VAB = 'X'  and
017700091123     C                             vlrtip     = 'VB' and
017800091123     C                             *IN50      = *on
017900091123     C                   seton                                        51
018000091123     C                   when      W_TV1S4VAT = 'X'  and
018100091123     C                             vlrtip     = 'VT' and
018200091123     C                             *IN50      = *on
018300091123     C                   seton                                        51
018400091123     C                   endsl
018500091123     C*
018600091123     C                   if        *in51 = *on
018700091015     C                   exsr      wrivab                                       => scarico VAB
018800091015     C                   endif
018900010202     C*
019000010202     C                   endif
019100091013     C*
019200091013     C                   endif
019300091013
019400990910
019500990910     C* Se non ci sono record con errori ...
019600000710     C                   if        §ctrno = 0
019700990910     C* ... restituisco esito OK.
019800990921     C                   eval      wrkesito = '0'
019900990910     C                   else
020000010201     C                   if        §ctrokvb > 0
020100990921     C                   eval      wrkesito = '1'
020200000710     C                   else
020300000710     C                   eval      wrkesito = '2'
020400990910     C                   endif
020500000710     C                   endif
020600990910     C*
020700091021     C                   if        %open(edivabwr)
020800091021     C                   close     edivabwr
020900990914     C                   endif
021000091021     C                   if        %open(edivatwr)
021100091021     C                   close     edivatwr
021200010201     C                   endif
021300990910     C*
021400091013     C                   if        (§ctrokvb > 0 or
021500091013     C                              §ctrokvt > 0)
021600000724     C                             and vlrpoi <> *zeros
021700010202     C                   exsr      invio
021800990920     C                   endif
021900990920     C*
022000910830     C                   ENDSR
022100000613     C***
022200010305
022300010305     C*----------------------------------------------------*
022400020305     C*  SCARICAMENTO BUFFER RECORDS VAB
022500010305     C*----------------------------------------------------*
022600020305     C     WRIVAB        BEGSR
022700010305     C*
022800060225     C* Quindi scarico il buffer del file d testata
022900091021     C                   write     edivab00                                     => scarico il VAB
023000091014     C                   ADD       1             §ctrokvb
023100010305     C*
023200010305     C                   ENDSR
023300091013
023400010201     C*----------------------------------------------------*
023500091021     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
023600010201     C*----------------------------------------------------*
023700020305     C     WRIVAT        BEGSR
023800091022     C*
023900060223     C* Scrivo solo se valorizzato qualcosa
024000060223     C                   IF        VATNOT <> *blanks
024100091021     C                   WRITE     EDIVAT00
024200090922     C                   ADD       1             §ctrokvt
024300091123     C                   ELSE
024400091123     C   50              ADD       1             §ctrokvt
024500060223     C                   ENDIF
024600010201     C*
024700010201     C                   ENDSR
024800091013
024900010202     C*----------------------------------------------------*
025000091021     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
025100010202     C*----------------------------------------------------*
025200091014     C     PREVABT       BEGSR
025300010202     C*
025400091021     C* Compongo il nome del membro da dare al EDIVATWR
025500010202     C                   eval      parmbr = vlrhdl
025600010202     C                   movel     'M'           parmbr
025700050627     C                   eval      parccm = %subst(vlrKSC:2:7)
025800010202     C                   eval      paropz = '1'
025900010202     C* Effettuo la chiamata al CLLE preposto
026000091014     C                   if        vlrtip = 'VB'
026100091021     C                   call(e)   'TITVEVTC'
026200010202     C                   parm                    parccm
026300010202     C                   parm                    parmbr
026400010202     C                   parm                    paropz
026500091014     C                   endif
026600091014     C*
026700091014     C                   if        vlrtip = 'VT'
026800091021     C                   call(e)   'TITVEVBC'
026900091014     C                   parm                    parccm
027000091014     C                   parm                    parmbr
027100091014     C                   parm                    paropz
027200091014     C                   endif
027300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
027400010202     C                   if        %error
027500010202     C                   movel     '1'           chkcall
027600010202     C                   else
027700010202     C                   movel     '0'           chkcall
027800010202     C                   endif
027900010202     C*
028000010202     C                   ENDSR
028100011113
028200000801
028300000801
028400990920      /TITLE Invio dei dati al punto operativo.
028500010202     C     invio         BEGSR
028600990920     C*
028700091021     C* 1° invio EDIVAT
028800010201     C                   reset                   dscmz
028900010201     C                   move      vlrpoi        cmzdst
029000091021     C                   eval      cmzfld = 'EDIVATWR'
029100010201     C                   eval      cmzmbd = vlrhdl
029200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
029300021009     C***                if        prmfir = *blanks
029400091021     C                   eval      cmzfla = 'EDIVAT0F'
029500091021     C                   eval      cmzmba = 'EDIVAT0F'
029600021009     C***                else
029700021009     C***                eval      cmzfla = prmfir
029800021009     C***                eval      cmzmba = prmfir
029900021009     C***                endif
030000010201     C                   eval      cmznrr = *zeros
030100020305     C                   move      §ctrokvt      cmznrr
030200021018     C                   eval      cmzlba = vlrfl1
030300010201     C                   call(e)   'TIS711C'
030400010201     C                   parm                    dscmz
030500010201     C                   parm      *blanks       esito
030600010205     C                   if        %error
030700010205     C                             or cmzerr = '1'
030800010205     C                             or esito  = '1'
030900010205     C                   eval      wrkesito = '3'
031000010205     C                   else
031100010201     C*
031200091021     C* 2° invio EDIVAB
031300010201     C                   reset                   dscmz
031400010201     C                   move      vlrpoi        cmzdst
031500091021     C                   eval      cmzfld = 'EDIVABWR'
031600010201     C                   eval      cmzmbd = vlrhdl
031700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
031800021009     C***                if        prmfir = *blanks
031900091021     C                   eval      cmzfla = 'EDIVAB0F'
032000091021     C                   eval      cmzmba = 'EDIVAB0F'
032100021009     C***                else
032200021009     C***                eval      cmzfla = prmfir
032300021009     C***                eval      cmzmba = prmfir
032400021009     C***                endif
032500010201     C                   eval      cmznrr = *zeros
032600010201     C                   move      §ctrokvb      cmznrr
032700021018     C                   eval      cmzlba = vlrfl1
032800010201     C                   call(e)   'TIS711C'
032900010201     C                   parm                    dscmz
033000010201     C                   parm      *blanks       esito
033100010201     C                   if        %error
033200010201     C                             or cmzerr = '1'
033300010201     C                             or esito  = '1'
033400010201     C                   eval      wrkesito = '3'
033500010201     C                   endif
033600010205     C                   endif
033700990920     C*
033800000613     C                   ENDSR
033900000613     C***
034000090331
034100090331
034200090331
034300070411     C     *pssr         BEGSR
034400070411     C*
034500091021     C                   if        %open(edivabwr)
034600091021     C                   close     edivabwr
034700070411     C                   endif
034800091021     C                   if        %open(edivatwr)
034900091021     C                   close     edivatwr
035000070411     C                   endif
035100070411     C*
035200070411     C* Effettuo la chiamata al CLLE preposto
035300091014     C                   if        vlrtip = 'VB'
035400091021     C                   call(e)   'TITVEVTC'
035500070411     C                   parm                    parccm
035600070411     C                   parm                    parmbr
035700070411     C                   parm      '2'           paropz
035800091014     C                   endif
035900091014     C*
036000091014     C                   if        vlrtip = 'VT'
036100091021     C                   call(e)   'TITVEVBC'
036200091014     C                   parm                    parccm
036300091014     C                   parm                    parmbr
036400091014     C                   parm      '2'           paropz
036500091014     C                   endif
036600070411     C*
036700070411     C                   eval      wrkesito = '2'
036800070411     C*
036900070411     C                   seton                                        LR
037000070411     C*
037100070411     C                   ENDSR     '*CANCL'
037200070411     C***
037300070411
037400090331
037500990910
037600000613     C     *inzsr        BEGSR
037700990910     C*
037800990910     C     *entry        plist
037900990920     C                   parm                    tivlrds
038000990921     C                   parm      wrkesito      esito
038100000724     C                   parm                    prmlit
038200000710     C                   parm                    prmfir
038300091021     C*
038400091021     C* CALCOLA LA DATA CORRENTE
038500091021     C                   time                    wn14             14 0
038600091021     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
038700091021     C                   movel     wn14          oracor            6 0          *ORA
038800091110     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
038900091110     C                   eval      datcor = %dec(%date() : *ISO)
039000000830     C*
039100000613     C                   ENDSR
039200000613     C***
