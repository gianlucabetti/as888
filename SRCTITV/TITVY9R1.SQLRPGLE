000100101007      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200990908     H dftactgrp(*yes)
000300990908
000400020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000500101007     FEDIVABwr  O    E             DISK    usropn
000600101007     FEDIVATwr  O    E             DISK    usropn
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
001900000613     D rrnum           s              6  0 INZ(*zeros)
002000070806     D depspe          s             12    INZ(*blanks)
002100010213     D depdat          s              8  0 INZ(*zeros)
002200010213     D depaas          s              4    INZ(*zeros)
002300070806     D curspe          s             12    INZ(*blanks)
002400010202     D parccm          s              8    INZ(*blanks)
002500010202     D parmbr          s             10    INZ(*blanks)
002600010202     D paropz          s              1    INZ(*blanks)
002700010202     D chkcall         s              1    INZ(*blanks)
002800020725     D tivinds       e ds                  extname(tivin00r) prefix(x_)
002900070530     D Num5_0          s              5  0
003000101007     D wNomeFile       s             30    INZ(*blanks)
003100020725
003200020725     D*------------------
003300020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003400020725     D*------------------
003500070530     D INPUT_DS        DS                  INZ
003600101007     D  VINDTA                     2048
003700101007     D  VINFLG                        1
003800101007     D  VINSPE                       12
003900101007     D  VINRRN                       15  0
004000070806     D*------------------
004100070806     D* DS REPERIMENTO NUMERATORE
004200070806     D*------------------
004300070806     D trul33ds      e ds                  inz
004400070806     D*------------------
004500070806     D* DS ARCHITETTURA
004600070806     D*------------------
004700070806     D kpjba         e ds                  inz
004800990908
004900010201
005000101007     C*
005100101007     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
005200101007     C
005300101007     C/EXEC SQL
005400101007     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
005500101007     C/END-EXEC
005600010201
005700000913     C                   reset                   rrnum
005800990921     C                   reset                   esito
005900990921     C                   reset                   wrkesito
006000000613     C*
006100070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
006200000613     C*
006300010202     C* Effettuo la chiamata al CLLE preposto
006400101007     C                   call(e)   'TITVEVTC'
006500010202     C                   parm                    parccm
006600010202     C                   parm                    parmbr
006700010202     C                   parm      '2'           paropz
006800000616     C*
006900000801     C
007000010201     C                   seton                                        LR
007100990908
007200000801
007300910830     C*--------------------------------------------------------
007400101007     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
007500910830     C*--------------------------------------------------------
007600070530     C     RWFILE        BEGSR
007700990910     C*
007800990914     C                   if        not %open(tivin00r)
007900990908     C                   open      tivin00r
008000990914     C                   endif
008100101007     C                   if        not %open(edivabwr)
008200101007     C                   open      edivabwr
008300990914     C                   endif
008400070530     C*
008500101007     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
008600020305     C                   exsr      prevat
008700010201     C*
008800010202     C                   if        chkcall = '0'
008900010202     C*
009000101007     C                   if        not %open(edivatwr)
009100101007     C                   open      edivatwr
009200010201     C                   endif
009300990910     C*
009400010201     C                   clear                   §CTROKVB          5 0
009500020305     C                   clear                   §CTROKVT          5 0
009600000801     C                   clear                   §CTRMO            5 0
009700000801     C                   clear                   §CTRNO            5 0
009800990910     C*
009900020725     C/EXEC SQL
010000101007     C+ declare C1 cursor for select
010100101007     C+ vindta, vinflg, substr(vindta, 7, 12) as sped, rrn(tivin00r)
010200060223     C+ from tivin00r
010300060223     C+ order by sped
010400020725     C+ for read only
010500020725     C/END-EXEC
010600020725     C
010700020725     C/EXEC SQL
010800020725     C+ open C1
010900020725     C/END-EXEC
011000020725     C
011100020725     C/EXEC SQL
011200070530     C+ Fetch C1 into :INPUT_DS
011300020725     C/END-EXEC
011400020725     C*
011500020725     C                   dow       sqlcod = *zeros
011600990913     C*
011700020725     C                   if        vindta > *blanks
011800000613     C                   add       1             rrnum
011900000801     C*
012000020725     C                   if        vinflg = *blanks
012100020725     C                             or vinflg = '0'
012200020725     C                             or vinflg = '2'
012300000801     C*
012400020725     C                   clear                   x_vinmsg
012500020725     C                   eval      x_vinflg = '1'
012600010305     C*
012700010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
012800140415     C                   EVAL      PiStr=%trim(%subst(vindta:7:12))
012900020305     C                   MOVEL(p)  PiStr         curspe
013000010305     C*
013100010305     C                   if        depspe = *blanks                             => 1° giro
013200010305     C                   eval      depspe = curspe                              => memorizz. spediz
013300020725     C                   clear                   tivinds
013400020725     C                   exsr      inzvar
013500020725     C                   exsr      defcam
013600070806     C                   exsr      repnum
013700020305     C                   exsr      impvab
013800101007     C                   exsr      impvat_e                                     => carico VAT
013900010305     C                   else
014000020725     C                   if        depspe <> curspe                             => rottura di spediz
014100010305     C                   eval      depspe = curspe                              => memorizz. spediz
014200020305     C                   exsr      wrivab
014300020725     C                   clear                   tivinds
014400020725     C                   exsr      inzvar
014500020725     C                   exsr      defcam
014600070806     C                   exsr      repnum
014700020305     C                   exsr      impvab
014800101007     C                   exsr      impvat_e                                     => carico VAT
014900020305     C                   else                                                   => x stessa spediz
015000020305     C                   exsr      impvab
015100101007     C                   exsr      impvat_e                                     => carico VAT
015200010305     C                   endif
015300010305     C                   endif
015400010305     C                   endif
015500000905     C*
015600000905     C                   else
015700020725     C                   eval      x_vinflg = '1'
015800000905     C                   endif
015900000905     C*
016000020725     C     VINRRN        chain     tivin000
016100020725     C                   if        %found(tivin00r)
016200020725     C                   eval      y_vinflg = x_vinflg
016300020725     C                   eval      y_vinmsg = x_vinmsg
016400020725     C                   update    tivin000
016500020725     C                   endif
016600020725     C*
016700020725     C/EXEC SQL
016800070530     C+ Fetch C1 into :INPUT_DS
016900020725     C/END-EXEC
017000020725     C*
017100020725     C                   enddo
017200020725     C*
017300020725     C/EXEC SQL
017400020725     C+ close C1
017500020725     C/END-EXEC
017600000905     C*
017700020305     C* Scarico i VAB rimasti "in sospeso"
017800020305     C                   exsr      wrivab
017900010202     C*
018000010202     C                   endif
018100990910
018200990910     C* Se non ci sono record con errori ...
018300000710     C                   if        §ctrno = 0
018400990910     C* ... restituisco esito OK.
018500990921     C                   eval      wrkesito = '0'
018600990910     C                   else
018700010201     C                   if        §ctrokvb > 0
018800990921     C                   eval      wrkesito = '1'
018900000710     C                   else
019000000710     C                   eval      wrkesito = '2'
019100990910     C                   endif
019200000710     C                   endif
019300990910     C*
019400990914     C                   if        %open(tivin00r)
019500990908     C                   close     tivin00r
019600990914     C                   endif
019700101007     C                   if        %open(edivabwr)
019800101007     C                   close     edivabwr
019900990914     C                   endif
020000101007     C                   if        %open(edivatwr)
020100101007     C                   close     edivatwr
020200010201     C                   endif
020300990910     C*
020400010201     C                   if        §ctrokvb > 0
020500000724     C                             and vlrpoi <> *zeros
020600010202     C                   exsr      invio
020700990920     C                   endif
020800990920     C*
020900910830     C                   ENDSR
021000000613     C***
021100010305
021200010305     C*----------------------------------------------------*
021300020305     C*  SCARICAMENTO BUFFER RECORDS VAB
021400010305     C*----------------------------------------------------*
021500020305     C     WRIVAB        BEGSR
021600101007     C*
021700101007     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
021800101007     C                   EVAL      VABCMR = %subst(wNomeFile:17:5)
021900101007     C                   EVAL      VABDCM = DATCOR
022000101007     C                   EVAL      VABDTS = DATCOR
022100101007     C                   EVAL      VABHMS = ORACOR
022200101007     C                   EVAL      VABCNT = 1
022300010305     C*
022400101007     C                   write     edivab00                                     => scarico il VAB
022500070530     C*
022600150429     C* Insieme alla testata scarico le estensini destinatario su VAT - tipi record A B S
022700070530     C                   if        wVAT_A <> *blanks
022800070530     C                   eval      VATTRC = 'A'
022900070530     C                   eval      VATNOT = wVAT_A
023000101007     C                   exsr      wrivat
023100070530     C                   endif
023200070530     C*
023300070530     C                   if        wVAT_B <> *blanks
023400070530     C                   eval      VATTRC = 'B'
023500070530     C                   eval      VATNOT = wVAT_B
023600101007     C                   exsr      wrivat
023700070530     C                   endif
023800150429     C*
023900150429     C                   if        wVAT_S <> *blanks
024000150429     C                   eval      VATTRC = 'S'
024100150429     C                   eval      VATNOT = wVAT_S
024200150429     C* i 2 flag devono essere blank
024300150429     C                   EVAL      %subst(VATNOT:17:1)=*blank
024400150429     C                   EVAL      %subst(VATNOT:18:1)=*blank
024500150429     C                   exsr      wrivat
024600150429     C                   endif
024700010305     C*
024800010305     C                   ENDSR
024900990920
025000000801     C*----------------------------------------------------*
025100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
025200000801     C*----------------------------------------------------*
025300010201     C     INZVAR        BEGSR
025400000801     C*
025500010201     C                   Z-ADD     *zeros        Num5_0
025600070530     C                   MOVEL     *blanks       wVAT_A           30
025700070530     C                   MOVEL     *blanks       wVAT_B           30
025800150429     C                   MOVEL     *blanks       wVAT_S           35
025900020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
026000020725     C                   MOVEL     '0'           FlgCAS            1
026100000801     C*
026200000801     C                   ENDSR
026300000801     C*----------------------------------------------------*
026400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
026500000801     C*----------------------------------------------------*
026600020904     C     DEFCAM        BEGSR
026700000801     C*
026800101007     C                   CLEAR                   EDIVAB00
026900101007     C                   CLEAR                   EDIVAT00
027000070530     C                   Z-ADD     0054201       VABCCM
027100070530     C                   Z-ADD     0054201       VATCCM
027200070530     C                   Z-ADD     005           VABLNP
027300070530     C                   Z-ADD     005           VATLNP
027400070807     C                   Z-ADD     000           VABCTR
027500020305     C                   MOVEL     '7Q'          VABCTM
027600071009     C                   MOVEL     'C'           VABTSP
027700070530     C                   MOVEL     '1'           VABCBO
027800070222     C* ... e poi verifico se sono stati passati come parametri
027900070222     C                   IF        vlrppt > *blanks
028000070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
028100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
028200070222     C                   EXSR      CHKNUM
028300070222     C                   IF        PiInt=*on
028400070222     C                   Z-ADD     PiVal         VABCCM
028500070222     C                   Z-ADD     PiVal         VATCCM
028600070222     C                   ENDIF
028700070222     C                   ENDIF
028800070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
028900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
029000070222     C                   EXSR      CHKNUM
029100070222     C                   IF        PiInt=*on
029200070222     C                   Z-ADD     PiVal         VABLNP
029300070222     C                   Z-ADD     PiVal         VATLNP
029400070222     C                   ENDIF
029500070222     C                   ENDIF
029600070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
029700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
029800070222     C                   EXSR      CHKNUM
029900070222     C                   IF        PiInt=*on
030000070222     C                   Z-ADD     PiVal         VABCTR
030100070222     C                   ENDIF
030200070222     C                   ENDIF
030300070222     C                   ENDIF
030400000801     C*
030500000801     C                   ENDSR
030600070806     C*----------------------------------------------------*
030700070806     C*  REPERIMENTO NUMERATORE X NUMERO SPEDIZIONE VAB/VAT
030800070806     C*----------------------------------------------------*
030900070806     C     REPNUM        BEGSR
031000070806     C*
031100070806     C* NSP => Stacco un numeratore da AZNUM
031200070806     C                   clear                   TRUL33DS
031300070806     C                   eval      I33OPE = *zeros
031400070806     C                   eval      I33CNU = 302
031500070806     C                   eval      I33NUM = 1
031600070806     C                   movel     TRUL33DS      KPJBU
031700070806     C                   call      'TRUL33R'
031800070806     C                   parm                    KPJBA
031900070806     C                   movel     KPJBU         TRUL33DS
032000070806     C                   if        O33ERR = *zeros
032100070806     C                   z-add     O33NRF        VABNSP
032200070806     C                   z-add     O33NRF        VATNSP
032300070806     C                   else
032400070806     C                   SETON                                        31
032500070806     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
032600070806     C                             + ' ' + 'VABNSP VATNSP'
032700070806     C                   endif
032800070806     C*
032900070806     C                   ENDSR
033000000801     C*----------------------------------------------------*
033100101007     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
033200000801     C*----------------------------------------------------*
033300010201     C     IMPVAB        BEGSR
033400070530     C*
033500070530     C                   SETOFF                                       3132
033600021113     C*
033700021113     C                   MOVE(P)   vlrpoi        VABFGS
033800021113     C                   MOVE(P)   vlrpoi        VATFGS
033900020725     C*
034000070530     C* Campi anno e mese/giorno
034100070530     C                   MOVEL     datcor        VABAAS
034200070530     C                   MOVEL     datcor        VATAAS
034300070530     C                   MOVE      datcor        VABMGS
034400070530     C*
034500070530     C* Reperimento campi ALFA
034600070530     C*
034700070530     C* Considerazioni sulla ragione sociale del destinatario da indicare
034800070530     C                   EVAL      VABRSD=%trim(%subst(vindta:322:30))
034900070530     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035000070530     C     '@':'A'       XLATE     VABRSD        VABRSD
035100070530     C* ==
035200070530     C                   EVAL      VABRD2=%trim(%subst(vindta:669:27))
035300070530     C                   EVAL      VABIND=%trim(%subst(vindta:352:30))
035400070530     C                   EVAL      VABLOD=%trim(%subst(vindta:412:30))
035500070530     C                   MOVEL     *BLANKS       wPRD             30
035600070530     C                   EVAL      wPRD=%trim(%subst(vindta:442:30))
035700070530     C     '<':' '       XLATE     wPRD          wPRD
035800070530     C     '>':' '       XLATE     wPRD          wPRD
035900070530     C     '(':' '       XLATE     wPRD          wPRD
036000070530     C     ')':' '       XLATE     wPRD          wPRD
036100070530     C                   EVAL      VABPRD=%trim(wPRD)
036200070530     C                   EVAL      VABRMA=%trim(%subst(vindta:7:12))
036300070530     C                   SELECT
036400070530     C                   WHEN      %trim(%subst(vindta:701:1))='F'
036500070530     C                   EVAL      VABCBO='1'
036600070530     C                   WHEN      %trim(%subst(vindta:701:1))='A'
036700070530     C                   EVAL      VABCBO='2'
036800070530     C                   ENDSL
036900070530     C*
037000070806     C* RMN
037100070530     C                   EVAL      PiStr=%trim(%subst(vindta:7:12))
037200070530     C                   EXSR      CHKNUM
037300070530     C                   IF        PiInt=*on
037400070530     C                   Z-ADD     PiVal         VABRMN
037500070530     C                   ELSE
037600070806     C                   SETON                                        32
037700070530     C                   Z-ADD     *zeros        VABRMN
037800070530     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
037900070806     C                             + ' ' + 'VABRMN'
038000070530     C                   ENDIF
038100070530     C* CAD
038200070530     C                   EVAL      PiStr=%trim(%subst(vindta:472:9))
038300070530     C                   EXSR      CHKNUM
038400070530     C                   IF        PiInt=*on
038500070530     C                   Z-ADD     PiVal         Num5_0
038600140415     C                   MOVEL(p)  Num5_0        VABCAD
038700070530     C                   ELSE
038800070530     C                   SETON                                        32
038900070530     C                   EVAL      VABCAD = *zeros
039000070530     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039100070530     C                             + ' ' + 'VABCAD'
039200070530     C                   ENDIF
039300070530     C* NCL
039400070530     C                   EVAL      PiStr=%trim(%subst(vindta:758:5))
039500070530     C                   EXSR      CHKNUM
039600070530     C                   IF        PiInt=*on
039700070530     C                   Z-ADD     PiVal         VABNCL
039800070530     C                   ELSE
039900070530     C                   SETON                                        32
040000070530     C                   Z-ADD     *zeros        VABNCL
040100070530     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040200070530     C                             + ' ' + 'VABNCL'
040300070530     C                   ENDIF
040400070530     C* PKB
040500070530     C                   EVAL      PiStr=%trim(%subst(vindta:718:8))
040600070530     C                   EXSR      CHKNUM
040700070530     C                   IF        PiNum=*on
040800070530     C                   Z-ADD     PiVal         VABPKB
040900070530     C                   ELSE
041000070530     C                   SETON                                        32
041100070530     C                   Z-ADD     *zeros        VABPKB
041200070530     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041300070530     C                             + ' ' + 'VABPKB'
041400070530     C                   ENDIF
041500140415     C* IAS
041600140416     C                   IF        %trim(%subst(vindta:741:11))<>'00000000.00'
041700140416     C                             and %trim(%subst(vindta:741:11))<>*blanks
041800140416     C                             and %trim(%subst(vindta:741:11))<>*zeros
041900140415     C                   EVAL      VABVAS = 'EUR'
042000140416     C                   EVAL      PiStr=%trim(%subst(vindta:741:11))
042100140415     C                   EXSR      CHKNUM
042200140415     C                   IF        PiNum=*on
042300140415     C                   Z-ADD     PiVal         VABIAS
042400140415     C                   ELSE
042500140415     C                   SETON                                        32
042600140415     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042700140415     C                             + ' ' + 'VABIAS'
042800140415     C                   ENDIF
042900140415     C                   ENDIF
043000070530     C*
043100070530     C* Campi estensione destinatario su VAT - tipi record A e B
043200070530     C* VAT - A
043300070530     C                   EVAL      wVAT_A=%trim(%subst(vindta:504:22))
043400070530     C* VAT - B
043500170111     C                   EVAL      wVAT_B=%trim(%subst(vindta:526:10))
043600150429     C* VAT - S
043700170111     C                   EVAL      wVAT_S=%trim(%subst(vindta:526:10))
043800070530     C*
043900070530     C* Considerazioni finali su CBO/CAS
044000070530     C                   IF        FlgCAS = '1'
044100070530     C                   IF        VABCBO = '1'
044200070530     C                   EVAL      VABCBO = '4'
044300070530     C                   ENDIF
044400070530     C                   IF        VABCBO = '2'
044500070530     C                   EVAL      VABCBO = '6'
044600070530     C                   ENDIF
044700070530     C                   ENDIF
044800020305     C*
044900011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
045000011113     C                   EXSR      CHKIMPDIV
045100010202     C*
045200000801     C* Ebbene...
045300000801     C                   ADD       1             §CTRMO
045400070530     C                   IF        *in31 <> *zeros OR
045500070530     C                             *in32 <> *zeros
045600000801     C                   ADD       1             §CTRNO
045700020725     C                   EVAL      x_vinflg = '2'
045800000801     C                   ELSE
045900010201     C                   ADD       1             §CTROKVB
046000000801     C                   ENDIF
046100000801     C*
046200000801     C                   ENDSR
046300010201     C*----------------------------------------------------*
046400101007     C*  CONSIDERAZIONI E VALORIZZAZ. RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
046500010201     C*----------------------------------------------------*
046600101007     C     IMPVAT_E      BEGSR
046700010201     C*
046800101007     C* Valorizzo l buffer di scrittura del EDIVAT
046900070530     C                   eval      VATTRC = 'E'
047000070530     C                   eval      VATNOT = %trim(%subst(vindta:575:18))
047100101007     C                   exsr      wrivat
047200010201     C*
047300010201     C                   ENDSR
047400101007
047500101007
047600101007
047700101007     C*----------------------------------------------------*
047800101007     C*  SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
047900101007     C*----------------------------------------------------*
048000101007     C     WRIVAT        BEGSR
048100101007     C*
048200101007     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
048300101007     C                   EVAL      VATCMR = %subst(wNomeFile:17:5)
048400101007     C                   EVAL      VATCNT = 1
048500101007     C*
048600101007     C* Valorizzo l buffer di scrittura del EDIVAT
048700101007     C                   if        VATNOT <> *blanks
048800101007     C                   write     EDIVAT00
048900101007     C                   endif
049000101007     C*
049100101007     C                   ENDSR
049200020904
049300020904
049400010202     C*----------------------------------------------------*
049500101007     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
049600010202     C*----------------------------------------------------*
049700020305     C     PREVAT        BEGSR
049800010202     C*
049900101007     C* Compongo il nome del membro da dare al EDIVATWR
050000010202     C                   eval      parmbr = vlrhdl
050100010202     C                   movel     'M'           parmbr
050200070530     C                   eval      parccm = %subst(vlrKSC:2:7)
050300010202     C                   eval      paropz = '1'
050400010202     C* Effettuo la chiamata al CLLE preposto
050500101007     C                   call(e)   'TITVEVTC'
050600010202     C                   parm                    parccm
050700010202     C                   parm                    parmbr
050800010202     C                   parm                    paropz
050900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051000010202     C                   if        %error
051100010202     C                   movel     '1'           chkcall
051200010202     C                   else
051300010202     C                   movel     '0'           chkcall
051400010202     C                   endif
051500010202     C*
051600010202     C                   ENDSR
051700000801     C*----------------------------------------------------*
051800000801     C*  CONTROLLO NUMERICITA' CAMPI
051900000801     C*----------------------------------------------------*
052000000801     C     CHKNUM        BEGSR
052100000801     C*
052200000801     C                   call(e)   'ISNUMERIC'
052300000801     C                   PARM                    PiStr            30
052400070530     C                   PARM      '.'           PiDecChr          1
052500000801     C                   PARM      *ZEROS        PiVal            30 9
052600000801     C                   PARM      '0'           PiInt             1
052700000801     C                   PARM      '0'           PiNum             1
052800000801     C                   IF        %error
052900000801     C                   EVAL      PiInt=*off
053000000801     C                   ENDIF
053100000801     C*
053200000801     C                   ENDSR
053300000801     C***
053400000801
053500011113
053600011113     C*----------------------------------------------------*
053700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
053800011113     C*----------------------------------------------------*
053900011113     C     CHKIMPDIV     BEGSR
054000011113     C*
054100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
054200011113     C                   Z-ADD     *zeros        wrkDec            9 9
054300011113     C*
054400011113     C* Come prima cosa effettuo considerazioni sulla divisa
054500011113     C                   IF        vabIAS > *zeros
054600011113     C                   IF        vabVAS <> 'EUR'
054700011113     C                   EVAL      vabVAS =  'ITL'
054800011113     C                   ENDIF
054900011113     C                   ENDIF
055000011113     C*
055100011113     C                   IF        vabCAS > *zeros
055200011113     C                   IF        vabVCA <> 'EUR'
055300011113     C                   EVAL      vabVCA =  'ITL'
055400011113     C                   ENDIF
055500011113     C                   ENDIF
055600011113     C*
055700011113     C                   IF        vabVMD > *zeros
055800020305     C                   IF        vabVAD <> 'EUR'
055900011113     C                   EVAL      vabVAD =  'ITL'
056000011113     C                   ENDIF
056100011113     C                   ENDIF
056200011113     C*
056300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056400011113     C                   Z-ADD     vabIAS        wrkDec
056500011113     C                   IF        wrkDec > *zeros
056600011113     C                   IF        vabVAS = 'ITL'
056700011113     C                   EVAL      vabIAS = *zeros
056800011113     C                   ENDIF
056900011113     C                   ENDIF
057000011113     C*
057100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
057200011113     C                   Z-ADD     vabCAS        wrkDec
057300011113     C                   IF        wrkDec > *zeros
057400011113     C                   IF        vabVCA = 'ITL'
057500011113     C                   EVAL      vabCAS = *zeros
057600011113     C                   ENDIF
057700011113     C                   ENDIF
057800011113     C*
057900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
058000011113     C                   Z-ADD     vabVMD        wrkDec
058100011113     C                   IF        wrkDec > *zeros
058200011113     C                   IF        vabVAD = 'ITL'
058300011113     C                   EVAL      vabVMD = *zeros
058400011113     C                   ENDIF
058500011113     C                   ENDIF
058600011113     C*
058700011113     C                   ENDSR
058800011113     C***
058900011113
059000011113
059100000801
059200000801
059300990920      /TITLE Invio dei dati al punto operativo.
059400010202     C     invio         BEGSR
059500990920     C*
059600101007     C* 1° invio EDIVAT
059700010201     C                   reset                   dscmz
059800010201     C                   move      vlrpoi        cmzdst
059900101007     C                   eval      cmzfld = 'EDIVATWR'
060000010201     C                   eval      cmzmbd = vlrhdl
060100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060200021009     C***                if        prmfir = *blanks
060300101007     C                   eval      cmzfla = 'EDIVAT0F'
060400101007     C                   eval      cmzmba = 'EDIVAT0F'
060500021009     C***                else
060600021009     C***                eval      cmzfla = prmfir
060700021009     C***                eval      cmzmba = prmfir
060800021009     C***                endif
060900010201     C                   eval      cmznrr = *zeros
061000020305     C                   move      §ctrokvt      cmznrr
061100021018     C                   eval      cmzlba = vlrfl1
061200010201     C                   call(e)   'TIS711C'
061300010201     C                   parm                    dscmz
061400010201     C                   parm      *blanks       esito
061500010205     C                   if        %error
061600010205     C                             or cmzerr = '1'
061700010205     C                             or esito  = '1'
061800010205     C                   eval      wrkesito = '3'
061900010205     C                   else
062000010201     C*
062100101007     C* 2° invio EDIVAB
062200010201     C                   reset                   dscmz
062300010201     C                   move      vlrpoi        cmzdst
062400010201     C                   eval      cmzfld = vlrfou
062500010201     C                   eval      cmzmbd = vlrhdl
062600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062700021009     C***                if        prmfir = *blanks
062800101007     C                   eval      cmzfla = 'EDIVAB0F'
062900101007     C                   eval      cmzmba = 'EDIVAB0F'
063000021009     C***                else
063100021009     C***                eval      cmzfla = prmfir
063200021009     C***                eval      cmzmba = prmfir
063300021009     C***                endif
063400010201     C                   eval      cmznrr = *zeros
063500010201     C                   move      §ctrokvb      cmznrr
063600021018     C                   eval      cmzlba = vlrfl1
063700010201     C                   call(e)   'TIS711C'
063800010201     C                   parm                    dscmz
063900010201     C                   parm      *blanks       esito
064000010201     C                   if        %error
064100010201     C                             or cmzerr = '1'
064200010201     C                             or esito  = '1'
064300010201     C                   eval      wrkesito = '3'
064400010201     C                   endif
064500010205     C                   endif
064600990920     C*
064700000613     C                   ENDSR
064800000613     C***
064900070411
065000070411     C     *pssr         BEGSR
065100070411     C*
065200070411     C                   if        %open(tivin00r)
065300070411     C                   close     tivin00r
065400070411     C                   endif
065500101007     C                   if        %open(edivabwr)
065600101007     C                   close     edivabwr
065700070411     C                   endif
065800101007     C                   if        %open(edivatwr)
065900101007     C                   close     edivatwr
066000070411     C                   endif
066100070411     C*
066200070411     C* Effettuo la chiamata al CLLE preposto
066300101007     C                   call(e)   'TITVEVTC'
066400070411     C                   parm                    parccm
066500070411     C                   parm                    parmbr
066600070411     C                   parm      '2'           paropz
066700070411     C*
066800070411     C                   eval      wrkesito = '2'
066900070411     C*
067000070411     C                   seton                                        LR
067100070411     C*
067200070411     C                   ENDSR     '*CANCL'
067300070411     C***
067400070411
067500990910
067600000613     C     *inzsr        BEGSR
067700990910     C*
067800990910     C     *entry        plist
067900990920     C                   parm                    tivlrds
068000990921     C                   parm      wrkesito      esito
068100000724     C                   parm                    prmlit
068200000710     C                   parm                    prmfir
068300101007     C*
068400101007     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
068500101007     C                   move(p)   vlrMSG        wNomeFile
068600000613     C*
068700000830     C* CALCOLA LA DATA CORRENTE
068800101007     C                   time                    wn14             14 0
068900101007     C                   movel     wn14          oracor            6 0          *ORA
069000101007     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
069100101007     C                   eval      datcor = %dec(%date() : *ISO)
069200000830     C*
069300000613     C                   ENDSR
069400000613     C***
