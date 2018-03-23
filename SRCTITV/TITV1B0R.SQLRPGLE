000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800070928     Ftitv1b0p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100070928     Ftitv1b0ps O    f  198        PRINTER usropn
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
002600070730     D tisi95ds      e ds
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100990915     D wrkdata         s               d
003200990915     D wrkora          s               t
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400000621     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600070928     D depspe          s             16    INZ(*blanks)
003700070928     D curspe          s             16    INZ(*blanks)
003800010202     D parccm          s              8    INZ(*blanks)
003900010202     D parmbr          s             10    INZ(*blanks)
004000010202     D paropz          s              1    INZ(*blanks)
004100010202     D chkcall         s              1    INZ(*blanks)
004200020725     D tivinds       e ds                  extname(tivin00r) prefix(x_)
004300070530     D Num5_0          s              5  0
004400000830
004500000830     D*------------------
004600000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004700000830     D*------------------
004800000830     D WLBDA8          DS                  INZ
004900000830     D  G08DAT                 1      8  0
005000000830     D  G08INV                 9     16  0
005100000830     D  G08ERR                17     17
005200000830     D  G08TGI                18     22  0
005300000830     D*
005400020725
005500020725     D*------------------
005600020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
005700020725     D*------------------
005800070530     D INPUT_DS        DS                  INZ
005900020725     D  VINDTA                 1   2048
006000020725     D  VINFLG              2049   2049
006100020725     D  VINSPE              2050   2059
006200020725     D  VINRRN              2060   2067  0
006300090112     D*------------------
006400090112     D* DS REPERIMENTO NUMERATORE
006500090112     D*------------------
006600090112     D trul33ds      e ds                  inz
006700090112     D*------------------
006800090112     D* DS ARCHITETTURA
006900090112     D*------------------
007000090112     D kpjba         e ds                  inz
007100990908
007200010201
007300010201
007400071120     C*
007500071120     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007600071120     C
007700071120     C/EXEC SQL
007800071120     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007900071120     C/END-EXEC
008000071120     C*
008100990915     C                   time                    wrkdata
008200990915     C                   time                    wrkora
008300000913     C                   reset                   rrnum
008400990921     C                   reset                   esito
008500990921     C                   reset                   wrkesito
008600000724     C*
008700000724     C* SE OCCORRE SPEDIRE IN FILIALE
008800000724     C                   if        vlrpoi <> *zeros
008900000724     C*
009000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009200000724     C     vlrpoi        chain     azorg01l
009300000724     C                   if        %found
009400000616     C                   movel(p)  CMD(1)        depcmd
009500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
009600000616     C*
009700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
009800011113     C                   Z-ADD     150           LENGH            15 5
009900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010000000616     C                   PARM                    depcmd
010100000616     C                   PARM                    LENGH
010200000724     C*
010300000724     C                   endif
010400000724     C                   endif
010500000616     C*
010600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010700000616     C                   movel(p)  CMD(2)        depcmd
010800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
010900000616     C*
011000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
011100011113     C                   Z-ADD     150           LENGH            15 5
011200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011300000616     C                   PARM                    depcmd
011400000616     C                   PARM                    LENGH
011500000616     C*
011600070928     C                   if        not %open(titv1b0ps)
011700070928     C                   open      titv1b0ps
011800000616     C                   except    testdett
011900000613     C                   endif
012000000613     C*
012100070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
012200000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012300000613     C*
012400010202     C* Effettuo la chiamata al CLLE preposto
012500050128     C                   call(e)   'TITVVTC'
012600010202     C                   parm                    parccm
012700010202     C                   parm                    parmbr
012800010202     C                   parm      '2'           paropz
012900070730     C*
013000070730     C* Effettuo lancio TISI95 solo x chiusura
013100070730     C                   CLEAR                   TISI95DS
013200070730     C                   EVAL      I95TLA = 'C'
013300070730     C                   CALL      'TISI95R'
013400070730     C                   PARM                    TISI95DS
013500010202     C*
013600070928     C                   if        %open(titv1b0ps)
013700000616     C                   except    findett
013800070928     C                   close     titv1b0ps
013900000613     C                   endif
014000000616     C*
014100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
014200011113     C                   Z-ADD     150           LENGH            15 5
014300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014400000616     C                   PARM                    CMD(3)
014500000616     C                   PARM                    LENGH
014600000616     C*
014700000801     C
014800010201     C                   seton                                        LR
014900000613
015000000613     C*--------------------------------------------------------
015100000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
015200000613     C*--------------------------------------------------------
015300000621     C     STPR          BEGSR
015400000613     C*
015500070928     C                   if        not %open(titv1b0p)
015600070928     C                   open      titv1b0p
015700990915     C                   endif
015800990915     C*
015900990915     C                   except    riepilogo
016000990915     C*
016100070928     C                   if        %open(titv1b0p)
016200070928     C                   close     titv1b0p
016300990914     C                   endif
016400990910     C*
016500000613     C                   ENDSR
016600000613     C***
016700990908
016800000801
016900910830     C*--------------------------------------------------------
017000070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
017100910830     C*--------------------------------------------------------
017200070530     C     RWFILE        BEGSR
017300990910     C*
017400990914     C                   if        not %open(tivin00r)
017500990908     C                   open      tivin00r
017600990914     C                   endif
017700021113     C                   if        not %open(fivabwwr)
017800021113     C                   open      fivabwwr
017900990914     C                   endif
018000070530     C*
018100021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
018200020305     C                   exsr      prevat
018300010201     C*
018400010202     C                   if        chkcall = '0'
018500010202     C*
018600021113     C                   if        not %open(fivatwwr)
018700021113     C                   open      fivatwwr
018800010201     C                   endif
018900990910     C*
019000010201     C                   clear                   §CTROKVB          5 0
019100020305     C                   clear                   §CTROKVT          5 0
019200000801     C                   clear                   §CTRMO            5 0
019300000801     C                   clear                   §CTRNO            5 0
019400990910     C*
019500020725     C/EXEC SQL
019600020725     C+ declare C1 cursor for select
019700070928     C+ vindta, vinflg, substr(vindta, 315, 16) as sped, rrn(tivin00r)
019800060223     C+ from tivin00r
019900060223     C+ order by sped
020000020725     C+ for read only
020100020725     C/END-EXEC
020200020725     C
020300020725     C/EXEC SQL
020400020725     C+ open C1
020500020725     C/END-EXEC
020600020725     C
020700020725     C/EXEC SQL
020800070530     C+ Fetch C1 into :INPUT_DS
020900020725     C/END-EXEC
021000020725     C*
021100020725     C                   dow       sqlcod = *zeros
021200990913     C*
021300020725     C                   if        vindta > *blanks
021400000613     C                   add       1             rrnum
021500000801     C*
021600020725     C                   if        vinflg = *blanks
021700020725     C                             or vinflg = '0'
021800020725     C                             or vinflg = '2'
021900000801     C*
022000020725     C                   clear                   x_vinmsg
022100020725     C                   eval      x_vinflg = '1'
022200010305     C*
022300010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
022400070928     C                   EVAL      PiStr=%trim(%subst(vindta:315:16))
022500020305     C                   MOVEL(p)  PiStr         curspe
022600010305     C*
022700010305     C                   if        depspe = *blanks                             => 1° giro
022800010305     C                   eval      depspe = curspe                              => memorizz. spediz
022900020725     C                   clear                   tivinds
023000090112     C                   exsr      repNSP
023100020305     C                   exsr      impvab
023200070928     C   50              exsr      wrivat                                       => carico VAT
023300010305     C                   else
023400020725     C                   if        depspe <> curspe                             => rottura di spediz
023500010305     C                   eval      depspe = curspe                              => memorizz. spediz
023600070928     C                   exsr      wrivab
023700020725     C                   clear                   tivinds
023800090112     C                   exsr      repNSP
023900020305     C                   exsr      impvab
024000070928     C   50              exsr      wrivat                                       => carico VAT
024100020305     C                   else                                                   => x stessa spediz
024200020305     C                   exsr      impvab
024300070928     C   50              exsr      wrivat                                       => carico VAT
024400010305     C                   endif
024500010305     C                   endif
024600010305     C                   endif
024700000905     C*
024800000905     C                   else
024900020725     C                   eval      x_vinflg = '1'
025000000905     C                   endif
025100000905     C*
025200020725     C     VINRRN        chain     tivin000
025300020725     C                   if        %found(tivin00r)
025400020725     C                   eval      y_vinflg = x_vinflg
025500020725     C                   eval      y_vinmsg = x_vinmsg
025600020725     C                   update    tivin000
025700020725     C                   endif
025800020725     C*
025900020725     C/EXEC SQL
026000070530     C+ Fetch C1 into :INPUT_DS
026100020725     C/END-EXEC
026200020725     C*
026300020725     C                   enddo
026400020725     C*
026500020725     C/EXEC SQL
026600020725     C+ close C1
026700020725     C/END-EXEC
026800000905     C*
026900020305     C* Scarico i VAB rimasti "in sospeso"
027000020305     C                   exsr      wrivab
027100010202     C*
027200010202     C                   endif
027300990910
027400990910     C* Se non ci sono record con errori ...
027500000710     C                   if        §ctrno = 0
027600990910     C* ... restituisco esito OK.
027700990921     C                   eval      wrkesito = '0'
027800990910     C                   else
027900010201     C                   if        §ctrokvb > 0
028000990921     C                   eval      wrkesito = '1'
028100000710     C                   else
028200000710     C                   eval      wrkesito = '2'
028300990910     C                   endif
028400000710     C                   endif
028500990910     C*
028600990914     C                   if        %open(tivin00r)
028700990908     C                   close     tivin00r
028800990914     C                   endif
028900021113     C                   if        %open(fivabwwr)
029000021113     C                   close     fivabwwr
029100990914     C                   endif
029200021113     C                   if        %open(fivatwwr)
029300021113     C                   close     fivatwwr
029400010201     C                   endif
029500990910     C*
029600010201     C                   if        §ctrokvb > 0
029700000724     C                             and vlrpoi <> *zeros
029800010202     C                   exsr      invio
029900990920     C                   endif
030000990920     C*
030100910830     C                   ENDSR
030200000613     C***
030300010305
030400010305     C*----------------------------------------------------*
030500020305     C*  SCARICAMENTO BUFFER RECORDS VAB
030600010305     C*----------------------------------------------------*
030700020305     C     WRIVAB        BEGSR
030800070730     C*
030900021113     C                   write     fivab000                                     => scarico il VAB
031000010305     C*
031100010305     C                   ENDSR
031200990920
031300000801     C*----------------------------------------------------*
031400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031500000801     C*----------------------------------------------------*
031600010201     C     INZVAR        BEGSR
031700000801     C*
031800010201     C                   Z-ADD     *zeros        Num5_0
031900020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032000020725     C                   MOVEL     '0'           FlgCAS            1
032100000801     C*
032200000801     C                   ENDSR
032300000801     C*----------------------------------------------------*
032400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
032500000801     C*----------------------------------------------------*
032600020904     C     DEFCAM        BEGSR
032700000801     C*
032800021113     C                   CLEAR                   FIVAB000
032900021113     C                   CLEAR                   FIVAT000
033000070730     C* Imposto i valori di default...
033100071002     C                   EVAL      VABCCM = 0063716
033200071002     C                   EVAL      VATCCM = 0063716
033300071002     C                   EVAL      VABLNP = 006
033400071002     C                   EVAL      VATLNP = 006
033500070928     C                   EVAL      VABCTR = 000
033600070730     C                   EVAL      VABCBO = '1'
033700070928     C                   EVAL      VABCTM = '7Q'
033800070222     C* ... e poi verifico se sono stati passati come parametri
033900070222     C                   IF        vlrppt > *blanks
034000070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
034100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034200070222     C                   EXSR      CHKNUM
034300070222     C                   IF        PiInt=*on
034400070222     C                   Z-ADD     PiVal         VABCCM
034500070222     C                   Z-ADD     PiVal         VATCCM
034600070222     C                   ENDIF
034700070222     C                   ENDIF
034800070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
034900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035000070222     C                   EXSR      CHKNUM
035100070222     C                   IF        PiInt=*on
035200070222     C                   Z-ADD     PiVal         VABLNP
035300070222     C                   Z-ADD     PiVal         VATLNP
035400070222     C                   ENDIF
035500070222     C                   ENDIF
035600070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
035700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
035800070222     C                   EXSR      CHKNUM
035900070222     C                   IF        PiInt=*on
036000070222     C                   Z-ADD     PiVal         VABCTR
036100070222     C                   ENDIF
036200070928     C                   ENDIF
036300070928     C                   IF        %subst(vlrppt:14:1) = 'C'
036400070928     C                   SETON                                        50
036500070928     C                   ELSE
036600070928     C                   SETOFF                                       50
036700070222     C                   ENDIF
036800070222     C                   ENDIF
036900000801     C*
037000000801     C                   ENDSR
037100090112     C*----------------------------------------------------*
037200090112     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
037300090112     C*----------------------------------------------------*
037400090112     C     REPNSP        BEGSR
037500090112     C*
037600090112     C                   EXSR      INZVAR
037700090112     C                   EXSR      DEFCAM
037800090112     C*
037900090112     C* NSP => Stacco un numeratore da AZNUM
038000090112     C                   clear                   TRUL33DS
038100090112     C                   eval      I33OPE = *zeros
038200090112     C                   eval      I33CNU = 302
038300090112     C                   eval      I33NUM = 1
038400090112     C                   movel     TRUL33DS      KPJBU
038500090112     C                   call      'TRUL33R'
038600090112     C                   parm                    KPJBA
038700090112     C                   movel     KPJBU         TRUL33DS
038800090112     C                   if        O33ERR = *zeros
038900090112     C                   z-add     O33NRF        VABNSP
039000090112     C                   z-add     O33NRF        VATNSP
039100090112     C                   else
039200090112     C                   SETON                                        31
039300090112     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039400090112     C                             + ' ' + 'VABNSP VATNSP'
039500090112     C                   endif
039600090112     C*
039700090112     C                   ENDSR
039800000801     C*----------------------------------------------------*
039900021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
040000000801     C*----------------------------------------------------*
040100010201     C     IMPVAB        BEGSR
040200070530     C*
040300070530     C                   SETOFF                                       3132
040400070928     C*
040500070928     C                   MOVE(P)   vlrpoi        VABFGS
040600070928     C                   MOVE(P)   vlrpoi        VATFGS
040700070928     C*
040800070928     C                   MOVEL     datcor        VABAAS
040900070928     C                   MOVEL     datcor        VATAAS
041000070928     C                   MOVE      datcor        VABMGS
041100070928     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
041200070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
041300070928     C     '@':'A'       XLATE     VABRSD        VABRSD
041400070928     C* ==
041500070928     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
041600070928     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
041700070928     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
041800070928     C                   EVAL      VABRMA=%trim(%subst(vindta:315:16))
041900070928     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
042000070928     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
042100070928     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
042200070928     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:80-35))
042300070928     C                   IF        %trim(%subst(vindta:12:1)) = 'D'
042400070928     C                   EVAL      VABTSP='E'
042500070928     C                   ENDIF
042600071003     C* CAD
042700070928     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
042800070928     C                   EXSR      CHKNUM
042900070928     C                   IF        PiInt=*on
043000070928     C                   Z-ADD     PiVal         Num5_0
043100070928     C                   MOVEL     Num5_0        VABCAD
043200070928     C                   ELSE
043300070928     C                   SETON                                        32
043400070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043500070928     C                             + ' ' + 'VABCAD'
043600070928     C                   ENDIF
043700090112     C* RMN
043800090112     C                   EVAL      PiStr=%trim(%subst(vindta:190:7))
043900090112     C                   EXSR      CHKNUM
044000090112     C                   IF        PiInt=*on
044100090112     C                   Z-ADD     PiVal         VABRMN
044200090112     C***                Z-ADD     PiVal         VABNSP
044300090112     C***                Z-ADD     PiVal         VATNSP
044400090112     C                   ELSE
044500090112     C                   SETON                                        31
044600070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044700090112     C***                          + ' ' + 'VABNSP VATNSP VABRMN'
044800090112     C                             + ' ' + 'VABRMN'
044900070928     C                   ENDIF
045000071003     C* NCL
045100070928     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
045200070928     C                   EXSR      CHKNUM
045300070928     C                   IF        PiInt=*on
045400070928     C                   Z-ADD     PiVal         VABNCL
045500070928     C                   ELSE
045600070928     C                   SETON                                        32
045700070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045800070928     C                             + ' ' + 'VABNCL'
045900070928     C                   ENDIF
046000071003     C* PKB
046100070928     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
046200070928     C                   EXSR      CHKNUM
046300070928     C                   IF        PiNum=*on
046400070928     C                   Z-ADD     PiVal         VABPKB
046500070928     C                   ELSE
046600070928     C                   SETON                                        32
046700070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046800070928     C                             + ' ' + 'VABPKB'
046900070928     C                   ENDIF
047000071003     C* CAS
047100070928     C                   IF        %trim(%subst(vindta:280:12))<>*blanks AND
047200070928     C                             %trim(%subst(vindta:280:12))<>*zeros  AND
047300070928     C                             %trim(%subst(vindta:280:12))<>'0,00'  AND
047400070928     C                             %trim(%subst(vindta:280:12))<>'000000000,00'
047500070928     C                   MOVEL     '1'           FlgCAS
047600070928     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
047700070928     C                   EXSR      CHKNUM
047800070928     C                   IF        PiNum=*on
047900071120     C                   EVAL      VABTIC = 'BM'
048000070928     C                   Z-ADD     PiVal         VABCAS
048100070928     C                   ELSE
048200070928     C                   SETON                                        32
048300070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048400070928     C                             + ' ' + 'VABCAS'
048500070928     C                   ENDIF
048600070928     C                   ENDIF
048700071003     C* IAS
048800071003     C                   IF        %trim(%subst(vindta:295:12))<>*blanks AND
048900071003     C                             %trim(%subst(vindta:295:12))<>*zeros  AND
049000071003     C                             %trim(%subst(vindta:295:12))<>'0,00'  AND
049100071003     C                             %trim(%subst(vindta:295:12))<>'000000000,00'
049200071003     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
049300071003     C                   EXSR      CHKNUM
049400071003     C                   IF        PiNum=*on
049500071003     C                   Z-ADD     PiVal         VABIAS
049600071003     C                   MOVEL     'EUR'         VABVAS
049700071003     C                   ELSE
049800071003     C                   SETON                                        32
049900071003     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050000071003     C                             + ' ' + 'VABIAS'
050100071003     C                   ENDIF
050200071003     C                   ENDIF
050300070730     C*
050400070730     C* Considerazioni finali su CBO/CAS
050500070730     C                   IF        FlgCAS = '1'
050600070730     C                   IF        VABCBO = '1'
050700070730     C                   EVAL      VABCBO = '4'
050800070730     C                   ENDIF
050900070730     C                   IF        VABCBO = '2'
051000070730     C                   EVAL      VABCBO = '6'
051100070730     C                   ENDIF
051200070730     C                   ENDIF
051300020305     C*
051400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
051500011113     C                   EXSR      CHKIMPDIV
051600010202     C*
051700000801     C* Ebbene...
051800000801     C                   ADD       1             §CTRMO
051900070530     C                   IF        *in31 <> *zeros OR
052000070530     C                             *in32 <> *zeros
052100000801     C                   ADD       1             §CTRNO
052200000801     C                   EVAL      recko = vindta
052300000801     C                   EXCEPT    rigadett
052400020725     C                   EVAL      x_vinflg = '2'
052500000801     C                   ELSE
052600010201     C                   ADD       1             §CTROKVB
052700000801     C                   ENDIF
052800000801     C*
052900000801     C                   ENDSR
053000010201     C*----------------------------------------------------*
053100021113     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
053200010201     C*----------------------------------------------------*
053300020305     C     WRIVAT        BEGSR
053400010201     C*
053500021113     C* Valorizzo l buffer di scrittura del FIVAT
053600070928     C* - TIPO RECORD "B"
053700070928     C                   if        %subst(vindta:92:15) <> *blanks
053800070928     C                   eval      VATTRC = 'B'
053900070928     C                   eval      VATNOT = %trim(%subst(vindta:92:15))
054000021113     C                   write     FIVAT000
054100020725     C                   endif
054200070928     C* - TIPO RECORD "E"
054300070928     C                   if        %subst(vindta:331:26) <> *blanks
054400070928     C                   eval      VATTRC = 'E'
054500070928     C                   eval      VATNOT = %trim(%subst(vindta:331:26))
054600070928     C                   write     FIVAT000
054700070928     C                   endif
054800010201     C*
054900010201     C                   ENDSR
055000020904
055100020904
055200010202     C*----------------------------------------------------*
055300021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
055400010202     C*----------------------------------------------------*
055500020305     C     PREVAT        BEGSR
055600010202     C*
055700021113     C* Compongo il nome del membro da dare al FIVATWWR
055800010202     C                   eval      parmbr = vlrhdl
055900010202     C                   movel     'M'           parmbr
056000070530     C                   eval      parccm = %subst(vlrKSC:2:7)
056100010202     C                   eval      paropz = '1'
056200010202     C* Effettuo la chiamata al CLLE preposto
056300050128     C                   call(e)   'TITVVTC'
056400010202     C                   parm                    parccm
056500010202     C                   parm                    parmbr
056600010202     C                   parm                    paropz
056700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
056800010202     C                   if        %error
056900010202     C                   movel     '1'           chkcall
057000010202     C                   else
057100010202     C                   movel     '0'           chkcall
057200010202     C                   endif
057300010202     C*
057400010202     C                   ENDSR
057500000801     C*----------------------------------------------------*
057600000801     C*  CONTROLLO NUMERICITA' CAMPI
057700000801     C*----------------------------------------------------*
057800000801     C     CHKNUM        BEGSR
057900000801     C*
058000000801     C                   call(e)   'ISNUMERIC'
058100000801     C                   PARM                    PiStr            30
058200070928     C                   PARM      ','           PiDecChr          1
058300000801     C                   PARM      *ZEROS        PiVal            30 9
058400000801     C                   PARM      '0'           PiInt             1
058500000801     C                   PARM      '0'           PiNum             1
058600000801     C                   IF        %error
058700000801     C                   EVAL      PiInt=*off
058800000801     C                   ENDIF
058900000801     C*
059000000801     C                   ENDSR
059100000801     C***
059200000801
059300011113
059400011113     C*----------------------------------------------------*
059500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
059600011113     C*----------------------------------------------------*
059700011113     C     CHKIMPDIV     BEGSR
059800011113     C*
059900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
060000011113     C                   Z-ADD     *zeros        wrkDec            9 9
060100011113     C*
060200011113     C* Come prima cosa effettuo considerazioni sulla divisa
060300011113     C                   IF        vabIAS > *zeros
060400011113     C                   IF        vabVAS <> 'EUR'
060500011113     C                   EVAL      vabVAS =  'ITL'
060600011113     C                   ENDIF
060700011113     C                   ENDIF
060800011113     C*
060900011113     C                   IF        vabCAS > *zeros
061000011113     C                   IF        vabVCA <> 'EUR'
061100011113     C                   EVAL      vabVCA =  'ITL'
061200011113     C                   ENDIF
061300011113     C                   ENDIF
061400011113     C*
061500011113     C                   IF        vabVMD > *zeros
061600020305     C                   IF        vabVAD <> 'EUR'
061700011113     C                   EVAL      vabVAD =  'ITL'
061800011113     C                   ENDIF
061900011113     C                   ENDIF
062000011113     C*
062100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
062200011113     C                   Z-ADD     vabIAS        wrkDec
062300011113     C                   IF        wrkDec > *zeros
062400011113     C                   IF        vabVAS = 'ITL'
062500011113     C                   EVAL      vabIAS = *zeros
062600011113     C                   ENDIF
062700011113     C                   ENDIF
062800011113     C*
062900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
063000011113     C                   Z-ADD     vabCAS        wrkDec
063100011113     C                   IF        wrkDec > *zeros
063200011113     C                   IF        vabVCA = 'ITL'
063300011113     C                   EVAL      vabCAS = *zeros
063400011113     C                   ENDIF
063500011113     C                   ENDIF
063600011113     C*
063700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
063800011113     C                   Z-ADD     vabVMD        wrkDec
063900011113     C                   IF        wrkDec > *zeros
064000011113     C                   IF        vabVAD = 'ITL'
064100011113     C                   EVAL      vabVMD = *zeros
064200011113     C                   ENDIF
064300011113     C                   ENDIF
064400011113     C*
064500011113     C                   ENDSR
064600011113     C***
064700011113
064800011113
064900000801
065000000801
065100990920      /TITLE Invio dei dati al punto operativo.
065200010202     C     invio         BEGSR
065300990920     C*
065400021113     C* 1° invio FIVAT
065500010201     C                   reset                   dscmz
065600010201     C                   move      vlrpoi        cmzdst
065700021113     C                   eval      cmzfld = 'FIVATWWR'
065800010201     C                   eval      cmzmbd = vlrhdl
065900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066000021009     C***                if        prmfir = *blanks
066100021113     C                   eval      cmzfla = 'FIVAT00F'
066200021113     C                   eval      cmzmba = 'FIVAT00F'
066300021009     C***                else
066400021009     C***                eval      cmzfla = prmfir
066500021009     C***                eval      cmzmba = prmfir
066600021009     C***                endif
066700010201     C                   eval      cmznrr = *zeros
066800020305     C                   move      §ctrokvt      cmznrr
066900021018     C                   eval      cmzlba = vlrfl1
067000010201     C                   call(e)   'TIS711C'
067100010201     C                   parm                    dscmz
067200010201     C                   parm      *blanks       esito
067300010205     C                   if        %error
067400010205     C                             or cmzerr = '1'
067500010205     C                             or esito  = '1'
067600010205     C                   eval      wrkesito = '3'
067700010205     C                   else
067800010201     C*
067900021113     C* 2° invio FIVAB
068000010201     C                   reset                   dscmz
068100010201     C                   move      vlrpoi        cmzdst
068200010201     C                   eval      cmzfld = vlrfou
068300010201     C                   eval      cmzmbd = vlrhdl
068400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068500021009     C***                if        prmfir = *blanks
068600021113     C                   eval      cmzfla = 'FIVAB00F'
068700021113     C                   eval      cmzmba = 'FIVAB00F'
068800021009     C***                else
068900021009     C***                eval      cmzfla = prmfir
069000021009     C***                eval      cmzmba = prmfir
069100021009     C***                endif
069200010201     C                   eval      cmznrr = *zeros
069300010201     C                   move      §ctrokvb      cmznrr
069400021018     C                   eval      cmzlba = vlrfl1
069500010201     C                   call(e)   'TIS711C'
069600010201     C                   parm                    dscmz
069700010201     C                   parm      *blanks       esito
069800010201     C                   if        %error
069900010201     C                             or cmzerr = '1'
070000010201     C                             or esito  = '1'
070100010201     C                   eval      wrkesito = '3'
070200010201     C                   endif
070300010205     C                   endif
070400990920     C*
070500000613     C                   ENDSR
070600000613     C***
070700070411
070800070411     C     *pssr         BEGSR
070900070411     C*
071000070411     C                   if        %open(tivin00r)
071100070411     C                   close     tivin00r
071200070411     C                   endif
071300070411     C                   if        %open(fivabwwr)
071400070411     C                   close     fivabwwr
071500070411     C                   endif
071600070411     C                   if        %open(fivatwwr)
071700070411     C                   close     fivatwwr
071800070411     C                   endif
071900070411     C*
072000070411     C* Effettuo la chiamata al CLLE preposto
072100070411     C                   call(e)   'TITVVTC'
072200070411     C                   parm                    parccm
072300070411     C                   parm                    parmbr
072400070411     C                   parm      '2'           paropz
072500070411     C*
072600070411     C                   eval      wrkesito = '2'
072700070411     C*
072800070411     C                   seton                                        LR
072900070411     C*
073000070411     C                   ENDSR     '*CANCL'
073100070411     C***
073200070411
073300990910
073400000613     C     *inzsr        BEGSR
073500990910     C*
073600990910     C     *entry        plist
073700990920     C                   parm                    tivlrds
073800990921     C                   parm      wrkesito      esito
073900000724     C                   parm                    prmlit
074000000710     C                   parm                    prmfir
074100000613     C*
074200000830     C* CALCOLA LA DATA CORRENTE
074300000830     C                   time                    wn14             14 0
074400000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
074500000830     C                   z-add     wn8           g08dat
074600000830     C                   z-add     *zeros        g08inv
074700000830     C                   movel     '0'           g08err
074800000830     C                   call      'XSRDA8'
074900000830     C                   parm                    wlbda8
075000000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
075100000830     C*
075200000613     C                   ENDSR
075300000613     C***
075400990908
075500070928     Otitv1b0p  E            riepilogo         2
075600990915     O                                              'Upload via Internet'
075700990915     O                                           +1 'Traduzione TIVIN00R -'
075800021113     O                                           +1 'FIVABWWR/FIVATWWR'
075900010201     O                                           +1 'Report testate bolle'
076000990915     O          E            riepilogo   2
076100990915     O                       wrkdata
076200990915     O                       wrkora              +1
076300990915     O                       procname            +1
076400990915     O          E            riepilogo   2
076500990915     O                                              'Cliente..................:'
076600990915     O                       VABCCM        z     +1
076700990915     O          E            riepilogo   2
076800990920     O                                              'Riferimento Strategi.....:'
076900990920     O                       vlrhdl              +1
077000990915     O          E            riepilogo   2
077100990915     O                                              'Giusti...................:'
077200010201     O                       §CTROKVB      2   +  1
077300990915     O          E            riepilogo   2
077400990915     O                                              'Sbagliati e corretti.....:'
077500971022     O                       §CTRMO        2   +  1
077600990915     O          E            riepilogo   2
077700990915     O                                              'Sbagliati e scartati.....:'
077800971022     O                       §CTRNO        2   +  1
077900000613
078000070928     Otitv1b0ps E            testdett          2
078100000613     O                                              'Upload via Internet'
078200000613     O                                           +1 'Traduzione TIVIN00R -'
078300021113     O                                           +1 'FIVABWWR/FIVATWWR'
078400010201     O                                           +1 'Report testate bolle'
078500000616     O          E            testdett    3
078600000613     O                                           +2 'N° rec'
078700000613     O                                           +3 'Anteprima contenuto'
078800000616     O          E            rigadett    2
078900000613     O                       rrnum               +2
079000000621     O                       recko               +3
079100000616     O          E            findett     2
079200000613     O                       wrkdata
079300000613     O                       wrkora              +1
079400000613     O                       procname            +1
079500000616     O          E            findett     2
079600000613     O                                              'Cliente..................:'
079700000613     O                       VABCCM        z     +1
079800000616     O          E            findett     2
079900000613     O                                              'Riferimento Strategi.....:'
080000000613     O                       vlrhdl              +1
080100000616     O          E            findett     2
080200000613     O                                              'Giusti...................:'
080300010201     O                       §CTROKVB      2   +  1
080400000616     O          E            findett     2
080500000613     O                                              'Sbagliati e corretti.....:'
080600000613     O                       §CTRMO        2   +  1
080700000616     O          E            findett     2
080800000613     O                                              'Sbagliati e scartati.....:'
080900000613     O                       §CTRNO        2   +  1
081000000512** CMD - COMANDI CL
081100070928OVRPRTF FILE(TITV1B0P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
081200070928OVRPRTF FILE(TITV1B0PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
081300070928DLTOVR FILE(TITV1B0P TITV1B0PS) LVL(*)
081400000512
081500000512
