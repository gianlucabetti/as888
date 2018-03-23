000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800090407     Ftitv1o6p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100090407     Ftitv1o6ps O    f  198        PRINTER usropn
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
003600090203     D depspe          s             10    INZ(*blanks)
003700090203     D curspe          s             10    INZ(*blanks)
003800010202     D parccm          s              8    INZ(*blanks)
003900010202     D parmbr          s             10    INZ(*blanks)
004000010202     D paropz          s              1    INZ(*blanks)
004100010202     D chkcall         s              1    INZ(*blanks)
004200080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
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
006300020725     D*
006400080923     D*------------------
006500080923     D* DS REPERIMENTO NUMERATORE
006600080923     D*------------------
006700080923     D trul33ds      e ds                  inz
006800080923     D*------------------
006900080923     D* DS ARCHITETTURA
007000080923     D*------------------
007100080923     D kpjba         e ds                  inz
007200080923     D*------------------
007300081113
007400081113     D*------------------
007500081113     D* LINKING A DEFINIZIONI ESTERNE
007600081113     D*------------------
007700081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007800090407     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007900081113
008000990908
008100010201
008200010201
008300080117     C*
008400080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008500080117     C
008600080117     C/EXEC SQL
008700080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008800080117     C/END-EXEC
008900080117     C
009000990915     C                   time                    wrkdata
009100990915     C                   time                    wrkora
009200000913     C                   reset                   rrnum
009300990921     C                   reset                   esito
009400990921     C                   reset                   wrkesito
009500000724     C*
009600000724     C* SE OCCORRE SPEDIRE IN FILIALE
009700000724     C                   if        vlrpoi <> *zeros
009800000724     C*
009900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
010000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010100000724     C     vlrpoi        chain     azorg01l
010200000724     C                   if        %found
010300000616     C                   movel(p)  CMD(1)        depcmd
010400020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010500000616     C*
010600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010700011113     C                   Z-ADD     150           LENGH            15 5
010800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010900000616     C                   PARM                    depcmd
011000000616     C                   PARM                    LENGH
011100000724     C*
011200000724     C                   endif
011300000724     C                   endif
011400000616     C*
011500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011600000616     C                   movel(p)  CMD(2)        depcmd
011700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011800000616     C*
011900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012000011113     C                   Z-ADD     150           LENGH            15 5
012100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012200000616     C                   PARM                    depcmd
012300000616     C                   PARM                    LENGH
012400000616     C*
012500090407     C                   if        not %open(titv1o6ps)
012600090407     C                   open      titv1o6ps
012700000616     C                   except    testdett
012800000613     C                   endif
012900000613     C*
013000070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
013100000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
013200000613     C*
013300010202     C* Effettuo la chiamata al CLLE preposto
013400050128     C                   call(e)   'TITVVTC'
013500010202     C                   parm                    parccm
013600010202     C                   parm                    parmbr
013700010202     C                   parm      '2'           paropz
013800070730     C*
013900070730     C* Effettuo lancio TISI95 solo x chiusura
014000070730     C                   CLEAR                   TISI95DS
014100070730     C                   EVAL      I95TLA = 'C'
014200070730     C                   CALL      'TISI95R'
014300070730     C                   PARM                    TISI95DS
014400010202     C*
014500090407     C                   if        %open(titv1o6ps)
014600000616     C                   except    findett
014700090407     C                   close     titv1o6ps
014800000613     C                   endif
014900000616     C*
015000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015100011113     C                   Z-ADD     150           LENGH            15 5
015200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015300000616     C                   PARM                    CMD(3)
015400000616     C                   PARM                    LENGH
015500000616     C*
015600000801     C
015700010201     C                   seton                                        LR
015800000613
015900000613     C*--------------------------------------------------------
016000000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
016100000613     C*--------------------------------------------------------
016200000621     C     STPR          BEGSR
016300000613     C*
016400090407     C                   if        not %open(titv1o6p)
016500090407     C                   open      titv1o6p
016600990915     C                   endif
016700990915     C*
016800990915     C                   except    riepilogo
016900990915     C*
017000090407     C                   if        %open(titv1o6p)
017100090407     C                   close     titv1o6p
017200990914     C                   endif
017300990910     C*
017400000613     C                   ENDSR
017500000613     C***
017600990908
017700000801
017800910830     C*--------------------------------------------------------
017900070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
018000910830     C*--------------------------------------------------------
018100070530     C     RWFILE        BEGSR
018200990910     C*
018300990914     C                   if        not %open(tivin00r)
018400990908     C                   open      tivin00r
018500990914     C                   endif
018600021113     C                   if        not %open(fivabwwr)
018700021113     C                   open      fivabwwr
018800990914     C                   endif
018900070530     C*
019000021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
019100020305     C                   exsr      prevat
019200010201     C*
019300010202     C                   if        chkcall = '0'
019400010202     C*
019500021113     C                   if        not %open(fivatwwr)
019600021113     C                   open      fivatwwr
019700010201     C                   endif
019800080117     C*
019900080117     C                   EXSR      INZVAR
020000080117     C                   EXSR      DEFCAM
020100990910     C*
020200010201     C                   clear                   §CTROKVB          5 0
020300020305     C                   clear                   §CTROKVT          5 0
020400000801     C                   clear                   §CTRMO            5 0
020500000801     C                   clear                   §CTRNO            5 0
020600990910     C*
020700020725     C/EXEC SQL
020800020725     C+ declare C1 cursor for select
020900090203     C+ vindta, vinflg, substr(vindta, 187, 10) as sped, rrn(tivin00r)
021000060223     C+ from tivin00r
021100060223     C+ order by sped
021200020725     C+ for read only
021300020725     C/END-EXEC
021400020725     C
021500020725     C/EXEC SQL
021600020725     C+ open C1
021700020725     C/END-EXEC
021800020725     C
021900020725     C/EXEC SQL
022000070530     C+ Fetch C1 into :INPUT_DS
022100020725     C/END-EXEC
022200020725     C*
022300020725     C                   dow       sqlcod = *zeros
022400990913     C*
022500020725     C                   if        vindta > *blanks
022600000613     C                   add       1             rrnum
022700000801     C*
022800020725     C                   if        vinflg = *blanks
022900020725     C                             or vinflg = '0'
023000020725     C                             or vinflg = '2'
023100000801     C*
023200020725     C                   clear                   x_vinmsg
023300020725     C                   eval      x_vinflg = '1'
023400010305     C*
023500010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
023600090203     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
023700020305     C                   MOVEL(p)  PiStr         curspe
023800010305     C*
023900071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
024000080923     C                   if        *in50 = *off
024100071003     C                   exsr      impvab
024200071003     C                   exsr      wrivab
024300071003     C                   exsr      wrivat_b                                     => carico VAT
024400071003     C                   else
024500080923     C*
024600071009     C                   if        wDISK = 'D'
024700071009     C                   exsr      impvab
024800071009     C                   exsr      wrivab
024900071009     C                   exsr      wrivat_b                                     => carico VAT
025000071009     C                   exsr      wrivat_e                                     => carico VAT
025100071009     C                   else
025200071009     C*
025300010305     C                   if        depspe = *blanks                             => 1° giro
025400010305     C                   eval      depspe = curspe                              => memorizz. spediz
025500080117     C                   clear                   tivinds
025600020305     C                   exsr      impvab
025700071003     C                   exsr      wrivat_b                                     => carico VAT
025800071003     C   50              exsr      wrivat_e                                     => carico VAT
025900010305     C                   else
026000020725     C                   if        depspe <> curspe                             => rottura di spediz
026100010305     C                   eval      depspe = curspe                              => memorizz. spediz
026200070928     C                   exsr      wrivab
026300080117     C                   clear                   tivinds
026400020305     C                   exsr      impvab
026500071003     C                   exsr      wrivat_b                                     => carico VAT
026600071003     C   50              exsr      wrivat_e                                     => carico VAT
026700020305     C                   else                                                   => x stessa spediz
026800090204     C***                exsr      impvab
026900071003     C                   exsr      wrivat_b                                     => carico VAT
027000071003     C   50              exsr      wrivat_e                                     => carico VAT
027100010305     C                   endif
027200010305     C                   endif
027300010305     C                   endif
027400071003     C                   endif
027500071009     C                   endif
027600000905     C*
027700000905     C                   else
027800020725     C                   eval      x_vinflg = '1'
027900000905     C                   endif
028000000905     C*
028100020725     C     VINRRN        chain     tivin000
028200020725     C                   if        %found(tivin00r)
028300020725     C                   eval      y_vinflg = x_vinflg
028400020725     C                   eval      y_vinmsg = x_vinmsg
028500020725     C                   update    tivin000
028600020725     C                   endif
028700020725     C*
028800020725     C/EXEC SQL
028900070530     C+ Fetch C1 into :INPUT_DS
029000020725     C/END-EXEC
029100020725     C*
029200020725     C                   enddo
029300020725     C*
029400020725     C/EXEC SQL
029500020725     C+ close C1
029600020725     C/END-EXEC
029700000905     C*
029800020305     C* Scarico i VAB rimasti "in sospeso"
029900071009     C                   if        wDISK = 'C'
030000071009     C                   exsr      wrivab
030100071009     C                   endif
030200010202     C*
030300010202     C                   endif
030400990910
030500990910     C* Se non ci sono record con errori ...
030600000710     C                   if        §ctrno = 0
030700990910     C* ... restituisco esito OK.
030800990921     C                   eval      wrkesito = '0'
030900990910     C                   else
031000010201     C                   if        §ctrokvb > 0
031100990921     C                   eval      wrkesito = '1'
031200000710     C                   else
031300000710     C                   eval      wrkesito = '2'
031400990910     C                   endif
031500000710     C                   endif
031600990910     C*
031700990914     C                   if        %open(tivin00r)
031800990908     C                   close     tivin00r
031900990914     C                   endif
032000021113     C                   if        %open(fivabwwr)
032100021113     C                   close     fivabwwr
032200990914     C                   endif
032300021113     C                   if        %open(fivatwwr)
032400021113     C                   close     fivatwwr
032500010201     C                   endif
032600990910     C*
032700010201     C                   if        §ctrokvb > 0
032800000724     C                             and vlrpoi <> *zeros
032900010202     C                   exsr      invio
033000990920     C                   endif
033100990920     C*
033200910830     C                   ENDSR
033300000613     C***
033400010305
033500010305     C*----------------------------------------------------*
033600020305     C*  SCARICAMENTO BUFFER RECORDS VAB
033700010305     C*----------------------------------------------------*
033800020305     C     WRIVAB        BEGSR
033900070730     C*
034000071003     C* Considerazioni finali
034100071003     C                   if        VABRMA = *blanks
034200071003     C                   movel(P)  VABRMN        VABRMA
034300071003     C                   endif
034400071003     C*
034500021113     C                   write     fivab000                                     => scarico il VAB
034600010305     C*
034700010305     C                   ENDSR
034800990920
034900000801     C*----------------------------------------------------*
035000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
035100000801     C*----------------------------------------------------*
035200010201     C     INZVAR        BEGSR
035300000801     C*
035400010201     C                   Z-ADD     *zeros        Num5_0
035500020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
035600020725     C                   MOVEL     '0'           FlgCAS            1
035700000801     C*
035800000801     C                   ENDSR
035900000801     C*----------------------------------------------------*
036000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
036100000801     C*----------------------------------------------------*
036200020904     C     DEFCAM        BEGSR
036300080923     C*
036400080923     C                   SETOFF                                       5051
036500000801     C*
036600021113     C                   CLEAR                   FIVAB000
036700021113     C                   CLEAR                   FIVAT000
036800070730     C* Imposto i valori di default...
036900090407     C                   EVAL      VABCCM = 0061248
037000090407     C                   EVAL      VATCCM = 0061248
037100090407     C                   EVAL      VABLNP = 006
037200090407     C                   EVAL      VATLNP = 006
037300090407     C                   EVAL      VABCTR = 300
037400070730     C                   EVAL      VABCBO = '1'
037500070222     C* ... e poi verifico se sono stati passati come parametri
037600070222     C                   IF        vlrppt > *blanks
037700070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
037800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
037900070222     C                   EXSR      CHKNUM
038000070222     C                   IF        PiInt=*on
038100070222     C                   Z-ADD     PiVal         VABCCM
038200070222     C                   Z-ADD     PiVal         VATCCM
038300070222     C                   ENDIF
038400070222     C                   ENDIF
038500070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
038600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
038700070222     C                   EXSR      CHKNUM
038800070222     C                   IF        PiInt=*on
038900070222     C                   Z-ADD     PiVal         VABLNP
039000070222     C                   Z-ADD     PiVal         VATLNP
039100070222     C                   ENDIF
039200070222     C                   ENDIF
039300070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
039400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
039500070222     C                   EXSR      CHKNUM
039600070222     C                   IF        PiInt=*on
039700070222     C                   Z-ADD     PiVal         VABCTR
039800070222     C                   ENDIF
039900070928     C                   ENDIF
040000071009     C                   MOVEL     *blanks       wDISK             1
040100071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
040200071009     C                   IF        wDISK <> *blanks
040300070928     C                   SETON                                        50
040400070222     C                   ENDIF
040500080923     C                   IF        %subst(vlrppt:15:1) = 'S'
040600080923     C                   SETON                                        51
040700080923     C                   ENDIF
040800070222     C                   ENDIF
040900071009     C*
041000071009     C   50              EVAL      VABCTM = '7Q'
041100000801     C*
041200000801     C                   ENDSR
041300000801     C*----------------------------------------------------*
041400021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
041500000801     C*----------------------------------------------------*
041600010201     C     IMPVAB        BEGSR
041700070530     C*
041800070530     C                   SETOFF                                       3132
041900070928     C*
042000070928     C                   EXSR      INZVAR
042100070928     C                   EXSR      DEFCAM
042200070928     C*
042300070928     C                   MOVE(P)   vlrpoi        VABFGS
042400070928     C                   MOVE(P)   vlrpoi        VATFGS
042500070928     C*
042600070928     C                   MOVEL     datcor        VABAAS
042700070928     C                   MOVEL     datcor        VATAAS
042800070928     C                   MOVE      datcor        VABMGS
042900070928     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
043000070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
043100070928     C     '@':'A'       XLATE     VABRSD        VABRSD
043200070928     C* ==
043300070928     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
043400070928     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
043500070928     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
043600081218     C***                EVAL      VABRMA=%trim(%subst(vindta:315:16))
043700090203     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
043800070928     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
043900080226     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
044000070928     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
044100070928     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
044200070928     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:80-35))
044300090407     C                   IF        %trim(%subst(vindta:390:1)) = 'F'
044400080226     C                   EVAL      VABCBO='1'
044500080226     C                   ENDIF
044600090407     C                   IF        %trim(%subst(vindta:390:1)) = 'A'
044700080226     C                   EVAL      VABCBO='2'
044800080226     C                   ENDIF
044900080226     C                   IF        %trim(%subst(vindta:12:1)) = 'D'
045000080226     C                   EVAL      VABTSP='E'
045100080226     C                   ENDIF
045200080226     C                   IF        %trim(%subst(vindta:16:1)) = '1'
045300080226     C                   EVAL      VABFFD='S'
045400080226     C                   ENDIF
045500081113     C                   IF        %trim(%subst(vindta:16:1)) = '2'
045600080328     C                   EVAL      VABTC1='A'
045700080226     C                   ENDIF
045800071003     C* CAD
045900070928     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
046000070928     C                   EXSR      CHKNUM
046100070928     C                   IF        PiInt=*on
046200070928     C                   Z-ADD     PiVal         Num5_0
046300070928     C                   MOVEL     Num5_0        VABCAD
046400070928     C                   ELSE
046500070928     C                   SETON                                        32
046600070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046700070928     C                             + ' ' + 'VABCAD'
046800070928     C                   ENDIF
046900080923     C* RMN
047000090407     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
047100080923     C                   EXSR      CHKNUM
047200080923     C                   IF        PiInt=*on
047300080923     C                   Z-ADD     PiVal         VABRMN
047400080923     C                   ELSE
047500080923     C                   SETON                                        32
047600080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047700080923     C                             + ' ' + 'VABRMN'
047800080923     C                   ENDIF
047900080923     C* NSP
048000080923     C                   IF        *in51 = *on
048100080923     C* NSP => Stacco un numeratore da AZNUM
048200080923     C                   clear                   TRUL33DS
048300080923     C                   eval      I33OPE = *zeros
048400080923     C                   eval      I33CNU = 302
048500080923     C                   eval      I33NUM = 1
048600080923     C                   movel     TRUL33DS      KPJBU
048700080923     C                   call      'TRUL33R'
048800080923     C                   parm                    KPJBA
048900080923     C                   movel     KPJBU         TRUL33DS
049000080923     C                   if        O33ERR = *zeros
049100080923     C                   z-add     O33NRF        VABNSP
049200080923     C                   z-add     O33NRF        VATNSP
049300080923     C                   else
049400080923     C                   SETON                                        31
049500080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049600080923     C                             + ' ' + 'VABNSP VATNSP'
049700080923     C                   endif
049800080923     C                   ELSE
049900090407     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
050000080923     C                   EXSR      CHKNUM
050100080923     C                   IF        PiInt=*on
050200080923     C                   Z-ADD     PiVal         VABNSP
050300080923     C                   Z-ADD     PiVal         VATNSP
050400080923     C                   ELSE
050500080923     C                   SETON                                        31
050600080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050700080923     C                             + ' ' + 'VABNSP VATNSP'
050800080923     C                   ENDIF
050900080923     C                   ENDIF
051000071003     C* NCL
051100070928     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
051200070928     C                   EXSR      CHKNUM
051300070928     C                   IF        PiInt=*on
051400070928     C                   Z-ADD     PiVal         VABNCL
051500070928     C                   ELSE
051600070928     C                   SETON                                        32
051700070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051800070928     C                             + ' ' + 'VABNCL'
051900070928     C                   ENDIF
052000071003     C* PKB
052100070928     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
052200070928     C                   EXSR      CHKNUM
052300070928     C                   IF        PiNum=*on
052400070928     C                   Z-ADD     PiVal         VABPKB
052500070928     C                   ELSE
052600070928     C                   SETON                                        32
052700070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052800070928     C                             + ' ' + 'VABPKB'
052900070928     C                   ENDIF
053000080923     C* CAS
053100080923     C                   IF        %trim(%subst(vindta:280:12))<>*blanks
053200080923     C                             AND
053300080923     C                             %trim(%subst(vindta:280:12))<>*zeros
053400080923     C                             AND
053500080923     C                             %trim(%subst(vindta:280:12))<>'0,00'
053600080923     C                             AND
053700080923     C                             %trim(%subst(vindta:280:12))<>'000000000,00'
053800080923     C                             AND
053900080923     C                             %trim(%subst(vindta:280:12))<>'000000000.00'
054000070928     C                   MOVEL     '1'           FlgCAS
054100070928     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
054200070928     C                   EXSR      CHKNUM
054300070928     C                   IF        PiNum=*on
054400070928     C                   Z-ADD     PiVal         VABCAS
054500070928     C                   ELSE
054600070928     C                   SETON                                        32
054700070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054800070928     C                             + ' ' + 'VABCAS'
054900070928     C                   ENDIF
055000070928     C                   ENDIF
055100071003     C* IAS
055200080923     C                   IF        %trim(%subst(vindta:295:12))<>*blanks
055300080923     C                             AND
055400080923     C                             %trim(%subst(vindta:295:12))<>*zeros
055500080923     C                             AND
055600080923     C                             %trim(%subst(vindta:295:12))<>'0,00'
055700080923     C                             AND
055800080229     C                             %trim(%subst(vindta:295:12))<>'000000000,00'
055900080923     C                             AND
056000080923     C                             %trim(%subst(vindta:295:12))<>'000000000.00'
056100080923     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
056200080923     C                   EXSR      CHKNUM
056300080923     C                   IF        PiNum=*on
056400080923     C                   Z-ADD     PiVal         VABIAS
056500080923     C                   ELSE
056600080923     C                   SETON                                        32
056700080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
056800080923     C                             + ' ' + 'VABIAS'
056900080923     C                   ENDIF
057000071003     C                   ENDIF
057100070730     C*
057200070730     C* Considerazioni finali su CBO/CAS
057300070730     C                   IF        FlgCAS = '1'
057400070730     C                   IF        VABCBO = '1'
057500070730     C                   EVAL      VABCBO = '4'
057600070730     C                   ENDIF
057700070730     C                   IF        VABCBO = '2'
057800070730     C                   EVAL      VABCBO = '6'
057900070730     C                   ENDIF
058000070730     C                   ENDIF
058100020305     C*
058200011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
058300011113     C                   EXSR      CHKIMPDIV
058400010202     C*
058500000801     C* Ebbene...
058600000801     C                   ADD       1             §CTRMO
058700070530     C                   IF        *in31 <> *zeros OR
058800070530     C                             *in32 <> *zeros
058900000801     C                   ADD       1             §CTRNO
059000000801     C                   EVAL      recko = vindta
059100000801     C                   EXCEPT    rigadett
059200020725     C                   EVAL      x_vinflg = '2'
059300000801     C                   ELSE
059400010201     C                   ADD       1             §CTROKVB
059500000801     C                   ENDIF
059600000801     C*
059700000801     C                   ENDSR
059800010201     C*----------------------------------------------------*
059900071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
060000010201     C*----------------------------------------------------*
060100071003     C     WRIVAT_B      BEGSR
060200090204     C*
060300090204     C                   eval      vatAAS = vabAAS
060400090204     C                   eval      vatLNP = vabLNP
060500090204     C                   eval      vatNRS = vabNRS
060600090204     C                   eval      vatNSP = vabNSP
060700010201     C*
060800021113     C* Valorizzo l buffer di scrittura del FIVAT
060900070928     C* - TIPO RECORD "B"
061000070928     C                   if        %subst(vindta:92:15) <> *blanks
061100070928     C                   eval      VATTRC = 'B'
061200070928     C                   eval      VATNOT = %trim(%subst(vindta:92:15))
061300021113     C                   write     FIVAT000
061400020725     C                   endif
061500010201     C*
061600010201     C                   ENDSR
061700071003     C*----------------------------------------------------*
061800071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
061900071003     C*----------------------------------------------------*
062000071003     C     WRIVAT_E      BEGSR
062100090204     C*
062200090204     C                   eval      vatAAS = vabAAS
062300090204     C                   eval      vatLNP = vabLNP
062400090204     C                   eval      vatNRS = vabNRS
062500090204     C                   eval      vatNSP = vabNSP
062600071003     C*
062700071003     C* Valorizzo l buffer di scrittura del FIVAT
062800071003     C* - TIPO RECORD "E"
062900071003     C                   if        %subst(vindta:331:26) <> *blanks
063000071003     C                   eval      VATTRC = 'E'
063100071003     C                   eval      VATNOT = %trim(%subst(vindta:331:26))
063200071003     C                   write     FIVAT000
063300071003     C                   endif
063400071003     C*
063500071003     C                   ENDSR
063600020904
063700020904
063800010202     C*----------------------------------------------------*
063900021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
064000010202     C*----------------------------------------------------*
064100020305     C     PREVAT        BEGSR
064200010202     C*
064300021113     C* Compongo il nome del membro da dare al FIVATWWR
064400010202     C                   eval      parmbr = vlrhdl
064500010202     C                   movel     'M'           parmbr
064600070530     C                   eval      parccm = %subst(vlrKSC:2:7)
064700010202     C                   eval      paropz = '1'
064800010202     C* Effettuo la chiamata al CLLE preposto
064900050128     C                   call(e)   'TITVVTC'
065000010202     C                   parm                    parccm
065100010202     C                   parm                    parmbr
065200010202     C                   parm                    paropz
065300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
065400010202     C                   if        %error
065500010202     C                   movel     '1'           chkcall
065600010202     C                   else
065700010202     C                   movel     '0'           chkcall
065800010202     C                   endif
065900010202     C*
066000010202     C                   ENDSR
066100000801     C*----------------------------------------------------*
066200000801     C*  CONTROLLO NUMERICITA' CAMPI
066300000801     C*----------------------------------------------------*
066400000801     C     CHKNUM        BEGSR
066500081113     C*
066600081113     C                   IF        PiDecChr = *blanks
066700081218     C                   EVAL      PiDecChr = ','
066800081113     C                   ENDIF
066900081113     C*
067000081113     C                   callp(e)  UBISNUM_Check(PiStr
067100081113     C                                          :PiDecChr
067200081113     C                                          :PiVal
067300081113     C                                          :PiNum
067400081113     C                                          :PiInt)
067500081113     C*
067600000801     C                   IF        %error
067700000801     C                   EVAL      PiInt=*off
067800000801     C                   ENDIF
067900000801     C*
068000000801     C                   ENDSR
068100000801     C***
068200000801
068300011113
068400011113     C*----------------------------------------------------*
068500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
068600011113     C*----------------------------------------------------*
068700011113     C     CHKIMPDIV     BEGSR
068800011113     C*
068900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
069000011113     C                   Z-ADD     *zeros        wrkDec            9 9
069100011113     C*
069200011113     C* Come prima cosa effettuo considerazioni sulla divisa
069300011113     C                   IF        vabIAS > *zeros
069400011113     C                   IF        vabVAS <> 'EUR'
069500011113     C                   EVAL      vabVAS =  'ITL'
069600011113     C                   ENDIF
069700011113     C                   ENDIF
069800011113     C*
069900011113     C                   IF        vabCAS > *zeros
070000011113     C                   IF        vabVCA <> 'EUR'
070100011113     C                   EVAL      vabVCA =  'ITL'
070200011113     C                   ENDIF
070300011113     C                   ENDIF
070400011113     C*
070500011113     C                   IF        vabVMD > *zeros
070600020305     C                   IF        vabVAD <> 'EUR'
070700011113     C                   EVAL      vabVAD =  'ITL'
070800011113     C                   ENDIF
070900011113     C                   ENDIF
071000011113     C*
071100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
071200011113     C                   Z-ADD     vabIAS        wrkDec
071300011113     C                   IF        wrkDec > *zeros
071400011113     C                   IF        vabVAS = 'ITL'
071500011113     C                   EVAL      vabIAS = *zeros
071600011113     C                   ENDIF
071700011113     C                   ENDIF
071800011113     C*
071900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
072000011113     C                   Z-ADD     vabCAS        wrkDec
072100011113     C                   IF        wrkDec > *zeros
072200011113     C                   IF        vabVCA = 'ITL'
072300011113     C                   EVAL      vabCAS = *zeros
072400011113     C                   ENDIF
072500011113     C                   ENDIF
072600011113     C*
072700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
072800011113     C                   Z-ADD     vabVMD        wrkDec
072900011113     C                   IF        wrkDec > *zeros
073000011113     C                   IF        vabVAD = 'ITL'
073100011113     C                   EVAL      vabVMD = *zeros
073200011113     C                   ENDIF
073300011113     C                   ENDIF
073400011113     C*
073500011113     C                   ENDSR
073600011113     C***
073700011113
073800011113
073900000801
074000000801
074100990920      /TITLE Invio dei dati al punto operativo.
074200010202     C     invio         BEGSR
074300990920     C*
074400021113     C* 1° invio FIVAT
074500010201     C                   reset                   dscmz
074600010201     C                   move      vlrpoi        cmzdst
074700021113     C                   eval      cmzfld = 'FIVATWWR'
074800010201     C                   eval      cmzmbd = vlrhdl
074900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
075000021009     C***                if        prmfir = *blanks
075100021113     C                   eval      cmzfla = 'FIVAT00F'
075200021113     C                   eval      cmzmba = 'FIVAT00F'
075300021009     C***                else
075400021009     C***                eval      cmzfla = prmfir
075500021009     C***                eval      cmzmba = prmfir
075600021009     C***                endif
075700010201     C                   eval      cmznrr = *zeros
075800020305     C                   move      §ctrokvt      cmznrr
075900021018     C                   eval      cmzlba = vlrfl1
076000010201     C                   call(e)   'TIS711C'
076100010201     C                   parm                    dscmz
076200010201     C                   parm      *blanks       esito
076300010205     C                   if        %error
076400010205     C                             or cmzerr = '1'
076500010205     C                             or esito  = '1'
076600010205     C                   eval      wrkesito = '3'
076700010205     C                   else
076800010201     C*
076900021113     C* 2° invio FIVAB
077000010201     C                   reset                   dscmz
077100010201     C                   move      vlrpoi        cmzdst
077200010201     C                   eval      cmzfld = vlrfou
077300010201     C                   eval      cmzmbd = vlrhdl
077400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
077500021009     C***                if        prmfir = *blanks
077600021113     C                   eval      cmzfla = 'FIVAB00F'
077700021113     C                   eval      cmzmba = 'FIVAB00F'
077800021009     C***                else
077900021009     C***                eval      cmzfla = prmfir
078000021009     C***                eval      cmzmba = prmfir
078100021009     C***                endif
078200010201     C                   eval      cmznrr = *zeros
078300010201     C                   move      §ctrokvb      cmznrr
078400021018     C                   eval      cmzlba = vlrfl1
078500010201     C                   call(e)   'TIS711C'
078600010201     C                   parm                    dscmz
078700010201     C                   parm      *blanks       esito
078800010201     C                   if        %error
078900010201     C                             or cmzerr = '1'
079000010201     C                             or esito  = '1'
079100010201     C                   eval      wrkesito = '3'
079200010201     C                   endif
079300010205     C                   endif
079400990920     C*
079500000613     C                   ENDSR
079600000613     C***
079700070411
079800070411     C     *pssr         BEGSR
079900070411     C*
080000070411     C                   if        %open(tivin00r)
080100070411     C                   close     tivin00r
080200070411     C                   endif
080300070411     C                   if        %open(fivabwwr)
080400070411     C                   close     fivabwwr
080500070411     C                   endif
080600070411     C                   if        %open(fivatwwr)
080700070411     C                   close     fivatwwr
080800070411     C                   endif
080900070411     C*
081000070411     C* Effettuo la chiamata al CLLE preposto
081100070411     C                   call(e)   'TITVVTC'
081200070411     C                   parm                    parccm
081300070411     C                   parm                    parmbr
081400070411     C                   parm      '2'           paropz
081500070411     C*
081600070411     C                   eval      wrkesito = '2'
081700070411     C*
081800070411     C                   seton                                        LR
081900070411     C*
082000070411     C                   ENDSR     '*CANCL'
082100070411     C***
082200070411
082300990910
082400000613     C     *inzsr        BEGSR
082500990910     C*
082600990910     C     *entry        plist
082700990920     C                   parm                    tivlrds
082800990921     C                   parm      wrkesito      esito
082900000724     C                   parm                    prmlit
083000000710     C                   parm                    prmfir
083100000613     C*
083200000830     C* CALCOLA LA DATA CORRENTE
083300000830     C                   time                    wn14             14 0
083400000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
083500000830     C                   z-add     wn8           g08dat
083600000830     C                   z-add     *zeros        g08inv
083700000830     C                   movel     '0'           g08err
083800000830     C                   call      'XSRDA8'
083900000830     C                   parm                    wlbda8
084000000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
084100000830     C*
084200000613     C                   ENDSR
084300000613     C***
084400990908
084500090407     Otitv1o6p  E            riepilogo         2
084600990915     O                                              'Upload via Internet'
084700990915     O                                           +1 'Traduzione TIVIN00R -'
084800021113     O                                           +1 'FIVABWWR/FIVATWWR'
084900010201     O                                           +1 'Report testate bolle'
085000990915     O          E            riepilogo   2
085100990915     O                       wrkdata
085200990915     O                       wrkora              +1
085300990915     O                       procname            +1
085400990915     O          E            riepilogo   2
085500990915     O                                              'Cliente..................:'
085600990915     O                       VABCCM        z     +1
085700990915     O          E            riepilogo   2
085800990920     O                                              'Riferimento Strategi.....:'
085900990920     O                       vlrhdl              +1
086000990915     O          E            riepilogo   2
086100990915     O                                              'Giusti...................:'
086200010201     O                       §CTROKVB      2   +  1
086300990915     O          E            riepilogo   2
086400990915     O                                              'Sbagliati e corretti.....:'
086500971022     O                       §CTRMO        2   +  1
086600990915     O          E            riepilogo   2
086700990915     O                                              'Sbagliati e scartati.....:'
086800971022     O                       §CTRNO        2   +  1
086900000613
087000090407     Otitv1o6ps E            testdett          2
087100000613     O                                              'Upload via Internet'
087200000613     O                                           +1 'Traduzione TIVIN00R -'
087300021113     O                                           +1 'FIVABWWR/FIVATWWR'
087400010201     O                                           +1 'Report testate bolle'
087500000616     O          E            testdett    3
087600000613     O                                           +2 'N° rec'
087700000613     O                                           +3 'Anteprima contenuto'
087800000616     O          E            rigadett    2
087900000613     O                       rrnum               +2
088000000621     O                       recko               +3
088100000616     O          E            findett     2
088200000613     O                       wrkdata
088300000613     O                       wrkora              +1
088400000613     O                       procname            +1
088500000616     O          E            findett     2
088600000613     O                                              'Cliente..................:'
088700000613     O                       VABCCM        z     +1
088800000616     O          E            findett     2
088900000613     O                                              'Riferimento Strategi.....:'
089000000613     O                       vlrhdl              +1
089100000616     O          E            findett     2
089200000613     O                                              'Giusti...................:'
089300010201     O                       §CTROKVB      2   +  1
089400000616     O          E            findett     2
089500000613     O                                              'Sbagliati e corretti.....:'
089600000613     O                       §CTRMO        2   +  1
089700000616     O          E            findett     2
089800000613     O                                              'Sbagliati e scartati.....:'
089900000613     O                       §CTRNO        2   +  1
090000000512** CMD - COMANDI CL
090100090407OVRPRTF FILE(TITV1O6P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
090200090407OVRPRTF FILE(TITV1O6PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
090300090407DLTOVR FILE(TITV1O6P TITV1O6PS) LVL(*)
090400000512
090500000512
