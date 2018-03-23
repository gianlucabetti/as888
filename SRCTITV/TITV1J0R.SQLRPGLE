000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800080923     Ftitv1j0p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100080923     Ftitv1j0ps O    f  198        PRINTER usropn
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
007300990908
007400010201
007500010201
007600080117     C*
007700080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007800080117     C
007900080117     C/EXEC SQL
008000080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008100080117     C/END-EXEC
008200080117     C
008300990915     C                   time                    wrkdata
008400990915     C                   time                    wrkora
008500000913     C                   reset                   rrnum
008600990921     C                   reset                   esito
008700990921     C                   reset                   wrkesito
008800000724     C*
008900000724     C* SE OCCORRE SPEDIRE IN FILIALE
009000000724     C                   if        vlrpoi <> *zeros
009100000724     C*
009200000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009300000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009400000724     C     vlrpoi        chain     azorg01l
009500000724     C                   if        %found
009600000616     C                   movel(p)  CMD(1)        depcmd
009700020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
009800000616     C*
009900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010000011113     C                   Z-ADD     150           LENGH            15 5
010100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010200000616     C                   PARM                    depcmd
010300000616     C                   PARM                    LENGH
010400000724     C*
010500000724     C                   endif
010600000724     C                   endif
010700000616     C*
010800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010900000616     C                   movel(p)  CMD(2)        depcmd
011000000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011100000616     C*
011200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
011300011113     C                   Z-ADD     150           LENGH            15 5
011400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011500000616     C                   PARM                    depcmd
011600000616     C                   PARM                    LENGH
011700000616     C*
011800080923     C                   if        not %open(titv1j0ps)
011900080923     C                   open      titv1j0ps
012000000616     C                   except    testdett
012100000613     C                   endif
012200000613     C*
012300070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
012400000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012500000613     C*
012600010202     C* Effettuo la chiamata al CLLE preposto
012700050128     C                   call(e)   'TITVVTC'
012800010202     C                   parm                    parccm
012900010202     C                   parm                    parmbr
013000010202     C                   parm      '2'           paropz
013100070730     C*
013200070730     C* Effettuo lancio TISI95 solo x chiusura
013300070730     C                   CLEAR                   TISI95DS
013400070730     C                   EVAL      I95TLA = 'C'
013500070730     C                   CALL      'TISI95R'
013600070730     C                   PARM                    TISI95DS
013700010202     C*
013800080923     C                   if        %open(titv1j0ps)
013900000616     C                   except    findett
014000080923     C                   close     titv1j0ps
014100000613     C                   endif
014200000616     C*
014300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
014400011113     C                   Z-ADD     150           LENGH            15 5
014500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014600000616     C                   PARM                    CMD(3)
014700000616     C                   PARM                    LENGH
014800000616     C*
014900000801     C
015000010201     C                   seton                                        LR
015100000613
015200000613     C*--------------------------------------------------------
015300000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
015400000613     C*--------------------------------------------------------
015500000621     C     STPR          BEGSR
015600000613     C*
015700080923     C                   if        not %open(titv1j0p)
015800080923     C                   open      titv1j0p
015900990915     C                   endif
016000990915     C*
016100990915     C                   except    riepilogo
016200990915     C*
016300080923     C                   if        %open(titv1j0p)
016400080923     C                   close     titv1j0p
016500990914     C                   endif
016600990910     C*
016700000613     C                   ENDSR
016800000613     C***
016900990908
017000000801
017100910830     C*--------------------------------------------------------
017200070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
017300910830     C*--------------------------------------------------------
017400070530     C     RWFILE        BEGSR
017500990910     C*
017600990914     C                   if        not %open(tivin00r)
017700990908     C                   open      tivin00r
017800990914     C                   endif
017900021113     C                   if        not %open(fivabwwr)
018000021113     C                   open      fivabwwr
018100990914     C                   endif
018200070530     C*
018300021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
018400020305     C                   exsr      prevat
018500010201     C*
018600010202     C                   if        chkcall = '0'
018700010202     C*
018800021113     C                   if        not %open(fivatwwr)
018900021113     C                   open      fivatwwr
019000010201     C                   endif
019100080117     C*
019200080117     C                   EXSR      INZVAR
019300080117     C                   EXSR      DEFCAM
019400990910     C*
019500010201     C                   clear                   §CTROKVB          5 0
019600020305     C                   clear                   §CTROKVT          5 0
019700000801     C                   clear                   §CTRMO            5 0
019800000801     C                   clear                   §CTRNO            5 0
019900990910     C*
020000020725     C/EXEC SQL
020100020725     C+ declare C1 cursor for select
020200070928     C+ vindta, vinflg, substr(vindta, 315, 16) as sped, rrn(tivin00r)
020300060223     C+ from tivin00r
020400060223     C+ order by sped
020500020725     C+ for read only
020600020725     C/END-EXEC
020700020725     C
020800020725     C/EXEC SQL
020900020725     C+ open C1
021000020725     C/END-EXEC
021100020725     C
021200020725     C/EXEC SQL
021300070530     C+ Fetch C1 into :INPUT_DS
021400020725     C/END-EXEC
021500020725     C*
021600020725     C                   dow       sqlcod = *zeros
021700990913     C*
021800020725     C                   if        vindta > *blanks
021900000613     C                   add       1             rrnum
022000000801     C*
022100020725     C                   if        vinflg = *blanks
022200020725     C                             or vinflg = '0'
022300020725     C                             or vinflg = '2'
022400000801     C*
022500020725     C                   clear                   x_vinmsg
022600020725     C                   eval      x_vinflg = '1'
022700010305     C*
022800010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
022900070928     C                   EVAL      PiStr=%trim(%subst(vindta:315:16))
023000020305     C                   MOVEL(p)  PiStr         curspe
023100010305     C*
023200071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
023300080923     C                   if        *in50 = *off
023400071003     C                   exsr      impvab
023500071003     C                   exsr      wrivab
023600071003     C                   exsr      wrivat_b                                     => carico VAT
023700071003     C                   else
023800080923     C*
023900071009     C                   if        wDISK = 'D'
024000071009     C                   exsr      impvab
024100071009     C                   exsr      wrivab
024200071009     C                   exsr      wrivat_b                                     => carico VAT
024300071009     C                   exsr      wrivat_e                                     => carico VAT
024400071009     C                   else
024500071009     C*
024600010305     C                   if        depspe = *blanks                             => 1° giro
024700010305     C                   eval      depspe = curspe                              => memorizz. spediz
024800080117     C                   clear                   tivinds
024900020305     C                   exsr      impvab
025000071003     C                   exsr      wrivat_b                                     => carico VAT
025100071003     C   50              exsr      wrivat_e                                     => carico VAT
025200010305     C                   else
025300020725     C                   if        depspe <> curspe                             => rottura di spediz
025400010305     C                   eval      depspe = curspe                              => memorizz. spediz
025500070928     C                   exsr      wrivab
025600080117     C                   clear                   tivinds
025700020305     C                   exsr      impvab
025800071003     C                   exsr      wrivat_b                                     => carico VAT
025900071003     C   50              exsr      wrivat_e                                     => carico VAT
026000020305     C                   else                                                   => x stessa spediz
026100020305     C                   exsr      impvab
026200071003     C                   exsr      wrivat_b                                     => carico VAT
026300071003     C   50              exsr      wrivat_e                                     => carico VAT
026400010305     C                   endif
026500010305     C                   endif
026600010305     C                   endif
026700071003     C                   endif
026800071009     C                   endif
026900000905     C*
027000000905     C                   else
027100020725     C                   eval      x_vinflg = '1'
027200000905     C                   endif
027300000905     C*
027400020725     C     VINRRN        chain     tivin000
027500020725     C                   if        %found(tivin00r)
027600020725     C                   eval      y_vinflg = x_vinflg
027700020725     C                   eval      y_vinmsg = x_vinmsg
027800020725     C                   update    tivin000
027900020725     C                   endif
028000020725     C*
028100020725     C/EXEC SQL
028200070530     C+ Fetch C1 into :INPUT_DS
028300020725     C/END-EXEC
028400020725     C*
028500020725     C                   enddo
028600020725     C*
028700020725     C/EXEC SQL
028800020725     C+ close C1
028900020725     C/END-EXEC
029000000905     C*
029100020305     C* Scarico i VAB rimasti "in sospeso"
029200071009     C                   if        wDISK = 'C'
029300071009     C                   exsr      wrivab
029400071009     C                   endif
029500010202     C*
029600010202     C                   endif
029700990910
029800990910     C* Se non ci sono record con errori ...
029900000710     C                   if        §ctrno = 0
030000990910     C* ... restituisco esito OK.
030100990921     C                   eval      wrkesito = '0'
030200990910     C                   else
030300010201     C                   if        §ctrokvb > 0
030400990921     C                   eval      wrkesito = '1'
030500000710     C                   else
030600000710     C                   eval      wrkesito = '2'
030700990910     C                   endif
030800000710     C                   endif
030900990910     C*
031000990914     C                   if        %open(tivin00r)
031100990908     C                   close     tivin00r
031200990914     C                   endif
031300021113     C                   if        %open(fivabwwr)
031400021113     C                   close     fivabwwr
031500990914     C                   endif
031600021113     C                   if        %open(fivatwwr)
031700021113     C                   close     fivatwwr
031800010201     C                   endif
031900990910     C*
032000010201     C                   if        §ctrokvb > 0
032100000724     C                             and vlrpoi <> *zeros
032200010202     C                   exsr      invio
032300990920     C                   endif
032400990920     C*
032500910830     C                   ENDSR
032600000613     C***
032700010305
032800010305     C*----------------------------------------------------*
032900020305     C*  SCARICAMENTO BUFFER RECORDS VAB
033000010305     C*----------------------------------------------------*
033100020305     C     WRIVAB        BEGSR
033200070730     C*
033300071003     C* Considerazioni finali
033400071003     C                   if        VABRMA = *blanks
033500071003     C                   movel(P)  VABRMN        VABRMA
033600071003     C                   endif
033700071003     C*
033800021113     C                   write     fivab000                                     => scarico il VAB
033900010305     C*
034000010305     C                   ENDSR
034100990920
034200000801     C*----------------------------------------------------*
034300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
034400000801     C*----------------------------------------------------*
034500010201     C     INZVAR        BEGSR
034600000801     C*
034700010201     C                   Z-ADD     *zeros        Num5_0
034800020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
034900020725     C                   MOVEL     '0'           FlgCAS            1
035000000801     C*
035100000801     C                   ENDSR
035200000801     C*----------------------------------------------------*
035300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
035400000801     C*----------------------------------------------------*
035500020904     C     DEFCAM        BEGSR
035600080923     C*
035700080923     C                   SETOFF                                       5051
035800000801     C*
035900021113     C                   CLEAR                   FIVAB000
036000021113     C                   CLEAR                   FIVAT000
036100070730     C* Imposto i valori di default...
036200080923     C                   EVAL      VABCCM = 0433924
036300080923     C                   EVAL      VATCCM = 0433924
036400080923     C                   EVAL      VABLNP = 043
036500080923     C                   EVAL      VATLNP = 043
036600070928     C                   EVAL      VABCTR = 000
036700070730     C                   EVAL      VABCBO = '1'
036800070222     C* ... e poi verifico se sono stati passati come parametri
036900070222     C                   IF        vlrppt > *blanks
037000070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
037100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
037200070222     C                   EXSR      CHKNUM
037300070222     C                   IF        PiInt=*on
037400070222     C                   Z-ADD     PiVal         VABCCM
037500070222     C                   Z-ADD     PiVal         VATCCM
037600070222     C                   ENDIF
037700070222     C                   ENDIF
037800070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
037900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
038000070222     C                   EXSR      CHKNUM
038100070222     C                   IF        PiInt=*on
038200070222     C                   Z-ADD     PiVal         VABLNP
038300070222     C                   Z-ADD     PiVal         VATLNP
038400070222     C                   ENDIF
038500070222     C                   ENDIF
038600070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
038700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
038800070222     C                   EXSR      CHKNUM
038900070222     C                   IF        PiInt=*on
039000070222     C                   Z-ADD     PiVal         VABCTR
039100070222     C                   ENDIF
039200070928     C                   ENDIF
039300071009     C                   MOVEL     *blanks       wDISK             1
039400071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
039500071009     C                   IF        wDISK <> *blanks
039600070928     C                   SETON                                        50
039700070222     C                   ENDIF
039800080923     C                   IF        %subst(vlrppt:15:1) = 'S'
039900080923     C                   SETON                                        51
040000080923     C                   ENDIF
040100070222     C                   ENDIF
040200071009     C*
040300071009     C   50              EVAL      VABCTM = '7Q'
040400000801     C*
040500000801     C                   ENDSR
040600000801     C*----------------------------------------------------*
040700021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
040800000801     C*----------------------------------------------------*
040900010201     C     IMPVAB        BEGSR
041000070530     C*
041100070530     C                   SETOFF                                       3132
041200070928     C*
041300070928     C                   EXSR      INZVAR
041400070928     C                   EXSR      DEFCAM
041500070928     C*
041600070928     C                   MOVE(P)   vlrpoi        VABFGS
041700070928     C                   MOVE(P)   vlrpoi        VATFGS
041800070928     C*
041900070928     C                   MOVEL     datcor        VABAAS
042000070928     C                   MOVEL     datcor        VATAAS
042100070928     C                   MOVE      datcor        VABMGS
042200070928     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
042300070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
042400070928     C     '@':'A'       XLATE     VABRSD        VABRSD
042500070928     C* ==
042600070928     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
042700070928     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
042800070928     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
042900070928     C                   EVAL      VABRMA=%trim(%subst(vindta:315:16))
043000070928     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
043100080226     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
043200070928     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
043300070928     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
043400070928     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:80-35))
043500080923     C                   IF        %trim(%subst(vindta:390:1)) = 'F'
043600080226     C                   EVAL      VABCBO='1'
043700080226     C                   ENDIF
043800080923     C                   IF        %trim(%subst(vindta:390:1)) = 'A'
043900080226     C                   EVAL      VABCBO='2'
044000080226     C                   ENDIF
044100080226     C                   IF        %trim(%subst(vindta:12:1)) = 'D'
044200080226     C                   EVAL      VABTSP='E'
044300080226     C                   ENDIF
044400080226     C                   IF        %trim(%subst(vindta:16:1)) = '1'
044500080226     C                   EVAL      VABFFD='S'
044600080226     C                   ENDIF
044700080923     C                   IF        %trim(%subst(vindta:16:1)) = '3'
044800080328     C                   EVAL      VABTC1='A'
044900080226     C                   ENDIF
045000071003     C* CAD
045100070928     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
045200070928     C                   EXSR      CHKNUM
045300070928     C                   IF        PiInt=*on
045400070928     C                   Z-ADD     PiVal         Num5_0
045500070928     C                   MOVEL     Num5_0        VABCAD
045600070928     C                   ELSE
045700070928     C                   SETON                                        32
045800070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045900070928     C                             + ' ' + 'VABCAD'
046000070928     C                   ENDIF
046100080923     C* RMN
046200080923     C                   EVAL      PiStr=%trim(%subst(vindta:186+1:11-1))
046300080923     C                   EXSR      CHKNUM
046400080923     C                   IF        PiInt=*on
046500080923     C                   Z-ADD     PiVal         VABRMN
046600080923     C                   ELSE
046700080923     C                   SETON                                        32
046800080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046900080923     C                             + ' ' + 'VABRMN'
047000080923     C                   ENDIF
047100080923     C* NSP
047200080923     C                   IF        *in51 = *on
047300080923     C* NSP => Stacco un numeratore da AZNUM
047400080923     C                   clear                   TRUL33DS
047500080923     C                   eval      I33OPE = *zeros
047600080923     C                   eval      I33CNU = 302
047700080923     C                   eval      I33NUM = 1
047800080923     C                   movel     TRUL33DS      KPJBU
047900080923     C                   call      'TRUL33R'
048000080923     C                   parm                    KPJBA
048100080923     C                   movel     KPJBU         TRUL33DS
048200080923     C                   if        O33ERR = *zeros
048300080923     C                   z-add     O33NRF        VABNSP
048400080923     C                   z-add     O33NRF        VATNSP
048500080923     C                   else
048600080923     C                   SETON                                        31
048700080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048800080923     C                             + ' ' + 'VABNSP VATNSP'
048900080923     C                   endif
049000080923     C                   ELSE
049100080923     C                   EVAL      PiStr=%trim(%subst(vindta:186+1:11-1))
049200080923     C                   EXSR      CHKNUM
049300080923     C                   IF        PiInt=*on
049400080923     C                   Z-ADD     PiVal         VABNSP
049500080923     C                   Z-ADD     PiVal         VATNSP
049600080923     C                   ELSE
049700080923     C                   SETON                                        31
049800080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049900080923     C                             + ' ' + 'VABNSP VATNSP'
050000080923     C                   ENDIF
050100080923     C                   ENDIF
050200071003     C* NCL
050300070928     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
050400070928     C                   EXSR      CHKNUM
050500070928     C                   IF        PiInt=*on
050600070928     C                   Z-ADD     PiVal         VABNCL
050700070928     C                   ELSE
050800070928     C                   SETON                                        32
050900070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051000070928     C                             + ' ' + 'VABNCL'
051100070928     C                   ENDIF
051200071003     C* PKB
051300070928     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
051400070928     C                   EXSR      CHKNUM
051500070928     C                   IF        PiNum=*on
051600070928     C                   Z-ADD     PiVal         VABPKB
051700070928     C                   ELSE
051800070928     C                   SETON                                        32
051900070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052000070928     C                             + ' ' + 'VABPKB'
052100070928     C                   ENDIF
052200080923     C* CAS
052300080923     C                   IF        %trim(%subst(vindta:280:12))<>*blanks
052400080923     C                             AND
052500080923     C                             %trim(%subst(vindta:280:12))<>*zeros
052600080923     C                             AND
052700080923     C                             %trim(%subst(vindta:280:12))<>'0,00'
052800080923     C                             AND
052900080923     C                             %trim(%subst(vindta:280:12))<>'000000000,00'
053000080923     C                             AND
053100080923     C                             %trim(%subst(vindta:280:12))<>'000000000.00'
053200070928     C                   MOVEL     '1'           FlgCAS
053300070928     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
053400070928     C                   EXSR      CHKNUM
053500070928     C                   IF        PiNum=*on
053600070928     C                   Z-ADD     PiVal         VABCAS
053700070928     C                   ELSE
053800070928     C                   SETON                                        32
053900070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054000070928     C                             + ' ' + 'VABCAS'
054100070928     C                   ENDIF
054200070928     C                   ENDIF
054300071003     C* IAS
054400080923     C                   IF        %trim(%subst(vindta:295:12))<>*blanks
054500080923     C                             AND
054600080923     C                             %trim(%subst(vindta:295:12))<>*zeros
054700080923     C                             AND
054800080923     C                             %trim(%subst(vindta:295:12))<>'0,00'
054900080923     C                             AND
055000080229     C                             %trim(%subst(vindta:295:12))<>'000000000,00'
055100080923     C                             AND
055200080923     C                             %trim(%subst(vindta:295:12))<>'000000000.00'
055300080923     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
055400080923     C                   EXSR      CHKNUM
055500080923     C                   IF        PiNum=*on
055600080923     C                   Z-ADD     PiVal         VABIAS
055700080923     C                   ELSE
055800080923     C                   SETON                                        32
055900080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
056000080923     C                             + ' ' + 'VABIAS'
056100080923     C                   ENDIF
056200071003     C                   ENDIF
056300070730     C*
056400070730     C* Considerazioni finali su CBO/CAS
056500070730     C                   IF        FlgCAS = '1'
056600070730     C                   IF        VABCBO = '1'
056700070730     C                   EVAL      VABCBO = '4'
056800070730     C                   ENDIF
056900070730     C                   IF        VABCBO = '2'
057000070730     C                   EVAL      VABCBO = '6'
057100070730     C                   ENDIF
057200070730     C                   ENDIF
057300020305     C*
057400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
057500011113     C                   EXSR      CHKIMPDIV
057600010202     C*
057700000801     C* Ebbene...
057800000801     C                   ADD       1             §CTRMO
057900070530     C                   IF        *in31 <> *zeros OR
058000070530     C                             *in32 <> *zeros
058100000801     C                   ADD       1             §CTRNO
058200000801     C                   EVAL      recko = vindta
058300000801     C                   EXCEPT    rigadett
058400020725     C                   EVAL      x_vinflg = '2'
058500000801     C                   ELSE
058600010201     C                   ADD       1             §CTROKVB
058700000801     C                   ENDIF
058800000801     C*
058900000801     C                   ENDSR
059000010201     C*----------------------------------------------------*
059100071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
059200010201     C*----------------------------------------------------*
059300071003     C     WRIVAT_B      BEGSR
059400010201     C*
059500021113     C* Valorizzo l buffer di scrittura del FIVAT
059600070928     C* - TIPO RECORD "B"
059700070928     C                   if        %subst(vindta:92:15) <> *blanks
059800070928     C                   eval      VATTRC = 'B'
059900070928     C                   eval      VATNOT = %trim(%subst(vindta:92:15))
060000021113     C                   write     FIVAT000
060100020725     C                   endif
060200010201     C*
060300010201     C                   ENDSR
060400071003     C*----------------------------------------------------*
060500071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
060600071003     C*----------------------------------------------------*
060700071003     C     WRIVAT_E      BEGSR
060800071003     C*
060900071003     C* Valorizzo l buffer di scrittura del FIVAT
061000071003     C* - TIPO RECORD "E"
061100071003     C                   if        %subst(vindta:331:26) <> *blanks
061200071003     C                   eval      VATTRC = 'E'
061300071003     C                   eval      VATNOT = %trim(%subst(vindta:331:26))
061400071003     C                   write     FIVAT000
061500071003     C                   endif
061600071003     C*
061700071003     C                   ENDSR
061800020904
061900020904
062000010202     C*----------------------------------------------------*
062100021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
062200010202     C*----------------------------------------------------*
062300020305     C     PREVAT        BEGSR
062400010202     C*
062500021113     C* Compongo il nome del membro da dare al FIVATWWR
062600010202     C                   eval      parmbr = vlrhdl
062700010202     C                   movel     'M'           parmbr
062800070530     C                   eval      parccm = %subst(vlrKSC:2:7)
062900010202     C                   eval      paropz = '1'
063000010202     C* Effettuo la chiamata al CLLE preposto
063100050128     C                   call(e)   'TITVVTC'
063200010202     C                   parm                    parccm
063300010202     C                   parm                    parmbr
063400010202     C                   parm                    paropz
063500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
063600010202     C                   if        %error
063700010202     C                   movel     '1'           chkcall
063800010202     C                   else
063900010202     C                   movel     '0'           chkcall
064000010202     C                   endif
064100010202     C*
064200010202     C                   ENDSR
064300000801     C*----------------------------------------------------*
064400000801     C*  CONTROLLO NUMERICITA' CAMPI
064500000801     C*----------------------------------------------------*
064600000801     C     CHKNUM        BEGSR
064700000801     C*
064800000801     C                   call(e)   'ISNUMERIC'
064900000801     C                   PARM                    PiStr            30
065000080923     C                   PARM      '.'           PiDecChr          1
065100000801     C                   PARM      *ZEROS        PiVal            30 9
065200000801     C                   PARM      '0'           PiInt             1
065300000801     C                   PARM      '0'           PiNum             1
065400000801     C                   IF        %error
065500000801     C                   EVAL      PiInt=*off
065600000801     C                   ENDIF
065700000801     C*
065800000801     C                   ENDSR
065900000801     C***
066000000801
066100011113
066200011113     C*----------------------------------------------------*
066300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
066400011113     C*----------------------------------------------------*
066500011113     C     CHKIMPDIV     BEGSR
066600011113     C*
066700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
066800011113     C                   Z-ADD     *zeros        wrkDec            9 9
066900011113     C*
067000011113     C* Come prima cosa effettuo considerazioni sulla divisa
067100011113     C                   IF        vabIAS > *zeros
067200011113     C                   IF        vabVAS <> 'EUR'
067300011113     C                   EVAL      vabVAS =  'ITL'
067400011113     C                   ENDIF
067500011113     C                   ENDIF
067600011113     C*
067700011113     C                   IF        vabCAS > *zeros
067800011113     C                   IF        vabVCA <> 'EUR'
067900011113     C                   EVAL      vabVCA =  'ITL'
068000011113     C                   ENDIF
068100011113     C                   ENDIF
068200011113     C*
068300011113     C                   IF        vabVMD > *zeros
068400020305     C                   IF        vabVAD <> 'EUR'
068500011113     C                   EVAL      vabVAD =  'ITL'
068600011113     C                   ENDIF
068700011113     C                   ENDIF
068800011113     C*
068900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
069000011113     C                   Z-ADD     vabIAS        wrkDec
069100011113     C                   IF        wrkDec > *zeros
069200011113     C                   IF        vabVAS = 'ITL'
069300011113     C                   EVAL      vabIAS = *zeros
069400011113     C                   ENDIF
069500011113     C                   ENDIF
069600011113     C*
069700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
069800011113     C                   Z-ADD     vabCAS        wrkDec
069900011113     C                   IF        wrkDec > *zeros
070000011113     C                   IF        vabVCA = 'ITL'
070100011113     C                   EVAL      vabCAS = *zeros
070200011113     C                   ENDIF
070300011113     C                   ENDIF
070400011113     C*
070500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
070600011113     C                   Z-ADD     vabVMD        wrkDec
070700011113     C                   IF        wrkDec > *zeros
070800011113     C                   IF        vabVAD = 'ITL'
070900011113     C                   EVAL      vabVMD = *zeros
071000011113     C                   ENDIF
071100011113     C                   ENDIF
071200011113     C*
071300011113     C                   ENDSR
071400011113     C***
071500011113
071600011113
071700000801
071800000801
071900990920      /TITLE Invio dei dati al punto operativo.
072000010202     C     invio         BEGSR
072100990920     C*
072200021113     C* 1° invio FIVAT
072300010201     C                   reset                   dscmz
072400010201     C                   move      vlrpoi        cmzdst
072500021113     C                   eval      cmzfld = 'FIVATWWR'
072600010201     C                   eval      cmzmbd = vlrhdl
072700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072800021009     C***                if        prmfir = *blanks
072900021113     C                   eval      cmzfla = 'FIVAT00F'
073000021113     C                   eval      cmzmba = 'FIVAT00F'
073100021009     C***                else
073200021009     C***                eval      cmzfla = prmfir
073300021009     C***                eval      cmzmba = prmfir
073400021009     C***                endif
073500010201     C                   eval      cmznrr = *zeros
073600020305     C                   move      §ctrokvt      cmznrr
073700021018     C                   eval      cmzlba = vlrfl1
073800010201     C                   call(e)   'TIS711C'
073900010201     C                   parm                    dscmz
074000010201     C                   parm      *blanks       esito
074100010205     C                   if        %error
074200010205     C                             or cmzerr = '1'
074300010205     C                             or esito  = '1'
074400010205     C                   eval      wrkesito = '3'
074500010205     C                   else
074600010201     C*
074700021113     C* 2° invio FIVAB
074800010201     C                   reset                   dscmz
074900010201     C                   move      vlrpoi        cmzdst
075000010201     C                   eval      cmzfld = vlrfou
075100010201     C                   eval      cmzmbd = vlrhdl
075200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
075300021009     C***                if        prmfir = *blanks
075400021113     C                   eval      cmzfla = 'FIVAB00F'
075500021113     C                   eval      cmzmba = 'FIVAB00F'
075600021009     C***                else
075700021009     C***                eval      cmzfla = prmfir
075800021009     C***                eval      cmzmba = prmfir
075900021009     C***                endif
076000010201     C                   eval      cmznrr = *zeros
076100010201     C                   move      §ctrokvb      cmznrr
076200021018     C                   eval      cmzlba = vlrfl1
076300010201     C                   call(e)   'TIS711C'
076400010201     C                   parm                    dscmz
076500010201     C                   parm      *blanks       esito
076600010201     C                   if        %error
076700010201     C                             or cmzerr = '1'
076800010201     C                             or esito  = '1'
076900010201     C                   eval      wrkesito = '3'
077000010201     C                   endif
077100010205     C                   endif
077200990920     C*
077300000613     C                   ENDSR
077400000613     C***
077500070411
077600070411     C     *pssr         BEGSR
077700070411     C*
077800070411     C                   if        %open(tivin00r)
077900070411     C                   close     tivin00r
078000070411     C                   endif
078100070411     C                   if        %open(fivabwwr)
078200070411     C                   close     fivabwwr
078300070411     C                   endif
078400070411     C                   if        %open(fivatwwr)
078500070411     C                   close     fivatwwr
078600070411     C                   endif
078700070411     C*
078800070411     C* Effettuo la chiamata al CLLE preposto
078900070411     C                   call(e)   'TITVVTC'
079000070411     C                   parm                    parccm
079100070411     C                   parm                    parmbr
079200070411     C                   parm      '2'           paropz
079300070411     C*
079400070411     C                   eval      wrkesito = '2'
079500070411     C*
079600070411     C                   seton                                        LR
079700070411     C*
079800070411     C                   ENDSR     '*CANCL'
079900070411     C***
080000070411
080100990910
080200000613     C     *inzsr        BEGSR
080300990910     C*
080400990910     C     *entry        plist
080500990920     C                   parm                    tivlrds
080600990921     C                   parm      wrkesito      esito
080700000724     C                   parm                    prmlit
080800000710     C                   parm                    prmfir
080900000613     C*
081000000830     C* CALCOLA LA DATA CORRENTE
081100000830     C                   time                    wn14             14 0
081200000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
081300000830     C                   z-add     wn8           g08dat
081400000830     C                   z-add     *zeros        g08inv
081500000830     C                   movel     '0'           g08err
081600000830     C                   call      'XSRDA8'
081700000830     C                   parm                    wlbda8
081800000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
081900000830     C*
082000000613     C                   ENDSR
082100000613     C***
082200990908
082300080923     Otitv1j0p  E            riepilogo         2
082400990915     O                                              'Upload via Internet'
082500990915     O                                           +1 'Traduzione TIVIN00R -'
082600021113     O                                           +1 'FIVABWWR/FIVATWWR'
082700010201     O                                           +1 'Report testate bolle'
082800990915     O          E            riepilogo   2
082900990915     O                       wrkdata
083000990915     O                       wrkora              +1
083100990915     O                       procname            +1
083200990915     O          E            riepilogo   2
083300990915     O                                              'Cliente..................:'
083400990915     O                       VABCCM        z     +1
083500990915     O          E            riepilogo   2
083600990920     O                                              'Riferimento Strategi.....:'
083700990920     O                       vlrhdl              +1
083800990915     O          E            riepilogo   2
083900990915     O                                              'Giusti...................:'
084000010201     O                       §CTROKVB      2   +  1
084100990915     O          E            riepilogo   2
084200990915     O                                              'Sbagliati e corretti.....:'
084300971022     O                       §CTRMO        2   +  1
084400990915     O          E            riepilogo   2
084500990915     O                                              'Sbagliati e scartati.....:'
084600971022     O                       §CTRNO        2   +  1
084700000613
084800080923     Otitv1j0ps E            testdett          2
084900000613     O                                              'Upload via Internet'
085000000613     O                                           +1 'Traduzione TIVIN00R -'
085100021113     O                                           +1 'FIVABWWR/FIVATWWR'
085200010201     O                                           +1 'Report testate bolle'
085300000616     O          E            testdett    3
085400000613     O                                           +2 'N° rec'
085500000613     O                                           +3 'Anteprima contenuto'
085600000616     O          E            rigadett    2
085700000613     O                       rrnum               +2
085800000621     O                       recko               +3
085900000616     O          E            findett     2
086000000613     O                       wrkdata
086100000613     O                       wrkora              +1
086200000613     O                       procname            +1
086300000616     O          E            findett     2
086400000613     O                                              'Cliente..................:'
086500000613     O                       VABCCM        z     +1
086600000616     O          E            findett     2
086700000613     O                                              'Riferimento Strategi.....:'
086800000613     O                       vlrhdl              +1
086900000616     O          E            findett     2
087000000613     O                                              'Giusti...................:'
087100010201     O                       §CTROKVB      2   +  1
087200000616     O          E            findett     2
087300000613     O                                              'Sbagliati e corretti.....:'
087400000613     O                       §CTRMO        2   +  1
087500000616     O          E            findett     2
087600000613     O                                              'Sbagliati e scartati.....:'
087700000613     O                       §CTRNO        2   +  1
087800000512** CMD - COMANDI CL
087900080923OVRPRTF FILE(TITV1J0P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
088000080923OVRPRTF FILE(TITV1J0PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
088100080923DLTOVR FILE(TITV1J0P TITV1J0PS) LVL(*)
088200000512
088300000512
