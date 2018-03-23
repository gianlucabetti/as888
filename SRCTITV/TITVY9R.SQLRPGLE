000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800070530     Ftitvy9p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100070530     Ftitvy9ps  O    f  198        PRINTER usropn
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
003200000613     D rrnum           s              6  0 INZ(*zeros)
003300000621     D recko           s            150    INZ(*blanks)
003400011113     D depcmd          s            150    INZ(*blanks)
003500070806     D depspe          s             12    INZ(*blanks)
003600010213     D depdat          s              8  0 INZ(*zeros)
003700010213     D depaas          s              4    INZ(*zeros)
003800070806     D curspe          s             12    INZ(*blanks)
003900010202     D parccm          s              8    INZ(*blanks)
004000010202     D parmbr          s             10    INZ(*blanks)
004100010202     D paropz          s              1    INZ(*blanks)
004200010202     D chkcall         s              1    INZ(*blanks)
004300020725     D tivinds       e ds                  extname(tivin00r) prefix(x_)
004400070530     D Num5_0          s              5  0
004500000830
004600000830     D*------------------
004700000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004800000830     D*------------------
004900000830     D WLBDA8          DS                  INZ
005000000830     D  G08DAT                 1      8  0
005100000830     D  G08INV                 9     16  0
005200000830     D  G08ERR                17     17
005300000830     D  G08TGI                18     22  0
005400000830     D*
005500020725
005600020725     D*------------------
005700020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
005800020725     D*------------------
005900070530     D INPUT_DS        DS                  INZ
006000020725     D  VINDTA                 1   2048
006100020725     D  VINFLG              2049   2049
006200070806     D  VINSPE              2050   2061
006300070806     D  VINRRN              2061   2069  0
006400070806     D*------------------
006500070806     D* DS REPERIMENTO NUMERATORE
006600070806     D*------------------
006700070806     D trul33ds      e ds                  inz
006800070806     D*------------------
006900070806     D* DS ARCHITETTURA
007000070806     D*------------------
007100070806     D kpjba         e ds                  inz
007200990908
007300010201
007400010201
007500990915     C                   time                    wrkdata
007600990915     C                   time                    wrkora
007700000913     C                   reset                   rrnum
007800990921     C                   reset                   esito
007900990921     C                   reset                   wrkesito
008000000724     C*
008100000724     C* SE OCCORRE SPEDIRE IN FILIALE
008200000724     C                   if        vlrpoi <> *zeros
008300000724     C*
008400000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008500000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008600000724     C     vlrpoi        chain     azorg01l
008700000724     C                   if        %found
008800000616     C                   movel(p)  CMD(1)        depcmd
008900020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
009000000616     C*
009100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
009200011113     C                   Z-ADD     150           LENGH            15 5
009300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009400000616     C                   PARM                    depcmd
009500000616     C                   PARM                    LENGH
009600000724     C*
009700000724     C                   endif
009800000724     C                   endif
009900000616     C*
010000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010100000616     C                   movel(p)  CMD(2)        depcmd
010200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
010300000616     C*
010400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
010500011113     C                   Z-ADD     150           LENGH            15 5
010600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010700000616     C                   PARM                    depcmd
010800000616     C                   PARM                    LENGH
010900000616     C*
011000070530     C                   if        not %open(titvy9ps)
011100070530     C                   open      titvy9ps
011200000616     C                   except    testdett
011300000613     C                   endif
011400000613     C*
011500070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
011600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011700000613     C*
011800010202     C* Effettuo la chiamata al CLLE preposto
011900050128     C                   call(e)   'TITVVTC'
012000010202     C                   parm                    parccm
012100010202     C                   parm                    parmbr
012200010202     C                   parm      '2'           paropz
012300010202     C*
012400070530     C                   if        %open(titvy9ps)
012500000616     C                   except    findett
012600070530     C                   close     titvy9ps
012700000613     C                   endif
012800000616     C*
012900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013000011113     C                   Z-ADD     150           LENGH            15 5
013100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013200000616     C                   PARM                    CMD(3)
013300000616     C                   PARM                    LENGH
013400000616     C*
013500000801     C
013600010201     C                   seton                                        LR
013700000613
013800000613     C*--------------------------------------------------------
013900000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014000000613     C*--------------------------------------------------------
014100000621     C     STPR          BEGSR
014200000613     C*
014300070530     C                   if        not %open(titvy9p)
014400070530     C                   open      titvy9p
014500990915     C                   endif
014600990915     C*
014700990915     C                   except    riepilogo
014800990915     C*
014900070530     C                   if        %open(titvy9p)
015000070530     C                   close     titvy9p
015100990914     C                   endif
015200990910     C*
015300000613     C                   ENDSR
015400000613     C***
015500990908
015600000801
015700910830     C*--------------------------------------------------------
015800070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
015900910830     C*--------------------------------------------------------
016000070530     C     RWFILE        BEGSR
016100990910     C*
016200990914     C                   if        not %open(tivin00r)
016300990908     C                   open      tivin00r
016400990914     C                   endif
016500021113     C                   if        not %open(fivabwwr)
016600021113     C                   open      fivabwwr
016700990914     C                   endif
016800070530     C*
016900021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
017000020305     C                   exsr      prevat
017100010201     C*
017200010202     C                   if        chkcall = '0'
017300010202     C*
017400021113     C                   if        not %open(fivatwwr)
017500021113     C                   open      fivatwwr
017600010201     C                   endif
017700990910     C*
017800010201     C                   clear                   §CTROKVB          5 0
017900020305     C                   clear                   §CTROKVT          5 0
018000000801     C                   clear                   §CTRMO            5 0
018100000801     C                   clear                   §CTRNO            5 0
018200990910     C*
018300020725     C/EXEC SQL
018400020725     C+ declare C1 cursor for select
018500070806     C+ vindta, vinflg, substr(vindta, 7, 12) as sped, rrn(tivin00r)
018600060223     C+ from tivin00r
018700060223     C+ order by sped
018800020725     C+ for read only
018900020725     C/END-EXEC
019000020725     C
019100020725     C/EXEC SQL
019200020725     C+ open C1
019300020725     C/END-EXEC
019400020725     C
019500020725     C/EXEC SQL
019600070530     C+ Fetch C1 into :INPUT_DS
019700020725     C/END-EXEC
019800020725     C*
019900020725     C                   dow       sqlcod = *zeros
020000990913     C*
020100020725     C                   if        vindta > *blanks
020200000613     C                   add       1             rrnum
020300000801     C*
020400020725     C                   if        vinflg = *blanks
020500020725     C                             or vinflg = '0'
020600020725     C                             or vinflg = '2'
020700000801     C*
020800020725     C                   clear                   x_vinmsg
020900020725     C                   eval      x_vinflg = '1'
021000010305     C*
021100010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
021200070806     C                   EVAL      PiStr=%trim(%subst(vindta:7:8))
021300020305     C                   MOVEL(p)  PiStr         curspe
021400010305     C*
021500010305     C                   if        depspe = *blanks                             => 1° giro
021600010305     C                   eval      depspe = curspe                              => memorizz. spediz
021700020725     C                   clear                   tivinds
021800020725     C                   exsr      inzvar
021900020725     C                   exsr      defcam
022000070806     C                   exsr      repnum
022100020305     C                   exsr      impvab
022200070530     C                   exsr      wrivat                                       => carico VAT
022300010305     C                   else
022400020725     C                   if        depspe <> curspe                             => rottura di spediz
022500010305     C                   eval      depspe = curspe                              => memorizz. spediz
022600020305     C                   exsr      wrivab
022700020725     C                   clear                   tivinds
022800020725     C                   exsr      inzvar
022900020725     C                   exsr      defcam
023000070806     C                   exsr      repnum
023100020305     C                   exsr      impvab
023200070530     C                   exsr      wrivat                                       => carico VAT
023300020305     C                   else                                                   => x stessa spediz
023400020305     C                   exsr      impvab
023500070530     C                   exsr      wrivat                                       => carico VAT
023600010305     C                   endif
023700010305     C                   endif
023800010305     C                   endif
023900000905     C*
024000000905     C                   else
024100020725     C                   eval      x_vinflg = '1'
024200000905     C                   endif
024300000905     C*
024400020725     C     VINRRN        chain     tivin000
024500020725     C                   if        %found(tivin00r)
024600020725     C                   eval      y_vinflg = x_vinflg
024700020725     C                   eval      y_vinmsg = x_vinmsg
024800020725     C                   update    tivin000
024900020725     C                   endif
025000020725     C*
025100020725     C/EXEC SQL
025200070530     C+ Fetch C1 into :INPUT_DS
025300020725     C/END-EXEC
025400020725     C*
025500020725     C                   enddo
025600020725     C*
025700020725     C/EXEC SQL
025800020725     C+ close C1
025900020725     C/END-EXEC
026000000905     C*
026100020305     C* Scarico i VAB rimasti "in sospeso"
026200020305     C                   exsr      wrivab
026300010202     C*
026400010202     C                   endif
026500990910
026600990910     C* Se non ci sono record con errori ...
026700000710     C                   if        §ctrno = 0
026800990910     C* ... restituisco esito OK.
026900990921     C                   eval      wrkesito = '0'
027000990910     C                   else
027100010201     C                   if        §ctrokvb > 0
027200990921     C                   eval      wrkesito = '1'
027300000710     C                   else
027400000710     C                   eval      wrkesito = '2'
027500990910     C                   endif
027600000710     C                   endif
027700990910     C*
027800990914     C                   if        %open(tivin00r)
027900990908     C                   close     tivin00r
028000990914     C                   endif
028100021113     C                   if        %open(fivabwwr)
028200021113     C                   close     fivabwwr
028300990914     C                   endif
028400021113     C                   if        %open(fivatwwr)
028500021113     C                   close     fivatwwr
028600010201     C                   endif
028700990910     C*
028800010201     C                   if        §ctrokvb > 0
028900000724     C                             and vlrpoi <> *zeros
029000010202     C                   exsr      invio
029100990920     C                   endif
029200990920     C*
029300910830     C                   ENDSR
029400000613     C***
029500010305
029600010305     C*----------------------------------------------------*
029700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
029800010305     C*----------------------------------------------------*
029900020305     C     WRIVAB        BEGSR
030000010305     C*
030100021113     C                   write     fivab000                                     => scarico il VAB
030200070530     C*
030300070530     C* Insieme alla testata scarico le estensini destinatario su VAT - tipi record A e B
030400070530     C                   if        wVAT_A <> *blanks
030500070530     C                   eval      VATTRC = 'A'
030600070530     C                   eval      VATNOT = wVAT_A
030700070530     C                   write     FIVAT000
030800070530     C                   endif
030900070530     C*
031000070530     C                   if        wVAT_B <> *blanks
031100070530     C                   eval      VATTRC = 'B'
031200070530     C                   eval      VATNOT = wVAT_B
031300070530     C                   write     FIVAT000
031400070530     C                   endif
031500010305     C*
031600010305     C                   ENDSR
031700990920
031800000801     C*----------------------------------------------------*
031900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032000000801     C*----------------------------------------------------*
032100010201     C     INZVAR        BEGSR
032200000801     C*
032300010201     C                   Z-ADD     *zeros        Num5_0
032400070530     C                   MOVEL     *blanks       wVAT_A           30
032500070530     C                   MOVEL     *blanks       wVAT_B           30
032600020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032700020725     C                   MOVEL     '0'           FlgCAS            1
032800000801     C*
032900000801     C                   ENDSR
033000000801     C*----------------------------------------------------*
033100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
033200000801     C*----------------------------------------------------*
033300020904     C     DEFCAM        BEGSR
033400000801     C*
033500021113     C                   CLEAR                   FIVAB000
033600021113     C                   CLEAR                   FIVAT000
033700070530     C                   Z-ADD     0054201       VABCCM
033800070530     C                   Z-ADD     0054201       VATCCM
033900070530     C                   Z-ADD     005           VABLNP
034000070530     C                   Z-ADD     005           VATLNP
034100070807     C                   Z-ADD     000           VABCTR
034200020305     C                   MOVEL     '7Q'          VABCTM
034300071009     C                   MOVEL     'C'           VABTSP
034400070530     C                   MOVEL     '1'           VABCBO
034500070222     C* ... e poi verifico se sono stati passati come parametri
034600070222     C                   IF        vlrppt > *blanks
034700070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
034800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034900070222     C                   EXSR      CHKNUM
035000070222     C                   IF        PiInt=*on
035100070222     C                   Z-ADD     PiVal         VABCCM
035200070222     C                   Z-ADD     PiVal         VATCCM
035300070222     C                   ENDIF
035400070222     C                   ENDIF
035500070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
035600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035700070222     C                   EXSR      CHKNUM
035800070222     C                   IF        PiInt=*on
035900070222     C                   Z-ADD     PiVal         VABLNP
036000070222     C                   Z-ADD     PiVal         VATLNP
036100070222     C                   ENDIF
036200070222     C                   ENDIF
036300070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
036400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
036500070222     C                   EXSR      CHKNUM
036600070222     C                   IF        PiInt=*on
036700070222     C                   Z-ADD     PiVal         VABCTR
036800070222     C                   ENDIF
036900070222     C                   ENDIF
037000070222     C                   ENDIF
037100000801     C*
037200000801     C                   ENDSR
037300070806     C*----------------------------------------------------*
037400070806     C*  REPERIMENTO NUMERATORE X NUMERO SPEDIZIONE VAB/VAT
037500070806     C*----------------------------------------------------*
037600070806     C     REPNUM        BEGSR
037700070806     C*
037800070806     C* NSP => Stacco un numeratore da AZNUM
037900070806     C                   clear                   TRUL33DS
038000070806     C                   eval      I33OPE = *zeros
038100070806     C                   eval      I33CNU = 302
038200070806     C                   eval      I33NUM = 1
038300070806     C                   movel     TRUL33DS      KPJBU
038400070806     C                   call      'TRUL33R'
038500070806     C                   parm                    KPJBA
038600070806     C                   movel     KPJBU         TRUL33DS
038700070806     C                   if        O33ERR = *zeros
038800070806     C                   z-add     O33NRF        VABNSP
038900070806     C                   z-add     O33NRF        VATNSP
039000070806     C                   else
039100070806     C                   SETON                                        31
039200070806     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039300070806     C                             + ' ' + 'VABNSP VATNSP'
039400070806     C                   endif
039500070806     C*
039600070806     C                   ENDSR
039700000801     C*----------------------------------------------------*
039800021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
039900000801     C*----------------------------------------------------*
040000010201     C     IMPVAB        BEGSR
040100070530     C*
040200070530     C                   SETOFF                                       3132
040300021113     C*
040400021113     C                   MOVE(P)   vlrpoi        VABFGS
040500021113     C                   MOVE(P)   vlrpoi        VATFGS
040600020725     C*
040700070530     C* Campi anno e mese/giorno
040800070530     C                   MOVEL     datcor        VABAAS
040900070530     C                   MOVEL     datcor        VATAAS
041000070530     C                   MOVE      datcor        VABMGS
041100070530     C*
041200070530     C* Reperimento campi ALFA
041300070530     C*
041400070530     C* Considerazioni sulla ragione sociale del destinatario da indicare
041500070530     C                   EVAL      VABRSD=%trim(%subst(vindta:322:30))
041600070530     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
041700070530     C     '@':'A'       XLATE     VABRSD        VABRSD
041800070530     C* ==
041900070530     C                   EVAL      VABRD2=%trim(%subst(vindta:669:27))
042000070530     C                   EVAL      VABIND=%trim(%subst(vindta:352:30))
042100070530     C                   EVAL      VABLOD=%trim(%subst(vindta:412:30))
042200070530     C                   MOVEL     *BLANKS       wPRD             30
042300070530     C                   EVAL      wPRD=%trim(%subst(vindta:442:30))
042400070530     C     '<':' '       XLATE     wPRD          wPRD
042500070530     C     '>':' '       XLATE     wPRD          wPRD
042600070530     C     '(':' '       XLATE     wPRD          wPRD
042700070530     C     ')':' '       XLATE     wPRD          wPRD
042800070530     C                   EVAL      VABPRD=%trim(wPRD)
042900070530     C                   EVAL      VABRMA=%trim(%subst(vindta:7:12))
043000070530     C                   SELECT
043100070530     C                   WHEN      %trim(%subst(vindta:701:1))='F'
043200070530     C                   EVAL      VABCBO='1'
043300070530     C                   WHEN      %trim(%subst(vindta:701:1))='A'
043400070530     C                   EVAL      VABCBO='2'
043500070530     C                   ENDSL
043600070530     C*
043700070806     C* RMN
043800070530     C                   EVAL      PiStr=%trim(%subst(vindta:7:12))
043900070530     C                   EXSR      CHKNUM
044000070530     C                   IF        PiInt=*on
044100070530     C                   Z-ADD     PiVal         VABRMN
044200070530     C                   ELSE
044300070806     C                   SETON                                        32
044400070530     C                   Z-ADD     *zeros        VABRMN
044500070530     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044600070806     C                             + ' ' + 'VABRMN'
044700070530     C                   ENDIF
044800070530     C* CAD
044900070530     C                   EVAL      PiStr=%trim(%subst(vindta:472:9))
045000070530     C                   EXSR      CHKNUM
045100070530     C                   IF        PiInt=*on
045200070530     C                   Z-ADD     PiVal         Num5_0
045300070530     C                   MOVEL(p)  Num5_0        VABCAD
045400070530     C                   ELSE
045500070530     C                   SETON                                        32
045600070530     C                   EVAL      VABCAD = *zeros
045700070530     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045800070530     C                             + ' ' + 'VABCAD'
045900070530     C                   ENDIF
046000070530     C* NCL
046100070530     C                   EVAL      PiStr=%trim(%subst(vindta:758:5))
046200070530     C                   EXSR      CHKNUM
046300070530     C                   IF        PiInt=*on
046400070530     C                   Z-ADD     PiVal         VABNCL
046500070530     C                   ELSE
046600070530     C                   SETON                                        32
046700070530     C                   Z-ADD     *zeros        VABNCL
046800070530     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046900070530     C                             + ' ' + 'VABNCL'
047000070530     C                   ENDIF
047100070530     C* PKB
047200070530     C                   EVAL      PiStr=%trim(%subst(vindta:718:8))
047300070530     C                   EXSR      CHKNUM
047400070530     C                   IF        PiNum=*on
047500070530     C                   Z-ADD     PiVal         VABPKB
047600070530     C                   ELSE
047700070530     C                   SETON                                        32
047800070530     C                   Z-ADD     *zeros        VABPKB
047900070530     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048000070530     C                             + ' ' + 'VABPKB'
048100070530     C                   ENDIF
048200070530     C*
048300070530     C* Campi estensione destinatario su VAT - tipi record A e B
048400070530     C* VAT - A
048500070530     C                   EVAL      wVAT_A=%trim(%subst(vindta:504:22))
048600070530     C* VAT - B
048700070530     C                   EVAL      wVAT_B=%trim(%subst(vindta:526:7))
048800070530     C*
048900070530     C* Considerazioni finali su CBO/CAS
049000070530     C                   IF        FlgCAS = '1'
049100070530     C                   IF        VABCBO = '1'
049200070530     C                   EVAL      VABCBO = '4'
049300070530     C                   ENDIF
049400070530     C                   IF        VABCBO = '2'
049500070530     C                   EVAL      VABCBO = '6'
049600070530     C                   ENDIF
049700070530     C                   ENDIF
049800020305     C*
049900011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
050000011113     C                   EXSR      CHKIMPDIV
050100010202     C*
050200000801     C* Ebbene...
050300000801     C                   ADD       1             §CTRMO
050400070530     C                   IF        *in31 <> *zeros OR
050500070530     C                             *in32 <> *zeros
050600000801     C                   ADD       1             §CTRNO
050700000801     C                   EVAL      recko = vindta
050800000801     C                   EXCEPT    rigadett
050900020725     C                   EVAL      x_vinflg = '2'
051000000801     C                   ELSE
051100010201     C                   ADD       1             §CTROKVB
051200000801     C                   ENDIF
051300000801     C*
051400000801     C                   ENDSR
051500010201     C*----------------------------------------------------*
051600021113     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
051700010201     C*----------------------------------------------------*
051800020305     C     WRIVAT        BEGSR
051900010201     C*
052000021113     C* Valorizzo l buffer di scrittura del FIVAT
052100070530     C                   if        %subst(vindta:575:18) <> *blanks
052200070530     C                   eval      VATTRC = 'E'
052300070530     C                   eval      VATNOT = %trim(%subst(vindta:575:18))
052400021113     C                   write     FIVAT000
052500020725     C                   endif
052600010201     C*
052700010201     C                   ENDSR
052800020904
052900020904
053000010202     C*----------------------------------------------------*
053100021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
053200010202     C*----------------------------------------------------*
053300020305     C     PREVAT        BEGSR
053400010202     C*
053500021113     C* Compongo il nome del membro da dare al FIVATWWR
053600010202     C                   eval      parmbr = vlrhdl
053700010202     C                   movel     'M'           parmbr
053800070530     C                   eval      parccm = %subst(vlrKSC:2:7)
053900010202     C                   eval      paropz = '1'
054000010202     C* Effettuo la chiamata al CLLE preposto
054100050128     C                   call(e)   'TITVVTC'
054200010202     C                   parm                    parccm
054300010202     C                   parm                    parmbr
054400010202     C                   parm                    paropz
054500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
054600010202     C                   if        %error
054700010202     C                   movel     '1'           chkcall
054800010202     C                   else
054900010202     C                   movel     '0'           chkcall
055000010202     C                   endif
055100010202     C*
055200010202     C                   ENDSR
055300000801     C*----------------------------------------------------*
055400000801     C*  CONTROLLO NUMERICITA' CAMPI
055500000801     C*----------------------------------------------------*
055600000801     C     CHKNUM        BEGSR
055700000801     C*
055800000801     C                   call(e)   'ISNUMERIC'
055900000801     C                   PARM                    PiStr            30
056000070530     C                   PARM      '.'           PiDecChr          1
056100000801     C                   PARM      *ZEROS        PiVal            30 9
056200000801     C                   PARM      '0'           PiInt             1
056300000801     C                   PARM      '0'           PiNum             1
056400000801     C                   IF        %error
056500000801     C                   EVAL      PiInt=*off
056600000801     C                   ENDIF
056700000801     C*
056800000801     C                   ENDSR
056900000801     C***
057000000801
057100011113
057200011113     C*----------------------------------------------------*
057300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
057400011113     C*----------------------------------------------------*
057500011113     C     CHKIMPDIV     BEGSR
057600011113     C*
057700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
057800011113     C                   Z-ADD     *zeros        wrkDec            9 9
057900011113     C*
058000011113     C* Come prima cosa effettuo considerazioni sulla divisa
058100011113     C                   IF        vabIAS > *zeros
058200011113     C                   IF        vabVAS <> 'EUR'
058300011113     C                   EVAL      vabVAS =  'ITL'
058400011113     C                   ENDIF
058500011113     C                   ENDIF
058600011113     C*
058700011113     C                   IF        vabCAS > *zeros
058800011113     C                   IF        vabVCA <> 'EUR'
058900011113     C                   EVAL      vabVCA =  'ITL'
059000011113     C                   ENDIF
059100011113     C                   ENDIF
059200011113     C*
059300011113     C                   IF        vabVMD > *zeros
059400020305     C                   IF        vabVAD <> 'EUR'
059500011113     C                   EVAL      vabVAD =  'ITL'
059600011113     C                   ENDIF
059700011113     C                   ENDIF
059800011113     C*
059900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
060000011113     C                   Z-ADD     vabIAS        wrkDec
060100011113     C                   IF        wrkDec > *zeros
060200011113     C                   IF        vabVAS = 'ITL'
060300011113     C                   EVAL      vabIAS = *zeros
060400011113     C                   ENDIF
060500011113     C                   ENDIF
060600011113     C*
060700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
060800011113     C                   Z-ADD     vabCAS        wrkDec
060900011113     C                   IF        wrkDec > *zeros
061000011113     C                   IF        vabVCA = 'ITL'
061100011113     C                   EVAL      vabCAS = *zeros
061200011113     C                   ENDIF
061300011113     C                   ENDIF
061400011113     C*
061500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
061600011113     C                   Z-ADD     vabVMD        wrkDec
061700011113     C                   IF        wrkDec > *zeros
061800011113     C                   IF        vabVAD = 'ITL'
061900011113     C                   EVAL      vabVMD = *zeros
062000011113     C                   ENDIF
062100011113     C                   ENDIF
062200011113     C*
062300011113     C                   ENDSR
062400011113     C***
062500011113
062600011113
062700000801
062800000801
062900990920      /TITLE Invio dei dati al punto operativo.
063000010202     C     invio         BEGSR
063100990920     C*
063200021113     C* 1° invio FIVAT
063300010201     C                   reset                   dscmz
063400010201     C                   move      vlrpoi        cmzdst
063500021113     C                   eval      cmzfld = 'FIVATWWR'
063600010201     C                   eval      cmzmbd = vlrhdl
063700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063800021009     C***                if        prmfir = *blanks
063900021113     C                   eval      cmzfla = 'FIVAT00F'
064000021113     C                   eval      cmzmba = 'FIVAT00F'
064100021009     C***                else
064200021009     C***                eval      cmzfla = prmfir
064300021009     C***                eval      cmzmba = prmfir
064400021009     C***                endif
064500010201     C                   eval      cmznrr = *zeros
064600020305     C                   move      §ctrokvt      cmznrr
064700021018     C                   eval      cmzlba = vlrfl1
064800010201     C                   call(e)   'TIS711C'
064900010201     C                   parm                    dscmz
065000010201     C                   parm      *blanks       esito
065100010205     C                   if        %error
065200010205     C                             or cmzerr = '1'
065300010205     C                             or esito  = '1'
065400010205     C                   eval      wrkesito = '3'
065500010205     C                   else
065600010201     C*
065700021113     C* 2° invio FIVAB
065800010201     C                   reset                   dscmz
065900010201     C                   move      vlrpoi        cmzdst
066000010201     C                   eval      cmzfld = vlrfou
066100010201     C                   eval      cmzmbd = vlrhdl
066200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066300021009     C***                if        prmfir = *blanks
066400021113     C                   eval      cmzfla = 'FIVAB00F'
066500021113     C                   eval      cmzmba = 'FIVAB00F'
066600021009     C***                else
066700021009     C***                eval      cmzfla = prmfir
066800021009     C***                eval      cmzmba = prmfir
066900021009     C***                endif
067000010201     C                   eval      cmznrr = *zeros
067100010201     C                   move      §ctrokvb      cmznrr
067200021018     C                   eval      cmzlba = vlrfl1
067300010201     C                   call(e)   'TIS711C'
067400010201     C                   parm                    dscmz
067500010201     C                   parm      *blanks       esito
067600010201     C                   if        %error
067700010201     C                             or cmzerr = '1'
067800010201     C                             or esito  = '1'
067900010201     C                   eval      wrkesito = '3'
068000010201     C                   endif
068100010205     C                   endif
068200990920     C*
068300000613     C                   ENDSR
068400000613     C***
068500070411
068600070411     C     *pssr         BEGSR
068700070411     C*
068800070411     C                   if        %open(tivin00r)
068900070411     C                   close     tivin00r
069000070411     C                   endif
069100070411     C                   if        %open(fivabwwr)
069200070411     C                   close     fivabwwr
069300070411     C                   endif
069400070411     C                   if        %open(fivatwwr)
069500070411     C                   close     fivatwwr
069600070411     C                   endif
069700070411     C*
069800070411     C* Effettuo la chiamata al CLLE preposto
069900070411     C                   call(e)   'TITVVTC'
070000070411     C                   parm                    parccm
070100070411     C                   parm                    parmbr
070200070411     C                   parm      '2'           paropz
070300070411     C*
070400070411     C                   eval      wrkesito = '2'
070500070411     C*
070600070411     C                   seton                                        LR
070700070411     C*
070800070411     C                   ENDSR     '*CANCL'
070900070411     C***
071000070411
071100990910
071200000613     C     *inzsr        BEGSR
071300990910     C*
071400990910     C     *entry        plist
071500990920     C                   parm                    tivlrds
071600990921     C                   parm      wrkesito      esito
071700000724     C                   parm                    prmlit
071800000710     C                   parm                    prmfir
071900000613     C*
072000000830     C* CALCOLA LA DATA CORRENTE
072100000830     C                   time                    wn14             14 0
072200000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
072300000830     C                   z-add     wn8           g08dat
072400000830     C                   z-add     *zeros        g08inv
072500000830     C                   movel     '0'           g08err
072600000830     C                   call      'XSRDA8'
072700000830     C                   parm                    wlbda8
072800000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
072900070530     C*
073000070530     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
073100070530     C
073200070530     C/EXEC SQL
073300070530     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
073400070530     C/END-EXEC
073500000830     C*
073600000613     C                   ENDSR
073700000613     C***
073800990908
073900070530     Otitvy9p   E            riepilogo         2
074000990915     O                                              'Upload via Internet'
074100990915     O                                           +1 'Traduzione TIVIN00R -'
074200021113     O                                           +1 'FIVABWWR/FIVATWWR'
074300010201     O                                           +1 'Report testate bolle'
074400990915     O          E            riepilogo   2
074500990915     O                       wrkdata
074600990915     O                       wrkora              +1
074700990915     O                       procname            +1
074800990915     O          E            riepilogo   2
074900990915     O                                              'Cliente..................:'
075000990915     O                       VABCCM        z     +1
075100990915     O          E            riepilogo   2
075200990920     O                                              'Riferimento Strategi.....:'
075300990920     O                       vlrhdl              +1
075400990915     O          E            riepilogo   2
075500990915     O                                              'Giusti...................:'
075600010201     O                       §CTROKVB      2   +  1
075700990915     O          E            riepilogo   2
075800990915     O                                              'Sbagliati e corretti.....:'
075900971022     O                       §CTRMO        2   +  1
076000990915     O          E            riepilogo   2
076100990915     O                                              'Sbagliati e scartati.....:'
076200971022     O                       §CTRNO        2   +  1
076300000613
076400070530     Otitvy9ps  E            testdett          2
076500000613     O                                              'Upload via Internet'
076600000613     O                                           +1 'Traduzione TIVIN00R -'
076700021113     O                                           +1 'FIVABWWR/FIVATWWR'
076800010201     O                                           +1 'Report testate bolle'
076900000616     O          E            testdett    3
077000000613     O                                           +2 'N° rec'
077100000613     O                                           +3 'Anteprima contenuto'
077200000616     O          E            rigadett    2
077300000613     O                       rrnum               +2
077400000621     O                       recko               +3
077500000616     O          E            findett     2
077600000613     O                       wrkdata
077700000613     O                       wrkora              +1
077800000613     O                       procname            +1
077900000616     O          E            findett     2
078000000613     O                                              'Cliente..................:'
078100000613     O                       VABCCM        z     +1
078200000616     O          E            findett     2
078300000613     O                                              'Riferimento Strategi.....:'
078400000613     O                       vlrhdl              +1
078500000616     O          E            findett     2
078600000613     O                                              'Giusti...................:'
078700010201     O                       §CTROKVB      2   +  1
078800000616     O          E            findett     2
078900000613     O                                              'Sbagliati e corretti.....:'
079000000613     O                       §CTRMO        2   +  1
079100000616     O          E            findett     2
079200000613     O                                              'Sbagliati e scartati.....:'
079300000613     O                       §CTRNO        2   +  1
079400000512** CMD - COMANDI CL
079500070530OVRPRTF FILE(TITVY9P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
079600070530OVRPRTF FILE(TITVY9PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
079700070530DLTOVR FILE(TITVY9P TITVY9PS) LVL(*)
079800000512
079900000512
