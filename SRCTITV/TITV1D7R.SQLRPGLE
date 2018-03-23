000100030625      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVADWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600021113     FFIVABwwr  O    E             DISK    usropn
000700030625     FFIVADwwr  O    E             DISK    usropn
000800080131     FtitvA2p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000080131     FtitvA2ps  O    f  198        PRINTER usropn
001100000621     F                                     oflind(*inob)
001200990908
001300000512     D*------------
001400000512     D* COMANDI
001500000512     D*------------
001600011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700000801     D*----------------------------------------------------
001800000801     D* DICHIARAZIOINE VARIABILI DI WRK
001900000801     D*----------------------------------------------------
002000990920     D dscmz         e ds                  inz
002100990910     D psds           sds
002200990910     D  procname         *PROC
002300990920     D tivlrds       e ds                  extname(tivlr00f)
002400990910     D esito           s              1
002500000724     D prmlit          s             10
002600000710     D prmfir          s             10
002700990921     D wrkesito        s                   like(esito)
002800990915     D wrkdata         s               d
002900990915     D wrkora          s               t
003000000613     D rrnum           s              6  0 INZ(*zeros)
003100000621     D recko           s            150    INZ(*blanks)
003200011113     D depcmd          s            150    INZ(*blanks)
003300080131     D depDest         s              7    INZ(*blanks)
003400080131     D curDest         s              7    INZ(*blanks)
003500010202     D parccm          s              8    INZ(*blanks)
003600010202     D parmbr          s             10    INZ(*blanks)
003700010202     D paropz          s              1    INZ(*blanks)
003800010202     D chkcall         s              1    INZ(*blanks)
003900020725     D tivinds       e ds                  extname(tivin00r) prefix(x_)
004000010213
004100000801     D  Num5_0         s              5  0
004200080131     D  wVADCDP_1      s                   like(VADCDP)
004300080131     D  wVADCDP_2      s                   like(VADCDP)
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
005600030625     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
005700020725     D*------------------
005800080131     D DS_SQL          DS                  INZ
005900080131     D  VINDTA                     2048
006000080131     D  VINFLG                        1
006100080131     D  VINDST                        7
006200080131     D  VINRRN                        8  0
006300020725     D*
006400080131
006500080131     D*------------------
006600080131     D* DEFINIZIONE DS DI LETTURA CAMPO DATI ARCHIVIO D INPUT
006700080131     D*------------------
006800080131     D DS_INPUT        DS                  INZ
006900080131     D  in_CCM                        7
007000080131     D  in_LNP                        3
007100080131     D  in_NSP                        7
007200080131     D  in_RSD                       35
007300080131     D  in_RD2                       35
007400080131     D  in_IND                       35
007500080131     D  in_CAD                        9
007600080131     D  in_LOD                       35
007700080131     D  in_PRD                        2
007800080131     D  in_NZD                        3
007900080131     D  in_CTR                        3
008000080131     D  in_NAS                       15
008100080131     D  in_NCL                        5
008200080131     D  in_FILLER001                  1
008300080131     D  in_PKB                        8
008400080131     D  in_RMN                       15
008500080131     D  in_RMA                       15
008600080131     D*
008700990908
008800010201
008900990915     C                   time                    wrkdata
009000990915     C                   time                    wrkora
009100000913     C                   reset                   rrnum
009200990921     C                   reset                   esito
009300990921     C                   reset                   wrkesito
009400000724     C*
009500000724     C* SE OCCORRE SPEDIRE IN FILIALE
009600000724     C                   if        vlrpoi <> *zeros
009700000724     C*
009800000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009900000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010000000724     C     vlrpoi        chain     azorg01l
010100000724     C                   if        %found
010200000616     C                   movel(p)  CMD(1)        depcmd
010300020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010400000616     C*
010500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010600011113     C                   Z-ADD     150           LENGH            15 5
010700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010800000616     C                   PARM                    depcmd
010900000616     C                   PARM                    LENGH
011000000724     C*
011100000724     C                   endif
011200000724     C                   endif
011300000616     C*
011400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011500000616     C                   movel(p)  CMD(2)        depcmd
011600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011700000616     C*
011800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
011900011113     C                   Z-ADD     150           LENGH            15 5
012000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012100000616     C                   PARM                    depcmd
012200000616     C                   PARM                    LENGH
012300000616     C*
012400080131     C                   if        not %open(titvA2ps)
012500080131     C                   open      titvA2ps
012600000616     C                   except    testdett
012700000613     C                   endif
012800000613     C*
012900030625     C                   EXSR      RWADI                                        LETT/SCR. VAB/VAD
013000000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
013100000616     C*
013200081006     C                   exsr      endela
013300081006     C*
013400000801     C
013500010201     C                   seton                                        LR
013600000613
013700000613     C*--------------------------------------------------------
013800000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013900000613     C*--------------------------------------------------------
014000000621     C     STPR          BEGSR
014100000613     C*
014200080131     C                   if        not %open(titvA2p)
014300080131     C                   open      titvA2p
014400990915     C                   endif
014500990915     C*
014600990915     C                   except    riepilogo
014700990915     C*
014800080131     C                   if        %open(titvA2p)
014900080131     C                   close     titvA2p
015000990914     C                   endif
015100990910     C*
015200000613     C                   ENDSR
015300000613     C***
015400990908
015500000801
015600910830     C*--------------------------------------------------------
015700030625     C* RWADI    LEGGE tivin00r E SCRIVE FIVABWWR e FIVADWWR  *
015800910830     C*--------------------------------------------------------
015900030625     C     RWADI         BEGSR
016000990910     C*
016100990914     C                   if        not %open(tivin00r)
016200990908     C                   open      tivin00r
016300990914     C                   endif
016400021113     C                   if        not %open(fivabwwr)
016500021113     C                   open      fivabwwr
016600990914     C                   endif
016700030625     C* Eseguo operazioni di aggiunta nuovo membro in FIVADWWR
016800030625     C                   exsr      prevad
016900010201     C*
017000010202     C                   if        chkcall = '0'
017100010202     C*
017200030625     C                   if        not %open(fivadwwr)
017300030625     C                   open      fivadwwr
017400010201     C                   endif
017500990910     C*
017600010201     C                   clear                   §CTROKVB          5 0
017700020305     C                   clear                   §CTROKVT          5 0
017800000801     C                   clear                   §CTRMO            5 0
017900000801     C                   clear                   §CTRNO            5 0
018000990910     C*
018100020725     C*
018200020725     C/EXEC SQL
018300020725     C+ declare C1 cursor for select
018400080131     C+ vindta, vinflg, substr(vindta, 11, 7) as dest, rrn(tivin00r)
018500030625     C+ from tivin00r order by dest
018600020725     C+ for read only
018700020725     C/END-EXEC
018800020725     C
018900020725     C/EXEC SQL
019000020725     C+ open C1
019100020725     C/END-EXEC
019200020725     C
019300020725     C/EXEC SQL
019400080131     C+ Fetch C1 into :DS_SQL
019500020725     C/END-EXEC
019600020725     C*
019700020725     C                   dow       sqlcod = *zeros
019800990913     C*
019900020725     C                   if        vindta > *blanks
020000000613     C                   add       1             rrnum
020100000801     C*
020200020725     C                   if        vinflg = *blanks
020300020725     C                             or vinflg = '0'
020400020725     C                             or vinflg = '2'
020500000801     C*
020600020725     C                   clear                   x_vinmsg
020700020725     C                   eval      x_vinflg = '1'
020800010305     C*
020900010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
021000080131     C                   EVAL      curDest=%trim(%subst(vindta:11:7))
021100010305     C*
021200030625     C                   if        depDest = *blanks                            => 1° giro
021300030625     C                   eval      depDest = curDest                            => memorizz. dest.
021400020725     C                   clear                   tivinds
021500030625     C                   exsr      inzvar
021600030625     C                   exsr      defcam
021700020305     C                   exsr      impvab
021800030625     C                   exsr      wrivad                                       => carico VAD
021900010305     C                   else
022000030625     C                   if        depDest <> curDest                           => rottura di dest.
022100030625     C                   eval      depDest  = curDest                           => memorizz. dest.
022200020305     C                   exsr      wrivab
022300020725     C                   clear                   tivinds
022400020725     C                   exsr      inzvar
022500020725     C                   exsr      defcam
022600020305     C                   exsr      impvab
022700030625     C                   exsr      wrivad                                       => carico VAD
022800030625     C                   else                                                   => x stesso dest.
022900020305     C                   exsr      impvab
023000030625     C                   exsr      wrivad                                       => carico VAD
023100010305     C                   endif
023200010305     C                   endif
023300010305     C                   endif
023400000905     C*
023500000905     C                   else
023600020725     C                   eval      x_vinflg = '1'
023700000905     C                   endif
023800000905     C*
023900020725     C     VINRRN        chain     tivin000
024000020725     C                   if        %found(tivin00r)
024100020725     C                   eval      y_vinflg = x_vinflg
024200020725     C                   eval      y_vinmsg = x_vinmsg
024300020725     C                   update    tivin000
024400020725     C                   endif
024500020725     C*
024600020725     C/EXEC SQL
024700080131     C+ Fetch C1 into :DS_SQL
024800020725     C/END-EXEC
024900020725     C*
025000020725     C                   enddo
025100020725     C*
025200020725     C/EXEC SQL
025300020725     C+ close C1
025400020725     C/END-EXEC
025500000905     C*
025600020305     C* Scarico i VAB rimasti "in sospeso"
025700020305     C                   exsr      wrivab
025800010202     C*
025900010202     C                   endif
026000990910
026100990910     C* Se non ci sono record con errori ...
026200000710     C                   if        §ctrno = 0
026300990910     C* ... restituisco esito OK.
026400990921     C                   eval      wrkesito = '0'
026500990910     C                   else
026600010201     C                   if        §ctrokvb > 0
026700990921     C                   eval      wrkesito = '1'
026800000710     C                   else
026900000710     C                   eval      wrkesito = '2'
027000990910     C                   endif
027100000710     C                   endif
027200990910     C*
027300010201     C                   if        §ctrokvb > 0
027400000724     C                             and vlrpoi <> *zeros
027500010202     C                   exsr      invio
027600990920     C                   endif
027700990920     C*
027800910830     C                   ENDSR
027900000613     C***
028000081006
028100081006     C*----------------------------------------------------*
028200081006     C*  SCARICAMENTO BUFFER RECORDS VAB
028300081006     C*----------------------------------------------------*
028400081006     C     WRIVAB        BEGSR
028500081006     C*
028600081006     C                   write     fivab000                                     => scarico il VAB
028700081006     C*
028800081006     C                   ENDSR
028900081006
029000000801     C*----------------------------------------------------*
029100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029200000801     C*----------------------------------------------------*
029300010201     C     INZVAR        BEGSR
029400000801     C*
029500010201     C                   Z-ADD     *zeros        Num5_0
029600080131     C                   MOVEL     *blanks       wVADCDP_1
029700080131     C                   MOVEL     *blanks       wVADCDP_2
029800000801     C*
029900000801     C                   ENDSR
030000000801     C*----------------------------------------------------*
030100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
030200000801     C*----------------------------------------------------*
030300020904     C     DEFCAM        BEGSR
030400000801     C*
030500021113     C                   CLEAR                   FIVAB000
030600030625     C                   CLEAR                   FIVAD000
030700080131     C                   Z-ADD     0054370       VABCCM
030800080131     C                   Z-ADD     0054370       VADCCM
030900080131     C                   Z-ADD     005           VABLNP
031000080131     C                   Z-ADD     005           VADLNP
031100080131     C                   Z-ADD     300           VABCTR
031200030625     C                   MOVEL     'DT'          VABCTM
031300080131     C                   Z-ADD     3,3           VABPKB
031400080131     C                   Z-ADD     2             VABNCL
031500080131     C*
031600080131     C* ... e poi verifico se sono stati passati come parametri
031700080131     C                   IF        vlrppt > *blanks
031800080131     C                   IF        %subst(vlrppt:1:7) <> *blanks
031900080131     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032000080131     C                   EXSR      CHKNUM
032100080131     C                   IF        PiInt=*on
032200080131     C                   Z-ADD     PiVal         VABCCM
032300080131     C                   Z-ADD     PiVal         VADCCM
032400080131     C                   ENDIF
032500080131     C                   ENDIF
032600080131     C                   IF        %subst(vlrppt:8:3) <> *blanks
032700080131     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032800080131     C                   EXSR      CHKNUM
032900080131     C                   IF        PiInt=*on
033000080131     C                   Z-ADD     PiVal         VABLNP
033100080131     C                   Z-ADD     PiVal         VADLNP
033200080131     C                   ENDIF
033300080131     C                   ENDIF
033400080131     C                   IF        %subst(vlrppt:11:3) <> *blanks
033500080131     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033600080131     C                   EXSR      CHKNUM
033700080131     C                   IF        PiInt=*on
033800080131     C                   Z-ADD     PiVal         VABCTR
033900080131     C                   ENDIF
034000080131     C                   ENDIF
034100080131     C                   IF        %subst(vlrppt:14:15) <> *blanks
034200080131     C                   EVAL      wVADCDP_1=%trim(%subst(vlrppt:14:15))
034300080131     C                   ENDIF
034400080131     C                   IF        %subst(vlrppt:29:15) <> *blanks
034500080131     C                   EVAL      wVADCDP_2=%trim(%subst(vlrppt:29:15))
034600080131     C                   ENDIF
034700080131     C                   ENDIF
034800000801     C*
034900000801     C                   ENDSR
035000000801     C*----------------------------------------------------*
035100030625     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
035200000801     C*----------------------------------------------------*
035300010201     C     IMPVAB        BEGSR
035400010305     C*
035500000801     C                   Z-ADD     *zeros        errore            1 0
035600021113     C*
035700021113     C                   MOVE(P)   vlrpoi        VABFGS
035800030625     C                   MOVE(P)   vlrpoi        VADFGS
035900020725     C*
036000020725     C* Campi anno e mese/giorno
036100030625     C                   MOVEL     datcor        VABAAS
036200030625     C                   MOVEL     datcor        VADAAS
036300030625     C                   MOVE(P)   datcor        VABMGS
036400080131     C*
036500080131     C* Ridefinisco il campo flat dei dati d input
036600080131     C                   EVAL      DS_INPUT = vindta
036700020725     C*
036800020725     C* Campi relativi al destinatario
036900080131     C                   EVAL      VABRSD=in_RSD
037000020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
037100020117     C     '@':'A'       XLATE     VABRSD        VABRSD
037200020117     C* ==
037300080131     C                   EVAL      VABRD2=in_RD2
037400080131     C                   EVAL      VABIND=in_IND
037500080131     C                   EVAL      VABLOD=in_LOD
037600080131     C                   EVAL      VABCAD=in_CAD
037700080131     C                   EVAL      VABPRD=in_PRD
037800080131     C                   EVAL      VABNZD=in_NZD
037900080131     C                   EVAL      VABRMA=in_RMA
038000080131     C                   EVAL      VABNAS=in_NAS
038100030625     C                   EVAL      VABCBO = '1'
038200080131     C* NSP
038300080131     C                   EVAL      PiStr=in_NSP
038400080131     C                   EXSR      CHKNUM
038500080131     C                   IF        PiInt=*on
038600080131     C                   Z-ADD     PiVal         VABNSP
038700080131     C                   Z-ADD     PiVal         VADNSP
038800080131     C                   ELSE
038900080131     C                   ADD       1             errore
039000080131     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039100080131     C                             + ' ' + 'VABNSP/VADNSP'
039200080131     C                   ENDIF
039300080131     C* RMN
039400080131     C                   EVAL      PiStr=in_RMN
039500010201     C                   EXSR      CHKNUM
039600010201     C                   IF        PiInt=*on
039700030625     C                   Z-ADD     PiVal         VABRMN
039800010201     C                   ELSE
039900080131     C                   Z-ADD     1             VABRMN
040000020725     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040100080131     C                             + ' ' + 'VABRMN'
040200010201     C                   ENDIF
040300020305     C*
040400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
040500011113     C                   EXSR      CHKIMPDIV
040600010202     C*
040700000801     C* Ebbene...
040800000801     C                   ADD       1             §CTRMO
040900010201     C                   IF        errore <> *zeros
041000000801     C                   ADD       1             §CTRNO
041100000801     C                   EVAL      recko = vindta
041200000801     C                   EXCEPT    rigadett
041300020725     C                   EVAL      x_vinflg = '2'
041400000801     C                   ELSE
041500010201     C                   ADD       1             §CTROKVB
041600000801     C                   ENDIF
041700000801     C*
041800000801     C                   ENDSR
041900010201     C*----------------------------------------------------*
042000030625     C*  SCARICAMENTO BUFFER RECORDS VAD
042100010201     C*----------------------------------------------------*
042200030625     C     WRIVAD        BEGSR
042300080131     C*
042400080131     C                   eval      VADNRS = VABNRS
042500010201     C*
042600080131     C* Se codice prodotto valorizzato => gestisco
042700080131     C                   if        wVADCDP_1 <> *blanks
042800080131     C                   eval      VADNCL = 1
042900080131     C                   eval      VADCDP = wVADCDP_1
043000080131     C                   write     FIVAD000
043100080131     C                   endif
043200080131     C*
043300080131     C                   if        wVADCDP_2 <> *blanks
043400080131     C                   eval      VADNCL = 1
043500080131     C                   eval      VADCDP = wVADCDP_2
043600080131     C                   write     FIVAD000
043700080131     C                   endif
043800010201     C*
043900010201     C                   ENDSR
044000020904
044100020904
044200010202     C*----------------------------------------------------*
044300030625     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVADWWR
044400010202     C*----------------------------------------------------*
044500030625     C     PREVAD        BEGSR
044600010202     C*
044700030625     C* Compongo il nome del membro da dare al FIVADWWR
044800010202     C                   eval      parmbr = vlrhdl
044900010202     C                   movel     'M'           parmbr
045000080131     C                   eval      parccm = vlrksc
045100010202     C                   eval      paropz = '1'
045200010202     C* Effettuo la chiamata al CLLE preposto
045300030625     C                   call(e)   'TITVVDC'
045400010202     C                   parm                    parccm
045500010202     C                   parm                    parmbr
045600010202     C                   parm                    paropz
045700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045800010202     C                   if        %error
045900010202     C                   movel     '1'           chkcall
046000010202     C                   else
046100010202     C                   movel     '0'           chkcall
046200010202     C                   endif
046300010202     C*
046400010202     C                   ENDSR
046500000801     C*----------------------------------------------------*
046600000801     C*  CONTROLLO NUMERICITA' CAMPI
046700000801     C*----------------------------------------------------*
046800000801     C     CHKNUM        BEGSR
046900000801     C*
047000000801     C                   call(e)   'ISNUMERIC'
047100000801     C                   PARM                    PiStr            30
047200000801     C                   PARM      ','           PiDecChr          1
047300000801     C                   PARM      *ZEROS        PiVal            30 9
047400000801     C                   PARM      '0'           PiInt             1
047500000801     C                   PARM      '0'           PiNum             1
047600000801     C                   IF        %error
047700000801     C                   EVAL      PiInt=*off
047800000801     C                   ENDIF
047900000801     C*
048000000801     C                   ENDSR
048100000801     C***
048200000801
048300011113
048400011113     C*----------------------------------------------------*
048500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
048600011113     C*----------------------------------------------------*
048700011113     C     CHKIMPDIV     BEGSR
048800011113     C*
048900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049000011113     C                   Z-ADD     *zeros        wrkDec            9 9
049100011113     C*
049200011113     C* Come prima cosa effettuo considerazioni sulla divisa
049300011113     C                   IF        vabIAS > *zeros
049400011113     C                   IF        vabVAS <> 'EUR'
049500011113     C                   EVAL      vabVAS =  'ITL'
049600011113     C                   ENDIF
049700011113     C                   ENDIF
049800011113     C*
049900011113     C                   IF        vabCAS > *zeros
050000011113     C                   IF        vabVCA <> 'EUR'
050100011113     C                   EVAL      vabVCA =  'ITL'
050200011113     C                   ENDIF
050300011113     C                   ENDIF
050400011113     C*
050500011113     C                   IF        vabVMD > *zeros
050600020305     C                   IF        vabVAD <> 'EUR'
050700011113     C                   EVAL      vabVAD =  'ITL'
050800011113     C                   ENDIF
050900011113     C                   ENDIF
051000011113     C*
051100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
051200011113     C                   Z-ADD     vabIAS        wrkDec
051300011113     C                   IF        wrkDec > *zeros
051400011113     C                   IF        vabVAS = 'ITL'
051500011113     C                   EVAL      vabIAS = *zeros
051600011113     C                   ENDIF
051700011113     C                   ENDIF
051800011113     C*
051900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
052000011113     C                   Z-ADD     vabCAS        wrkDec
052100011113     C                   IF        wrkDec > *zeros
052200011113     C                   IF        vabVCA = 'ITL'
052300011113     C                   EVAL      vabCAS = *zeros
052400011113     C                   ENDIF
052500011113     C                   ENDIF
052600011113     C*
052700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
052800011113     C                   Z-ADD     vabVMD        wrkDec
052900011113     C                   IF        wrkDec > *zeros
053000011113     C                   IF        vabVAD = 'ITL'
053100011113     C                   EVAL      vabVMD = *zeros
053200011113     C                   ENDIF
053300011113     C                   ENDIF
053400011113     C*
053500011113     C                   ENDSR
053600011113     C***
053700011113
053800011113
053900000801
054000000801
054100990920      /TITLE Invio dei dati al punto operativo.
054200010202     C     invio         BEGSR
054300990920     C*
054400030625     C* 1° invio FIVAD
054500010201     C                   reset                   dscmz
054600010201     C                   move      vlrpoi        cmzdst
054700030625     C                   eval      cmzfld = 'FIVADWWR'
054800010201     C                   eval      cmzmbd = vlrhdl
054900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055000021009     C***                if        prmfir = *blanks
055100030625     C                   eval      cmzfla = 'FIVAD00F'
055200030625     C                   eval      cmzmba = 'FIVAD00F'
055300021009     C***                else
055400021009     C***                eval      cmzfla = prmfir
055500021009     C***                eval      cmzmba = prmfir
055600021009     C***                endif
055700010201     C                   eval      cmznrr = *zeros
055800020305     C                   move      §ctrokvt      cmznrr
055900021018     C                   eval      cmzlba = vlrfl1
056000010201     C                   call(e)   'TIS711C'
056100010201     C                   parm                    dscmz
056200010201     C                   parm      *blanks       esito
056300010205     C                   if        %error
056400010205     C                             or cmzerr = '1'
056500010205     C                             or esito  = '1'
056600010205     C                   eval      wrkesito = '3'
056700010205     C                   else
056800010201     C*
056900021113     C* 2° invio FIVAB
057000010201     C                   reset                   dscmz
057100010201     C                   move      vlrpoi        cmzdst
057200010201     C                   eval      cmzfld = vlrfou
057300010201     C                   eval      cmzmbd = vlrhdl
057400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057500021009     C***                if        prmfir = *blanks
057600021113     C                   eval      cmzfla = 'FIVAB00F'
057700021113     C                   eval      cmzmba = 'FIVAB00F'
057800021009     C***                else
057900021009     C***                eval      cmzfla = prmfir
058000021009     C***                eval      cmzmba = prmfir
058100021009     C***                endif
058200010201     C                   eval      cmznrr = *zeros
058300010201     C                   move      §ctrokvb      cmznrr
058400021018     C                   eval      cmzlba = vlrfl1
058500010201     C                   call(e)   'TIS711C'
058600010201     C                   parm                    dscmz
058700010201     C                   parm      *blanks       esito
058800010201     C                   if        %error
058900010201     C                             or cmzerr = '1'
059000010201     C                             or esito  = '1'
059100010201     C                   eval      wrkesito = '3'
059200010201     C                   endif
059300010205     C                   endif
059400990920     C*
059500000613     C                   ENDSR
059600000613     C***
059700081006
059800081006
059900081006
060000081006
060100081006     C*----------------------------------------------------*
060200081006     C*  ROUTINE FINE ELABORAZIOE
060300081006     C*----------------------------------------------------*
060400081006     C     ENDELA        BEGSR
060500081006     C*
060600081006     C                   if        %open(tivin00r)
060700081006     C                   close     tivin00r
060800081006     C                   endif
060900081006     C                   if        %open(fivabwwr)
061000081006     C                   close     fivabwwr
061100081006     C                   endif
061200081006     C                   if        %open(fivadwwr)
061300081006     C                   close     fivadwwr
061400081006     C                   endif
061500081006     C*
061600081006     C* Effettuo la chiamata al CLLE preposto
061700081006     C                   call(e)   'TITVVDC'
061800081006     C                   parm                    parccm
061900081006     C                   parm                    parmbr
062000081006     C                   parm      '2'           paropz
062100081006     C*
062200081006     C                   if        %open(titvA2ps)
062300081006     C                   except    findett
062400081006     C                   close     titvA2ps
062500081006     C                   endif
062600081006     C*
062700081006     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
062800081006     C                   Z-ADD     150           LENGH            15 5
062900081006     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
063000081006     C                   PARM                    CMD(3)
063100081006     C                   PARM                    LENGH
063200081006     C*
063300081006     C                   ENDSR
063400081006
063500081006
063600070326
063700070326
063800070326     C     *pssr         BEGSR
063900081006     C*
064000081006     C                   if        %open(tivin00r)
064100081006     C                   close     tivin00r
064200081006     C                   endif
064300081006     C                   if        %open(fivabwwr)
064400081006     C                   close     fivabwwr
064500081006     C                   endif
064600081006     C                   if        %open(fivadwwr)
064700081006     C                   close     fivadwwr
064800081006     C                   endif
064900070326     C*
065000070326     C* Effettuo la chiamata al CLLE preposto
065100070326     C                   call(e)   'TITVVDC'
065200070326     C                   parm                    parccm
065300070326     C                   parm                    parmbr
065400070326     C                   parm      '2'           paropz
065500070326     C*
065600070326     C                   eval      wrkesito = '2'
065700070326     C*
065800070326     C                   ENDSR     '*CANCL'
065900070326     C***
066000070326
066100070326
066200990910
066300000613     C     *inzsr        BEGSR
066400990910     C*
066500990910     C     *entry        plist
066600990920     C                   parm                    tivlrds
066700990921     C                   parm      wrkesito      esito
066800000724     C                   parm                    prmlit
066900000710     C                   parm                    prmfir
067000000613     C*
067100000830     C* CALCOLA LA DATA CORRENTE
067200000830     C                   time                    wn14             14 0
067300000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
067400000830     C                   z-add     wn8           g08dat
067500000830     C                   z-add     *zeros        g08inv
067600000830     C                   movel     '0'           g08err
067700000830     C                   call      'XSRDA8'
067800000830     C                   parm                    wlbda8
067900000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
068000000830     C*
068100000613     C                   ENDSR
068200000613     C***
068300990908
068400080131     OtitvA2p   E            riepilogo         2
068500990915     O                                              'Upload via Internet'
068600990915     O                                           +1 'Traduzione TIVIN00R -'
068700030625     O                                           +1 'FIVABWWR/FIVADWWR'
068800010201     O                                           +1 'Report testate bolle'
068900990915     O          E            riepilogo   2
069000990915     O                       wrkdata
069100990915     O                       wrkora              +1
069200990915     O                       procname            +1
069300990915     O          E            riepilogo   2
069400990915     O                                              'Cliente..................:'
069500990915     O                       VABCCM        z     +1
069600990915     O          E            riepilogo   2
069700990920     O                                              'Riferimento Strategi.....:'
069800990920     O                       vlrhdl              +1
069900990915     O          E            riepilogo   2
070000990915     O                                              'Giusti...................:'
070100010201     O                       §CTROKVB      2   +  1
070200990915     O          E            riepilogo   2
070300990915     O                                              'Sbagliati e corretti.....:'
070400971022     O                       §CTRMO        2   +  1
070500990915     O          E            riepilogo   2
070600990915     O                                              'Sbagliati e scartati.....:'
070700971022     O                       §CTRNO        2   +  1
070800000613
070900080131     OtitvA2ps  E            testdett          2
071000000613     O                                              'Upload via Internet'
071100000613     O                                           +1 'Traduzione TIVIN00R -'
071200030625     O                                           +1 'FIVABWWR/FIVADWWR'
071300010201     O                                           +1 'Report testate bolle'
071400000616     O          E            testdett    3
071500000613     O                                           +2 'N° rec'
071600000613     O                                           +3 'Anteprima contenuto'
071700000616     O          E            rigadett    2
071800000613     O                       rrnum               +2
071900000621     O                       recko               +3
072000000616     O          E            findett     2
072100000613     O                       wrkdata
072200000613     O                       wrkora              +1
072300000613     O                       procname            +1
072400000616     O          E            findett     2
072500000613     O                                              'Cliente..................:'
072600000613     O                       VABCCM        z     +1
072700000616     O          E            findett     2
072800000613     O                                              'Riferimento Strategi.....:'
072900000613     O                       vlrhdl              +1
073000000616     O          E            findett     2
073100000613     O                                              'Giusti...................:'
073200010201     O                       §CTROKVB      2   +  1
073300000616     O          E            findett     2
073400000613     O                                              'Sbagliati e corretti.....:'
073500000613     O                       §CTRMO        2   +  1
073600000616     O          E            findett     2
073700000613     O                                              'Sbagliati e scartati.....:'
073800000613     O                       §CTRNO        2   +  1
073900000512** CMD - COMANDI CL
074000080131OVRPRTF FILE(TITVA2P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
074100080131OVRPRTF FILE(TITVA2PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
074200080131DLTOVR FILE(TITVA2P TITVA2PS) LVL(*)
074300000512
074400000512
