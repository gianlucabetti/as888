000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400050621     Fazorg01l  iF   E           K DISK
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021017     FFIVABwwr  O    E             DISK    usropn
000700021017     FFIVATwwr  O    E             DISK    usropn
000800050621     Ftitvm9p   O    F  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100050621     Ftitvm9ps  O    F  198        PRINTER usropn
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
003500050621     D wGiro           s              1  0 INZ(*zeros)
003600010202     D parccm          s              8    INZ(*blanks)
003700010202     D parmbr          s             10    INZ(*blanks)
003800010202     D paropz          s              1    INZ(*blanks)
003900010202     D chkcall         s              1    INZ(*blanks)
004000050706     D wNOTE           s             70    INZ(*blanks)
004100050621
004200000830
004300000830     D*------------------
004400000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004500000830     D*------------------
004600000830     D WLBDA8          DS                  INZ
004700000830     D  G08DAT                 1      8  0
004800000830     D  G08INV                 9     16  0
004900000830     D  G08ERR                17     17
005000000830     D  G08TGI                18     22  0
005100000830     D*
005200990908
005300010201
005400010201
005500990915     C                   time                    wrkdata
005600990915     C                   time                    wrkora
005700000913     C                   reset                   rrnum
005800990921     C                   reset                   esito
005900990921     C                   reset                   wrkesito
006000000724     C*
006100000724     C* SE OCCORRE SPEDIRE IN FILIALE
006200021017     C                   if        vlrpoi <> *zeros
006300000724     C*
006400000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006500000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006600021017     C     vlrpoi        chain     azorg01l
006700000724     C                   if        %found
006800000616     C                   movel(p)  CMD(1)        depcmd
006900020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007000000616     C*
007100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007200011113     C                   Z-ADD     150           LENGH            15 5
007300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007400000616     C                   PARM                    depcmd
007500000616     C                   PARM                    LENGH
007600000724     C*
007700000724     C                   endif
007800000724     C                   endif
007900000616     C*
008000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008100000616     C                   movel(p)  CMD(2)        depcmd
008200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008300000616     C*
008400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008500011113     C                   Z-ADD     150           LENGH            15 5
008600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008700000616     C                   PARM                    depcmd
008800000616     C                   PARM                    LENGH
008900000616     C*
009000050621     C                   if        not %open(titvm9ps)
009100050621     C                   open      titvm9ps
009200000616     C                   except    testdett
009300000613     C                   endif
009400000613     C*
009500030226     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009700000613     C*
009800010202     C* Effettuo la chiamata al CLLE preposto
009900031201     C                   call(e)   'TITVVTC'
010000010202     C                   parm                    parccm
010100010202     C                   parm                    parmbr
010200010202     C                   parm      '2'           paropz
010300010202     C*
010400050621     C                   if        %open(titvm9ps)
010500000616     C                   except    findett
010600050621     C                   close     titvm9ps
010700000613     C                   endif
010800000616     C*
010900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011000011113     C                   Z-ADD     150           LENGH            15 5
011100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011200000616     C                   PARM                    CMD(3)
011300000616     C                   PARM                    LENGH
011400000616     C*
011500010201     C                   seton                                        LR
011600000613
011700000613
011800000613     C*--------------------------------------------------------
011900000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012000000613     C*--------------------------------------------------------
012100000621     C     STPR          BEGSR
012200000613     C*
012300050621     C                   if        not %open(titvm9p)
012400050621     C                   open      titvm9p
012500990915     C                   endif
012600990915     C*
012700990915     C                   except    riepilogo
012800990915     C*
012900050621     C                   if        %open(titvm9p)
013000050621     C                   close     titvm9p
013100990914     C                   endif
013200990910     C*
013300000613     C                   ENDSR
013400000613     C***
013500990908
013600000801
013700910830     C*--------------------------------------------------------
013800030226     C* RWFILE LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR    *
013900910830     C*--------------------------------------------------------
014000030226     C     RWFILE        BEGSR
014100990910     C*
014200990914     C                   if        not %open(tivin00r)
014300990908     C                   open      tivin00r
014400990914     C                   endif
014500021017     C                   if        not %open(fivabwwr)
014600021017     C                   open      fivabwwr
014700990914     C                   endif
014800021017     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
014900020305     C                   exsr      prevat
015000010201     C*
015100010202     C                   if        chkcall = '0'
015200010202     C*
015300021017     C                   if        not %open(fivatwwr)
015400021017     C                   open      fivatwwr
015500010201     C                   endif
015600990910     C*
015700010201     C                   clear                   §CTROKVB          5 0
015800020305     C                   clear                   §CTROKVT          5 0
015900000801     C                   clear                   §CTRMO            5 0
016000000801     C                   clear                   §CTRNO            5 0
016100990910     C*
016200921023     C                   DO        *HIVAL
016300990913     C*
016400990915     C                   READ      tivin00r                               70
016500000905     C                   if        vindta > *blanks
016600000613     C                   add       1             rrnum
016700000801     C*
016800000801     C                   if        *in70 = *off
016900000801     C                             and
017000000801     C                             (vinflg = *blanks
017100000801     C                              or vinflg = '0'
017200000801     C                              or vinflg = '2')
017300000801     C*
017400000801     C                   clear                   vinmsg
017500050621     C*
017600050621     C                   exsr      impvab
017700000905     C*
017800000905     C                   else
017900000905     C                   eval      vinflg = '1'
018000000905     C                   endif
018100030411     C                   endif
018200000905     C*
018300000905     C  N70              update    tivin000
018400000905     C*
018500991022     C  N70              ENDdo
018600030430     C*
018700050621     C* Al termine della lettura scarico i VAB ancora in "canna"
018800050621     C                   write     FIVAB000                                     => scarico VAB
018900010202     C*
019000010202     C                   endif
019100050621     C*
019200990910     C* Se non ci sono record con errori ...
019300000710     C                   if        §ctrno = 0
019400990910     C* ... restituisco esito OK.
019500990921     C                   eval      wrkesito = '0'
019600990910     C                   else
019700010201     C                   if        §ctrokvb > 0
019800990921     C                   eval      wrkesito = '1'
019900000710     C                   else
020000000710     C                   eval      wrkesito = '2'
020100990910     C                   endif
020200000710     C                   endif
020300990910     C*
020400990914     C                   if        %open(tivin00r)
020500990908     C                   close     tivin00r
020600990914     C                   endif
020700021017     C                   if        %open(fivabwwr)
020800021017     C                   close     fivabwwr
020900990914     C                   endif
021000021017     C                   if        %open(fivatwwr)
021100021017     C                   close     fivatwwr
021200010201     C                   endif
021300021017     C*
021400021017     C                   if        vlrpoi <> 999
021500021017     C                   eval      vlrpoi = vlrpoi
021600021017     C                   endif
021700990910     C*
021800010201     C                   if        §ctrokvb > 0
021900021017     C                             and vlrpoi > *zeros
022000010202     C                   exsr      invio
022100990920     C                   endif
022200990920     C*
022300910830     C                   ENDSR
022400000613     C***
022500990920
022600000801     C*----------------------------------------------------*
022700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022800000801     C*----------------------------------------------------*
022900010201     C     INZVAR        BEGSR
023000000801     C*
023100050621     C                   Z-ADD     *zeros        Num5_0            5 0
023200050621     C                   Z-ADD     *zeros        Num7_0            7 0
023300030310     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
023400030310     C                   MOVEL     '0'           FlgCAS            1
023500000801     C*
023600000801     C                   ENDSR
023700000801     C*----------------------------------------------------*
023800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
023900000801     C*----------------------------------------------------*
024000000801     C     DEFCAM        BEGSR
024100000801     C*
024200020619     C* Imposto i valori di default...
024300050621     C                   Z-ADD     0502385       VABCCM
024400050621     C                   Z-ADD     0502385       VATCCM
024500021017     C                   Z-ADD     vlrpoi        VABFGS
024600021017     C                   Z-ADD     vlrpoi        VATFGS
024700050621     C                   Z-ADD     050           VABLNP
024800050621     C                   Z-ADD     050           VATLNP
024900030226     C                   Z-ADD     000           VABCTR
025000021017     C                   MOVEL     '7Q'          VABCTM
025100050321     C                   MOVEL     '1'           VABCBO
025200020619     C* ... e poi verifico se sono stati passati come parametri
025300020619     C                   IF        vlrppt > *blanks
025400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
025500020619     C                   EXSR      CHKNUM
025600020619     C                   IF        PiInt=*on
025700020619     C                   Z-ADD     PiVal         VABCCM
025800020619     C                   Z-ADD     PiVal         VATCCM
025900020619     C                   ENDIF
026000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
026100020619     C                   EXSR      CHKNUM
026200020619     C                   IF        PiInt=*on
026300020619     C                   Z-ADD     PiVal         VABLNP
026400020619     C                   Z-ADD     PiVal         VATLNP
026500020619     C                   ENDIF
026600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
026700020619     C                   EXSR      CHKNUM
026800020619     C                   IF        PiInt=*on
026900020619     C                   Z-ADD     PiVal         VABCTR
027000020619     C                   ENDIF
027100020619     C                   ENDIF
027200000801     C*
027300000801     C                   ENDSR
027400000801     C*----------------------------------------------------*
027500030415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
027600000801     C*----------------------------------------------------*
027700010201     C     IMPVAB        BEGSR
027800010305     C*
027900050621     C                   Z-ADD     *zeros        errore            1 0
028000000830     C                   MOVEL     datcor        VABAAS
028100000830     C                   MOVE      datcor        VABMGS
028200030226     C*
028300050621     C*
028400050621     C                   MOVEL     *blanks       wKey             50
028500050621     C                   Z-ADD     *zeros        wLen              4 0
028600050621     C                   EVAL      wKey = 'Destinatario:'
028700050621     C                   EVAL      wLen = %len(%trim(wKey))
028800050621     C                   IF        %len(%trim(vindta)) > wLen AND
028900050621     C                             %subst(%trim(vindta):1:wLen) = wKey
029000050621     C* Se nn è il 1° giro scarico il buffer della testata precedente
029100050621     C                   IF        wGiro = 1
029200050621     C                   WRITE     FIVAB000
029300050621     C                   ELSE
029400050621     C                   EVAL      wGiro = 1
029500050621     C                   ENDIF
029600050621     C*
029700050621     C                   CLEAR                   FIVAB000
029800050621     C                   EXSR      INZVAR
029900050621     C                   EXSR      DEFCAM
030000050621     C                   EVAL      VABRSD = %trim(%subst(%trim(vindta):1+wLen))
030100050621     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
030200050621     C     '@':'A'       XLATE     VABRSD        VABRSD
030300050621     C* ==
030400050621     C                   ENDIF
030500050621     C*
030600050621     C*
030700050621     C                   MOVEL     *blanks       wKey
030800050621     C                   Z-ADD     *zeros        wLen
030900050621     C                   EVAL      wKey = 'Via:'
031000050621     C                   EVAL      wLen = %len(%trim(wKey))
031100050621     C                   IF        %len(%trim(vindta)) > wLen AND
031200050621     C                             %subst(%trim(vindta):1:wLen) = wKey
031300050621     C                   EVAL      VABIND = %trim(%subst(%trim(vindta):1+wLen))
031400050621     C                   ENDIF
031500050621     C*
031600050621     C*
031700050621     C                   MOVEL     *blanks       wKey
031800050621     C                   Z-ADD     *zeros        wLen
031900050621     C                   EVAL      wKey = 'Cap:'
032000050621     C                   EVAL      wLen = %len(%trim(wKey))
032100050621     C                   IF        %len(%trim(vindta)) > wLen AND
032200050621     C                             %subst(%trim(vindta):1:wLen) = wKey
032300050621     C                   EVAL      PiStr = %trim(%subst(%trim(vindta):1+wLen))
032400050621     C                   EXSR      CHKNUM
032500050621     C                   IF        PiInt=*on
032600050621     C                   Z-ADD     PiVal         Num5_0
032700050621     C                   MOVEL(P)  Num5_0        VABCAD
032800050621     C                   ELSE
032900050621     C                   ADD       1             errore
033000050621     C                   EVAL      vinmsg = %trimr(vinmsg)
033100050621     C                             + ' ' + 'VABCAD'
033200050621     C                   ENDIF
033300050621     C                   ENDIF
033400050621     C*
033500050621     C*
033600050621     C                   MOVEL     *blanks       wKey
033700050621     C                   Z-ADD     *zeros        wLen
033800050621     C                   EVAL      wKey = 'Città.'
033900050621     C                   EVAL      wLen = %len(%trim(wKey))
034000050621     C                   IF        %len(%trim(vindta)) > wLen AND
034100050621     C                             %subst(%trim(vindta):1:wLen) = wKey
034200050621     C                   EVAL      VABLOD = %trim(%subst(%trim(vindta):1+wLen))
034300050621     C                   ENDIF
034400050621     C*
034500050621     C*
034600050621     C                   MOVEL     *blanks       wKey
034700050621     C                   Z-ADD     *zeros        wLen
034800050621     C                   EVAL      wKey = 'Prov.:'
034900050621     C                   EVAL      wLen = %len(%trim(wKey))
035000050621     C                   IF        %len(%trim(vindta)) > wLen AND
035100050621     C                             %subst(%trim(vindta):1:wLen) = wKey
035200050621     C                   EVAL      VABPRD = %trim(%subst(%trim(vindta):1+wLen))
035300050621     C                   ENDIF
035400050621     C*
035500050621     C*
035600050621     C                   MOVEL     *blanks       wKey
035700050621     C                   Z-ADD     *zeros        wLen
035800050621     C                   EVAL      wKey = 'Peso:'
035900050621     C                   EVAL      wLen = %len(%trim(wKey))
036000050621     C                   IF        %len(%trim(vindta)) > wLen AND
036100050621     C                             %subst(%trim(vindta):1:wLen) = wKey
036200050621     C                   EVAL      PiStr = %trim(%subst(%trim(vindta):1+wLen))
036300050621     C     'kg':'  '     XLATE     PiStr         PiStr
036400050621     C                   EVAL      PiStr = %trim(PiStr)
036500050621     C                   EXSR      CHKNUM
036600050621     C                   IF        PiNum=*on
036700050621     C                   Z-ADD     PiVal         VABPKB
036800050621     C                   ELSE
036900050621     C                   ADD       1             errore
037000050621     C                   EVAL      vinmsg = %trimr(vinmsg)
037100050621     C                             + ' ' + 'VABPKB'
037200050621     C                   ENDIF
037300050621     C                   ENDIF
037400050621     C*
037500050621     C*
037600050621     C                   MOVEL     *blanks       wKey
037700050621     C                   Z-ADD     *zeros        wLen
037800050621     C                   EVAL      wKey = 'N° Bolla:'
037900050621     C                   EVAL      wLen = %len(%trim(wKey))
038000050621     C                   IF        %len(%trim(vindta)) > wLen AND
038100050621     C                             %subst(%trim(vindta):1:wLen) = wKey
038200050621     C                   EVAL      PiStr = %trim(%subst(%trim(vindta):1+wLen))
038300050621     C                   EXSR      CHKNUM
038400050621     C                   IF        PiInt=*on
038500050621     C                   Z-ADD     PiVal         VABNSP
038600050621     C                   Z-ADD     PiVal         VABRMN
038700050621     C                   EVAL      VABRMA = %trim(PiStr)
038800050621     C                   ELSE
038900050621     C                   ADD       1             errore
039000050621     C                   EVAL      vinmsg = %trimr(vinmsg)
039100050621     C                             + ' ' + 'VABNSP VABRMN VABRMA'
039200050621     C                   ENDIF
039300050621     C                   ENDIF
039400050621     C*
039500050621     C*
039600050621     C                   MOVEL     *blanks       wKey
039700050621     C                   Z-ADD     *zeros        wLen
039800050621     C                   EVAL      wKey = 'Contrassegno:'
039900050621     C                   EVAL      wLen = %len(%trim(wKey))
040000050621     C                   IF        %len(%trim(vindta)) > wLen AND
040100050621     C                             %subst(%trim(vindta):1:wLen) = wKey
040200050621     C                   EVAL      PiStr = %trim(%subst(%trim(vindta):1+wLen))
040300050621     C     'Euro':'    ' XLATE     PiStr         PiStr
040400050621     C                   EVAL      PiStr = %trim(PiStr)
040500050621     C                   IF        PiStr <> *blanks
040600050621     C                   EVAL      FlgCAS = '1'
040700050621     C                   ENDIF
040800050621     C                   EXSR      CHKNUM
040900050621     C                   IF        PiNum=*on
041000050621     C                   Z-ADD     PiVal         VABCAS
041100050621     C                   EVAL      VABVCA = 'EUR'
041200050621     C                   ELSE
041300050621     C                   ADD       1             errore
041400050621     C                   EVAL      vinmsg = %trimr(vinmsg)
041500050621     C                             + ' ' + 'VABCAS'
041600050621     C                   ENDIF
041700050621     C                   ENDIF
041800050706     C*
041900050706     C*
042000050706     C                   MOVEL     *blanks       wKey
042100050706     C                   Z-ADD     *zeros        wLen
042200050706     C                   EVAL      wKey = 'Note:'
042300050706     C                   EVAL      wLen = %len(%trim(wKey))
042400050706     C                   IF        %len(%trim(vindta)) > wLen AND
042500050706     C                             %subst(%trim(vindta):1:wLen) = wKey
042600050706     C                   EVAL      wNOTE  = %trim(%subst(%trim(vindta):1+wLen))
042700050708     C                   IF        wNOTE <> *blanks
042800050708     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
042900050708     C                   EVAL      VABNOT = %trim(VABNOT)
043000050708     C                   EVAL      VABNT2 = %subst(wNOTE:1+35:35)
043100050708     C                   EVAL      VABNT2 = %trim(VABNT2)
043200050706     C                   ENDIF
043300050708     C                   ENDIF
043400050621     C*
043500050621     C*
043600050621     C                   MOVEL     *blanks       wKey
043700050621     C                   Z-ADD     *zeros        wLen
043800050621     C                   EVAL      wKey = 'Colli:'
043900050621     C                   EVAL      wLen = %len(%trim(wKey))
044000050621     C                   IF        %len(%trim(vindta)) > wLen AND
044100050621     C                             %subst(%trim(vindta):1:wLen) = wKey
044200050621     C                   EVAL      PiStr = %trim(%subst(%trim(vindta):1+wLen))
044300050621     C                   EXSR      CHKNUM
044400050621     C                   IF        PiInt=*on
044500050621     C                   Z-ADD     PiVal         VABNCL
044600050621     C                   ELSE
044700050621     C                   ADD       1             errore
044800050621     C                   EVAL      vinmsg = %trimr(vinmsg)
044900050621     C                             + ' ' + 'VABNCL'
045000050621     C                   ENDIF
045100050621     C                   ENDIF
045200050621     C*
045300050621     C*
045400050621     C                   MOVEL     *blanks       wKey
045500050621     C                   Z-ADD     *zeros        wLen
045600050621     C                   EVAL      wKey = 'Chi sono:'
045700050621     C                   EVAL      wLen = %len(%trim(wKey))
045800050621     C                   IF        %len(%trim(vindta)) > wLen AND
045900050621     C                             %subst(%trim(vindta):1:wLen) = wKey
046000050621     C                   CLEAR                   FIVAT000
046100050621     C                   EVAL      VATFGS = VABFGS
046200050621     C                   EVAL      VATCCM = VABCCM
046300050621     C                   EVAL      VATAAS = VABAAS
046400050621     C                   EVAL      VATLNP = VABLNP
046500050621     C                   EVAL      VATNRS = VABNRS
046600050621     C                   EVAL      VATNSP = VABNSP
046700050621     C                   EVAL      VATTRC = 'E'
046800050621     C                   EVAL      VATNOT = %trim(%subst(%trim(vindta):1+wLen))
046900050621     C                   WRITE     FIVAT000
047000050621     C                   ENDIF
047100030310     C*
047200050321     C* Considerazioni finali su CBO/CAS
047300030310     C                   IF        FlgCAS = '1'
047400050321     C                   IF        VABCBO = '1'
047500010205     C                   EVAL      VABCBO = '4'
047600050321     C                   ENDIF
047700050321     C                   IF        VABCBO = '2'
047800050321     C                   EVAL      VABCBO = '6'
047900050321     C                   ENDIF
048000010205     C                   ENDIF
048100020305     C*
048200011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
048300011113     C                   EXSR      CHKIMPDIV
048400010202     C*
048500000801     C* Ebbene...
048600000801     C                   ADD       1             §CTRMO
048700010201     C                   IF        errore <> *zeros
048800000801     C                   ADD       1             §CTRNO
048900000801     C                   EVAL      recko = vindta
049000000801     C                   EXCEPT    rigadett
049100000801     C                   EVAL      vinflg = '2'
049200000801     C                   ELSE
049300010201     C                   ADD       1             §CTROKVB
049400050708     C                   EVAL      vinflg = '1'
049500000801     C                   ENDIF
049600000801     C*
049700000801     C                   ENDSR
049800010202     C*----------------------------------------------------*
049900021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
050000010202     C*----------------------------------------------------*
050100020305     C     PREVAT        BEGSR
050200010202     C*
050300021017     C* Compongo il nome del membro da dare al FIVATWWR
050400010202     C                   eval      parmbr = vlrhdl
050500010202     C                   movel     'M'           parmbr
050600050317     C                   eval      parccm = %subst(vlrKSC:2:7)
050700010202     C                   eval      paropz = '1'
050800010202     C* Effettuo la chiamata al CLLE preposto
050900031201     C                   call(e)   'TITVVTC'
051000010202     C                   parm                    parccm
051100010202     C                   parm                    parmbr
051200010202     C                   parm                    paropz
051300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051400010202     C                   if        %error
051500010202     C                   movel     '1'           chkcall
051600010202     C                   else
051700010202     C                   movel     '0'           chkcall
051800010202     C                   endif
051900010202     C*
052000010202     C                   ENDSR
052100000801     C*----------------------------------------------------*
052200000801     C*  CONTROLLO NUMERICITA' CAMPI
052300000801     C*----------------------------------------------------*
052400000801     C     CHKNUM        BEGSR
052500050621     C*
052600050329     C                   IF        PiDecChr = *blanks
052700050329     C                   EVAL      PiDecChr = ','
052800050329     C                   ENDIF
052900000801     C*
053000000801     C                   call(e)   'ISNUMERIC'
053100000801     C                   PARM                    PiStr            30
053200050329     C                   PARM                    PiDecChr          1
053300000801     C                   PARM      *ZEROS        PiVal            30 9
053400000801     C                   PARM      '0'           PiInt             1
053500000801     C                   PARM      '0'           PiNum             1
053600000801     C                   IF        %error
053700000801     C                   EVAL      PiInt=*off
053800030107     C                   EVAL      PiNum=*off
053900000801     C                   ENDIF
054000000801     C*
054100000801     C                   ENDSR
054200000801     C***
054300000801
054400011113
054500011113     C*----------------------------------------------------*
054600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
054700011113     C*----------------------------------------------------*
054800011113     C     CHKIMPDIV     BEGSR
054900011113     C*
055000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
055100011113     C                   Z-ADD     *zeros        wrkDec            9 9
055200011113     C*
055300011113     C* Come prima cosa effettuo considerazioni sulla divisa
055400011113     C                   IF        vabIAS > *zeros
055500011113     C                   IF        vabVAS <> 'EUR'
055600011113     C                   EVAL      vabVAS =  'ITL'
055700011113     C                   ENDIF
055800011113     C                   ENDIF
055900011113     C*
056000011113     C                   IF        vabCAS > *zeros
056100011113     C                   IF        vabVCA <> 'EUR'
056200011113     C                   EVAL      vabVCA =  'ITL'
056300011113     C                   ENDIF
056400011113     C                   ENDIF
056500011113     C*
056600011113     C                   IF        vabVMD > *zeros
056700020305     C                   IF        vabVAD <> 'EUR'
056800011113     C                   EVAL      vabVAD =  'ITL'
056900011113     C                   ENDIF
057000011113     C                   ENDIF
057100011113     C*
057200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
057300011113     C                   Z-ADD     vabIAS        wrkDec
057400011113     C                   IF        wrkDec > *zeros
057500011113     C                   IF        vabVAS = 'ITL'
057600011113     C                   EVAL      vabIAS = *zeros
057700011113     C                   ENDIF
057800011113     C                   ENDIF
057900011113     C*
058000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
058100011113     C                   Z-ADD     vabCAS        wrkDec
058200011113     C                   IF        wrkDec > *zeros
058300011113     C                   IF        vabVCA = 'ITL'
058400011113     C                   EVAL      vabCAS = *zeros
058500011113     C                   ENDIF
058600011113     C                   ENDIF
058700011113     C*
058800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
058900011113     C                   Z-ADD     vabVMD        wrkDec
059000011113     C                   IF        wrkDec > *zeros
059100011113     C                   IF        vabVAD = 'ITL'
059200011113     C                   EVAL      vabVMD = *zeros
059300011113     C                   ENDIF
059400011113     C                   ENDIF
059500011113     C*
059600011113     C                   ENDSR
059700011113     C***
059800021017
059900000801
060000000801
060100990920      /TITLE Invio dei dati al punto operativo.
060200010202     C     invio         BEGSR
060300990920     C*
060400021017     C* 1° invio FIVAT
060500010201     C                   reset                   dscmz
060600021017     C                   move      vlrpoi        cmzdst
060700021017     C                   eval      cmzfld = 'FIVATWWR'
060800010201     C                   eval      cmzmbd = vlrhdl
060900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
061000021017     C                   eval      cmzfla = 'FIVAT00F'
061100021017     C                   eval      cmzmba = 'FIVAT00F'
061200010201     C                   eval      cmznrr = *zeros
061300020305     C                   move      §ctrokvt      cmznrr
061400021018     C                   eval      cmzlba = vlrfl1
061500010201     C                   call(e)   'TIS711C'
061600010201     C                   parm                    dscmz
061700010201     C                   parm      *blanks       esito
061800010205     C                   if        %error
061900010205     C                             or cmzerr = '1'
062000010205     C                             or esito  = '1'
062100010205     C                   eval      wrkesito = '3'
062200010205     C                   else
062300010201     C*
062400021017     C* 2° invio FIVAB
062500010201     C                   reset                   dscmz
062600021017     C                   move      vlrpoi        cmzdst
062700010201     C                   eval      cmzfld = vlrfou
062800010201     C                   eval      cmzmbd = vlrhdl
062900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063000021017     C                   eval      cmzfla = 'FIVAB00F'
063100021017     C                   eval      cmzmba = 'FIVAB00F'
063200010201     C                   eval      cmznrr = *zeros
063300010201     C                   move      §ctrokvb      cmznrr
063400021018     C                   eval      cmzlba = vlrfl1
063500010201     C                   call(e)   'TIS711C'
063600010201     C                   parm                    dscmz
063700010201     C                   parm      *blanks       esito
063800010201     C                   if        %error
063900010201     C                             or cmzerr = '1'
064000010201     C                             or esito  = '1'
064100010201     C                   eval      wrkesito = '3'
064200010201     C                   endif
064300010205     C                   endif
064400990920     C*
064500000613     C                   ENDSR
064600000613     C***
064700070411
064800070411     C     *pssr         BEGSR
064900070411     C*
065000070411     C                   if        %open(tivin00r)
065100070411     C                   close     tivin00r
065200070411     C                   endif
065300070411     C                   if        %open(fivabwwr)
065400070411     C                   close     fivabwwr
065500070411     C                   endif
065600070411     C                   if        %open(fivatwwr)
065700070411     C                   close     fivatwwr
065800070411     C                   endif
065900070411     C*
066000070411     C* Effettuo la chiamata al CLLE preposto
066100070411     C                   call(e)   'TITVVTC'
066200070411     C                   parm                    parccm
066300070411     C                   parm                    parmbr
066400070411     C                   parm      '2'           paropz
066500070411     C*
066600070411     C                   eval      wrkesito = '2'
066700070411     C*
066800070411     C                   seton                                        LR
066900070411     C*
067000070411     C                   ENDSR     '*CANCL'
067100070411     C***
067200070411
067300990910
067400000613     C     *inzsr        BEGSR
067500990910     C*
067600990910     C     *entry        plist
067700990920     C                   parm                    tivlrds
067800990921     C                   parm      wrkesito      esito
067900000724     C                   parm                    prmlit
068000000710     C                   parm                    prmfir
068100000613     C*
068200000830     C* CALCOLA LA DATA CORRENTE
068300000830     C                   time                    wn14             14 0
068400000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
068500000830     C                   z-add     wn8           g08dat
068600000830     C                   z-add     *zeros        g08inv
068700000830     C                   movel     '0'           g08err
068800000830     C                   call      'XSRDA8'
068900000830     C                   parm                    wlbda8
069000000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
069100000830     C*
069200000613     C                   ENDSR
069300000613     C***
069400990908
069500050621     Otitvm9p   E            riepilogo         2
069600990915     O                                              'Upload via Internet'
069700990915     O                                           +1 'Traduzione TIVIN00R -'
069800021017     O                                           +1 'FIVABWWR/FIVATWWR'
069900010201     O                                           +1 'Report testate bolle'
070000990915     O          E            riepilogo   2
070100990915     O                       wrkdata
070200990915     O                       wrkora              +1
070300990915     O                       procname            +1
070400990915     O          E            riepilogo   2
070500990915     O                                              'Cliente..................:'
070600990915     O                       VABCCM        z     +1
070700990915     O          E            riepilogo   2
070800990920     O                                              'Riferimento Strategi.....:'
070900990920     O                       vlrhdl              +1
071000990915     O          E            riepilogo   2
071100990915     O                                              'Giusti...................:'
071200010201     O                       §CTROKVB      2   +  1
071300990915     O          E            riepilogo   2
071400990915     O                                              'Sbagliati e corretti.....:'
071500971022     O                       §CTRMO        2   +  1
071600990915     O          E            riepilogo   2
071700990915     O                                              'Sbagliati e scartati.....:'
071800971022     O                       §CTRNO        2   +  1
071900000613
072000050621     Otitvm9ps  E            testdett          2
072100000613     O                                              'Upload via Internet'
072200000613     O                                           +1 'Traduzione TIVIN00R -'
072300021017     O                                           +1 'FIVABWWR/FIVATWWR'
072400010201     O                                           +1 'Report testate bolle'
072500000616     O          E            testdett    3
072600000613     O                                           +2 'N° rec'
072700000613     O                                           +3 'Anteprima contenuto'
072800000616     O          E            rigadett    2
072900000613     O                       rrnum               +2
073000000621     O                       recko               +3
073100000616     O          E            findett     2
073200000613     O                       wrkdata
073300000613     O                       wrkora              +1
073400000613     O                       procname            +1
073500000616     O          E            findett     2
073600000613     O                                              'Cliente..................:'
073700000613     O                       VABCCM        z     +1
073800000616     O          E            findett     2
073900000613     O                                              'Riferimento Strategi.....:'
074000000613     O                       vlrhdl              +1
074100000616     O          E            findett     2
074200000613     O                                              'Giusti...................:'
074300010201     O                       §CTROKVB      2   +  1
074400000616     O          E            findett     2
074500000613     O                                              'Sbagliati e corretti.....:'
074600000613     O                       §CTRMO        2   +  1
074700000616     O          E            findett     2
074800000613     O                                              'Sbagliati e scartati.....:'
074900000613     O                       §CTRNO        2   +  1
075000000512** CMD - COMANDI CL
075100050621OVRPRTF FILE(TITVM9P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
075200050621OVRPRTF FILE(TITVM9PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
075300050621DLTOVR FILE(TITVM9P TITVM9PS) LVL(*)
075400000512
075500000512
