000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000500110404     Ftivlre01l iF   E           K DISK
000600110404     Fwfmbs00f  O    E             DISK
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990910     D psds           sds
001200990910     D  procname         *PROC
001300990920     D tivlrds       e ds                  extname(tivlr00f)
001400110404     D tivinds       e ds                  extname(tivin00r) prefix(x_)
001500990910     D esito           s              1
001600000724     D prmlit          s             10
001700000710     D prmfir          s             10
001800990921     D wrkesito        s                   like(esito)
001900000613     D rrnum           s              6  0 INZ(*zeros)
002000110404     D wCount          s              6  0 INZ(*zeros)
002100100607
002200100607     D*------------------
002300100607     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
002400100607     D*------------------
002500110616     D InStringa       S          65535A   VARYING                              (stringa input)
002600100607     D InSepara        S             10A                                        (separatore)
002700100607     D InTypeOut       S              1A                                        (tipo output)
002800110617     D wSkParam        S          65535A                                        (output)
002900100607     D OutErrore       S              1A                                        (flag errore)
003000100607     D OutMsg          S             80A                                        (messaggio errore)
003100110404     D SkSplitCSV_3    S             64    DIM(1024)
003200020725
003300110404     D*------------------
003400110404     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003500110404     D*------------------
003600110404     D INPUT_DS        DS                  INZ
003700110404     D  VINDTA                     2048
003800110404     D  VINFLG                        1
003900110404     D  VINRRN                        8  0
004000110404     D*
004100010201
004200110404
004300110404
004400110404
004500080117     C*
004600080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
004700080117     C
004800080117     C/EXEC SQL
004900080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
005000080117     C/END-EXEC
005100100607     C*
005200000913     C                   reset                   rrnum
005300990921     C                   reset                   esito
005400990921     C                   reset                   wrkesito
005500110404     C                   reset                   wCount
005600110404     C*
005700070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
005800100607     C*
005900100607     C* Effettuo lancio XSPLIT solo x chiusura
006000100607     C                   CALL(e)   'XSPLIT2'
006100110616     C                   PARM      ''            InStringa
006200100607     C                   PARM                    InSepara
006300100607     C                   PARM                    InTypeOut
006400110616     C                   PARM      ''            wSkParam
006500100607     C                   PARM                    OutErrore
006600100607     C                   PARM                    OutMsg
006700000616     C*
006800010201     C                   seton                                        LR
006900990908
007000000801
007100910830     C*--------------------------------------------------------
007200070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
007300910830     C*--------------------------------------------------------
007400070530     C     RWFILE        BEGSR
007500990910     C*
007600990914     C                   if        not %open(tivin00r)
007700990908     C                   open      tivin00r
007800990914     C                   endif
007900990910     C*
008000020725     C/EXEC SQL
008100020725     C+ declare C1 cursor for select
008200110404     C+ vindta, vinflg, rrn(tivin00r)
008300060223     C+ from tivin00r
008400020725     C+ for read only
008500020725     C/END-EXEC
008600020725     C
008700020725     C/EXEC SQL
008800020725     C+ open C1
008900020725     C/END-EXEC
009000020725     C
009100020725     C/EXEC SQL
009200070530     C+ Fetch C1 into :INPUT_DS
009300020725     C/END-EXEC
009400020725     C*
009500020725     C                   dow       sqlcod = *zeros
009600990913     C*
009700100310     C                   if        vindta > *blanks
009800000613     C                   add       1             rrnum
009900000801     C*
010000020725     C                   if        vinflg = *blanks
010100020725     C                             or vinflg = '0'
010200020725     C                             or vinflg = '2'
010300000801     C*
010400110404     C                   clear                   tivinds
010500020725     C                   eval      x_vinflg = '1'
010600110404     C                   if        %subst(vindta:1:3) = '\02' AND
010700110404     C                             %subst(vindta:%len(%trim(vindta))-2:3)='\0A'
010800110404     C                   add       1             wCount
010900110509     C                   exsr      wrifil
011000110404     C                   else
011100110404     C                   eval      x_vinflg = '1'
011200110404     C                   endif
011300110404     C*
011400071009     C                   endif
011500000905     C*
011600000905     C                   else
011700020725     C                   eval      x_vinflg = '1'
011800000905     C                   endif
011900000905     C*
012000020725     C     VINRRN        chain     tivin000
012100020725     C                   if        %found(tivin00r)
012200020725     C                   eval      y_vinflg = x_vinflg
012300020725     C                   eval      y_vinmsg = x_vinmsg
012400020725     C                   update    tivin000
012500020725     C                   endif
012600020725     C*
012700020725     C/EXEC SQL
012800070530     C+ Fetch C1 into :INPUT_DS
012900020725     C/END-EXEC
013000020725     C*
013100020725     C                   enddo
013200020725     C*
013300020725     C/EXEC SQL
013400020725     C+ close C1
013500020725     C/END-EXEC
013600000905     C*
013700010202     C*
013800110404     C* ... restituisco sempre esito OK.
013900990921     C                   eval      wrkesito = '0'
014000990910     C*
014100990914     C                   if        %open(tivin00r)
014200990908     C                   close     tivin00r
014300990914     C                   endif
014400990920     C*
014500910830     C                   ENDSR
014600000613     C***
014700000801     C*----------------------------------------------------*
014800110404     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
014900000801     C*----------------------------------------------------*
015000110509     C     WRIFIL        BEGSR
015100070530     C*
015200110404     C                   CLEAR                   WFMBS000
015300110404     C*
015400110404     C                   EVAL      §mbsDAT = datcor
015500110404     C                   EVAL      §mbsORA = oracor
015600110404     C*
015700110404     C* Reperisco il nome del file ricevuto in upload
015800110404     C                   EVAL      vlreTRC = 'FIL' + vlrISV
015900110404     C     KEYvlre01_C   CHAIN     tivlre01l
016000110404     C                   IF        %found(tivlre01l)
016100110404     C                   EVAL      §mbsNAMF = vlreFILORI
016200110404     C                   ENDIF
016300110404     C*
016400110404     C                   EVAL      §mbsPRG = wCount
016500110404     C*
016600110404     C* Verticalizzo le spunte ricevute
016700110404     C                   eval      InStringa =
016800110415     C                             %subst(vindta:4:%len(%trim(vindta))-6)
016900100607     C*
017000100607     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
017100110509     C                   CALL(e)   'XSPLIT2'
017200100607     C                   PARM                    InStringa
017300100607     C                   PARM      ';'           InSepara
017400110404     C                   PARM      '3'           InTypeOut
017500110616     C                   PARM      ''            wSkParam
017600100607     C                   PARM                    OutErrore
017700100607     C                   PARM                    OutMsg
017800110404     C                   MOVEA     wSkParam      SkSplitCSV_3
017900110509     C*
018000110509     C* Se errore in chiamata "splittatore" => marco il record corrente
018100110509     C                   if        %error
018200110509     C                   eval      x_vinflg = '2'
018300110509     C                   else
018400110509     C                   eval      x_vinflg = '1'
018500110509     C                   endif
018600100607     C*
018700100607     C* Valorizzo la schiera delle intestazioni colonne (elementi)
018800100607     C                   z-add     1             i                 4 0
018900110404     C                   dow       i <= %elem(SkSplitCSV_3)
019000110404     C                   if        SkSplitCSV_3(i) = *blanks
019100110404     C                   leave
019200110404     C                   else
019300110404     C                   if        %trim(SkSplitCSV_3(i)) <> *all'?'
019400110404     C                   eval      §mbsBRCD = SkSplitCSV_3(i)
019500110404     C                   write     WFMBS000
019600100607     C                   endif
019700110404     C                   endif
019800100607     C                   add       1             i
019900100607     C                   enddo
020000071003     C*
020100071003     C                   ENDSR
020200070411
020300110404
020400110404
020500070411     C     *pssr         BEGSR
020600070411     C*
020700070411     C                   if        %open(tivin00r)
020800070411     C                   close     tivin00r
020900070411     C                   endif
021000070411     C*
021100070411     C                   eval      wrkesito = '2'
021200070411     C*
021300070411     C                   seton                                        LR
021400070411     C*
021500070411     C                   ENDSR     '*CANCL'
021600070411     C***
021700070411
021800110404
021900990910
022000000613     C     *inzsr        BEGSR
022100990910     C*
022200990910     C     *entry        plist
022300990920     C                   parm                    tivlrds
022400990921     C                   parm      wrkesito      esito
022500000724     C                   parm                    prmlit
022600000710     C                   parm                    prmfir
022700100607     C*
022800110404     C* Chiave su TIVLRE01L - completa
022900110404     C     KEYvlre01_C   KLIST
023000110404     C                   KFLD                    vlreTRC
023100110404     C                   KFLD                    vlrPRG
023200000613     C*
023300000830     C* CALCOLA LA DATA CORRENTE
023400110404     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE
023500091223     C                   eval      datcor = %dec(%date() : *ISO)
023600110404     C                   time                    wn14             14 0
023700110404     C                   movel     wn14          oracor            6 0          *ORA  CORRENTE
023800000830     C*
023900000613     C                   ENDSR
024000000613     C***
