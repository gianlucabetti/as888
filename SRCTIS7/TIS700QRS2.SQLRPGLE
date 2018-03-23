000100990907      /TITLE Lettura log upload via Internet.
000200121217     H dftactgrp(*no) actgrp(*caller)
000300990907
000400000714     Ftivlr06l  uf   e           k disk    rename(tivlr000:tivlr600)
000500090415     Ftivlrlog  o  a e             disk    rename(tivlrlog:tivlrlog0)
000600990907
000700140801     D tivlrnrrp       s             10s 0
000800000714
000900000714     D* Informazioni sul job
001000000714     D psds           sds
001100000714     D  psjobname            244    253
001200000714     D  psuser               254    263
001300000714     D  psjobnbr             264    269s 0
001400010524
001500010524     D* DS di ridefinizione flag operativi sul log UPLOAD (tivlr00f)
001600010524     D dsvlr3        e ds
001700140801
001800140801     D* DS di ridefinizione file fisico letture SQL
001900140801     D tivlrds       e ds                  extname(tivlr00f) inz
002000990907
002100000710     D* variabili di work
002200000710     D  prmpgi         s             10
002300000710     D  prmppi         s             50
002400000717     D  prmfit         s             10
002500000717     D  prmfi3         s             10
002600000717     D  prmfi4         s             10
002700000714     D  prmesito       s              1    inz(*blanks)
002800000714     D  wrkmsg         s             80    inz(*blanks)
002900000713     D  conta          s              2  0
003000000713     D  contaa         s              2
003100000714     D  contaOK        s              2  0
003200000714     D  contaaOK       s              2
003300000714     D  contaKO        s              2  0
003400000714     D  wrkoggiiso     s               d
003500000714     D  wrkoggi        s              8  0
003600000714     D  wrkora         s              6  0
003700000714     D  datime         s             14
003800030616     D  savMSG         s             30
003900070920     D  savSQZ         s                   like(vlrsqz) inz(*loval)
004000000710
004100081010     C*
004200081010     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
004300081010     C
004400081010     C/EXEC SQL
004500081010     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
004600081010     C/END-EXEC
004700081010     C*
004800130617     C                   setoff                                       lrrt
004900140801     C*
005000140801     C/EXEC SQL
005100140801     C+ declare M1 cursor for
005200140801     C+ select tivlr00f.*, rrn(tivlr00f) from tivlr00f
005300140801     C+ where vlrFLG = '0' and substr(vlrFL3, 4, 2) = :inPROC
005400170817     C+ order by vlrdat, vlrora, vlrPRG, vlrSQZ
005500140801     C+ for read only
005600140801     C/END-EXEC
005700140801     C
005800140801     C/EXEC SQL
005900140801     C+ open M1
006000140801     C/END-EXEC
006100140801     C*
006200140801     C                   dou       sqlcod > *zeros OR *inrt OR *inlr
006300991109     C*
006400140801     C/EXEC SQL
006500140801     C+ Fetch M1 into :tivlrds, :tivlrnrrp
006600140801     C/END-EXEC
006700090415     C*
006800090415     C* Inizializzo buffer TIVLRLOG
006900090415     C                   setoff                                       55
007000090415     C                   clear                   vlrDATIE
007100090415     C                   clear                   vlrDATFE
007200090415     C                   clear                   vlrDATIP
007300090415     C                   clear                   vlrDATFP
007400991105     C*
007500140801     C* Gestione ENDJOB.
007600140801     C                   shtdn                                        lr
007700140801     C   lr              return
007800140801     C*
007900140801     C                   if        sqlcod = *zeros AND not *inlr
008000140801     C*
008100030613     C* Salvo subito il contenuto attuale del campo messaggio (se pieno a questo punto vuol dire che
008200030613     C* è stato scritto dalla procedura FTP/e-mail e riporta in nome del file ricevuto dal cliente)
008300030613     C                   if        %len(%trim(vlrmsg)) > 0
008400030613     C                   eval      savMSG = %trim(vlrmsg)
008500030613     C                   endif
008600140801     C*
008700990914     C* Se leggo un record a stato '0'
008800000717     C* e con i campi fondamentali pieni
008900990914     C* lo elaboro.
009000990908     C                   if        vlrflg = '0'
009100990921     C                             and vlrksc <> *blanks
009200170817     C                             and vlrtip <> *blanks
009300990914     C                             and vlrhdl <> *blanks
009400070920     C*
009500070920     C* Salvo il numero sequenza del record attuale
009600070920     C                   eval      savSQZ = vlrSQZ
009700070920     C*
009800170817     C* Se è un record di una normale operazione "mono-file" (oppure rilancio da Gestione del Log)
009900010524     C                   movel(p)  vlrfl3        dsvlr3
010000170817     C***                if        vlrfl2 = '01' or §vlr31 = '1'
010100170817     C                   if        vlrfl2 = '01'
010200000714     C                   exsr      exeproc
010300000714     C                   else
010400000714     C                   exsr      exemulti
010500000714     C                   endif
010600090415     C*
010700090415     C* Scrittura record TIVLRLOG
010800090415     C                   if        *in55 = *on and vlrpoi <> *zeros
010900140801     C                   eval      vlrRRN  = tivlrnrrp
011000090415     C                   eval      vlrPROC = %subst(vlrFL3:4:2)
011100090415     C                   eval      vlrNOMF = savMSG
011200090415     C                   eval      vlrPGI  = prmpgi
011300090415     C                   eval      *in54 = *off                                 *switch on/off write
011400090415     C   54              write     tivlrlog0
011500090415     C                   endif
011600991105     C*
011700991105     C                   endif
011800140801     C                   else
011900140801     C*
012000140804     C* Se fine file => chiusura
012100140804     C                   seton                                        lr
012200000713     C                   endif
012300991109     C*
012400991109     C                   enddo
012500140801     C*
012600140801     C/EXEC SQL
012700140801     C+ close M1
012800140801     C/END-EXEC
012900140801     C*
013000000714
013100000714
013200000714
013300000714      /TITLE Routine di controllo operazioni "multi-file"
013400000714     C*
013500000714     C     exemulti      begsr
013600000714     C*
013700000714     C                   z-add     *zeros        conta
013800000714     C                   movel     *zeros        contaa
013900000714     C*
014000000714     C/EXEC SQL
014100000830     C+ declare C1 cursor for select count(*)
014200000714     C+ from tivlr00f where
014300000714     C+ vlrprg = :vlrprg and vlrksc = :vlrksc
014400170817     C+ and vlrsqz = :savSQZ and vlrflg <> 'M'
014500000714     C+ for fetch only
014600000714     C/END-EXEC
014700000714     C
014800000714     C/EXEC SQL
014900000714     C+ open C1
015000000714     C/END-EXEC
015100000714     C
015200000714     C/EXEC SQL
015300000714     C+ Fetch C1 into :conta
015400000714     C/END-EXEC
015500000714     C*
015600000714     C                   move      conta         contaa
015700000714     C*
015800000714     C                   if        sqlcod = *zeros and
015900170817     C                             conta  > 1      and
016000000714     C                             contaa = vlrfl2
016100081010     C                   exsr      exeproc
016200081010     C                   endif
016300000724     C
016400000724     C/EXEC SQL
016500000724     C+ close C1
016600000724     C/END-EXEC
016700000724     C
016800000714     C*
016900000714     C                   endsr
017000000714
017100000714
017200000714
017300000714      /TITLE Routine di controllo operazioni "post-traduzione"
017400000714     C*
017500000714     C     exepost       begsr
017600000714     C*
017700000714     C                   z-add     *zeros        contaOK
017800000714     C                   movel     *zeros        contaaOK
017900000714     C                   z-add     *zeros        contaKO
018000000714     C*
018100000714     C/EXEC SQL
018200000830     C+ declare C2 cursor for select count(*)
018300000714     C+ from tivlr00f where
018400170817     C+ vlrprg = :vlrprg and vlrksc = :vlrksc
018500170817     C+ and vlrsqz = :savSQZ
018600000725     C+ and vlrflg = '1'
018700000714     C+ for fetch only
018800000714     C/END-EXEC
018900000714     C
019000000714     C/EXEC SQL
019100000714     C+ open C2
019200000714     C/END-EXEC
019300000714     C
019400000714     C/EXEC SQL
019500000714     C+ Fetch C2 into :contaOK
019600000714     C/END-EXEC
019700000714     C*
019800000714     C*
019900000714     C/EXEC SQL
020000000830     C+ declare C3 cursor for select count(*)
020100000714     C+ from tivlr00f where
020200170817     C+ vlrprg = :vlrprg and vlrksc = :vlrksc
020300170817     C+ and vlrsqz = :savSQZ
020400000725     C+ and vlrflg = '2'
020500000714     C+ for fetch only
020600000714     C/END-EXEC
020700000714     C
020800000714     C/EXEC SQL
020900000714     C+ open C3
021000000714     C/END-EXEC
021100000714     C
021200000714     C/EXEC SQL
021300000714     C+ Fetch C3 into :contaKO
021400000714     C/END-EXEC
021500000714     C*
021600000714     C                   move      contaOK       contaaOK
021700000714     C*
021800000714     C                   if        sqlcod = *zeros and
021900000714     C                             contaaOK = vlrfl2 and
022000000714     C                             contaKO = *zeros
022100000714     C                   exsr      exepgm
022200000714     C                   endif
022300000724     C
022400000724     C/EXEC SQL
022500000724     C+ close C2
022600000724     C/END-EXEC
022700000724     C
022800000724     C
022900000724     C/EXEC SQL
023000000724     C+ close C3
023100000724     C/END-EXEC
023200000724     C
023300000714     C*
023400000714     C                   endsr
023500000714
023600000714
023700000714      /TITLE Lancio l'eventuale PGM "post-traduzione"
023800000714     C*
023900000714     C     exepgm        begsr
024000000714     C* Lancio il pgm in variabile con relativi parametri
024100000714     C* (tutto ciò solo se chiaramente c'è qualcosa da lanciare)
024200000714    C                   if        prmpgi <> *blanks
024300090415     C*
024400090415     C* Valorizzazione file TIVLRLOG
024500090415     C*
024600090415     C/EXEC SQL
024700090415     C+ SET :vlrDATIP = NOW()
024800090415     C/END-EXEC
024900090415     C*
025000090415     C
025100000714     C* Chiamo il pgm post traduzione.
025200000714     C                   call(e)   'TIS700C1'
025300000714     C                   parm                    vlrprg
025400000714     C                   parm                    vlrksc
025500000714     C                   parm                    datime
025600000714     C                   parm                    prmpgi
025700000714     C                   parm                    prmppi
025800000717     C                   parm                    prmfit
025900000717     C                   parm                    prmfi3
026000000717     C                   parm                    prmfi4
026100000714     C                   parm      *blanks       prmesito
026200090415     C*
026300090415     C* Valorizzazione file TIVLRLOG
026400090415     C*
026500090415     C/EXEC SQL
026600090415     C+ SET :vlrDATFP = NOW()
026700090415     C/END-EXEC
026800090415     C*
026900090415     C
027000010523     C*
027100000714     C* Se la call è finita in errore
027200010523     C* o il lancio del PGM ha avuto degli errori (e comunque ha cambiato esito).
027300000714     C                   if        %error
027400010523     C                             or (prmesito <> '0' and prmesito <> *blanks)
027500000714     C     k01vlr        setll     tivlr06l
027600000714     C                   if        %found(tivlr06l)
027700000714     C     k01vlr        reade     tivlr06l
027800000714     C                   dow       not %eof
027900070920     C                   if        vlrsqz = savSQZ
028000000714     C                   if        vlrflg <> '2' or vlrflg <> '4'
028100000714     C                   eval      wrkmsg = 'Errori solo in post traduzione.'
028200000714     C                   else
028300000714     C                   eval      wrkmsg = 'Errori anche in post traduzione.'
028400000714     C                   endif
028500000714     C                   eval      vlrflg = '2'
028600000714     C                   eval      vlrmsg =
028700020909     C                               '(SEDE)'
028800000714     C                             + '/'
028900000714     C                             + %trim(psjobname)
029000000714     C                             + '/'
029100000714     C                             + %trim(psuser)
029200000714     C                             + '/'
029300000714     C                             + %trim(%editc(psjobnbr:'Z'))
029400000714     C                             + %trim(wrkmsg)
029500030613     C                   move      savMSG        vlrmsg
029600000714     C                   update    tivlr600
029700000714     C                   endif
029800000714     C     k01vlr        reade     tivlr06l
029900000714     C                   enddo
030000000714     C                   endif
030100000714     C                   endif
030200000714     C*
030300000714     C                   endif
030400000714     C*
030500000714     C                   endsr
030600000714
030700000714
030800000714
030900000714
031000000717      /TITLE Routine di avio processamento record di log
031100000714     C*
031200000714     C     exeproc       begsr
031300090415     C*
031400090415     C                   seton                                        55
031500090415     C*
031600090415     C* Valorizzazione file TIVLRLOG
031700090415     C*
031800090415     C/EXEC SQL
031900090415     C+ SET :vlrDATIE = NOW()
032000090415     C/END-EXEC
032100090415     C*
032200090415     C
032300000714     C*
032400000714     C                   eval      prmpgi = *blanks
032500000714     C                   eval      prmppi = *blanks
032600000714     C                   call(e)   'TIS701R'
032700140801     C                   parm                    tivlrnrrp
032800010820     C                   parm      *blanks       prmpgi
032900010820     C                   parm      *blanks       prmppi
033000010820     C                   parm      *blanks       prmfit
033100010820     C                   parm      *blanks       prmfi3
033200010820     C                   parm      *blanks       prmfi4
033300090415     C*
033400090415     C* Valorizzazione file TIVLRLOG
033500090415     C*
033600090415     C/EXEC SQL
033700090415     C+ SET :vlrDATFE = NOW()
033800090415     C/END-EXEC
033900090415     C*
034000170817     C                   exsr      exepost
034100000714     C*
034200000714     C                   endsr
034300170817
034400000714
034500000714
034600000714      /TITLE Routine di *init PGM
034700000714     C*
034800000714     C     *inzsr        begsr
034900140801     C*
035000140801     C     *entry        plist
035100140801     C                   parm                    inPROC            2
035200000714     C*
035300000714     C     K01vlr        klist
035400000714     C                   kfld                    vlrprg
035500000714     C                   kfld                    vlrksc
035600000714     C*
035700000714     C                   time                    wrkoggiiso
035800000714     C                   time                    wrkora
035900000714     C     *iso          move      wrkoggiiso    wrkoggi
036000000714     C                   move(p)   wrkora        datime
036100000714     C                   movel     wrkoggi       datime
036200000714     C*
036300000714     C                   endsr
036400000714
