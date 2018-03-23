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
005400170821     C+ and (vlrPRG, vlrSQZ, decimal(vlrFL2)) in (
005500170821     C+ select vlrPRG, vlrSQZ, count(*) from tivlr00f
005600170821     C+ where vlrFLG not in (' ', 'M')
005700170821     C+ group by vlrPRG, vlrSQZ)
005800170821     C+ order by vlrdat, vlrora, vlrPRG, vlrSQZ
005900140801     C+ for read only
006000140801     C/END-EXEC
006100140801     C
006200140801     C/EXEC SQL
006300140801     C+ open M1
006400140801     C/END-EXEC
006500140801     C*
006600140801     C                   dou       sqlcod > *zeros OR *inrt OR *inlr
006700991109     C*
006800140801     C/EXEC SQL
006900140801     C+ Fetch M1 into :tivlrds, :tivlrnrrp
007000140801     C/END-EXEC
007100090415     C*
007200090415     C* Inizializzo buffer TIVLRLOG
007300090415     C                   setoff                                       55
007400090415     C                   clear                   vlrDATIE
007500090415     C                   clear                   vlrDATFE
007600090415     C                   clear                   vlrDATIP
007700090415     C                   clear                   vlrDATFP
007800991105     C*
007900140801     C* Gestione ENDJOB.
008000140801     C                   shtdn                                        lr
008100140801     C   lr              return
008200140801     C*
008300140801     C                   if        sqlcod = *zeros AND not *inlr
008400140801     C*
008500030613     C* Salvo subito il contenuto attuale del campo messaggio (se pieno a questo punto vuol dire che
008600030613     C* è stato scritto dalla procedura FTP/e-mail e riporta in nome del file ricevuto dal cliente)
008700030613     C                   if        %len(%trim(vlrmsg)) > 0
008800030613     C                   eval      savMSG = %trim(vlrmsg)
008900030613     C                   endif
009000140801     C*
009100990914     C* Se leggo un record a stato '0'
009200000717     C* e con i campi fondamentali pieni
009300990914     C* lo elaboro.
009400990908     C                   if        vlrflg = '0'
009500990921     C                             and vlrksc <> *blanks
009600170817     C                             and vlrtip <> *blanks
009700990914     C                             and vlrhdl <> *blanks
009800070920     C*
009900070920     C* Salvo il numero sequenza del record attuale
010000070920     C                   eval      savSQZ = vlrSQZ
010100070920     C*
010200170817     C* Se è un record di una normale operazione "mono-file" (oppure rilancio da Gestione del Log)
010300010524     C                   movel(p)  vlrfl3        dsvlr3
010400170817     C***                if        vlrfl2 = '01' or §vlr31 = '1'
010500170817     C                   if        vlrfl2 = '01'
010600000714     C                   exsr      exeproc
010700000714     C                   else
010800000714     C                   exsr      exemulti
010900000714     C                   endif
011000090415     C*
011100090415     C* Scrittura record TIVLRLOG
011200090415     C                   if        *in55 = *on and vlrpoi <> *zeros
011300140801     C                   eval      vlrRRN  = tivlrnrrp
011400090415     C                   eval      vlrPROC = %subst(vlrFL3:4:2)
011500090415     C                   eval      vlrNOMF = savMSG
011600090415     C                   eval      vlrPGI  = prmpgi
011700090415     C                   eval      *in54 = *off                                 *switch on/off write
011800090415     C   54              write     tivlrlog0
011900090415     C                   endif
012000991105     C*
012100991105     C                   endif
012200140801     C                   else
012300140801     C*
012400140804     C* Se fine file => chiusura
012500140804     C                   seton                                        lr
012600000713     C                   endif
012700991109     C*
012800991109     C                   enddo
012900140801     C*
013000140801     C/EXEC SQL
013100140801     C+ close M1
013200140801     C/END-EXEC
013300140801     C*
013400000714
013500000714
013600000714
013700000714      /TITLE Routine di controllo operazioni "multi-file"
013800000714     C*
013900000714     C     exemulti      begsr
014000000714     C*
014100000714     C                   z-add     *zeros        conta
014200000714     C                   movel     *zeros        contaa
014300000714     C*
014400000714     C/EXEC SQL
014500000830     C+ declare C1 cursor for select count(*)
014600000714     C+ from tivlr00f where
014700000714     C+ vlrprg = :vlrprg and vlrksc = :vlrksc
014800170821     C+ and vlrsqz = :savSQZ
014900170821     C+ and vlrflg not in (' ', 'M')
015000000714     C+ for fetch only
015100000714     C/END-EXEC
015200000714     C
015300000714     C/EXEC SQL
015400000714     C+ open C1
015500000714     C/END-EXEC
015600000714     C
015700000714     C/EXEC SQL
015800000714     C+ Fetch C1 into :conta
015900000714     C/END-EXEC
016000000714     C*
016100000714     C                   move      conta         contaa
016200000714     C*
016300000714     C                   if        sqlcod = *zeros and
016400170817     C                             conta  > 1      and
016500000714     C                             contaa = vlrfl2
016600081010     C                   exsr      exeproc
016700081010     C                   endif
016800000724     C
016900000724     C/EXEC SQL
017000000724     C+ close C1
017100000724     C/END-EXEC
017200000724     C
017300000714     C*
017400000714     C                   endsr
017500000714
017600000714
017700000714
017800000714      /TITLE Routine di controllo operazioni "post-traduzione"
017900000714     C*
018000000714     C     exepost       begsr
018100000714     C*
018200000714     C                   z-add     *zeros        contaOK
018300000714     C                   movel     *zeros        contaaOK
018400000714     C                   z-add     *zeros        contaKO
018500000714     C*
018600000714     C/EXEC SQL
018700000830     C+ declare C2 cursor for select count(*)
018800000714     C+ from tivlr00f where
018900170817     C+ vlrprg = :vlrprg and vlrksc = :vlrksc
019000170817     C+ and vlrsqz = :savSQZ
019100000725     C+ and vlrflg = '1'
019200000714     C+ for fetch only
019300000714     C/END-EXEC
019400000714     C
019500000714     C/EXEC SQL
019600000714     C+ open C2
019700000714     C/END-EXEC
019800000714     C
019900000714     C/EXEC SQL
020000000714     C+ Fetch C2 into :contaOK
020100000714     C/END-EXEC
020200000714     C*
020300000714     C*
020400000714     C/EXEC SQL
020500000830     C+ declare C3 cursor for select count(*)
020600000714     C+ from tivlr00f where
020700170817     C+ vlrprg = :vlrprg and vlrksc = :vlrksc
020800170817     C+ and vlrsqz = :savSQZ
020900000725     C+ and vlrflg = '2'
021000000714     C+ for fetch only
021100000714     C/END-EXEC
021200000714     C
021300000714     C/EXEC SQL
021400000714     C+ open C3
021500000714     C/END-EXEC
021600000714     C
021700000714     C/EXEC SQL
021800000714     C+ Fetch C3 into :contaKO
021900000714     C/END-EXEC
022000000714     C*
022100000714     C                   move      contaOK       contaaOK
022200000714     C*
022300000714     C                   if        sqlcod = *zeros and
022400000714     C                             contaaOK = vlrfl2 and
022500000714     C                             contaKO = *zeros
022600000714     C                   exsr      exepgm
022700000714     C                   endif
022800000724     C
022900000724     C/EXEC SQL
023000000724     C+ close C2
023100000724     C/END-EXEC
023200000724     C
023300000724     C
023400000724     C/EXEC SQL
023500000724     C+ close C3
023600000724     C/END-EXEC
023700000724     C
023800000714     C*
023900000714     C                   endsr
024000000714
024100000714
024200000714      /TITLE Lancio l'eventuale PGM "post-traduzione"
024300000714     C*
024400000714     C     exepgm        begsr
024500000714     C* Lancio il pgm in variabile con relativi parametri
024600000714     C* (tutto ciò solo se chiaramente c'è qualcosa da lanciare)
024700000714    C                   if        prmpgi <> *blanks
024800090415     C*
024900090415     C* Valorizzazione file TIVLRLOG
025000090415     C*
025100090415     C/EXEC SQL
025200090415     C+ SET :vlrDATIP = NOW()
025300090415     C/END-EXEC
025400090415     C*
025500090415     C
025600000714     C* Chiamo il pgm post traduzione.
025700000714     C                   call(e)   'TIS700C1'
025800000714     C                   parm                    vlrprg
025900000714     C                   parm                    vlrksc
026000000714     C                   parm                    datime
026100000714     C                   parm                    prmpgi
026200000714     C                   parm                    prmppi
026300000717     C                   parm                    prmfit
026400000717     C                   parm                    prmfi3
026500000717     C                   parm                    prmfi4
026600000714     C                   parm      *blanks       prmesito
026700090415     C*
026800090415     C* Valorizzazione file TIVLRLOG
026900090415     C*
027000090415     C/EXEC SQL
027100090415     C+ SET :vlrDATFP = NOW()
027200090415     C/END-EXEC
027300090415     C*
027400090415     C
027500010523     C*
027600000714     C* Se la call è finita in errore
027700010523     C* o il lancio del PGM ha avuto degli errori (e comunque ha cambiato esito).
027800000714     C                   if        %error
027900010523     C                             or (prmesito <> '0' and prmesito <> *blanks)
028000000714     C     k01vlr        setll     tivlr06l
028100000714     C                   if        %found(tivlr06l)
028200000714     C     k01vlr        reade     tivlr06l
028300000714     C                   dow       not %eof
028400070920     C                   if        vlrsqz = savSQZ
028500000714     C                   if        vlrflg <> '2' or vlrflg <> '4'
028600000714     C                   eval      wrkmsg = 'Errori solo in post traduzione.'
028700000714     C                   else
028800000714     C                   eval      wrkmsg = 'Errori anche in post traduzione.'
028900000714     C                   endif
029000000714     C                   eval      vlrflg = '2'
029100000714     C                   eval      vlrmsg =
029200020909     C                               '(SEDE)'
029300000714     C                             + '/'
029400000714     C                             + %trim(psjobname)
029500000714     C                             + '/'
029600000714     C                             + %trim(psuser)
029700000714     C                             + '/'
029800000714     C                             + %trim(%editc(psjobnbr:'Z'))
029900000714     C                             + %trim(wrkmsg)
030000030613     C                   move      savMSG        vlrmsg
030100000714     C                   update    tivlr600
030200000714     C                   endif
030300000714     C     k01vlr        reade     tivlr06l
030400000714     C                   enddo
030500000714     C                   endif
030600000714     C                   endif
030700000714     C*
030800000714     C                   endif
030900000714     C*
031000000714     C                   endsr
031100000714
031200000714
031300000714
031400000714
031500000717      /TITLE Routine di avio processamento record di log
031600000714     C*
031700000714     C     exeproc       begsr
031800090415     C*
031900090415     C                   seton                                        55
032000090415     C*
032100090415     C* Valorizzazione file TIVLRLOG
032200090415     C*
032300090415     C/EXEC SQL
032400090415     C+ SET :vlrDATIE = NOW()
032500090415     C/END-EXEC
032600090415     C*
032700090415     C
032800000714     C*
032900000714     C                   eval      prmpgi = *blanks
033000000714     C                   eval      prmppi = *blanks
033100000714     C                   call(e)   'TIS701R'
033200140801     C                   parm                    tivlrnrrp
033300010820     C                   parm      *blanks       prmpgi
033400010820     C                   parm      *blanks       prmppi
033500010820     C                   parm      *blanks       prmfit
033600010820     C                   parm      *blanks       prmfi3
033700010820     C                   parm      *blanks       prmfi4
033800090415     C*
033900090415     C* Valorizzazione file TIVLRLOG
034000090415     C*
034100090415     C/EXEC SQL
034200090415     C+ SET :vlrDATFE = NOW()
034300090415     C/END-EXEC
034400090415     C*
034500170817     C                   exsr      exepost
034600000714     C*
034700000714     C                   endsr
034800170817
034900000714
035000000714
035100000714      /TITLE Routine di *init PGM
035200000714     C*
035300000714     C     *inzsr        begsr
035400140801     C*
035500140801     C     *entry        plist
035600140801     C                   parm                    inPROC            2
035700000714     C*
035800000714     C     K01vlr        klist
035900000714     C                   kfld                    vlrprg
036000000714     C                   kfld                    vlrksc
036100000714     C*
036200000714     C                   time                    wrkoggiiso
036300000714     C                   time                    wrkora
036400000714     C     *iso          move      wrkoggiiso    wrkoggi
036500000714     C                   move(p)   wrkora        datime
036600000714     C                   movel     wrkoggi       datime
036700000714     C*
036800000714     C                   endsr
036900000714
