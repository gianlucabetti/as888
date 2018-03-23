000100050707     H DECEDIT('0.') DATEDIT(*YMD.)
000200050922     H* Fieu45R *----------------------------------------------------*
000300900418     H*                                                              *
000400050922     H* Abbinamento ORMDPD con ORM bartolini                         *
000500050922     h* elabora tutti i rekord del file spia senza numero orm bartolini
000600060811      **?___________________________________________________________ *
000700050922     Ffidpo05l  uf   E           k DISK    commit
000800080530     FtiDP300f  o  a E           k DISK    commit
000900050922     Ffnorm07l  if   E           k DISK
001000060920     Ffnblp01l  if   E           k DISK
001100070626     Ffiar404l  if   E           k DISK    infds(ar4ds)
001200060213     Ffiar401l  if   E           k DISK    rename(fiar4000:fiar41)
001300070626     Ffiar400f  if   E             DISK    rename(fiar4000:fiar40)
001400070626     F                                     Prefix(F_)
001500070626      *
001600070626     d ar4ds           ds
001700070626     d  ar4_nrr              397    400b 0
001800060920      **
001900070613     D dsbl4M        E DS
002000900418     D KPJBA         E DS
002100060622     D dsbl4i        E DS
002200060814     D trul33ds      E DS
002300050704
002400060809     D alfa10          s             10
002500050922     D trek            s                   like(ar4trc)
002600050923     D trek14          s             14
002700060811     D alfa15          s             15
002800050920      **?__________________________________________________________________ */
002900050704      *    main
003000050920      **?__________________________________________________________________ */
003100050919
003200050922     c     *loval        setll     fidpo05l
003300050705     c                   do        *hival
003400050922     c                   read      fidpo05l
003500050705
003600050922     c                   if        %eof(fidpo05l)
003700050705     c                   leave
003800050705     c                   endif
003900050705
004000050922      * prepara la chiave per agganciare l'orm
004100060809     c                   clear                   alfa10
004200060809     c                   movel     dpodep        alfa10
004300060809     c                   move      dpoord        alfa10
004400060811     c                   eval      alfa15 = 'DPD'+ %Trim(alfa10)
004500060814
004600050922      * aggancia l'orm Bartolini
004700050922     c     alfa15        setll     fnorm07l
004800050922     c                   do        *hival
004900050922     c     alfa15        reade     fnorm07l
005000050922     c                   if        %eof(fnorm07l)
005100050922     c                   leave
005200050922     c                   endif
005300060814
005400050922      * verifica il rekord se arriva da file
005500050922     c                   if        ormtco <> 'F'
005600050922     c                   iter
005700050922     c                   endif
005800060920
005900050922     c                   select
006000061006
006100061006      **?se l'ORM è stato inserito nel nostro sistema imposto il legame ORM senza il PARCEL
006200061006      **?   poichè il Parcel si può avere solo all'atto della bollettazione (fase 900)
006300061006     c                   when      ormfao <= 600
006400080527      *
006500061006     c                   move      ormPOE        dpoPOE
006600061006     c                   move      ormNsr        dpoNrs
006700061006     c                   move      ormNOR        dpoNOR
006800061006     c                   move      ormNRV        dpoNRV
006900061006     c                   move      ormDAO        dpoDTORM
007000060814
007100060920      **?seleziona fase 900 e verifica se ORM bollettato x portare    _____ */
007200060920      **?lo status a 12                                               _____ */
007300050922     c                   when      ormfao = 900
007400080527      *
007500050922     c                   eval      trek14 =
007600050922     c                             %trim(%editc(ormpoe:'X')) +
007700050922     c                             %trim(%editc(ormnsr:'X')) +
007800050922     c                             %trim(%editc(ormnor:'X')) +
007900050922     c                             %trim(%editc(ormnrv:'X'))
008000050922     c                   move      'M'           trek
008100070613     c                   move      'N'           Sped_xOrm         1
008200130708     c**** kar4          chain     fiar404l
008300130708      **
008400130708     c     kar4          setll     fiar404l
008500130708     c     kar4          reade     fiar404l
008600070613      *? x potenziali ORM con + colli (DPD ne gestisce solo uno x ORdine nel clearing)?
008700070613     c                   dow       not %Eof(fiar404l)
008800130709      ** si è usato il fisico xchè il 404L ha AR4NOT limitato ai primi 14 caratteri
008900070626     c     ar4_nrr       chain     fiar400f
009000070626     c                   if        %Found(fiar400f)
009100070626     c                   movel     F_AR4NOT      dsbl4M
009200070613     c                   if        §B4CLG = 'S'
009300070613     c                   move      'S'           Sped_xOrm         1
009400070613     c                   leave
009500070613     c                   end
009600070626     c                   end
009700070613     c     kar4          reade     fiar404l
009800070613     c                   enddo
009900070613      ***
010000070613      *? se trovato la spedizione abbinata procede...  ?
010100070613     c********           if        %found(fiar404l)
010200070613     c                   if        Sped_xOrm = 'S'
010300060920      * x sicurezza controlla che sia data x partita la spedizione DPC > 0
010400060920     C     KBLP          chain     fnBLP01l
010500060920     c                   if        %found(fnBLP01l) and blpDPC > 0
010600080527      *
010700060622     c                   move      'I'           trek
010800060213     c     kar1          chain     fiar401l
010900080527      *
011000080527      *? se trovato il Parcel?
011100080527     c                   if        %found(fiar401l)  or
011200080527      *?        ?
011300080527      *? oppure  se NON trovato il Parcel xchè si tratta di un ritiro Italia?
011400080527      *?   da consegnare in Italia e quindi non avrà mai il Parcel
011500080527     c                             NOT %found(fiar401l) and
011600080527     c                             ORMNAR=*blank and ORMNAC=*blank
011700080527      *?  x Italia su Italia occorre staccare un Parcel fittizio e completare l'invio
011800080527     c                   if        %found(fiar401l)
011900060622     c                   movel     ar4not        dsbl4i
012000060622     c                   movel     §B4IPN        dponrp
012100080527     c                   else
012200080527     c**? attribuisce un numero parcel fittizio per chiudere come consegnato
012300080530     c                   if        dpoNRP = *blank
012400080527     c                   clear                   trul33ds
012500080527     c                   z-add     25            i33cnu
012600080527     c                   z-add     1             i33num
012700080527     c                   movel     trul33ds      kpjbu
012800080527     c                   call      'TRUL33R'
012900080527     c                   PARM                    kpjba
013000080527     c                   movel     kpjbu         trul33ds
013100080530     c     o33err        Ifgt      *zeros
013200080527      *  per errore non avanza lo stato del FIDPO
013300080530     c                   Else
013400080527     c                   move      o33nri        dponrp
013500080530      **
013600080530      **? deve creare gli Scan Fissi da inviare a DPD poichè si aspettano
013700080530      **? sul giro nostro Locale gli scan:
013800080530      *?   05 - Ritirato
013900080530      *?   02 - Ingresso in Filiale
014000080530      *?   03 - Messa in Consegna
014100080530      *?   13 - Consegnato
014200080530     c                   exsr      Crea_Tipd3
014300080530     c                   Endif
014400080530     c                   end
014500080527
014600080527     c                   end
014700080527
014800050922     c                   move      ormPOE        dpoPOE
014900050929     c                   move      ormNsr        dpoNrs
015000050922     c                   move      ormNOR        dpoNOR
015100050929     c                   move      ormNRV        dpoNRV
015200060915     c                   move      ormDAO        dpoDTORM
015300060920     c                   z-add     12            dpoSTS
015400060920     c                   z-add     0             dpoDTR
015500050922     c                   endif
015600060920     c                   endif
015700050922     c                   endif
015800050922     c
015900060920      **?se la fase è 999, attribuisce un numero parcel fittizio per     __ */
016000060920      **?chiudere l'orm impostando lo status a 14 di mancato ritiro      __ */
016100060814     c                   when      ormfao = 999
016200060814     c
016300060814     c                   clear                   trul33ds
016400060814     c                   z-add     25            i33cnu
016500060814     c                   z-add     1             i33num
016600060814     c                   movel     trul33ds      kpjbu
016700060814     c                   call      'TRUL33R'
016800060814     c                   PARM                    kpjba
016900060814     c                   movel     kpjbu         trul33ds
017000060814
017100060814     c     o33err        ifgt      *zeros
017200060814      *  per errore non avanza lo stato del FIDPO
017300060814     c                   else
017400060908     c                   move      o33nri        dponrp
017500060915     c                   move      ormPOE        dpoPOE
017600060915     c                   move      ormNsr        dpoNrs
017700060915     c                   move      ormNOR        dpoNOR
017800060915     c                   move      ormNRV        dpoNRV
017900060915     c                   move      ormDAO        dpoDTORM
018000061213     c*****   Non deve essere segnalato come errore
018100061213     c*****              move      '04'          dpoCODER
018200061213     c                   move      *blank        dpoCODER
018300060920     c                   z-add     14            dpoSTS
018400060920     c                   z-add     0             dpoDTR
018500060814     c                   endif
018600060814
018700050922      * altre fasi bypassate
018800050922     c                   other
018900050922     c                   iter
019000050922     c                   endsl
019100150928
019200150928     c                   enddo
019300061006      *
019400061006     c                   update    fidpo000
019500050705
019600050705     c                   enddo
019700050707     c                   commit
019800050704      *
019900050705     c                   seton                                        lr
020000050920      **?__________________________________________________________________ */
020100050512     c     *inzsr        begsr
020200050920      **?__________________________________________________________________ */
020300050512     c*
020400050705     c     *entry        plist
020500050705     C                   parm                    kpjba
020600050705     C                   TIME                    ORADAT           14 0
020700050705     C                   MOVEL     ORADAT        ORATR             6 0
020800050704
020900050922     c     kar4          klist
021000050922     C                   kfld                    trek
021100050929     C                   kfld                    trek14
021200050922     c     kar1          klist
021300050922     C                   kfld                    AR4AAS
021400050922     C                   kfld                    AR4LNP
021500050922     C                   kfld                    AR4NRS
021600050922     C                   kfld                    AR4NSP
021700050922     C                   kfld                    trek
021800060920     c     KBLP          klist
021900060920     C                   kfld                    AR4AAS
022000060920     C                   kfld                    AR4LNP
022100060920     C                   kfld                    AR4NRS
022200060920     C                   kfld                    AR4NSP
022300080530      *
022400050512     c                   endsr
022500080530      **?===================================================================*/
022600080530      *  crea gli Scan Fissi da inviare
022700080530      **?===================================================================*/
022800080530     c     Crea_Tipd3    begsr
022900080530     c*
023000080530     c*  Attenzione questi SCAN FISSI non SONO gli stessi delle Deliveries
023100080530     c*   scritti con il FIEU12R sono quelli necessari x gli ORM.
023200080530     c*
023300080530     c*  scrive Scan 05 Pick-up
023400080530      *?   05 - Ritirato
023500080530     c                   clear                   tidp3000
023600080530     c                   z-add     blpAAS        dp3AAS
023700080530     c                   z-add     blpLNP        dp3LNP
023800080530     c                   z-add     blpNRS        dp3NRS
023900080530     c                   z-add     blpNSP        dp3NSP
024000080530     c                   movel     dponrp        dp3PRN
024100080530     c                   movel     '05'          dp3EVD
024200080530     c                   movel     'ORM'         dp3CEV
024300080611     c                   z-add     blpLNP        dp3FLE
024400080530     c                   z-add     ormDAR        dp3DEV
024500080530     c                   z-add     ormORR        dp3HEV
024600151211     c                   movel     blpAAS        dp3DTSPE
024700151211     c                   move      blpMGS        dp3DTSPE
024800080530     c                   write     tidp3000
024900080530     c*
025000080530     c*  scrive Scan 02 Inbound
025100080530      *?   02 - Data entrata a magazzino
025200080530     c                   clear                   tidp3000
025300080530     c                   z-add     blpAAS        dp3AAS
025400080530     c                   z-add     blpLNP        dp3LNP
025500080530     c                   z-add     blpNRS        dp3NRS
025600080530     c                   z-add     blpNSP        dp3NSP
025700080530     c                   movel     dponrp        dp3PRN
025800080611     c                   z-add     blpLNA        dp3FLE
025900080530     c                   movel     '02'          dp3EVD
026000080603     c                   movel     'ORM'         dp3CEV
026100151211     c                   movel     blpAAS        dp3DTSPE
026200151211     c                   move      blpMGS        dp3DTSPE
026300080530     c                   write     tidp3000
026400080530     c*
026500080530     c*  scrive Scan 03 Messo in Consegna
026600080530      *?   03 - Messo in Consegna
026700080530     c                   clear                   tidp3000
026800080530     c                   z-add     blpAAS        dp3AAS
026900080530     c                   z-add     blpLNP        dp3LNP
027000080530     c                   z-add     blpNRS        dp3NRS
027100080530     c                   z-add     blpNSP        dp3NSP
027200080530     c                   movel     dponrp        dp3PRN
027300080611     c                   z-add     blpLNA        dp3FLE
027400080530     c                   movel     '03'          dp3EVD
027500080603     c                   movel     'ORM'         dp3CEV
027600151211     c                   movel     blpAAS        dp3DTSPE
027700151211     c                   move      blpMGS        dp3DTSPE
027800080530     c                   write     tidp3000
027900080530     c*
028000080530     c*  scrive Scan 13 Consegnato
028100080530      *?   13 - Consegnato
028200080530     c                   clear                   tidp3000
028300080530     c                   z-add     blpAAS        dp3AAS
028400080530     c                   z-add     blpLNP        dp3LNP
028500080530     c                   z-add     blpNRS        dp3NRS
028600080530     c                   z-add     blpNSP        dp3NSP
028700080530     c                   movel     dponrp        dp3PRN
028800080611     c                   z-add     blpLNA        dp3FLE
028900080530     c                   movel     '13'          dp3EVD
029000080603     c                   movel     'ORM'         dp3CEV
029100151211     c                   movel     blpAAS        dp3DTSPE
029200151211     c                   move      blpMGS        dp3DTSPE
029300080530     c                   write     tidp3000
029400080530     c*
029500080530     c                   endsr
029600080530      **?===================================================================*/
