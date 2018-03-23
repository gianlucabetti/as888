000100050707     H DECEDIT('0.') DATEDIT(*YMD.)
000200050922     H* Fieu45R *----------------------------------------------------*
000300900418     H*                                                              *
000400050922     H* Abbinamento ORMDPD con ORM bartolini                         *
000500050922     h* elabora tutti i rekord del file spia senza numero orm bartolini
000600171019      ** ___________________________________________________________ *
000700050922     Ffidpo05l  uf   E           k DISK    commit
000800080530     FtiDP300f  o  a E           k DISK    commit
000900050922     Ffnorm07l  if   E           k DISK
001000060920     Ffnblp01l  if   E           k DISK
001100070626     Ffiar404l  if   E           k DISK    infds(ar4ds)
001200171019     F*****fiar401l  if   E           k DISK    rename(fiar4000:fiar41)
001300171019     Ffipnd01l  if   E           k DISK
001400070626     Ffiar400f  if   E             DISK    rename(fiar4000:fiar40)
001500070626     F                                     Prefix(F_)
001600070626      *
001700070626     d ar4ds           ds
001800070626     d  ar4_nrr              397    400b 0
001900060920      **
002000070613     D dsbl4M        E DS
002100900418     D KPJBA         E DS
002200171019     D***** dsbl4i        E DS
002300060814     D trul33ds      E DS
002400050704
002500060809     D alfa10          s             10
002600050922     D trek            s                   like(ar4trc)
002700050923     D trek14          s             14
002800060811     D alfa15          s             15
002900171019      ** __________________________________________________________________ */
003000050704      *    main
003100171019      ** __________________________________________________________________ */
003200050919
003300050922     c     *loval        setll     fidpo05l
003400050705     c                   do        *hival
003500050922     c                   read      fidpo05l
003600050705
003700050922     c                   if        %eof(fidpo05l)
003800050705     c                   leave
003900050705     c                   endif
004000050705
004100050922      * prepara la chiave per agganciare l'orm
004200060809     c                   clear                   alfa10
004300060809     c                   movel     dpodep        alfa10
004400060809     c                   move      dpoord        alfa10
004500060811     c                   eval      alfa15 = 'DPD'+ %Trim(alfa10)
004600060814
004700050922      * aggancia l'orm Bartolini
004800050922     c     alfa15        setll     fnorm07l
004900050922     c                   do        *hival
005000050922     c     alfa15        reade     fnorm07l
005100050922     c                   if        %eof(fnorm07l)
005200050922     c                   leave
005300050922     c                   endif
005400060814
005500050922      * verifica il rekord se arriva da file
005600050922     c                   if        ormtco <> 'F'
005700050922     c                   iter
005800050922     c                   endif
005900060920
006000050922     c                   select
006100061006
006200171019      ** se l'ORM è stato inserito nel nostro sistema imposto il legame ORM senza il PARCEL
006300171019      **    poichè il Parcel si può avere solo all'atto della bollettazione (fase 900)
006400061006     c                   when      ormfao <= 600
006500080527      *
006600061006     c                   move      ormPOE        dpoPOE
006700061006     c                   move      ormNsr        dpoNrs
006800061006     c                   move      ormNOR        dpoNOR
006900061006     c                   move      ormNRV        dpoNRV
007000061006     c                   move      ormDAO        dpoDTORM
007100060814
007200171019      ** seleziona fase 900 e verifica se ORM bollettato x portare    _____ */
007300171019      ** lo status a 12                                               _____ */
007400050922     c                   when      ormfao = 900
007500080527      *
007600050922     c                   eval      trek14 =
007700050922     c                             %trim(%editc(ormpoe:'X')) +
007800050922     c                             %trim(%editc(ormnsr:'X')) +
007900050922     c                             %trim(%editc(ormnor:'X')) +
008000050922     c                             %trim(%editc(ormnrv:'X'))
008100050922     c                   move      'M'           trek
008200070613     c                   move      'N'           Sped_xOrm         1
008300130708     c**** kar4          chain     fiar404l
008400130708      **
008500130708     c     kar4          setll     fiar404l
008600130708     c     kar4          reade     fiar404l
008700171019      *  x potenziali ORM con + colli (DPD ne gestisce solo uno x ORdine nel clearing)?
008800070613     c                   dow       not %Eof(fiar404l)
008900130709      ** si è usato il fisico xchè il 404L ha AR4NOT limitato ai primi 14 caratteri
009000070626     c     ar4_nrr       chain     fiar400f
009100070626     c                   if        %Found(fiar400f)
009200070626     c                   movel     F_AR4NOT      dsbl4M
009300070613     c                   if        §B4CLG = 'S'
009400070613     c                   move      'S'           Sped_xOrm         1
009500070613     c                   leave
009600070613     c                   end
009700070626     c                   end
009800070613     c     kar4          reade     fiar404l
009900070613     c                   enddo
010000070613      ***
010100171019      *  se trovato la spedizione abbinata procede...  ?
010200070613     c********           if        %found(fiar404l)
010300070613     c                   if        Sped_xOrm = 'S'
010400060920      * x sicurezza controlla che sia data x partita la spedizione DPC > 0
010500060920     C     KBLP          chain     fnBLP01l
010600060920     c                   if        %found(fnBLP01l) and blpDPC > 0
010700171019      ***
010800171019      * vecchio file AR4 (I)
010900171019     c****               move      'I'           trek
011000171019     c**** kar1          chain     fiar401l
011100171019     c********           if        %found(fiar401l)  or
011200171019     c********                     NOT %found(fiar401l) and
011300171019     c********                     ORMNAR=*blank and ORMNAC=*blank
011400171019      *?        ?
011500171019      *  se trovato il Parcel?
011600171019      *  oppure  se NON trovato il Parcel xchè si tratta di un ritiro Italia?
011700171019      *    da consegnare in Italia e quindi non avrà mai il Parcel
011800171030     c     kpnd          chain     fipnd01l
011900171019     c                   if        %found(fipnd01l)  or
012000171019     c                             NOT %found(fipnd01l) and
012100171019     c                             ORMNAR=*blank and ORMNAC=*blank
012200171019      *?        ?
012300171019      *   x Italia su Italia occorre staccare un Parcel fittizio e completare l'invio
012400171019     c********           if        %found(fiar401l)
012500171019     c********           movel     ar4not        dsbl4i
012600171019     c********           movel     §B4IPN        dponrp
012700171019      *?        ?
012800171019     c                   if        %found(fipnd01l)
012900171019     c                   movel     pndIPN        dponrp
013000080527     c                   else
013100171019     c**  attribuisce un numero parcel fittizio per chiudere come consegnato
013200080530     c                   if        dpoNRP = *blank
013300080527     c                   clear                   trul33ds
013400080527     c                   z-add     25            i33cnu
013500080527     c                   z-add     1             i33num
013600080527     c                   movel     trul33ds      kpjbu
013700080527     c                   call      'TRUL33R'
013800080527     c                   PARM                    kpjba
013900080527     c                   movel     kpjbu         trul33ds
014000080530     c     o33err        Ifgt      *zeros
014100080527      *  per errore non avanza lo stato del FIDPO
014200080530     c                   Else
014300080527     c                   move      o33nri        dponrp
014400080530      **
014500171019      **  deve creare gli Scan Fissi da inviare a DPD poichè si aspettano
014600171019      **  sul giro nostro Locale gli scan:
014700171019      *    05 - Ritirato
014800171019      *    02 - Ingresso in Filiale
014900171019      *    03 - Messa in Consegna
015000171019      *    13 - Consegnato
015100080530     c                   exsr      Crea_Tipd3
015200080530     c                   Endif
015300080530     c                   end
015400080527
015500080527     c                   end
015600080527
015700050922     c                   move      ormPOE        dpoPOE
015800050929     c                   move      ormNsr        dpoNrs
015900050922     c                   move      ormNOR        dpoNOR
016000050929     c                   move      ormNRV        dpoNRV
016100060915     c                   move      ormDAO        dpoDTORM
016200060920     c                   z-add     12            dpoSTS
016300060920     c                   z-add     0             dpoDTR
016400050922     c                   endif
016500060920     c                   endif
016600050922     c                   endif
016700050922     c
016800171019      ** se la fase è 999, attribuisce un numero parcel fittizio per     __ */
016900171019      ** chiudere l'orm impostando lo status a 14 di mancato ritiro      __ */
017000060814     c                   when      ormfao = 999
017100060814     c
017200060814     c                   clear                   trul33ds
017300060814     c                   z-add     25            i33cnu
017400060814     c                   z-add     1             i33num
017500060814     c                   movel     trul33ds      kpjbu
017600060814     c                   call      'TRUL33R'
017700060814     c                   PARM                    kpjba
017800060814     c                   movel     kpjbu         trul33ds
017900060814
018000060814     c     o33err        ifgt      *zeros
018100060814      *  per errore non avanza lo stato del FIDPO
018200060814     c                   else
018300060908     c                   move      o33nri        dponrp
018400060915     c                   move      ormPOE        dpoPOE
018500060915     c                   move      ormNsr        dpoNrs
018600060915     c                   move      ormNOR        dpoNOR
018700060915     c                   move      ormNRV        dpoNRV
018800060915     c                   move      ormDAO        dpoDTORM
018900061213     c*****   Non deve essere segnalato come errore
019000061213     c*****              move      '04'          dpoCODER
019100061213     c                   move      *blank        dpoCODER
019200060920     c                   z-add     14            dpoSTS
019300060920     c                   z-add     0             dpoDTR
019400060814     c                   endif
019500060814
019600050922      * altre fasi bypassate
019700050922     c                   other
019800050922     c                   iter
019900050922     c                   endsl
020000150928
020100150928     c                   enddo
020200061006      *
020300061006     c                   update    fidpo000
020400050705
020500050705     c                   enddo
020600050707     c                   commit
020700050704      *
020800050705     c                   seton                                        lr
020900171019      ** __________________________________________________________________ */
021000050512     c     *inzsr        begsr
021100171019      ** __________________________________________________________________ */
021200050512     c*
021300050705     c     *entry        plist
021400050705     C                   parm                    kpjba
021500050705     C                   TIME                    ORADAT           14 0
021600050705     C                   MOVEL     ORADAT        ORATR             6 0
021700050704
021800050922     c     kar4          klist
021900050922     C                   kfld                    trek
022000050929     C                   kfld                    trek14
022100171019      *
022200171019     c**** kar1          klist
022300171019     C*******            kfld                    AR4AAS
022400171019     C*******            kfld                    AR4LNP
022500171019     C*******            kfld                    AR4NRS
022600171019     C*******            kfld                    AR4NSP
022700171019     C*******            kfld                    trek
022800171019      ****
022900060920     c     KBLP          klist
023000060920     C                   kfld                    AR4AAS
023100060920     C                   kfld                    AR4LNP
023200060920     C                   kfld                    AR4NRS
023300060920     C                   kfld                    AR4NSP
023400171030      ****
023500171030     c     Kpnd          klist
023600171030     C                   kfld                    blpAAS
023700171030     C                   kfld                    blpLNP
023800171030     C                   kfld                    blpNRS
023900171030     C                   kfld                    blpNSP
024000080530      *
024100050512     c                   endsr
024200171019      ** ===================================================================*/
024300080530      *  crea gli Scan Fissi da inviare
024400171019      ** ===================================================================*/
024500080530     c     Crea_Tipd3    begsr
024600080530     c*
024700080530     c*  Attenzione questi SCAN FISSI non SONO gli stessi delle Deliveries
024800080530     c*   scritti con il FIEU12R sono quelli necessari x gli ORM.
024900080530     c*
025000080530     c*  scrive Scan 05 Pick-up
025100171019      *    05 - Ritirato
025200080530     c                   clear                   tidp3000
025300080530     c                   z-add     blpAAS        dp3AAS
025400080530     c                   z-add     blpLNP        dp3LNP
025500080530     c                   z-add     blpNRS        dp3NRS
025600080530     c                   z-add     blpNSP        dp3NSP
025700080530     c                   movel     dponrp        dp3PRN
025800080530     c                   movel     '05'          dp3EVD
025900080530     c                   movel     'ORM'         dp3CEV
026000080611     c                   z-add     blpLNP        dp3FLE
026100080530     c                   z-add     ormDAR        dp3DEV
026200080530     c                   z-add     ormORR        dp3HEV
026300151211     c                   movel     blpAAS        dp3DTSPE
026400151211     c                   move      blpMGS        dp3DTSPE
026500080530     c                   write     tidp3000
026600080530     c*
026700080530     c*  scrive Scan 02 Inbound
026800171019      *    02 - Data entrata a magazzino
026900080530     c                   clear                   tidp3000
027000080530     c                   z-add     blpAAS        dp3AAS
027100080530     c                   z-add     blpLNP        dp3LNP
027200080530     c                   z-add     blpNRS        dp3NRS
027300080530     c                   z-add     blpNSP        dp3NSP
027400080530     c                   movel     dponrp        dp3PRN
027500080611     c                   z-add     blpLNA        dp3FLE
027600080530     c                   movel     '02'          dp3EVD
027700080603     c                   movel     'ORM'         dp3CEV
027800151211     c                   movel     blpAAS        dp3DTSPE
027900151211     c                   move      blpMGS        dp3DTSPE
028000080530     c                   write     tidp3000
028100080530     c*
028200080530     c*  scrive Scan 03 Messo in Consegna
028300171019      *    03 - Messo in Consegna
028400080530     c                   clear                   tidp3000
028500080530     c                   z-add     blpAAS        dp3AAS
028600080530     c                   z-add     blpLNP        dp3LNP
028700080530     c                   z-add     blpNRS        dp3NRS
028800080530     c                   z-add     blpNSP        dp3NSP
028900080530     c                   movel     dponrp        dp3PRN
029000080611     c                   z-add     blpLNA        dp3FLE
029100080530     c                   movel     '03'          dp3EVD
029200080603     c                   movel     'ORM'         dp3CEV
029300151211     c                   movel     blpAAS        dp3DTSPE
029400151211     c                   move      blpMGS        dp3DTSPE
029500080530     c                   write     tidp3000
029600080530     c*
029700080530     c*  scrive Scan 13 Consegnato
029800171019      *    13 - Consegnato
029900080530     c                   clear                   tidp3000
030000080530     c                   z-add     blpAAS        dp3AAS
030100080530     c                   z-add     blpLNP        dp3LNP
030200080530     c                   z-add     blpNRS        dp3NRS
030300080530     c                   z-add     blpNSP        dp3NSP
030400080530     c                   movel     dponrp        dp3PRN
030500080611     c                   z-add     blpLNA        dp3FLE
030600080530     c                   movel     '13'          dp3EVD
030700080603     c                   movel     'ORM'         dp3CEV
030800151211     c                   movel     blpAAS        dp3DTSPE
030900151211     c                   move      blpMGS        dp3DTSPE
031000080530     c                   write     tidp3000
031100080530     c*
031200080530     c                   endsr
031300171019      ** ===================================================================*/
