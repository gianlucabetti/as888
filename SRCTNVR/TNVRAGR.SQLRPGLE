000100160615      *===================================================================*
000200160615      *Tnvragr* aggiornamento FIAR5 record FAT anno e mese statistica pesi
000300160615      *===================================================================*
000400040119
000500040119     h decedit('0,') datedit(*dmy/)
000600040119     h option(*nodebugio)
000700040119
000800040119      *---------------------------------------------------------------*
000900040119
001100160615     Ffiar531c  uf   e           k disk
001200160615     Fwfvpd11f  if   e           k disk
001800040119      *
001900160615     d dAr5Fat       e ds
002100050526     d kpjba         e ds
004100050607     d*
004200160615     dFiar5ds        e ds                  extname(FIAR500F) prefix(W_)
004900040331      *
005000050526     D***
005100040331      *
005200040331      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
005201160615     d tipobolla       s              2    inz('F1')
005202160615     d kAr5Trd         s                   Like(Ar5Trd) inz('FAT')
005300050526      *
010600120323     C/EXEC SQL
010700120323     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
010800120323     C/END-EXEC
010900120323     C*
011000160615      * se flag peso ustao in tassazione è = D
011100160615      * se anno e mese statistica pesi uguale a 0000
011300120321
011400120321     C/EXEC SQL
011500120321     C+ declare C1 cursor for
011600160615     C+ select * from fiar500f where
011700160615     C+ substr(ar5uni, 9, 1) = 'D' and
011800160615     C+ substr(ar5uni, 58, 6) = '000000'
012100160615     C+ order by ar5aas , ar5lnp, ar5nrs, ar5nsp
012200120321     C+ for read only
012300120321     C/END-EXEC
012400120321     C
012500120321     C/EXEC SQL
012600120321     C+ open C1
012700120321     C/END-EXEC
012800120321     C
012900120321     C/EXEC SQL
013000160615     C+ Fetch C1 into :Fiar5ds
013100120321     C/END-EXEC
013200120321     C*
013300120321     C                   dow       sqlcod = *zeros
013400120321
013500160615      * aggancio il file di lavoro
013501160615     c     Kvpd11        chain     wfvpd11f
013502160615     c                   if        %found(wfvpd11f)
013600160615     c                   If        vpdVdes = 'S' and vpdVanno > 0 and
013601160615     c                             vpdVmese > 0
013602160615      * aggancio Fiar5
013603160615     c     KFiar5        chain     Fiar531c
013604160615     c                   if        %found(fiar531c)
013605160615     c                   eval      dar5fat = ar5uni
013606160615     c                   eval      §AR5SVAAA = vpdVanno
013607160615     c                   eval      §AR5SVAMM = vpdVmese
013608160615     c                   eval      ar5uni = Dar5fat
013609160615     c                   update    Fiar5000
013610160615     c                   endif
013611160615
013612160615     c                   endif
013613160615     c                   endif
014600120321     C*
014700120321     C/EXEC SQL
014800160615     C+ Fetch C1 into :Fiar5ds
014900120321     C/END-EXEC
015000120321     C*
015100120321     C                   enddo
015200120321     C*
015300120321     C/EXEC SQL
015400120321     C+ close C1
015500120321     C/END-EXEC
015600120321     C*
015900120323
016000120323     c                   eval      *inLR = *on
051100120323      *
051200050609      *************************************************************************
051300050609      * Operazioni iniziali
051400050609      *************************************************************************
051500050609     C     *inzsr        BEGSR
051600050609     c*
054200050609
054300050609     c     kFiar5        Klist
054301160615     c                   kfld                    w_ar5aas
054302160615     c                   kfld                    w_ar5lnp
054303160615     c                   kfld                    w_ar5nrs
054304160615     c                   kfld                    w_ar5nsp
054800050609     c                   Kfld                    kAr5Trd
054900050609
055900081105
056000160615      * Chiave file di lavoro WFVPD11
056100160615     c     KVPD11        Klist
056200160615     c                   kfld                    w_ar5aas
056300160615     c                   kfld                    w_ar5lnp
056400160615     c                   kfld                    w_ar5nrs
056500160615     c                   kfld                    w_ar5nsp
056600160615     c                   Kfld                    Tipobolla
056601160615
057400050609     c     *entry        plist
057500050609     c                   parm                    kpjba
057700060915
061500050609     c                   endsr
