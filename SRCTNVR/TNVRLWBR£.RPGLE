000100980127     H DECEDIT('0,') DATEDIT(*YMD.)
000200981106      *---------------------------------------------------------------------*
000300031202      * mette 6 nel 1° byte del numero bolla doppia (quella vecchia)
000400001113      *---------------------------------------------------------------------*
000500020820     Ffiblpa0e  ip   E             DISK
000600031202     Fecc2v30C  uF   E           K DISK
000700031202     Feccev30C  uF   E           K DISK
000800031202     Feccet30C  uF   E           K DISK
000900031202     Fecced30C  uF   E           K DISK
001000031202     Ftitah30C  uF   E           K DISK
001100031202     Ffiar530C  uF   E           K DISK
001200031202     FTITAS30C  uF   E           K DISK
001300020820     FTITA430C  uF   E           K DISK
001400020820     FTITAa30C  uF   E           K DISK
001500020820     FTITA730C  uF   E           K DISK
001600020820     FTITAt30C  uF   E           K DISK
001700020820     Ffndct02l  iF   E           K DISK
001800020820     Ftiasc02l  iF   E           K DISK
001900001023     F*----------
002000020820     FTNCSB03L  uF   E           K DISK
002100001026     F*----------
002200031202     FTNCSv01L  uF   E           K DISK
002300031202     F*----------
002400020820     FFNEVB01L  uF   E           K DISK
002500001026     F*----------
002600020820     FFIGCP01L  iF   E           K DISK
002700001110     F*----------
002800001110     FFNLBL02L  IF   E           K DISK
002900001026     F*----------
003000001024     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
003100020820     ITITAS000      01
003200020820     ITITAS010      02
003300020820     ITITASP00      03
003400020820     ITITA4000      01
003500020820     ITITA4P00      03
003600020820     ITITAa000      01
003700020820     ITITAaP00      03
003800020820     ITITA7000      01
003900020820     ITITA7P00      03
004000020820     ITITAt000      01
004100020820     ITITAtP00      03
004200031202     ITITAh000      01
004300031202     ITITAhP00      03
004400031202     Ifiar5000      01
004500031202     Ifiar5P00      03
004600031202     Iecc2v000      01
004700031202     Iecc2vP00      03
004800031202     Ieccev000      01
004900031202     IeccevP00      03
005000031202     Ieccet000      01
005100031202     IeccetP00      03
005200031202     Iecced000      01
005300031202     IeccedP00      03
005400981106      *---------------------------------------------------------------------*
005500980127      * Ciclo principale
005600980127      *---------------------------------------------------------------------*
005700031202     c     *entry        plist
005800031202     c                   parm                    aggiorna          8
005900031202     c     aggiorna      ifne      'AGGIORNA'
006000031202     C                   seton                                        99
006100031202     c                   endif
006200020820     c*
006300020820     c                   setoff                                       010203
006400020820     C     Kspe          Chain     TITAS30c                           21
006500020820     c                   if        *in21='0'
006600031202     c                   movel     6             tasnsp
006700020820     c                   except    aggtas
006800020820     c                   endif
006900020820     C     Kspe          Chain     tncsb03l                           21
007000020820     c                   if        *in21='0'
007100031202     c                   movel     6             csbnsp
007200020820     c                   except    aggcsb
007300020820     c                   endif
007400020820     c*
007500020820     c                   setoff                                       2122
007600020820     c                   setoff                                       010203
007700020820     C     Kspe          setll     TITA430c                               21
007800020820     c   21              do        *hival
007900020820     c     kspe          reade     tita430c                               22
008000020820     c                   if        *in22='0'
008100031202     c                   movel     6             ta4nsp
008200020820     c                   except    aggta4
008300020820     c                   endif
008400020820     c  n22              enddo
008500020820     c*
008600020820     c                   setoff                                       2122
008700020820     c                   setoff                                       010203
008800020820     C     Kspe          setll     TITAa30c                               21
008900020820     c   21              do        *hival
009000020820     c     kspe          reade     titaa30c                               22
009100020820     c                   if        *in22='0'
009200031202     c                   movel     6             taansp
009300020820     c                   except    aggtaa
009400020820     c                   endif
009500020820     c  n22              enddo
009600020820     c*
009700020820     c                   setoff                                       2122
009800020820     c                   setoff                                       010203
009900020820     C     Kspe          setll     TITA730c                               21
010000020820     c   21              do        *hival
010100020820     c     kspe          reade     tita730c                               22
010200020820     c                   if        *in22='0'
010300031202     c                   movel     6             ta7nsp
010400020820     c                   except    aggta7
010500020820     c                   endif
010600020820     c  n22              enddo
010700020820     c*
010800020820     c                   setoff                                       2122
010900020820     c                   setoff                                       010203
011000020820     C     Kspe          setll     TITAt30c                               21
011100020820     c   21              do        *hival
011200020820     c     kspe          reade     titat30c                               22
011300020820     c                   if        *in22='0'
011400031202     c                   movel     6             tatnsp
011500020820     c                   except    aggtat
011600020820     c                   endif
011700020820     c  n22              enddo
011800031202     c*** aggiunti
011900031202     c                   setoff                                       2122
012000031202     c                   setoff                                       010203
012100031202     C     Kspe          setll     tncsv01l                               21
012200031202     c   21              do        *hival
012300031202     c     kspe          reade     tncsv01l                               22
012400031202     c                   if        *in22='0'
012500031202     c                   movel     6             csvnsp
012600031202     c                   except    aggcsv
012700031202     c                   endif
012800031202     c  n22              enddo
012900031202     C*
013000031202     c                   setoff                                       2122
013100031202     c                   setoff                                       010203
013200031202     C     Kspe          setll     TITAh30c                               21
013300031202     c   21              do        *hival
013400031202     c     kspe          reade     titah30c                               22
013500031202     c                   if        *in22='0'
013600031202     c                   movel     6             tahnsp
013700031202     c                   except    aggtah
013800031202     c                   endif
013900031202     c  n22              enddo
014000001023     C*
014100031202     c                   setoff                                       2122
014200031202     c                   setoff                                       010203
014300031202     C     Kspe          setll     fiar530c                               21
014400031202     c   21              do        *hival
014500031202     c     kspe          reade     fiar530c                               22
014600040324     c                   if        *in22='0' and
014700040324     c                             blpdim >= ar5dac
014800040324     c                   movel     6             ar5nsp
014900040324     c                   except    aggar5
015000031202     c                   endif
015100031202     c  n22              enddo
015200031202     C*
015300031202     c                   setoff                                       2122
015400031202     c                   setoff                                       010203
015500031202     C     Kspe          setll     ecc2v30c                               21
015600031202     c   21              do        *hival
015700031202     c     kspe          reade     ecc2v30c                               22
015800031202     c                   if        *in22='0'
015900031202     c                   movel     6             c2vnsp
016000031202     c                   except    aggc2v
016100031202     c                   endif
016200031202     c  n22              enddo
016300031202     C*
016400031202     c                   setoff                                       2122
016500031202     c                   setoff                                       010203
016600031202     C     Kspe          setll     eccev30c                               21
016700031202     c   21              do        *hival
016800031202     c     kspe          reade     eccev30c                               22
016900031202     c                   if        *in22='0'
017000031202     c                   movel     6             cevnsp
017100031202     c                   except    aggcev
017200031202     c                   endif
017300031202     c  n22              enddo
017400031202     C*
017500031202     c                   setoff                                       2122
017600031202     c                   setoff                                       010203
017700031202     C     Kspe          setll     eccet30c                               21
017800031202     c   21              do        *hival
017900031202     c     kspe          reade     eccet30c                               22
018000031202     c                   if        *in22='0'
018100031202     c                   movel     6             cetnsp
018200031202     c                   except    aggcet
018300031202     c                   endif
018400031202     c  n22              enddo
018500031202     C*
018600031202     c                   setoff                                       2122
018700031202     c                   setoff                                       010203
018800031202     C     Kspe          setll     ecced30c                               21
018900031202     c   21              do        *hival
019000031202     c     kspe          reade     ecced30c                               22
019100031202     c                   if        *in22='0'
019200031202     c                   movel     6             cednsp
019300031202     c                   except    aggced
019400031202     c                   endif
019500031202     c  n22              enddo
019600031202     C*
019700020820     c                   setoff                                       2122
019800020820     C     Kspe          setll     fnevb01l                               21
019900020820     c   21              do        *hival
020000020820     c     kspe          reade     fnevb01l                               22
020100020820     c                   if        *in22='0' and
020200020820     c                             blpdim >= evbdtv
020300031202     c                   movel     6             evbnsp
020400020820     c                   except    aggevb
020500020820     c                   endif
020600020820     c  n22              enddo
020700020820     c**
020800020820     c**
020900020820     C     Kgia          Chain     figcp01l                           21
021000020820     c                   if        *in21='0'
021100020820     c                   except    prtgia
021200020820     c                   endif
021300020820     C     Kspe          Chain     fndct02l                           21
021400020820     c                   if        *in21='0'
021500020820     c                   except    prtdct
021600020820     c                   endif
021700020820     C     Kspe          Chain     tiasc02l                           21
021800020820     c                   if        *in21='0'
021900020820     c                   except    prtasc
022000020820     c                   endif
022100020820     C     Kspe          Chain     fnlbl02l                           21
022200020820     c                   if        *in21='0'
022300020820     c                   except    prtlbl
022400020820     c                   endif
022500020820     c*
022600020820     C*
022700961003      * Fine Lavoro
022800980127      *----------------------------------------------------*
022900980127      *   *INZSR: Operazioni iniziali                      *
023000980127      *----------------------------------------------------*
023100980127     C     *INZSR        BEGSR
023200001113      *
023300031202     C                   z-add     0             tipzero           2 0
023400980127      *
023500001026     C     KSPE          KLIST
023600020820     C                   KFLD                    blpAAS
023700020820     C                   KFLD                    blpLNP
023800020820     C                   KFLD                    blpNRS
023900020820     C                   KFLD                    blpNSP
024000001026      *
024100001026     C     KGIA          KLIST
024200020820     C                   KFLD                    blpAAS
024300020820     C                   KFLD                    blpLNP
024400020820     C                   KFLD                    blpNRS
024500020820     C                   KFLD                    blpNSP
024600001026     C                   KFLD                    TipZero
024700020820     c                   endsr
024800001026      *
024900001024      *-------------------------------------------------------------------------
025000020820     OQSYSPRT   E            prtlbl      1
025100020820     O                                            5 '**LBL'
025200020820     O                       blpaas           +   1
025300020820     O                       blplnp           +   1
025400020820     O                       blpnrs           +   1
025500020820     O                       blpnsp           +   1
025600020820     O          E            prtasc      1
025700020820     O                                            5 '**ASC'
025800020820     O                       blpaas           +   1
025900020820     O                       blplnp           +   1
026000020820     O                       blpnrs           +   1
026100020820     O                       blpnsp           +   1
026200020820     O          E            prtdct      1
026300020820     O                                            5 '**DCT'
026400020820     O                       blpaas           +   1
026500020820     O                       blplnp           +   1
026600020820     O                       blpnrs           +   1
026700020820     O                       blpnsp           +   1
026800020820     O                       dctmgc           +   2
026900020820     O          E            prtgia      1
027000020820     O                                            5 '**GIA'
027100020820     O                       blpaas           +   1
027200020820     O                       blplnp           +   1
027300020820     O                       blpnrs           +   1
027400020820     O                       blpnsp           +   1
027500020820     O                       gcpmgc           +   2
027600020820     O          E            aggtas      2
027700020820     O                                            3 'tas'
027800020820     O                       blpaas           +   1
027900020820     O                       blplnp           +   1
028000020820     O                       blpnrs           +   1
028100020820     O                       blpnsp           +   1
028200020820     O                       tasdcm           +   2
028300020820     O                                          132 'TNVRLWBR'
028400020820     O                       UDATE         Y    123
028500020820     O          E            aggcsb      1
028600020820     O                                            3 'csb'
028700020820     O                       blpaas           +   1
028800020820     O                       blplnp           +   1
028900020820     O                       blpnrs           +   1
029000020820     O                       blpnsp           +   1
029100031202     O          E            aggcsv      1
029200031202     O                                            3 'csv'
029300031202     O                       blpaas           +   1
029400031202     O                       blplnp           +   1
029500031202     O                       blpnrs           +   1
029600031202     O                       blpnsp           +   1
029700020820     O          E            aggtat      1
029800020820     O                                            3 'tat'
029900020820     O                       blpaas           +   1
030000020820     O                       blplnp           +   1
030100020820     O                       blpnrs           +   1
030200020820     O                       blpnsp           +   1
030300020820     O          E            aggta7      1
030400020820     O                                            3 'ta7'
030500020820     O                       blpaas           +   1
030600020820     O                       blplnp           +   1
030700020820     O                       blpnrs           +   1
030800020820     O                       blpnsp           +   1
030900020820     O          E            aggtaa      1
031000020820     O                                            3 'taa'
031100020820     O                       blpaas           +   1
031200020820     O                       blplnp           +   1
031300020820     O                       blpnrs           +   1
031400020820     O                       blpnsp           +   1
031500020820     O          E            aggta4      1
031600020820     O                                            3 'ta4'
031700020820     O                       blpaas           +   1
031800020820     O                       blplnp           +   1
031900020820     O                       blpnrs           +   1
032000020820     O                       blpnsp           +   1
032100040326     O                       ta4trc           +   1
032200020820     O          E            aggevb      1
032300020820     O                                            3 'evb'
032400020820     O                       blpaas           +   1
032500020820     O                       blplnp           +   1
032600020820     O                       blpnrs           +   1
032700020820     O                       blpnsp           +   1
032800020820     o                       evbdtv           +   1
032900031202     O          E            aggtah      1
033000031202     O                                            3 'tah'
033100031202     O                       blpaas           +   1
033200031202     O                       blplnp           +   1
033300031202     O                       blpnrs           +   1
033400031202     O                       blpnsp           +   1
033500031202     O          E            aggar5      1
033600031202     O                                            3 'ar5'
033700031202     O                       blpaas           +   1
033800031202     O                       blplnp           +   1
033900031202     O                       blpnrs           +   1
034000031202     O                       blpnsp           +   1
034100031202     O          E            aggc2v      1
034200031202     O                                            3 'c2v'
034300031202     O                       blpaas           +   1
034400031202     O                       blplnp           +   1
034500031202     O                       blpnrs           +   1
034600031202     O                       blpnsp           +   1
034700031202     O          E            aggcev      1
034800031202     O                                            3 'cev'
034900031202     O                       blpaas           +   1
035000031202     O                       blplnp           +   1
035100031202     O                       blpnrs           +   1
035200031202     O                       blpnsp           +   1
035300031202     O          E            aggcet      1
035400031202     O                                            3 'cet'
035500031202     O                       blpaas           +   1
035600031202     O                       blplnp           +   1
035700031202     O                       blpnrs           +   1
035800031202     O                       blpnsp           +   1
035900031202     O          E            aggced      1
036000031202     O                                            3 'ced'
036100031202     O                       blpaas           +   1
036200031202     O                       blplnp           +   1
036300031202     O                       blpnrs           +   1
036400031202     O                       blpnsp           +   1
036500031202     o***
036600020820     Otitas000  E   n99 01   aggtas
036700020820     O                       tasnsp
036800020820     Otitas010  E   n99 02   aggtas
036900020820     O                       tasnsp
037000020820     Otitasp00  E   n99 03   aggtas
037100020820     O                       tasnsp
037200020820     Otita4000  E   n99 01   aggta4
037300020820     O                       ta4nsp
037400020820     Otita4p00  E   n99 03   aggta4
037500020820     O                       ta4nsp
037600020820     Otitaa000  E   n99 01   aggtaa
037700020820     O                       taansp
037800020820     Otitaap00  E   n99 03   aggtaa
037900020820     O                       taansp
038000020820     Otita7000  E   n99 01   aggta7
038100020820     O                       ta7nsp
038200020820     Otita7p00  E   n99 03   aggta7
038300020820     O                       ta7nsp
038400020820     Otitat000  E   n99 01   aggtat
038500020820     O                       tatnsp
038600020820     Otitatp00  E   n99 03   aggtat
038700020820     O                       tatnsp
038800020820     Otncsb000  E   n99      aggcsb
038900020820     O                       csbnsp
039000031202     Otncsv000  E   n99      aggcsv
039100031202     O                       csvnsp
039200020820     Ofnevb000  E   n99      aggevb
039300020820     O                       evbnsp
039400031202     Otitah000  E   n99 01   aggtah
039500031202     O                       tahnsp
039600031202     Otitahp00  E   n99 03   aggtah
039700031202     O                       tahnsp
039800031202     Ofiar5000  E   n99 01   aggar5
039900031202     O                       ar5nsp
040000031202     Ofiar5p00  E   n99 03   aggar5
040100031202     O                       ar5nsp
040200031202     Oecc2v000  E   n99 01   aggc2v
040300031202     O                       c2vnsp
040400031202     Oecc2vp00  E   n99 03   aggc2v
040500031202     O                       c2vnsp
040600031202     Oeccev000  E   n99 01   aggcev
040700031202     O                       cevnsp
040800031202     Oeccevp00  E   n99 03   aggcev
040900031202     O                       cevnsp
041000031202     Oeccet000  E   n99 01   aggcet
041100031202     O                       cetnsp
041200031202     Oeccetp00  E   n99 03   aggcet
041300031202     O                       cetnsp
041400031202     Oecced000  E   n99 01   aggced
041500031202     O                       cednsp
041600031202     Oeccedp00  E   n99 03   aggced
041700031202     O                       cednsp
