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
002900040517     FFNLBL01L  IF   E           K DISK
003000040517     F                                     RENAME(fnlbl000:fnlbl002)
003100001026     F*----------
003200001024     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
003300020820     ITITAS000      01
003400020820     ITITAS010      02
003500020820     ITITASP00      03
003600020820     ITITA4000      01
003700020820     ITITA4P00      03
003800020820     ITITAa000      01
003900020820     ITITAaP00      03
004000020820     ITITA7000      01
004100020820     ITITA7P00      03
004200020820     ITITAt000      01
004300020820     ITITAtP00      03
004400031202     ITITAh000      01
004500031202     ITITAhP00      03
004600031202     Ifiar5000      01
004700031202     Ifiar5P00      03
004800031202     Iecc2v000      01
004900031202     Iecc2vP00      03
005000031202     Ieccev000      01
005100031202     IeccevP00      03
005200031202     Ieccet000      01
005300031202     IeccetP00      03
005400031202     Iecced000      01
005500031202     IeccedP00      03
005600981106      *---------------------------------------------------------------------*
005700980127      * Ciclo principale
005800980127      *---------------------------------------------------------------------*
005900031202     c     *entry        plist
006000031202     c                   parm                    aggiorna          8
006100031202     c     aggiorna      ifne      'AGGIORNA'
006200031202     C                   seton                                        99
006300031202     c                   endif
006400020820     c*
006500020820     c                   setoff                                       010203
006600020820     C     Kspe          Chain     TITAS30c                           21
006700020820     c                   if        *in21='0'
006800031202     c                   movel     6             tasnsp
006900020820     c                   except    aggtas
007000020820     c                   endif
007100020820     C     Kspe          Chain     tncsb03l                           21
007200020820     c                   if        *in21='0'
007300031202     c                   movel     6             csbnsp
007400020820     c                   except    aggcsb
007500020820     c                   endif
007600020820     c*
007700020820     c                   setoff                                       2122
007800020820     c                   setoff                                       010203
007900020820     C     Kspe          setll     TITA430c                               21
008000020820     c   21              do        *hival
008100020820     c     kspe          reade     tita430c                               22
008200020820     c                   if        *in22='0'
008300031202     c                   movel     6             ta4nsp
008400020820     c                   except    aggta4
008500020820     c                   endif
008600020820     c  n22              enddo
008700020820     c*
008800020820     c                   setoff                                       2122
008900020820     c                   setoff                                       010203
009000020820     C     Kspe          setll     TITAa30c                               21
009100020820     c   21              do        *hival
009200020820     c     kspe          reade     titaa30c                               22
009300020820     c                   if        *in22='0'
009400031202     c                   movel     6             taansp
009500020820     c                   except    aggtaa
009600020820     c                   endif
009700020820     c  n22              enddo
009800020820     c*
009900020820     c                   setoff                                       2122
010000020820     c                   setoff                                       010203
010100020820     C     Kspe          setll     TITA730c                               21
010200020820     c   21              do        *hival
010300020820     c     kspe          reade     tita730c                               22
010400020820     c                   if        *in22='0'
010500031202     c                   movel     6             ta7nsp
010600020820     c                   except    aggta7
010700020820     c                   endif
010800020820     c  n22              enddo
010900020820     c*
011000020820     c                   setoff                                       2122
011100020820     c                   setoff                                       010203
011200020820     C     Kspe          setll     TITAt30c                               21
011300020820     c   21              do        *hival
011400020820     c     kspe          reade     titat30c                               22
011500020820     c                   if        *in22='0'
011600031202     c                   movel     6             tatnsp
011700020820     c                   except    aggtat
011800020820     c                   endif
011900020820     c  n22              enddo
012000031202     c*** aggiunti
012100031202     c                   setoff                                       2122
012200031202     c                   setoff                                       010203
012300031202     C     Kspe          setll     tncsv01l                               21
012400031202     c   21              do        *hival
012500031202     c     kspe          reade     tncsv01l                               22
012600031202     c                   if        *in22='0'
012700031202     c                   movel     6             csvnsp
012800031202     c                   except    aggcsv
012900031202     c                   endif
013000031202     c  n22              enddo
013100031202     C*
013200031202     c                   setoff                                       2122
013300031202     c                   setoff                                       010203
013400031202     C     Kspe          setll     TITAh30c                               21
013500031202     c   21              do        *hival
013600031202     c     kspe          reade     titah30c                               22
013700031202     c                   if        *in22='0'
013800031202     c                   movel     6             tahnsp
013900031202     c                   except    aggtah
014000031202     c                   endif
014100031202     c  n22              enddo
014200001023     C*
014300031202     c                   setoff                                       2122
014400031202     c                   setoff                                       010203
014500031202     C     Kspe          setll     fiar530c                               21
014600031202     c   21              do        *hival
014700031202     c     kspe          reade     fiar530c                               22
014800040324     c                   if        *in22='0' and
014900040324     c                             blpdim >= ar5dac
015000040324     c                   movel     6             ar5nsp
015100040324     c                   except    aggar5
015200031202     c                   endif
015300031202     c  n22              enddo
015400031202     C*
015500031202     c                   setoff                                       2122
015600031202     c                   setoff                                       010203
015700031202     C     Kspe          setll     ecc2v30c                               21
015800031202     c   21              do        *hival
015900031202     c     kspe          reade     ecc2v30c                               22
016000031202     c                   if        *in22='0'
016100031202     c                   movel     6             c2vnsp
016200031202     c                   except    aggc2v
016300031202     c                   endif
016400031202     c  n22              enddo
016500031202     C*
016600031202     c                   setoff                                       2122
016700031202     c                   setoff                                       010203
016800031202     C     Kspe          setll     eccev30c                               21
016900031202     c   21              do        *hival
017000031202     c     kspe          reade     eccev30c                               22
017100031202     c                   if        *in22='0'
017200031202     c                   movel     6             cevnsp
017300031202     c                   except    aggcev
017400031202     c                   endif
017500031202     c  n22              enddo
017600031202     C*
017700031202     c                   setoff                                       2122
017800031202     c                   setoff                                       010203
017900031202     C     Kspe          setll     eccet30c                               21
018000031202     c   21              do        *hival
018100031202     c     kspe          reade     eccet30c                               22
018200031202     c                   if        *in22='0'
018300031202     c                   movel     6             cetnsp
018400031202     c                   except    aggcet
018500031202     c                   endif
018600031202     c  n22              enddo
018700031202     C*
018800031202     c                   setoff                                       2122
018900031202     c                   setoff                                       010203
019000031202     C     Kspe          setll     ecced30c                               21
019100031202     c   21              do        *hival
019200031202     c     kspe          reade     ecced30c                               22
019300031202     c                   if        *in22='0'
019400031202     c                   movel     6             cednsp
019500031202     c                   except    aggced
019600031202     c                   endif
019700031202     c  n22              enddo
019800031202     C*
019900020820     c                   setoff                                       2122
020000020820     C     Kspe          setll     fnevb01l                               21
020100020820     c   21              do        *hival
020200020820     c     kspe          reade     fnevb01l                               22
020300020820     c                   if        *in22='0' and
020400020820     c                             blpdim >= evbdtv
020500031202     c                   movel     6             evbnsp
020600020820     c                   except    aggevb
020700020820     c                   endif
020800020820     c  n22              enddo
020900020820     c**
021000020820     c**
021100020820     C     Kgia          Chain     figcp01l                           21
021200020820     c                   if        *in21='0'
021300020820     c                   except    prtgia
021400020820     c                   endif
021500020820     C     Kspe          Chain     fndct02l                           21
021600020820     c                   if        *in21='0'
021700020820     c                   except    prtdct
021800020820     c                   endif
021900020820     C     Kspe          Chain     tiasc02l                           21
022000020820     c                   if        *in21='0'
022100020820     c                   except    prtasc
022200020820     c                   endif
022300020820     C     Kspe          Chain     fnlbl02l                           21
022400020820     c                   if        *in21='0'
022500020820     c                   except    prtlbl
022600020820     c                   endif
022700040517     C     Kspe          Chain     fnlbl01l                           21
022800040517     c                   if        *in21='0'
022900040517     c                   except    prtlblf
023000040517     c                   endif
023100020820     c*
023200020820     C*
023300961003      * Fine Lavoro
023400980127      *----------------------------------------------------*
023500980127      *   *INZSR: Operazioni iniziali                      *
023600980127      *----------------------------------------------------*
023700980127     C     *INZSR        BEGSR
023800001113      *
023900031202     C                   z-add     0             tipzero           2 0
024000980127      *
024100001026     C     KSPE          KLIST
024200020820     C                   KFLD                    blpAAS
024300020820     C                   KFLD                    blpLNP
024400020820     C                   KFLD                    blpNRS
024500020820     C                   KFLD                    blpNSP
024600001026      *
024700001026     C     KGIA          KLIST
024800020820     C                   KFLD                    blpAAS
024900020820     C                   KFLD                    blpLNP
025000020820     C                   KFLD                    blpNRS
025100020820     C                   KFLD                    blpNSP
025200001026     C                   KFLD                    TipZero
025300020820     c                   endsr
025400001026      *
025500001024      *-------------------------------------------------------------------------
025600020820     OQSYSPRT   E            prtlbl      1
025700020820     O                                            5 '**LBL'
025800020820     O                       blpaas           +   1
025900020820     O                       blplnp           +   1
026000020820     O                       blpnrs           +   1
026100020820     O                       blpnsp           +   1
026200040517     O                                        +   1 'Come MAMMA'
026300040517     OQSYSPRT   E            prtlblf     1
026400040517     O                                            5 '**LBL'
026500040517     O                       blpaas           +   1
026600040517     O                       blplnp           +   1
026700040517     O                       blpnrs           +   1
026800040517     O                       blpnsp           +   1
026900040517     O                                        +   1 'Come FIGLIA'
027000020820     O          E            prtasc      1
027100020820     O                                            5 '**ASC'
027200020820     O                       blpaas           +   1
027300020820     O                       blplnp           +   1
027400020820     O                       blpnrs           +   1
027500020820     O                       blpnsp           +   1
027600020820     O          E            prtdct      1
027700020820     O                                            5 '**DCT'
027800020820     O                       blpaas           +   1
027900020820     O                       blplnp           +   1
028000020820     O                       blpnrs           +   1
028100020820     O                       blpnsp           +   1
028200020820     O                       dctmgc           +   2
028300020820     O          E            prtgia      1
028400020820     O                                            5 '**GIA'
028500020820     O                       blpaas           +   1
028600020820     O                       blplnp           +   1
028700020820     O                       blpnrs           +   1
028800020820     O                       blpnsp           +   1
028900020820     O                       gcpmgc           +   2
029000020820     O          E            aggtas      2
029100020820     O                                            3 'tas'
029200020820     O                       blpaas           +   1
029300020820     O                       blplnp           +   1
029400020820     O                       blpnrs           +   1
029500020820     O                       blpnsp           +   1
029600020820     O                       tasdcm           +   2
029700020820     O                                          132 'TNVRLWBR'
029800020820     O                       UDATE         Y    123
029900020820     O          E            aggcsb      1
030000020820     O                                            3 'csb'
030100020820     O                       blpaas           +   1
030200020820     O                       blplnp           +   1
030300020820     O                       blpnrs           +   1
030400020820     O                       blpnsp           +   1
030500031202     O          E            aggcsv      1
030600031202     O                                            3 'csv'
030700031202     O                       blpaas           +   1
030800031202     O                       blplnp           +   1
030900031202     O                       blpnrs           +   1
031000031202     O                       blpnsp           +   1
031100020820     O          E            aggtat      1
031200020820     O                                            3 'tat'
031300020820     O                       blpaas           +   1
031400020820     O                       blplnp           +   1
031500020820     O                       blpnrs           +   1
031600020820     O                       blpnsp           +   1
031700020820     O          E            aggta7      1
031800020820     O                                            3 'ta7'
031900020820     O                       blpaas           +   1
032000020820     O                       blplnp           +   1
032100020820     O                       blpnrs           +   1
032200020820     O                       blpnsp           +   1
032300020820     O          E            aggtaa      1
032400020820     O                                            3 'taa'
032500020820     O                       blpaas           +   1
032600020820     O                       blplnp           +   1
032700020820     O                       blpnrs           +   1
032800020820     O                       blpnsp           +   1
032900020820     O          E            aggta4      1
033000020820     O                                            3 'ta4'
033100020820     O                       blpaas           +   1
033200020820     O                       blplnp           +   1
033300020820     O                       blpnrs           +   1
033400020820     O                       blpnsp           +   1
033500040326     O                       ta4trc           +   1
033600020820     O          E            aggevb      1
033700020820     O                                            3 'evb'
033800020820     O                       blpaas           +   1
033900020820     O                       blplnp           +   1
034000020820     O                       blpnrs           +   1
034100020820     O                       blpnsp           +   1
034200020820     o                       evbdtv           +   1
034300031202     O          E            aggtah      1
034400031202     O                                            3 'tah'
034500031202     O                       blpaas           +   1
034600031202     O                       blplnp           +   1
034700031202     O                       blpnrs           +   1
034800031202     O                       blpnsp           +   1
034900031202     O          E            aggar5      1
035000031202     O                                            3 'ar5'
035100031202     O                       blpaas           +   1
035200031202     O                       blplnp           +   1
035300031202     O                       blpnrs           +   1
035400031202     O                       blpnsp           +   1
035500031202     O          E            aggc2v      1
035600031202     O                                            3 'c2v'
035700031202     O                       blpaas           +   1
035800031202     O                       blplnp           +   1
035900031202     O                       blpnrs           +   1
036000031202     O                       blpnsp           +   1
036100031202     O          E            aggcev      1
036200031202     O                                            3 'cev'
036300031202     O                       blpaas           +   1
036400031202     O                       blplnp           +   1
036500031202     O                       blpnrs           +   1
036600031202     O                       blpnsp           +   1
036700031202     O          E            aggcet      1
036800031202     O                                            3 'cet'
036900031202     O                       blpaas           +   1
037000031202     O                       blplnp           +   1
037100031202     O                       blpnrs           +   1
037200031202     O                       blpnsp           +   1
037300031202     O          E            aggced      1
037400031202     O                                            3 'ced'
037500031202     O                       blpaas           +   1
037600031202     O                       blplnp           +   1
037700031202     O                       blpnrs           +   1
037800031202     O                       blpnsp           +   1
037900031202     o***
038000020820     Otitas000  E   n99 01   aggtas
038100020820     O                       tasnsp
038200020820     Otitas010  E   n99 02   aggtas
038300020820     O                       tasnsp
038400020820     Otitasp00  E   n99 03   aggtas
038500020820     O                       tasnsp
038600020820     Otita4000  E   n99 01   aggta4
038700020820     O                       ta4nsp
038800020820     Otita4p00  E   n99 03   aggta4
038900020820     O                       ta4nsp
039000020820     Otitaa000  E   n99 01   aggtaa
039100020820     O                       taansp
039200020820     Otitaap00  E   n99 03   aggtaa
039300020820     O                       taansp
039400020820     Otita7000  E   n99 01   aggta7
039500020820     O                       ta7nsp
039600020820     Otita7p00  E   n99 03   aggta7
039700020820     O                       ta7nsp
039800020820     Otitat000  E   n99 01   aggtat
039900020820     O                       tatnsp
040000020820     Otitatp00  E   n99 03   aggtat
040100020820     O                       tatnsp
040200020820     Otncsb000  E   n99      aggcsb
040300020820     O                       csbnsp
040400031202     Otncsv000  E   n99      aggcsv
040500031202     O                       csvnsp
040600020820     Ofnevb000  E   n99      aggevb
040700020820     O                       evbnsp
040800031202     Otitah000  E   n99 01   aggtah
040900031202     O                       tahnsp
041000031202     Otitahp00  E   n99 03   aggtah
041100031202     O                       tahnsp
041200031202     Ofiar5000  E   n99 01   aggar5
041300031202     O                       ar5nsp
041400031202     Ofiar5p00  E   n99 03   aggar5
041500031202     O                       ar5nsp
041600031202     Oecc2v000  E   n99 01   aggc2v
041700031202     O                       c2vnsp
041800031202     Oecc2vp00  E   n99 03   aggc2v
041900031202     O                       c2vnsp
042000031202     Oeccev000  E   n99 01   aggcev
042100031202     O                       cevnsp
042200031202     Oeccevp00  E   n99 03   aggcev
042300031202     O                       cevnsp
042400031202     Oeccet000  E   n99 01   aggcet
042500031202     O                       cetnsp
042600031202     Oeccetp00  E   n99 03   aggcet
042700031202     O                       cetnsp
042800031202     Oecced000  E   n99 01   aggced
042900031202     O                       cednsp
043000031202     Oeccedp00  E   n99 03   aggced
043100031202     O                       cednsp
