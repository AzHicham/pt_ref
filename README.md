# pt_ref [![Build status](https://travis-ci.org/TeXitoi/pt_ref.svg?branch=master)](https://travis-ci.org/TeXitoi/pt_ref)

Explore the public transport referencial with a nice DSL.

This project is a little funny POC, but nothing really polished.

## Tutorial

First, you need a [NTFS dataset](https://github.com/CanalTP/navitia/blob/dev/documentation/ntfs/ntfs_fr.md). You can find NTFS dataset at the [navitia open databar](https://www.navitia.io/datasets). If you are in a hurry, get the [Île-de-France dataset (click on ntfs.zip)](https://navitia.opendatasoft.com/explore/dataset/fr-idf/table/?sort=type_file)

Unzip your dataset.

You need a [rust](https://www.rust-lang.org/) nightly toolchain.

```
$ curl https://sh.rustup.rs -sSf | sh
$ rustup toolchain install nightly
```

Now, download and build pt_ref

```
$ git clone https://github.com/TeXitoi/pt_ref.git
$ cd pt_ref
$ cargo +nightly build --release
```

And now you can play with it. I like to use [rlwrap](https://github.com/hanslub42/rlwrap) but that's optional. I also redirect stdout to a file to not spam the terminal (with a `tail -f` on the file on another terminal).

```
$ rlwrap ./target/release/pt_ref ~/dev/run/navitia/data/fr-idf/ntfs/ > /tmp/pt_ref.log
Reading NTFS... done in 14s 727ms 83us 21ns.
> get stop_area <- get vehicle_journey <- stop_area.id = OIF:SA:8738400 and network.id = TN
85 objects in 15ms 777us 242ns.
> get route <- (stop_area.id = OIF:SA:8775860 and stop_area.id = OIF:SA:8727100) - (physical_mode.id = Bus or physical_mode.id = Metro)
4 objects in 4ms 78us 527ns.
> get line <- (get connection <- get line <- line.code(13) and physical_mode.id = Metro) - (line.code(13) and physical_mode.id = Metro or physical_mode.id = Bus)
17 objects in 5ms 649us 555ns.
> 
```

On `/tmp/pt_ref.log`, you'll have the json of your queries.
