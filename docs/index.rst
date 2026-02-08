hsd-data Documentation
======================

.. image:: https://img.shields.io/badge/license-BSD--2--Clause--Patent-blue.svg
   :alt: License

**hsd-data** is a multi-format structured data IO library for Fortran. It
builds on `hsd-fortran <https://github.com/dftbplus/hsd-fortran>`_ to provide
unified loading and dumping of structured data in **HSD**, **XML**, **JSON**,
**TOML**, and **HDF5** formats.

Application code works exclusively with the familiar ``hsd_table`` /
``hsd_value`` tree from hsd-fortran — the backend handles all format-specific
serialization.

Features
--------

- **Unified API** — ``data_load`` / ``data_dump`` dispatch on format
  automatically (extension-based detection) or via explicit format constants.
- **Round-trip safe** — loading from one format and dumping to another preserves
  structure, values, and attributes (within each format's capabilities).
- **Built-in backends** — HSD, XML (pure-Fortran pull parser + serializer),
  JSON (pure-Fortran recursive-descent parser + serializer). No external
  XML/JSON libraries required.
- **Optional backends** — TOML (via `toml-f <https://github.com/toml-f/toml-f>`_),
  HDF5 (via HDF5 Fortran API).
- **CLI tool** — ``hsd-convert`` converts between any supported format pair.

Quick Start
-----------

.. code-block:: fortran

   program example
     use hsd_data
     implicit none

     type(hsd_table) :: root
     type(hsd_error_t), allocatable :: error

     ! Load from any supported format (auto-detected from extension)
     call data_load("input.hsd", root, error)
     if (allocated(error)) then
       call error%print()
       stop 1
     end if

     ! Dump to another format
     call data_dump(root, "output.json", error)

     ! Or use the high-level converter
     call data_convert("input.xml", "output.hsd", error)

   end program example

Contents
--------

.. toctree::
   :maxdepth: 2
   :caption: User Documentation

   installation
   user_guide
   format_mapping
   cli

.. toctree::
   :maxdepth: 2
   :caption: Reference

   api


Indices and tables
==================

* :ref:`genindex`
* :ref:`search`
