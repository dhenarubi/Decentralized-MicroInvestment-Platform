# Decentralized MicroInvestment Platform

A decentralized platform built on Stacks blockchain that enables users to pool funds into smart contracts for investing in local small businesses, with automated profit sharing based on individual contributions.

## Overview

This platform provides a transparent and efficient way to:
- Enable small businesses to register and receive investments
- Allow investors to contribute funds securely
- Track investments and business performance
- Automate investment management through smart contracts

## Smart Contract Features

### Core Functions

- `invest`: Allows users to invest STX tokens into registered businesses
- `register-business`: Enables businesses to register on the platform
- `get-investment`: View investment details for any investor
- `get-business-info`: Retrieve information about registered businesses

### Data Structures

- `investments`: Maps investor addresses to their investment amounts and timestamps
- `business-pool`: Tracks registered businesses, total raised funds, and active status

## Testing

The test suite covers core functionality including:
- Business registration
- Investment processing
- Data retrieval and validation

Run tests using:
```bash
npm test
