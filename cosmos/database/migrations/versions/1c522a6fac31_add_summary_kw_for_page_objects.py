"""add summary, kw for page_objects

Revision ID: 1c522a6fac31
Revises: 7ca1f041894f
Create Date: 2020-05-13 13:25:28.293675

"""
from alembic import op
import sqlalchemy as sa
from sqlalchemy import Column, Integer, String, JSON, LargeBinary, ForeignKey, Boolean, Numeric, Text


# revision identifiers, used by Alembic.
revision = '1c522a6fac31'
down_revision = '7ca1f041894f'
branch_labels = None
depends_on = None


def upgrade():
    op.add_column('page_objects', Column('summary', Text(10000)))
    op.add_column('page_objects', Column('keywords', Text()))

def downgrade():
    op.drop_column('page_objects', 'summary')
    op.drop_column('page_objects', 'keywords')
